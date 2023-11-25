(ns lights.speech
  "Speech recognition"
  (:require [lights.core :as core]
            [clojure.java.io :as io])
  (:import (edu.cmu.sphinx.api Configuration
                               AbstractSpeechRecognizer
                               LiveSpeechRecognizer
                               StreamSpeechRecognizer)
           (edu.cmu.sphinx.result WordResult)))

(defn config
  "Builds a configuration."
  []
  (doto (Configuration.)
    (.setAcousticModelPath
      "resource:/edu/cmu/sphinx/models/en-us/en-us")
    (.setGrammarPath
      "resource:/speech/")
    (.setGrammarName "lights")
    (.setUseGrammar true)
    (.setDictionaryPath
      "resource:/edu/cmu/sphinx/models/en-us/cmudict-en-us.dict")
    (.setLanguageModelPath
      "resource:/edu/cmu/sphinx/models/en-us/en-us.lm.bin")))

(defn recognizer
  "A live speech recognizer."
  [config]
  (let [recognizer (LiveSpeechRecognizer. config)]
    (.startRecognition recognizer true)
    recognizer))

(defn stop-recognition!
  "Stops a recognizer."
  [^AbstractSpeechRecognizer r]
  (.stopRecognition r))

(defn word-result->map
  "Convert a WordResult to a map."
  [^WordResult r]
  {:word       (.getSpelling (.getWord (.getPronunciation r)))
   :confidence (.getConfidence r)
   :score      (.getScore r)
   :filler?    (.isFiller r)
   :time-frame (.getTimeFrame r)})

(defn get-result
  "Gets a result from a recognizer."
  [^LiveSpeechRecognizer recognizer]
  (let [res (.getResult recognizer)]
    {:hypothesis (.getHypothesis res)
     :words      (map word-result->map (.getWords res))}))

(defn result-seq
  "A lazy sequence of recognizer results."
  [recognizer]
  (lazy-seq
    (let [res (get-result recognizer)]
      (cons res (result-seq recognizer)))))

(defn command-seq
  "Lazy sequence of recognized commands."
  [recognizer]
  (->> recognizer
       result-seq
       (remove #(= "<unk>" (:hypothesis %)))))

(defmacro with-recognizer
  [[r-sym r-expr] & body]
  `(let [~r-sym ~r-expr]
     (try
       ~@body
       (finally
         (stop-recognition! ~r-sym)))))

(defn interpret!
  "Evaluates a command."
  [command]
  (let [parsed (->> command
                    :words
                    (remove :filler?)
                    (map :word)
                    (map keyword))]
    (case (first parsed)
      :lights
      (case (second parsed)
        :higher (core/higher!)
        :lower  (core/lower!)
        :off    (prn :off)
        :on     (prn :on)))))

(defonce transform (atom nil))

(defn training-transform
  "Reads an audio file and builds a stats model."
  []
  (with-recognizer [r (StreamSpeechRecognizer. (config))]
    (.startRecognition r (io/input-stream (io/resource "training.wav")))
    (let [stats (.createStats r 1)]
      (loop [i 0]
        (when-let [result (.getResult r)]
          (println (str i ": " (.getHypothesis result)))
          (.collect stats result)
          (recur (inc i))))
      (.createTransform stats))))

(defn retrain!
  "Rebuilds the recognizer transform."
  []
  (reset! transform (training-transform)))

(defn listen
  "Listen for commands."
  []
  (with-recognizer [r (recognizer (config))]
    (.setTransform r @transform)
    (->> r
         command-seq
         (take 5)
         (map interpret!)
         dorun)))
