(ns lights.core
  (:require [lights [color :as c]
                    [hue :as h]]
            [clj-http.client :as http]
            [clojure [edn :as edn]
                     [pprint :refer [pprint]]]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [hickory [core :as hickory]
             [select :as hs]]
            [cheshire.core :as json]
            [clojure.core.reducers :as r])
  (:import (java.io PushbackReader)))

(def config-file
  "Where do we squirrel away your config map?"
  ".config.edn")

(defn load-config
  "Returns config from file."
  []
  (try
    (with-open [r (PushbackReader. (io/reader config-file))]
      (edn/read r))))

(defn config
  "Takes CLI options. Loads config from file, merging in options map. Validates
  config map, and returns it."
  ([]
   (config {}))
  ([opts]
   (let [c (merge (load-config) opts)]
     (assert (string? (:user c)))
     (assert (string? (:address c)))
     c)))

(defn save-config!
  "Saves config map to file."
  [config]
  (with-open [w (io/writer config-file)]
    (binding [*out* w]
      (pprint config))))

(defn map-kv
  "Takes a function (f [k v]) which returns [k v], and builds a new map by
  applying f to every pair."
  [f m]
  (into {} (r/map f m)))

(defn map-vals
  "Maps values in a map."
  [f m]
  (map-kv (fn [[k v]] [k (f v)]) m))

(defn clip
  "Clips a value to the 0-255 range."
  [x]
  (min 255 (max 0 x)))

(defn lights->clusters
  "Groups a sequence of lights together if they share a common prefix with only
  letters or numbers distinguishing them. Takes a lights map, returns a vector
  of vectors of lights in the same cluster."
  [lights]
  (->> lights
       (group-by (fn [light]
                   (let [name (:name (:metadata light))]
                     (if-let [match (re-find #"^(.+?)\s+(\d+|[A-Z])$" name)]
                       (nth match 1)
                       name))))
       vals))

(defn rand-web-palette
  "Get a color palette from the web"
  []
  (->> (http/get "http://www.colourlovers.com/api/palettes/random")
       :body
       hickory/parse
       hickory/as-hickory
       (hs/select (hs/child (hs/tag :palettes)
                            (hs/tag :palette)
                            (hs/tag :colors)
                            (hs/tag :hex)))
       (map :content)
       (map first)
       (map c/hex)))

(defn rand-palette
  "Generate a random color palette."
  []
  (let [angle   (condp < (rand)
                  1/2   0 ; Over-represented because it's so rare we get a
                          ; chance to come here aesthetically
                  1/4   1/2
                  1/16  1/3
                        1/4)
        angle   (* angle (if (< (rand) 1/2) -1 1))
        h       (rand) ; primary hue
        dh      1/24   ; noise in hue space
        ds      1/4    ; noise in saturation space
        dv      2/3]   ; noise in value space
    (prn :--- angle)
    (->> (range 12)
         (map (fn [i]
                (c/hsv (- h (* angle i) (rand dh))
                       (- 1 (rand ds))
                       (- 1 (rand dv))))))))

(defn perturb-color
  "Takes a color and returns a nearby Hue color with a bit of noise."
  [color]
  (-> color
      (c/perturb-h 1/12)
      (c/perturb-s 1/8)
      (c/perturb-v 1/6)))

(defn near-hue?
  "Are two colors nearby each other in hue space, or would transitioning
  between them bring the color uncomfortably close to white?"
  [c1 c2]
  (let [; How far are we going in hue space?
        dh (Math/abs (- (c/h c1) (c/h c2)))]
    (or (< dh 1/3)    ; no wrap
        (< 2/3 dh)))) ; wrap

(defn light-color
  "Takes a light map from the Hue API, returns its color."
  [light]
  (c/hue (:xy (:color light))
         (:brightness (:dimming light))))

(defn color-update
  "Takes a config map, a light map, and a color. Produces a color update map
  with :dimming and :color transitioning to that color."
  [config light color']
  (let [color' (c/->hue color')]
    {(:id light)
     {:dynamics {:duration (-> (:interval config)
                               (* 1000) ; millis
                               (* 3/5)  ; spend some time stable
                               long)}
      :dimming {:brightness (:bri color')}
      :color {:xy {:x (:x color')
                   :y (:y color')}}}}))

(defn apply-color-to-light
  "Applies a color to a particular light, yielding a settings map for lights!
  with a single key (:id light) to a map of values to set for that light. Or
  nil if it doesn't think it can execute this transition while preserving
  ~aesthetic~."
  [config color' light]
  (let [color  (light-color light)
        color' (perturb-color color')]
    (when (near-hue? color color')
      (color-update config light color'))))

(defn apply-palette-to-cluster
  "Applies a palette to a specific cluster, yielding a settings map for
  lights! Or nil if it can't find a way to do that aesthetically."
  [config palette cluster]
  (loop [colors (shuffle palette)]
    (when (seq colors)
      (let [[color & colors] colors
            settings (map (partial apply-color-to-light config color) cluster)]
        (if (some nil? settings)
          ; Can't work with this color
          (recur colors)
          (reduce merge {} settings))))))

(defn apply-palette!
  "Takes a config map, a palette, and applies it to lights over dt seconds.
  Each cluster gets the same state plus a little noise. Avoids taking lights
  through low-saturation colors by ensuring their hues are close-ish. Returns
  nil if it can't preserve aesthetic."
  [config palette]
  (let [clusters (-> config h/lights lights->clusters)
        settings (map (partial apply-palette-to-cluster config palette) clusters)]
    (if (some nil? settings)
      (do (println "Skipping palette; can't transform aesthetically")
          nil)
      (h/lights! config (reduce merge {} settings)))))

(defn party!
  "Takes a config map and continuously adjusts the lights to random palettes
  every interval seconds."
  [config]
  (let [palette' (rand-palette)]
    (when (apply-palette! config palette')
      ; Applied
      (Thread/sleep (* 1000 (:interval config))))
    (recur config)))

(def cli-opts
  "tools.cli argument parsing spec"
  [
   ["-i" "--interval SECONDS" "Number of seconds between transitions."
    :default  60
    :parse-fn parse-long
    :validate [pos? "Must be positive"]]

   ["-a" "--address ADDRESS" "IP address or DNS name of your Hue bridge."]

   ["-u" "--user USERNAME" "Username for your Hue bridge."]
  ])

(defn -main
  "Main entry point"
  ([]
   (println "Usage: lein run <cmd> <flags ...>")
   (println)
   (println "Commands: auth, party")
   (println)
   (println "Flags:")
   (println (:summary (cli/parse-opts [] cli-opts))))
  ([cmd & args]
   (let [{:keys [options arguments summary errors]}
         (cli/parse-opts args cli-opts)]
     (when errors
       (doseq [e errors]
         (println "Error: " e))
       (System/exit 1))

     (case cmd
       "auth"
       (do (save-config!
             {:user    (h/create-api-key! (:address options))
              :address (:address options)})
           (println "Auth complete. You may now party."))

       "party"
       (let [config (config options)]
         (party! config))))))
