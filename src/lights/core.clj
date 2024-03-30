(ns lights.core
  ; TODO: https://github.com/ZeroOne3010/yetanotherhueapi
  (:require [lights.color :as c]
            [me.raynes.clhue [config :as conf]
             [lights :as lights]]
            [clj-http.client :as http]
            [clojure [edn :as edn]
                     [pprint :refer [pprint]]]
            [clojure.java.io :as io]
            [clojure.tools.cli :as cli]
            [hickory [core :as h]
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
  "Takes CLI options. Loads config from file. Validates config map, and returns
  it."
  [opts]
  (let [c (merge (load-config) opts)]
    (assert (string? (:user c)))
    (assert (string? (:address c)))
    c))

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

(defn auth!
  [opts]
  (conf/auth-user opts "clojure lights"))

(defn lights
  "Gets lights config."
  [config]
  (->> (lights/lights config)
       ; Strip out bulbs that don't do color
       (filter (comp :xy :state second))
       ; Tack on IDs.
       (map (fn [[id light]]
              [id (assoc light :id id)]))
       (into (sorted-map))))

(defn lights->clusters
  "Groups lights together if they share a common prefix with only letters or
  numbers distinguishing them. Takes a lights map, returns a vector of
  vectors of lights in the same cluster."
  [lights]
  (->> (vals lights)
       (group-by (fn [light]
                   (let [name (:name light)]
                     (if-let [match (re-find #"^(.+?)\s+(\d+|A-Z)$" name)]
                       (nth match 1)
                       name))))
       vals))

(defn lights!
  "Applies settings to multiple lights, given a map of light IDs to a map of
  state names to values."
  [config settings]
  ;(pprint settings)
  (doall
    (map (fn [[id state]]
            (lights/light config (name id) state))
          settings)))

(defn scale!
  [config factor]
  (->> (lights/lights config)
       (map-vals #(->> % :state :bri (+ factor) clip (hash-map :bri)))
       (lights! config)))

(defn higher!
  [config]
  (scale! config 50))

(defn lower!
  [config]
  (scale! -50))

(defn rand-web-palette
  "Get a color palette from the web"
  []
  (->> (http/get "http://www.colourlovers.com/api/palettes/random")
       :body
       h/parse
       h/as-hickory
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
                  3/4   0
                  1/2   1/2
                  1/4   1/3
                  1/16  1/4
                        1/5)
        angle   (* angle (if (< (rand) 1/2) -1 1))
        h       (rand) ; primary hue
        dh      1/16   ; noise in hue space
        ds      1/4    ; noise in saturation space
        dv      2/3]   ; noise in value space
    (->> (range 12)
         (map (fn [i]
                (c/hsv (- h (* angle i) (rand dh))
                       (- 1 (rand ds))
                       (- 1 (rand dv))))))))

(defn light->gamut
  "Returns a gamut map from a light's reported gamut in the API."
  [light]
  (zipmap [:red :green :blue]
          (:colorgamut (:control (:capabilities light)))))

(defn apply-color-to-light
  "Applies a color to a particular light, yielding a settings map for lights!
  with a single key (:id light) to a map of values to set for that light. Or
  nil if it doesn't think it can execute this transition while preserving
  ~aesthetic~."
  [config color' light]
  (let [; Current color
        state (:state light)
        _     (when-not (:xy state)
                (prn :no-xy)
                (pprint light))
        color (c/hue (:xy state) (:bri state))
        hue   (c/h color)
        ; New color, with a bit of noise
        color' (-> color'
                   (c/perturb-h 1/12)
                   (c/perturb-s 1/8)
                   (c/perturb-l 1/6))
        ; Is the new color in the light's gamut?
        ; TODO: I don't think gamut checking is actually working at all
        in-gamut? true
        ;_ (pprint light)
        ;gamut (light->gamut light)
        ;_ (prn :gamut gamut)
        ;in-gamut? (c/in-lamp-reach? (:xy (c/->hue color' (:modelid light)))
        ;                            gamut)
        ;_ (prn :in-gamut? in-gamut?)
        ; How far are we going in hue space?
        dh (Math/abs (- (c/h color) (c/h color')))
        near-color? (or (< dh 1/4)   ; no wrap
                        (< 3/4 dh))] ; wrap
    ; (prn :dh dh)
    (when (and in-gamut? near-color?)
      {(:id light)
       ; Close enough
       (assoc (c/->hue-hsb color' (:modelid light))
              :transitiontime
              (-> (:interval config)
                  ; deciseconds
                  (* 10)
                  ; Hold for a bit
                  (* 3/5)
                  long))})))

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
  (let [clusters (-> config lights lights->clusters)
        settings (map (partial apply-palette-to-cluster config palette) clusters)]
    (if (some nil? settings)
      (do (println "Skipping palette; can't transform aesthetically")
          nil)
      (lights! config (reduce merge {} settings)))))

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
       (do (println "Press the button on your Hue bridge, then hit enter within 30 seconds.")
           (read-line)
           (let [auth (auth! options)]
             (when-not (:success auth)
               (pprint auth)
               (System/exit 2))
             (save-config!
               {:user    (:username (:success auth))
                :address (:address options)})
             (println "Auth complete. You may now party.")))

       "party"
       (let [config (config options)]
         (party! config))))))
