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
                     (if-let [match (re-find #"^(.+?)\s+(\d+|[A-Z]|Left|Right)$" name)]
                       (nth match 1)
                       name))))
       vals))

(defn symmetric-cluster?
  "Is a cluster of lights symmetric? That is, does it have a left and a right
  side? We like to give these lights similar colors."
  [lights]
  (->> lights
       (map (comp :name :metadata))
       (every? (partial re-find #".*(Left|Right)$"))
       boolean))

(defn large-cluster?
  "For large clusters (3+ lights), we introduce more variation."
  [lights]
  (<= 3 (count lights)))

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

; Borrowed from Jepsen.util
(defn rand-exp
  "Generates a exponentially distributed random value with rate parameter
  lambda."
  [lambda]
  (* (Math/log (- 1 (rand))) (- lambda)))

(defn rand-distribution
  "Generates a random value with a distribution (default `:uniform`) of:
   ```clj
   ; Uniform distribution from min (inclusive, default 0) to max (exclusive, default Long/MAX_VALUE).
   {:distribution :uniform, :min 0, :max 1024}

   ; Geometric distribution with mean 1/p.
   {:distribution :geometric, :p 1e-3}

   ; Select a value from a sequence with equal probability.
   {:distribution :one-of, :values [-1, 4097, 1e+6]}

   ; Select a value based on weights. :weights are {value weight ...}
   {:distribution :weighted :weights {1e-3 1 1e-4 3 1e-5 1}}
   ```"
  ([] (rand-distribution {}))
  ([distribution-map]
   (let [{:keys [distribution min max p values weights]} distribution-map
         distribution (or distribution :uniform)
         min (or min 0)
         max (or max Long/MAX_VALUE)
         _   (assert (case distribution
                       :uniform   (< min max)
                       :geometric (number? p)
                       :one-of    (seq values)
                       :weighted  (and (map? weights)
                                       (->> weights
                                            vals
                                            (every? number?)))
                       false)
                     (str "Invalid distribution-map: " distribution-map))]
     (case distribution
       :uniform   (long (Math/floor (+ min (* (rand) (- max min)))))
       :geometric (long (Math/ceil  (/ (Math/log (rand))
                                       (Math/log (- 1.0 p)))))
       :one-of    (rand-nth values)
       :weighted  (let [values  (keys weights)
                        weights (reductions + (vals weights))
                        total   (last weights)
                        choices (map vector values weights)]
                    (let [choice (rand-int total)]
                      (loop [[[v w] & more] choices]
                        (if (< choice w)
                          v
                          (recur more)))))))))

(defn rand-palette
  "Generate a random color palette."
  []
  (let [; Are we broadly a monochrome, opposing, tertiary, or quaternary scheme?
        anchor-n (condp < (rand)
                   3/4  1
                   1/2  2
                   1/4  3
                        4)
        h       (rand) ; primary hue
        dh      1/24   ; noise in hue space
        ds      1/12   ; noise in saturation space
        dv      1/3    ; noise in value space
        ; Generate anchor hues widely separated around the color wheel
        anchors (map (fn [i]
                       (+ h (* i (/ anchor-n))))
                     (range anchor-n))
        ;_ (println "Raw anchors:" anchors)
        ; Often we only want, say, two of those 4 anchors. Note that we
        ; deliberately over-represent more monochrome schemes because they're
        ; more likely to be rejected during transition. To tune this
        ; distribution, try (->> (repeatedly #(l/rand-exp 1)) (map (fn [x]
        ; (long (min x 4)))) (take 10000) frequencies (into (sorted-map)))
        anchor-n (-> (rand-distribution {:distribution :geometric
                                         :p 1/2})
                     (min 4))
        anchors  (take anchor-n (shuffle anchors))]
    ;(println "Restricted anchors count" (count anchors))
    ; Now expand those anchor hues into a larger palette
    (mapcat (fn [anchor]
              (map (fn [i]
                     ; Add some noise to the anchor, producing a color
                     (c/hsv (- anchor (rand dh))
                            (- 1 (rand ds))
                            (- 1 (rand dv))))
                   ; A few colors per anchor. Mostly one, but sometimes up to
                   ; 8, which creates strong bias with sparing highlights.
                   (range (rand-distribution {:distribution :geometric
                                              :p 2/3}))))
           anchors)))

(defn perturb-color
  "Takes a color and an optional factor, and returns a nearby Hue color with a
  bit of noise."
  ([color]
   (perturb-color color 1))
  ([color factor]
   (-> color
       (c/perturb-h (* 1/16 factor))
       (c/perturb-s (* 1/12 factor))
       (c/perturb-v (* 1/6 factor)))))

(defn near-hue?
  "Are two colors nearby each other in hue space, or would transitioning
  between them bring the color uncomfortably close to white?"
  [c1 c2]
  (let [; How far are we going in hue space?
        dh (Math/abs (- (c/h c1) (c/h c2)))]
    ; (prn :c1 c1 :l (c/l c1) :v (c/v c1) :bri (-> c1 c/->hue :bri))
    (or (< dh 1/3)        ; no wrap
        (< 2/3 dh)        ; wrap
        (< (c/l c1) 1/10)    ; h1 very dim
        (< (c/l c2) 1/10)))) ; h2 very dim

(defn light-color
  "Takes a light map from the Hue API, returns its color."
  [light]
  (c/hue (:xy (:color light))
         (:brightness (:dimming light))))

(defn dynamics-update
  "Yields a partial map for updating a light, given a config, with the
  transition time :dynamics field."
  [config]
  {:duration (-> (:interval config)
                 (* 1000) ; millis
                 (* 3/5)  ; spend some time stable
                 long)})

(defn color-update
  "Takes a config map, a light map, and a color. Produces a color update map
  with :dimming and :color transitioning to that color. If bri is too low,
  turns off."
  [config light color']
  (let [color' (c/->hue color')]
    (prn "Color:" color')
    {(:id light)
     {; An experiment--turning off lights entirely when we ask for very dim
      ; values. I'm not sure I like this--the transition is rather abrupt.
      :on       {:on true #_(< 0 (:bri color'))}
      :dynamics (dynamics-update config)
      :dimming {:brightness (* (:bri color')
                               ; Scale by CLI brightness
                               (:brightness-max config)
                               1/100)}
      :color {:xy {:x (:x color')
                   :y (:y color')}}}}))

(defn apply-color-to-light
  "Applies a color to a particular light, yielding a settings map for lights!
  with a single key (:id light) to a map of values to set for that light. Or
  nil if it doesn't think it can execute this transition while preserving
  ~aesthetic~."
  [config color' light]
  ;(pprint light)
  (let [color (light-color light)]
    (if (near-hue? color color')
      ; We can execute an aesthetic transition
      (color-update config light color')
      ; Try turning the light off instead.
      (do ;(prn "color" color "->" color' " too far, turning off")
          ;(color-update config light (c/assoc-l color 0))
          nil ; Abort
          ))))

(defn gradient-colors
  "Takes a gradient light, returns a vector of colors."
  [light]
  (let [bri (:brightness (:dimming light))]
    (mapv (fn [point]
            (c/hue (:xy (:color point)) bri))
          (:points (:gradient light)))))

(defn apply-gradient-to-light-
  "Picks a few points from a gradient and constructs a settings map for lights!
  which applies palette colors to a light as a gradient. Returns nil if we fail
  to generate a new gradient, which will happen often."
  [config palette light]
  (let [colors   (gradient-colors light)
        palette (shuffle palette)
        ; Get the endpoints. If the light is a single color this will be empty.
        p1 (or (first colors) (first palette))
        p2 (or (peek colors)  (second palette))
        ; Find new endpoint colors
        p1 (first (filter (partial near-hue? p1) (shuffle palette)))
        p2 (first (filter (partial near-hue? p2) (shuffle palette)))]
    (when (and p1 p2)
      (let [; Perturb and pin to max values
            ; Pin to max value
            p1 (-> p1 perturb-color (c/assoc-v 1))
            p2 (-> p2 perturb-color (c/assoc-v 1))
            ; Build a five-point gradient between these two, rotating through
            ; hue space. Start by computing a hue angle...
            h1 (c/h p1)
            h2 (c/h p2)
            dh (- h2 h1)
            ; We can go either direction in hue space, but prefer a smaller
            ; angle more often--it stops us from getting locked into rainbow
            ; schemes and lets us do more monochromes
            [dh1 dh2] (sort-by abs [dh (- 1 dh)])
            dh (if (< (rand) 4/5)
                 dh1
                 dh2)
            ; Number of points
            n 5
            dh (/ dh n)
            ; Interpolate linearly through saturation space
            ds (/ (- (c/s p2) (c/s p1)) n)
            ; Expand to 5 color points
            colors' (mapv (fn [i]
                            (-> (c/hsv (+ h1       (* i dh))
                                       (+ (c/s p1) (* i ds))
                                       1) ; Always max value
                                c/->hue))
                          (range n))
            points' (mapv (fn [color]
                           {:color {:xy (select-keys color [:x :y])}})
                         colors')]
        ; Double-check that our newly generated midpoint won't pull us too far
        ; from the current midpoint.
        (if (or (< (count colors) 3)
                  (near-hue? (nth colors 2) (nth colors' 2)))
          {(:id light)
           {:dynamics (dynamics-update config)
            ; Strips are a lot dimmer than normal lights and should be maxed out
            :dimming {:brightness (rand-int 100)}
            :gradient {:points points'}}}
          ;(prn "New gradient midpoint too far from current midpoint")
          )))))

(defn apply-gradient-to-light
  "Like apply-gradient-to-light-, but retries several times in hopes of getting
  a working transition."
  [config palette light]
  (loop [tries 10]
    (when (pos? tries)
      (or (apply-gradient-to-light- config palette light)
          (recur (dec tries))))))

(defn apply-palette-to-light
  "Applies a palette, possibly with a preferred color, to a light, returning an
  update map or nil."
  [config palette color light]
  (if (h/gradient? light)
    (apply-gradient-to-light config palette light)
    (apply-color-to-light config color light)))

(defn merge-settings
  "Merges several individual settings maps. If any are nil, returns nil; this
  represents a failure to satisfy aesthetic requirements."
  [settings]
  (when (every? identity settings)
    (reduce merge {} settings)))

(defn apply-palette-to-cluster
  "Applies a palette to a specific cluster, yielding a settings map for
  lights! Or nil if it can't find a way to do that aesthetically."
  [config palette cluster]
  (let [monochrome? (if (large-cluster? cluster)
                      ; For large clusters, we can do everything as one color,
                      ; or break it up.
                      (rand-nth [true false])
                      ; For small clusters, we always do monochrome.
                      true)
        ; Large clusters can be overwhelmingly bright. We often like to disable
        ; some (or even all) of the lights.
        palette (if (large-cluster? cluster)
                  (let [n (rand-int (inc (count palette)))
                        [bright dim] (split-at n (shuffle palette))]
                    (concat bright (map #(c/assoc-v % 0) dim)))
                  palette)]
    (if monochrome?
      ; Try to find a single color that works
      (loop [colors (shuffle palette)]
        (when (seq colors)
          (let [[color & colors] colors
                settings
                (merge-settings
                  (cond
                    ; If we're dealing with a symmetric cluster, give them the
                    ; same color.
                    (symmetric-cluster? cluster)
                    (map (partial apply-palette-to-light config palette color)
                         cluster)

                    ; Otherwise, introduce some random noise
                    true
                    (map (partial apply-palette-to-light config palette)
                         (map perturb-color (repeat color))
                         cluster)))]
            (or settings
                ; Didn't work, try another color
                (recur colors)))))
      ; Break the cluster up into individual lights and try them separately
      (->> cluster
           (map vector)
           (map (partial apply-palette-to-cluster config palette))
           merge-settings))))

(defn apply-palette!
  "Takes a config map, a palette, and applies it to lights over dt seconds.
  Each cluster gets the same state plus a little noise. Avoids taking lights
  through low-saturation colors by ensuring their hues are close-ish, or by
  turning them off. Returns nil if it can't preserve aesthetic transitions."
  [config palette]
  (let [clusters (-> config h/lights lights->clusters)
        settings (map (partial apply-palette-to-cluster config palette) clusters)]
    (if (some nil? settings)
      (do ;(println "Skipping palette; can't transform aesthetically")
          (print ".")
          (flush)
          nil)
      (do (h/lights! config (reduce merge {} settings))
          (print "O")
          (flush)
          true))))

(defn party!
  "Takes a config map and continuously adjusts the lights to random palettes
  every interval seconds."
  [config]
  (let [palette' (rand-palette)]
    (try (when (apply-palette! config palette')
           ; Applied
           (Thread/sleep (* 1000 (:interval config))))
         (catch java.io.EOFException e
           (.printStackTrace e)
           (Thread/sleep 1000)))
    (recur config)))

(def cli-opts
  "tools.cli argument parsing spec"
  [
   ["-i" "--interval SECONDS" "Number of seconds between transitions."
    :default  60
    :parse-fn parse-long
    :validate [pos? "Must be positive"]]

   ["-a" "--address ADDRESS" "IP address or DNS name of your Hue bridge."]

   ["-b" "--brightness-max PERCENT" "Maximum brightness (of standard lamps). 0-100."
    :default 100
    :parse-fn parse-long
    :validate [#(<= 0 % 100) "Must be between 0 and 100"]]

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
       (do (let [bridge (or (:address options)
                            (h/discover))]
             (save-config!
               {:user    (h/create-api-key! bridge)
                :address bridge}))
           (println "Auth complete. You may now party."))

       "party"
       (let [config (config options)]
         (party! config))))))
