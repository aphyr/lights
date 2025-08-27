(ns lights.hue
  "Talks to the Hue API."
  (:refer-clojure :exclude [get])
  (:require [clojure [pprint :refer [pprint]]]
            [clojure.tools.logging :refer [info warn]]
            [clj-http.client :as http]
            [cheshire.core :as json]
            [lights.color :as c]
            [slingshot.slingshot :refer [try+ throw+]])
  (:import (io.github.zeroone3010.yahueapi Color
                                           HueBridge
                                           HueBridgeConnectionBuilder)
           (io.github.zeroone3010.yahueapi.v2 Hue
                                              Light
                                              LightImpl)
           (io.github.zeroone3010.yahueapi.discovery
             HueBridgeDiscoveryService
             HueBridgeDiscoveryService$DiscoveryMethod)
           (java.util.function Consumer)))

(defn discover
  "Discovers a Hue bridge, returning its IP or nil."
  []
  (println "Discovering a Hue bridge...")
  (when-let [bridge (-> (HueBridgeDiscoveryService.)
                        (.discoverBridges
                          (reify Consumer (accept [_ _]))
                          (make-array HueBridgeDiscoveryService$DiscoveryMethod
                                      0))
                        .get
                        first)]
    (.getIp bridge)))

(defn create-api-key!
  "Takes a bridge IP and an app name, and returns an API key."
  ([bridge-ip]
   (create-api-key! bridge-ip "clojure lights"))
  ([bridge-ip app-name]
   (assert (string? bridge-ip))
   (println "Setting up app on Hue bridge" bridge-ip)
   (let [fut (.. (HueBridgeConnectionBuilder. bridge-ip)
                 (initializeApiConnection app-name))]
     (println "Press the button on your bridge to continue...")
     (.get fut))))

(defn hue
  "Makes a new Hue client given a config map of :address and :user."
  [config]
  (Hue. (:address config) (:user config)))

(defn req!
  "Makes an HTTP request to the API. Takes a config map, a path under /clip/,
  and a map to merge in to clj-http's options."
  [config path opts]
  ;(pprint opts)
  (let [r (http/request
            (merge {:url (str "https://" (:address config) "/clip/" path)
                    :insecure? true
                    :headers {:hue-application-key (:user config)}
                    :as :json
                    :coerce :unexceptional
                    }
                   opts))
        body (:body r)]
    (when (seq (:errors r))
      (throw+ {:type :hue
               :config config
               :path path
               :opts opts
               :errors (:errors r)}))
    (:data body)))

(defn get
  "Makes an HTTP get request."
  ([config path]
   (get config path {}))
  ([config path opts]
   (req! config path (assoc opts :method :get))))

(defn put!
  "Makes an HTTP put request."
  ([config path]
   (put! config path {}))
  ([config path opts]
   (put! config path opts nil))
  ([config path opts body]
   (if body
     (req! config path (assoc opts
                              :method :put
                              :body (json/generate-string body)
                              :content-type :json))
     (req! config path (assoc opts
                             :method :put)))))

(defn post!
  "Makes an HTTP post request."
  [config path opts]
  (req! config path (assoc opts :method :post)))

(defn lights
  "Gets the current lights as a vector."
  [config]
  ;(->> (.getLights hue)
  ;     (into (sorted-map)))
  (get config "v2/resource/light" {}))

(defn light!
  "Applies an update to a particular light by ID. See
  https://developers.meethue.com/develop/hue-api-v2/api-reference/#resource_light__id__put. For instance:

    (h/light! c tv
      {:dimming {:brightness (rand-int 100)}
       :dynamics {:duration 4000}
       :color {:xy {:x (rand) :y (rand)}}})"
  [config id state]
  (put! config (str "v2/resource/light/" id) {} state))

(defn lights!
  "Takes a map of light IDs to state update maps (see light!), and applies all
  those settings. This is slow, maybe someday I can try the scene API here."
  [config states]
  (let [t1 (System/nanoTime)]
    (->> states
         (mapv (fn [[id state]] (light! config id state))))
    #_(info "Applied in" (-> (System/nanoTime)
                           (- t1)
                           (/ 1e6)
                           float)
          "ms")))

(defn on?
  "Is a light object on?"
  [light]
  (:on (:on light)))

(defn color?
  "Does a light object support color?"
  [light]
  (:xy (:color light)))

(defn gradient?
  "Does a light support gradients?"
  [light]
  (:gradient light))
