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
                              :method       :put
                              :content-type :json
                              :body         (json/generate-string body)))
     (req! config path (assoc opts
                             :method :put)))))

(defn post!
  "Makes an HTTP post request."
  ([config path opts]
   (post! config path (assoc opts :method :post)))
  ([config path opts body]
   (req! config path (assoc opts
                            :method       :post
                            :content-type :json
                            :body         (json/generate-string body)))))

(defn delete!
  "Makes an HTTP delete request."
  ([config path]
   (delete! config path {}))
  ([config path opts]
   (req! config path (assoc opts :method :delete))))

(defn lights
  "Gets the current lights as a vector."
  [config]
  ;(->> (.getLights hue)
  ;     (into (sorted-map)))
  (get config "v2/resource/light" {}))

(defn light-ids
  "The IDS of all lights. Cached in the config when possible."
  [config]
  (or (:light-ids @(:cache config))
      (let [light-ids (mapv :id (lights config))]
        (swap! (:cache config) assoc :light-ids light-ids)
        light-ids)))

(defn light!
  "Applies an update to a particular light by ID. See
  https://developers.meethue.com/develop/hue-api-v2/api-reference/#resource_light__id__put. For instance:

    (h/light! c tv
      {:dimming {:brightness (rand-int 100)}
       :dynamics {:duration 4000}
       :color {:xy {:x (rand) :y (rand)}}})"
  [config id state]
  (put! config (str "v2/resource/light/" id) {} state))

(defn service-groups
  "Returns all service groups."
  [config]
  (get config "/v2/resource/service_group" {}))

(defn grouped-lights
  "Returns all grouped light services."
  [config]
  (get config "/v2/resource/grouped_light"))

(defn scenes
  "Lists all scenes."
  [config]
  (get config "/v2/resource/scene"))

(defn bridges
  "Lists all bridges."
  [config]
  (get config "/v2/resource/bridge"))

(def default-bridge
  "A memoized, default bridge."
  (memoize (fn [config] (first (bridges config)))))

(defn bridge-homes
  "Lists all bridge homes."
  [config]
  (get config "/v2/resource/bridge-homes"))

(defn zones
  "Lists all zones."
  [config]
  (get config "/v2/resource/zone"))

(defn create-zone!
  "Creates a zone containing the given set of light IDs."
  [config name archetype light-ids]
  (post! config "/v2/resource/zone" {}
         {:type "zone"
          :metadata {:name      name
                     :archetype archetype}
          :children (mapv (fn [id]
                            {:rid   id
                             :rtype "light"})
                          light-ids)}))

(defn delete-zone!
  "Deletes a zone by ID."
  [config id]
  (delete! config (str "/v2/resource/zone/" id)))

(def global-zone-name
  "The name of the special global zone, which we use to control all lights at
  once."
  "global")

(defn global-zone
  "Finds the global zone in the bridge, or nil if one doesn't exist."
  [config]
  (->> (zones config)
       (filter (fn [zone]
                 (= global-zone-name (:name (:metadata zone)))))
       first))

(defn ensure-global-zone!
  "Finds or creates the special global zone, and updates it to include every
  light we know about. Returns the ID of the zone. As a side effect, records
  the global zone ID in the config's cache, under :global-zone-id."
  [config]
  (let [light-ids (light-ids config)
        id (-> (if-let [zone-id (:id (global-zone config))]
                 ; Update
                 (put! config (str "/v2/resource/zone/" zone-id) {}
                       {:children (mapv (fn [id] {:rtype "light", :rid id})
                                        light-ids)})
                 ; Create
                 (create-zone! config global-zone-name "home" light-ids))
               first
               :rid)]
    (swap! (:cache config) assoc :global-zone-id id)
    id))

(defn delete-global-zone!
  "Cleans up the global zone. As a side effect, clears the global zone ID from
  the config cache."
  [config]
  (when-let [id (:id (global-zone config))]
    (swap! (:cache config) dissoc :global-zone-id)
    (delete-zone! config id)))

(defn scenes
  "Lists all scenes."
  [config]
  (get config "/v2/resource/scene"))

(defn create-scene!
  "Creates a new scene. Takes a map of light IDs to state updates for each
  light."
  [config name states]
  (let [gzid (:global-zone-id @(:cache config))
        _ (assert gzid "No global zone ID! Did you forget to ensure-global-zone!?")
        r (post! config "v2/resource/scene" {}
                 {:actions (mapv (fn [[id state]]
                                   {:target {:rid id, :rtype "light"}
                                    :action state})
                                 states)
                  :metadata {:name  name
                             :image {; how do you even get one of these
                                     :rid "b90c8900-a6b7-422c-a5d3-e170187dbf8c"
                                     :rtype "public_image"}}
                  :group {:rtype "zone"
                          :rid   gzid}})]
    r))

(def global-scene-name
  "The name we use for the special global scene."
  "global")

(defn global-scene
  "Finds the global scene in the Hue API, or nil."
  [config]
  (->> (scenes config)
       (filter (fn [scene]
                 (= global-scene-name (:name (:metadata scene)))))
       first))

(defn ensure-global-scene!
  "Finds or creates the special global scene, returning its ID. As a side
  effect, caches that ID in :global-scene-id in the config's cache."
  [config]
  (let [id (or (:id (global-scene config))
               (-> (create-scene! config global-scene-name
                                  ; By default, every light off
                                  (->> (light-ids config)
                                       (map (fn [id] [id {:on {:on false}}]))
                                       (into {})))
                   first
                   :rid))]
    (swap! (:cache config) assoc :global-scene-id id)
    id))

(defn set-global-scene!
  "Takes a config and a map of light ids to light states. Sets the global
  scene's actions to those."
  [config states]
  (let [sid (:global-scene-id @(:cache config))
        _ (assert sid "No global scene ID in cache; did you call ensure-global-scene!?")]
    (put! config (str "v2/resource/scene/" sid) {}
          {:actions (mapv (fn [[id state]]
                            {:target {:rid id, :rtype "light"}
                             :action state})
                          states)})))

(defn recall-global-scene!
  "Recalls the global scene in the given number of milliseconds."
  [config duration]
  (let [sid (:global-scene-id @(:cache config))]
    (assert sid "No global scene id!")
    (put! config (str "v2/resource/scene/" sid) {}
          {:recall {:action   "active"
                    :duration duration}})))

(defn lights!
  "Takes a map of light IDs to state update maps (see light!), and applies all
  those settings. This is slow, maybe someday I can try the scene API here."
  [config states]
  (let [t1 (System/nanoTime)
        duration (->> states
                      vals
                      (keep (comp :duration :dynamics))
                      (reduce max 0))]
    (set-global-scene! config states)
    (recall-global-scene! config duration)
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
