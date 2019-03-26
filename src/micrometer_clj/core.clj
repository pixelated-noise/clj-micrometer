(ns micrometer-clj.core
  (:refer-clojure :exclude [find count])
  (:require [clojure.string :as str]
            [clojure.core.protocols :as p]
            [clojure.datafy :refer [datafy]])
  (:import [java.util.concurrent TimeUnit]
           [java.util.concurrent.atomic AtomicLong]
           [io.micrometer.core.instrument
            Meter MeterRegistry Timer Counter Tag Tags Gauge
            Meter$Id Meter$Type]
           [io.micrometer.core.instrument.simple
            SimpleMeterRegistry]
           [io.micrometer.core.instrument.composite
            CompositeMeterRegistry]))

(def time-units
  {:nanos   TimeUnit/NANOSECONDS
   :micros  TimeUnit/MICROSECONDS
   :millis  TimeUnit/MILLISECONDS
   :seconds TimeUnit/SECONDS
   :minutes TimeUnit/MINUTES
   :hours   TimeUnit/HOURS
   :days    TimeUnit/DAYS})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti make-meter (fn [_ x] (:type x)))

(defn add-meters! [registry meters-specs]
  (doseq [m meters-specs]
    (make-meter registry m)))

(defn composite-registry []
  (CompositeMeterRegistry.))

(defn simple-registry []
  (SimpleMeterRegistry.))

(defn- flatten-tags [tags]
  (->> tags seq flatten (map clojure.core/name) (into-array String)))

(defn set-common-tags! [registry tags]
  (-> registry .config (.commonTags (flatten-tags tags))))

(defn default-registry
  ([]
   (default-registry {:tags nil}))
  ([{:keys [tags meters meter-filters]}]
   (let [reg (doto (composite-registry)
               (.add (simple-registry)))]

     (when tags
       (set-common-tags! reg tags))

     (when meters
       (add-meters! reg meters))

     (when meter-filters
       (doseq [f meter-filters]
         (-> reg .config (.meterFilter f))))
     reg)))

(defn- tags->map [tags]
  (->> tags
       seq
       (map (fn [tag]
              [(keyword (.getKey tag))
               (.getValue tag)]))
       (into {})))

(defn- map->tags [tags]
  (map (fn [[k v]] (Tag/of (name k) (name v))) tags))

(defn meters [registry]
  (seq (.getMeters registry)))

(defn meter-name [meter]
  (some-> meter .getId .getName))

(defn meter-tags [meter]
  (-> meter .getId .getTags tags->map))

(defn meter-description [meter]
  (-> meter .getId .getDescription))

(defn find [registry name]
  (some->> registry meters (filter #(= name (meter-name %))) first))

(extend-protocol p/Datafiable
  MeterRegistry
  (datafy [this]
    {:meters (map datafy (meters this))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn counter
  ([registry name]
   (counter registry name nil))
  ([registry name tags]
   (.counter registry name (flatten-tags tags))))

(defn inc! [counter]
  (.increment counter))

(defn count [counter]
  (.count counter))

(extend-protocol p/Datafiable
  Counter
  (p/datafy [this]
    {:name  (meter-name this)
     :tags  (meter-tags this)
     :type  :counter
     :count (count this)}))

(defmethod make-meter :counter
  [registry {:keys [name tags base-unit description]}]
  (cond-> (Counter/builder name)
    tags (.tags (map->tags tags))
    base-unit (.baseUnit base-unit)
    description (.description description)
    :then (.register registry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn timer
  ([registry name]
   (timer registry name nil))
  ([registry name tags]
   (.timer registry name (flatten-tags tags))))

(defn start-sample! []
  (Timer/start))

(defn stop-sample! [sample timer]
  (.stop sample timer))

(defn wrap-timer* [timer fun]
  (fn [& args]
    (let [sample (start-sample!)
          ret    (apply fun args)]
      (stop-sample! sample timer)
      ret)))

(defmacro wrap-timer [timer & body]
  `(let [sample# (start-sample!)
         ret#    (do ~@body)]
     (stop-sample! sample# ~timer)
     ret#))

(defn total-time [timer unit]
  (.totalTime timer (time-units unit)))

(defn max-time [timer unit]
  (.max timer (time-units unit)))

(defn mean-time [timer unit]
  (.mean timer (time-units unit)))

(defn rate [timer unit]
  (/ (count timer)
     (total-time timer unit)))

(defn record! [timer amount unit]
  (.record timer amount (time-units unit)))

(extend-protocol p/Datafiable
  Timer
  (datafy [this]
    (let [unit :millis]
      {:name       (meter-name this)
       :tags       (meter-tags this)
       :type       :timer
       :count      (count this)
       :total-time (total-time this unit)
       :max-time   (max-time this unit)
       :mean-time  (mean-time this unit)})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gauge
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn gauge
  ([registry name]
   (gauge registry name {}))
  ([registry name tags]
   (let [an (AtomicLong. 0)
         a  (atom 0)]
     (.gauge registry name (map->tags tags) an)
     (add-watch a :gauge (fn [_ _ _ n]
                           (.set an n)))
     a)))

(extend-protocol p/Datafiable
  Gauge
  (datafy [this]
    (let [unit :millis]
      {:name       (meter-name this)
       :tags       (meter-tags this)
       :type       :gauge
       :value      (.value this)
       :desciption (meter-description this)})))

(defmethod make-meter :gauge
  [registry {:keys [name tags base-unit description strong-ref]}]
  (cond-> (Counter/builder name)
    tags (.tags (map->tags tags))
    base-unit (.baseUnit base-unit)
    description (.description description)
    strong-ref (.strongReference strong-ref)
    :then (.register registry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (do
    ;;(def reg (-> dev/sys :composite))

    (def cc (counter reg "foo" {:machine "turbo"}))

    (def reg
      (default-registry
       {:tags   {:machine "Stathis' dev machine"}
        :meters [{:type :counter
                  :name "foo"
                  :tags {:machine "turbo"}}
                 {:type :gauge
                  :name "function"}]}))

    (inc! cc)
    (count cc)

    (def tt (timer reg "function"))

    (defn slow-fn []
      (Thread/sleep 2000)
      77)

    (def slow-instrumented (wrap-timer* tt slow-fn))

    (def gg (gauge reg "my-gauge"))
    (swap! gg inc)
    (reset! gg 888)

    )

  (slow-instrumented)

  (wrap-timer
   tt
   (Thread/sleep 3000)
   99)

  )
