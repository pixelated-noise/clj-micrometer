(ns clj-micrometer.simple
  (:require [clj-micrometer.core :as core])
  (:refer-clojure :exclude [reset! time])
  (:import [io.micrometer.core.instrument Metrics]
           [java.util.concurrent.atomic AtomicLong]))

;; reference:
;; https://github.com/micrometer-metrics/micrometer/blob/master/micrometer-core/src/main/java/io/micrometer/core/instrument/Metrics.java
;; https://github.com/micrometer-metrics/micrometer/blob/master/micrometer-core/src/main/java/io/micrometer/core/instrument/MeterRegistry.java

(defn- metric-name [x]
  (cond (string? x)            x
        (simple-keyword? x)    (name x)
        (qualified-keyword? x) (str (namespace x) "." (name x))
        :else                  (throw (ex-info "metric name has to be a string or keyword" {:name x}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counter

(defn counter
  ([name]
   (counter name nil))
  ([name tags]
   (Metrics/counter (metric-name name) (core/map->tags tags))))

(defn inc!
  ([name]
   (inc! name nil))
  ([name tags]
   (doto (counter name tags)
     (.increment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gauge

(def gauge-values (atom {}))

(defn gauge [name tags]
  (if-let [number (get @gauge-values [name tags])]
    (do
      (Metrics/gauge (metric-name name) (core/map->tags tags) number)
      number)
    (let [number (AtomicLong. 0)]
      (clojure.core/swap! gauge-values assoc [name tags] number)
      (Metrics/gauge (metric-name name) (core/map->tags tags) number)
      number)))

(defn reset!
  ([name x]
   (when x
     (reset! name nil x)))
  ([name tags x]
   (when x
     (.set (gauge name tags) x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; timer

(defn timer
  ([name]
   (timer name nil))
  ([name tags]
   (Metrics/timer (metric-name name) (core/map->tags tags))))

(defn start-sample! []
  (core/start-sample!))

(defn stop-sample!
  ([sample name]
   (stop-sample! sample name nil))
  ([sample name tags]
   (.stop sample (timer name tags))))

(defmacro time [timer & body]
  `(let [sample# (start-sample!)
         ret#    (do ~@body)]
     (.stop sample# ~timer)
     ret#))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; registry

(defn add-registry! [registry]
  (Metrics/addRegistry registry))

(defn init!
  ([]
   (init! nil))
  ([opts]
   (add-registry! (core/simple-registry))
   (core/config-registry! Metrics/globalRegistry opts)))

(defn registry->data []
  (core/->data Metrics/globalRegistry))

(def global-registry Metrics/globalRegistry)
