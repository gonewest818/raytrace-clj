(ns raytrace-clj.metrics
  (:require [metrics.core :refer [new-registry]]
            [metrics.counters :refer [counter value inc!]]
            [metrics.reporters.console :as console]))

;;; set up metrics
(def metrics-reg  (new-registry))
(def count-rays   (counter metrics-reg "total-rays"))
(def count-pixels (counter metrics-reg "total-pixels"))
(def count-aabb   (counter metrics-reg "aabb.intersection.total"))
(def CR (console/reporter metrics-reg {}))

(defn start []
  (console/start CR 10))

(defn stop []
  (console/start CR 10))
(defn increment! [met]
  (inc! met))

