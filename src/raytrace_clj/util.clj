(ns raytrace-clj.util
  (:require [clojure.core.matrix :as mat]))

(defn vec3
  "convenience function to make 3d vectors"
  [u v w]
  (mat/matrix [u v w]))

(defn ray
  "simple model for a ray (note: direction is not normalized))"
  [origin direction time]
  {:origin origin :direction direction :time time})

(defn point-at-parameter
  "evaluate parameterized ray at position t"
  [ray t]
  (mat/add (:origin ray)
           (mat/mul (:direction ray) t)))

(defn rand-in-unit-disk
  "randomly choose point within the unit disk"
  []
  (defn square [] 
    (vec3 (- (* 2.0 (rand)) 1.0) 
          (- (* 2.0 (rand)) 1.0)
          0))
  (loop [p (square)]
    (if (>= (mat/dot p p) 1.0) 
      (recur (square))
      p)))

(defn rand-in-unit-sphere
  "randomly choose point within the unit sphere"
  []
  (defn cube [] 
    (vec3 (- (* 2.0 (rand)) 1.0) 
          (- (* 2.0 (rand)) 1.0)
          (- (* 2.0 (rand)) 1.0)))
  (loop [p (cube)]
    (if (>= (mat/dot p p) 1.0) 
      (recur (cube))
      p)))

