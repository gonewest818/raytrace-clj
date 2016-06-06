(ns raytrace-clj.util
  (:require [clojure.core.matrix :as mat]))

(defn vec3-old
  "convenience function to make 3d vectors"
  [u v w]
  (mat/matrix [u v w]))

;;; macro seems dramatically faster
(defmacro vec3 [a b c]
  (let [tmp (gensym 'vec3)]
    `(let [~tmp (mat/new-vector 3)]
       (mat/mset! ~tmp 0 ~a)
       (mat/mset! ~tmp 1 ~b)
       (mat/mset! ~tmp 2 ~c)
       ~tmp)))

(defn ray
  "simple model for a ray (note: direction is not normalized))"
  [origin direction t]
  {:origin origin :direction direction :time t})

(defn point-at-parameter
  "evaluate parameterized ray at position t"
  [ray t]
  (mat/add! (mat/mul! (mat/clone (:direction ray)) t)
            (:origin ray)))

(defn make-seeded-prng
  "create a random number generator with seed"
  [seed]
  (let [gen (java.util.Random. seed)]
    (fn [] (.nextDouble gen))))

(def prng (make-seeded-prng 0))

(defn rand-in-unit-disk
  "randomly choose point within the unit disk"
  []
  (let [square (fn [] (vec3 (- (* 2.0 (rand)) 1.0)
                            (- (* 2.0 (rand)) 1.0)
                            0))]
    (loop [p (square)]
      (if (>= (mat/dot p p) 1.0)
        (recur (square))
        p))))

(defn rand-in-unit-sphere
  "randomly choose point within the unit sphere"
  []
  (let [cube (fn [] (vec3 (- (* 2.0 (rand)) 1.0)
                          (- (* 2.0 (rand)) 1.0)
                          (- (* 2.0 (rand)) 1.0)))]
    (loop [p (cube)]
      (if (>= (mat/dot p p) 1.0)
        (recur (cube))
        p))))

(defmacro dlet [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(println (name '~n) ":" ~v)]))
                   (partition 2 bindings))]
     ~@body))
