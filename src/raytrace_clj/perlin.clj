(ns raytrace-clj.perlin
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))


(def random-floats
  ;; is a seq the best data type for performance?
  (take 256 (repeatedly rand)))

(def perm-x
  (shuffle (range 256)))

(def perm-y
  (shuffle (range 256)))

(def perm-z
  (shuffle (range 256)))

(defn trilinear-coefficients
  "fetch 8 nearest neighbors from random-floats starting from ijk"
  [ijk]
  (let [[i j k] (seq ijk)]
    (for [di (range 2)
          dj (range 2)
          dk (range 2)]
      (nth random-floats
           (bit-xor (nth perm-x (bit-and (int (+ i di)) 255))
                    (nth perm-y (bit-and (int (+ j dj)) 255))
                    (nth perm-z (bit-and (int (+ k dk)) 255)))))))

(defn trilinear-interp
  "interpolate 8 nearest neighbors"
  [coeffs uvw]
  (let [[u v w] (seq uvw)]
    (reduce +
            (for [i (range 2)
                  j (range 2)
                  k (range 2)]
              (* (+ (* i u) (* (- 1.0 i) (- 1.0 u)))
                 (+ (* j v) (* (- 1.0 j) (- 1.0 v)))
                 (+ (* k w) (* (- 1.0 k) (- 1.0 w)))
                 (nth coeffs (+ k (* 2 j) (* 4 i))))))))

(defn noise
  "compute pseudorandom noise at point p"
  [p]
  (let [ijk (mat/emap #(Math/floor %) p)
        uvw (mat/emap #(* % % (- 3 (* 2 %))) 
                      (mat/sub p ijk))]
    (trilinear-interp (trilinear-coefficients ijk) uvw)))
