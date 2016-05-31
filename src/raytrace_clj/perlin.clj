(ns raytrace-clj.perlin
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))


(def random-vectors
  ;; is a lazy seq the best data type for performance?
  (repeatedly 256 #(mat/normalise (rand-in-unit-sphere))))

(def perm-x
  (shuffle (range 256)))

(def perm-y
  (shuffle (range 256)))

(def perm-z
  (shuffle (range 256)))

(defn perlin-coefficients
  "fetch 8 nearest neighbors from random-floats starting from ijk"
  [ijk]
  (let [[i j k] (seq ijk)]
    (for [di (range 2)
          dj (range 2)
          dk (range 2)]
      (nth random-vectors
           (bit-xor (nth perm-x (bit-and (int (+ i di)) 255))
                    (nth perm-y (bit-and (int (+ j dj)) 255))
                    (nth perm-z (bit-and (int (+ k dk)) 255)))))))

(defn perlin-interp
  "smoothly interpolate 8 nearest perlin unit vectors"
  [coeffs uvw]
  (let [[uu vv ww] (seq (mat/emap #(* % % (- 3 (* 2 %))) uvw))]
    (reduce +
            (for [i (range 2)
                  j (range 2)
                  k (range 2)]
              (let [weight-v (mat/sub uvw (vec3 i j k))]
                (* (+ (* i uu) (* (- 1.0 i) (- 1.0 uu)))
                   (+ (* j vv) (* (- 1.0 j) (- 1.0 vv)))
                   (+ (* k ww) (* (- 1.0 k) (- 1.0 ww)))
                   (mat/dot weight-v (nth coeffs (+ k (* 2 j) (* 4 i))))))))))

(defn noise
  "compute pseudorandom noise at point p"
  [p]
  (let [ijk (mat/emap #(int (Math/floor %)) p)
        uvw (mat/sub p ijk)]
    (perlin-interp (perlin-coefficients ijk) uvw)))

(defn turbulence
  "compute composite noise with multiple summed frequencies"
  [p depth]
  (loop [acc 0
         pt  p
         w   1.0
         i   0]
    (if (= i depth)
      (Math/abs acc)
      (recur (+ acc (* w (noise pt)))
             (mat/mul 2.0 pt)
             (/ w 2.0)
             (inc i)))))
