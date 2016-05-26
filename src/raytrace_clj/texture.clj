(ns raytrace-clj.texture
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.perlin :refer [noise turbulence]]
            [raytrace-clj.util :refer :all]))

(defprotocol texture
  (sample [this u v p]))

(defrecord constant-texture [color]
  texture
  (sample [this u v p] color))

(defrecord checkerboard-texture [tex0 tex1 scale]
  texture
  (sample [this u v p]
    (let [sines (mat/ereduce * (mat/emap #(Math/sin %) (mat/mul scale p)))]
      (if (neg? sines)
        (sample tex0 u v p)
        (sample tex1 u v p)))))

(defrecord noise-texture [scale]
  texture
  (sample [this u v p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (noise (mat/mul scale p)))))))

(defrecord turbulence-texture [scale depth]
  texture
  (sample [this u v p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (Math/sin (+ (* scale (mat/mget p 2))
                                      (* 10.0 (turbulence p depth)))))))))
