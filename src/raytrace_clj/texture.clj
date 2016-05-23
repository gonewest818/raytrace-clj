(ns raytrace-clj.texture
  (:require [clojure.core.matrix :as mat]
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
      (if (< sines 0)
        (sample tex0 u v p)
        (sample tex1 u v p)))))
