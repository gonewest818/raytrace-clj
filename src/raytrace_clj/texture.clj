(ns raytrace-clj.texture
  (:require [clojure.core.matrix :as mat]
            [mikera.image.core :refer [load-image get-pixel width height]]
            [mikera.image.colours :refer [components-rgb components-argb]]
            [raytrace-clj.perlin :refer [noise turbulence]]
            [raytrace-clj.util :refer :all]))

(defprotocol texture
  (sample [this uv p]))

(defrecord constant-texture [color]
  texture
  (sample [this uv p] color))

(defrecord checkerboard-texture [tex0 tex1 scale]
  texture
  (sample [this uv p]
    (let [sines (mat/ereduce * (mat/emap #(Math/sin %) (mat/mul scale p)))]
      (if (neg? sines)
        (sample tex0 uv p)
        (sample tex1 uv p)))))

(defrecord noise-texture [scale]
  texture
  (sample [this uv p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (noise (mat/mul scale p)))))))

(defrecord turbulence-texture [scale depth]
  texture
  (sample [this uv p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (Math/sin (+ (* scale (mat/mget p 2))
                                      (* 10.0 (turbulence p depth)))))))))
(defrecord marble-texture [scale depth]
  texture
  (sample [this uv p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (Math/sin (+ (* scale (mat/mget p 2))
                                      (* 10.0 (turbulence p depth)))))))))

(defrecord flip-texture-coord-u [tex]
  texture
  (sample [this [u v] p]
    (sample tex [(- 1.0 u) v] p)))

(defrecord flip-texture-coord-v [tex]
  texture
  (sample [this [u v] p]
    (sample tex [u (- 1.0 v)] p)))

(defrecord image-texture [image]
  texture
  (sample [this [u v] p]
    (let [i (int (* u (width image)))
          j (int (* v (height image)))
          rgb (components-rgb (get-pixel image i j))
          color (map #(/ % 255.0) rgb)]
      color)))

(defn make-image-texture
  "construct an image texture from a file"
  [file]
  (->image-texture (load-image file)))
