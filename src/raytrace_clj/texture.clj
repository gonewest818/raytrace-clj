(ns raytrace-clj.texture
  (:require [clojure.core.matrix :as mat]
            [mikera.image.core :refer [load-image get-pixel width height]]
            [mikera.image.colours :refer [components-rgb components-argb]]
            [raytrace-clj.perlin :refer [noise turbulence]]
            [raytrace-clj.util :refer :all]))

(defprotocol Texture
  (sample [this uv p]))

;;;
;;; Constant color

(defrecord Constant [color]
  Texture
  (sample [this uv p] color))

(defn constant
  "constant color as a texture"
  [& {:keys [color]}]
  (->Constant color))

;;;
;;; UV Gradient

(defrecord UVGradient [co cu cv cuv]
  Texture
  (sample [this [u v] p]
    (let [a (mat/add (mat/mul cu (- 1 u))
                     (mat/mul co u))
          b (mat/add (mat/mul cuv (- 1 u))
                     (mat/mul cv u))]
      (mat/add (mat/mul b (- 1 v))
               (mat/mul a v)))))

(defn uv-gradient
  "gradient ramp along u and v directions"
  [& {:keys [co cu cv cuv]}]
  (->UVGradient co cu cv cuv))

;;;
;;; Checkerboard as a solid texture

(defrecord Checkerboard [tex0 tex1 scale]
  Texture
  (sample [this uv p]
    (let [sines (mat/ereduce * (mat/emap #(Math/sin %) (mat/mul scale p)))]
      (if (neg? sines)
        (sample tex0 uv p)
        (sample tex1 uv p)))))

(defn checkerboard
  "checkerboard solid texture"
  [& {:keys [tex0 tex1 scale]}]
  (->Checkerboard tex0 tex1 scale))

;;;
;;; Perlin noise

(defrecord PerlinNoise [scale]
  Texture
  (sample [this uv p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (noise (mat/mul scale p)))))))

(defn perlin-noise
  "Perlin noise"
  [& {:keys [scale]}]
  (->PerlinNoise scale))

;;;
;;; Perlin turbulence

(defrecord PerlinTurbulence [scale depth]
  Texture
  (sample [this uv p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (turbulence (mat/mul scale p) depth))))))

(defn perlin-turbulence
  "Perlin turbulence function"
  [& {:keys [scale depth]}]
  (->PerlinTurbulence scale depth))

;;;
;;; Marble

(defrecord Marble [scale depth]
  Texture
  (sample [this uv p]
    (mat/mul (vec3 1 1 1)
             (* 0.5 (inc (Math/sin (+ (* scale (mat/mget p 2))
                                      (* 10.0 (turbulence p depth)))))))))

(defn marble
  "Perlin turbulence shaped as a marble texture"
  [& {:keys [scale depth]}]
  (->Marble scale depth))

;;;
;;; Flip texture coordinates in U and V

(defrecord FlipTextureU [tex]
  Texture
  (sample [this [u v] p]
    (sample tex [(- 1.0 u) v] p)))

(defn flip-texture-u
  "flip texture coordinate in u"
  [& {:keys [tex]}]
  (->FlipTextureU tex))

(defrecord FlipTextureV [tex]
  Texture
  (sample [this [u v] p]
    (sample tex [u (- 1.0 v)] p)))

(defn flip-texture-v
  "flip texture coordinate in v"
  [& {:keys [tex]}]
  (->FlipTextureV tex))

;;;
;;; Image texture map

(defrecord ImageMap [image]
  Texture
  (sample [this [u v] p]
    (let [i (int (* u (width image)))
          j (int (* v (height image)))
          rgb (components-rgb (get-pixel image i j))
          color (map #(/ % 255.0) rgb)]
      color)))

(defn image-map
  "construct an image texture from a filename"
  [& {:keys [filename]}]
  (->ImageMap (load-image filename)))
