(ns raytrace-clj.scene
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer [vec3]]
            [raytrace-clj.texture
             :refer [->constant-texture ->checkerboard-texture
                     ->noise-texture ->turbulence-texture]]
            [raytrace-clj.shader :refer [->lambertian ->metal ->dielectric]]
            [raytrace-clj.hitable :refer [->sphere ->moving-sphere]]))


(defn make-two-spheres
  "two touching spheres"
  []
  (let [checker (->checkerboard-texture
                 (->constant-texture (vec3 0.2 0.3 0.1))
                 (->constant-texture (vec3 0.9 0.9 0.9))
                 10)]
    (list (->sphere (vec3 0 -10 0) 10 (->lambertian checker))
          (->sphere (vec3 0  10 0) 10 (->lambertian checker)))))


(defn make-two-perlin-spheres
  "two touching spheres"
  []
  (let [perlin (->turbulence-texture 4 7)]
    (list (->sphere (vec3 0 -1000 0) 1000 (->lambertian perlin))
          (->sphere (vec3 0     2 0)    2 (->lambertian perlin)))))


(defn make-random-scene
  "make a random scene"
  [n moving]
  (concat
   ;; first create the hero objects
   (list
    (->sphere (vec3 0 -1000 0) 1000
              (->lambertian
               (->checkerboard-texture
                (->constant-texture (vec3 0.2 0.3 0.1))
                (->constant-texture (vec3 0.9 0.9 0.9))
                10))) ; ground
    (->sphere (vec3  0 1 0) 1
              (->dielectric 1.5))
    (->sphere (vec3 -4 1 0) 1
              (->lambertian
               (->constant-texture (vec3 0.4 0.2 0.1))))
    (->sphere (vec3  4 1 0) 1
              (->metal
               (->constant-texture (vec3 0.7 0.6 0.5)) 0.0)))
   ;; then add the random ones
   (for [a (range (- n) n)
         b (range (- n) n)
         :let [center (vec3 (+ a (* 0.9 (rand)))
                            0.2
                            (+ b (* 0.9 (rand))))
               choose-mat (rand)]
         :when (> (mat/magnitude (mat/sub center (vec3 4 0.2 0))) 0.9)]
     (cond
       ;; 80% are diffuse
       (< choose-mat 0.8)
       (if moving
         (->moving-sphere center 0.0
                          (mat/add center (vec3 0 (* 0.5 (rand)) 0)) 1.0
                          0.2
                          (->lambertian
                           (->constant-texture (vec3 (* (rand) (rand))
                                                     (* (rand) (rand))
                                                     (* (rand) (rand))))))
         (->sphere center 0.2
                   (->lambertian
                    (->constant-texture (vec3 (* (rand) (rand))
                                              (* (rand) (rand))
                                              (* (rand) (rand)))))))
       ;; 15% are metal
       (< choose-mat 0.95)
       (->sphere center 0.2
                 (->metal
                  (->constant-texture (vec3 (* 0.5 (inc (rand)))
                                            (* 0.5 (inc (rand)))
                                            (* 0.5 (inc (rand)))))
                  (* 0.5 (rand))))
       :else ; the last 5% are glass
       (->sphere center 0.2 (->dielectric 1.5))))))
