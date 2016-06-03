(ns raytrace-clj.scene
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer [vec3]]
            [raytrace-clj.texture
             :refer [->constant-texture
                     ->checkerboard-texture
                     ->noise-texture
                     ->turbulence-texture
                     ->marble-texture
                     ->flip-texture-coord-u
                     ->flip-texture-coord-v
                     ->image-texture
                     make-image-texture]]
            [raytrace-clj.shader
             :refer [->lambertian
                     ->metal
                     ->dielectric
                     ->diffuse-light]]
            [raytrace-clj.hitable
             :refer [->sphere
                     ->moving-sphere
                     ->rect-xy
                     ->rect-yz
                     ->rect-xz
                     ->flip-normals
                     make-box
                     ->translate
                     make-rotate-y
                     make-constant-medium
                     make-bvh]]))


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

(defn make-textured-sphere
  "texture mapped sphere"
  []
  (let [checker (->checkerboard-texture
                 (->constant-texture (vec3 0.2 0.3 0.1))
                 (->constant-texture (vec3 0.9 0.9 0.9))
                 ;(->marble-texture 4 5)
                 10)]
    (list (->sphere (vec3 0 -10 0) 10
                    (->lambertian checker))
          (->sphere (vec3 0 1.0 0) 1.0
                    (->lambertian
                     (->flip-texture-coord-v
                      (make-image-texture "earth.png")))))))

(defn make-subsurface-sphere
  "subsurface reflection sphere"
  []
  (let [checker (->checkerboard-texture
                 (->constant-texture (vec3 0.2 0.3 0.1))
                 (->constant-texture (vec3 0.9 0.9 0.9))
                 ;(->marble-texture 4 5)
                 10)]
    (list (->sphere (vec3 0 -10 0) 10
                    (->lambertian checker))
          (->sphere (vec3 0 0 0) 1000
                    (->diffuse-light
                     (->constant-texture
                      (mat/mul 2.0 (vec3 0.15 0.25 0.35)))))
          (make-constant-medium
           (->sphere (vec3 0 1.0 0) 1.0
                     (->dielectric 1.5))
           0.9
           (->constant-texture (vec3 0.2 0.4 0.9))))))

(defn make-example-light
  "scene with rectangular area light"
  []
  (let [perlin (->lambertian (->turbulence-texture 4 3))
        gray  (->lambertian (->constant-texture (vec3 0.6 0.6 0.6)))
        light (->diffuse-light (->constant-texture (vec3 4 4 4)))]
    (list (->sphere (vec3 0 -1000 0) 1000 gray)
          (->sphere (vec3 0     2 0)    2 gray)
          (->sphere (vec3 0     7 0)    2 light)
          (->sphere (vec3 0     0 0) 1000
                    (->diffuse-light
                     (->constant-texture
                      (mat/mul 0.15 (vec3 0.3 0.5 0.8)))))
          (->rect-xy 3 1 5 3 -2 light))))

(defn make-final
  "book 2 final example"
  []
  (let [white  (->lambertian (->constant-texture (vec3 0.73 0.73 0.73)))
        ground (->lambertian (->constant-texture (vec3 0.48 0.83 0.53)))
        orange (->lambertian (->constant-texture (vec3 0.7 0.3 0.1)))
        light  (->diffuse-light (->constant-texture (vec3 7 7 7)))
        glass  (->dielectric 1.5)
        metal  (->metal (->constant-texture (vec3 0.8 0.8 0.9)) 10)
        bndry  (->sphere (vec3 360 150 145) 70 glass)
        earth  (->lambertian (->flip-texture-coord-v
                              (make-image-texture "earth.png")))
        marble (->lambertian (->noise-texture 0.1))
        nb 20
        ns 1000]
    (list
     ;; ground is a grid of boxes
     (make-bvh
      (for [i (range nb)
            j (range nb)]
        (let [w 100
              p0 (vec3 (+ -1000 (* i w)) 0 (+ -1000 (* j w)))
              p1 (mat/add p0 (vec3 w (* 100 (+ (rand) 0.01)) w))]
          (make-box p0 p1 ground)))
      0.0 1.0)
     ;; overhead light
     (->rect-xz 123 147 423 412 554 light)
     ;; orange moving sphere
     (->moving-sphere (vec3 400 400 200) 0 (vec3 430 400 200) 1 50 orange)
     ;; glass sphere
     (->sphere (vec3 260 150 45) 50 glass)
     ;; metal sphere
     (->sphere (vec3 0 150 145) 50 metal)
     ;; glass with subsurface scattering
     bndry
     (make-constant-medium bndry 0.2 (->constant-texture (vec3 0.2 0.4 0.9)))
     ;; overall haze
     (make-constant-medium (->sphere (vec3 0 0 0) 5000 glass)
                           0.0001
                           (->constant-texture (vec3 1 1 1)))
     ;; earth
     (->sphere (vec3 400 200 400) 100 earth)
     ;; marble
     (->sphere (vec3 220 280 300) 80 marble)
     ;; spheres packed in a box
     (->translate
      (make-rotate-y
       (make-bvh
        (for [n (range ns)]
          (->sphere (mat/mul 165.0 (vec3 (rand) (rand) (rand))) 10 white))
        0.0 1.0)
       15)
      (vec3 -100 270 395)))))

(defn make-cornell-box
  "classic cornell box"
  []
  (let [red   (->lambertian (->constant-texture (vec3 0.65 0.05 0.05)))
        white (->lambertian (->constant-texture (vec3 0.73 0.73 0.73)))
        green (->lambertian (->constant-texture (vec3 0.12 0.45 0.15)))
        light (->diffuse-light (->constant-texture (vec3 7 7 7)))]
    (list (->flip-normals (->rect-yz   0   0 555 555 555 green))
                          (->rect-yz   0   0 555 555   0 red)
                          ;(->rect-xz 213 227 343 332 554 light)
                          (->rect-xz 113 127 443 432 554 light)
          (->flip-normals (->rect-xz   0   0 555 555 555 white))
                          (->rect-xz   0   0 555 555   0 white)
          (->flip-normals (->rect-xy   0   0 555 555 555 white))
          (make-constant-medium
           (->translate (make-rotate-y
                         (make-box (vec3 0 0 0)
                                   (vec3 165 165 165)
                                   white)
                         -18.0)
                        (vec3 130 0 65))
           0.01
           (->constant-texture (vec3 1 1 1)))
          (make-constant-medium
           (->translate (make-rotate-y
                         (make-box (vec3 0 0 0)
                                   (vec3 165 330 165)
                                   white)
                         15.0)
                        (vec3 265 0 295))
           0.01
           (->constant-texture (vec3 0 0 0))))))

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
