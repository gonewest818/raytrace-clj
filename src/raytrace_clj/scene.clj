(ns raytrace-clj.scene
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer [vec3]]
            [raytrace-clj.camera :as cam]
            [raytrace-clj.texture :as tex]
            [raytrace-clj.shader :as shad]
            [raytrace-clj.hitable :as hit]))

(defn make-two-spheres
  "two touching spheres"
  [nx ny]
  (let [checker
        (tex/checkerboard :tex0 (tex/constant :color (vec3 0.2 0.3 0.1))
                          :tex1 (tex/constant :color (vec3 0.9 0.9 0.9))
                          :scale 10)]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 13 2 3)
              :lookat (vec3 0 1 0)
              :vup (vec3 0 1 0)
              :vfov 40
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/uv-sphere :center (vec3 0 0 0)
                          :radius 1000
                          :material (shad/diffuse-light
                                     :tex (tex/uv-gradient
                                           :co (vec3 1 1 1)
                                           :cu (vec3 1 1 1)
                                           :cv (vec3 0.5 0.7 1.0)
                                           :cuv (vec3 0.5 0.7 1.0))))
              (hit/sphere :center (vec3 0 -10 0)
                          :radius 10
                          :material (shad/lambertian :albedo checker))
              (hit/uv-sphere :center (vec3 0 2 0)
                             :radius 2
                             :material  (shad/lambertian
                                        ; :albedo checker
                                         :albedo (tex/uv-gradient
                                                  :co (vec3 0 1 0)
                                                  :cu (vec3 0 1 1)
                                                  :cv (vec3 1 0 1)
                                                  :cuv (vec3 1 0 0)))))
             0.0 1.0)}))

(defn make-two-perlin-spheres
  "two perlin noise spheres"
  [nx ny]
  (let [turb (tex/perlin-turbulence :scale 4 :depth 7)]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 13 2 3)
              :lookat (vec3 0 1 0)
              :vup (vec3 0 1 0)
              :vfov 40
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/sphere :center (vec3 0 0 0)
                          :radius 1000
                          :material (shad/diffuse-light
                                     :tex (tex/constant
                                           :color (mat/mul 0.8
                                                           (vec3 0.3 0.5 0.8)))))
              (hit/sphere :center (vec3 0 -1000 0)
                          :radius 1000
                          :material (shad/lambertian :albedo turb))
              (hit/sphere :center (vec3 0 2 0)
                          :radius 2
                          :material (shad/lambertian :albedo turb)))
             0.0 1.0)}))

(defn make-two-triangles
  "two triangles, view down the -z axis"
  [nx ny]
  (let [white (tex/constant :color (vec3 0.9 0.9 0.9))
        red (tex/constant :color (vec3 0.9 0 0))]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 1 1 -10)
              :lookat (vec3 1 1 0)
              :vup (vec3 0 1 0)
              :vfov 20
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/sphere
               :center (vec3 0 0 0)
               :radius 1000
               :material(shad/diffuse-light
                         :tex (tex/constant
                               :color (mat/mul 0.8
                                               (vec3 0.3 0.5 0.8)))))
              (hit/triangle
               :v0 (vec3 0 0 0)
               :v1 (vec3 0 1 0)
               :v2 (vec3 1 0 0)
               :material (shad/lambertian :albedo red))
              (hit/triangle
               :v0 (vec3 1 1 0)
               :v1 (vec3 1 2 0)
               :v2 (vec3 2 1 0)
               :material (shad/lambertian :albedo white)))
             0.0 1.0)}))

(defn make-textured-sphere
  "texture mapped sphere"
  [nx ny]
  (let [checker (tex/checkerboard
                 :tex0 (tex/constant :color (vec3 0.2 0.3 0.1))
                 :tex1 (tex/constant :color (vec3 0.9 0.9 0.9))
                      ;(tex/marble :scale 4 :depth 5)
                 :scale 10)
        earth  (tex/flip-texture-v 
                :tex (tex/image-map :filename "earth.png"))]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 13 2 3)
              :lookat (vec3 0 1 0)
              :vup (vec3 0 1 0)
              :vfov 15
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/sphere :center (vec3 0 0 0)
                          :radius 1000
                          :material (shad/diffuse-light
                                     :tex (tex/constant
                                           :color (mat/mul 0.8
                                                           (vec3 0.3 0.5 0.8)))))
              (hit/sphere :center (vec3 0 -10 0)
                          :radius 10
                          :material (shad/lambertian :albedo checker))
              (hit/uv-sphere :center (vec3 0 1 0)
                             :radius 1
                             :material (shad/lambertian :albedo earth)))
             0.0 1.0)}))

(defn make-subsurface-sphere
  "subsurface reflection sphere"
  [nx ny]
  (let [checker (tex/checkerboard
                 :tex0 (tex/constant :color (vec3 0.2 0.3 0.1))
                 :tex1 (tex/constant :color (vec3 0.9 0.9 0.9))
                 :scale 10)
        ball (hit/sphere :center (vec3 0 2 0)
                         :radius 2
                         :material (shad/dielectric :ri 1.5))
        medium (hit/constant-medium
                :boundary ball
                :density 0.9
                :albedo (tex/constant :color (vec3 0.2 0.4 0.9)))]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 13 2 3)
              :lookat (vec3 0 1 0)
              :vup (vec3 0 1 0)
              :vfov 40
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/sphere :center (vec3 0 0 0)
                          :radius 1000
                          :material (shad/diffuse-light
                                     :tex (tex/constant
                                           :color (mat/mul 0.8
                                                           (vec3 0.3 0.5 0.8)))))
              (hit/sphere :center (vec3 0 -10 0)
                          :radius 10
                          :material (shad/lambertian :albedo checker))
              medium
              ball)
             0.0 1.0)}))

(defn make-example-light
  "scene with rectangular area light"
  [nx ny]
  (let [perlin (shad/lambertian :albedo (tex/perlin-turbulence :scale 4 :depth 3))
        gray (shad/lambertian :albedo (tex/constant :color (vec3 0.6 0.6 0.6)))
        light (shad/diffuse-light :tex (tex/constant :color (vec3 4 4 4)))]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 13 2 3)
              :lookat (vec3 0 1 0)
              :vup (vec3 0 1 0)
              :vfov 40
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/sphere
               :center (vec3 0 -1000 0)
               :radius 1000
               :material gray)
              (hit/sphere
               :center (vec3 0 2 0)
               :radius 2
               :material gray)
              (hit/sphere
               :center (vec3 0 7 0)
               :radius 2
               :material light)
              ;; (hit/sphere
              ;;  :center (vec3 0 0 0)
              ;;  :radius 1000
              ;;  :material (shad/diffuse-light
              ;;             :tex (tex/constant
              ;;                   :color (mat/mul 0.15 (vec3 0.3 0.5 0.8)))))
              (hit/rect-xy :x0 3 :y0 1 :x1 5 :y1 3 :k -2 :material light))
             0.0 1.0)}))

(defn make-cornell-box
  "classic cornell box"
  [nx ny classic]
  (let [red   (shad/lambertian :albedo (tex/constant :color (vec3 0.65 0.05 0.05)))
        white (shad/lambertian :albedo (tex/constant :color (vec3 0.73 0.73 0.73)))
        green (shad/lambertian :albedo (tex/constant :color (vec3 0.12 0.45 0.15)))
        light (shad/diffuse-light :tex (tex/constant :color (vec3 7 7 7)))]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 278 278 -800)
              :lookat (vec3 278 278 0)
              :vup (vec3 0 1 0)
              :vfov 40
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              (hit/flip-normals
               :item (hit/rect-yz :y0 0 :z0 0
                                  :y1 555 :z1 555 :k 555
                                  :material green))
              (hit/rect-yz :y0 0 :z0 0
                           :y1 555 :z1 555 :k 0
                           :material red)
              (if classic
                ;; small light
                (hit/rect-xz :x0 213 :z0 227
                             :x1 343 :z1 332 :k 554
                             :material light)
                ;; bigger light
                (hit/rect-xz :x0 113 :z0 127
                             :x1 443 :z1 432 :k 554
                             :material light))
              (hit/flip-normals
               :item (hit/rect-xz :x0 0 :z0 0
                                  :x1 555 :z1 555 :k 555
                                  :material white))
              (hit/rect-xz :x0 0 :z0 0
                           :x1 555 :z1 555 :k 0
                           :material white)
              (hit/flip-normals
               :item (hit/rect-xy :x0 0 :y0 0
                                  :x1 555 :y1 555 :k 555
                                  :material white))
              (if classic
                ;; solid box
                (hit/translate
                 :item (hit/rotate-y
                        :item (hit/box :p0 (vec3 0 0 0)
                                       :p1 (vec3 165 165 165)
                                       :material white)
                        :theta -18.0)
                 :offset (vec3 130 0 65))
                ;; foggy box
                (hit/constant-medium
                 :boundary (hit/translate
                            :item (hit/rotate-y
                                   :item (hit/box :p0 (vec3 0 0 0)
                                                  :p1 (vec3 165 165 165)
                                                  :material white)
                                   :theta -18.0)
                            :offset (vec3 130 0 65))
                 :density 0.01
                 :albedo (tex/constant :color (vec3 1 1 1))))
              (if classic
                ;; solid box
                (hit/translate
                 :item (hit/rotate-y
                        :item (hit/box :p0 (vec3 0 0 0)
                                       :p1 (vec3 165 330 165)
                                       :material white)
                        :theta 15.0)
                 :offset (vec3 265 0 295))
                ;; foggy box
                (hit/constant-medium
                 :boundary (hit/translate
                            :item (hit/rotate-y
                                   :item (hit/box :p0 (vec3 0 0 0)
                                                  :p1 (vec3 165 330 165)
                                                  :material white)
                                   :theta 15.0)
                            :offset (vec3 265 0 295))
                 :density 0.01
                 :albedo (tex/constant :color (vec3 0 0 0)))))
             0.0 1.0 )}))

  (defn make-random-scene
    "make a random scene"
    [nx ny n moving]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 13 2 3)
              :lookat (vec3 0 0 0)
              :vup (vec3 0 1 0)
              :vfov 20
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world
     (hit/make-bvh 
      (concat
       ;; first create the hero objects
       (list
        (hit/uv-sphere                  ; sky dome
         :center (vec3 0 0 0)
         :radius 1000
         :material (shad/diffuse-light
                    :tex (tex/uv-gradient
                          :co (vec3 1 1 1)
                          :cu (vec3 1 1 1)
                          :cv (vec3 0.5 0.7 1.0)
                          :cuv (vec3 0.5 0.7 1.0))))
        (hit/sphere                     ; ground
         :center (vec3 0 -1000 0)
         :radius  1000
         :material (shad/lambertian
                    :albedo (tex/checkerboard
                     :tex0 (tex/constant :color (vec3 0.2 0.3 0.1))
                     :tex1 (tex/constant :color (vec3 0.9 0.9 0.9))
                     :scale 10)))
        (hit/sphere                     ; glass
         :center (vec3  0 1 0)
         :radius 1
         :material (shad/dielectric :ri 1.5))
        (hit/sphere                     ; plastic
         :center (vec3 -4 1 0)
         :radius 1
         :material (shad/lambertian
                    :albedo (tex/constant :color (vec3 0.4 0.2 0.1))))
        (hit/sphere                     ; metal
         :center (vec3  4 1 0)
         :radius  1
         :material (shad/metal
                    :albedo (tex/constant :color (vec3 0.7 0.6 0.5))
                    :fuzz 0.0)))
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
             (hit/moving-sphere
              :center0 center
              :t0 0.0
              :center1 (mat/add center (vec3 0 (* 0.5 (rand)) 0)) 
              :t1 1.0
              :radius 0.2
              :material (shad/lambertian
                         :albedo (tex/constant :color (vec3 (* (rand) (rand))
                                                            (* (rand) (rand))
                                                            (* (rand) (rand))))))
             (hit/sphere
              :center center
              :radius 0.2
              :material (shad/lambertian
                         :albedo (tex/constant :color (vec3 (* (rand) (rand))
                                                            (* (rand) (rand))
                                                            (* (rand) (rand)))))))
           ;; 15% are metal
           (< choose-mat 0.95)
           (hit/sphere
            :center center
            :radius 0.2
            :material (shad/metal
                       :albedo (tex/constant :color (vec3 (* 0.5 (inc (rand)))
                                                          (* 0.5 (inc (rand)))
                                                          (* 0.5 (inc (rand)))))
                       :fuzz (* 0.5 (rand))))
           :else ; the last 5% are glass
           (hit/sphere
            :center center
            :radius 0.2
            :material (shad/dielectric :ri 1.5)))))
      0.0 1.0)})


(defn make-final
  "book 2 final example"
  [nx ny]
  (let [white  (shad/lambertian :albedo (tex/constant :color (vec3 0.73 0.73 0.73)))
        ground (shad/lambertian :albedo (tex/constant :color (vec3 0.48 0.83 0.53)))
        orange (shad/lambertian :albedo (tex/constant :color (vec3 0.7 0.3 0.1)))
        light  (shad/diffuse-light :tex (tex/constant :color (vec3 7 7 7)))
        glass  (shad/dielectric :ri 1.5)
        metal  (shad/metal :albedo (tex/constant :color (vec3 0.8 0.8 0.9)) :fuzz 10)
        bndry  (hit/sphere :center (vec3 360 150 145) :radius 70 :material glass)
        earth  (shad/lambertian :albedo (tex/flip-texture-v
                                         :tex (tex/image-map :filename "earth.png")))
        marble (shad/lambertian :albedo (tex/marble :scale 0.1 :depth 4))
        nb 20
        ns 1000]
    {:camera (cam/thin-lens-camera
              :lookfrom (vec3 478 278 -600)
              :lookat (vec3 278 278 0)
              :vup (vec3 0 1 0)
              :vfov 40
              :aspect (/ (float nx) (float ny))
              :aperture 0.0
              :focus-dist 10.0
              :t0 0.0
              :t1 1.0)
     :world (hit/make-bvh
             (list
              ;; ground is a grid of boxes
              (hit/make-bvh
               (for [i (range nb)
                     j (range nb)]
                 (let [w 100
                       p0 (vec3 (+ -1000 (* i w)) 0 (+ -1000 (* j w)))
                       p1 (mat/add p0 (vec3 w (* 100 (+ (rand) 0.01)) w))]
                   (hit/box :p0 p0 :p1 p1 :material ground)))
               0.0 1.0)
              ;; overhead light
              (hit/rect-xz :x0 123 :z0 147 :x1 423 :z1 412 :k 554 :material light)
              ;; orange moving sphere
              (hit/moving-sphere :center0 (vec3 400 400 200) :t0 0
                                 :center1 (vec3 430 400 200) :t1 1
                                 :radius 50
                                 :material orange)
              ;; glass sphere
              (hit/sphere :center (vec3 260 150 45) :radius 50 :material glass)
              ;; metal sphere
              (hit/sphere :center (vec3 0 150 145) :radius 50 :material metal)
              ;; glass with subsurface scattering
              bndry
              (hit/constant-medium :boundary bndry
                                   :density 0.2
                                   :albedo (tex/constant :color (vec3 0.2 0.4 0.9)))
              ;; overall haze
              (hit/constant-medium :boundary (hit/sphere :center (vec3 0 0 0)
                                                         :radius 5000
                                                         :material glass)
                                   :density 0.0001
                                   :albedo (tex/constant :color (vec3 1 1 1)))
              ;; earth
              (hit/uv-sphere :center (vec3 400 200 400) :radius 100 :material earth)
              ;; marble
              (hit/sphere :center (vec3 220 280 300) :radius 80 :material marble)
              ;; spheres packed in a box
              (hit/translate
               :item (hit/rotate-y
                      :item (hit/make-bvh
                             (for [n (range ns)]
                               (hit/sphere
                                :center (mat/mul 165.0 (vec3 (rand) (rand) (rand)))
                                :radius 10
                                :material white))
                             0.0 1.0)
                      :theta 15)
               :offset (vec3 -100 270 395)))
             0.0 1.0)}))


  
