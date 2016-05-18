(ns raytrace-clj.core
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]
            [raytrace-clj.hitable :refer :all]
            [raytrace-clj.shader :refer :all]
            [raytrace-clj.camera :refer :all]
            [raytrace-clj.scene :refer [make-random-scene]]
            [raytrace-clj.display :refer [show-progress]]
            [mikera.image.core :refer [new-image set-pixel save]]
            [mikera.image.colours :refer [rgb-from-components]])
  (:gen-class))

(mat/set-current-implementation :vectorz)

(defn color
  "compute color at pixel"
  [r world depth]
  (if-let [hrec (hit? world r 0.001 Float/MAX_VALUE)]
    ; hit, return scattered ray unless recursion depth too deep
    (if-let [scat (and (< depth 50) 
                       (scatter (:material hrec) r hrec))]
      (mat/mul (:attenuation scat) (color (:scattered scat) world (inc depth)))
      (vec3 0.0 0.0 0.0)) ; if depth too deep, just black
    ; else miss, return sky gradient
    (let [uy (mat/mget (mat/normalise (:direction r)) 1)
          t (* 0.5 (+ uy 1.0))]
      (mat/lerp (vec3 1.0 1.0 1.0) 
                (vec3 0.5 0.7 1.0)
                t))))

(defn -main
  [& [name ix iy is]]
  (let [tstart (System/currentTimeMillis)
        filename (if name name "render.png")
        nx (if ix (Integer/parseUnsignedInt ix) 200)
        ny (if iy (Integer/parseUnsignedInt iy) 100)
        ns (if is (Integer/parseUnsignedInt is) 100)
        image (new-image nx ny)
        lookfrom (vec3 13 2 3)
        lookat (vec3 0 0 0)
        camera (make-thin-lens-camera lookfrom
                                      lookat
                                      (vec3 0 1 0)
                                      20
                                      (/ (float nx) (float ny))
                                      0.1
                                      10.0)
        world (->hitlist (make-random-scene))]
    (doseq [j (range ny)
            i (range nx)]
      (let [[ir ig ib]
            (->> 
             (repeatedly ns #(vector (/ (+ (float i) (rand)) nx)
                                     (/ (+ (float j) (rand)) ny)))
             (pmap (fn [[u v]] (color (get-ray camera u v) world 0)))
             (reduce mat/add)
             (mat/mul (/ 1.0 ns))
             (mat/sqrt)
             (mat/mul 255.99)
             (mat/emap int)
             (seq))]    ; needed because vectorz/vector can't be destructured
        (set-pixel image i (- (dec ny) j) (rgb-from-components ir ig ib))
        (if (= i (dec nx)) (show-progress image j filename tstart))))
    (save image filename)
    (println "wrote" filename)))
