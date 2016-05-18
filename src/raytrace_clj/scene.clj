(ns raytrace-clj.scene
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer [vec3]]
            [raytrace-clj.shader :refer [->lambertian ->metal ->dielectric]]
            [raytrace-clj.hitable :refer [->sphere]]
))

(defn make-random-scene
  "make a random scene"
  []
  (concat
   ;; first create the hero objects
   (list
    (->sphere (vec3 0 -1000 0) 1000 (->lambertian (vec3 0.5 0.5 0.5))) ; ground
    (->sphere (vec3  0 1 0) 1 (->dielectric 1.5))
    (->sphere (vec3 -4 1 0) 1 (->lambertian (vec3 0.4 0.2 0.1)))
    (->sphere (vec3  4 1 0) 1 (->metal (vec3 0.7 0.6 0.5) 0.0)))
   ;; then add the random ones
   (for [a (range -11 11)
         b (range -11 11)
         :let [center (vec3 (+ a (* 0.9 (rand)))
                            0.2
                            (+ b (* 0.9 (rand))))
               choose-mat (rand)]
         :when (> (mat/length (mat/sub center (vec3 4 0.2 0))) 0.9)]
     (cond
       ;; 80% are diffuse
       (< choose-mat 0.8)
       (->sphere center 0.2 (->lambertian (vec3 (* (rand) (rand))
                                                (* (rand) (rand))
                                                (* (rand) (rand)))))
       ;; 15% are metal
       (< choose-mat 0.95)
       (->sphere center 0.2 (->metal (vec3 (* 0.5 (+ 1 (rand)))
                                           (* 0.5 (+ 1 (rand)))
                                           (* 0.5 (+ 1 (rand))))
                                     (* 0.5 (rand))))
       :else ; the last 5% are glass
       (->sphere center 0.2 (->dielectric 1.5))))))
