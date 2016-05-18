(ns raytrace-clj.camera
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))

(defprotocol camera
  (get-ray [this u v]))

(defrecord pinhole-camera [origin lleft horiz vert]
  camera
  (get-ray [this u v]
    (ray origin 
         (mat/add lleft
                  (mat/mul u horiz)
                  (mat/mul v vert)
                  (mat/negate origin))
         0)))

(defn make-pinhole-camera
  "make a pinhole camera based on field of view and aspect ratio"
  [lookfrom lookat vup vfov aspect]
  (let [theta       (* vfov (/ Math/PI 180.0))
        half-height (Math/tan (/ theta 2.0))
        half-width  (* aspect half-height)
        w           (mat/normalise (mat/sub lookfrom lookat))
        u           (mat/normalise (mat/cross vup w))
        v           (mat/cross w u)]
    (pinhole-camera. lookfrom
                     (mat/sub lookfrom
                              (mat/add (mat/mul half-width u)
                                       (mat/mul half-height v)
                                       w))
                     (mat/mul 2.0 half-width u)
                     (mat/mul 2.0 half-height v))))

(defrecord thin-lens-camera [origin lleft horiz vert u v w aperture t0 t1]
  camera
  (get-ray [this s t]
    (let [lens-radius (/ aperture 2.0)
          rd          (mat/mul lens-radius (rand-in-unit-disk))
          offset      (mat/add (mat/mul u (mat/mget rd 0))
                               (mat/mul v (mat/mget rd 1)))]
      (ray (mat/add origin offset)
           (mat/add lleft
                    (mat/mul s horiz)
                    (mat/mul t vert)
                    (mat/negate origin)
                    (mat/negate offset))
           (+ t0 (* (- t1 t0) (rand)))))))

(defn make-thin-lens-camera
  "make a thin-lens camera based on aperture and focus distance"
  [lookfrom lookat vup vfov aspect aperture focus-dist t0 t1]
  (let [theta       (* vfov (/ Math/PI 180.0))
        half-height (Math/tan (/ theta 2.0))
        half-width  (* aspect half-height)
        w           (mat/normalise (mat/sub lookfrom lookat))
        u           (mat/normalise (mat/cross vup w))
        v           (mat/cross w u)]
    (thin-lens-camera. lookfrom
                       (mat/sub lookfrom
                                (mat/add (mat/mul focus-dist half-width u)
                                         (mat/mul focus-dist half-height v)
                                         (mat/mul focus-dist w)))
                       (mat/mul 2.0 focus-dist half-width u)
                       (mat/mul 2.0 focus-dist half-height v)
                       u v w aperture t0 t1)))
