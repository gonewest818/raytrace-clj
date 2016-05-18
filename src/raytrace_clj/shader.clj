(ns raytrace-clj.shader
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))

(defn reflect
  "reflect incoming vector v around normal n"
  [v n]
  (mat/sub v (mat/mul 2.0 (mat/dot v n) n)))

(defn refract
  "refract incoming vector v around normal n"
  [v n ni-over-nt]
  (let [uv (mat/normalise v)
        dt (mat/dot uv n)
        discriminant (- 1.0 (* ni-over-nt ni-over-nt (- 1 (* dt dt))))]
    (if (> discriminant 0)
      (mat/sub (mat/mul ni-over-nt
                        (mat/sub uv (mat/mul n dt)))
               (mat/mul n (Math/sqrt discriminant))))))

(defprotocol shader
  (scatter [this ray-in hrec]))

(defrecord lambertian [albedo]
  shader
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [target (mat/add p normal (rand-in-unit-sphere))]
      {:scattered (ray p (mat/sub target p) (:time ray-in))
       :attenuation albedo})))

(defrecord metal [albedo fuzz]
  shader
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [reflected (reflect (mat/normalise (:direction ray-in)) normal)
          scattered (ray p
                         (mat/add reflected
                                  (mat/mul fuzz
                                           (rand-in-unit-sphere)))
                         (:time ray-in))]
      (if (> (mat/dot (:direction scattered) normal) 0)
        {:scattered scattered
         :attenuation albedo}))))

(defn schlick
  "polynomial approximation of glass reflectivity"
  [cosine ri]
  (let [r0 (/ (- 1.0 ri) (+ 1.0 ri))
        r0 (* r0 r0)]
    (+ r0 (* (- 1.0 r0) (Math/pow (- 1.0 cosine) 5)))))

(defrecord dielectric [ri]
  shader
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [ray-direction (:direction ray-in)
          ray-dot-n     (mat/dot ray-direction normal)
          [outward-normal ni-over-nt cosine]
          (if (> ray-dot-n 0)
            [(mat/negate normal)        ; outward-normal
             ri                         ; ni-over-nt
             (* ri (/ ray-dot-n         ; cosine
                      (mat/magnitude ray-direction)))]
            [normal                     ; outward-normal
             (/ 1.0 ri)                 ; ni-over-nt
             (- (/ ray-dot-n            ; cosine
                   (mat/magnitude ray-direction)))])]
      (if-let [refr (refract ray-direction outward-normal ni-over-nt)]
        ;; if refraction possible, then choose reflect or refract randomly
        (if (< (rand) (schlick cosine ri))
          ;; reflected
          {:scattered (ray p (reflect ray-direction normal) (:time ray-in))
           :attenuation (vec3 1 1 1)}
          ;; refracted
          {:scattered (ray p refr (:time ray-in))
           :attenuation (vec3 1 1 1)})
        ;; otherwise reflected
        {:scattered (ray p (reflect ray-direction normal) (:time ray-in))
         :attenuation (vec3 1 1 1)}))))
