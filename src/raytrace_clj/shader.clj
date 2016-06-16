(ns raytrace-clj.shader
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.texture :refer :all]
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
    (if (pos? discriminant)
      (mat/sub (mat/mul ni-over-nt
                        (mat/sub uv (mat/mul n dt)))
               (mat/mul n (Math/sqrt discriminant))))))

(defprotocol Shader
  (scatter [this ray-in hrec])
  (emitted [this uv p]))

;;;
;;; Lambertian shader

(defrecord Lambertian [albedo]
  Shader
  (scatter [this ray-in {:keys [t p uv normal material]}]
    (let [target (mat/add p normal (rand-in-unit-sphere))]
      {:scattered (ray p (mat/sub target p) (:time ray-in))
       :attenuation (sample albedo uv p)}))
  (emitted [this uv p]
    (vec3 0 0 0)))

(defn lambertian
  "make a lambertian shader"
  [& {:keys [albedo]}]
  (->Lambertian albedo))

;;;
;;; Metal shader

(defrecord Metal [albedo fuzz]
  Shader
  (scatter [this ray-in {:keys [t p uv normal material]}]
    (let [reflected (reflect (mat/normalise (:direction ray-in)) normal)
          scattered (ray p
                         (mat/add reflected
                                  (mat/mul fuzz
                                           (rand-in-unit-sphere)))
                         (:time ray-in))]
      (if (pos? (mat/dot (:direction scattered) normal))
        {:scattered scattered
         :attenuation (sample albedo uv p)})))
  (emitted [this uv p]
    (vec3 0 0 0)))

(defn metal
  "make a metal shader"
  [& {:keys [albedo fuzz]}]
  (->Metal albedo fuzz))

;;;
;;; Dielectric / glass shader

(defn schlick
  "polynomial approximation of glass reflectivity"
  [cosine ri]
  (let [r0 (/ (- 1.0 ri) (+ 1.0 ri))
        r0 (* r0 r0)]
    (+ r0 (* (- 1.0 r0) (Math/pow (- 1.0 cosine) 5)))))

(defrecord Dielectric [ri]
  Shader
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [ray-direction (:direction ray-in)
          ray-dot-n     (mat/dot ray-direction normal)
          [outward-normal ni-over-nt cosine]
          (if (pos? ray-dot-n)
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
         :attenuation (vec3 1 1 1)})))
  (emitted [this uv p]
    (vec3 0 0 0)))

(defn dielectric
  "make a dielectric shader"
  [& {:keys [ri]}]
  (->Dielectric ri))

;;;
;;; Diffuse light

(defrecord DiffuseLight [tex]
  Shader
  (scatter [this ray-in hrec]
    nil)
  (emitted [this uv p]
    (sample tex uv p)))

(defn diffuse-light
  "make a diffuse light"
  [& {:keys [tex]}]
  (->DiffuseLight tex))

;;;
;;; Isotropic reflector (for participating media)

(defrecord Isotropic [albedo]
  Shader
  (scatter [this ray-in hrec]
    (let [p (:p hrec)
          uv (:uv hrec)
          t (:t hrec)]
      {:scattered (ray p (rand-in-unit-sphere) t)
       :attenuation (sample albedo uv p)}))
  (emitted [this uv p]
    (vec3 0 0 0)))

(defn isotropic
  "make an isotropic material"
  [& {:keys [albedo]}]
  (->Isotropic albedo))
