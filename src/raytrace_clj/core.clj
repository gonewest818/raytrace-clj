(ns raytrace-clj.core
  (:require [clojure.core.matrix :as mat]
            [mikera.image.core :refer [new-image set-pixel width height show save]]
            [mikera.image.colours :refer [rgb-from-components]])
  (:import [org.imgscalr Scalr])
  (:gen-class))

(mat/set-current-implementation :vectorz)

(defn vec3
  "convenience function to make 3d vectors"
  [u v w]
  (mat/matrix [u v w]))

(defn ray
  "simple model for a ray (note: direction is not normalized))"
  [a b]
  {:origin a :direction b})

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

(defn point-at-parameter
  "evaluate parameterized ray at position t"
  [ray t]
  (mat/add (:origin ray)
           (mat/mul (:direction ray) t)))

(defn rand-in-unit-disk
  "randomly choose point within the unit disk"
  []
  (defn square [] 
    (vec3 (- (* 2.0 (rand)) 1.0) 
          (- (* 2.0 (rand)) 1.0)
          0))
  (loop [p (square)]
    (if (>= (mat/dot p p) 1.0) 
      (recur (square))
      p)))

(defn rand-in-unit-sphere
  "randomly choose point within the unit sphere"
  []
  (defn cube [] 
    (vec3 (- (* 2.0 (rand)) 1.0) 
          (- (* 2.0 (rand)) 1.0)
          (- (* 2.0 (rand)) 1.0)))
  (loop [p (cube)]
    (if (>= (mat/dot p p) 1.0) 
      (recur (cube))
      p)))

;; (defmacro dlet [bindings & body]
;;   `(let [~@(mapcat (fn [[n v]]
;;                        (if (or (vector? n) (map? n))
;;                            [n v]
;;                          [n v '_ `(println (name '~n) ":" ~v)]))
;;                    (partition 2 bindings))]
;;      ~@body))

(defprotocol hitable
  (hit? [this r t-min t-max]))

(defrecord hitlist [hitable-list]
  hitable
  (hit? [this r t-min t-max]
    (let [closest (reduce 
                   (fn [v obj] 
                     (if-let [hrec (hit? obj r t-min (:t v))]
                       hrec
                       v))
                   {:missed true :t t-max}
                   hitable-list)]
      (if (not (:missed closest))
        closest))))

(defrecord sphere [center radius material]
  hitable
  (hit? [this r t-min t-max]
    (let [oc (mat/sub (:origin r) center)
          a (mat/dot (:direction r) (:direction r))
          b (mat/mul 2.0 (mat/dot oc (:direction r)))
          c (- (mat/dot oc oc) (* radius radius))
          discriminant (- (* b b) (* 4.0 a c))]
      (if (> discriminant 0)
        (let [t (/ (- (- b) (Math/sqrt discriminant)) (* 2.0 a))
              p (point-at-parameter r t)]
          (if (and (> t t-min) (< t t-max))
            {:t t :p p 
             :normal (mat/div (mat/sub p center) radius)
             :material material}))))))

(defprotocol shader
  (scatter [this ray-in hrec]))

(defrecord lambertian [albedo]
  shader
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [target (mat/add p normal (rand-in-unit-sphere))]
      {:scattered (ray p (mat/sub target p)) 
       :attenuation albedo})))

(defrecord metal [albedo fuzz]
  shader
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [reflected (reflect (mat/normalise (:direction ray-in)) normal)
          scattered (ray p (mat/add reflected
                                    (mat/mul fuzz
                                             (rand-in-unit-sphere))))]
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
          {:scattered (ray p (reflect ray-direction normal))
           :attenuation (vec3 1 1 1)}
          ;; refracted
          {:scattered (ray p refr)
           :attenuation (vec3 1 1 1)})
        ;; otherwise reflected
        {:scattered (ray p (reflect ray-direction normal))
         :attenuation (vec3 1 1 1)}))))

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
          ;[ux uy uz] (mat/normalise (:direction r))
          t (* 0.5 (+ uy 1.0))]
      (mat/lerp (vec3 1.0 1.0 1.0) 
                (vec3 0.5 0.7 1.0)
                t))))

(defprotocol camera
  (get-ray [this u v]))

(defrecord pinhole-camera [origin lleft horiz vert]
  camera
  (get-ray [this u v]
    (ray origin 
         (mat/add lleft
                  (mat/mul u horiz)
                  (mat/mul v vert)
                  (mat/negate origin)))))

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

(defrecord thin-lens-camera [origin lleft horiz vert u v w aperture]
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
                    (mat/negate offset))))))

(defn make-thin-lens-camera
  "make a thin-lens camera based on aperture and focus distance"
  [lookfrom lookat vup vfov aspect aperture focus-dist]
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
                       u v w aperture)))

(defn make-random-scene
  "make a random scene"
  []
  (concat
   ;; hero objects
   (list
    (sphere. (vec3 0 -1000 0) 1000 (lambertian. (vec3 0.5 0.5 0.5))) ; ground
    (sphere. (vec3  0 1 0) 1 (dielectric. 1.5))
    (sphere. (vec3 -4 1 0) 1 (lambertian. (vec3 0.4 0.2 0.1)))
    (sphere. (vec3  4 1 0) 1 (metal. (vec3 0.7 0.6 0.5) 0.0)))
   ;; random ones
   (for [a (range -11 11)
         b (range -11 11)
         :let [center (vec3 (+ a (* 0.9 (rand)))
                            0.2
                            (+ b (* 0.9 (rand))))
               choose-mat (rand)]
         :when (> (mat/length (mat/sub center (vec3 4 0.2 0))) 0.9)]
     (cond
       (< choose-mat 0.8) ; diffuse
       (sphere. center 0.2 (lambertian. (vec3 (* (rand) (rand))
                                              (* (rand) (rand))
                                              (* (rand) (rand)))))
       (< choose-mat 0.95) ; metal
       (sphere. center 0.2 (metal. (vec3 (* 0.5 (+ 1 (rand)))
                                         (* 0.5 (+ 1 (rand)))
                                         (* 0.5 (+ 1 (rand))))
                                   (* 0.5 (rand))))
       :else ; glass
       (sphere. center 0.2 (dielectric. 1.5))))))

(defn show-progress
  [image j filename tstart]
  (let [elapsed-time   (/ (- (System/currentTimeMillis) tstart) 1000.0)
        nx (width image)
        ny (height image)
        zoom (min (Math/floorDiv 1280 nx) (Math/floorDiv 800 ny) 4)]
    (if (> zoom 1) 
      (show (Scalr/resize image
                          org.imgscalr.Scalr$Method/SPEED
                          org.imgscalr.Scalr$Mode/FIT_EXACT
                          (* zoom nx)
                          (* zoom ny)
                          nil)
            :title filename)
      (show image :title filename))
    (println (format "%10.3f seconds" elapsed-time) ":"
             (inc j) "/" ny "complete")))

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
        world (hitlist. (make-random-scene))]
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
