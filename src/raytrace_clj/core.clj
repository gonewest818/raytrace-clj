(ns raytrace-clj.core
  (:require [clojure.core.matrix :as mat])
  (:gen-class))


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

(defn point-at-parameter
  "evaluate parameterized ray at position t"
  [ray t]
  (mat/add (:origin ray)
           (mat/mul (:direction ray) t)))

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

(defprotocol scatterable
  (scatter [this ray-in hrec]))

(defrecord lambertian [albedo]
  scatterable
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [target (mat/add p normal (rand-in-unit-sphere))]
      {:scattered (ray p (mat/sub target p)) 
       :attenuation albedo})))

(defrecord metal [albedo fuzz]
  scatterable
  (scatter [this ray-in {:keys [t p normal material]}]
    (let [reflected (reflect (mat/normalise (:direction ray-in)) normal)
          scattered (ray p (mat/add reflected
                                    (mat/mul fuzz
                                             (rand-in-unit-sphere))))]
      (if (> (mat/dot (:direction scattered) normal) 0)
        {:scattered scattered
         :attenuation albedo}))))

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
    (let [[ux uy uz] (mat/normalise (:direction r))
          t (* 0.5 (+ uy 1.0))]
      (mat/lerp (vec3 1.0 1.0 1.0) 
                (vec3 0.5 0.7 1.0)
                t))))

(def camera
  {:origin (vec3 0.0 0.0 0.0)
   :lleft  (vec3 -2.0 -1.0 -1.0)
   :horiz  (vec3 4.0 0.0 0.0)
   :vert   (vec3 0.0 2.0 0.0)})

(defn get-ray
  [cam u v]
  (let [{:keys [origin lleft horiz vert]} cam]
    (ray origin 
         (mat/add lleft
                  (mat/mul u horiz)
                  (mat/mul v vert)
                  (mat/negate origin)))))

(defn -main
  [& [is ix iy]]
  (let [nx (if ix (Integer/parseUnsignedInt ix) 200)
        ny (if iy (Integer/parseUnsignedInt iy) 100)
        ns (if is (Integer/parseUnsignedInt is) 100)
        world  (hitlist. [(sphere. (vec3 0 0 -1) 0.5 
                                   (lambertian. (vec3 0.8 0.3 0.3)))
                          (sphere. (vec3 0 -100.5 -1) 100
                                   (lambertian. (vec3 0.8 0.8 0)))
                          (sphere. (vec3 1 0 -1) 0.5 
                                   (metal. (vec3 0.8 0.6 0.2) 0.3))
                          (sphere. (vec3 -1 0 -1) 0.5 
                                   (metal. (vec3 0.8 0.8 0.8) 1.0))])]
    (println (str "P3\n" nx " " ny "\n255\n"))
    (doseq [j (range (dec ny) -1 -1)
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
             (mat/emap int))]
        (println ir ig ib))))
  (shutdown-agents))
