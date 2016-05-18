(ns raytrace-clj.hitable
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))

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
