(ns raytrace-clj.hitable
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))

(defprotocol hitable
  (hit? [this r t-min t-max])
  (bbox [this t0 t1]))

;;; 
;;; brute force search over all objects in scene

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

;;; 
;;; bounding box

(defrecord aabb [vmin vmax]
  hitable
  (hit? [this r t-min t-max]
    "intersect ray with bounding box in the interval [t-min, t-max]"
    ;; simplest way to express this is with core.matrix, but
    ;; would this be faster by doing element-wise computations
    ;; and bailing when the overlap test fails for any element?
    (let [m (mat/sub vmin (mat/div (:origin r) (:direction r)))
          n (mat/sub vmax (mat/div (:origin r) (:direction r)))
          t0 (mat/emap min m n)
          t1 (mat/emap max m n)
          tmin (max (mat/maximum t0) t-min)
          tmax (min (mat/minimum t1) t-max)]
      (> tmax tmin)))) ; true or false

(defn make-surrounding-bbox [box0 box1]
  "compute aabb that surrounds the two given aabb's"
  (let [small (mat/emap min (:vmin box0) (:vmin box1))
        big   (mat/emap max (:vmax box0) (:vmax box1))]
    (->aabb small big)))

;;; 
;;; bounding volume hierarchy

(defrecord bvh-node [left right box]
  hitable
  (hit? [this r t-min t-max]
    (if (hit? box r t-min t-max)
      (let [hl (hit? left r t-min t-max)
            hr (hit? right r t-min t-max)]
        (cond (and hl hr) (if (< (:t hl) (:t hr)) hl hr)
              hl hl
              hr hr))))
  (bbox [this t0 t1] box))

(defn make-bvh [hitable-list t0 t1]
  (let [axis (rand-int 3)
        my-list (sort-by #(mat/mget (:vmin (bbox % t0 t1)) axis) 
                         hitable-list)
        n (count my-list)]
    (cond (= n 1) (let [L (first my-list)]
                    (->bvh-node L L (bbox L t0 t1)))
          (= n 2) (let [L (first my-list)
                        R (second my-list)]
                    (->bvh-node L R (make-surrounding-bbox
                                     (bbox L t0 t1) (bbox R t0 t1))))
          :else (let [sp (split-at (/ n 2) hitable-list)
                      L  (make-bvh (first sp) t0 t1)
                      R  (make-bvh (second sp) t0 t1)]
                   (->bvh-node L R (make-surrounding-bbox
                                    (bbox L t0 t1) (bbox R t0 t1)))))))

;;; 
;;; spheres

(defrecord sphere [center radius material]
  hitable
  (hit? [this r t-min t-max]
    (let [oc (mat/sub (:origin r) center)
          a (mat/dot (:direction r) (:direction r))
          b (mat/mul 2.0 (mat/dot oc (:direction r)))
          c (- (mat/dot oc oc) (* radius radius))
          discriminant (- (* b b) (* 4.0 a c))]
      (if (>= discriminant 0)
        (or
         (let [t (/ (- (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p 
              :normal (mat/div (mat/sub p center) radius)
              :material material}))
         (let [t (/ (+ (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p 
              :normal (mat/div (mat/sub p center) radius)
              :material material}))))))
  (bbox [this t0 t1]
    (let [vec3r (vec3 radius radius radius)] 
      (->aabb (mat/sub center vec3r)
              (mat/add center vec3r)))))

(defn center-at-time 
  [center0 t0 center1 t1 t]
  (mat/lerp center0 center1
            (/ (- t t0) (- t1 t0))))


(defrecord moving-sphere [center0 t0 center1 t1 radius material]
  hitable
  (hit? [this r t-min t-max]
    (let [center-t (center-at-time center0 t0 center1 t1 (:time r))
          oc (mat/sub (:origin r) center-t)
          a (mat/dot (:direction r) (:direction r))
          b (mat/mul 2.0 (mat/dot oc (:direction r)))
          c (- (mat/dot oc oc) (* radius radius))
          discriminant (- (* b b) (* 4.0 a c))]
      (if (>= discriminant 0)
        (or 
         (let [t (/ (- (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p 
              :normal (mat/div (mat/sub p center-t) radius)
              :material material}))
         (let [t (/ (+ (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p 
              :normal (mat/div (mat/sub p center-t) radius)
              :material material}))))))
  (bbox [this t-start t-end]
    (let [c-start (center-at-time center0 t0 center1 t1 t-start)
          c-end   (center-at-time center0 t0 center1 t1 t-end)
          vec3r   (vec3 radius radius radius)]
      (make-surrounding-bbox
       (->aabb (mat/sub c-start vec3r) (mat/add c-start vec3r))
       (->aabb (mat/sub c-end vec3r) (mat/add c-end vec3r))))))
