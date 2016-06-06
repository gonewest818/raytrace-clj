(ns raytrace-clj.hitable
  (:require [clojure.core.matrix :as mat]
            [raytrace-clj.shader :refer [->isotropic]]
            [raytrace-clj.metrics :as metric]
            [raytrace-clj.util :refer :all]))

(defprotocol hitable
  (hit? [this r t-min t-max]
    "does ray intersect inside the interval [t-min, t-max]?")
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
    (metric/increment! metric/count-aabb)
    (let [org (:origin r)
          dir (:direction r)
          m (mat/div! (mat/sub! (mat/clone vmin) org) dir)
          n (mat/div! (mat/sub! (mat/clone vmax) org) dir)
          t0 (mat/emap! min (mat/clone m) n)
          t1 (mat/emap! max m n)
          tmin (max (mat/maximum t0) t-min)
          tmax (min (mat/minimum t1) t-max)]
      (> tmax tmin)))) ; true or false

(defrecord aabb-unrolled [vmin vmax]
  hitable
  (hit? [this r t-min t-max]
    (metric/increment! metric/count-aabb)
    (let [org (:origin r)
          dir (:direction r)]
      (loop [tmin t-min
             tmax t-max
             index 0]
        (let [o (mat/mget org index)
              d (mat/mget dir index)
              v1 (mat/mget vmin index)
              v2 (mat/mget vmax index)
              m (/ (- v1 o) d)
              n (/ (- v2 o) d)
              t0 (min m n)
              t1 (max m n)]
          (if (< index 2)
            (recur (max t0 tmin)
                   (min t1 tmax)
                   (inc index))
            (> (min t1 tmax) (max t0 tmin))))))))

; test
(comment (let [r (ray (vec3 -10 -10 -10)
                      (vec3 1 1 1)
                      0)
               b (->aabb (vec3 1 1 1)
                         (vec3 2 2 2))]
           (time (dotimes [n 1000000]
                   (hit? b r 0 Float/MAX_VALUE)))) )

(defn make-surrounding-bbox
  "compute aabb that surrounds the two given aabb's"
  [box0 box1]
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
          :else (let [sp (split-at (/ n 2) my-list)
                      L  (make-bvh (first sp) t0 t1)
                      R  (make-bvh (second sp) t0 t1)]
                   (->bvh-node L R (make-surrounding-bbox
                                    (bbox L t0 t1) (bbox R t0 t1)))))))

;;;
;;; spheres

(defn get-sphere-uv
  "Compute uv coordinates based on spherical coords. Assumes p sits on
  a unit sphere at the origin."
  [p]
  (let [px    (mat/mget p 0)
        py    (mat/mget p 1)
        pz    (mat/mget p 2)
        phi   (Math/atan2 pz px)
        theta (Math/asin py)
        u     (- 1.0 (/ (+ phi Math/PI) (* 2.0 Math/PI)))
        v     (/ (+ theta (/ Math/PI 2.0)) Math/PI)]
    [u v]))

(defrecord sphere [center radius material]
  hitable
  (hit? [this r t-min t-max]
    (let [org (:origin r)
          dir (:direction r)
          oc (mat/sub org center)
          a (mat/dot dir dir)
          b (* 2.0 (mat/dot oc dir))
          c (- (mat/dot oc oc) (* radius radius))
          discriminant (- (* b b) (* 4.0 a c))]
      (if (>= discriminant 0)
        (or
         (let [t (/ (- (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)
               cpn (mat/normalise (mat/sub p center))]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p
              :uv (get-sphere-uv cpn)
              :normal cpn
              :material material}))
         (let [t (/ (+ (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)
               cpn (mat/normalise (mat/sub p center))]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p
              :uv (get-sphere-uv cpn)
              :normal cpn
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
          org (:origin r)
          dir (:direction r)
          oc (mat/sub org center-t)
          a (mat/dot dir dir)
          b (* 2.0 (mat/dot oc dir))
          c (- (mat/dot oc oc) (* radius radius))
          discriminant (- (* b b) (* 4.0 a c))]
      (if (>= discriminant 0)
        (or
         (let [t (/ (- (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)
               cpn (mat/normalise (mat/sub p center-t))]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p
              :uv (get-sphere-uv cpn)
              :normal cpn
              :material material}))
         (let [t (/ (+ (- b) (Math/sqrt discriminant)) (* 2.0 a))
               p (point-at-parameter r t)
               cpn (mat/normalise (mat/sub p center-t))]
           (if (and (> t t-min) (< t t-max))
             {:t t :p p
              :uv (get-sphere-uv cpn)
              :normal cpn
              :material material}))))))
  (bbox [this t-start t-end]
    (let [c-start (center-at-time center0 t0 center1 t1 t-start)
          c-end   (center-at-time center0 t0 center1 t1 t-end)
          vec3r   (vec3 radius radius radius)]
      (make-surrounding-bbox
       (->aabb (mat/sub c-start vec3r) (mat/add c-start vec3r))
       (->aabb (mat/sub c-end vec3r) (mat/add c-end vec3r))))))

;;;
;;; rectangles

(defrecord rect-xy [x0 y0 x1 y1 k material]
  hitable
  (hit? [this r t-min t-max]
    (let [ror (:origin r)
          rdi (:direction r)
          ror-x (mat/mget ror 0)
          ror-y (mat/mget ror 1)
          ror-z (mat/mget ror 2)
          rdi-x (mat/mget rdi 0)
          rdi-y (mat/mget rdi 1)
          rdi-z (mat/mget rdi 2)
          t (/ (- k ror-z) rdi-z)]
      (if (and (>= t t-min) (<= t t-max))
        (let [x (+ ror-x (* t rdi-x))
              y (+ ror-y (* t rdi-y))]
          (if (and (>= x x0) (<= x x1)
                   (>= y y0) (<= y y1))
            {:t t
             :p (point-at-parameter r t)
             :uv [(/ (- x x0) (- x1 x0))
                  (/ (- y y0) (- y1 y0))]
             :normal (vec3 0 0 1)
             :material material})))))
  (bbox [this t-start t-end]
    (->aabb (vec3 x0 y0 (- k 0.0001))
            (vec3 x1 y1 (+ k 0.0001)))))

(defrecord rect-xz [x0 z0 x1 z1 k material]
  hitable
  (hit? [this r t-min t-max]
    (let [ror (:origin r)
          rdi (:direction r)
          ror-x (mat/mget ror 0)
          ror-y (mat/mget ror 1)
          ror-z (mat/mget ror 2)
          rdi-x (mat/mget rdi 0)
          rdi-y (mat/mget rdi 1)
          rdi-z (mat/mget rdi 2)
          t (/ (- k ror-y) rdi-y)]
      (if (and (>= t t-min) (<= t t-max))
        (let [x (+ ror-x (* t rdi-x))
              z (+ ror-z (* t rdi-z))]
          (if (and (>= x x0) (<= x x1)
                   (>= z z0) (<= z z1))
            {:t t
             :p (point-at-parameter r t)
             :uv [(/ (- x x0) (- x1 x0))
                  (/ (- z z0) (- z1 z0))]
             :normal (vec3 0 1 0)
             :material material})))))
  (bbox [this t-start t-end]
    (->aabb (vec3 x0 (- k 0.0001) z0)
            (vec3 x1 (+ k 0.0001) z1))))

(defrecord rect-yz [y0 z0 y1 z1 k material]
  hitable
  (hit? [this r t-min t-max]
    (let [ror (:origin r)
          rdi (:direction r)
          ror-x (mat/mget ror 0)
          ror-y (mat/mget ror 1)
          ror-z (mat/mget ror 2)
          rdi-x (mat/mget rdi 0)
          rdi-y (mat/mget rdi 1)
          rdi-z (mat/mget rdi 2)
          t (/ (- k ror-x) rdi-x)]
      (if (and (>= t t-min) (<= t t-max))
        (let [y (+ ror-y (* t rdi-y))
              z (+ ror-z (* t rdi-z))]
          (if (and (>= y y0) (<= y y1)
                   (>= z z0) (<= z z1))
            {:t t
             :p (point-at-parameter r t)
             :uv [(/ (- y y0) (- y1 y0))
                  (/ (- z z0) (- z1 z0))]
             :normal (vec3 1 0 0)
             :material material})))))
  (bbox [this t-start t-end]
    (->aabb (vec3 (- k 0.0001) y0 z0)
            (vec3 (+ k 0.0001) y1 z1))))

(comment (let [b (->rect-yz 0 0 2 2 1 nil)
               r (ray (vec3 -10 1 1) (vec3 1 0 0) 0)]
           (time
            (dotimes [n 1000000]
              (hit? b r 0 Float/MAX_VALUE)))) )


;;;
;;; flip normals, as a wrapper

(defrecord flip-normals [obj]
  hitable
  (hit? [this r t-min t-max]
    (if-let [hrec (hit? obj r t-min t-max)]
      (update-in hrec [:normal] mat/negate)))
  (bbox [this t-start t-end]
    (bbox obj t-start t-end)))

;;;
;;; translate instance, as a wrapper

(defrecord translate [obj offset]
  hitable
  (hit? [this r t-min t-max]
    (let [trans-r (update-in r [:origin] #(mat/sub % offset))]
      (if-let [hrec (hit? obj trans-r t-min t-max)]
        (update-in hrec [:p] #(mat/add % offset)))))
  (bbox [this t-start t-end]
    (if-let [my-box (bbox obj t-start t-end)]
      (->aabb (mat/add (:vmin my-box) offset)
              (mat/add (:vmax my-box) offset)))))

;;;
;;; rotate instance, as a wrapper

(defrecord rotate-y [obj rotated-bbox sin-theta cos-theta]
  hitable
  (hit? [this r t-min t-max]
    (let [ror (:origin r)
          rdi (:direction r)
          ox (mat/mget ror 0)
          oy (mat/mget ror 1)
          oz (mat/mget ror 2)
          dx (mat/mget rdi 0)
          dy (mat/mget rdi 1)
          dz (mat/mget rdi 2)
          ;; pre-rotate the inbound ray before hit testing
          rot-o (vec3 (- (* cos-theta ox) (* sin-theta oz))
                      oy
                      (+ (* sin-theta ox) (* cos-theta oz)))
          rot-d (vec3 (- (* cos-theta dx) (* sin-theta dz))
                      dy
                      (+ (* sin-theta dx) (* cos-theta dz)))
          rot-r (ray rot-o rot-d (:time r))]
      ;; perform the hit test
      (if-let [hrec (hit? obj rot-r t-min t-max)]
        (let [hp (:p hrec)
              hn (:normal hrec)
              px (mat/mget hp 0)
              py (mat/mget hp 1)
              pz (mat/mget hp 2)
              nx (mat/mget hn 0)
              ny (mat/mget hn 1)
              nz (mat/mget hn 2)
              ;; ... and if hit, then post-rotate the p and normal
              rot-p (vec3 (+ (* cos-theta px) (* sin-theta pz))
                          py
                          (+ (- (* sin-theta px)) (* cos-theta pz)))
              rot-n (vec3 (+ (* cos-theta nx) (* sin-theta nz))
                          ny
                          (+ (- (* sin-theta nx)) (* cos-theta nz)))]
          ;; updating the hit record with the new values
          (-> hrec
              (assoc-in [:p] rot-p)
              (assoc-in [:normal] rot-n))))))
  (bbox [this t-start t-end] rotated-bbox))

(defn make-rotate-y [obj theta]
  (let [radians (* theta (/ Math/PI 180.0))
        cos-th  (Math/cos radians)
        sin-th  (Math/sin radians)
        box     (bbox obj 0 1)
        old-min (:vmin box)
        old-max (:vmax box)
        [new-min new-max]
        ;; this reduces over rotated corners to find new min & max
        (reduce #(vector (mat/emap min (nth %1 0) %2)
                         (mat/emap max (nth %1 1) %2))
                ;; here we seed the result of the reduce
                (vector (vec3 Float/MAX_VALUE
                              Float/MAX_VALUE
                              Float/MAX_VALUE)
                        (vec3 (- Float/MAX_VALUE)
                              (- Float/MAX_VALUE)
                              (- Float/MAX_VALUE)))
                ;; this generates all 8 corners of the bbox, rotated
                (for [x [(mat/mget old-min 0) (mat/mget old-max 0)]
                      y [(mat/mget old-min 1) (mat/mget old-max 1)]
                      z [(mat/mget old-min 2) (mat/mget old-max 2)]
                      ;; ...applying the 2D rotation matrix to each corner
                      :let [new-x (+    (* cos-th x)  (* sin-th z))
                            new-z (+ (- (* sin-th x)) (* cos-th z))]]
                  (vec3 new-x y new-z)))]
    (->rotate-y obj
                (->aabb new-min new-max)
                sin-th
                cos-th)))

;;;
;;; axis-aligned box

(defrecord box [p0 p1 sides]
  hitable
  (hit? [this r t-min t-max]
    (hit? sides r t-min t-max))
  (bbox [this t-start t-end]
    (->aabb p0 p1)))

(defn make-box
  "factory to make a box record"
  [p0 p1 material]
  (let [[x0 y0 z0] (seq p0)
        [x1 y1 z1] (seq p1)]
    (->box p0 p1
           (->hitlist
            (list
             (->rect-xy x0 y0 x1 y1 z1 material)
             (->flip-normals (->rect-xy x0 y0 x1 y1 z0 material))
             (->rect-xz x0 z0 x1 z1 y1 material)
             (->flip-normals (->rect-xz x0 z0 x1 z1 y0 material))
             (->rect-yz y0 z0 y1 z1 x1 material)
             (->flip-normals (->rect-yz y0 z0 y1 z1 x0 material)))))))

;;;
;;; constant-medium, i.e. fog or other participating media

(defrecord constant-medium [boundary density phase-fn]
  hitable
  (hit? [this r t-min t-max]
    (if-let [hrec1 (hit? boundary r (- Float/MAX_VALUE) Float/MAX_VALUE)]
      (if-let [hrec2 (hit? boundary r (+ (:t hrec1) 0.0001) Float/MAX_VALUE)]
        (let [t1 (:t hrec1)
              t2 (:t hrec2)
              t1 (if (< t1 t-min) t-min t1)
              t2 (if (> t2 t-max) t-max t2)]
          (if (< t1 t2)
            (let [t1 (if (< t1 0) 0 t1)
                  mag-r-dir (mat/magnitude (:direction r))
                  dist-in-boundary (* (- t2 t1) mag-r-dir)
                  hit-distance (- (/ (Math/log (rand)) density))]
              (if (< hit-distance dist-in-boundary)
                (let [new-t (+ t1 (/ hit-distance mag-r-dir))]
                  {:t new-t
                   :p (point-at-parameter r new-t)
                   :uv [0 0]            ; arbitrary
                   :normal (vec3 1 0 0) ; arbitrary
                   :material phase-fn}))))))))
  (bbox [this t-start t-end]
    (bbox boundary t-start t-end)))

(defn make-constant-medium
  [boundary density albedo]
  (->constant-medium boundary density (->isotropic albedo)))
