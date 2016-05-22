(ns raytrace-clj.hitable-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]
            [raytrace-clj.hitable :refer :all]
            [raytrace-clj.shader :refer [->lambertian]]))

(def gridpoints (for [i (range -1 2)
                      j (range -1 2)
                      k (range -1 2)]
                  (mat/mul 25 (vec3 i j k))))

(def directions (for [i (range -1 2)
                      j (range -1 2)
                      k (range -1 2)
                      :when (or (not= 0 i)
                                (not= 0 j)
                                (not= 0 k))]
                  (mat/mul 5.0 (vec3 i j k))))

(def material (->lambertian (vec3 0.8 0.8 0.8)))

(deftest sphere-tests
  (testing "sphere"
    (doseq [origin gridpoints] 
      (let [radius 1.0
            s (->sphere origin radius material)]
        (is (= raytrace_clj.hitable.sphere (type s))  "constructor")
        (is (mat/equals origin (:center s))           "get center")
        (is (= radius (:radius s))                    "get radius")
        (is (= material (:material s))                "get material")
        (doseq [dir directions]
          (is (hit? s (ray (mat/add origin dir) (mat/negate dir) 0.0) 
                    0.0 Float/MAX_VALUE)              "intersect ray")
          (is (not (hit? s (ray (mat/add origin dir) dir 0.1)
                         0.0 Float/MAX_VALUE))        "non-intersecting ray"))
        (is (hit? s (ray (mat/add origin (vec3 radius radius 0))
                         (vec3 -1 0 0) 0.0) 
                  0.0 Float/MAX_VALUE)                "grazing ray x")
        (is (hit? s (ray (mat/add origin (vec3 radius radius 0))
                         (vec3 0 -1 0) 0.0) 
                  0.0 Float/MAX_VALUE)                "grazing ray y")
        (is (hit? s (ray (mat/add origin (vec3 radius 0 radius))
                         (vec3 0 0 -1) 0.0) 
                  0.0 Float/MAX_VALUE)                "grazing ray z")
        (is (hit? s (ray origin (vec3 1 1 1) 0.0)
                  0.0 Float/MAX_VALUE)                "intersect ray from inside")
        (is (= (- (mat/mget origin 0) radius)
               (mat/mget (:vmin (bbox s 0 0)) 0))     "bbox min x coord")
        (is (= (- (mat/mget origin 1) radius)
               (mat/mget (:vmin (bbox s 0 0)) 1))     "bbox min y coord")
        (is (= (- (mat/mget origin 2) radius)
               (mat/mget (:vmin (bbox s 0 0)) 2))     "bbox min z coord")
        (is (= (+ (mat/mget origin 0) radius)
               (mat/mget (:vmax (bbox s 0 0)) 0))     "bbox max x coord")
        (is (= (+ (mat/mget origin 1) radius)
               (mat/mget (:vmax (bbox s 0 0)) 1))     "bbox max y coord")
        (is (= (+ (mat/mget origin 2) radius)
               (mat/mget (:vmax (bbox s 0 0)) 2))     "bbox max z coord")))))

(deftest moving-sphere-tests
  (testing "moving-sphere"
    (doseq [origin gridpoints] 
      (let [destination (mat/add origin (vec3 10 20 30))
            radius 1.0
            t0 0.1
            t1 0.9
            s (->moving-sphere origin t0 destination t1 radius material)]
        (is (= (type s) raytrace_clj.hitable.moving-sphere)
                                                      "constructor")
        (is (mat/equals origin (:center0 s))          "get center0")
        (is (mat/equals destination (:center1 s))     "get center1")
        (is (= t0 (:t0 s))                            "get t0")
        (is (= t1 (:t1 s))                            "get t1")
        (is (= radius (:radius s))                    "get radius")
        (is (= material (:material s))                "get material") 
        (doseq [dir directions]
          (is (hit? s (ray (mat/add origin dir) (mat/negate dir) t0)
                    0.0 Float/MAX_VALUE)              "intersect ray")
          (is (not (hit? s (ray (mat/add origin dir) dir t0) 
                         0.0 Float/MAX_VALUE))        "non-intersecting ray"))
        (is (hit? s (ray origin (vec3 1 1 1) t0)
                  0.0 Float/MAX_VALUE)                "intersect ray from inside")
        (testing "bounding box computations"
          (is (= (- (mat/mget origin 0) radius)
                 (mat/mget (:vmin (bbox s t0 t0)) 0)) "bbox min x coord")
          (is (= (- (mat/mget origin 1) radius)
                 (mat/mget (:vmin (bbox s t0 t0)) 1)) "bbox min y coord")
          (is (= (- (mat/mget origin 2) radius)
                 (mat/mget (:vmin (bbox s t0 t0)) 2)) "bbox min z coord")
          (is (= (+ (mat/mget origin 0) radius)
                 (mat/mget (:vmax (bbox s t0 t0)) 0)) "bbox max x coord")
          (is (= (+ (mat/mget origin 1) radius)
                 (mat/mget (:vmax (bbox s t0 t0)) 1)) "bbox max y coord")
          (is (= (+ (mat/mget origin 2) radius)
                 (mat/mget (:vmax (bbox s t0 t0)) 2)) "bbox max z coord")))))
  (testing "center-at-time"
    (let [pa (vec3 0 0 0)
          pb (vec3 1 2 3)]
      (is (= pa (center-at-time pa 0 pb 1 0))         "lerp t=0")
      (is (= pb (center-at-time pa 0 pb 1 1))         "lerp t=1")
      (is (= (vec3 0.5 1.0 1.5) 
             (center-at-time pa 0 pb 1 0.5))          "lerp t=0.5"))))


(deftest aabb-tests
  (testing "constructor"
    (let [a (vec3 -1 -2 -3)
          b (vec3 4 5 6)]
      (is (= (type (->aabb a b))
             raytrace_clj.hitable.aabb)               "constructor")
      (is (mat/equals a (:vmin (->aabb a b)))         "get vmin")
      (is (mat/equals b (:vmax (->aabb a b)))         "get vmax")))
  (testing "intersection"
    (let [a (vec3 -1 -1 -1)
          b (vec3 1 1 1)
          box (->aabb a b)]
      (is (hit? box (ray (vec3 0 0 0) (vec3 1 1 1) 0)
                0 Float/MAX_VALUE)                    "from inside")
      (is (hit? box (ray (vec3 -2 0 0) (vec3 1 0 0) 0)
                0 Float/MAX_VALUE)                    "along x")
      (is (hit? box (ray (vec3 0 -2 0) (vec3 0 1 0) 0)
                0 Float/MAX_VALUE)                    "along y")
      (is (hit? box (ray (vec3 0 0 -2) (vec3 0 0 1) 0)
                0 Float/MAX_VALUE)                    "along z")
      (is (hit? box (ray (vec3 -2 1 0) (vec3 1 0 0) 0)
                0 Float/MAX_VALUE)                    "grazing x")
      (is (hit? box (ray (vec3 1 -2 0) (vec3 0 1 0) 0)
                0 Float/MAX_VALUE)                    "grazing y")
      (is (hit? box (ray (vec3 1 0 -2) (vec3 0 0 1) 0)
                0 Float/MAX_VALUE)                    "grazing z")))
  (testing "combining"
    (let [a (bbox (->sphere (vec3 -1 2 -3) 0.1 nil) 0 0)
          b (bbox (->sphere (vec3 1 -2 3) 0.1 nil) 0 0)
          c (make-surrounding-bbox a b)]
      (is (= -1.1 (mat/mget (:vmin c) 0))             "min x")
      (is (= -2.1 (mat/mget (:vmin c) 1))             "min y")
      (is (= -3.1 (mat/mget (:vmin c) 2))             "min z")
      (is (= 1.1 (mat/mget (:vmax c) 0))              "max x")
      (is (= 2.1 (mat/mget (:vmax c) 1))              "max y")
      (is (= 3.1 (mat/mget (:vmax c) 2))              "max z"))))


(deftest bvh-tests
  (testing "constructor"
    (let [f (vec3 0 0 0)
          g (vec3 1 1 1)
          bb (->aabb f g)]
      (is (= (type (->bvh-node bb bb bb))
             raytrace_clj.hitable.bvh-node))))
  (testing "scene construction"
    (let [points [(vec3 0 0 0)
                  (vec3 1 2 3)
                  (vec3 -2 -1 -3)
                  (vec3 3 -1 2)
                  (vec3 -3 2 1)]
          spheres (map #(->sphere % 0.1 material) points)]
      ;; (doseq [i (range 1 6)]
      ;;   (printf "==%s==\n" i)
      ;;   (clojure.pprint/pprint (make-bvh (take i spheres) 0 1)))

      ;; #spy/p (hit? #spy/p (->sphere (vec3 4 1 0) 1.0 material)
      ;;              #spy/p (ray (vec3 4 5 20) 
      ;;                          (mat/sub (vec3 4 1 0) (vec3 4 5 20))
      ;;                          0) 0 1)

      ;; #spy/p (hit? #spy/p (make-bvh (list (->sphere (vec3 4 1 0) 1.0 material)) 0 1)
      ;;              #spy/p (ray (vec3 4 5 20) 
      ;;                          (mat/sub (vec3 4 1 0) (vec3 4 5 20))
      ;;                          0)
      ;;              0 1)

      ;; #spy/p (hit? #spy/p (->aabb (vec3 3 0 -1) (vec3 5 2 1))
      ;;              #spy/p (ray (vec3 4 5 20) 
      ;;                          (mat/sub (vec3 4 1 0) (vec3 4 5 20))
      ;;                          0)
      ;;              0 Float/MAX_VALUE)


)))
