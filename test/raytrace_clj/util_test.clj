(ns raytrace-clj.util-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix :as mat]
            [raytrace-clj.util :refer :all]))


(deftest vec3-tests
  (testing "construction with integers"
    (def vi (vec3 1 2 3))
    (is (mat/array? vi) "type check")
    (is (= 1.0 (mat/mget vi 0)) "get index 0")
    (is (= 2.0 (mat/mget vi 1)) "get index 1")
    (is (= 3.0 (mat/mget vi 2)) "get index 2"))
  (testing "construction with floats"
    (def vf (vec3 1.0 2.0 3.0))
    (is (mat/array? vf) "type check")
    (is (= 1.0 (mat/mget vf 0)) "get index 0")
    (is (= 2.0 (mat/mget vf 1)) "get index 1")
    (is (= 3.0 (mat/mget vf 2))) "get index 2")
  (testing "construction with rationals"
    (def vr (vec3 1/2 1/4 1/8))
    (is (mat/array? vr) "type check")
    (is (= 0.5 (mat/mget vr 0)) "get index 0")
    (is (= 0.25 (mat/mget vr 1)) "get index 1")
    (is (= 0.125 (mat/mget vr 2))) "get index 2")
  (testing "bounds errors"
    (is (thrown? ArrayIndexOutOfBoundsException
                 (mat/mget (vec3 1 2 3) -1)) "negative index")
    (is (thrown? ArrayIndexOutOfBoundsException
                 (mat/mget (vec3 1 2 3) 3)) "index too large")))


(deftest ray-tests
  (testing "construction"
    (def r (ray (vec3 1 2 3) (vec3 4 5 6)))
    (is (contains? (set (keys r)) :origin))
    (is (contains? (set (keys r)) :direction)) 
    (is (mat/equals (:direction r) (vec3 4 5 6)) "get direction")
    (is (mat/equals (:origin r) (vec3 1 2 3)) "get origin")))


(deftest point-at-parameter-test
  (testing "point-at-parameter"
    (def r (ray (vec3 1 2 3) (vec3 4 5 6)))
    (is (= (vec3 1 2 3) (point-at-parameter r 0)))
    (is (= (vec3 5 7 9) (point-at-parameter r 1)))
    (is (= (vec3 -3 -3 -3) (point-at-parameter r -1)))))

