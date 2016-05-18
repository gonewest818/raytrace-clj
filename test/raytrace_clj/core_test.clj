(ns raytrace-clj.core-test
  (:require [clojure.test :refer :all]
            [raytrace-clj.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1) "testing equality")))

(deftest exception-test
  (testing "divide by zero exception"
    (is (thrown? ArithmeticException (/ 1 0)))
    (is (thrown-with-msg? ArithmeticException
                          #"Divide by zero"
                          (/ 1 0)))))

