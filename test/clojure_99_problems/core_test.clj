(ns clojure-99-problems.core-test
  (:require [clojure.test :refer :all]
            [clojure-99-problems.core :refer :all]))

;; Test problem 01: Find the last element of a list

(deftest test-my-last
  (testing "my-last"
    (is (= 3 (my-last [1 2 3])))
    (is (= 1 (my-last [1]))))
  (testing "my-last with empty list"
    (is (= nil (my-last [])))))
;; Test problem 02: Find the last but one element of a list

(deftest test-my-but-last
  (testing "my-but-last"
    (is (= [2 3] (my-but-last [1 2 3])))
    (is (= [1 2] (my-but-last [1 2]))))
  (testing "my-but-last with empty list"
    (is (= [] (my-but-last []))))
  (testing "my-but-last with one element list"
    (is (= [1] (my-but-last [1])))))

;; Test problem 03: Find the K'th element of a list

(deftest test-element-at
  (testing "element-at"
    (is (= 3 (element-at [1 2 3] 2)))
    (is (= 1 (element-at [1 2 3] 0)))
  (testing "element-at with empty list"
    (is (= nil (element-at [] 0))))))


(run-tests)
