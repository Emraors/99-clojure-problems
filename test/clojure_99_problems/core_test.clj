(ns clojure-99-problems.core-test
  (:require [clojure.test :refer :all]
            [clojure-99-problems.core :refer :all]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]))

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

;; Test problem 04: Find the number of elements of a list

(deftest test-lenght
  (testing "lenght of empty list"
    (is (= 0 (length []))))
  (testing "lenght of a list"
    (is (= 3 (length [1 2 3])))))

;; Test problem 05: Reverse a list

(defspec test-reverse 100
  (prop/for-all [list (gen/vector gen/int)]
    (= list (my-reverse (my-reverse list)))))



;; Test problem 28: Sorting a list of lists according to length of sublists

(deftest test-lsort
  (testing "lsort"
    (is (= (length-sort [[1 2 3] [1 2] [1 2 3 4 5] [1 2 3 4]])
           [[1 2] [1 2 3] [1 2 3 4] [1 2 3 4 5]])))
  (testing "lsort with empty list"
    (is (= (length-sort []) []))))


(defspec test-lsort 100
  (prop/for-all [list-of-lists (gen/vector (gen/vector gen/int))]
    (let [sorted-list (length-sort list-of-lists)]
      (every? (fn [[x y]] (<= (count x) (count y))) (partition 2 sorted-list)))))



(run-tests)
