(ns clojure-99-problems.core)

;; Problem 01: Find the last box of a list.

(defn my-last
  [[x & tail :as xs]]
  (if (empty? tail)
    x
    (my-last tail)))

;; Problem 02: Find the last but one box of a list.

(defn my-but-last
  [[x & tail :as xs]]
  (if (empty? (rest tail))
    xs
    (my-but-last tail)))

;; Problem 03: Find the K'th element of a list.

(defn element-at
  [[x & tail :as xs] n]
  (if (= n 1)
    x
    (element-at tail (dec n))))

;; Problem 04: Find the number of elements of a list.

(defn length
  [[x & tail :as xs]]
  (if (empty? xs)
    0
    (+ 1 (length tail))))

;; Problem 05: Reverse a list.

(defn my-reverse
  [xs]
  (reduce (fn [part-res value] (cons value part-res)) '() xs))

;; Problem 06: Find out whether a list is a palindrome.

(defn is-palindrome?
  [xs]
  (= xs (my-reverse xs)))

;; Problem 07: Flatten a nested list structure.

(defn my-flatten
  [[x & tail :as xs]]
  (cond (empty? xs) nil
        (seq? x) (concat (my-flatten x) (my-flatten tail))
        :else (cons x (my-flatten tail))))

;; Problem 08: Eliminate consecutive duplicates of list elements.

(defn compress
  [xs]
  (->> xs
       (reduce (fn [[p & tail :as part-res] value]
                 (if-not (= value p) (cons value part-res) part-res))
               '())
       (reverse)))
