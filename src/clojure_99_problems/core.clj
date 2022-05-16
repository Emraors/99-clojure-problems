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

;; Problem 09: Pack consecutive duplicates of list elements into sublists.

(defn pack-an-element
  [el [[x & tail :as ys] & rests :as xs]]
  (if (= el x )
    (cons (cons el ys) rests)
    (cons (list el) xs)))

(defn pack
  [[x & tail :as xs]]
  (if (empty? tail)
    '()
    (pack-an-element x (pack tail))))

;; Problem 10: Run-length encoding of a list.

(defn encode
  [xs]
  (->> xs
       (pack)
       (map (fn [[x & tail :as el]] (list x (count el))))))

;; Problem 11: Modified run-length encoding.

(defn pair-simplifier
   [[x num]]
  (if (>= num 2 )
    (list x num)
    x))

(defn encode-modified
  [xs]
  (->> xs
       (encode)
       (map pair-simplifier)))

(encode-modified '(a a a a b c c a a d e e e e))

;; Problem 12: Decode a run-length encoded list.

(defn decode
  [xs]
  (->> xs
       (map (fn [el] (if (seq? el)
                       (take (second el) (first el))
                       (list el))))
       (flatten)))

;; Problem 13: Run-length encoding of a list (direct solution).

(defn count-repetition
  [el [[x numb] & rests :as xs]]
  (if (= el x )
    (cons (list el (inc numb)) rests)
    (cons (list el 1) xs)))

(defn pack-dir
  [[x & tail :as xs]]
  (if (empty? tail)
    '()
    (count-repetition x (pack-dir tail))))

(defn pack-updated
  [xs]
  (->> xs
       (pack-dir)
       (map pair-simplifier)))

;; Problem 14: Duplicate the elements of a list.

(defn duplicate
  [xs]
  (->> xs
       (map (fn [el] (take 2 (repeat el))))
       (flatten)))

;; Problem 15: Replicate the elements of a list a given number of times.

(defn repli
  [xs n]
  (->> xs
       (map (fn [el] (take n (repeat el))))
       (flatten)))

;; We can also use replicate, from clojure.core

(defn repli-core
  [xs n]
  (->> xs
       (map (partial replicate n))
       (flatten)))
