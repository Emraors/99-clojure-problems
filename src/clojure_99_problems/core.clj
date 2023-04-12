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
  (if (= n 0)
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

;; Problem 16: Drop every N'th element from a list.

(defn drop
  [xs n]
  (->> xs
       (reduce (fn [[partial-list counter] value]
                 (if (= 0 (mod counter n))
                   (list partial-list (inc counter))
                   (list (conj partial-list value) (inc counter))))
               (list [] 1))
       (first)
       (apply list)))

;; Problem 17: Split a list into two parts; the length of the first part is given.

(defn deep-cons
  [el [x & tail :as xs]]
  (cons (cons el x) tail))

(defn split
  [[x & tail :as xs] n]
  (if (= 0 n)
    (list (list) xs)
    (deep-cons x (split tail (dec n)))))

;; Problem 18: Extract a slice from a list.

(defn slice
  [xs n m]
  (first (split (second (split xs (dec n))) (inc (- m n)))))

(slice '(a b c d e f g h i k) 3 7)

;; Problem 19: Rotate a list N places to the left.

(defn rotate
  [xs n]
  (let [length (count xs)
        m (mod (+ length n) length)
        splitted-list (split xs m)
        first-segment (first splitted-list)
        second-segment (second splitted-list)]
    (concat second-segment first-segment)))

;; Problem 20: Remove the K'th element from a list.

(defn remove-at
  [xs n]
  (let [splitted-list (split xs (dec n))
        first-segment (first splitted-list)
        second-segment (rest (second splitted-list))]
    (concat first-segment second-segment)))

;; Problem 21: Insert an element at a given position into a list.

(defn insert-at
  [el xs n]
  (let [splitted-list (split xs (dec n))
        first-segment (first splitted-list)
        second-segment (cons el (second splitted-list))]
    (concat first-segment second-segment)))

;; Problem 22: Create a list containing all integers within a given range.

(defn list-builder
  [init fin op]
  (if (= init fin)
    (list fin)
    (cons init (list-builder (op init) fin op))))

(defmulti my-range (fn [init fin]
                     (if (<= init fin)
                       :inc
                       :dec)))

(defmethod my-range :inc [init fin] (list-builder init fin inc))

(defmethod my-range :dec [init fin] (list-builder init fin dec))

;; Problem 23: Extract a given number of randomly selected elements from a list.

(defn rnd-select
  [xs n]
  (->> (list xs '())
       (iterate (fn [[xs part-res]]
                  (let [len (dec (count xs))
                        rand-pos (inc (rand-int len))]
                    (list (remove-at xs rand-pos) (cons (element-at xs rand-pos) part-res)))))
       (take (inc n))
       (last)
       (second)))

;; Problem 24: Lotto: Draw N different random numbers from the set 1..M.

(defn lotto-select
  [n m]
  (rnd-select (my-range 1 m) n))

;; Problem 25: Generate a random permutation of the elements of a list.

(defn rnd-permu
  [xs]
  (rnd-select xs (count xs)))

;; Problem 26: Generate the combinations of K distinct objects chosen from the N
;; elements of a list.

(defn combinations
  [[x & tail :as xs] n]
    (if (= n 0)
        (list [])
        (if (empty? tail)
        (list [])
        (let [with-x (map (fn [el] (cons x el)) (combinations tail (dec n)))
                without-x (combinations tail n)]
            (concat with-x without-x)))))

;; Problem 27: Group the elements of a set into disjoint subsets.
;; TODO

;; Problem 28: Sorting a list of lists according to length of sublists.

(defn length-sort [xs] (sort-by count xs))
