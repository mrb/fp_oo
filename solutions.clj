;;; Just Enough Clojure

;; Exercise 1

(def second
  (fn [list] (first (rest list))))

;; Exercise 2

(def third
  (fn [list] (first (rest (rest list)))))

(def third
  (fn [list] (second (rest list))))

;; Exercise 3

(def add-squares
  (fn [& list]
    (apply + (map * list list))))

;; Exercise 4

(def bizarre-factorial
  (fn [num] (apply * (range 1 (inc num)))))

;; Exercise 5

; TAKE & FILTER - take-n-odds - take the first N odd elements in a sequence
(def take-n-odds
  (fn [to-take from-list]
    (take to-take
      (filter odd? from-list))))

; DISTINCT

; CONCAT

; REPEAT

; INTERLEAVE & PARTITION - create-groups - create sequences of N from 2 sequences
; e.g. create pairs from a seq of X coords and a seq of Y coords
(def create-groups
  (fn [group-size first second]
    (partition group-size (interleave first second))))

; DROP

; FLATTEN

;; Exercise 6

(def prefix-of?
  (fn [candidate sequence]
    (= candidate
       (take (count candidate) sequence))))

;; Exercise 7

;; I didn't read past the original description and wrote this version,
;; which follows none of the requirements but is pretty nice:
(def tails
  (fn [sequence]
    (cons sequence
    (cond (zero? (count sequence)) '()
          :else (tails (rest sequence))))))

;; Then I got mad stumped and read both hints and smanged this:
(def tails
  (fn [sequence]
      (reverse (map take
        (range (inc (count sequence)))
        (repeat (inc (count sequence)) sequence)))))

;; Exercise 8

;; (def puzzle (fn [list] (list list)))
;; (puzzle '(1 2 3))
;; This throws an error of some sort* because of the rules for binding function
;; parameters. Since you can pass anything in to any function at any time, list
;; must be assumed to be the binding provided by the function parameter. There
;; is too much ambiguity otherwise.

;; To the author's point in re: the substitution rule for functions, that looks
;; like this:

;; (puzzle '(1 2 3))
;; ((123) (123))

;; That doesn't work!

;; * ClassCastException clojure.lang.PersistentList cannot be cast to clojure.lang.IFn  user/puzzle

;;; A Barely Believable Object

;; Exercise 1 - Add, with no shift
(def add
  (fn [point1 point2]
    (Point (+ (x point1) (x point2))
           (+ (y point1) (y point2)))))

;; Exercise 1 - Add, with shift
(def add-with-shift
  (fn [point1 point2]
    (shift point1 (x point2) (y point2))))

;; Exercise 2 - Make - an alternate constructor
(def make
  (fn [class & values]
    (apply class values)))

;; Exercise 3 - Equal trianges
(def equal-triangles?
  (fn [triangle1 triangle2]
    (= triangle1 triangle2)))

;; Exercise 4 - Many Equal trianges
(def equal-triangles?
  (fn [& triangles]
    (apply = triangles)))

;; Exercise 5 - Valid Traingle
(def valid-triangle?
  (fn [point1 point2 point3]
    (= (count (distinct point1 point2 point3)) 3)))
