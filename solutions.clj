;;; Chapter 2 - Just Enough Clojure

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

;;; Chapter 3 - A Barely Believable Object

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

;;; Chapter 4 - All the Class in a Constructor

;; Change the Point constructor to add x and y accessors (getters).
;; Use them in shift. Implement add, and have it use shift.
(def Point
  (fn [x y]
    {:x x,
     :y y
     :__class_symbol__ 'Point
     :__methods__ {
       :class :__class_symbol__
       :shift (fn [this xinc yinc]
                (make Point (+ (send-to this :x) xinc)
                            (+ (send-to this :y) yinc)))
       :add (fn [this that]
                (send-to this :shift (send-to that :x)
                                     (send-to that :y)))
       :x :x
       :y :y}}))

;;; Chapter 5 - Moving the Class Out of the Constructor

;; Exercise 1
(def apply-message-to
  (fn [class instance message args]
    (apply (message (:__instance_methods__ class)) instance args)))

(def make
  (fn [class & args]
    (let [seeded {:__class_symbol__ (:__own_symbol__ class)}]
      (apply-message-to class seeded :add-instance-values args))))

(def send-to
  (fn [instance message & args]
    (let [class (eval (:__class_symbol__ instance))]
      (apply-message-to class instance message args))))

;; Exercise 2
(def Point
{
  :__own_symbol__ 'Point
  :__instance_methods__
  {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :class-name  :__class_symbol__
    :class (fn [this] (eval (:__class_symbol__ this)))
    :shift (fn [this xinc yinc]
             (make Point (+ (:x this) xinc)
                         (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift (:x other)
                                (:y other)))
   }
 })

;; Exercise 3
(def Point
{
  :__own_symbol__ 'Point
  :__instance_methods__
  {
    :add-instance-values (fn [this x y]
                           (assoc this :x x :y y))
    :class-name  :__class_symbol__
    :class (fn [this] (eval (:__class_symbol__ this)))
    :shift (fn [this xinc yinc]
             (make Point (+ (:x this) xinc)
                         (+ (:y this) yinc)))
    :add (fn [this other]
           (send-to this :shift (:x other)
                                (:y other)))
    :origin (fn [this] (make Point 0 0))
   }
 })

;; Instances created before class redefinition benefit from the dynamic nature
;; of the method dispatch in the system we're working on. Because the instances
;; have a reference to the class and don't hold methods themselves, this works.

;; Exercise 4
(def apply-message-to
  (fn [class instance message args]
    (let [method (message (:__instance_methods__ class))]
      (cond
        (nil? method) (message instance)
        :else (apply method instance args)))))

;; Exercise 5
;; Sending a message that isn't a method or instance variable will return nil.
;; You might expect that it would throw some kind of error instead.

;; Chapter 6 - Inheritance (and Recursion)

;; Exercise 1
(def factorial
  (fn [n]
    (if
     (zero? n) 1
     (* n (factorial (dec n))))))

;; Exercise 2
(def factorial-1
     (fn [n so-far]
       (if (zero? n)
         so-far
         (factorial-1 (dec n) (* n so-far)))))

(def factorial
  (fn [n]
    (factorial-1 n 1)))

;; Exercise 3
(def recursive-function
  (fn [n so-far]
    (if (empty? n)
      so-far
      (recursive-function (rest n)
                          (+ (first n) so-far)))))

;; Exercise 4
(def recursive-function
  (fn [n so-far]
    (if (empty? n)
      so-far
      (recursive-function (rest n)
                          (* (first n) so-far)))))

;; Exercise 5
(def recursive-function
  (fn [op n so-far]
    (if (empty? n)
      so-far
      (recursive-function op (rest n)
                             (op (first n) so-far)))))

;; Exercise 6
(def weird-fn
  (fn [val map]
    (assoc map val 0)))

(def weird-fn-with-position
  (fn [val map]
    (assoc map val (count map))))
