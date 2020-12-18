(ns programming-bitcoin.elliptic-curve
  "Chapter 2"
  (:require [clojure.math.numeric-tower :as nt]
            [programming-bitcoin.finite-fields :refer [FieldElementOp + - * / ** zero?]]))

(defprotocol PointOp
  (pid [pt])
  (p+ [p1 p2])
  (scalar* [p s]))

(defn same-curve? 
  [p1 p2]
  (and (= (:a p1) (:a p2))
       (= (:b p1) (:b p2))))

(extend-type Number
  FieldElementOp
  (+ [this other] (clojure.core/+ this other))
  (- [this other] (clojure.core/- this other))
  (* [this other] (if (number? other)  (clojure.core/* this other) (scalar* other this)))
  (/ [this other] (int (clojure.core// this other)))
  (** [this exp] (nt/expt this exp))
  (zero? [this] (clojure.core/zero? this)))

;;y ^ 2 = x ^ 3 + a x + b
(defrecord Point [x y a b]
  PointOp
  (pid [_] (Point. nil nil a b))
  (p+ [this p2]
      (assert (same-curve? this p2))
      (cond
        (nil? x) p2
        (nil? (:x p2)) this
        (= this p2)  (if (zero? y)
                       (pid this)
                       (let [s  (/ (+ (* (** x 2) 3) a) (* y 2))
                             x3 (- (** s 2) (* x 2))
                             y3 (- (* s (- x x3)) y)]
                         (Point. x3 y3 a b)))
        :else (if (= x (:x p2))
                (pid this)
                (let [s  (/ (- (:y p2) y) (- (:x p2) x))
                      x3 (- (- (** s 2) x) (:x p2))]
                  (Point. x3 (- (* s (- x x3)) y) a b)))))
  (scalar* [this scalar]
           (loop [coeff scalar
                  current this
                  result (pid this)]
             (cond
               (zero? coeff) result
               (even? coeff)  (recur (quot coeff 2) (p+ current current) result)
               :else (recur (quot coeff 2) (p+ current current) (p+ result current)))))
  FieldElementOp
  (+ [this other] 
     (p+ this other))
  (* 
   [this other] (when (number? other) (scalar* this other))))


(defn ->Point
  [x y a b]
  (when (or (nil? x) (nil? y) (= (** y 2) (+ (+ (** x 3) (* a x)) b)))
    (Point. x y a b)))

(defn valid? 
  [x y a b]
  (some? (->Point x y a b)))

(defn order-group
  [pt]
  (count (take-while #(not (=  pt (second %))) (iterate (fn [[i r]] [(inc i) (scalar* pt i)]) [2 nil]))))

#_ (+ (->Point 2 5 5 7) (->Point -1 -1 5 7))
;;Execrcise 4
#_ (+ (->Point 2 5 5 7) (->Point -1 -1 5 7))

;;Exercise 6
#_ (+ (->Point -1 -1 5 7) (->Point -1 -1 5 7))

#_ (** 2 2)

#_ (scalar* (->Point 2 5 5 7) 2) 


