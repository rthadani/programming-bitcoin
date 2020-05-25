(ns programming-bitcoin.elliptic-curve
  (:require [programming-bitcoin.finite-fields :refer [FieldElementOp +f -f *f divf powf]]))


(defprotocol PointOp
  (pid [pt])
  (p+ [p1 p2])
  (scalar* [p1 s]))

(defn same-curve? 
  [p1 p2]
  (and (= (:a p1) (:a p2))
       (= (:b p1) (:b p2))))


(extend-protocol FieldElementOp
 java.lang.Number
  (+f [this other] (+ this other))
  (-f [this other] (- this other))
  (*f [this other] (* this other))
  (divf [this other] (/ this other))
  (powf [this exp] (Math/pow this exp)))

;;y ^ 2 = x ^ 3 + a x + b
(defrecord Point [x y a b]
  PointOp
  (pid [_] (Point. nil nil a b))
  (p+ [this p2]
    (prn this p2)
    (assert (same-curve? this p2))
    (cond
      (nil? x) p2
      (nil? (:x p2)) this
      (= this p2)  (if (zero? y)
                     (pid this)
                     (let [s (divf (+f (*f 3 (powf x 2)) a) (*f 2 y))
                           x3 (-f (*f s s) (*f 2 x))
                           y3 (-f (*f s (-f x x3)) y)]
                       (Point. x3 y3 a b)))
      :else (if (= x (:x p2))
              (pid this)
              (let [s (divf (-f (:y p2) y) (-f (:x p2) x))
                    x3 (-f (-f (*f s s) x) (:x p2))]
                (Point. x3 (-f (*f s (-f x x3)) y) a b)))))
  (scalar* [_ s]
           ))


(defn make-point
  [x y a b]
  (when (= (powf y 2) (+f (+f (powf x 3) (*f a x)) b))
    (Point. x y a b)))

(defn valid? 
  [x y a b]
  (some? (make-point x y a b)))


#_ (p+ (make-point 2 5 5 7) (make-point -1 -1 5 7))
;;Execrcise 4
#_ (p+ (make-point 2 5 5 7) (make-point -1 -1 5 7))

;;Exercise 6
#_ (p+ (make-point -1 -1 5 7) (make-point -1 -1 5 7))

#_ (powf 2 2)