(ns programming-bitcoin.elliptic-curve)


(defprotocol EllipticOp
  (identity [pt])
  (e+ [p1 p2]))

(defn same-curve? 
  [p1 p2]
  (and (= (:a p1) (:a p2))
       (= (:b p1) (:b p2))))

;;y ^ 2 = x ^ 3 + a x + b
(defrecord Point [x y a b]
  EllipticOp
  (identity [_] (Point. nil nil a b))
  (e+ [this p2] (cond
                  (nil? x) p2
                  (nil? (:x p2)) this)))


(defn make-point 
  [x y a b]
  (when (= (*  y y) (+ (* x x x) (* a x) b))
    (Point. x y a b)))

(defn valid? 
  [x y a b]
  (some? (make-point x y a b)))

