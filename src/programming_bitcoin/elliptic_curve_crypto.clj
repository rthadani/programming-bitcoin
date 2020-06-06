(ns  programming-bitcoin.elliptic-curve-crypto
  (:require [programming-bitcoin.finite-fields :as f]
            [programming-bitcoin.elliptic-curve :as p]))

(def prime 223)
(def a (f/make-field-element 0 prime))
(def b (f/make-field-element 7 prime))
(def x1 (f/make-field-element 192 prime))
(def y1 (f/make-field-element 105 prime))
(def x2 (f/make-field-element 17 prime))
(def y2 (f/make-field-element 56 prime))
(def p1 (p/make-point x1 y1 a b) )
(def p2 (p/make-point x2 y2 a b) )

;;Exercise 1
#_ (p/p+ p1 p2)
#_ (p/scalar* p1 2)
#_ (let [prime 223
         a (f/make-field-element 0 prime)
         b (f/make-field-element 7 prime)
         x (f/make-field-element 47 prime)
         y (f/make-field-element 71 prime)
         point (p/make-point x y a b)]
     (p/scalar* point 2))


;;Exercise 5
(def x3 (f/make-field-element 15 prime))
(def y3 (f/make-field-element 86 prime))
(def pt (p/make-point x3 y3 a b))
#_ (count (take-while #(not (=  pt (second %))) (iterate (fn [[i r]] [(inc i) (p/scalar* pt i)]) [2 nil])))

(defn s256-field
  [num]
  (let [prime (-> (biginteger (Math/pow 2 256))
                  (- (biginteger (Math/pow 2 32)))
                  (- 977)
                  biginteger)]
    #_(assert (.isProbablePrime prime 1))
    (f/make-field-element num prime)))

(defn s256-point
  [x y]
  (let [a (s256-field 0)
        b (s256-field 7)
        x (s256-field x)
        y (s256-field y)]
    (p/make-point x y a b)))

#_  (s256-field N)


(defonce N (biginteger 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141))
#_ (s256-point (biginteger 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)
                   (biginteger 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))
#_(defonce G (s256-point (biginteger 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798)
                   (biginteger 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8)))