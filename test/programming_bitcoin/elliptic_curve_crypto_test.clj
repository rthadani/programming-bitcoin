(ns programming-bitcoin.elliptic-curve-crypto-test
  (:require [midje.sweet :refer :all]
            [programming-bitcoin.elliptic-curve :refer :all]
            [programming-bitcoin.finite-fields :refer :all]))

#_(tabular "ECC Scalar multiplication"
       (fact "Exercise 4"
             (let [prime 223
                   a (->FieldElement 0 prime)
                   b (->FieldElement 7 prime)
                   x (->FieldElement 47 prime)
                   y (->FieldElement 71 prime)
                   point (->Point x y a b)]
               (scalar* point ?scalar) => ?expected
               
               ?scalar  ?expected
               2 => {:x 1 :y 2 :a {:num 0 :prime prime} :b {:num 4 :prime prime}}
               )))
