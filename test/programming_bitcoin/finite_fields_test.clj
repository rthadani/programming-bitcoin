(ns programming-bitcoin.finite-fields-test
  (:require [midje.sweet :refer :all]
            [programming-bitcoin.finite-fields :refer :all]))

(facts "Finite field base tests"
       (fact "Field elements can be created with primes"
             (->FieldElement 7 19) => {:num 7 :prime 19})
       (fact "Two fields with the same prime belong to the same set"
             (same-set? (->FieldElement 7 19) (->FieldElement 8 19)) => truthy))

(facts "Finite fields - Exercise 8"       
       (fact "Division"
             (/ (->FieldElement 3 31) (->FieldElement 24 31)) => {:num 4 :prime 31})
       (fact "negative Exponent"
            (** (->FieldElement 17 31) -3) => {:num 29 :prime 31})
       (fact "Complex op"
             (-> (->FieldElement 4 31)
                 (** -4)
                 (* (->FieldElement 11 31))) => {:num 13 :prime 31}))
