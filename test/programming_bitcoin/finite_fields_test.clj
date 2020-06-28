(ns programming-bitcoin.finite-fields-test
  (:require [midje.sweet :refer :all]
            [programming-bitcoin.finite-fields :refer :all]))

(println "Finite field tests")

(facts "Finite field base tests"
       (fact "Field elements can be created with primes"
             (make-field-element 7 19) => {:num 7 :prime 19})
       (fact "Two fields with the same prime belong to the same set"
             (same-set? (make-field-element 7 19) (make-field-element 8 19)) => truthy))

(facts "Finite fields - Exercise 8"       
       (fact "Division"
             (/ (make-field-element 3 31) (make-field-element 24 31)) => {:num 4 :prime 31})
       (fact "negative Exponent"
            (** (make-field-element 17 31) -3) => {:num 29 :prime 31})
       (fact "Complex op"
             (-> (make-field-element 4 31)
                 (** -4)
                 (* (make-field-element 11 31))) => {:num 13 :prime 31}))




