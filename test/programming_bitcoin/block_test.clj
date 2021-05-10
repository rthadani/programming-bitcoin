(ns programming-bitcoin.block-test
  (:require [midje.sweet :refer :all]
           [programming-bitcoin.block :refer :all]
           [programming-bitcoin.helper :as h]
           [buddy.core.codecs :refer [hex->bytes]]))

(let [b (parse (clojure.java.io/input-stream (hex->bytes "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d")))]
(tabular "BIP tests"
 (fact "Exercise 6 7 8"
       (?bip b) => ?expected)
       ?bip ?expected
       bip9? TRUTHY
       bip91? FALSEY
       bip141? TRUTHY)) 