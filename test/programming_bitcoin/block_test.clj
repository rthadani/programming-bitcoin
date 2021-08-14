(ns programming-bitcoin.block-test
  (:require [midje.sweet :refer :all]
           [programming-bitcoin.block :refer :all]
           [programming-bitcoin.helper :as h]
           [buddy.core.codecs :refer [hex->bytes]]
           [clojure.java.io :as io] ))

(fact "parses a block correctly"
      (let [parsed-block (parse (io/input-stream (hex->bytes "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d")))
            prev-block (hex->bytes "000000000000000000fd0c220a0a8c3bc5a7b487e8c8de0dfa2373b12894c38e")
            merkle-root (hex->bytes "be258bfd38db61f957315c3f9e9c5e15216857398d50402d5089a8e0fc50075b")]
        (:version parsed-block) => 0x20000002
        (java.util.Arrays/equals (:previous-block parsed-block) prev-block) => TRUTHY
        (java.util.Arrays/equals (:merkle-root parsed-block) merkle-root) => TRUTHY
        (:timestamp parsed-block) => 0x59a7771e
        (java.util.Arrays/equals (:bits parsed-block) (hex->bytes "e93c0118")) => TRUTHY
        (java.util.Arrays/equals (:nonce parsed-block) (hex->bytes "a4ffd71d")) => TRUTHY
        (long (difficulty parsed-block)) => 888171856257
        (h/bits->target (:bits parsed-block)) => 0x13ce9000000000000000000000000000000000000000000)) 

(fact "serializes a block correctly"
      (let [raw-block (hex->bytes "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d")]
      (java.util.Arrays/equals (serialize (parse (io/input-stream raw-block))) raw-block) => TRUTHY))

(let [b (parse (clojure.java.io/input-stream (hex->bytes "020000208ec39428b17323fa0ddec8e887b4a7c53b8c0a0a220cfd0000000000000000005b0750fce0a889502d40508d39576821155e9c9e3f5c3157f961db38fd8b25be1e77a759e93c0118a4ffd71d")))]
(tabular "BIP tests"
 (fact "Exercise 6 7 8"
       (?bip b) => ?expected)
       ?bip ?expected
       bip9? TRUTHY
       bip91? FALSEY
       bip141? TRUTHY)) 

(fact "Exercise 12 New bits given first and last blocks"
     (let [last-block (parse (io/input-stream 
                              (hex->bytes "000000203471101bbda3fe307664b3283a9ef0e97d9a38a7eacd8800000000000000000010c8aba8479bbaa5e0848152fd3c2289ca50e1c3e58c9a4faaafbdf5803c5448ddb845597e8b0118e43a81d3")))
           first-block (parse (io/input-stream
                               (hex->bytes "02000020f1472d9db4b563c35f97c428ac903f23b7fc055d1cfc26000000000000000000b3f449fcbe1bc4cfbcb8283a0d2c037f961a3fdf2b8bedc144973735eea707e1264258597e8b0118e5f00474")))
           time-differential (-' (:timestamp last-block) (:timestamp first-block))
           new-bits (h/calculate-new-bits (:bits last-block) time-differential)]
       (h/hexify new-bits) => "80df6217"
       ) )
