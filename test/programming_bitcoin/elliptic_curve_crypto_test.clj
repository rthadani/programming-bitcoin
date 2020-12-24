(ns programming-bitcoin.elliptic-curve-crypto-test
  (:require [midje.sweet :refer :all]
            [programming-bitcoin.elliptic-curve :refer :all]
            [programming-bitcoin.helper :refer :all]
            [programming-bitcoin.finite-fields :refer :all]
            [programming-bitcoin.elliptic-curve-crypto :refer :all]
            [clojure.tools.logging :as log]))
(let [prime 223
      a     (->FieldElement 0 prime)
      b     (->FieldElement 7 prime)]
  (tabular "Points on curve?"
           (fact "Exercise 1"
                 (not (nil? (->Point (->FieldElement ?x prime) (->FieldElement ?y prime) a b))) => ?expected)
           ?x ?y ?expected
           192 105 truthy
           17 56 truthy
           200 19 FALSEY
           1 193 truthy
           42 99 FALSEY)

  (tabular "Point addition"
           (fact "Exercise 2"
                 (+ (->Point (->FieldElement ?x1 prime) (->FieldElement ?y1 prime) a b)
                     (->Point (->FieldElement ?x2 prime) (->FieldElement ?y2 prime) a b)) => ?expected)
           ?x1 ?y1 ?x2 ?y2 ?expected
           143 98 76 66 (->Point (->FieldElement 47 prime)
                                 (->FieldElement 71 prime)
                                 (->FieldElement 0 prime)
                                 (->FieldElement 7 prime))
           47 71 17 56 (->Point (->FieldElement 215 prime)
                                (->FieldElement 68 prime)
                                (->FieldElement 0 prime)
                                (->FieldElement 7 prime))
           170 142 60 139  (->Point (->FieldElement 220 prime)
                                    (->FieldElement 181 prime)
                                    (->FieldElement 0 prime)
                                    (->FieldElement 7 prime))
           )

  (tabular "ECC Scalar multiplication"
           (fact "Exercise 4"
                 (* (->Point (->FieldElement ?x prime) (->FieldElement ?y prime) a b) ?scalar) => ?expected)

           ?x ?y  ?scalar ?expected
           47  71  2 (->Point (->FieldElement 36 prime)
                              (->FieldElement 111 prime)
                              (->FieldElement 0 prime)
                              (->FieldElement 7 prime))

           47  71  4 (->Point (->FieldElement 194 prime)
                              (->FieldElement 51 prime)
                              (->FieldElement 0 prime)
                              (->FieldElement 7 prime))
           47  71  8 (->Point (->FieldElement 116 prime)
                              (->FieldElement 55 prime)
                              (->FieldElement 0 prime)
                              (->FieldElement 7 prime))
           47  71  21 (->Point nil
                               nil
                               (->FieldElement 0 prime)
                               (->FieldElement 7 prime)))
(fact "Exercise 5 Group order"
      (order-group (->Point (->FieldElement 15 prime) (->FieldElement 86 prime) a b)) => 7)) 


(tabular "Verify signatures"
         (fact "Exercise 6"
               (verify-signature (->S256-Point ?x ?y) ?z (->Signature ?r ?s)) => ?expected)
         ?x ?y ?z ?r ?s ?expected
         0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34 0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4 truthy
         0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34 0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6 truthy)

(let [e 12345
      z (bytes->number (hash-256 "Programming Bitcoin!"))
      k 1234567890
      r (get-in (* k G) [:x :num])
      k-inv (powmod k (- N 2) N)
      s (mod (* (+ z (* e r)) k-inv) N)]
  (log/info (pprint-s256-point (* e G)) (hex64 z) (hex64 r) (hex64 s))
  (fact "Exercise 7 Sign message signature hash match "
        (hex64 z) => "0x969f6056aa26f7d2795fd013fe88868d09c9f6aed96965016e1936ae47060d48")
  (fact "Exercise 7 signature random val"
        (hex64 r) => "0x2b698a0f0a4041b77e63488ad48c23e8e8838dd1fb7520408b121697b782ef22")
  (fact "Exercise 7 signature s"
        (hex64 s) => "0x1dbc63bfef4416705e602a7b564161167076d8b20990a0f26f316cff2cb0bc1a")
  (fact "Verify signature"
        (verify-signature (* e G) z (->Signature r s)) => truthy))

(let [private-key (->PrivateKey  12345)
      z (bytes->number (hash-256 "Programming Bitcoin!"))
      signature (sign private-key z)]
  (fact "Signing works as intended"
        (verify-signature (:point private-key) z signature) => truthy))