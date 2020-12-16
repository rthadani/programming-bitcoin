(ns programming-bitcoin.serialization-test
 (:require [midje.sweet :refer :all]
           [programming-bitcoin.serialization :refer :all]
           [programming-bitcoin.elliptic-curve-crypto :refer :all]
           [buddy.core.codecs :refer [hex->bytes]]))

(tabular "Exercise 1"
         (fact "uncompressed sec"
               (hexify (sec (:point (->PrivateKey ?secret)) false)) => ?expected)
         ?secret ?expected
         5000 "04ffe558e388852f0120e46af2d1b370f85854a8eb0841811ece0e3e03d282d57c315dc72890a4f10a1481c031b03b351b0dc79901ca18a00cf009dbdb157a1d10"
         (biginteger (Math/pow 2018 5)) "04027f3da1918455e03c46f659266a1bb5204e959db7364d2f473bdf8f0a13cc9dff87647fd023c13b4a4994f17691895806e1b40b57f4fd22581a4f46851f3b06"
         0xdeadbeef12345 "04d90cd625ee87dd38656dd95cf79f65f60f7273b67d3096e68bd81e4f5342691f842efa762fd59961d0e99803c61edba8b3e3f7dc3a341836f97733aebf987121")

(tabular "Exercise 2"
         (fact "compressed sec"
               (hexify (sec (:point (->PrivateKey ?secret)))) => ?expected)
         ?secret ?expected
         5001 "0357a4f368868a8a6d572991e484e664810ff14c05c0fa023275251151fe0e53d1" 
         (biginteger (Math/pow 2019 5)) "02722e88f24ec2695b13e3d0f52ad4e180acdee2fe9e1f2cb947cc25395fbc8691" 
         0xdeadbeef54321 "0296be5b1292f6c856b3c5654e886fc13511462059089cdf9c479623bfcbe77690")

(tabular "Exercise 4"
         (fact "test base 58"
               (encode-base58 (hex->bytes ?hex)) => ?expected)
         ?hex ?expected
         "7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d" "9MA8fRQrT4u8Zj8ZRd6MAiiyaxb2Y1CMpvVkHQu5hVM6"
         "eff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c" "4fE3H2E6XMp4SsxtwinF7w9a34ooUrwWe4WsW1458Pd"
         "c7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6" "EQJsjkd6JaGwxrjEhfeqPenqHwrBmPQZjJGNSCHBkcF7")

(tabular "Exercise 5"
         (fact "addresses for private key"
               (point->address (:point (->PrivateKey ?secret)) :compressed? ?compressed :testnet? ?testnet) => ?expected)
         ?secret ?compressed ?testnet ?expected
         5002 false true "mmTPbXQFxboEtNRkwfh6K51jvdtHLxGeMA"
         (biginteger (Math/pow 2020 5)) true true "mopVkxp8UhXqRYbCYJsbeE1h1fiF64jcoH"
         (biginteger 0x12345deadbeef) true false "1F1Pn2y6pDb68E5nYJJeba4TLg2U7B6KF1")