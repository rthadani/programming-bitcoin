(ns programming-bitcoin.network-test
  (:require [midje.sweet :refer :all]
           [programming-bitcoin.network :refer :all]
           [programming-bitcoin.helper :as h]
           [buddy.core.codecs :refer [hex->bytes]]
           [buddy.core.bytes :as bytes] 
           [clojure.java.io :as io]))

(fact "Exercise 2 - Determine network message"
      (let [envelope (parse-envelope (io/input-stream (hex->bytes "f9beb4d976657261636b000000000000000000005df6e0e2")))]
        (String. (:command envelope)) => "verack"))

(fact "Test Envelope Serialize"
      (let [raw (hex->bytes "f9beb4d976657261636b000000000000000000005df6e0e2")
            envelope (parse-envelope (io/input-stream raw))]
        (bytes/equals? raw (serialize-envelope envelope)) => TRUTHY))

(fact "it should serialize version messages"
      (let [v (version-message-with-defaults {:timestamp 0 :nonce (byte-array (repeat 8 0))})]
        (bytes/equals? (serialize-message v) (hex->bytes "7f11010000000000000000000000000000000000000000000000000000000000000000000000ffff000000008d20000000000000000000000000000000000000ffff000000008d200000000000000000182f70726f6772616d6d696e67626974636f696e3a302e312f0000000000"))  => TRUTHY) )
