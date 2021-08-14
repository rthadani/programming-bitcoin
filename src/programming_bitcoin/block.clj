(ns programming-bitcoin.block
  (:require [programming-bitcoin.helper :as h]
            [clojure.math.numeric-tower :as n])
  (:import java.io.InputStream))

(defrecord Block [version previous-block merkle-root timestamp bits nonce])

(defn parse 
  [^InputStream stream]
  (let [version     (h/le-bytes->number (h/read-bytes stream 4))
        prev-block  (byte-array (reverse (h/read-bytes stream 32)))
        merkle-root (byte-array (reverse (h/read-bytes stream 32)))
        timestamp   (h/le-bytes->number (h/read-bytes stream 4))
        bits        (h/read-bytes stream 4)
        nonce       (h/read-bytes stream 4)]
    (->Block version prev-block merkle-root timestamp bits nonce)))

(defn serialize 
  [{:keys [version previous-block merkle-root timestamp bits nonce]}]
  (byte-array (concat (h/number->le-bytes version 4) 
                      (reverse previous-block)
                      (reverse merkle-root) 
                      (h/number->le-bytes timestamp 4)
                      bits 
                      nonce)))

(defn hash
  [block]
  (-> (serialize block)
      h/hash-256
      reverse
      byte-array))


(defn bip9?
  [{:keys [version]}]
  (-> (biginteger version)
      (.shiftRight 29)
      (= 1)))

(defn bip91?
  [{:keys  [version]}]
   (-> (biginteger version)
       (.shiftRight 4)
       (.and BigInteger/ONE)
       (= 1)))

(defn bip141? 
  [{:keys [version]}]
  (-> (biginteger version)
      (.shiftRight 1)
      (.and BigInteger/ONE)
      (= 1)))

(defn difficulty 
  [{:keys [bits]}]
  (/ (*' 0xffff (n/expt 256 (- 0x1d 3))) (h/bits->target bits)))


(defn valid-pow?
  [{:keys [bits] :as block}]
  (< (h/le-bytes->number (h/hash-256 (serialize block))) (h/bits->target bits)))
