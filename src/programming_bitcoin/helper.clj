(ns programming-bitcoin.helper
            (:require [buddy.core.hash :as hash]))

(defn hash-256
  [m]
  (hash/sha256  (hash/sha256 m)))

(defn number->bytes ;;big-endian encoding python to_bytes
  [n length]
  (let [ba     (.toByteArray (biginteger n))
        byte-array-len (count ba)
        zeroes         (repeat (- length byte-array-len) (byte 0))]
    (if (> byte-array-len length)
      (byte-array (drop (- byte-array-len length) (seq ba)))
      (byte-array (concat zeroes ba)))))

(defn bytes->number
  [b]
  (biginteger (byte-array (into [0] b))))

(defn hex64 
  [n] 
  (format "0x%064x" (biginteger n)))

(defn hash-160
  [s]
  (hash/ripemd160 (hash/sha256 s)))

(defn number->le-bytes
  [n length]
  (let [a (.toByteArray (biginteger n)),
        l (count a),
        zeros (repeat (- length l) (byte 0))]
    (if (> l length) 
      (byte-array (reverse (drop (- l length) (seq a))))
      (byte-array (reverse (concat zeros a))))))


(defn le-bytes->number
  "little endian decoding" 
  [b]
  (bytes->number (reverse b)))