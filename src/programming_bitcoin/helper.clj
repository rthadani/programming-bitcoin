(ns programming-bitcoin.helper
  (:require [buddy.core.hash :as hash]
            [clojure.java.io :as io])
  (:import java.io.InputStream))

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

(defn read-bytes 
  [^InputStream stream length]
  (byte-array (for [_ (range length)] (.read stream))))

(defn read-varint
  [^InputStream s]
  (let [i (.read s)]
    (case i
      0xfd (le-bytes->number (read-bytes s 2))
      0xfe (le-bytes->number (read-bytes s 4))
      0xff (le-bytes->number (read-bytes s 8))
      i)))

(defn encode-varint
  [i]
  (condp #(< %2 %) i 
         0xfd (byte-array [(unchecked-byte i)])
         0x10000 (byte-array (cons (unchecked-byte 0xfd) (number->le-bytes i 2)))
         0x100000000  (byte-array (cons (unchecked-byte 0xfe) (number->le-bytes i 4)))
         0x10000000000000000  (byte-array (cons (unchecked-byte 0xfe) (number->le-bytes i 8)))
         (throw (ex-info "Integer too large" {:varint i}))))

(defn unsigned-byte 
  [b]
  (if (neg? b) (+ 256 b) b))

(defn hexify 
  [b]
  (apply str
    (map #(format "%02x" (byte %)) b)))
