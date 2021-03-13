(ns programming-bitcoin.op
(:require [programming-bitcoin.helper :as h]
          [programming-bitcoin.serialization :as ser]
          [programming-bitcoin.elliptic-curve-crypto :as ecc]
          [buddy.core.bytes :as bytes]
          [clojure.tools.logging :as log]
          [clojure.math.numeric-tower :as m])
  (:import java.util.Stack))

(declare encode-num)
(declare decode-num)

(defn op-dup 
  [^Stack stack]
  (if (.empty stack) 
    false
    (do (.push stack (.peek stack))
        true)))

(defn op-hash256
  [^Stack stack]
  (if (.empty stack)
    false
    (let [e (.pop stack)]
      (.push stack (h/hash-256 e))
      true)))

(defn op-hash160
  [^Stack stack]
  (if (.empty stack)
    false
    (let [e (.pop stack)]
      (.push stack (h/hash-160 e))
      true)))

(defn op-0
  [^Stack stack]
  (.push stack (encode-num 0)) true)

(defn op-1 [^Stack stack]
  (.push stack (encode-num 1)) true)

(defn op-1negate [^Stack stack]
  (.push stack (encode-num -1)) true)

(defn op-2 [^Stack stack]
  (.push stack (encode-num 2)) true)

(defn op-3 [^Stack stack]
  (.push stack (encode-num 3)) true)

(defn op-4 [^Stack stack]
  (.push stack (encode-num 4)) true)

(defn op-5 [^Stack stack]
  (.push stack (encode-num 5)) true)

(defn op-6 [^Stack stack]
  (.push stack (encode-num 6)) true)

(defn op-7 [^Stack stack]
  (.push stack (encode-num 7)) true)

(defn op-8 [^Stack stack]
  (.push stack (encode-num 8)) true)

(defn op-9 [^Stack stack]
  (.push stack (encode-num 9)) true)

(defn op-10 [^Stack stack]
  (.push stack (encode-num 10)) true)

(defn op-11 [^Stack stack]
  (.push stack (encode-num 11)) true)

(defn op-12 [^Stack stack]
  (.push stack (encode-num 12)) true)

(defn op-13 [^Stack stack]
  (.push stack (encode-num 13)) true)

(defn op-14 [^Stack stack]
  (.push stack (encode-num 14)) true)

(defn op-15 [^Stack stack]
  (.push stack (encode-num 15)) true)

(defn op-16 [^Stack stack]
  (.push stack (encode-num 16)) true)

(defn op-checksig
  [^Stack stack z]
  {:pre [(>= (.size stack) 2)]}
  (let [sec-bytes (.pop stack)
        der-bytes (.pop stack)
        der-bytes (bytes/slice der-bytes 0 (dec (count der-bytes)))
        public-key (try (ser/parse-sec sec-bytes) (catch Exception e e))
        signature (try (ser/parse-der der-bytes) (catch Exception e e))]
    (cond
      (instance? Exception public-key) (do (log/error "Invalid public-key") false)
      (instance? Exception signature) (do (log/error "Invalid signature") false)
      (ecc/verify-signature public-key z signature) (do (op-1 stack) true)
      :else (do (op-0 stack) true))))

(defn op-equal 
  [^Stack stack]
  (if (< (.size stack) 2) 
    false
    (let [t1  (.pop stack)
          t2  (.pop stack)]
      (if (= (vec t1) (vec t2))
        (.push stack (encode-num 1))
        (.push stack (encode-num 0)))
      true)))

(defn op-verify 
  [^Stack stack]
  (cond
    (< (count stack) 1) false
    (= 0 (decode-num (.pop stack))) false
    :else true))

(defn op-equalverify
  [^Stack stack]
  (and (op-equal stack) (op-verify stack)))

(defn op-add 
  [^Stack stack]
  (if (< (.size stack) 2)
    false
      (let [t1 (decode-num (.pop stack))
            t2 (decode-num (.pop stack))]
        (.push stack (encode-num (+ t1 t2)))
        true)))

(defn op-sub 
  [^Stack stack]
  (if (< (.size stack) 2)
    false
      (let [t1 (decode-num (.pop stack))
            t2 (decode-num (.pop stack))]
        (.push stack (encode-num (- t1 t2)))
        true)))

(defn op-mul 
  [^Stack stack]
  (if (< (.size stack) 2)
    false
      (let [t1 (decode-num (.pop stack))
            t2 (decode-num (.pop stack))]
        (.push stack (encode-num (* t1 t2)))
        true)))

(defn encode-num
  [num]
  (if (zero? num)
    (byte-array [])
    (let [abs-num (m/abs num)
          negative (< num 0)
          result (loop [abs-num abs-num r []] (if
                                               (zero? abs-num)
                                                r
                                                (recur (unsigned-bit-shift-right abs-num 8) (conj r (bit-and abs-num 0xff)))))]
      (byte-array
       (if (not (zero? (bit-and (last result) 0x80)))
         (if negative
           (conj result 0x80)
           (conj result 0))
         (if negative
           (conj (butlast result) (bit-or (last result) 0x80))
           result))))))

(defn decode-num
  [element]
  (if (empty? element)
    0
    (let [r (rseq (vec element))
          negative? (not (zero? (bit-and (first r) 0x80)))]
      (loop [result (if negative? (bit-and (first r) 0x7f) (h/unsigned-byte (first r)))
             c (rest r)]
        (if (empty? c)
          (if negative? (* -1 result) result)
          (recur (+ (bit-shift-left result 8) (h/unsigned-byte (first c))) (rest c)))))))

(def opcode-functions
  {0  op-0
   81 op-1
   82 op-2
   83 op-3
   84 op-4
   85 op-5
   86 op-6
   87 op-7
   88 op-8
   89 op-9
   90 op-10
   91 op-11
   92 op-12
   93 op-13
   94 op-14
   95 op-15
   96 op-16
   105 op-verify
   118 op-dup
   135 op-equal
   136 op-equalverify
   147 op-add
   148 op-sub
   149 op-mul
   170 op-hash256
   172 op-checksig
   169 op-hash160})

(def opcode-strings
  {0   "OP_0"
   81  "OP_1"
   82  "OP_2"
   83  "OP_3"
   84  "OP_4"
   85  "OP_5"
   86  "OP_6"
   87  "OP_7"
   88  "OP_8"
   89  "OP_9"
   90  "OP_10"
   91  "OP_11"
   92  "OP_12"
   93  "OP_13"
   94  "OP_14"
   95  "OP_15"
   96  "OP_16"
   105 "OP_VERIFY"
   118 "OP_DUP"
   135 "OP_EQUAL"
   136 "OP_EQUALVERIFY"
   147 "OP_ADD"
   148 "OP_SUB"
   149 "OP_MUL"
   170 "OP_HASH256"
   172 "OP_CHECKSIG"
   169 "OP_HASH160"})
