(ns programming-bitcoin.transactions
  "Chapters 5 6 7"
  (:require [programming-bitcoin.helper :as h]
            [buddy.core.bytes :as bytes]
            [buddy.core.codecs :as codecs]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import java.io.InputStream))

(defrecord Tx
  [version tx-ins tx-outs lock-time testnet?])

(defn ->Tx
 ([version tx-ins tx-outs lock-time]
  (Tx. version  tx-ins tx-outs lock-time false))
  
 ([version tx-ins tx-outs lock-time testnet?]
  (Tx. version  tx-ins tx-outs lock-time testnet?)))

(defrecord TxIn
  [prev-tx prev-index script-sig sequence])

(defn ->TxIn
  ([prev-tx prev-index]
   (->TxIn prev-tx prev-index nil 0xffffffff))
  ([prev-tx prev-index script-sig sequence]
   (TxIn. prev-tx prev-index script-sig sequence)))

(defrecord TxOut
  [amount script-pubkey])


(defn get-url
  [testnet?]
  (if testnet?
    "http://testnet.programmingbitcoin.com"
    "http://mainnet.programmingbitcoin.com"))

(declare parse-tx)
(defn fetch-tx
  [tx-id & [testnet?]]
  (let [url (format "%s/tx/%s.hex" (get-url testnet?) tx-id)
        raw (codecs/hex->bytes (str/trim (slurp url)))] 
    (parse-tx (io/input-stream raw) testnet?)))

(def fetch-tx (memoize fetch-tx))

(defn parse-script
  [])

(defn hash
  [tx]
  )

(defn parse-tx-in
  [^InputStream s]
  (let [prev-tx (byte-array (reverse (h/read-bytes s 32)))
        prev-index (h/le-bytes->number (h/read-bytes s 4))
        script-sig nil
        sequence (h/le-bytes->number (h/read-bytes s 4))]
    (->TxIn prev-tx prev-index script-sig sequence)))

(defn serialize-tx-in
  [{:keys [prev-tx prev-index script-sig sequence]}]
  (bytes/concat (reverse prev-tx)
                (h/number->le-bytes prev-index 4)
                nil
                (h/number->le-bytes sequence 4)))

(defn fetch-tx-in 
  [{:keys [prev-tx]} & {:keys [testnet?] :or {testnet? false}}]
  (fetch-tx (codecs/bytes->hex prev-tx) testnet?))

(defn value-tx-in 
  [{:keys [prev-index] :as tx-in} & {:keys [testnet?] :or {testnet? false}}]
  (-> (fetch-tx-in tx-in :testnet? testnet?)
      (:tx-outs)
      (nth prev-index)
      :amount))

(defn parse-tx-out
  [^InputStream s]
  (let [amount (h/le-bytes->number (h/read-bytes s 8))
        script-pubkey nil]
    (TxOut. amount script-pubkey)))

(defn serialize-tx-out
  [{:keys [amount script-pubkey]}]
  (bytes/concat (h/number->le-bytes amount 8)
                (byte-array [])))

(defn parse-tx
  [^InputStream s & {:keys [testnet?] :or {testnet? false}}]
  (let [version (h/le-bytes->number (h/read-bytes s 4))
        num-inputs (h/read-varint s)
        tx-ins (vec (for [_ (range num-inputs)] (parse-tx-in s)))
        num-outputs (h/read-varint s)
        tx-outs (vec (for [_ (range num-outputs)] (parse-tx-out s)))
        lock-time (h/le-bytes->number (h/read-bytes s 4))]
    (->Tx version tx-ins tx-outs lock-time testnet?)))

(defn serialize-tx
  [{:keys [version tx-ins tx-outs lock-time testnet?]}]
  (byte-array (concat 
               (h/number->le-bytes version 4)
               (h/encode-varint (count tx-ins))
               (vec (for [tx-in tx-ins] (serialize-tx-in tx-in)))
               (h/encode-varint (count tx-outs))
               (vec (for [tx-out tx-outs] (serialize-tx-out tx-out)))
               (h/number->le-bytes lock-time 4))))

(defn fee [{:keys [tx-ins tx-outs testnet?]}]
  (let [input-sum (apply + (map #(value-tx-in % :testnet? testnet?) tx-ins))
        output-sum (apply + (map :amount tx-outs))]
    (- input-sum output-sum)))