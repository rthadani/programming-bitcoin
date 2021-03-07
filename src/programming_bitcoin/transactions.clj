(ns programming-bitcoin.transactions
  "Chapters 5 6 7"
  (:require [programming-bitcoin.helper :as h]
            [programming-bitcoin.script :as script]
            [programming-bitcoin.elliptic-curve-crypto :as ecc]
            [programming-bitcoin.serialization :as ser] 
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
    (parse-tx (io/input-stream raw) :testnet? testnet?)))

(def fetch-tx (memoize fetch-tx))

(defn parse-tx-in
  [^InputStream s]
  (let [prev-tx (byte-array (reverse (h/read-bytes s 32)))
        prev-index (h/le-bytes->number (h/read-bytes s 4))
        script-sig (script/parse s)
        sequence (h/le-bytes->number (h/read-bytes s 4))]
    (->TxIn prev-tx prev-index script-sig sequence)))

(defn serialize-tx-in
  [{:keys [prev-tx prev-index script-sig sequence]}]
  (bytes/concat (reverse prev-tx)
                (h/number->le-bytes prev-index 4)
                (script/serialize script-sig)
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
        script-pubkey (script/parse s)]
    (TxOut. amount script-pubkey)))

(defn serialize-tx-out
  [{:keys [amount script-pubkey]}]
  (bytes/concat (h/number->le-bytes amount 8)
                (script/serialize script-pubkey)))

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
  [{:keys [version tx-ins tx-outs lock-time]}]
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

(defn script-pubkey-tx-in [{:keys [prev-index] :as tx-in} testnet?]
  (let [{:keys [tx-outs]} (fetch-tx-in tx-in :testnet? testnet?)]
    (:script-pubkey (nth tx-outs prev-index))))

(def SIGHASH_ALL 1)
(def SIGHASH_NONE 2)
(def SIGHASH_SINGLE 3)

(defn sig-hash
  [{:keys [version tx-ins tx-outs lock-time testnet?] :as tx} input-index]
  (let [s (concat (h/number->le-bytes version 4) (h/encode-varint (count tx-ins)))
        s (byte-array 
           (concat s
                   (apply concat (for [i    (range 0 (count tx-ins))
                                       :let [{:keys [prev-tx prev-index sequence]} (tx-ins i)]]
                                   (serialize-tx-in
                                    (->TxIn prev-tx prev-index (when (= i input-index) (script-pubkey-tx-in (tx-ins i) testnet?)) sequence))))
                   (h/encode-varint (count tx-outs))
                   (apply concat (for [tx-out tx-outs] (serialize-tx-out tx-out)))
                   (h/number->le-bytes lock-time 4)
                   (h/number->le-bytes SIGHASH_ALL 4)))]
    (h/bytes->number (h/hash-256 s))))

(defn verify-tx-input
  [{:keys [tx-ins testnet?] :as tx} input-index]
  (let [tx-in (tx-ins input-index)
        script-pubkey (script-pubkey-tx-in tx-in testnet?)
        z (sig-hash tx input-index)
        combined (concat (:script-sig tx-in) script-pubkey)]
    (script/evaluate combined z)))

(defn sign-input
  [tx input-index private-key]
  (let [z (sig-hash tx input-index)
       der (ser/der (ecc/sign private-key z))
       sig (bytes/concat der (h/number->bytes SIGHASH_ALL 1))
       sec (ser/sec (:point private-key))
       new-tx (assoc-in tx [:tx-ins input-index :script-sig] [sig sec])]
    (when (verify-tx-input new-tx input-index)
      new-tx)))

(defn verify-tx
  [{:keys [tx-ins] :as tx}]
  (and (> (fee tx) 0) 
       (every? identity (for [i (range 0 (count tx-ins))] (verify-tx-input tx i)))))

(defn ->clj
  [{:keys [version tx-ins tx-outs lock-time]}]
  {:version version
   :tx-ins (str/join "," (map #(str (h/hexify (:prev-tx %)) ":" (:prev-index %))  tx-ins))
   :tx-outs  (str/join "," (map #(str (:amount %) ":" (script/to-string  (:script-pubkey %))) tx-outs))
   :lock-time lock-time})
