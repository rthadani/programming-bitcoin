(ns programming-bitcoin.transactions
  "Chapters 5 6 7"
  (:require [programming-bitcoin.helper :as h]
            [programming-bitcoin.script :as script]
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

#_ (def tx (clojure.string/replace "010000000456919960ac691763688d3d3bcea9ad6ecaf875df5339e
148a1fc61c6ed7a069e010000006a47304402204585bcdef85e6b1c6af5c2669d4830ff86e42dd
205c0e089bc2a821657e951c002201024a10366077f87d6bce1f7100ad8cfa8a064b39d4e8fe4e
a13a7b71aa8180f012102f0da57e85eec2934a82a585ea337ce2f4998b50ae699dd79f5880e253
dafafb7feffffffeb8f51f4038dc17e6313cf831d4f02281c2a468bde0fafd37f1bf882729e7fd
3000000006a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1c
dc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a716012
1035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937feffffff567
bf40595119d1bb8a3037c356efd56170b64cbcc160fb028fa10704b45d775000000006a4730440
2204c7c7818424c7f7911da6cddc59655a70af1cb5eaf17c69dadbfc74ffa0b662f02207599e08
bc8023693ad4e9527dc42c34210f7a7d1d1ddfc8492b654a11e7620a0012102158b46fbdff65d0
172b7989aec8850aa0dae49abfb84c81ae6e5b251a58ace5cfeffffffd63a5e6c16e620f86f375
925b21cabaf736c779f88fd04dcad51d26690f7f345010000006a47304402200633ea0d3314bea
0d95b3cd8dadb2ef79ea8331ffe1e61f762c0f6daea0fabde022029f23b3e9c30f080446150b23
852028751635dcee2be669c2a1686a4b5edf304012103ffd6f4a67e94aba353a00882e563ff272
2eb4cff0ad6006e86ee20dfe7520d55feffffff0251430f00000000001976a914ab0c0b2e98b1a
b6dbf67d4750b0a56244948a87988ac005a6202000000001976a9143c82d7df364eb6c75be8c80
df2b3eda8db57397088ac46430600" #"\n" ""))
#_ (count tx)
#_ (let [stream (clojure.java.io/input-stream (buddy.core.codecs/hex->bytes tx))
         parsed-tx (parse-tx stream)]
     (println (script/to-string (:script-sig ((:tx-ins parsed-tx) 1))))
     (println (-> (:tx-outs parsed-tx)
                  (first)
                  (:script-sig)
                  (script/to-string))))