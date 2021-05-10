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
  (byte-array (concat (reverse prev-tx)
                      (h/number->bytes prev-index 4)
                      (script/serialize script-sig)
                      (h/number->le-bytes sequence 4))))

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
               (apply concat (for [tx-in tx-ins] (serialize-tx-in tx-in)))
               (h/encode-varint (count tx-outs))
               (apply concat (for [tx-out tx-outs] (serialize-tx-out tx-out)))
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
  [{:keys [version tx-ins tx-outs lock-time testnet?] :as tx} input-index redeem-script]
  (let [s (concat (h/number->le-bytes version 4) (h/encode-varint (count tx-ins)))
        s (byte-array
           (concat s
                   (apply concat (for [i    (range 0 (count tx-ins))
                                       :let [{:keys [prev-tx prev-index sequence]} (tx-ins i)]]
                                   (serialize-tx-in
                                    (->TxIn prev-tx prev-index (when (= i input-index)
                                                                 (if redeem-script
                                                                   redeem-script
                                                                   (script-pubkey-tx-in (tx-ins i) testnet?))) sequence))))
                   (h/encode-varint (count tx-outs))
                   (apply concat (for [tx-out tx-outs] (serialize-tx-out tx-out)))
                   (h/number->le-bytes lock-time 4)
                   (h/number->le-bytes SIGHASH_ALL 4)))]
    (h/bytes->number (h/hash-256 s))))

(defn verify-tx-input
  [{:keys [tx-ins testnet?] :as tx} input-index]
  (let [tx-in (tx-ins input-index)
        script-pubkey (script-pubkey-tx-in tx-in testnet?)
        redeem-script (when (script/p2sh-script? script-pubkey)
                        (as-> (last (:script-sig tx-in)) $
                          (bytes/concat (h/encode-varint (count $)) $)
                          (script/parse (io/input-stream $))))
        z (sig-hash tx input-index redeem-script)
        combined (concat (:script-sig tx-in) script-pubkey)]
    (script/evaluate combined z)))

(defn sign-input
  [tx input-index private-key]
  (let [z (sig-hash tx input-index nil)
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

(defn coinbase-tx?
  [{:keys [tx-ins]}]
  (and (= (count tx-ins) 1)
       (bytes/equals? (:prev-tx (first tx-ins)) (byte-array (repeat 32 (byte 0))))
       (= (:prev-index (first tx-ins)) 0xffffffff)))

(defn coinbase-height
  [{:keys [tx-ins] :as tx}]
  (when (coinbase-tx? tx)
    (h/le-bytes->number (nth (:script-sig (tx-ins 0)) 0))))
    

#_ (let [hex-tx  (clojure.string/replace "0100000001868278ed6ddfb6c1ed3ad5f8181eb0c7a385aa0836f01d5e4789e6
bd304d87221a000000db00483045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8
fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4e
e942a8993701483045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef
53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e754022
01475221022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb702103
b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb7152aeffffffff04
d3b11400000000001976a914904a49878c0adfc3aa05de7afad2cc15f483a56a88ac7f40090000
0000001976a914418327e3f3dda4cf5b9089325a4b95abdfa0334088ac722c0c00000000001976
a914ba35042cfe9fc66fd35ac2224eebdafd1028ad2788acdc4ace020000000017a91474d691da
1574e6b3c192ecfb52cc8984ee7b6c568700000000" #"\n" "")
         hex-sec "03b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb71"
         hex-der (clojure.string/replace "3045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8ee
f53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e754022" #"\n" "")
         hex-redeem-script (clojure.string/replace "475221022626e955ea6ea6d98850c994f9107b036b1334f18ca88
30bfff1295d21cfdb702103b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7f
bdbd4bb7152ae" #"\n" "")
         sec (codecs/hex->bytes hex-sec)
         der (codecs/hex->bytes hex-der)
         redeem-script (script/parse (clojure.java.io/input-stream (codecs/hex->bytes hex-redeem-script)))
         stream (clojure.java.io/input-stream (codecs/hex->bytes hex-tx))
         tx-obj (parse-tx  stream) 
         i ((:tx-ins tx-obj) 0)
         _ (println i)
         s (bytes/concat (h/number->le-bytes (:version tx-obj) 4)
                         (h/encode-varint (count (:tx-ins tx-obj)))
                         (serialize-tx-in (->TxIn (:prev-tx i) (:prev-index i) redeem-script (:sequence i)))
                         (h/encode-varint (count (:tx-outs tx-obj)))
                         (apply bytes/concat (for [tx-out (:tx-outs tx-obj)] (serialize-tx-out tx-out)))
                         (h/number->le-bytes (:lock-time tx-obj) 4)
                         (h/number->le-bytes SIGHASH_ALL 4))
         z (h/bytes->number (h/hash-256 s)) 
         _ (println z)
         point (ser/parse-sec sec)
         sig (ser/parse-der der)]
         (println (->clj tx-obj))
         (ecc/verify-signature point z sig))