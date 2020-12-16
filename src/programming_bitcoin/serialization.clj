(ns programming-bitcoin.serialization
  (:require [programming-bitcoin.elliptic-curve-crypto :as ecc]
            [programming-bitcoin.finite-fields :as ff :refer [+ ** -]]
            [programming-bitcoin.helper :refer [hash-256 number->bytes bytes->number hash-160]]))

;;utils 
(defn hexify 
  [b]
  (apply str
    (map #(format "%02x" (byte %)) b)))

(defn sub-array
  [b s e]
  (byte-array (->> (seq b)
                   (drop s)
                   (take (- e s)))))
;;

(defn sec 
  ([point]
   (sec point true))
  ([point compressed?]
   (if (not compressed?)
     (byte-array (concat [4]
                         (number->bytes (get-in point [:x :num]) 32)
                         (number->bytes (get-in point [:y :num]) 32)))
     (if (even? (get-in point [:y :num]))
       (byte-array (concat [2]
                           (number->bytes (get-in point [:x :num]) 32)))
       (byte-array (concat [3]
                           (number->bytes (get-in point [:x :num]) 32)))))))

(defn parse
  [sec-bytes]
  (let [flag (nth sec-bytes 0)]
    (if 
      (= flag 4) (ecc/->S256-Point (bytes->number (sub-array sec-bytes 1 33))
                                   (bytes->number (sub-array sec-bytes 33 65)))
      (let [x (ecc/->S256-Field (bytes->number (sub-array sec-bytes 1 33)))
            alpha (+ (** x 3) ecc/B)
            beta (** alpha (/ (inc ecc/P) 4))
            even-beta (if (even? (:num beta)) beta (ecc/->S256-Field (- ecc/P (:num beta))))
            odd-beta (if (even? (:num beta)) (ecc/->S256-Field (- ecc/P (:num beta))) beta)]
        (if (= flag 2)
          (ecc/->S256-Point x even-beta)
          (ecc/->S256-Point x odd-beta))) )))

(defn der
  [{:keys [r s]}]
  (let [rbin (.toByteArray (biginteger r))
        rbin (concat [2 (count rbin)] rbin)
        sbin (.toByteArray (biginteger s))
        sbin (concat [2 (sbin)] sbin)]
    (byte-array (concat [0x30 (+ (count rbin) (count sbin))] rbin sbin))))

(def BASE58-ALPHABET "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(defn encode-base58
  [b]
  (let [num-zeroes (count (take-while #(zero? %) b))
        num (bytes->number b)
        prefix (repeat num-zeroes \1)]
    (loop [n num
           result []]
      (if (zero? n)
        (apply str (concat prefix result))
        (recur (quot n 58) (cons (nth BASE58-ALPHABET (rem n 58)) result))))))

(defn encode-base58-checksum
  [b]
  (encode-base58 (byte-array (concat b (take 4 (hash-256 (byte-array b)))))))

(defn hash-sec [s256-point & {:keys [compressed?] :or {compressed? true}}]
  (hash-160 (sec s256-point compressed?)))

(defn point->address
  [s256-point & {:keys [compressed? testnet?] :or {compressed? true testnet? false}}]
  (let [h160 (hash-sec s256-point :compressed? compressed?)]
    (println (seq h160))
    (if testnet?
      (encode-base58-checksum (cons (byte 0x6f) h160))
      (encode-base58-checksum (cons (byte 0x00) h160))))) ;;public key is a s256 point which is the address to send to

(defn secret->wif
  [secret {:keys [compressed? testnet?] :or {compressed? true testnet? false}}]
  (let [secret-bytes (number->bytes secret 32)
        testnet-byte (if testnet? [(byte 0xef)] [(byte 0x80)])
        compressed-byte (if compressed? [(byte 0x01)] [])]
    (encode-base58 (byte-array (concat testnet-byte secret-bytes compressed-byte)))))

#_ (hexify (sec ecc/G))
#_ (parse (sec ecc/G false))
#_ (parse (sec ecc/G))
#_ (parse (sec (:point (ecc/->PrivateKey 5001))))
#_ (point->address (:point (ecc/->PrivateKey (biginteger 5002))) :compressed? false :testnet? true)
#_  (point->address (:point (ecc/->PrivateKey (biginteger (Math/pow 2020 5)))) :compressed? true :testnet? true)
#_  (point->address (:point (ecc/->PrivateKey (biginteger 0x12345deadbeef))) :compressed? true :testnet? false)
