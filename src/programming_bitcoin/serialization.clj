(ns programming-bitcoin.serialization
  (:require [programming-bitcoin.elliptic-curve-crypto :as ecc]
            [programming-bitcoin.finite-fields :as ff :refer[+ ** -]]))

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
                         (ecc/number->bytes (get-in point [:x :num]) 32)
                         (ecc/number->bytes (get-in point [:y :num]) 32)))
     (if (even? (get-in point [:y :num]))
       (byte-array (concat [2]
                           (ecc/number->bytes (get-in point [:x :num]) 32)))
       (byte-array (concat [3]
                           (ecc/number->bytes (get-in point [:x :num]) 32)))))))

(defn parse
  [sec-bytes]
  (let [flag (nth sec-bytes 0)]
    (if 
      (= flag 4) (ecc/->S256-Point (ecc/bytes->number (sub-array sec-bytes 1 33))
                                   (ecc/bytes->number (sub-array sec-bytes 33 65)))
      (let [x (ecc/->S256-Field (ecc/bytes->number (sub-array sec-bytes 1 33)))
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
        sbin (concat [2 (count rbin)] rbin)]
    (byte-array (concat [0x30 (+ (count rbin) (count sbin))] rbin sbin))))

(def BASE58-ALPHABET "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(defn encode-base58
  [b]
  (let [num-zeroes (count (take-while #(zero? %) b))
        num (ecc/bytes->number b)
        prefix (repeat num-zeroes \1)]
    (loop [n num
           result []]
      (if (zero? n)
        (apply str (concat prefix result))
        (recur (quot n 58) (cons (nth BASE58-ALPHABET (rem n 58)) result))))))

#_ (hexify (sec ecc/G))
#_ (parse (sec ecc/G false))
#_ (parse (sec ecc/G))
#_ (parse (sec (:point (ecc/->PrivateKey 5001))))
