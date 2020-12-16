(ns  programming-bitcoin.elliptic-curve-crypto
  (:require [clojure.math.numeric-tower :as nt]
            [buddy.core.mac :as mac]
            [programming-bitcoin.finite-fields :as f  :refer [+ - *]]
            [programming-bitcoin.elliptic-curve :as p]
            [programming-bitcoin.helper :refer [number->bytes bytes->number]]))

(def P (clojure.core/- (nt/expt 2 256) (nt/expt 2 32) 977))

(defn ->S256-Field
  [num]
  (let [prime P]
    #_(assert (.isProbablePrime prime 1))
    (f/->FieldElement num prime)))


(def A (->S256-Field 0))
(def B (->S256-Field 7))

(defn ->S256-Point
  [x y]
  (let [x (if (number? x)(->S256-Field x) x)
        y (if (number? y)(->S256-Field y) y)]
    (p/->Point x y A B)))

(def N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)
(def G (->S256-Point 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
                 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))

;;helpers
(defn pprint-s256-point
  [{:keys [x y]}]
   (str "S256-Point(" (f/pprint-field x) ","  (f/pprint-field y) ")"))
;;;

(defn verify-signature
  [public-key z {:keys [r s]}]
  (let [s-inv (f/powmod s (- N 2) N)
        u (mod (* z s-inv) N)
        v (mod (* r s-inv) N)
        total (+ (* v public-key)(* u G)) ]
    (= (get-in total [:x :num]) r)))

(defrecord Signature [r s])
(defrecord PrivateKey [secret point]) 
(defn ->PrivateKey [secret]
  (PrivateKey. secret (* secret G)))

(defn deterministic-k 
  [secret z]
  (let [k (byte-array 32 (byte 0))
        v (byte-array 32 (byte 1))
        z (if (> z N) (- z N) z)
        z-bytes (number->bytes z 32)
        secret-bytes (number->bytes secret 32)
        k (mac/hash  (byte-array (concat v [0] secret-bytes z-bytes)) {:key k :alg :hmac+sha256})
        v (mac/hash v {:key k :alg :hmac+sha256})
        k (mac/hash (byte-array (concat v [1] secret-bytes z-bytes)) {:key k
                                                                      :alg :hmac+sha256})]
    (loop [k k
           v (mac/hash v {:key k :alg :hmac+sha256})
           candidate (bytes->number v)]
      (if (and (>= candidate 1) (<= candidate N))
        candidate
        (let [k (mac/hash (byte-array (concat v [0]) {:key k :alg :hmac+sha256}))
              v (mac/hash v {:key k :alg :hmac+sha256})]
          (recur k v (bytes->number v)))))))

(defn sign 
  [{:keys [secret]} z]
  (let  [k (deterministic-k secret z)
        r (get-in (* k G) [:x :num])
        k-inv (f/powmod k (- N 2) N)
        s (mod (* (+ z (* r secret)) k-inv) N)]
    (if (> s (quot N 2)) 
      (->Signature r (- N s))
      (->Signature r s))))
