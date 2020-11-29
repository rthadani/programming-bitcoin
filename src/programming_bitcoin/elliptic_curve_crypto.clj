(ns  programming-bitcoin.elliptic-curve-crypto
  (:require [clojure.math.numeric-tower :as nt]
            [buddy.core.hash :as hash]
            [buddy.core.mac :as mac]
            [programming-bitcoin.finite-fields :as f  :refer [+ - *]]
            [programming-bitcoin.elliptic-curve :as p]))

(defn ->S256-Field
  [num]
  (let [prime (clojure.core/- (nt/expt 2 256) (nt/expt 2 32) 977)]
    #_(assert (.isProbablePrime prime 1))
    (f/->FieldElement num prime)))


(defn ->S256-Point
  [x y]
  (let [a (->S256-Field 0)
        b (->S256-Field 7)
        x (->S256-Field x)
        y (->S256-Field y)]
    (p/->Point x y a b)))

(def N 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141)
(def G (->S256-Point 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
                 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8))
;;helpers
(defn hash-256
  [m]
  (hash/sha256  (hash/sha256 m)))

(defn number->bytes ;;big-endian encoding python to_bytes
  [n length]
  (let [byte-array     (.toByteArray (biginteger n))
        byte-array-len (count byte-array)
        zeroes         (repeat (- length byte-array-len) (byte 0))]
    (if (> byte-array-len length)
      (byte-array (drop (- byte-array-len length) (seq byte-array)))
      (byte-array (concat zeroes byte-array)))))

(defn bytes->number
  [b]
  (biginteger (byte-array (into [0] b))))

(defn hex64 
  [n] 
  (format "0x%064x" (biginteger n)))

(defn pprint-s256-point
  [{:keys [x y]}]
   (str "S256-Point(" (f/pprint-field x) ","  (f/pprint-field y) ")"))
;;;

(defn verify-signature
  [public-key z {:keys [r s]}]
  (let [s-inv (f/powmod s (- N 2) N)
        u (mod (* z s-inv) N)
        v (mod (* r s-inv) N)
        total (p/p+ (* v public-key)(* u G)) ]
    (= (get-in total [:x :num]) r)))

(defrecord Signature [r s])
(defrecord PrivateKey [secret point]) 
(defn ->PrivateKey [secret]
  (PrivateKey. secret (* G secret)))

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

#_ (println (* N G))
#_ (def prime 223)
#_ (def a (f/->FieldElement 0 prime))
#_ (def b (f/->FieldElement 7 prime))
#_ (def x1 (f/->FieldElement 192 prime))
#_ (def y1 (f/->FieldElement 105 prime))
#_ (def x2 (f/->FieldElement 17 prime))
#_ (def y2 (f/->FieldElement 56 prime))
#_ (def p1 (p/->Point x1 y1 a b) )
#_ (def p2 (p/->Point x2 y2 a b) )

;;Exercise 1
#_ (p/p+ p1 p2)
#_ (p/scalar* p1 2)
#_ (let [prime 223
         a (f/->FieldElement 0 prime)
         b (f/->FieldElement 7 prime)
         x (f/->FieldElement 47 prime)
         y (f/->FieldElement 71 prime)
         point (p/->Point x y a b)]
     (p/scalar* point 2))


;;Exercise 5
#_ (def x3 (f/->FieldElement 15 prime))
#_ (def y3 (f/->FieldElement 86 prime))
#_ (def pt (p/->Point x3 y3 a b))
#_ (count (take-while #(not (=  pt (second %))) (iterate (fn [[i r]] [(inc i) (p/scalar* pt i)]) [2 nil])))

;;Exercise 6
#_ (def P (->S256-Point 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
                      0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34))
#_ (def z 0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60)
#_ (def sig (->Signature  0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395 
                          0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4))
#_ (verify-signature P z sig)

#_ (let [z 0x7c076ff316692a3d7eb3c3bb0f8b1488cf72e1afcd929e29307032997a838a3d
         sig (->Signature 0xeff69ef2b1bd93a66ed5219add4fb51e11a840f404876325a1e8ffe0529a2c 0xc7207fee197d27c618aea621406f6bf5ef6fca38681d82b2f06fddbdce6feab6)]
     (verify-signature P z sig))

;;Exercise 7
#_ (let [e 12345
         z (bytes->number (hash-256 "Programming Bitcoin!"))
         k 1234567890
         r (get-in (* k G) [:x :num])
         k-inv (f/powmod k (- N 2) N)
         s (mod (* (+ z (* e r)) k-inv) N)]
     (println (pprint-s256-point (* e G)) (hex64 z) (hex64 r) (hex64 s)))