(ns programming-bitcoin.finite-fields)

(defprotocol FieldElementOp
  (+ [f1 f2])
  (- [f1 f2])
  (* [f1 f2])
  (/ [f1 f2])
  (** [f exp])
  (zero? [f]))

(defn same-set? [f1 f2]
  (= (:prime f1) (:prime f2)))

(defn powmod [b e m]
  (.modPow (biginteger b) (biginteger e) (biginteger m)))

(defrecord FieldElement [num prime]
  FieldElementOp
  (+ [this other]  (and (same-set? this other) 
                        (FieldElement. (mod (clojure.core/+ num (:num other)) prime) prime)))
  (- [this other] (and (same-set? this other) 
                        (FieldElement. (mod (clojure.core/- num (:num other)) prime) prime)))
  (* [this other] (if (number? other)
                     (FieldElement. (mod (clojure.core/* num other) prime) prime)
                     (and (same-set? this other) (FieldElement. (mod (clojure.core/* num (:num other)) prime) prime))))
  (/ [this other] (and (same-set? this other) 
                          (-> (powmod (:num other) (clojure.core/- prime 2) prime)
                              (clojure.core/* num)
                              (mod prime) 
                              (FieldElement. prime))))
  (** [this exp] (FieldElement. (let [exp (mod exp (dec prime))] 
                                  (powmod num exp prime)) 
                                  prime))
  (zero? [_] (clojure.core/zero? (mod num prime))))

;;Fermat's test for primes
(defn prime?
  [p]
  (->> (repeatedly #(inc (biginteger (rand (dec p)))))
       (take 50)
       (every? #(= (powmod % p p) %))))

(defn ->FieldElement [num prime]
  (when (and (prime? prime) (< num prime) (>= num 0))
    (FieldElement. num prime)))

(defn pprint-field
  [{:keys [num]}]
  (when num
    (format "0x%064x" (biginteger num))))


#_ (->FieldElement 7 19)
#_ (same-set? (->FieldElement 7 19) (->FieldElement 8 19))
#_ (+ (->FieldElement 7 13) (->FieldElement 12 13))
#_ (* (->FieldElement 3 13) (->FieldElement 12 13))
#_ (* (->FieldElement 3 13) 3)
#_ (** (->FieldElement 3 13) 3)
#_ (powmod 5 2 3)
#_ (/ (->FieldElement 2 19) (->FieldElement 7 19))

;;Excercise 8
#_ (/ (->FieldElement 3 31) (->FieldElement 24 31))
#_ (** (->FieldElement 17 31) -3)
#_ (-> (->FieldElement 4 31)
       (** -4)
        (* (->FieldElement 11 31)))


