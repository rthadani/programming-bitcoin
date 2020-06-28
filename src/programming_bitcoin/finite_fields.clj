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

(defn make-field-element [num prime]
  (when (and (prime? prime) (< num prime) (>= num 0))
    (FieldElement. num prime)))



#_ (make-field-element 7 19)
#_ (same-set? (make-field-element 7 19) (make-field-element 8 19))
#_ (+ (make-field-element 7 13) (make-field-element 12 13))
#_ (* (make-field-element 3 13) (make-field-element 12 13))
#_ (* (make-field-element 3 13) 3)
#_ (** (make-field-element 3 13) 3)
#_ (powmod 5 2 3)
#_ (/ (make-field-element 2 19) (make-field-element 7 19))

;;Excercise 8
#_ (/ (make-field-element 3 31) (make-field-element 24 31))
#_ (** (make-field-element 17 31) -3)
#_ (-> (make-field-element 4 31)
       (** -4)
        (* (make-field-element 11 31)))


