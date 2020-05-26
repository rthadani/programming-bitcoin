(ns programming-bitcoin.finite-fields)

(defprotocol FieldElementOp
  (+f [f1 f2])
  (-f [f1 f2])
  (*f [f1 f2])
  (divf [f1 f2])
  (powf [f exp])
  (zerof? [f]))

(defn same-set? [f1 f2]
  (= (:prime f1) (:prime f2)))

(defn powmod [b e m]
  (->> (repeat e b) 
       (reduce (fn [r b]  (mod (* r b) m)) 1)))

(defrecord FieldElement [num prime]
  FieldElementOp
  (+f [this other] (println this other) (and (same-set? this other) 
                        (FieldElement. (mod (+ num (:num other)) prime) prime)))
  (-f [this other] (and (same-set? this other) 
                        (FieldElement. (mod (- num (:num other)) prime) prime)))
  (*f [this other] (if (number? other)
                     (FieldElement. (mod (* num other) prime) prime)
                     (and (same-set? this other) (FieldElement. (mod (* num (:num other)) prime) prime))))
  (divf [this other] (and (same-set? this other) 
                          (-> (powmod (:num other) (- prime 2) prime)
                              (* num)
                              (mod prime) 
                              int
                              (FieldElement. prime))))
  (powf [this exp] (FieldElement. (let [exp (mod exp (dec prime))] 
                                    (int (powmod num exp prime))) 
                                  prime))
  (zerof? [_] (zero? (mod num prime))))

;;Fermat's test for primes
(defn prime?
  [p]
  (->> (repeatedly #(inc (rand-int (dec p))))
       (take 50)
       (every? #(= (powmod % p p) %))))

(defn make-field-element [num prime]
  (if (and (prime? prime) (< num prime) (>= num 0))
    (FieldElement. num prime)
    nil))



#_ (make-field-element 7 19)
#_ (same-set? (make-field-element 7 19) (make-field-element 8 19))
#_ (+f (make-field-element 7 13) (make-field-element 12 13))
#_ (*f (make-field-element 3 13) (make-field-element 12 13))
#_ (*f (make-field-element 3 13) 3)
#_ (powf (make-field-element 3 13) 3)
#_ (powmod 5 2 3)
#_ (divf (make-field-element 2 19) (make-field-element 7 19))

;;Excercise 8
#_ (divf (make-field-element 3 31) (make-field-element 24 31))
#_ (powf (make-field-element 17 31) -3)
#_ (-> (make-field-element 4 31)
       (powf -4)
        (*f (make-field-element 11 31)))


