(ns programming-bitcoin.finite-fields)

(defprotocol FieldElementOp
  (+f [f1 f2])
  (-f [f1 f2])
  (*f [f1 f2])
  (divf [f1 f2])
  (powf [f1 exp]))

(defn same-set? [f1 f2]
  (= (:prime f1) (:prime f2)))

(defn powmod [b e m]
  (->> (repeat e b) 
       (reduce (fn [r b] (println r b) (mod (* r b) m)) 1)))

(defrecord FieldElement [num prime]
  FieldElementOp
  (+f [this other] (and (same-set? this other) 
                        (FieldElement. (mod (+ num (:num other)) prime) prime)))
  (-f [this other] (and (same-set? this other) 
                        (FieldElement. (mod (- num (:num other)) prime) prime)))
  (*f [this other] (and (same-set? this other) 
                        (FieldElement. (mod (* num (:num other)) prime) prime)))
  (divf [this other] (and (same-set? this other) 
                          (-> (powmod (:num other) (- prime 2) prime)
                              (* num)
                              (mod prime) 
                              int
                              (FieldElement. prime))))
  (powf [this exp] (FieldElement. ()(int (powmod num exp prime)) prime)))

(defn make-field-element [num prime]
  (if (<= num prime)
    (FieldElement. num prime)
    nil))


#_ (make-field-element 7 19)
#_ (same-set? (make-field-element 7 19) (make-field-element 8 19))
#_ (+f (make-field-element 7 13) (make-field-element 12 13))
#_ (*f (make-field-element 3 13) (make-field-element 12 13))
#_ (powf (make-field-element 3 13) 3)
#_ (powmod 5 2 3)
#_ (divf (make-field-element 2 19) (make-field-element 7 19))

;;Excercise 8
#_ (divf (make-field-element 3 31) (make-field-element 24 31))
#_ (powf (make-field-element 17 31) -3)
#_ (-> (make-field-element 4 31)
       (powf -4)
        (*f (make-field-element 11 31)))


