(ns programming-bitcoin.finite-fields)

(defprotocol FieldElementOp
  (f+ [other])
  (fdot [other])
  (f- [other])
  (finv []))


(defn make-field-element [num prime]
  (if (>= num prime)))

(defrecord FieldElement [num prime]
  )