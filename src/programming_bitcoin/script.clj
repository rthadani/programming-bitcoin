(ns programming-bitcoin.script
  (:require [programming-bitcoin.helper :as h]
            [programming-bitcoin.op :as op]
            [buddy.core.bytes :as bytes]
            [clojure.tools.logging :as log])
  (:import java.io.InputStream
           java.util.Stack))

(defn parse
  [^InputStream s]
  (let [length (h/read-varint s)
        [cmds ctr] (loop [cmds []
               counter 0]
          (if (>= counter length)
            [cmds counter]
            (let [current-byte (first (h/read-bytes s 1))]
              (cond
                (and (>= current-byte 1) (<= current-byte 75))
                (recur (conj cmds (h/read-bytes s (int current-byte))) (+ counter  1 (int current-byte)))
                (= current-byte 76)
                (let [data-length (h/le-bytes->number (h/read-bytes s 1))]
                  (recur (conj cmds (h/read-bytes s data-length)) (+ counter 2 data-length)))
                (= current-byte 77)
                (let [data-length (h/le-bytes->number (h/read-bytes s 2))]
                  (recur (conj cmds (h/read-bytes s data-length)) (+ counter 3 data-length)))
                :else (recur (conj cmds current-byte) (inc counter))))) )]
    (if (not= ctr length)
      (throw (ex-info "Failed to parse script" {:cmds cmds :counter ctr :length length}))
      cmds)))

(defn raw-serialize
  [cmds]
  (loop [result []
         cmds cmds]
    (if (empty? cmds) 
      (byte-array result)
      (let [cmd (first cmds)]
        (if (number? cmd)
          (recur (conj result (h/number->le-bytes cmd 1)) (rest cmds))
          (let [length (count cmd)]
            (cond
              (< length 75) (recur (conj result (h/number->le-bytes length 1) cmd) (rest cmds))
              (and (> length 75) (< length 0x100)) (recur (conj result (h/number->le-bytes 76 1) (h/number->bytes length 1) cmd) (rest cmds))
              (and (> length 0x100) (<= length 520)) (recur (conj result (h/number->le-bytes 77 1) (h/number->le-bytes length 2) cmd) (rest cmds))
              :else (throw (ex-info "Too long a cmd" {:cmd    cmd
                                                      :length length})))))))))

(defn serialize
  [script]
  (let [result (raw-serialize script)
        total (h/encode-varint (count result))]
    (bytes/concat total result))) 

(defn evaluate
   [script z]
  (let [stack (Stack.)
        alt-stack (Stack.)]
    (loop [cmds (seq script)]
      (if (empty? cmds) 
        (and (not (.empty stack)) (not= (.peek stack) (byte-array [])))
        (let [cmd (first cmds)
              cmds (rest cmds)]
          (if (not (number? cmd))
            (do 
              (.push stack (h/unsigned-byte cmd))
              (recur cmds))
            (let [cmd (h/unsigned-byte cmd)
                  operation (op/opcode-functions cmd)]
              (log/info (str "Executing " cmd operation) stack)
              (cond
                (not operation) false
                (#{99 100} cmd) (and (operation stack cmds) (recur cmds))
                (#{107 108} cmd) (and (operation stack alt-stack) (recur cmds))
                (#{172 173 174 175} cmd) (and (operation stack z) (recur cmds))
                :else (and (operation stack) (recur cmds))))))))
    (cond 
      (zero? (.size stack)) false
      (empty? (.peek stack)) false
      :else true)))

(defn combine-script
  [cmds1 cmds2]
  (concat cmds1 cmds2))

(defn to-string
  [cmds]
  (if (empty? cmds)
    ""
    (let [cmd (first cmds)]
      (if (number? cmd)
        (if (contains? op/opcode-strings (h/unsigned-byte cmd))
          (str (op/opcode-strings (h/unsigned-byte cmd)) " " (to-string (rest cmds)))
          (str (str "OP_" (int (h/unsigned-byte cmd))) " " (to-string (rest cmds))))
        (str (h/hexify cmd) " " (to-string (rest cmds)))))))

