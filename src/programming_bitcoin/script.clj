(ns programming-bitcoin.script
  (:require [programming-bitcoin.helper :as h]
            [programming-bitcoin.op :as op]
            [buddy.core.bytes :as bytes]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io])
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
         cmds (seq cmds)]
    (if (empty? cmds) 
      (byte-array result)
      (let [cmd (first cmds)]
        (if (number? cmd)
          (recur (concat result (h/number->le-bytes cmd 1)) (rest cmds))
          (let [length (count cmd)
                cmds (rest cmds)]
            (cond
              (< length 75) (recur (concat result (h/number->le-bytes length 1) cmd) cmds)
              (and (> length 75) (< length 0x100)) (recur (concat result (h/number->le-bytes 76 1) (h/number->le-bytes length 1) cmd) cmds)
              (and (> length 0x100) (<= length 520)) (recur (concat result (h/number->le-bytes 77 1) (h/number->le-bytes length 2) cmd) cmds)
              :else (throw (ex-info "Too long a cmd" {:cmd    cmd
                                                      :length length})))))))))

(defn serialize
  [script]
  (let [result (raw-serialize script)
        total (h/encode-varint (count result))]
    (bytes/concat total result))) 

(defn p2sh-script? [[cmd0 cmd1 cmd2 :as cmds]]
  (and (= (count cmds) 3)
       (= cmd0 0xa9) (bytes? cmd1) (= (count cmd1) 20) (= cmd2 0x87)))

(defn evaluate-p2sh
  [[cmd0 cmd1 cmd2 :as cmds] cmd ^Stack stack]
  (if (p2sh-script? cmds)
    #_(and (= (count cmds) 3)
           (= cmd0 0xa9)
           (bytes? cmd1)
           (= cmd2 0x87))
    (if (not (op/op-hash160 stack)) 
      false
      (do 
        (.push stack cmd1)
        (cond
          (not (op/op-equal stack)) false
          (not (op/op-verify stack)) (do (log/info "bad p2sh h160") false)
          :else (let [redeem-script (bytes/concat  (h/encode-varint (count cmd)) cmd)
                      stream (io/input-stream redeem-script)]
                  (parse stream)))))
    cmds))


(defn evaluate
   [script z]
  (let [stack (Stack.)
        alt-stack (Stack.)
        result (loop [cmds (seq script)]
                 (if (empty? cmds)
                   (and (not (.empty stack)) (not= (.peek stack) (byte-array [])))
                   (let [cmd (first cmds)
                         cmds (rest cmds)]
                     (if (not (number? cmd))
                       (do
                         (.push stack cmd)
                         (let [cmds (evaluate-p2sh cmds cmd stack)]
                           (recur cmds)))
                       (let [cmd (h/unsigned-byte cmd)
                             operation (op/opcode-functions cmd)]
                         (log/debug (str "Executing " cmd operation) stack (.size stack))
                         (cond
                           (not operation) false
                           (#{99 100} cmd) (and (operation stack cmds) (recur cmds))
                           (#{107 108} cmd) (and (operation stack alt-stack) (recur cmds))
                           (#{172 173 174 175} cmd) (and (operation stack z) (recur cmds))
                           :else (and (operation stack) (recur cmds))))))))]
    (cond
      (not result) false
      (zero? (.size stack)) false
      (empty? (.pop stack)) false
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

(defn p2pkh->script
  [h160]
  [0x76 0xa9 h160 0x88 0xac])