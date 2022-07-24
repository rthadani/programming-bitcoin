(ns programming-bitcoin.network
  (:require [programming-bitcoin.helper :as h]
            [programming-bitcoin.block :as block]
            [buddy.core.bytes :as bytes]
            [buddy.core.nonce :as nonce]
            [buddy.core.codecs :refer [hex->bytes bytes->hex]]
            [aleph.tcp :as tcp]
            [byte-streams]
            [manifold.stream :as s]
            [clojure.tools.logging :as log]
            [clojure.java.io :as io])
  (:import java.io.InputStream))

(def NETWORK_MAGIC (byte-array (map unchecked-byte [0xf9 0xbe 0xb4 0xd9])))
(def TESTNET_NETWORK_MAGIC (byte-array (map unchecked-byte [0x0b 0x11 0x09 0x07])))

(defmulti serialize-message :command-name)
(defmulti parse-message (fn [{:keys [command-name]} payload] command-name))

(defrecord NetworkEnvelope [magic command payload testnet?])

(defn ->NetworkEnvelope
   [command payload & testnet?]
   (NetworkEnvelope. (if testnet? TESTNET_NETWORK_MAGIC NETWORK_MAGIC) (.getBytes command) payload testnet?))

(defn envelope->edn
  [{:keys [magic command payload]}]
  {:magic   (format "%X" (h/bytes->number magic))
   :command (String. command)
   :payload (h/hexify payload)})

(defn parse-envelope
  [^InputStream stream]
  (let [magic            (h/read-bytes stream 4)
        command          (byte-array (remove #{0} (h/read-bytes stream 12)))
        payload-length   (h/le-bytes->number (h/read-bytes stream 4))
        payload-checksum (h/read-bytes stream 4)
        payload          (h/read-bytes stream payload-length)
        payload-hash     (h/hash-256 payload)]
    (if (not (bytes/equals? (bytes/slice payload-hash 0 4) payload-checksum))
      (throw (ex-info "Invalid payload" 
                      {:magic            (h/hexify magic)
                       :command          (String. command)
                       :payload-length   payload-length
                       :payload-hash     payload-hash
                       :payload-checksum payload-checksum}))
      (cond
        (bytes/equals? magic NETWORK_MAGIC) (NetworkEnvelope. magic command payload false)
        (bytes/equals? magic TESTNET_NETWORK_MAGIC) (NetworkEnvelope. magic command payload true)
        :else (throw (ex-info "Invalid envelope magic" {:magic (h/hexify magic)} ))))))

(defn serialize-envelope
  [{:keys [magic command payload]}]
  (byte-array (concat
               magic
               command 
               (repeat (- 12 (count command)) 0)
               (h/number->le-bytes (count payload) 4)
               (bytes/slice (h/hash-256 payload) 0 4)
               payload)))

(defrecord VersionMessage
           [command-name version services timestamp 
            receiver-services receiver-ip receiver-port
            sender-services sender-ip sender-port 
            nonce user-agent latest-block relay?])

(defn version-message-with-defaults [m]
  (let [{:keys [version services timestamp receiver-services
                receiver-ip receiver-port sender-services sender-ip
                sender-port nonce user-agent latest-block relay?]}
        (merge {:version           70015
                :services          0
                :receiver-services 0
                :receiver-ip       (byte-array (repeat 4 0))
                :receiver-port     8333
                :sender-services   0
                :sender-ip         (byte-array (repeat 4 0))
                :sender-port       8333
                :user-agent        "/programmingbitcoin:0.1/"
                :latest-block      0
                :relay?            false
                :timestamp         (quot (System/currentTimeMillis) 1000)
                :nonce             (nonce/random-bytes 8)}
               m)]
    (->VersionMessage "version" version services timestamp 
                      receiver-services receiver-ip receiver-port 
                      sender-services sender-ip sender-port 
                      nonce user-agent latest-block relay?)))

(defmethod serialize-message "version"
  [{:keys [version services timestamp receiver-services
           receiver-ip receiver-port sender-services sender-ip
           sender-port nonce user-agent latest-block relay?]}]
  (byte-array
   (concat (h/number->le-bytes version 4) 
           (h/number->le-bytes services 8)
           (h/number->le-bytes timestamp 8) (h/number->le-bytes receiver-services 8)
           (hex->bytes "00000000000000000000ffff") receiver-ip
           (h/number->le-bytes receiver-port 2) (h/number->le-bytes sender-services 8)
           (hex->bytes "00000000000000000000ffff") sender-ip
           (h/number->le-bytes sender-port 2) nonce
           (h/encode-varint (count user-agent)) (.getBytes ^String user-agent)
           (h/number->le-bytes latest-block 4)
           (if relay? [1] [0]))))

(defmethod parse-message "version" [_ payload]
  {:command-name "version"
   :payload      payload})


(defrecord VerackMessage [command-name])
(defmethod serialize-message "verack" [_] (byte-array []))
(defmethod parse-message "verack" [_ _] (VerackMessage. "verack"))

(defrecord GetHeadersMessage [command-name version num-hashes start-block end-block])
(defn get-headers-message-with-defaults [m]
  (let [{:keys [version num-hashes start-block end-block]} (merge {:version 70015 
                                                                   :num-hashes 1 
                                                                   :end-block (byte-array (repeat 32 0))} 
                                                                  m)]
    (assert start-block "A start block is required")
    (GetHeadersMessage. "getheaders" version num-hashes start-block end-block)))

(defmethod serialize-message "getheaders" 
  [{:keys [version num-hashes start-block end-block]}]
  (byte-array (concat (h/number->le-bytes version 4)
                      (h/encode-varint num-hashes)
                      (reverse start-block)
                      (reverse end-block))))

(defrecord Headers [command-name blocks])
(defmethod parse-message "headers" [_ ^InputStream payload]
  (let [num-headers (h/read-varint payload) ]
    (->> (for [_ (range num-headers)
               :let [b (block/parse payload)
                     num-txs (h/read-varint payload)]
               _ (assert (zero? num-txs) "number of transactions not zero")]
           b)
         
         (Headers. "headers"))))

;;The node sever cod
(defn simple-node
  [{:keys [host port testnet? logging?]
    :or   {testnet? true 
           host     "testnet.programmingbitcoin.com" 
           logging? false}}]
  (let [port     (if-not port (if (false? testnet?) 8333 18333) port)
        endpoint {:host host
                  :port port}
        stream   @(tcp/client endpoint)]
    (assoc endpoint :duplex-stream stream :input-stream (byte-streams/to-input-stream stream) :logging? logging?)))

(defn send-message
  [{:keys [duplex-stream testnet? logging?]} message]
  (let [envelope (->NetworkEnvelope (:command-name message)
                                    (serialize-message message)
                                    testnet?)]
    (when logging?
      (log/info "Sending envelope" (envelope->edn envelope)))
    @(s/put! duplex-stream (serialize-envelope envelope))))

(defn receive-message
  [{:keys [input-stream logging? testnet?]}]
  (let [{:keys [command payload] :as envelope} (parse-envelope input-stream)]
    (when logging? 
      (log/info "Receiving envelope " (envelope->edn envelope)))
    (parse-message {:command command :testnet? testnet?} (io/input-stream payload))))

(defn wait-for
  [node commands]
  (loop []
    (let [{:keys [command-name] :as  msg} (receive-message node)]
      (cond
        (= command-name "version") (send-message node (VerackMessage. "verack"))
        (= command-name "ping")  (send-message node (assoc msg :command-name "pong"))
        (contains? commands command-name) msg
        :else (recur)))))

(defn handshake
  [node]
  (send-message node (version-message-with-defaults {}))
  (loop [verack-received?   false
         version-receieved? false]
    (when-not (and verack-received? version-receieved?)
      (let [{:keys [command-name]} (wait-for node #{"version" "verack"})]
        (cond 
          (= command-name "version") (recur verack-received? true)
          (= command-name "verack") (recur true version-receieved?) 
          :else (recur verack-received? version-receieved?))))))

(defn validate-blocks
  []
  (let [previous (atom (block/parse (io/input-stream block/GENESIS-BLOCK)))
        hash-previous #(block/hash @previous)
        first-epoch-timestamp (atom (:timestamp @previous))
        expected-bits (atom block/LOWEST-BITS)
        node (simple-node {:host "mainnet.programmingbitcoin.com" :testnet? false :logging? true})
        count (atom 1)]
    (handshake node)
    (dotimes [_ 19]
      (let [get-headers (get-headers-message-with-defaults {:start-block (hash-previous)})
            _ (send-message node get-headers)
            headers (wait-for node #{"headers"})] 
        (doseq [header (:blocks headers)]
          (cond 
            (not (block/valid-pow? header)) (throw (ex-info "Bad PoW at block" {:count @count}))
            (not (bytes/equals? (:previous-block header) (hash-previous))) (throw (ex-info "Discontinuous block" {:count @count}))
            (zero? (mod @count 2016)) (let [time-diff (- (:timestamp @previous) @first-epoch-timestamp)]
                                        (reset! expected-bits (h/calculate-new-bits (:bits @previous) time-diff))
                                        (println (bytes->hex @expected-bits))
                                        (reset! first-epoch-timestamp (:timestamp header))))
          (when (not (bytes/equals? (:bits header) @expected-bits))
            (throw (ex-info "Bad bits at block" {:count @count :expected-bits (bytes->hex @expected-bits) :header-bits (bytes->hex (:bits header))})))
          (reset! previous header)
          (swap! count inc))))))

(comment 
  (validate-blocks))
