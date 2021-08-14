(ns programming-bitcoin.network
(:require [programming-bitcoin.helper :as h]
          [buddy.core.bytes :as bytes]
          [buddy.core.nonce :as nonce]
          [buddy.core.codecs :refer [hex->bytes]])
 (:import java.io.InputStream))

(def NETWORK_MAGIC (byte-array (map unchecked-byte [0xf9 0xbe 0xb4 0xd9])))
(def TESTNET_NETWORK_MAGIC (byte-array (map unchecked-byte [0x0b 0x11 0x09 0x07])))

(defrecord NetworkEnvelope [magic command payload testnet?])
#_(defn ->NetworkEnvelope
  [command payload & testnet?]
  (NetworkEnvelope (if testnet? TESTNET_NETWORK_MAGIC NETWORK_MAGIC) command payload testnet?))

(defn parse-envelope
  [^InputStream stream]
  (let [magic (h/read-bytes stream 4)
        command (byte-array (remove #{0} (h/read-bytes stream 12)))
        payload-length (h/le-bytes->number (h/read-bytes stream 4))
        payload-checksum (h/read-bytes stream 4)
        payload (h/read-bytes stream payload-length)
        payload-hash (h/hash-256 payload)]
    (if (not (bytes/equals? (bytes/slice payload-hash 0 4) payload-checksum))
      (throw (ex-info "Invalid payload" 
                      {:magic (h/hexify magic)
                       :command (String. command)
                       :payload-length payload-length
                       :payload-hash payload-hash
                       :payload-checksum payload-checksum}))
      (cond
        (bytes/equals? magic NETWORK_MAGIC)(NetworkEnvelope. magic command payload false)
        (bytes/equals? magic TESTNET_NETWORK_MAGIC) (NetworkEnvelope. magic command payload true)
        :else (throw (ex-info "Invalid envelope magic" {:magic (h/hexify magic)} ))))))

(defn serialize-envelope
  [{:keys [magic command payload] :as envelope}]
  (byte-array (concat
               magic
               command (repeat (- 12 (count command)) 0)
               (h/number->bytes (count payload) 4)
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
        (merge {:version 70015, :services 0, :receiver-services 0
                :receiver-ip (byte-array (repeat 4 0)) :receiver-port 8333
                :sender-services 0, :sender-ip (byte-array (repeat 4 0))
                :sender-port 8333 :user-agent "/programmingbitcoin:0.1/"
                :latest-block 0, :relay? false
                :timestamp (quot (System/currentTimeMillis) 1000)
                :nonce (nonce/random-bytes 8)}
               m)]
    (->VersionMessage "version" version services timestamp 
                      receiver-services receiver-ip receiver-port 
                      sender-services sender-ip sender-port 
                      nonce user-agent latest-block relay?)))


(defmulti serialize-message :command-name)
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

