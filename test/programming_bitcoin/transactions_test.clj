(ns programming-bitcoin.transactions-test
 (:require [midje.sweet :refer :all]
            [programming-bitcoin.helper :refer :all]
            [programming-bitcoin.elliptic-curve-crypto :refer :all]
            [programming-bitcoin.script :as script]
            [programming-bitcoin.transactions :as tx]
            [programming-bitcoin.serialization :refer :all]
            [buddy.core.codecs :as c]
            [buddy.core.bytes :as bytes]))

(let [tx (clojure.string/replace "010000000456919960ac691763688d3d3bcea9ad6ecaf875df5339e
148a1fc61c6ed7a069e010000006a47304402204585bcdef85e6b1c6af5c2669d4830ff86e42dd
205c0e089bc2a821657e951c002201024a10366077f87d6bce1f7100ad8cfa8a064b39d4e8fe4e
a13a7b71aa8180f012102f0da57e85eec2934a82a585ea337ce2f4998b50ae699dd79f5880e253
dafafb7feffffffeb8f51f4038dc17e6313cf831d4f02281c2a468bde0fafd37f1bf882729e7fd
3000000006a47304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1c
dc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a716012
1035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937feffffff567
bf40595119d1bb8a3037c356efd56170b64cbcc160fb028fa10704b45d775000000006a4730440
2204c7c7818424c7f7911da6cddc59655a70af1cb5eaf17c69dadbfc74ffa0b662f02207599e08
bc8023693ad4e9527dc42c34210f7a7d1d1ddfc8492b654a11e7620a0012102158b46fbdff65d0
172b7989aec8850aa0dae49abfb84c81ae6e5b251a58ace5cfeffffffd63a5e6c16e620f86f375
925b21cabaf736c779f88fd04dcad51d26690f7f345010000006a47304402200633ea0d3314bea
0d95b3cd8dadb2ef79ea8331ffe1e61f762c0f6daea0fabde022029f23b3e9c30f080446150b23
852028751635dcee2be669c2a1686a4b5edf304012103ffd6f4a67e94aba353a00882e563ff272
2eb4cff0ad6006e86ee20dfe7520d55feffffff0251430f00000000001976a914ab0c0b2e98b1a
b6dbf67d4750b0a56244948a87988ac005a6202000000001976a9143c82d7df364eb6c75be8c80
df2b3eda8db57397088ac46430600" #"\n" "")
      stream (clojure.java.io/input-stream (buddy.core.codecs/hex->bytes tx))
      parsed-tx (tx/parse-tx stream)]
  (fact "Chapter - 6 - Exercise 5 - Script Sig"
        (script/to-string (:script-sig ((:tx-ins parsed-tx) 1))) => "304402207899531a52d59a6de200179928ca900254a36b8dff8bb75f5f5d71b1cdc26125022008b422690b8461cb52c3cc30330b23d574351872b7c361e9aae3649071c1a71601 035d5c93d9ac96881f19ba1f686f15f009ded7c62efe85a872e6a19b43c15a2937 ")
  (fact "Chapter-6 - Exercise 5 - Script Pubkey"
        (-> (:tx-outs parsed-tx)
            (first)
            (:script-pubkey)
            (script/to-string)) => "OP_DUP OP_HASH160 ab0c0b2e98b1ab6dbf67d4750b0a56244948a879 OP_EQUALVERIFY OP_CHECKSIG "))

   (fact "Chapter 7 - Am i creating money"
      (let [raw-tx (c/hex->bytes "0100000001813f79011acb80925dfe69b3def355fe914bd1d96a3f5f71bf8303c6a989c7d1000000006b483045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed01210349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278afeffffff02a135ef01000000001976a914bc3b654dca7e56b04dca18f2566cdaf02e8d9ada88ac99c39800000000001976a9141c4bc762dd5423e332166702cb75f40df79fea1288ac19430600")
            stream (clojure.java.io/input-stream raw-tx)
            transaction (tx/parse-tx stream)]
        (>= (tx/fee transaction) 0) => TRUTHY))

   (fact "Chapter 7 - verify signature"
      (let [sec (c/hex->bytes "0349fc4e631e3624a545de3f89f5d8684c7b8138bd94bdd531d2e213bf016b278a")
            der  (c/hex->bytes "3045022100ed81ff192e75a3fd2304004dcadb746fa5e24c5031ccfcf21320b0277457c98f02207a986d955c6e0cb35d446a89d3f56100f4d7f67801c31967743a9c8e10615bed")
            z  0x27e0c5994dec7824e56dec6b2fcb342eb7cdb0d0957c2fce9882f715e85d81a6
            point (parse-sec sec)
            signature (parse-der der)]
        (verify-signature point z signature) => TRUTHY))

   (fact "Chapter 7 - Make and sign transaction"
      (let [prev-tx (c/hex->bytes "0d6fe5213c0b3291f208cba8bfb59b7476dffacc4e5cb66f6eb20a080843a299")
            prev-index 13
            tx-in (tx/->TxIn prev-tx prev-index)
            change-amount (int (* 0.33 100000000))
            change-h160 (decode-base-58 "mzx5YhAH9kNHtcN481u6WkjeHjYtVeKVh2")
            change-script (script/p2pkh->script change-h160)
            change-output (tx/->TxOut change-amount change-script)
            target-amount (* 0.1 100000000)
            target-h160 (decode-base-58 "mnrVtF8DWjMu839VW3rBfgYaAfKk8983Xf")
            target-script (script/p2pkh->script target-h160)
            target-output (tx/->TxOut target-amount target-script)
            tx-obj (tx/->Tx 1 [tx-in] [change-output target-output] 0 true)
            private-key (->PrivateKey 8675309)
            tx-signed (tx/sign-input tx-obj 0 private-key)
            serialized (tx/serialize-tx tx-signed) ]
            (tx/->clj tx-obj) => {:version 1 
                                      :tx-ins "0d6fe5213c0b3291f208cba8bfb59b7476dffacc4e5cb66f6eb20a080843a299:13"
                                      :tx-outs "33000000:OP_DUP OP_HASH160 d52ad7ca9b3d096a38e752c2018e6fbc40cdf26f OP_EQUALVERIFY OP_CHECKSIG ,1.0E7:OP_DUP OP_HASH160 507b27411ccf7f16f10297de6cef3f291623eddf OP_EQUALVERIFY OP_CHECKSIG " 
                                      :lock-time 0}
            (tx/->clj tx-obj) => (tx/->clj tx-signed)
            (tx/verify-tx tx-signed) => TRUTHY
            (tx/verify-tx tx-obj) => FALSEY
            (nil? serialized) => FALSEY))

(fact "Chapter 8 - Exercise 4 Validate second signature"
      (let [hex-tx  (clojure.string/replace "0100000001868278ed6ddfb6c1ed3ad5f8181eb0c7a385aa0836f01d5e4789e6
bd304d87221a000000db00483045022100dc92655fe37036f47756db8102e0d7d5e28b3beb83a8
fef4f5dc0559bddfb94e02205a36d4e4e6c7fcd16658c50783e00c341609977aed3ad00937bf4e
e942a8993701483045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8eef
53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e754022
01475221022626e955ea6ea6d98850c994f9107b036b1334f18ca8830bfff1295d21cfdb702103
b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb7152aeffffffff04
d3b11400000000001976a914904a49878c0adfc3aa05de7afad2cc15f483a56a88ac7f40090000
0000001976a914418327e3f3dda4cf5b9089325a4b95abdfa0334088ac722c0c00000000001976
a914ba35042cfe9fc66fd35ac2224eebdafd1028ad2788acdc4ace020000000017a91474d691da
1574e6b3c192ecfb52cc8984ee7b6c568700000000" #"\n" "")
            hex-sec "03b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7fbdbd4bb71"
            hex-der (clojure.string/replace "3045022100da6bee3c93766232079a01639d07fa869598749729ae323eab8ee
f53577d611b02207bef15429dcadce2121ea07f233115c6f09034c0be68db99980b9a6c5e754022" #"\n" "")
            hex-redeem-script (clojure.string/replace "475221022626e955ea6ea6d98850c994f9107b036b1334f18ca88
30bfff1295d21cfdb702103b287eaf122eea69030a0e9feed096bed8045c8b98bec453e1ffac7f
bdbd4bb7152ae" #"\n" "")
            sec (c/hex->bytes hex-sec)
            der (c/hex->bytes hex-der)
            redeem-script (script/parse (clojure.java.io/input-stream (c/hex->bytes hex-redeem-script)))
            stream (clojure.java.io/input-stream (c/hex->bytes hex-tx))
            tx-obj (tx/parse-tx  stream)
            z (tx/sig-hash tx-obj 0 redeem-script)
            point (parse-sec sec)
            sig (parse-der der)]
            (verify-signature point z sig) => TRUTHY))
