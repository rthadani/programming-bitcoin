(ns programming-bitcoin.transactions-test
 (:require [midje.sweet :refer :all]
            [programming-bitcoin.helper :refer :all]
            [programming-bitcoin.elliptic-curve-crypto :refer :all]
            [programming-bitcoin.script :as script]
            [programming-bitcoin.transactions :as tx]
            [programming-bitcoin.serialization :refer :all]
            [buddy.core.codecs :as c ]))

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
        (>= (tx/fee transaction) 0) => truthy))

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
            ]
            (tx/->clj tx-obj) => {:version 1 
                                      :tx-ins "0d6fe5213c0b3291f208cba8bfb59b7476dffacc4e5cb66f6eb20a080843a299:13"
                                      :tx-outs "33000000:OP_DUP OP_HASH160 d52ad7ca9b3d096a38e752c2018e6fbc40cdf26f OP_EQUALVERIFY OP_CHECKSIG ,1.0E7:OP_DUP OP_HASH160 507b27411ccf7f16f10297de6cef3f291623eddf OP_EQUALVERIFY OP_CHECKSIG " 
                                      :lock-time 0}))
