(ns programming-bitcoin.script-test
 (:require [midje.sweet :refer :all]
            [programming-bitcoin.helper :refer :all]
            [programming-bitcoin.script :refer :all]))

(fact "Exercise 3- Unlock ScriptPubKey"
      (let [script-pubkey [0x76 0x76 0x95 0x93 0x56 0x87]
            script-sig [0x52]]
        (evaluate (combine-script script-sig script-pubkey) 0) => truthy))
