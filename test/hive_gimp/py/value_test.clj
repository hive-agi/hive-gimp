(ns hive-gimp.py.value-test
  "The spliced value: golden, property and mutation over `value/lit`."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-gimp.py.value :as value]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftrifecta lit hive-gimp.py.value/lit
  {:golden-path "test/golden/hive_gimp/py/lit.edn"
   :cases       {:nil nil :string "s/t" :keyword :a/b :vector [1 2.5] :map {"a" [true]} :set #{1}}
   :xf          (fn [l] [(value/lit? l) (:value l)])
   :gen         (mg/generator value/LiteralValue)
   :pred        (fn [l] (and (value/lit? l) (hs/validate value/SplicedValue l)))
   :mutations   [["identity" identity]
                 ["drops-the-value" (fn [_] (value/->Lit nil))]
                 ["plain-map" (fn [v] {:value v})]]})

(defspec a-spliced-value-keeps-its-value 200
  (prop/for-all [v (mg/generator value/LiteralValue)]
    (= v (:value (value/lit v)))))

(deftest schemas-are-registered-and-generate-what-they-validate
  (is (= value/SplicedValue (get (hs/registered) :hive-gimp.py.value/lit)))
  (is (= value/LiteralValue (get (hs/registered) :hive-gimp.py.value/literal-value)))
  (is (every? #(hs/validate value/SplicedValue %) (mg/sample value/SplicedValue {:size 20})))
  (is (not (hs/validate value/SplicedValue {:value 1})) "a map shaped like a Lit is not one"))
