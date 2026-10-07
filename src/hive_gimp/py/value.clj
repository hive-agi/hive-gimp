(ns hive-gimp.py.value
  "A Clojure value spliced into Python forms with `~x`: lowered as a literal,
   never read as syntax."
  (:require [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord Lit [value])

(defn lit
  "`v` as a spliced value."
  [v]
  (->Lit v))

(defn lit?
  "True when `x` is a spliced value."
  [x]
  (instance? Lit x))

;; =============================================================================
;; Schemas
;; =============================================================================

(def LiteralValue
  "A Clojure value a Python literal can spell: nil, booleans, strings,
   keywords, integers, finite doubles, and collections of them, two levels
   deep (deeper values spell the same way; the schema bounds what it checks)."
  (let [scalar [:or :nil :boolean :string :keyword :int
                [:double {:gen/NaN? false :gen/infinite? false}]]
        k      [:or :string :keyword :int]
        colls  (fn [x] [[:vector {:gen/max 3} x] [:set {:gen/max 3} x] [:map-of {:gen/max 3} k x]])
        level1 (into [:or scalar] (colls scalar))]
    (into [:or scalar] (concat (colls scalar) (colls level1)))))

(def SplicedValue
  "A `Lit` around any literal value."
  [:fn {:error/message "a spliced value (hive-gimp.py.value/Lit)"
        :gen/schema    LiteralValue
        :gen/fmap      lit}
   lit?])

(hs/register-all! {::literal-value LiteralValue
                   ::lit           SplicedValue})

(m/=> lit [:=> [:cat :any] SplicedValue])
(m/=> lit? [:=> [:cat :any] :boolean])
