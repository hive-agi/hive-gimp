(ns hive-gimp.py.names-test
  "Symbols as Python names: golden, property and mutation over `sym->py`,
   and the refusal's shape."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-gimp.py.names :as names]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private gen-kebab-symbol
  (gen/let [segments (gen/vector (gen/elements ["get" "images" "a" "b2" "x_y" "Gimp" "set"]) 1 3)
            ns       (gen/elements [nil "Gimp" "Gimp.ChannelOps" "gi.repository"])]
    (symbol ns (str/join "-" segments))))

(deftrifecta sym->py hive-gimp.py.names/sym->py
  {:golden-path "test/golden/hive_gimp/py/sym-to-py.edn"
   :cases       {:plain          'a
                 :kebab          'get-images
                 :qualified      'Gimp/get-images
                 :dotted         'a.b-c
                 :class-constant 'Gimp.ChannelOps/REPLACE}
   :gen         gen-kebab-symbol
   :pred        (fn [py] (and (hs/validate names/PyName py) (not (str/includes? py "-"))))
   :mutations   [["keeps-dashes" (fn [s] (str (when (namespace s) (str (namespace s) ".")) (name s)))]
                 ["drops-namespace" (fn [s] (str/replace (name s) "-" "_"))]
                 ["slash-not-dot" (fn [s] (str/replace (str s) "-" "_"))]]})

(defn- refusal [thunk]
  (try (thunk) nil (catch clojure.lang.ExceptionInfo x [(ex-message x) (ex-data x)])))

(deftest refusals-carry-the-reason-and-the-form
  (doseq [bad ['ok? 'a/b? (symbol "1x") "str" 1 nil]]
    (let [[message data] (refusal #(names/sym->py bad))]
      (is (hs/validate names/Refusal data) (pr-str bad))
      (is (= bad (:form data)))
      (is (re-find #"not a Python name" message)))))

(deftest keyword-arguments-spell-like-names
  (is (= "fill_type" (names/keyword->py :fill-type)))
  (is (hs/validate names/Refusal (second (refusal #(names/keyword->py :bad?))))))

(deftest schemas-are-registered
  (is (= names/PyName (get (hs/registered) :hive-gimp.py.names/py-name)))
  (is (= names/Refusal (get (hs/registered) :hive-gimp.py.names/refusal)))
  (is (not (hs/validate names/PyName "a-b")))
  (is (not (hs/validate names/PyName "x.")))
  (is (hs/validate names/PyName "Gimp.ChannelOps.REPLACE")))
