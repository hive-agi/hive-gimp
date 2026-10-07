(ns hive-gimp.py.result-test
  "The eval protocol: golden, property and mutation over `result/parse` and
   the eval module the facade builds from `result/wrap`."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-gimp.py :as py]
            [hive-gimp.py.ast :as ast]
            [hive-gimp.py.result :as result]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; parse: stdout -> EvalResult
;; =============================================================================

(def ^:private JsonValue
  "Values that survive a JSON round trip with keyword keys unchanged."
  [:or :nil :boolean :string :int
   [:vector {:gen/max 3} [:or :int :string :boolean]]
   [:map-of {:gen/max 3} :keyword [:or :int :string :nil]]])

(def ^:private gen-plain-line
  (gen/fmap #(str/replace % "\u001e" "") gen/string-alphanumeric))

(def ^:private gen-stdout
  "Printed lines with the sentinel line somewhere among them."
  (gen/let [before (gen/vector gen-plain-line 0 3)
            after  (gen/vector gen-plain-line 0 2)
            data   (mg/generator JsonValue)]
    (str/join "\n" (concat before [(str result/sentinel (json/write-str data))] after))))

(defn- sentinel-lines [stdout]
  (filter #(str/starts-with? % result/sentinel) (str/split-lines stdout)))

(deftrifecta parse hive-gimp.py.result/parse
  {:golden-path "test/golden/hive_gimp/py/result_parse.edn"
   :cases       {:empty      ""
                 :nil-stdout nil
                 :value-only (str result/sentinel "[2168, 2096]")
                 :printed    (str "hello\n" result/sentinel "{\"id\": 3, \"type\": \"Layer\", \"name\": \"P\"}\nbye")
                 :last-wins  (str result/sentinel "1\n" result/sentinel "2")
                 :no-value   "just printing"}
   :gen         gen-stdout
   :pred        #(hs/validate result/EvalResult %)
   :mutations   [["first-sentinel-wins"
                  (fn [s] (assoc (result/parse s) :data
                                 (some-> (first (sentinel-lines (or s "")))
                                         (subs (count result/sentinel))
                                         (json/read-str :key-fn keyword))))]
                 ["sentinel-kept-in-stdout" (fn [s] (assoc (result/parse s) :stdout (or s "")))]
                 ["string-keys"
                  (fn [s] (update (result/parse s) :data
                                  #(if (map? %) (update-keys % name) %)))]]})

(defspec parse-recovers-the-printed-value-and-the-rest 200
  (prop/for-all [before (gen/vector gen-plain-line 0 3)
                 data   (mg/generator JsonValue)]
    (= {:data data :stdout (str/join "\n" before)}
       (result/parse (str/join "\n" (conj before (str result/sentinel (json/write-str data))))))))

;; =============================================================================
;; wrap: the eval module, through the facade's eval-source
;; =============================================================================

(def ^:private gen-name (gen/elements '[a b img Gimp/get-images x-y]))

(def ^:private gen-expr-form
  (gen/recursive-gen
   (fn [x] (gen/one-of [(gen/vector x 0 2)
                        (gen/fmap (fn [[f args]] (apply list f args)) (gen/tuple gen-name (gen/vector x 0 2)))
                        (gen/fmap (fn [[a b]] (list '+ a b)) (gen/tuple x x))]))
   (gen/one-of [gen-name (gen/elements [nil true 0 1 "s" 2.5])])))

(def ^:private gen-forms
  (gen/vector (gen/one-of [gen-expr-form
                           (gen/fmap (fn [[t v]] (list 'def t v)) (gen/tuple (gen/elements '[a b]) gen-expr-form))])
              0 3))

(def ^:private print-line
  "print((\"\\u001ehive-gimp.py \" + __hg_json.dumps(__hg_r, default=__hg_default)))")

(defn- well-formed-eval? [src]
  (let [lines (str/split-lines src)]
    (and (= "import json as __hg_json" (first lines))
         (= print-line (last lines))
         (str/starts-with? (last (butlast lines)) "__hg_r = "))))

(deftrifecta eval-source hive-gimp.py/eval-source
  {:golden-path "test/golden/hive_gimp/py/result_eval_source.edn"
   :cases       {:empty     []
                 :one-value '[(Gimp/get-images)]
                 :lead      '[(def img (first (Gimp/get-images))) [(.get-width img) (.get-height img) layer]]
                 :nil-last  '[(f) nil]
                 :statement '[(import json) (json/dumps {"a" 1})]}
   :gen         gen-forms
   :pred        well-formed-eval?
   :mutations   [["prelude-dropped" (fn [fs] (str/join "\n" (drop 5 (str/split-lines (py/eval-source fs)))))]
                 ["value-never-printed" (fn [fs] (str/join "\n" (butlast (str/split-lines (py/eval-source fs)))))]
                 ["last-form-as-statement" (fn [fs] (py/eval-source (conj (vec fs) nil)))]]})

(deftest the-wrapped-module-is-a-valid-ast
  (is (ast/valid? (result/wrap [{:py/node :expr-stmt :expr {:py/node :name :id "f"}}]
                               {:py/node :name :id "x"})))
  (is (ast/valid? (result/wrap [] nil)) "no last form binds None"))

(deftest schemas-are-registered
  (is (= result/EvalResult (get (hs/registered) :hive-gimp.py.result/eval-result))))
