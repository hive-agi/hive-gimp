(ns hive-gimp.py.render-module-trifecta-test
  "render/literal and render/module as trifectas. Literals: one golden case
   per literal rule of docs/py-strata.md, a property over values generated
   from :hive-gimp.py/literal-value, a mutant per rule. Modules: whole blocks,
   the property every generated valid Module must hold (renders, deterministic,
   balanced, indented by fours, one top-level line per statement at least),
   and mutants of the joining."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-gimp.py.render :as r]
            [hive-gimp.py.trifecta-support :as s :refer [n nm lit call stmt]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; render/literal
;; =============================================================================

(def literal-cases
  {:nil            nil
   :true           true
   :false          false
   :string         "logo v2"
   :string-slash   "s/t"
   :string-escapes "q\"\n\\"
   :string-unicode "funerária"
   :keyword        :a
   :keyword-ns     :a/b
   :long           42
   :negative       -7
   :bigint         (bigint 9)
   :ratio          1/2
   :double         2.5
   :bigdec         2.5M
   :nan            ##NaN
   :inf            ##Inf
   :-inf           ##-Inf
   :vector         [1 [2 "x"]]
   :list           '(1 2)
   :map            (array-map "a" 1 :b nil)
   :map-empty      {}
   :set-empty      #{}
   :set-one        #{1}
   :nested         (array-map :a [1 1/2 nil true] :b #{})})

(def ^:private original-literal (deref #'r/literal))

(defn- literal-on
  "A mutant of render/literal that answers `bad` when `pred` holds."
  [pred bad]
  (fn [v] (if (pred v) (bad v) (original-literal v))))

(defn- literal-holds?
  "Deterministic, balanced, one line; a string reads back as itself."
  [v out]
  (and (string? out)
       (= out (r/literal v))
       (s/balanced? out)
       (not (str/includes? out "\n"))
       (or (not (string? v)) (= v (json/read-str out)))))

(deftrifecta render-literal hive-gimp.py.render/literal
  {:golden-path   "test/golden/hive_gimp/py/render_literal.edn"
   :cases         literal-cases
   :gen           s/gen-literal
   :property-type :pred-io
   :pred          literal-holds?
   :num-tests     300
   :mutations
   [["nil-null"           (literal-on nil? (constantly "null"))]
    ["bool-lowercase"     (literal-on boolean? str)]
    ["slash-escaped"      (literal-on string? #(json/write-str %))]
    ["string-single-quoted" (literal-on string? #(str "'" % "'"))]
    ["keyword-keeps-colon" (literal-on keyword? #(json/write-str (str %)))]
    ["keyword-drops-ns"   (literal-on keyword? #(json/write-str (name %)))]
    ["ratio-as-division"  (literal-on ratio? str)]
    ["nan-bare"           (literal-on #(and (double? %) (Double/isNaN %)) (constantly "nan"))]
    ["inf-bare"           (literal-on #(and (double? %) (Double/isInfinite %)) #(if (pos? %) "inf" "-inf"))]
    ["bigint-suffixed"    (literal-on #(instance? clojure.lang.BigInt %) pr-str)]
    ["empty-set-braces"   (literal-on #(and (set? %) (empty? %)) (constantly "{}"))]
    ["map-equals"         (literal-on map? #(str "{" (str/join ", " (map (fn [[k v]] (str (r/literal k) "=" (r/literal v))) %)) "}"))]
    ["vector-tuple"       (literal-on sequential? #(str "(" (str/join ", " (map r/literal %)) ")"))]
    ["sequence-no-space"  (literal-on sequential? #(str "[" (str/join "," (map r/literal %)) "]"))]]})

;; =============================================================================
;; render/module
;; =============================================================================

(def module-cases
  {:empty     []
   :one       [(n :import :module "json")]
   :sequence  [(n :import :module "json")
               (n :assign :target (nm "a") :value (lit 1))
               (stmt (call "print" [(nm "a")]))]
   :blocks    [(n :def :name "f" :params {:fixed ["x"]}
                  :body [(n :for :target (nm "i") :iter (call "range" [(nm "x")])
                            :body [(n :if :test (nm "i") :body [])])
                         (n :return :value (nm "x"))])
               (stmt (call "f" [(lit 3)]))]
   :raw-lines [(n :raw-stmt :code "x = 1\ny = 2") (stmt (nm "x"))]})

(defn- module-holds?
  "Every generated valid Module renders, deterministically, balanced,
   indented by fours, with at least one column-zero line per statement."
  [m out]
  (and (string? out)
       (= out (r/module m))
       (s/balanced? out)
       (s/indentation-ok? out)
       (>= (s/top-level-lines out) (count m))))

(deftrifecta render-module hive-gimp.py.render/module
  {:golden-path   "test/golden/hive_gimp/py/render_module.edn"
   :cases         module-cases
   :gen           s/gen-module
   :property-type :pred-io
   :pred          module-holds?
   :num-tests     300
   :mutations
   [["crlf"            #(str/join "\r\n" (r/stmts %))]
    ["blank-separated" #(str/join "\n\n" (r/stmts %))]
    ["trailing-newline" #(str (str/join "\n" (r/stmts %)) (when (seq %) "\n"))]
    ["drops-last"      #(str/join "\n" (r/stmts (butlast %)))]
    ["reversed"        #(str/join "\n" (r/stmts (reverse %)))]]})
