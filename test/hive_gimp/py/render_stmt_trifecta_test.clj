(ns hive-gimp.py.render-stmt-trifecta-test
  "render/stmt as a trifecta: one golden case per statement rendering rule of
   docs/py-strata.md (byte-exact, test/golden/hive_gimp/py/render_stmt.edn),
   a property over statements generated from :hive-gimp.py/stmt, and one
   mutant per rule that the goldens must kill."
  (:require [clojure.string :as str]
            [hive-gimp.py.render :as r]
            [hive-gimp.py.trifecta-support :as s :refer [n nm lit call stmt]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private a (nm "a"))
(def ^:private b (nm "b"))
(def ^:private i-l (n :tuple :items [(nm "i") (nm "l")]))

(def cases
  {:assign           (n :assign :target (nm "t") :value (n :tuple :items [(lit 1)]))
   :assign-tuple     (n :assign :target i-l :value (nm "x"))
   :assign-attr      (n :assign :target (n :attr :object (nm "o") :attr "x") :value (lit nil))
   :expr-stmt        (stmt (call "f"))
   :if               (n :if :test a :body [(stmt b)])
   :if-empty         (n :if :test a :body [])
   :if-orelse-nil    (n :if :test a :body [(stmt b)] :orelse nil)
   :if-orelse        (n :if :test a :body [(stmt b)] :orelse [(stmt a)])
   :if-orelse-empty  (n :if :test a :body [(stmt b)] :orelse [])
   :for              (n :for :target (nm "x") :iter (nm "xs") :body [(stmt (call "print" [(nm "x")]))])
   :for-destructure  (n :for :target i-l :iter (call "enumerate" [(nm "ls")])
                        :body [(n :if :test (n :binop :op ">" :operands [(nm "i") (lit 0)])
                                  :body [(stmt (call "print" [(nm "i")]))])])
   :for-empty        (n :for :target (nm "x") :iter (nm "xs") :body [])
   :while            (n :while :test a :body [(stmt b)])
   :while-empty      (n :while :test a :body [])
   :def              (n :def :name "half" :params {:fixed ["a" "b"] :rest nil}
                        :body [(n :if :test (n :binop :op ">" :operands [a b])
                                  :body [(n :return :value (n :binop :op "/" :operands [a (lit 2)]))]
                                  :orelse [(n :assign :target (nm "c") :value (n :binop :op "*" :operands [b (lit 2)]))
                                           (n :return :value (nm "c"))])])
   :def-rest         (n :def :name "f" :params {:fixed [] :rest "r"} :body [])
   :return           (n :return :value a)
   :return-nil       (n :return :value nil)
   :return-absent    (n :return)
   :import           (n :import :module "json")
   :import-as        (n :import :module "numpy" :as "np")
   :from-import      (n :from-import :module "gi.repository" :names ["Gio" "Gegl"])
   :try-finally      (n :try :body [(stmt (call "f"))]
                        :handlers [{:type (nm "Exception") :name "e" :body [(stmt (call "print" [(nm "e")]))]}]
                        :finally [(stmt (call "g"))])
   :try-no-finally   (n :try :body [] :handlers [{:type (nm "Exception") :name "e" :body []}] :finally nil)
   :try-two-handlers (n :try :body [(stmt a)]
                        :handlers [{:type (nm "KeyError") :name "k" :body []}
                                   {:type (nm "Exception") :body [(stmt b)]}])
   :try-bare-except  (n :try :body [(stmt a)] :handlers [{:body []}])
   :raw-stmt         (n :raw-stmt :code "x = 1\ny = 2")
   :raw-stmt-one     (n :raw-stmt :code "print(x)")})

(def ^:private original (deref #'r/stmt))

(defn- on
  "A mutant of render/stmt that answers `bad` for `kind` and defers the rest."
  [kind bad]
  (fn [node] (if (= kind (:py/node node)) (bad node) (original node))))

(defn- reindent [lines from to]
  (mapv #(str/replace % (re-pattern (str "^" from)) to) lines))

(defn- stmt-holds?
  "Rendering is deterministic, a vector of balanced lines, and indented by
   fours with every block opened four deeper."
  [node out]
  (let [src (str/join "\n" out)]
    (and (vector? out)
         (seq out)
         (= out (r/stmt node))
         (s/balanced? src)
         (s/indentation-ok? src))))

(deftrifecta render-stmt hive-gimp.py.render/stmt
  {:golden-path   "test/golden/hive_gimp/py/render_stmt.edn"
   :cases         cases
   :gen           s/gen-stmt
   :property-type :pred-io
   :pred          stmt-holds?
   :num-tests     300
   :mutations
   [["indent-two"            #(reindent (original %) "    " "  ")]
    ["indent-tab"            #(reindent (original %) "    " "\t")]
    ["assign-walrus"         (on :assign #(vector (str (r/expr (:target %)) " := " (r/expr (:value %)))))]
    ["expr-stmt-semicolon"   (on :expr-stmt #(vector (str (r/expr (:expr %)) ";")))]
    ["if-empty-block-blank"  (on :if #(if (empty? (:body %)) [(str "if " (r/expr (:test %)) ":")] (original %)))]
    ["if-else-always"        (on :if #(original (update % :orelse (fnil identity []))))]
    ["if-else-never"         (on :if #(original (dissoc % :orelse)))]
    ["if-else-only-nonempty" (on :if #(original (cond-> % (empty? (:orelse %)) (dissoc :orelse))))]
    ["for-of"                (on :for #(update (original %) 0 str/replace " in " " of "))]
    ["while-no-colon"        (on :while #(update (original %) 0 str/replace #":$" ""))]
    ["def-drops-rest"        (on :def #(original (assoc-in % [:params :rest] nil)))]
    ["def-lambda-keyword"    (on :def #(update (original %) 0 str/replace #"^def " "fn "))]
    ["return-none-explicit"  (on :return #(vector (str "return " (if (some? (:value %)) (r/expr (:value %)) "None"))))]
    ["import-drops-alias"    (on :import #(original (dissoc % :as)))]
    ["from-import-no-space"  (on :from-import #(vector (str "from " (:module %) " import " (str/join "," (:names %)))))]
    ["try-except-comma"      (on :try #(mapv (fn [l] (str/replace l " as " ", ")) (original %)))]
    ["try-drops-finally"     (on :try #(original (dissoc % :finally)))]
    ["try-first-handler-only" (on :try #(original (update % :handlers (comp vec (partial take 1)))))]
    ["raw-stmt-one-line"     (on :raw-stmt #(vector (str/replace (:code %) "\n" "; ")))]]})
