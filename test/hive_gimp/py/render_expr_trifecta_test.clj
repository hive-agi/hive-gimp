(ns hive-gimp.py.render-expr-trifecta-test
  "render/expr as a trifecta: one golden case per expression rendering rule of
   docs/py-strata.md (byte-exact, test/golden/hive_gimp/py/render_expr.edn),
   a property over expressions generated from :hive-gimp.py/expr, and one
   mutant per rule that the goldens must kill."
  (:require [clojure.string :as str]
            [hive-gimp.py.render :as r]
            [hive-gimp.py.trifecta-support :as s :refer [n nm lit call]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private a (nm "a"))
(def ^:private b (nm "b"))
(def ^:private x (nm "x"))

(def cases
  {:name             (nm "Gimp.get_images")
   :raw              (n :raw :code "1 + 1")
   :literal          (lit {"a" [1 nil] :b "s/t"})
   :attr             (n :attr :object (nm "img") :attr "get_name")
   :call             (n :call :fn (nm "f") :args [a (lit 1)] :kwargs [["w" (lit 32)]])
   :call-empty       (call "f")
   :call-kwargs-only (n :call :fn (nm "f") :args [] :kwargs [["k" a] ["j" b]])
   :subscript        (n :subscript :object (nm "xs") :index (lit 0))
   :slice-lower      (n :slice :object (nm "xs") :lower (lit 1) :upper nil)
   :slice-upper      (n :slice :object (nm "xs") :upper (lit 2))
   :slice-both       (n :slice :object (nm "xs") :lower (lit 1) :upper (lit 2))
   :binop-binary     (n :binop :op "+" :operands [a b])
   :binop-nary       (n :binop :op "*" :operands [a b (lit 2)])
   :binop-word       (n :binop :op "and" :operands [a b])
   :binop-nested     (n :binop :op "*" :operands [(n :binop :op "+" :operands [a (lit 1)]) (lit 2)])
   :unop-minus       (n :unop :op "-" :operand a)
   :unop-not         (n :unop :op "not" :operand a)
   :cond-expr        (n :cond-expr :test a :then b :else (lit nil))
   :lambda           (n :lambda :params {:fixed ["a"] :rest "r"}
                        :body (n :binop :op "+" :operands [a (lit 1)]))
   :lambda-fixed     (n :lambda :params {:fixed ["a" "b"]} :body a)
   :lambda-nullary   (n :lambda :params {:fixed []} :body a)
   :comp             (n :comp
                        :element (n :binop :op "**" :operands [x (lit 2)])
                        :clauses [{:for x :in (call "range" [(lit 5)])}
                                  {:if (n :binop :op "==" :operands
                                          [(n :binop :op "%" :operands [x (lit 2)]) (lit 0)])}])
   :comp-destructure (n :comp :element (nm "l")
                        :clauses [{:for (n :tuple :items [(nm "i") (nm "l")]) :in (nm "xs")}])
   :list             (n :list :items [a b])
   :list-empty       (n :list :items [])
   :tuple            (n :tuple :items [a b])
   :tuple-one        (n :tuple :items [a])
   :tuple-empty      (n :tuple :items [])
   :set              (n :set :items [a])
   :set-empty        (n :set :items [])
   :dict             (n :dict :entries [[(lit "k") a] [(lit 1) b]])
   :dict-empty       (n :dict :entries [])})

(def ^:private original (deref #'r/expr))

(defn- on
  "A mutant of render/expr that answers `bad` for `kind` and defers the rest."
  [kind bad]
  (fn [node] (if (= kind (:py/node node)) (bad node) (original node))))

(defn- items [xs] (str/join ", " (map r/expr xs)))

(defn- expr-holds?
  "Rendering is deterministic, bracket-balanced and single-line."
  [node out]
  (and (string? out)
       (= out (r/expr node))
       (s/balanced? out)
       (not (str/includes? out "\n"))))

(deftrifecta render-expr hive-gimp.py.render/expr
  {:golden-path   "test/golden/hive_gimp/py/render_expr.edn"
   :cases         cases
   :gen           s/gen-expr
   :property-type :pred-io
   :pred          expr-holds?
   :num-tests     300
   :mutations
   [["name-dashes"          (on :name #(str/replace (:id %) "_" "-"))]
    ["raw-parenthesised"    (on :raw #(str "(" (:code %) ")"))]
    ["literal-pr-str"       (on :literal #(pr-str (:value %)))]
    ["attr-arrow"           (on :attr #(str (r/expr (:object %)) "->" (:attr %)))]
    ["call-drops-kwargs"    (on :call #(str (r/expr (:fn %)) "(" (items (:args %)) ")"))]
    ["call-kwargs-colon"    (on :call #(str (r/expr (:fn %)) "("
                                            (str/join ", " (concat (map r/expr (:args %))
                                                                   (map (fn [[k v]] (str k ": " (r/expr v))) (:kwargs %))))
                                            ")"))]
    ["subscript-parens"     (on :subscript #(str (r/expr (:object %)) "(" (r/expr (:index %)) ")"))]
    ["slice-none-bounds"    (on :slice #(str (r/expr (:object %)) "["
                                             (if (:lower %) (r/expr (:lower %)) "None") ":"
                                             (if (:upper %) (r/expr (:upper %)) "None") "]"))]
    ["binop-unparenthesised" (on :binop #(str/join (str " " (:op %) " ") (map r/expr (:operands %))))]
    ["binop-binary-only"    (on :binop #(str "(" (r/expr (first (:operands %))) " " (:op %) " "
                                             (r/expr (second (:operands %))) ")"))]
    ["unop-unparenthesised" (on :unop #(str (:op %) (when (= "not" (:op %)) " ") (r/expr (:operand %))))]
    ["unop-always-spaced"   (on :unop #(str "(" (:op %) " " (r/expr (:operand %)) ")"))]
    ["cond-expr-c-ternary"  (on :cond-expr #(str "(" (r/expr (:test %)) " ? " (r/expr (:then %))
                                                 " : " (r/expr (:else %)) ")"))]
    ["lambda-drops-rest"    (on :lambda #(str "(lambda " (str/join ", " (:fixed (:params %))) ": "
                                              (r/expr (:body %)) ")"))]
    ["lambda-unparenthesised" (on :lambda #(subs (original %) 1 (dec (count (original %)))))]
    ["comp-generator"       (on :comp #(let [o (original %)] (str "(" (subs o 1 (dec (count o))) ")")))]
    ["comp-drops-filters"   (on :comp #(original (update % :clauses (partial filterv :for))))]
    ["list-as-tuple"        (on :list #(str "(" (items (:items %)) ")"))]
    ["tuple-no-trailing-comma" (on :tuple #(str "(" (items (:items %)) ")"))]
    ["tuple-as-list"        (on :tuple #(str "[" (items (:items %)) "]"))]
    ["set-empty-braces"     (on :set #(str "{" (items (:items %)) "}"))]
    ["set-as-list"          (on :set #(str "[" (items (:items %)) "]"))]
    ["dict-equals"          (on :dict #(str "{" (str/join ", " (map (fn [[k v]] (str (r/expr k) "=" (r/expr v)))
                                                                     (:entries %))) "}"))]
    ["dict-as-call"         (on :dict #(str "dict(" (str/join ", " (map (fn [[k v]] (str (r/expr k) ": " (r/expr v)))
                                                                         (:entries %))) ")"))]]})
