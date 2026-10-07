(ns hive-gimp.py.ast-trifecta-test
  "ast/valid? as a trifecta: golden verdicts over conforming and malformed
   nodes (test/golden/hive_gimp/py/ast_valid.edn), the property that every
   value generated from the registered schemas validates, and mutants of the
   validator the goldens must kill."
  (:require [clojure.test.check.generators :as gen]
            [hive-gimp.py.ast :as ast]
            [hive-gimp.py.trifecta-support :as s :refer [n nm lit]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private x (nm "x"))

(def cases
  {:ok-name            x
   :ok-dotted-name     (nm "Gimp.get_images")
   :ok-call            (n :call :fn x :args [(lit 1)] :kwargs [["k" (lit "v")]])
   :ok-comp            (n :comp :element x :clauses [{:for (n :tuple :items [x x]) :in x} {:if x}])
   :ok-lambda          (n :lambda :params {:fixed ["a"] :rest "r"} :body x)
   :ok-slice-absent    (n :slice :object x)
   :ok-return-absent   (n :return)
   :ok-try             (n :try :body [] :handlers [{:type x :name "e" :body []}])
   :ok-module          [(n :import :module "json") (n :expr-stmt :expr x)]
   :ok-module-empty    []
   :bad-missing-key    (n :call :fn x :args [])
   :bad-extra-key      (assoc x :ns "Gimp")
   :bad-node-kind      (n :walrus :target x)
   :bad-name-spelling  (nm "get-images")
   :bad-attr-object    (n :attr :object "x" :attr "y")
   :bad-binop-arity    (n :binop :op "+" :operands [x])
   :bad-binop-op       (n :binop :op "===" :operands [x x])
   :bad-unop-op        (n :unop :op "!" :operand x)
   :bad-block-expr     (n :if :test x :body [x])
   :bad-target-literal (n :for :target (lit 1) :iter x :body [])
   :bad-literal-symbol (n :literal :value 'sym)
   :bad-kwarg-name     (n :call :fn x :args [] :kwargs [["k-v" x]])
   :bad-module-expr    [x]})

(def ^:private original (deref #'ast/valid?))

(def ^:private gen-any-node
  (gen/one-of [s/gen-expr s/gen-stmt s/gen-module]))

(deftrifecta ast-valid hive-gimp.py.ast/valid?
  {:golden-path "test/golden/hive_gimp/py/ast_valid.edn"
   :cases       cases
   :gen         gen-any-node
   :pred        true?
   :num-tests   300
   :mutations
   [["always-true"     (fn ([_] true) ([_ _] true))]
    ["always-false"    (fn ([_] false) ([_ _] false))]
    ["everything-expr" (fn ([v] (original ast/Expr v)) ([sch v] (original sch v)))]
    ["vectors-unchecked" (fn ([v] (or (vector? v) (original v))) ([sch v] (original sch v)))]
    ["open-maps"       (fn ([v] (original (if (map? v) (select-keys v [:py/node :id :fn :args :kwargs :object :attr :op :operands :test :body :target :iter :value :params :element :clauses :handlers :module :names]) v)))
                         ([sch v] (original sch v)))]]})
