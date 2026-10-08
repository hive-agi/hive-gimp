(ns hive-gimp.py.ast-test
  "The Python AST's malli schemas: every node shape in docs/py-strata.md is
   accepted, and a node with a missing, extra or mistyped key is refused."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-gimp.py.ast :as ast]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- n [k & kvs] (apply array-map :py/node k kvs))

(def ^:private x (n :name :id "x"))
(def ^:private one (n :literal :value 1))

(def ^:private exprs
  {:name      x
   :literal   (n :literal :value {:a [1 nil "s"]})
   :attr      (n :attr :object x :attr "get_name")
   :call      (n :call :fn x :args [one] :kwargs [["k" one]])
   :subscript (n :subscript :object x :index one)
   :slice     (n :slice :object x :lower one :upper nil)
   :binop     (n :binop :op "+" :operands [x one x])
   :unop      (n :unop :op "not" :operand x)
   :cond-expr (n :cond-expr :test x :then one :else x)
   :lambda    (n :lambda :params {:fixed ["a"] :rest "r"} :body x)
   :comp      (n :comp :element x :clauses [{:for (n :tuple :items [x x]) :in x} {:if x}])
   :list      (n :list :items [x one])
   :tuple     (n :tuple :items [x])
   :set       (n :set :items [])
   :dict      (n :dict :entries [[one x]])
   :raw       (n :raw :code "1 + 1")})

(def ^:private stmts
  {:assign      (n :assign :target x :value one)
   :expr-stmt   (n :expr-stmt :expr x)
   :if          (n :if :test x :body [] :orelse nil)
   :for         (n :for :target x :iter x :body [(n :expr-stmt :expr x)])
   :while       (n :while :test x :body [])
   :def         (n :def :name "half" :params {:fixed [] :rest nil} :body [(n :return :value x)])
   :return      (n :return :value nil)
   :import      (n :import :module "numpy" :as "np")
   :from-import (n :from-import :module "gi.repository" :names ["Gio" "Gegl"])
   :try         (n :try :body [] :handlers [{:type x :name "e" :body []}] :finally nil)
   :raw-stmt    (n :raw-stmt :code "x = 1\ny = 2")})

(deftest every-contract-node-has-a-schema
  (is (= (set (keys exprs)) (set (keys ast/expr-nodes))))
  (is (= (set (keys stmts)) (set (keys ast/stmt-nodes)))))

(deftest every-expression-node-validates
  (doseq [[k node] exprs]
    (testing k
      (is (ast/valid? ast/Expr node))
      (is (ast/valid? node) "the one-arity infers Expr")
      (is (nil? (ast/explain node))))))

(deftest every-statement-node-validates
  (doseq [[k node] stmts]
    (testing k
      (is (ast/valid? ast/Stmt node))
      (is (ast/valid? node) "the one-arity infers Stmt")))
  (is (ast/valid? ast/Module (vec (vals stmts))))
  (is (ast/valid? (vec (vals stmts))) "a vector is a Module"))

(deftest nilable-keys-may-be-absent
  (is (ast/valid? (n :return)))
  (is (ast/valid? (n :import :module "json")))
  (is (ast/valid? (n :slice :object x)))
  (is (ast/valid? (n :lambda :params {:fixed []} :body x))))

(deftest malformed-nodes-are-refused
  (testing "a missing key"
    (is (= {:kwargs ["missing required key"]}
           (ast/explain (n :call :fn x :args [])))))
  (testing "an extra key: node maps are closed"
    (is (not (ast/valid? (assoc x :ns "Gimp")))))
  (testing "a mistyped child"
    (is (not (ast/valid? (n :attr :object "x" :attr "y"))))
    (is (not (ast/valid? (n :if :test x :body [x]))) "a block holds statements, not expressions"))
  (testing "an unknown node"
    (is (not (ast/valid? (n :walrus :target x)))))
  (testing "a binop needs two operands"
    (is (not (ast/valid? (n :binop :op "+" :operands [x])))))
  (testing "a statement is not an expression"
    (is (not (ast/valid? ast/Expr (:return stmts))))))

(deftest targets-are-names-or-tuples-of-targets
  (is (ast/valid? ast/Target x))
  (is (ast/valid? ast/Target (n :tuple :items [x (n :tuple :items [x x])])))
  (is (not (ast/valid? ast/Target one)))
  (is (not (ast/valid? (n :for :target (n :tuple :items [one]) :iter x :body [])))))

(deftest params-shape
  (is (ast/valid? ast/Params {:fixed ["a" "b"] :rest "r"}))
  (is (not (ast/valid? ast/Params {:fixed [:a]}))))
