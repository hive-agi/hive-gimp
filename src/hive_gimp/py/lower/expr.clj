(ns hive-gimp.py.lower.expr
  "`expr-form` registrations: every expression head the contract lists."
  (:require [clojure.string :as str]
            [hive-gimp.py.lower.core :as core
             :refer [expr-form lower-expr call-node attr-node name-node literal-node]]
            [hive-gimp.py.names :as names]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- register!
  "Register `f` as the `expr-form` lowering of every head in `heads`."
  [heads f]
  (doseq [h heads] (.addMethod ^clojure.lang.MultiFn expr-form h f)))

;; =============================================================================
;; Infix operators, as data
;; =============================================================================

(def infix-operators
  "Each row registers one infix head. `:unary` is the operator a one-operand
   form lowers to, when the head has one."
  [{:head '+ :op "+"} {:head '- :op "-" :unary "-"} {:head '* :op "*"}
   {:head '/ :op "/"} {:head 'mod :op "%"} {:head 'quot :op "//"}
   {:head '** :op "**"} {:head '< :op "<"} {:head '> :op ">"}
   {:head '<= :op "<="} {:head '>= :op ">="} {:head '= :op "=="}
   {:head 'not= :op "!="} {:head 'is :op "is"} {:head 'and :op "and"}
   {:head 'or :op "or"} {:head 'bit-and :op "&"} {:head 'bit-or :op "|"}])

(defn register-infix!
  "Register one infix operator row as an `expr-form`."
  [{:keys [head op unary]}]
  (register! [head]
             (fn [[h & operands :as form]]
               (cond
                 (and unary (= 1 (count operands)))
                 {:py/node :unop :op unary :operand (lower-expr (first operands))}

                 (< (count operands) 2)
                 (names/refuse (str h " needs two or more operands.") form)

                 :else
                 {:py/node :binop :op op :operands (mapv lower-expr operands)}))))

(run! register-infix! infix-operators)

;; =============================================================================
;; Methods, attributes, chains, constructors
;; =============================================================================

(register! '[. py.]
           (fn [[_ obj m & args]]
             (call-node (attr-node (lower-expr obj) (names/sym->py m)) args)))

(register! '[py.-]
           (fn [[_ obj a]] (attr-node (lower-expr obj) (names/sym->py a))))

(defn- chain-step
  "One `py..` step over the node `acc`: `-a` reads an attribute, `m` or
   `(m args)` calls a method."
  [acc step]
  (cond
    (and (symbol? step) (str/starts-with? (name step) "-"))
    (attr-node acc (names/sym->py (symbol (subs (name step) 1))))

    (symbol? step) (call-node (attr-node acc (names/sym->py step)) [])
    (seq? step)    (call-node (attr-node acc (names/sym->py (first step))) (rest step))
    :else          (names/refuse "A py.. step is -attr, method or (method args)." step)))

(register! '[.. py..]
           (fn [[_ obj & steps]] (reduce chain-step (lower-expr obj) steps)))

(register! '[new]
           (fn [[_ cls & args]] (call-node (name-node (names/sym->py cls)) args)))

;; =============================================================================
;; Control and functions
;; =============================================================================

(register! '[not]
           (fn [[_ x]] {:py/node :unop :op "not" :operand (lower-expr x)}))

(register! '[if]
           (fn [[_ c t e]]
             {:py/node :cond-expr :test (lower-expr c) :then (lower-expr t) :else (lower-expr e)}))

(register! '[fn]
           (fn [[_ & args :as form]]
             (if (= 2 (count args))
               {:py/node :lambda :params (core/lower-params (first args)) :body (lower-expr (second args))}
               (names/refuse "A fn compiles to a Python lambda: one parameter vector and one expression; a body belongs in defn." form))))

(defn- comp-clauses
  "`for` bindings as comprehension clauses: `x xs` iterates, `:when c` filters."
  [bindings]
  (mapv (fn [[b x]]
          (if (= :when b)
            {:if (lower-expr x)}
            {:for (core/lower-target b) :in (lower-expr x)}))
        (partition 2 bindings)))

(register! '[for]
           (fn [[_ bindings body]]
             {:py/node :comp :element (lower-expr body) :clauses (comp-clauses bindings)}))

;; =============================================================================
;; Collections and sequence helpers
;; =============================================================================

(register! '[tuple]
           (fn [[_ & items]] {:py/node :tuple :items (mapv lower-expr items)}))

(defn- subscript [object index] {:py/node :subscript :object object :index index})

(register! '[get nth]
           (fn [[_ x k & [default :as more]]]
             (if (seq more)
               {:py/node :call :fn (attr-node (lower-expr x) "get")
                :args    [(lower-expr k) (lower-expr default)] :kwargs []}
               (subscript (lower-expr x) (lower-expr k)))))

(register! '[get-in]
           (fn [[_ x ks]] (reduce #(subscript %1 (lower-expr %2)) (lower-expr x) ks)))

(def positional-readers
  "Heads that read one fixed position of a sequence."
  {'first 0 'second 1 'last -1})

(doseq [[h i] positional-readers]
  (register! [h] (fn [[_ x]] (subscript (lower-expr x) (literal-node i)))))

(register! '[slice]
           (fn [[_ x a b]]
             {:py/node :slice :object (lower-expr x)
              :lower (when (some? a) (lower-expr a))
              :upper (when (some? b) (lower-expr b))}))

(register! '[count]
           (fn [[_ x]] {:py/node :call :fn (name-node "len") :args [(lower-expr x)] :kwargs []}))

(register! '[contains?]
           (fn [[_ coll k]] {:py/node :binop :op "in" :operands [(lower-expr k) (lower-expr coll)]}))

(def steppers
  "Heads that add or subtract one."
  {'inc "+" 'dec "-"})

(doseq [[h op] steppers]
  (register! [h] (fn [[_ x]] {:py/node :binop :op op :operands [(lower-expr x) (literal-node 1)]})))

(register! '[str]
           (fn [[_ & parts]]
             {:py/node :call
              :fn      (attr-node (literal-node "") "join")
              :args    [{:py/node :call :fn (name-node "map")
                         :args [(name-node "str") {:py/node :list :items (mapv lower-expr parts)}]
                         :kwargs []}]
              :kwargs  []}))

(register! '[raw]
           (fn [[_ code]] {:py/node :raw :code code}))
