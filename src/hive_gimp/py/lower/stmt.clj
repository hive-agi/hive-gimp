(ns hive-gimp.py.lower.stmt
  "`stmt-form` registrations: every statement head the contract lists."
  (:require [hive-gimp.py.lower.core :as core
             :refer [stmt-form lower-expr lower-stmts lower-target name-node]]
            [hive-gimp.py.names :as names]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- register!
  "Register `f` as the `stmt-form` lowering of every head in `heads`."
  [heads f]
  (doseq [h heads] (.addMethod ^clojure.lang.MultiFn stmt-form h f)))

;; =============================================================================
;; Bindings and blocks
;; =============================================================================

(register! '[def]
           (fn [[_ t x]] [(core/assign-node (lower-target t) (lower-expr x))]))

(register! '[set!]
           (fn [[_ place x]] [(core/assign-node (lower-expr place) (lower-expr x))]))

(register! '[do]
           (fn [[_ & body]] (lower-stmts body)))

(register! '[let]
           (fn [[_ bindings & body]] (into (core/binding-assigns bindings) (lower-stmts body))))

(register! '[if]
           (fn [[_ c t e :as form]]
             [{:py/node :if :test (lower-expr c) :body (lower-stmts [t])
               :orelse  (when (> (count form) 3) (lower-stmts [e]))}]))

(def guarded-blocks
  "Heads `(h test & body)` lowering to one block, with how each spells the test
   and which node it builds."
  {'when     {:node :if :test identity}
   'when-not {:node :if :test (fn [t] {:py/node :unop :op "not" :operand t})}
   'while    {:node :while :test identity}})

(doseq [[h {:keys [node test]}] guarded-blocks]
  (register! [h]
             (fn [[_ c & body]]
               [(cond-> {:py/node node :test (test (lower-expr c)) :body (lower-stmts body)}
                  (= :if node) (assoc :orelse nil))])))

;; =============================================================================
;; Loops
;; =============================================================================

(defn- loops
  "Nested `for` nodes over `bindings`, innermost holding `body`; `iter` wraps
   each lowered collection."
  [bindings body iter]
  (if-let [[b x & more] (seq bindings)]
    [{:py/node :for :target (lower-target b) :iter (iter (lower-expr x)) :body (loops more body iter)}]
    body))

(def loop-forms
  "Loop heads and how each spells its collection."
  {'doseq   identity
   'dotimes (fn [n] {:py/node :call :fn (name-node "range") :args [n] :kwargs []})})

(doseq [[h iter] loop-forms]
  (register! [h] (fn [[_ bindings & body]] (loops bindings (lower-stmts body) iter))))

;; =============================================================================
;; Functions, imports, errors, escape
;; =============================================================================

(register! '[defn]
           (fn [[_ n params & body]]
             [{:py/node :def :name (names/sym->py n) :params (core/lower-params params)
               :body    (core/lower-tail body)}]))

(register! '[return]
           (fn [[_ & more]] [{:py/node :return :value (when more (lower-expr (first more)))}]))

(defn- import-node
  "`(import m)`, `(import [m :as a])` or `(import m [names])`."
  [[a b :as args]]
  (cond
    (and (symbol? a) (vector? b)) {:py/node :from-import :module (names/sym->py a) :names (mapv names/sym->py b)}
    (symbol? a)                   {:py/node :import :module (names/sym->py a) :as nil}
    (vector? a)                   (let [[m _ alias] a]
                                    {:py/node :import :module (names/sym->py m) :as (names/sym->py alias)})
    :else (names/refuse "import takes a module, [module :as alias] or module [names]." (cons 'import args))))

(register! '[import]
           (fn [[_ & args]] [(import-node args)]))

(register! '[try]
           (fn [[_ & clauses]] [(core/lower-try clauses lower-stmts)]))

(register! '[raw]
           (fn [[_ code]] [{:py/node :raw-stmt :code code}]))

