(ns hive-gimp.py.lower.tail
  "`tail-form` registrations: how the last form of a `defn` body returns."
  (:require [hive-gimp.py.lower.core :as core
             :refer [tail-form lower-expr lower-stmts lower-tail]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- register!
  "Register `f` as the `tail-form` lowering of every head in `heads`."
  [heads f]
  (doseq [h heads] (.addMethod ^clojure.lang.MultiFn tail-form h f)))

(register! '[do]
           (fn [[_ & body]] (lower-tail body)))

(register! '[let]
           (fn [[_ bindings & body]] (into (core/binding-assigns bindings) (lower-tail body))))

(register! '[if]
           (fn [[_ c t e]]
             [{:py/node :if :test (lower-expr c) :body (lower-tail [t]) :orelse (lower-tail [e])}]))

(def guarded-tails
  "Heads `(h test & body)` whose body's value is returned when the test holds,
   and how each spells the test."
  {'when     identity
   'when-not (fn [t] {:py/node :unop :op "not" :operand t})})

(doseq [[h test] guarded-tails]
  (register! [h]
             (fn [[_ c & body]]
               [{:py/node :if :test (test (lower-expr c)) :body (lower-tail body) :orelse nil}])))

(register! '[try]
           (fn [[_ & clauses]] [(core/lower-try clauses lower-tail)]))

(def statement-tails
  "Heads that end a function body as a plain statement: they have no value to
   return."
  '[def set! doseq dotimes while defn return import raw])

(register! statement-tails (fn [form] (lower-stmts [form])))
