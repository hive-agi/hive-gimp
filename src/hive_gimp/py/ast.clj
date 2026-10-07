(ns hive-gimp.py.ast
  "The Python AST as data: the DOMAIN value of `hive-gimp.py`.

   Every node is a map carrying `:py/node`. Lowering (`hive-gimp.py.lower`)
   produces these, rendering (`hive-gimp.py.render`) consumes them, and nothing
   in between speaks Python text. The node shapes are the contract in
   `docs/py-strata.md`; this namespace is where that contract is checkable.

   OPEN BY REGISTRATION

   The node vocabulary is two maps, `expr-nodes` and `stmt-nodes`, keyed by the
   `:py/node` value. A new node is a new entry, never an edit to a dispatch:
   `Expr` and `Stmt` are `:multi` schemas built from those maps. Every node map
   is CLOSED, so a misspelt key is a validation error rather than a silently
   ignored field. A key documented as `-or-nil` may be absent or nil.

   Pure: no effects, no Python, no rendering."
  (:require [malli.core :as m]
            [malli.error :as me]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Shared shapes
;; =============================================================================

(def ^:private expr [:ref ::expr])
(def ^:private stmt [:ref ::stmt])
(def ^:private block [:vector stmt])

(defn- node
  "A closed node map: `:py/node` plus `entries`."
  [& entries]
  (into [:map {:closed true} [:py/node :keyword]] entries))

(defn- nilable
  "An `-or-nil` key: absent, nil, or a `schema`."
  [k schema]
  [k {:optional true} [:maybe schema]])

(def ^:private params
  [:map {:closed true}
   [:fixed [:vector :string]]
   [:rest {:optional true} [:maybe :string]]])

;; =============================================================================
;; Node registries: one entry per :py/node
;; =============================================================================

(def expr-nodes
  "Expression node schemas by `:py/node`. Register a node by adding an entry."
  {:name      (node [:id :string])
   :literal   (node [:value :any])
   :attr      (node [:object expr] [:attr :string])
   :call      (node [:fn expr]
                    [:args [:vector expr]]
                    [:kwargs [:vector [:tuple :string expr]]])
   :subscript (node [:object expr] [:index expr])
   :slice     (node [:object expr] (nilable :lower expr) (nilable :upper expr))
   :binop     (node [:op :string] [:operands [:vector {:min 2} expr]])
   :unop      (node [:op :string] [:operand expr])
   :cond-expr (node [:test expr] [:then expr] [:else expr])
   :lambda    (node [:params [:ref ::params]] [:body expr])
   :comp      (node [:element expr]
                    [:clauses [:vector [:or
                                        [:map {:closed true} [:for [:ref ::target]] [:in expr]]
                                        [:map {:closed true} [:if expr]]]]])
   :list      (node [:items [:vector expr]])
   :tuple     (node [:items [:vector expr]])
   :set       (node [:items [:vector expr]])
   :dict      (node [:entries [:vector [:tuple expr expr]]])
   :raw       (node [:code :string])})

(def stmt-nodes
  "Statement node schemas by `:py/node`. Register a node by adding an entry."
  {:assign      (node [:target expr] [:value expr])
   :expr-stmt   (node [:expr expr])
   :if          (node [:test expr] [:body block] (nilable :orelse block))
   :for         (node [:target [:ref ::target]] [:iter expr] [:body block])
   :while       (node [:test expr] [:body block])
   :def         (node [:name :string] [:params [:ref ::params]] [:body block])
   :return      (node (nilable :value expr))
   :import      (node [:module :string] (nilable :as :string))
   :from-import (node [:module :string] [:names [:vector {:min 1} :string]])
   :try         (node [:body block]
                      [:handlers [:vector [:map {:closed true}
                                           (nilable :type expr)
                                           (nilable :name :string)
                                           [:body block]]]]
                      (nilable :finally block))
   :raw-stmt    (node [:code :string])})

(defn- multi [nodes]
  (into [:multi {:dispatch :py/node}] (sort-by key nodes)))

(def ^:private target-nodes
  ;; T: a :name, or a :tuple of targets (the destructuring form).
  {:name  (:name expr-nodes)
   :tuple (node [:items [:vector [:ref ::target]]])})

(def registry
  "The local malli registry behind every public schema."
  {::expr   (multi expr-nodes)
   ::stmt   (multi stmt-nodes)
   ::target (multi target-nodes)
   ::params params
   ::module block})

(defn- schema-of [k] (m/schema [:schema {:registry registry} k]))

(def Expr   "Any expression node."              (schema-of ::expr))
(def Stmt   "Any statement node."               (schema-of ::stmt))
(def Target "An assignment or loop target."     (schema-of ::target))
(def Params "`{:fixed [\"a\"] :rest \"r\"}`."   (schema-of ::params))
(def Module "A vector of statements: one block." (schema-of ::module))

;; =============================================================================
;; Validation
;; =============================================================================

(defn- infer
  "The schema a bare value is checked against: a vector is a Module, a
   statement node a Stmt, anything else an Expr."
  [x]
  (cond (vector? x)                           Module
        (contains? stmt-nodes (:py/node x))   Stmt
        :else                                 Expr))

(defn valid?
  "True when `x` conforms to `schema` (Expr, Stmt, Target, Params, Module).
   With one argument the schema is inferred: vector Module, statement node
   Stmt, otherwise Expr."
  ([x] (valid? (infer x) x))
  ([schema x] (m/validate schema x)))

(defn explain
  "nil when `x` conforms, else the humanized malli explanation."
  ([x] (explain (infer x) x))
  ([schema x] (some-> (m/explain schema x) me/humanize)))
