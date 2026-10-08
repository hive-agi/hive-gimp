(ns hive-gimp.py.ast
  "The Python AST as data: the DOMAIN value of `hive-gimp.py`.

   Every node is a map carrying `:py/node`. Lowering (`hive-gimp.py.lower`)
   produces these, rendering (`hive-gimp.py.render`) consumes them, and nothing
   in between speaks Python text. The node shapes are the contract in
   `docs/py-strata.md`; this namespace is where that contract is checkable.

   REGISTERED, NOT PRIVATE

   Every schema is registered in the hive schema registry (hive-schemas, over
   hive-spi) at load time, under `:hive-gimp.py/*` for the aggregates and
   `:hive-gimp.py.expr/<node>` / `:hive-gimp.py.stmt/<node>` per node. Any
   namespace, test or tool can then validate or GENERATE an AST by key, and
   the generators are what the trifecta properties run on.

   OPEN BY REGISTRATION

   The node vocabulary is two maps, `expr-nodes` and `stmt-nodes`, keyed by the
   `:py/node` value. A new node is a new entry, never an edit to a dispatch:
   `Expr` and `Stmt` are `:multi` schemas built from those maps. Every node map
   is CLOSED and pins its own `:py/node`, so a misspelt key is a validation
   error rather than a silently ignored field. A key documented as `-or-nil`
   may be absent or nil.

   Pure apart from the idempotent registration: no Python, no rendering."
  (:require [hive-schemas.schema :as sch]
            [malli.core :as m]
            [malli.error :as me]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Leaf vocabulary
;; =============================================================================

(def ^:private expr  [:ref :hive-gimp.py/expr])
(def ^:private block [:vector {:gen/max 3} [:ref :hive-gimp.py/stmt]])

(defn- many [schema] [:vector {:gen/max 3} schema])

(def binary-operators
  "The Python spellings a `:binop` may carry. Register an operator here."
  #{"+" "-" "*" "/" "%" "//" "**" "<" ">" "<=" ">=" "==" "!=" "is" "is not"
    "in" "not in" "and" "or" "&" "|" "^" "<<" ">>"})

(def unary-operators
  "The Python spellings a `:unop` may carry. Register an operator here."
  #{"-" "+" "~" "not"})

(def ^:private leaves
  {:hive-gimp.py/identifier
   [:re {:gen/elements ["a" "b" "x" "img" "get_name" "_hg"]}
    #"^[A-Za-z_][A-Za-z0-9_]*$"]

   :hive-gimp.py/dotted-name
   [:re {:gen/elements ["x" "img" "Gimp.get_images" "gi.repository" "a.b_c"]}
    #"^[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*$"]

   :hive-gimp.py/source
   [:string {:gen/elements ["1 + 1" "x" "f(a)[0]"]}]

   :hive-gimp.py/source-lines
   [:string {:gen/elements ["x = 1" "x = 1\ny = 2" "print(x)"]}]

   :hive-gimp.py/literal-value
   [:or
    :nil :boolean :string :keyword :int
    [:double {:gen/NaN? true :gen/infinite? true}]
    [:fn {:gen/elements [1/2 -3/4 (bigint 9) 2.5M]} number?]
    [:sequential {:gen/max 3} [:ref :hive-gimp.py/literal-value]]
    [:set {:gen/max 3} [:ref :hive-gimp.py/literal-value]]
    [:map-of {:gen/max 3} [:or :string :keyword :int] [:ref :hive-gimp.py/literal-value]]]

   :hive-gimp.py/params
   [:map {:closed true}
    [:fixed (many :hive-gimp.py/identifier)]
    [:rest {:optional true} [:maybe :hive-gimp.py/identifier]]]})

;; =============================================================================
;; Node registries: one entry per :py/node
;; =============================================================================

(defn- node
  "A closed node map pinned to `kind`, plus `entries`."
  [kind & entries]
  (into [:map {:closed true} [:py/node [:= kind]]] entries))

(defn- nilable
  "An `-or-nil` key: absent, nil, or a `schema`."
  [k schema]
  [k {:optional true} [:maybe schema]])

(def expr-nodes
  "Expression node schemas by `:py/node`. Register a node by adding an entry."
  {:name      (node :name [:id :hive-gimp.py/dotted-name])
   :literal   (node :literal [:value :hive-gimp.py/literal-value])
   :attr      (node :attr [:object expr] [:attr :hive-gimp.py/identifier])
   :call      (node :call [:fn expr] [:args (many expr)]
                    [:kwargs (many [:tuple :hive-gimp.py/identifier expr])])
   :subscript (node :subscript [:object expr] [:index expr])
   :slice     (node :slice [:object expr] (nilable :lower expr) (nilable :upper expr))
   :binop     (node :binop [:op (into [:enum] (sort binary-operators))]
                    [:operands [:vector {:min 2 :gen/max 3} expr]])
   :unop      (node :unop [:op (into [:enum] (sort unary-operators))] [:operand expr])
   :cond-expr (node :cond-expr [:test expr] [:then expr] [:else expr])
   :lambda    (node :lambda [:params :hive-gimp.py/params] [:body expr])
   :comp      (node :comp [:element expr]
                    [:clauses (many [:or
                                     [:map {:closed true} [:for [:ref :hive-gimp.py/target]] [:in expr]]
                                     [:map {:closed true} [:if expr]]])])
   :list      (node :list [:items (many expr)])
   :tuple     (node :tuple [:items (many expr)])
   :set       (node :set [:items (many expr)])
   :dict      (node :dict [:entries (many [:tuple expr expr])])
   :raw       (node :raw [:code :hive-gimp.py/source])})

(def stmt-nodes
  "Statement node schemas by `:py/node`. Register a node by adding an entry."
  {:assign      (node :assign [:target expr] [:value expr])
   :expr-stmt   (node :expr-stmt [:expr expr])
   :if          (node :if [:test expr] [:body block] (nilable :orelse block))
   :for         (node :for [:target [:ref :hive-gimp.py/target]] [:iter expr] [:body block])
   :while       (node :while [:test expr] [:body block])
   :def         (node :def [:name :hive-gimp.py/identifier]
                      [:params :hive-gimp.py/params] [:body block])
   :return      (node :return (nilable :value expr))
   :import      (node :import [:module :hive-gimp.py/dotted-name]
                      (nilable :as :hive-gimp.py/identifier))
   :from-import (node :from-import [:module :hive-gimp.py/dotted-name]
                      [:names [:vector {:min 1 :gen/max 3} :hive-gimp.py/identifier]])
   :try         (node :try [:body block]
                      [:handlers (many [:map {:closed true}
                                        (nilable :type expr)
                                        (nilable :name :hive-gimp.py/identifier)
                                        [:body block]])]
                      (nilable :finally block))
   :raw-stmt    (node :raw-stmt [:code :hive-gimp.py/source-lines])})

(def ^:private target-nodes
  ;; T: a :name, or a :tuple of targets (the destructuring form).
  {:name  (:name expr-nodes)
   :tuple (node :tuple [:items (many [:ref :hive-gimp.py/target])])})

;; =============================================================================
;; Registration
;; =============================================================================

(defn- keyed
  "`{node schema}` as `{:<ns>/<node> schema}`."
  [ns nodes]
  (into {} (map (fn [[k s]] [(keyword ns (name k)) s])) nodes))

(defn- multi
  "A `:multi` on `:py/node` whose branches are the registered node keys."
  [ns nodes]
  (into [:multi {:dispatch :py/node}]
        (map (fn [k] [k (keyword ns (name k))]))
        (sort (keys nodes))))

(def schemas
  "Every schema this namespace contributes, by registry key."
  (merge leaves
         (keyed "hive-gimp.py.expr" expr-nodes)
         (keyed "hive-gimp.py.stmt" stmt-nodes)
         (keyed "hive-gimp.py.target" target-nodes)
         {:hive-gimp.py/expr   (multi "hive-gimp.py.expr" expr-nodes)
          :hive-gimp.py/stmt   (multi "hive-gimp.py.stmt" stmt-nodes)
          :hive-gimp.py/target (multi "hive-gimp.py.target" target-nodes)
          :hive-gimp.py/module block}))

(sch/register-all! schemas)

(def Expr         "Any expression node."                 (sch/schema :hive-gimp.py/expr))
(def Stmt         "Any statement node."                  (sch/schema :hive-gimp.py/stmt))
(def Target       "An assignment or loop target."        (sch/schema :hive-gimp.py/target))
(def Params       "`{:fixed [\"a\"] :rest \"r\"}`."      (sch/schema :hive-gimp.py/params))
(def Module       "A vector of statements: one block."   (sch/schema :hive-gimp.py/module))
(def LiteralValue "A Clojure value render can spell."    (sch/schema :hive-gimp.py/literal-value))

;; =============================================================================
;; Validation
;; =============================================================================

(defn- infer
  "The schema a bare value is checked against: a vector is a Module, a
   statement node a Stmt, anything else an Expr."
  [x]
  (cond (vector? x)                         Module
        (contains? stmt-nodes (:py/node x)) Stmt
        :else                               Expr))

(defn valid?
  "True when `x` conforms to `schema` (Expr, Stmt, Target, Params, Module, or
   a registry key). With one argument the schema is inferred: vector Module,
   statement node Stmt, otherwise Expr."
  ([x] (valid? (infer x) x))
  ([schema x] (m/validate (sch/schema schema) x)))

(defn explain
  "nil when `x` conforms, else the humanized malli explanation."
  ([x] (explain (infer x) x))
  ([schema x] (some-> (m/explain (sch/schema schema) x) me/humanize)))

(m/=> valid?  [:function [:=> [:cat :any] :boolean] [:=> [:cat :any :any] :boolean]])
(m/=> explain [:function [:=> [:cat :any] :any] [:=> [:cat :any :any] :any]])
