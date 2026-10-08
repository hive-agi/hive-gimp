(ns hive-gimp.py.lower.core
  "The lowering kernel: Clojure forms to Python AST maps (see docs/py-strata.md).

   Three open registries, each a multimethod on the head symbol of a list form:
   `expr-form` (an expression node), `stmt-form` (a vector of statement nodes)
   and `tail-form` (a vector of statement nodes ending in a return). Adding a
   form is a `defmethod`; this namespace only holds the defaults, the sugar
   rules and the node builders the registrations share."
  (:require [clojure.string :as str]
            [hive-gimp.py.names :as names]
            [hive-gimp.py.value :as value]
            [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Node builders
;; =============================================================================

(defn name-node [id] {:py/node :name :id id})

(defn literal-node [v] {:py/node :literal :value v})

(defn attr-node [object attr] {:py/node :attr :object object :attr attr})

(defn head
  "The head symbol of a list form, or nil."
  [form]
  (when (and (seq? form) (symbol? (first form))) (first form)))

;; =============================================================================
;; Registries
;; =============================================================================

(defmulti expr-form
  "A non-empty list form as one Python expression node."
  head)

(defmulti stmt-form
  "A form as a vector of Python statement nodes."
  head)

(defmulti tail-form
  "The last form of a function body as a vector of statement nodes that
   returns its value."
  head)

(defn registered?
  "True when `sym` has its own `expr-form` registration."
  [sym]
  (contains? (methods expr-form) sym))

;; =============================================================================
;; Sugar, resolved before dispatch
;; =============================================================================

(def sugar-rules
  "Head spellings rewritten to a registered head. Ordered: the first rule whose
   `:match?` accepts the head's name rewrites `[head name args]`."
  [{:sugar   :attribute
    :match?  #(str/starts-with? % ".-")
    :rewrite (fn [_ n [obj & more]] (list* 'py.- obj (symbol (subs n 2)) more))}
   {:sugar   :method
    :match?  #(and (str/starts-with? % ".") (not= "." %))
    :rewrite (fn [_ n [obj & more]] (list* 'py. obj (symbol (subs n 1)) more))}
   {:sugar   :constructor
    :match?  #(and (str/ends-with? % ".") (not= "." %))
    :rewrite (fn [h n args] (list* 'new (symbol (namespace h) (subs n 0 (dec (count n)))) args))}])

(defn desugar
  "`form` with a sugared head rewritten; registered heads are never rewritten."
  [form]
  (let [h (head form)]
    (if (or (nil? h) (registered? h))
      form
      (let [n (name h)]
        (if-let [rule (first (filter #((:match? %) n) sugar-rules))]
          ((:rewrite rule) h n (rest form))
          form)))))

;; =============================================================================
;; Expressions
;; =============================================================================

(declare lower-args)

(defn lower-with
  "`(registry form)`, a malformed form's host exception turned into the
   unsupported-form refusal; refusals raised inside pass through unchanged."
  [registry form]
  (try
    (registry form)
    (catch clojure.lang.ExceptionInfo e (throw e))
    (catch Exception e
      (names/refuse (str "Malformed `" (or (head form) "form") "`: " (.getMessage e)
                         " See docs/py-strata.md for the form's shape.")
                    form))))

(defn lower-expr
  "One Clojure form as a Python expression node."
  [form]
  (cond
    (value/lit? form) (literal-node (:value form))
    (symbol? form)    (name-node (names/sym->py form))
    (seq? form)       (if (empty? form)
                        {:py/node :tuple :items []}
                        (lower-with expr-form (desugar form)))
    (vector? form)    {:py/node :list :items (mapv lower-expr form)}
    (map? form)       {:py/node :dict :entries (mapv (fn [[k v]] [(lower-expr k) (lower-expr v)]) form)}
    (set? form)       {:py/node :set :items (mapv lower-expr form)}
    :else             (literal-node form)))

(defn lower-args
  "Positional arguments, then each `:k v` pair as a keyword argument:
   `{:args [E] :kwargs [[\"k\" E]]}`."
  [args]
  (loop [args (seq args) out {:args [] :kwargs []}]
    (if-let [[a & more] args]
      (if (keyword? a)
        (if more
          (recur (next more) (update out :kwargs conj [(names/keyword->py a) (lower-expr (first more))]))
          (names/refuse (str "Keyword argument " a " has no value: write `" a " value`.") a))
        (recur more (update out :args conj (lower-expr a))))
      out)))

(defn call-node
  "A call of the node `f` with Clojure argument forms `args`."
  [f args]
  (merge {:py/node :call :fn f} (lower-args args)))

(defn lower-target
  "A binding target: a symbol is a name, a vector a destructuring tuple."
  [t]
  (cond
    (symbol? t) (name-node (names/sym->py t))
    (vector? t) {:py/node :tuple :items (mapv lower-target t)}
    :else       (names/refuse "A binding target is a symbol or a vector of them." t)))

(defn lower-params
  "`[a b & r]` as `{:fixed [\"a\" \"b\"] :rest \"r\"}`."
  [params]
  (let [[fixed [_ rest-param]] (split-with #(not= '& %) params)]
    {:fixed (mapv names/sym->py fixed)
     :rest  (when rest-param (names/sym->py rest-param))}))

(defmethod expr-form :default [[h & args]]
  (call-node (lower-expr h) args))

;; =============================================================================
;; Statements and tails
;; =============================================================================

(defn lower-stmts
  "Forms as a vector of Python statement nodes."
  [forms]
  (into [] (mapcat #(lower-with stmt-form %)) forms))

(defn lower-tail
  "A function body as statements, its last form's value returned."
  [body]
  (into (lower-stmts (butlast body)) (lower-with tail-form (last body))))

(defn assign-node [target value] {:py/node :assign :target target :value value})

(defn binding-assigns
  "A `let` binding vector as assignments."
  [bindings]
  (mapv (fn [[t x]] (assign-node (lower-target t) (lower-expr x))) (partition 2 bindings)))

(defn lower-try
  "A `try` body with `(catch T e ...)` and `(finally ...)` clauses; the body
   and the handlers lower through `lower-body`, the finally block as statements."
  [clauses lower-body]
  (let [catch?   #(= 'catch (head %))
        finally? #(= 'finally (head %))
        fin      (first (filter finally? clauses))]
    {:py/node  :try
     :body     (lower-body (remove #(or (catch? %) (finally? %)) clauses))
     :handlers (mapv (fn [[_ cls e & body]]
                       {:type (lower-expr cls) :name (names/sym->py e) :body (lower-body body)})
                     (filter catch? clauses))
     :finally  (when fin (lower-stmts (rest fin)))}))

(defmethod stmt-form :default [form]
  [{:py/node :expr-stmt :expr (lower-expr form)}])

(defmethod tail-form :default [form]
  (if (some? form)
    [{:py/node :return :value (lower-expr form)}]
    []))

;; =============================================================================
;; Schemas: the shape lowering guarantees. The full node grammar is
;; hive-gimp.py.ast's; this is the floor every lowered value stands on.
;; =============================================================================

(def Node
  "Any lowered Python AST node: a map tagged with `:py/node`."
  [:map [:py/node :keyword]])

(def Module
  "A vector of statement nodes."
  [:vector Node])

(def Params
  "A lowered parameter vector."
  [:map [:fixed [:vector names/PyName]] [:rest [:maybe names/PyName]]])

(hs/register-all! {::node Node ::module Module ::params Params})

(m/=> lower-expr [:=> [:cat :any] Node])
(m/=> lower-stmts [:=> [:cat [:maybe [:sequential :any]]] Module])
(m/=> lower-tail [:=> [:cat [:maybe [:sequential :any]]] Module])
(m/=> lower-target [:=> [:cat :any] Node])
(m/=> lower-params [:=> [:cat [:vector :symbol]] Params])
(m/=> lower-args [:=> [:cat [:maybe [:sequential :any]]]
                  [:map [:args [:vector Node]] [:kwargs [:vector [:tuple names/PyName Node]]]]])
