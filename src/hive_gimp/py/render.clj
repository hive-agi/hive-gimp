(ns hive-gimp.py.render
  "The Python AST (`hive-gimp.py.ast`) as Python source text.

   Pure and byte-exact: the rules are the ones `docs/py-strata.md` states and
   `test/hive_gimp/py_test.clj` asserts. Nothing here reads Clojure forms; it
   speaks only nodes.

   OPEN BY REGISTRATION

   `expr` and `stmt` are multimethods on `:py/node`, and a literal is spelt
   through the `PyLiteral` protocol extended per value type. A new node is a
   `defmethod`, a new literal type an `extend-protocol`; no dispatch is edited.
   A node with no method, or a value with no literal, is refused with
   `{:hive-gimp/reason :py/unsupported-form}`."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- refuse [message form]
  (throw (ex-info message {:hive-gimp/reason :py/unsupported-form :form form})))

(defn- comma [xs] (str/join ", " xs))

;; =============================================================================
;; Literals
;; =============================================================================

(defprotocol PyLiteral
  (literal [v] "A Clojure value as Python literal source."))

(defn string-literal
  "JSON string syntax is valid Python string syntax once `/` is left bare
   (Python keeps the backslash of an unknown escape like `\\/`)."
  [s]
  (json/write-str s :escape-slash false))

(defn- float-literal [d]
  (let [d (double d)]
    (cond (Double/isNaN d)      "float('nan')"
          (Double/isInfinite d) (if (pos? d) "float('inf')" "float('-inf')")
          :else                 (str d))))

(extend-protocol PyLiteral
  nil                          (literal [_] "None")
  Boolean                      (literal [b] (if b "True" "False"))
  String                       (literal [s] (string-literal s))
  clojure.lang.Keyword         (literal [k] (string-literal (subs (str k) 1)))
  Number                       (literal [n] (float-literal n))
  Long                         (literal [n] (str n))
  Integer                      (literal [n] (str n))
  Short                        (literal [n] (str n))
  Byte                         (literal [n] (str n))
  clojure.lang.BigInt          (literal [n] (str n))
  java.math.BigInteger         (literal [n] (str n))
  clojure.lang.Ratio           (literal [r] (str (double r)))
  java.util.Map                (literal [m] (str "{" (comma (map (fn [[k v]] (str (literal k) ": " (literal v))) m)) "}"))
  java.util.Set                (literal [s] (if (empty? s) "set()" (str "{" (comma (map literal s)) "}")))
  clojure.lang.Sequential      (literal [xs] (str "[" (comma (map literal xs)) "]"))
  Object                       (literal [v] (refuse (str "No Python literal for a " (type v) ".") v)))

;; =============================================================================
;; Expressions
;; =============================================================================

(defmulti expr
  "One expression node as Python source."
  :py/node)

(defmethod expr :default [n]
  (refuse (str "No renderer for expression node " (pr-str (:py/node n)) ".") n))

(defn params
  "`{:fixed [\"a\" \"b\"] :rest \"r\"}` as `a, b, *r`."
  [{:keys [fixed rest]}]
  (comma (cond-> (vec fixed) rest (conj (str "*" rest)))))

(defn- items [xs] (comma (map expr xs)))

(defn- tuple-source [xs]
  (str "(" (items xs) (when (= 1 (count xs)) ",") ")"))

(defmethod expr :name [{:keys [id]}] id)

(defmethod expr :literal [{:keys [value]}] (literal value))

(defmethod expr :raw [{:keys [code]}] code)

(defmethod expr :attr [{:keys [object attr]}] (str (expr object) "." attr))

(defmethod expr :call [{f :fn :keys [args kwargs]}]
  (str (expr f) "("
       (comma (concat (map expr args)
                      (map (fn [[k v]] (str k "=" (expr v))) kwargs)))
       ")"))

(defmethod expr :subscript [{:keys [object index]}]
  (str (expr object) "[" (expr index) "]"))

(defmethod expr :slice [{:keys [object lower upper]}]
  (str (expr object) "[" (some-> lower expr) ":" (some-> upper expr) "]"))

(defmethod expr :binop [{:keys [op operands]}]
  (str "(" (str/join (str " " op " ") (map expr operands)) ")"))

(defmethod expr :unop [{:keys [op operand]}]
  (str "(" op (when (re-find #"\w$" op) " ") (expr operand) ")"))

(defmethod expr :cond-expr [{:keys [test then else]}]
  (str "(" (expr then) " if " (expr test) " else " (expr else) ")"))

(defmethod expr :lambda [{:keys [body] :as n}]
  (str "(lambda " (params (:params n)) ": " (expr body) ")"))

(defn- clause [c]
  (cond (contains? c :for) (str "for " (expr (:for c)) " in " (expr (:in c)))
        (contains? c :if)  (str "if " (expr (:if c)))
        :else              (refuse "A comprehension clause is {:for T :in E} or {:if E}." c)))

(defmethod expr :comp [{:keys [element clauses]}]
  (str "[" (str/join " " (cons (expr element) (map clause clauses))) "]"))

(defmethod expr :list [n] (str "[" (items (:items n)) "]"))

(defmethod expr :tuple [n] (tuple-source (:items n)))

(defmethod expr :set [n]
  (if (empty? (:items n)) "set()" (str "{" (items (:items n)) "}")))

(defmethod expr :dict [{:keys [entries]}]
  (str "{" (comma (map (fn [[k v]] (str (expr k) ": " (expr v))) entries)) "}"))

;; =============================================================================
;; Statements: each renders to a vector of lines
;; =============================================================================

(defmulti stmt
  "One statement node as a vector of Python source lines."
  :py/node)

(defmethod stmt :default [n]
  (refuse (str "No renderer for statement node " (pr-str (:py/node n)) ".") n))

(defn stmts
  "Statement nodes as Python source lines."
  [nodes]
  (vec (mapcat stmt nodes)))

(defn- indent
  "A block's lines indented four spaces; an empty block is `pass`."
  [nodes]
  (mapv #(str "    " %) (or (seq (stmts nodes)) ["pass"])))

(defn- header [line body] (into [line] (indent body)))

(defmethod stmt :assign [{:keys [target value]}]
  [(str (expr target) " = " (expr value))])

(defmethod stmt :expr-stmt [n] [(expr (:expr n))])

(defmethod stmt :if [{:keys [test body orelse]}]
  (cond-> (header (str "if " (expr test) ":") body)
    (some? orelse) (into (header "else:" orelse))))

(defmethod stmt :for [{:keys [target iter body]}]
  (header (str "for " (expr target) " in " (expr iter) ":") body))

(defmethod stmt :while [{:keys [test body]}]
  (header (str "while " (expr test) ":") body))

(defmethod stmt :def [{:keys [name body] :as n}]
  (header (str "def " name "(" (params (:params n)) "):") body))

(defmethod stmt :return [{:keys [value]}]
  [(if (some? value) (str "return " (expr value)) "return")])

(defmethod stmt :import [{:keys [module as]}]
  [(str "import " module (when as (str " as " as)))])

(defmethod stmt :from-import [{:keys [module names]}]
  [(str "from " module " import " (comma names))])

(defn- handler [{:keys [type name body]}]
  (header (str "except" (when type (str " " (expr type))) (when name (str " as " name)) ":") body))

(defmethod stmt :try [{:keys [body handlers finally]}]
  (cond-> (into (header "try:" body) (mapcat handler handlers))
    (some? finally) (into (header "finally:" finally))))

(defmethod stmt :raw-stmt [{:keys [code]}] (str/split-lines code))

;; =============================================================================
;; Module
;; =============================================================================

(defn module
  "A vector of statement nodes as one Python source block."
  [nodes]
  (str/join "\n" (stmts nodes)))
