(ns hive-gimp.py.lower
  "Clojure forms to the Python AST (docs/py-strata.md). Loads every form
   registration; extend by adding a method to `expr-form`, `stmt-form` or
   `tail-form`."
  (:require [hive-gimp.py.lower.core :as core]
            [hive-gimp.py.lower.expr]
            [hive-gimp.py.lower.stmt]
            [hive-gimp.py.lower.tail]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def expr-form
  "A non-empty list form as one expression node, by head symbol."
  core/expr-form)

(def stmt-form
  "A form as a vector of statement nodes, by head symbol."
  core/stmt-form)

(def tail-form
  "A body's last form as statement nodes that return its value, by head symbol."
  core/tail-form)

(defn lower-expr
  "One Clojure form as a Python expression node."
  [form]
  (core/lower-expr form))

(defn lower-stmts
  "Clojure forms as a vector of Python statement nodes (a module)."
  [forms]
  (core/lower-stmts forms))

(defn lower-tail
  "A function body as statement nodes, its last form's value returned."
  [body]
  (core/lower-tail body))
