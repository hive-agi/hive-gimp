(ns hive-gimp.py.template
  "The `forms` macro: Clojure forms quoted as data, with `~x` splicing the
   value of `x` and `~@xs` each value of `xs` as `value/Lit`."
  (:require [hive-gimp.py.value :as value]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- unquote? [f] (and (seq? f) (= 'clojure.core/unquote (first f))))

(defn- splice? [f] (and (seq? f) (= 'clojure.core/unquote-splicing (first f))))

(declare template)

(defn- items
  "Code building the elements of a collection form, splices flattened."
  [fs]
  `(concat ~@(map (fn [f] (if (splice? f) `(map value/lit ~(second f)) [(template f)])) fs)))

(defn template
  "Code that rebuilds `form` as data, every `~x` evaluated to a `Lit`."
  [form]
  (cond
    (unquote? form) `(value/lit ~(second form))
    (seq? form)     `(apply list ~(items form))
    (vector? form)  `(vec ~(items form))
    (map? form)     `(apply array-map ~(items (mapcat identity form)))
    (set? form)     `(set ~(items form))
    (symbol? form)  `'~form
    :else           form))

(defmacro forms
  "`body` as a vector of forms. `~x` is the VALUE of Clojure `x`, lowered as a
   Python literal; `~@xs` splices each value of `xs`."
  [& body]
  `(vector ~@(map template body)))
