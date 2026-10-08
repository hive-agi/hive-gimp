(ns hive-gimp.py
  "Drive GIMP's Python with Clojure forms instead of Python strings.

   The facade over the strata of docs/py-strata.md: `template` collects the
   forms, `lower` promotes them to the Python AST, `render` and `result` are
   the pipeline, and `exec-forms` / `eval-forms` are the only boundary, one
   round trip through `hive-gimp.exec` over the transport port.

       (py/eval g
         (def img (first (Gimp/get-images)))
         [(.get-width img) (.get-height img) ~label])
       ;; => [2168 2096 \"logo\"]

   `eval` makes GIMP print its last expression as JSON and parses it back, so
   the answer is Clojure data, not a repr. GIMP objects come back as maps with
   their id, type and name.

   THE FORMS

   Names    `Gimp/get-images` is `Gimp.get_images`; `-` becomes `_`.
   Calls    `(f a :k v)` is `f(a, k=v)`; a keyword in an argument list always
            opens a keyword argument. `(Cls. a)` constructs.
   Methods  `(.m obj a)`, `(py. obj m a)`, `(. obj m a)`; attributes
            `(.-attr obj)`, `(py.- obj attr)`; chains `(py.. obj -attr (m a) n)`.
   Values   strings, numbers, nil/true/false, keywords (as strings), vectors
            (lists), maps (dicts), sets, `(tuple a b)`.
   Exprs    arithmetic and comparison operators, `and or not`, `get nth first
            second last slice count contains? inc dec str`, `(if c a b)`,
            `(fn [x] expr)` (a lambda), `(for [x xs :when c] expr)`.
   Stmts    `def set! do let if when when-not doseq dotimes while defn return
            import try raw`.
   Imports  `(import json)`, `(import [numpy :as np])`,
            `(import gi.repository [Gio Gegl])`.
   Escape   `(raw \"any python\")` emits its string unchanged.

   A new form is a `defmethod` in `hive-gimp.py.lower`, never an edit here.
   State persists between calls (the plugin execs into one context): a `def`
   here is visible to the next call."
  (:refer-clojure :exclude [eval])
  (:require [hive-gimp.exec :as exec]
            [hive-gimp.py.lower :as lower]
            [hive-gimp.py.render :as render]
            [hive-gimp.py.result :as result]
            [hive-gimp.py.template :as template]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Pure: forms to Python source
;; =============================================================================

(defn ->python
  "Forms as one Python source block."
  [forms]
  (render/module (lower/lower-stmts forms)))

(defmacro forms
  "`body` as data the compiler reads. `~x` is the VALUE of Clojure `x`, sent as
   a Python literal; `~@xs` splices each value of `xs`."
  [& body]
  `(template/forms ~@body))

(defn eval-source
  "The Python an `eval` of `forms` sends: every form but the last as a
   statement, then the last one's value printed as JSON after a sentinel."
  [forms]
  (let [lst (last forms)]
    (render/module (result/wrap (lower/lower-stmts (butlast forms))
                                (when (some? lst) (lower/lower-expr lst))))))

;; =============================================================================
;; Boundary: the one round trip
;; =============================================================================

(defn- transport-of [target] (or (:transport target) target))

(defn- run [target source]
  (let [out (exec/run (transport-of target) [source])]
    (cond-> out (not= :ok (:outcome out)) (assoc :python source))))

(defn exec-forms
  "Run `forms` in GIMP. An `Outcome` whose `:value` is the captured stdout."
  [target forms]
  (let [out (run target (->python forms))]
    (cond-> out (= :ok (:outcome out)) (update :value #(apply str %)))))

(defn eval-forms
  "Run `forms` in GIMP. An `Outcome` whose `:value` is the last form's value as
   Clojure data, and whose `:stdout` is anything the forms printed."
  [target forms]
  (let [out (run target (eval-source forms))]
    (if (= :ok (:outcome out))
      (let [{:keys [data stdout]} (result/parse (apply str (:value out)))]
        (assoc out :value data :stdout stdout))
      out)))

(defn- value! [outcome]
  (if (= :ok (:outcome outcome))
    (:value outcome)
    (throw (ex-info (str "GIMP refused the forms: " (:message outcome)) outcome))))

;; =============================================================================
;; Macros: forms written inline
;; =============================================================================

(defmacro exec
  "Run Clojure forms as Python in GIMP. Returns the `Outcome`."
  [target & body]
  `(exec-forms ~target (forms ~@body)))

(defmacro eval
  "Run Clojure forms as Python in GIMP. Returns the `Outcome`, `:value` being
   the last form's value as data."
  [target & body]
  `(eval-forms ~target (forms ~@body)))

(defmacro exec!
  "As `exec`, returning the stdout and throwing on failure."
  [target & body]
  `(#'value! (exec ~target ~@body)))

(defmacro eval!
  "As `eval`, returning the value and throwing on failure."
  [target & body]
  `(#'value! (eval ~target ~@body)))
