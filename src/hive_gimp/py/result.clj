(ns hive-gimp.py.result
  "The eval protocol: a module whose last value GIMP prints as JSON after a
   sentinel, and the reading of that stdout back into Clojure data.

   `wrap` speaks AST (lowered statements in, a module out); `parse` speaks
   text (stdout in, `EvalResult` out). Neither touches a transport."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-gimp.py.ast :as ast]
            [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def sentinel
  "The prefix of the one stdout line that carries the value."
  "\u001ehive-gimp.py ")

(def ^:private prelude
  ;; GIMP objects come back as {id, type, name}; anything else as its repr.
  {:py/node :raw-stmt
   :code    (str/join "\n"
                      ["import json as __hg_json"
                       "def __hg_default(o):"
                       "    if hasattr(o, 'get_id'):"
                       "        return {'id': o.get_id(), 'type': type(o).__name__, 'name': o.get_name() if hasattr(o, 'get_name') else None}"
                       "    return repr(o)"])})

(defn- nm [id] {:py/node :name :id id})

(def ^:private result-name (nm "__hg_r"))

(def ^:private print-result
  {:py/node :expr-stmt
   :expr    {:py/node :call
             :fn      (nm "print")
             :args    [{:py/node  :binop
                        :op       "+"
                        :operands [{:py/node :literal :value sentinel}
                                   {:py/node :call
                                    :fn      {:py/node :attr :object (nm "__hg_json") :attr "dumps"}
                                    :args    [result-name]
                                    :kwargs  [["default" (nm "__hg_default")]]}]}]
             :kwargs  []}})

(defn wrap
  "The module an eval sends: the prelude, the `lead` statements, `value`
   (an expression node, or nil for None) bound to the result, then printed."
  [lead value]
  (-> [prelude]
      (into lead)
      (conj {:py/node :assign :target result-name
             :value   (or value {:py/node :literal :value nil})}
            print-result)))

(def EvalResult
  "What an eval's stdout reads as: the value, and everything else printed."
  [:map [:data :any] [:stdout :string]])

(defn- sentinel? [line] (str/starts-with? line sentinel))

(defn parse
  "`stdout` as an `EvalResult`: the last sentinel line's JSON as `:data`
   (keyword keys), every other line as `:stdout`."
  [stdout]
  (let [lines  (str/split-lines (or stdout ""))
        result (last (filter sentinel? lines))]
    {:data   (when result (json/read-str (subs result (count sentinel)) :key-fn keyword))
     :stdout (str/join "\n" (remove sentinel? lines))}))

(hs/register-all! {::eval-result EvalResult})

(m/=> wrap [:=> [:cat [:sequential ast/Stmt] [:maybe ast/Expr]] ast/Module])
(m/=> parse [:=> [:cat [:maybe :string]] EvalResult])
