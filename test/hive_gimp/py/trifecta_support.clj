(ns hive-gimp.py.trifecta-support
  "Shared vocabulary for the py-strata trifectas: node builders, the
   structural oracles a rendered Python block must satisfy, generators drawn
   from the registered AST schemas, and one property type registered into
   hive-test.

   `:pred-io` (registered on `hive-test.trifecta/emit-property`) calls the
   predicate with BOTH the generated input and the subject's output. The
   built-in `:pred` sees only the output, which cannot state determinism or
   any input-to-output relation."
  (:require [clojure.string :as str]
            [clojure.test.check.clojure-test :as tc]
            [clojure.test.check.properties :as prop]
            [hive-gimp.py.ast]
            [hive-schemas.schema :as sch]
            [hive-test.trifecta :as trifecta]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Property type: predicate over input AND output
;; =============================================================================

(defmethod trifecta/emit-property :pred-io
  [_ {:keys [gen pred num-tests]} {:keys [name var-sym]}]
  `(tc/defspec ~(symbol (str name "-property")) ~(or num-tests 200)
     (prop/for-all [v# ~gen]
       (~pred v# (~var-sym v#)))))

;; =============================================================================
;; Generators, from the registered schemas
;; =============================================================================

(def gen-module  (sch/generator :hive-gimp.py/module))
(def gen-expr    (sch/generator :hive-gimp.py/expr))
(def gen-stmt    (sch/generator :hive-gimp.py/stmt))
(def gen-literal (sch/generator :hive-gimp.py/literal-value))

;; =============================================================================
;; Node builders
;; =============================================================================

(defn n   [k & kvs] (apply array-map :py/node k kvs))
(defn nm  [id] (n :name :id id))
(defn lit [v] (n :literal :value v))
(defn call
  ([f] (call f []))
  ([f args] (n :call :fn (if (string? f) (nm f) f) :args (vec args) :kwargs [])))
(defn stmt [e] (n :expr-stmt :expr e))

;; =============================================================================
;; Structural oracles over rendered Python
;; =============================================================================

(def ^:private closer {\( \) \[ \] \{ \}})

(defn- skip-string
  "Index just past the string literal opened by `q` at `i`."
  [s i q]
  (loop [j (inc i)]
    (cond (>= j (count s))        j
          (= \\ (.charAt ^String s j)) (recur (+ j 2))
          (= q (.charAt ^String s j))  (inc j)
          :else                   (recur (inc j)))))

(defn balanced?
  "Every bracket outside a string literal is closed, in order."
  [^String s]
  (loop [i 0 stack ()]
    (if (>= i (count s))
      (empty? stack)
      (let [c (.charAt s i)]
        (cond
          (#{\" \'} c)          (recur (skip-string s i c) stack)
          (closer c)            (recur (inc i) (cons (closer c) stack))
          (#{\) \] \}} c)       (and (= c (first stack)) (recur (inc i) (rest stack)))
          :else                 (recur (inc i) stack))))))

(defn- indent-of [line] (count (take-while #{\space} line)))

(defn indentation-ok?
  "Every line is indented by a multiple of four, and a line opening a block
   (ending in `:`) is followed by one indented exactly four deeper."
  [source]
  (let [lines (if (str/blank? source) [] (str/split-lines source))]
    (and (every? #(zero? (mod (indent-of %) 4)) lines)
         (every? (fn [[a b]] (or (not (str/ends-with? a ":"))
                                 (= (+ 4 (indent-of a)) (indent-of (or b "")))))
                 (partition 2 1 [nil] lines)))))

(defn top-level-lines
  "Lines a module renders at column zero."
  [source]
  (if (str/blank? source) 0 (count (remove #(str/starts-with? % " ") (str/split-lines source)))))
