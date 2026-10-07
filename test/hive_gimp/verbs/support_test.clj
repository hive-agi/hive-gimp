(ns hive-gimp.verbs.support-test
  "Shared vocabulary for the verb family trifectas, and the gates over the
   catalogue as a whole.

   Every family trifecta has the same subject, `hive-gimp.verbs/program-of`
   over a `VerbCall`, golden-checked on the rendered Python. Its property
   generates calls from each verb's own `:verb/args` schema; its mutants are
   builder bugs injected for ONE verb through `on`."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-gimp.py :as py]
            [hive-gimp.py.trifecta-support :as s]
            [hive-gimp.verbs :as verbs]
            [hive-gimp.verbs.catalog :as catalog]
            [hive-schemas.schema :as hs]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Generators
;; =============================================================================

(defn family-verbs
  "Catalogue keys of `family` (\"layer\", \"image\" ...)."
  [family]
  (filterv #(= family (namespace %)) (keys (catalog/catalogue))))

(defn gen-call
  "A `VerbCall` of one of `family`'s verbs, its arguments generated from the
   verb's `:verb/args`."
  [family]
  (gen/one-of
   (for [k (family-verbs family)]
     (gen/fmap (fn [args] {:verb k :args (vec args)})
               (hs/generator (:args (catalog/lookup k)))))))

;; =============================================================================
;; Oracles
;; =============================================================================

(def ^:private statement-heads
  '#{def set! do let when when-not while doseq dotimes defn return import try raw})

(defn program-holds?
  "A program is a non-empty vector of forms, deterministic, rendering to
   bracket-balanced, well-indented Python, and ending in an EXPRESSION, so
   `eval` can answer it."
  [call forms]
  (let [source (py/->python forms)
        lst    (last forms)]
    (and (vector? forms)
         (seq forms)
         (= forms (verbs/program-of call))
         (s/balanced? source)
         (s/indentation-ok? source)
         (not (and (seq? lst) (contains? statement-heads (first lst))))
         (string? (py/eval-source forms)))))

(def original
  "`program-of` as loaded, so a mutant can defer to it without recursing
   through the var it replaces."
  (deref #'verbs/program-of))

(defn on
  "A mutant of `program-of` that builds `verb` with `bad` (a fn of the call)
   and defers every other verb."
  [verb bad]
  (fn [call] (if (= verb (:verb call)) (bad call) (original call))))

(defn with-args
  "A mutant builder for `verb` that rewrites its arguments with `f` first."
  [verb f]
  (on verb #(original (update % :args f))))

(defn drop-forms
  "A mutant builder for `verb` whose program loses every form headed `head`."
  [verb head]
  (on verb #(vec (remove (fn [f] (and (seq? f) (= head (first f)))) (original %)))))

(defn call [verb & args] {:verb verb :args (vec args)})

(defn uncovered
  "Verbs of `family` with no case in `cases`."
  [family cases]
  (sort (remove (set (map :verb (vals cases))) (family-verbs family))))

;; =============================================================================
;; Catalogue gates (universe read off the FILES, not off the registry)
;; =============================================================================

(def ^:private verbs-dir "src/hive_gimp/verbs")

(def ^:private not-families #{"value" "catalog"})

(defn- family-files []
  (->> (.listFiles (io/file verbs-dir))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))
       (map #(str/replace (.getName ^java.io.File %) #"\.clj$" ""))
       (remove not-families)
       sort))

(deftest every-family-file-is-a-registered-family-with-verbs
  (let [files (family-files)]
    (is (= 7 (count files)) "the walk found the seven families; an empty walk would pass vacuously")
    (doseq [f files]
      (testing f
        (is (some #{(symbol (str "hive-gimp.verbs." f))} (catalog/registered-namespaces)))
        (is (seq (family-verbs f)))))))

(deftest every-verb-declares-its-contract
  (doseq [[k {:keys [doc args answers]}] (catalog/catalogue)]
    (testing k
      (is (not (str/blank? doc)))
      (is (= :cat (first args)) ":verb/args is a malli :cat over the arguments")
      (is (some? answers)))))

(deftest every-verbs-namespace-stays-under-400-lines
  (doseq [f (cons (io/file "src/hive_gimp/verbs.clj")
                  (filter #(str/ends-with? (.getName ^java.io.File %) ".clj")
                          (.listFiles (io/file verbs-dir))))]
    (testing (.getName ^java.io.File f)
      (is (< (count (str/split-lines (slurp f))) 400)))))

(deftest command-of-is-a-snake-case-command-name
  (doseq [k (keys (catalog/catalogue))]
    (is (re-matches #"^[a-z][a-z0-9_]*$" (catalog/command-of k)))))
