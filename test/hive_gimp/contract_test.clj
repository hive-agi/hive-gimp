(ns hive-gimp.contract-test
  "The descriptor algebra.

   Every property here was unreachable while this logic lived inside
   `hive-gimp.catalog` behind a `delay` over `io/resource`: a generator cannot
   produce a classpath. Extracting the promote stratum is what made them
   expressible, and the order-independence of `index` is the one that would
   actually have caught a plausible bug."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-gimp.contract :as contract]
            [hive-gimp.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-command-name
  (gen/fmap #(str "c_" %) (gen/elements ["a" "b" "c" "d"])))

(def gen-param
  (gen/let [n (gen/elements ["width" "height" "layer_name" "image_index"])
            t (gen/elements [:long :string :boolean :vector :map])]
    {:name (clojure.string/replace n "_" "-") :wire n :schema t :default nil :nilable? true}))

(def gen-descriptor
  ;; Parameters are de-duplicated by name: a Descriptor with the same parameter
  ;; twice is not a shape the contract can hold, and generating one would make
  ;; `describe-preserves-every-parameter` fail for a reason about the generator
  ;; rather than about the subject.
  (gen/let [command gen-command-name
            canon?  gen/boolean
            suffix  (gen/elements ["x" "y" "z"])
            params  (gen/vector gen-param 0 4)]
    {:command command
     :tool    (if canon? (contract/canonical-tool command) (str "gimp_alt_" suffix))
     :doc     "generated"
     :params  (vec (vals (into {} (map (juxt :name identity)) params)))}))

;; =============================================================================
;; prefer
;; =============================================================================

(defspec prefer-is-commutative 300
  ;; The property that makes `index` order-independent. A `prefer` that picked
  ;; by argument position would make the catalog depend on file order, and the
  ;; symptom would be a command that has parameters on one machine and not on
  ;; another.
  (prop/for-all [a gen-descriptor b gen-descriptor]
    (= (contract/prefer a b) (contract/prefer b a))))

(defspec prefer-is-idempotent 200
  (prop/for-all [a gen-descriptor]
    (= a (contract/prefer a a))))

(defspec prefer-always-returns-an-argument 300
  ;; A `prefer` that returned nil for an unanticipated case would silently drop
  ;; a command, and the catalog would simply be smaller than the contract.
  (prop/for-all [a gen-descriptor b gen-descriptor]
    (let [winner (contract/prefer a b)]
      (or (= winner a) (= winner b)))))

(deftest prefer-favours-the-canonical-claimant
  (let [borrowed  {:command "get_gimp_info" :tool "gimp_check_server" :doc "" :params []}
        canonical {:command "get_gimp_info" :tool "gimp_get_gimp_info" :doc ""
                   :params [{:name "verbose" :wire "verbose" :schema :boolean :default false}]}]
    (is (= canonical (contract/prefer borrowed canonical)))
    (is (= canonical (contract/prefer canonical borrowed)))))

;; =============================================================================
;; index
;; =============================================================================

(defspec index-is-independent-of-input-order 300
  ;; The heart of it, and not a theoretical concern: `index` folds `prefer`
  ;; over descriptors read from TWO resource files, so an order-dependence here
  ;; is a catalog that differs by file order.
  ;;
  ;; This property found a real one on its third case. `prefer` asked
  ;; `(canonical? a)` and then `(canonical? b)`, which answers differently for
  ;; (a, b) and (b, a) when BOTH are canonical, which is exactly what a row
  ;; duplicated across commands.edn and commands_extra.edn looks like.
  ;;
  ;; `(into {} (map (juxt :command identity)))` is last-write-wins and fails
  ;; this outright; that is the shape this stratum exists to keep out.
  (prop/for-all [descriptors (gen/vector gen-descriptor 0 12)]
    (let [expected (contract/index descriptors)]
      (= expected
         (contract/index (reverse descriptors))
         (contract/index (sort-by :tool descriptors))
         (contract/index (shuffle descriptors))))))

(defspec index-keeps-one-entry-per-command 300
  (prop/for-all [descriptors (gen/vector gen-descriptor 0 12)]
    (let [indexed (contract/index descriptors)]
      (and (= (set (keys indexed)) (set (map :command descriptors)))
           (every? (fn [[command d]] (= command (:command d))) indexed)))))

(defspec index-never-invents-a-descriptor 200
  (prop/for-all [descriptors (gen/vector gen-descriptor 0 12)]
    (every? (set descriptors) (vals (contract/index descriptors)))))

;; =============================================================================
;; aliases and lookup
;; =============================================================================

(defspec every-published-tool-name-resolves 300
  ;; Including the ones `index` dropped. Answering `unknown command` for a name
  ;; this contract itself publishes is worse than resolving it to the survivor.
  (prop/for-all [descriptors (gen/vector gen-descriptor 1 12)]
    (let [indexed (contract/index descriptors)
          alias-map (contract/aliases descriptors)]
      (every? #(some? (contract/lookup indexed alias-map (:tool %))) descriptors))))

(defspec lookup-accepts-both-vocabularies 300
  (prop/for-all [descriptors (gen/vector gen-descriptor 1 12)]
    (let [indexed (contract/index descriptors)
          alias-map (contract/aliases descriptors)]
      (every? (fn [[command d]]
                (= d (contract/lookup indexed alias-map command)
                   (contract/lookup indexed alias-map (:tool d))))
              (filter (fn [[_ d]] (= (:tool d) (get alias-map (:tool d) (:tool d)) (:tool d))) indexed)))))

(deftest lookup-of-an-unknown-name-is-nil-not-an-exception
  (is (nil? (contract/lookup {} {} "nope")))
  (is (nil? (contract/lookup {} {} nil))))

;; =============================================================================
;; describe
;; =============================================================================

(defspec describe-preserves-every-parameter 300
  (prop/for-all [d gen-descriptor]
    (= (mapv :name (:params d))
       (mapv :name (:params (contract/describe d))))))

(defspec describe-never-throws 300
  (prop/for-all [d gen-descriptor]
    (map? (contract/describe d))))

(deftest describe-marks-required-and-default-distinctly
  (let [d {:command "new_canvas" :tool "gimp_new_canvas" :doc ""
           :params [{:name "width" :wire "width" :schema :long :required? true}
                    {:name "fill" :wire "fill" :schema :string :default "white"}]}
        {:keys [params]} (contract/describe d)]
    (is (= {:name "width" :type "long" :required true} (first params)))
    (is (= {:name "fill" :type "string" :default "white"} (second params)))))

;; =============================================================================
;; search
;; =============================================================================

(defspec search-results-are-always-a-subset-of-the-catalog 200
  (prop/for-all [descriptors (gen/vector gen-descriptor 0 12)
                 q (gen/elements ["c_" "a" "generated" "zzz"])]
    (let [indexed (contract/index descriptors)]
      (every? (set (vals indexed)) (contract/search indexed q)))))

(deftest search-is-case-insensitive-on-both-sides
  (let [indexed {"blur_image" {:command "blur_image" :tool "gimp_blur_image"
                               :doc "Apply a Gaussian BLUR." :params []}}]
    (is (seq (contract/search indexed "BLUR")))
    (is (seq (contract/search indexed "gaussian")))))

;; =============================================================================
;; invalid
;; =============================================================================

(deftest invalid-names-the-offender
  (let [bad {:command "x" :tool "not_prefixed" :doc "" :params []}]
    (is (= 1 (count (contract/invalid [bad]))))
    (is (= "x" (:command (first (contract/invalid [bad])))))
    (is (some? (:problem (first (contract/invalid [bad])))))))

(defspec generated-descriptors-conform 200
  ;; A generator that produced non-conformant fixtures would make every
  ;; property above vacuous.
  (prop/for-all [d gen-descriptor]
    (schema/descriptor? d)))
