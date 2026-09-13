(ns hive-gimp.command-test
  "The promote layer. Every subject here takes a VALUE, which is what lets the
   generators below reach it at all."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]
            [hive-gimp.command :as command]
            [hive-gimp.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def descriptor
  {:command "adjust_curves"
   :tool    "gimp_adjust_curves"
   :doc     "Adjust tonal curves for a layer."
   :params  [{:name "preset" :wire "preset" :schema :string :default "s_curve"}
             {:name "points" :wire "points" :schema :vector :nilable? true :default nil}
             {:name "channel" :wire "channel" :schema :string :default "value"}
             {:name "image-index" :wire "image_index" :schema :long :default 0}
             {:name "layer-name" :wire "layer_name" :schema :string :nilable? true :default nil}]})

(def required-descriptor
  {:command "new_canvas"
   :tool    "gimp_new_canvas"
   :doc     "Create a new blank canvas."
   :params  [{:name "width" :wire "width" :schema :long :required? true}
             {:name "height" :wire "height" :schema :long :required? true}
             {:name "fill" :wire "fill" :schema :string :default "white"}]})

(deftest fixtures-are-real-descriptors
  ;; A fixture that drifts from the schema tests a contract nobody ships.
  (is (schema/descriptor? descriptor))
  (is (schema/descriptor? required-descriptor)))

;; =============================================================================
;; Defaults
;; =============================================================================

(deftest defaults-are-applied-here-not-left-to-the-plugin
  (let [{:keys [ok]} (command/->command descriptor {})]
    (is (= {"preset" "s_curve" "points" nil "channel" "value"
            "image_index" 0 "layer_name" nil}
           (:params ok))
        "the plugin reads absent params with .get, so a default that disagreed would be invisible")))

(deftest nil-and-false-survive-as-defaults
  (testing "contains? rather than a truthy check"
    (let [d (assoc descriptor :params [{:name "flag" :wire "flag" :schema :boolean :default false}])]
      (is (= {"flag" false} (:params (:ok (command/->command d {}))))))))

(deftest supplied-arguments-beat-defaults
  (is (= 3 (get-in (command/->command descriptor {:image-index 3}) [:ok :params "image_index"]))))

;; =============================================================================
;; Coercion
;; =============================================================================

(deftest coercion-is-lenient-about-shape-and-strict-about-type
  (testing "MCP clients are not uniformly typed"
    (is (= 5 (get-in (command/->command descriptor {:image-index "5"}) [:ok :params "image_index"])))
    (is (= [[0 0] [255 255]]
           (get-in (command/->command descriptor {:points "[[0,0],[255,255]]"}) [:ok :params "points"]))))

  (testing "but a value that cannot be read as its type is an error, never a pass-through"
    (let [res (command/->command descriptor {:image-index "not-a-number"})]
      (is (r/err? res))
      (is (= :gimp/invalid-parameter (:error res)))
      (is (re-find #"image-index" (:message res))
          "naming the parameter is the whole point; GIMP would answer with a traceback about someone else's line number"))))

(deftest a-whole-double-is-an-integer-that-survived-json
  (is (= 4 (get-in (command/->command descriptor {:image-index 4.0}) [:ok :params "image_index"])))
  (is (r/err? (command/->command descriptor {:image-index 4.5}))
      "a fractional value is not a silently truncated index"))

(deftest null-is-accepted-only-where-the-plugin-accepts-it
  (is (r/ok? (command/->command descriptor {:layer-name nil})))
  (is (r/err? (command/->command descriptor {:channel nil}))
      "channel has a default but is not nilable, so an explicit null is a different thing from omitting it"))

;; =============================================================================
;; Rejection
;; =============================================================================

(deftest unknown-parameters-are-rejected-with-the-accepted-list
  (let [res (command/->command descriptor {:imag-index 0})]
    (is (r/err? res))
    (is (= :gimp/unknown-parameter (:error res)))
    (is (re-find #"image-index" (:message res))
        "a typo should be answered with the list that contains the intended name")))

(deftest missing-required-parameters-are-named
  (let [res (command/->command required-descriptor {:width 100})]
    (is (= :gimp/missing-parameter (:error res)))
    (is (re-find #"height" (:message res)))))

(deftest a-non-descriptor-is-an-unknown-command
  (is (= :gimp/unknown-command (:error (command/->command {:nonsense true} {})))))

;; =============================================================================
;; Properties
;; =============================================================================

(defspec built-commands-always-conform 200
  (prop/for-all [args (gen/map (gen/elements [:preset :channel :image-index :layer-name])
                               (gen/one-of [gen/string-alphanumeric gen/small-integer (gen/return nil)]))]
    (let [res (command/->command descriptor args)]
      (or (r/err? res)
          (schema/gimp-command? (:ok res))))))

(defspec build-is-total 300
  ;; Never throws and never returns nil: every input is either a command or a
  ;; named error. The tool layer depends on this to have one error path.
  (prop/for-all [args (gen/map gen/keyword gen/any-printable)]
    (let [res (command/->command descriptor args)]
      (or (r/ok? res) (r/err? res)))))

(defspec every-built-param-key-is-a-wire-name 200
  ;; The vocabulary boundary: nothing kebab-case may cross into the wire map.
  (prop/for-all [args (gen/map (gen/elements [:preset :channel :image-index :layer-name])
                               gen/string-alphanumeric)]
    (let [res (command/->command descriptor args)]
      (or (r/err? res)
          (every? #(re-matches #"^[a-z][a-z0-9_]*$" %)
                  (keys (:params (:ok res))))))))
