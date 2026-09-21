(ns hive-gimp.codec-test
  "Framing is the part of this library that a bug hides in longest, because a
   desynced socket produces wrong ANSWERS rather than errors. These assertions
   are about the boundary, not about JSON."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-dsl.result :as r]
            [hive-gimp.codec :as codec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest encode-preserves-explicit-nulls
  (testing "an omitted optional is sent as null, exactly as the reference client sends it"
    (let [frame (codec/encode {:type "auto_levels"
                               :params {"image_index" 0 "layer_name" nil}})
          parsed (json/read-str frame)]
      (is (contains? (get parsed "params") "layer_name")
          "dropping the key would send a different message than every other client of this plugin")
      (is (nil? (get-in parsed ["params" "layer_name"]))))))

(deftest encode-terminates-the-frame
  (is (clojure.string/ends-with? (codec/encode {:type "select_all" :params {}}) "\n")))

(deftest complete-frame-requires-an-object
  (testing "the strengthening over both reference clients"
    ;; Both Python clients stop at the first successful parse of any JSON
    ;; value. A response truncated mid-object can leave a prefix that parses
    ;; as a bare value, and the rest of the real answer is then read as the
    ;; head of the next one.
    (is (false? (codec/complete-frame? "123")))
    (is (false? (codec/complete-frame? "\"partial\"")))
    (is (false? (codec/complete-frame? "[1,2]")))
    (is (true?  (codec/complete-frame? "{\"status\":\"success\"}"))))

  (testing "nothing is not a frame"
    (is (false? (codec/complete-frame? "")))
    (is (false? (codec/complete-frame? nil)))))

(deftest complete-frame-refuses-trailing-input
  (testing "an object followed by the head of another is not one whole frame"
    (is (false? (codec/complete-frame? "{\"status\":\"success\"}{\"sta")))
    (is (false? (codec/complete-frame? "{\"status\":\"success\"}{}")))
    (is (false? (codec/complete-frame? "{\"status\":\"success\"} x"))))
  (testing "JSON whitespace around the object is still a frame"
    (is (true? (codec/complete-frame? "{\"status\":\"success\"}\n")))
    (is (true? (codec/complete-frame? " \t{\"status\":\"success\"}\r\n"))))
  (testing "decode refuses the same buffers"
    (is (= :gimp/unparseable-response
           (:error (codec/decode "{\"status\":\"success\"}{\"sta"))))))

(deftest complete-frame-is-false-for-every-proper-prefix
  (testing "a partially received object never reads as complete"
    (let [whole (json/write-str {"status" "success"
                                 "results" {"width" 640 "height" 480 "name" "layer"}})]
      (doseq [n (range 1 (count whole))]
        (is (false? (codec/complete-frame? (subs whole 0 n)))
            (str "prefix of length " n " must not be treated as a whole frame"))))))

(deftest decode-distinguishes-its-three-failures
  (testing "unparseable, not-an-object and malformed are different diagnoses"
    (is (= :gimp/unparseable-response (:error (codec/decode "{\"status\":"))))
    (is (= :gimp/unparseable-response (:error (codec/decode "[1,2,3]"))))
    (is (= :gimp/malformed-response   (:error (codec/decode "{\"nope\":1}"))))
    (is (= :gimp/malformed-response   (:error (codec/decode "{\"status\":\"weird\"}")))
        "a status the plugin never emits is malformed, not success")))

(deftest decode-accepts-both-plugin-shapes
  (is (r/ok? (codec/decode "{\"status\":\"success\",\"results\":{\"a\":1}}")))
  (is (r/ok? (codec/decode "{\"status\":\"error\",\"error\":\"boom\"}")))
  (testing "results is open: a plugin newer than this contract must not be rejected"
    (is (r/ok? (codec/decode "{\"status\":\"success\",\"results\":1,\"future_field\":true}")))))

;; =============================================================================
;; Properties
;; =============================================================================

(def gen-command
  (gen/let [type   (gen/elements ["auto_levels" "select_all" "export_image" "new_canvas"])
            params (gen/map (gen/elements ["image_index" "layer_name" "file_path" "width"])
                            (gen/one-of [gen/small-integer
                                         (gen/return nil)
                                         gen/string-alphanumeric
                                         gen/boolean]))]
    {:type type :params params}))

(defspec encode-round-trips-through-json 200
  (prop/for-all [command gen-command]
    (let [parsed (json/read-str (codec/encode command))]
      (and (= (:type command) (get parsed "type"))
           (= (:params command) (get parsed "params"))))))

(defspec every-encoded-frame-is-a-complete-frame 200
  ;; The two halves of the protocol agree: what we produce is what our own
  ;; reader would accept as whole. A framing change that broke one and not the
  ;; other would show up here.
  (prop/for-all [command gen-command]
    (codec/complete-frame? (clojure.string/trim (codec/encode command)))))

(defspec decode-never-throws 300
  ;; The read loop calls this on whatever arrived. An exception here would
  ;; escape as an unattributed transport error.
  (prop/for-all [s gen/string]
    (let [result (codec/decode s)]
      (or (r/ok? result) (r/err? result)))))
