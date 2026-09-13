(ns hive-gimp.client-test
  "The pipeline end to end, against doubles. Nothing here redefines a var, and
   nothing here needs GIMP."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-gimp.client :as client]
            [hive-gimp.exec :as exec]
            [hive-gimp.response :as response]
            [hive-gimp.stub :as stub]
            [hive-gimp.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest a-success-is-unwrapped-once-and-only-once
  (let [t (stub/always (stub/success-frame {"width" 640}))
        outcome (client/invoke t "get_image_metadata")]
    (is (response/ok? outcome))
    (is (= {"width" 640} (:value outcome))
        "the envelope is interpreted here, not at eighty call sites")))

(deftest a-plugin-error-becomes-a-classified-outcome
  (let [t (stub/always (stub/error-frame "No images are currently open in GIMP"))
        outcome (client/invoke t "auto_levels")]
    (is (not (response/ok? outcome)))
    (is (= :gimp/no-image (:reason outcome))
        "actionable, where :gimp/command-failed would not be")))

(deftest an-unrecognised-error-is-not-forced-into-a-nearby-bucket
  (let [t (stub/always (stub/error-frame "Gegl operation failed in an unexpected way"))]
    (is (= :gimp/command-failed (:reason (client/invoke t "sharpen"))))))

(deftest a-transport-failure-arrives-in-the-same-shape-as-a-gimp-failure
  (testing "one error shape for all three failure origins"
    (let [t (stub/failing :gimp/not-listening "Nothing is listening on 127.0.0.1:9877.")
          outcome (client/invoke t "select_all")]
      (is (= :gimp/not-listening (:reason outcome)))
      (is (= :error (:outcome outcome)))
      (is (string? (:message outcome))))))

(deftest an-unforeseen-exception-is-reported-as-a-surprise
  (let [t (reify hive-gimp.ports/IGimpTransport
            (transport-id [_] :hostile)
            (round-trip! [_ _] (throw (RuntimeException. "kaboom"))))]
    (is (= :gimp/transport-error (:reason (client/invoke t "select_all")))
        "a surprise labelled as a familiar failure is worse than an honest surprise")))

(deftest a-promote-failure-never-reaches-the-socket
  (let [inner (stub/always (stub/success-frame "unreachable"))
        t     (stub/recording inner)
        outcome (client/invoke t "new_canvas" {:width 10})]
    (is (= :gimp/missing-parameter (:reason outcome)))
    (is (empty? (stub/sent-frames t))
        "an invalid call must not consume a GIMP round trip")))

(deftest an-unknown-command-is-answered-without-a-round-trip
  (let [t (stub/recording (stub/always (stub/success-frame "unreachable")))
        outcome (client/invoke t "definitely_not_a_command")]
    (is (= :gimp/unknown-command (:reason outcome)))
    (is (empty? (stub/sent-frames t)))))

(deftest a-tool-name-and-a-command-name-reach-the-same-command
  (let [t (stub/scripted (stub/success-frame "a") (stub/success-frame "b"))
        r (stub/recording t)]
    (client/invoke r "auto_levels")
    (client/invoke r "gimp_auto_levels")
    (is (= ["auto_levels" "auto_levels"] (mapv #(get % "type") (stub/sent-requests r)))
        "a client that echoes back the tool name it was given should not be punished")))

;; =============================================================================
;; Framing, asserted through the recording decorator
;; =============================================================================

(deftest the-frame-sent-is-the-frame-the-plugin-parses
  (let [t (stub/recording (stub/always (stub/success-frame "ok")))]
    (client/invoke t "auto_levels" {:image-index 2})
    (let [[req] (stub/sent-requests t)]
      (is (= "auto_levels" (get req "type")))
      (is (= 2 (get-in req ["params" "image_index"])))
      (is (contains? (get req "params") "layer_name")
          "the null optional is present, as the reference client sends it"))))

;; =============================================================================
;; exec
;; =============================================================================

(deftest exec-sends-the-marker-that-actually-evaluates
  (testing "the reference protocol document advertises a marker the plugin does not implement"
    (let [t (stub/recording (stub/always (stub/success-frame ["4"])))]
      (exec/run t ["2 + 2"] {:mode :eval})
      (let [[req] (stub/sent-requests t)]
        (is (= "python-fu-eval" (first (get-in req ["params" "args"])))
            "only this literal reaches the eval branch; pyGObject-eval silently execs")
        (is (= ["2 + 2"] (second (get-in req ["params" "args"]))))))))

(deftest exec-always-sends-params
  (testing "the plugin's fall-through branch reads j[\"params\"] unguarded"
    (let [t (stub/recording (stub/always (stub/success-frame ["ok"])))]
      (exec/run t ["print(1)"])
      (is (contains? (first (stub/sent-requests t)) "params")
          "omitting params would raise KeyError inside the plugin"))))

(deftest exec-preamble-is-opt-in
  (let [t (stub/recording (stub/always (stub/success-frame ["ok"])))]
    (exec/run t ["print(1)"])
    (is (= 1 (count (second (get-in (first (stub/sent-requests t)) ["params" "args"])))))
    (exec/run t ["print(1)"] {:preamble? true})
    (is (< 1 (count (second (get-in (second (stub/sent-requests t)) ["params" "args"]))))
        "a snippet that creates its own image must not be handed one")))

(deftest exec-rejects-a-non-string-payload
  (let [t (stub/recording (stub/always (stub/success-frame ["ok"])))]
    (is (= :gimp/invalid-parameter (:reason (exec/run t [42]))))
    (is (empty? (stub/sent-frames t)))))

;; =============================================================================
;; invoke!
;; =============================================================================

(deftest invoke-bang-throws-with-the-reason-attached
  (let [t (stub/always (stub/error-frame "No images are currently open in GIMP"))]
    (is (thrown? clojure.lang.ExceptionInfo (client/invoke! t "auto_levels")))
    (try
      (client/invoke! t "auto_levels")
      (catch clojure.lang.ExceptionInfo e
        (is (= :gimp/no-image (:hive-gimp/reason (ex-data e))))))))
