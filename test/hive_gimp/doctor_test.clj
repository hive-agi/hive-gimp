(ns hive-gimp.doctor-test
  "The version gate matters more than it looks. This contract is derived from a
   GIMP 3.x plugin; on a 2.10 host the failures surface deep inside GIMP and
   read as nonsense. Deciding it here, over a value, is what makes the answer
   arrive before the confusion does."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-gimp.doctor :as doctor]
            [hive-gimp.stub :as stub]
            [hive-gimp.doctor.verdict :as verdict]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Version parsing
;; =============================================================================

(deftest parse-version-reads-the-shapes-gimp-actually-reports
  (is (= 3 (:major (verdict/parse-version "3.0.4"))))
  (is (= 2 (:major (verdict/parse-version "GNU Image Manipulation Program version 2.10.36"))))
  (is (= 10 (:minor (verdict/parse-version "2.10.36"))))
  (is (= 2 (:minor (verdict/parse-version "3.2"))))
  (is (nil? (:patch (verdict/parse-version "3.2"))))
  (is (nil? (verdict/parse-version "no version here"))))

;; =============================================================================
;; Verdicts
;; =============================================================================

(deftest gimp-2-is-rejected-with-the-reason-and-the-fix
  (let [v (verdict/version-verdict (verdict/parse-version "2.10.36"))]
    (is (false? (:ok? v)))
    (is (= :gimp/version-too-old (:status v)))
    (is (re-find #"2\.10" (:message v)))
    (is (re-find #"flatpak" (:message v))
        "a verdict without a remediation is a complaint")))

(deftest gimp-3-passes
  (let [v (verdict/version-verdict (verdict/parse-version "3.0.4"))]
    (is (true? (:ok? v)))
    (is (= :gimp/version-ok (:status v)))))

(deftest a-newer-major-is-a-warning-not-a-refusal
  (let [v (verdict/version-verdict (verdict/parse-version "4.0.0"))]
    (is (true? (:ok? v))
        "refusing to run against a future GIMP would be a bug that ships on a calendar")
    (is (= :gimp/version-newer (:status v)))))

(deftest an-unreadable-version-is-its-own-verdict
  (let [v (verdict/version-verdict nil)]
    (is (false? (:ok? v)))
    (is (= :gimp/version-unknown (:status v)))))

;; =============================================================================
;; Version discovery walks the payload
;; =============================================================================

(deftest the-version-is-found-wherever-the-plugin-put-it
  (testing "the plugin builds this map from several accessors and the useful one moves"
    (let [t (stub/always (stub/success-frame {"version" {"version_method" "3.0.4"}}))
          stage (doctor/check-version t)]
      (is (true? (:ok? stage)))
      (is (= 3 (get-in stage [:version :major]))))

    (let [t (stub/always (stub/success-frame {"env" {"nested" {"deep" ["GIMP 3.2.1"]}}}))
          stage (doctor/check-version t)]
      (is (= 3 (get-in stage [:version :major]))))))

;; =============================================================================
;; Report
;; =============================================================================

(deftest a-closed-gimp-produces-one-actionable-line-not-two-copies-of-it
  (let [report (doctor/report {:transport (stub/failing :gimp/not-listening
                                                        "Nothing is listening on 127.0.0.1:9877.")})]
    (is (false? (:ok? report)))
    (is (= #{:contract :plugin} (set (map :stage (:stages report))))
        "the version stage must not run and repeat the connection error")
    (is (re-find #"Start MCP Server" (->> (:stages report) (filter #(= :plugin (:stage %))) first :hint)))))

(deftest a-healthy-stack-reports-ready
  (let [t (stub/always (stub/success-frame {"running" true "version" "3.0.4"}))
        report (doctor/report {:transport t})]
    (is (true? (:ok? report)))
    (is (= "hive-gimp is ready." (:summary report)))))

(deftest a-live-gimp-on-the-wrong-major-fails-the-report
  (let [t (stub/always (stub/success-frame {"version" {"version_method" "2.10.36"}}))
        report (doctor/report {:transport t})]
    (is (false? (:ok? report)))
    (is (re-find #"version" (:summary report)))))

(deftest the-optional-python-stage-never-fails-the-report
  (testing "a doctor that reports red for an optional capability trains its reader to ignore it"
    (let [t (stub/always (stub/success-frame {"version" "3.0.4"}))
          report (doctor/report {:transport t
                                 :host-python (stub/host-python :python/no-libpython)})]
      (is (true? (:ok? report)))
      (is (true? (->> (:stages report) (filter #(= :python (:stage %))) first :optional?))))))

(deftest the-contract-stage-runs-without-a-transport
  (let [report (doctor/report {})]
    (is (true? (:ok? report)))
    (is (= [:contract] (map :stage (:stages report))))))
