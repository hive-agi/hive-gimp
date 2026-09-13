(ns hive-gimp.doctor
  "PIPELINE. Runs the preflight stages and assembles the report.

   Every judgement this makes lives in `hive-gimp.doctor.verdict`, which is
   pure. What remains here is sequencing and the two calls that touch a
   transport, which is the only part that needs a double to exercise.

   Staged, and each stage is a DIFFERENT remediation. Collapsing them into one
   boolean is what makes an integration feel haunted: `GIMP is not available`
   is true whether the plugin was never installed, GIMP is closed, the port is
   held by something else, or GIMP is a major version this contract does not
   describe. Those are four different afternoons."
  (:require [clojure.string :as str]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.doctor.verdict :as verdict]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- stage
  [id ok? message extra]
  (merge {:stage id :ok? ok? :message message} extra))

;; =============================================================================
;; Stages
;; =============================================================================

(defn check-contract
  "The command contract loaded and conformed. Touches no transport, so it is
   the one stage that still reports something useful with GIMP closed."
  []
  (let [bad (catalog/invalid)]
    (stage :contract
           (empty? bad)
           (if (empty? bad)
             (str (count (catalog/commands)) " commands loaded.")
             (str (count bad) " command descriptor(s) do not conform to the schema."))
           {:commands (count (catalog/commands))
            :invalid  (vec (take 5 bad))})))

(defn check-plugin
  "The plugin is listening AND is the plugin.

   `check_server` is answered by the plugin itself, so a success here proves
   more than a completed TCP handshake does: anything can accept a connection
   on 9877."
  [transport]
  (let [outcome (client/invoke transport "check_server")]
    (if (response/ok? outcome)
      (stage :plugin true "The GIMP MCP plugin is running and answering."
             {:details (:value outcome)})
      (stage :plugin false (:message outcome)
             {:reason (:reason outcome)
              :hint   (verdict/connection-hint (:reason outcome))}))))

(defn check-version
  "GIMP's major version is one this contract describes."
  [transport]
  (let [outcome (client/invoke transport "get_gimp_info")]
    (if-not (response/ok? outcome)
      (stage :version false "Could not ask GIMP for its version." {:reason (:reason outcome)})
      (let [version (verdict/find-version (:value outcome))
            v       (verdict/version-verdict version)]
        (stage :version (:ok? v) (:message v)
               {:version version :status (:status v)})))))

(defn check-python
  "The OPTIONAL host-side Python port.

   Marked `:optional?` so it can never fail the report. A missing Python costs
   the pixel helpers and nothing else."
  [host-python]
  (let [{:keys [status hint modules]} (ports/python-status host-python)]
    (stage :python (= :python/available status)
           (if (= :python/available status)
             (str "Host-side Python available. Modules: "
                  (str/join ", " (map key (filter val modules))))
             (str "Host-side Python unavailable (" (name status)
                  "). This is optional; GIMP commands are unaffected."))
           {:optional? true :status status :hint hint :modules modules})))

;; =============================================================================
;; Report
;; =============================================================================

(defn report
  "Run every applicable stage and summarize.

   The plugin stage short-circuits the version stage on purpose: asking an
   absent GIMP for its version produces a second copy of the first error, which
   pushes the actionable line off the top of the report."
  [{:keys [transport host-python]}]
  (let [contract (check-contract)
        plugin   (when transport (check-plugin transport))
        version  (when (:ok? plugin) (check-version transport))
        python   (when host-python (check-python host-python))
        stages   (vec (remove nil? [contract plugin version python]))]
    {:ok?       (every? :ok? (remove :optional? stages))
     :transport (when transport (ports/transport-id transport))
     :stages    stages
     :summary   (verdict/summarize stages)}))
