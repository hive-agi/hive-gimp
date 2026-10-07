(ns hive-gimp.lifecycle.heal
  "PIPELINE. Observe -> plan -> run the plan's steps through the port, round
   after round, until the planner says healthy or gives up.

   The loop owns no decision. Which remedy, and when to stop escalating, is
   `hive-gimp.lifecycle.model/plan`; this namespace only carries the history
   forward and records every round, so the report says exactly what was seen,
   what was decided and what each step answered.

   A `system` is `{:probe ILinkProbe :lifecycle ILifecycle :clock IClock
   :policy Policy}`. Adding a step kind is a `defmethod run-step` here plus its
   enum member in `model/StepKind` (OCP)."
  (:require [hive-gimp.lifecycle.model :as model]
            [hive-gimp.lifecycle.port :as port]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Steps
;; =============================================================================

(defmulti run-step
  "Run one `Step`, answering a `StepResult`."
  (fn [_system _endpoint step] (:step/kind step)))

(defn- result
  "A `StepResult` from an adapter answer `{:ok? :detail}`."
  [step answer]
  (cond-> {:step/kind (:step/kind step) :ok? (boolean (:ok? answer))}
    (contains? answer :detail) (assoc :detail (:detail answer))))

(defmethod run-step :restart-server [{:keys [lifecycle]} endpoint step]
  (result step (port/restart-server! lifecycle endpoint)))

(defmethod run-step :kill-listener [{:keys [lifecycle]} endpoint step]
  (result step (port/kill-listener! lifecycle endpoint)))

(defmethod run-step :relaunch-in-gui [{:keys [lifecycle]} endpoint step]
  (result step (port/relaunch-in-gui! lifecycle endpoint)))

(defmethod run-step :launch-headless [{:keys [lifecycle]} endpoint step]
  (result step (port/launch-headless! lifecycle endpoint)))

(defmethod run-step :stop-owned [{:keys [lifecycle]} _endpoint step]
  (result step (port/stop-owned! lifecycle)))

(defmethod run-step :await-answer [{:keys [probe clock]} endpoint step]
  (let [timeout (:step/timeout-ms step 0)
        poll    (:step/poll-ms step 500)
        start   (port/now-ms clock)]
    (loop []
      (let [obs (port/observe probe endpoint)]
        (cond
          (:answering? obs)
          {:step/kind :await-answer :ok? true
           :detail {:waited-ms (- (port/now-ms clock) start)}}

          (>= (- (port/now-ms clock) start) timeout)
          {:step/kind :await-answer :ok? false
           :detail {:waited-ms (- (port/now-ms clock) start) :last obs}}

          :else
          (do (port/sleep! clock poll) (recur)))))))

;; =============================================================================
;; Rounds
;; =============================================================================

(defn- run-steps
  "Run `steps` in order, stopping after the first failure: a launch that did
   not start leaves nothing for the await after it to wait on."
  [system endpoint steps]
  (reduce (fn [acc step]
            (let [r (run-step system endpoint step)]
              (if (:ok? r) (conj acc r) (reduced (conj acc r)))))
          []
          steps))

(defn- outcome
  [rounds plan]
  (case (:plan/verdict plan)
    :healthy (if (= 1 (count rounds)) :already-healthy :healed)
    :failed))

(defn status
  "One observation and its classification, touching nothing."
  [{:keys [probe]} endpoint]
  (let [obs (port/observe probe endpoint)]
    {:observed obs :state (model/classify obs)}))

(defn heal!
  "Bring the MCP link at `endpoint` to answering, without a human.

   Returns a `HealReport`: the outcome, the final state, and every round as
   {:observed :plan :results}. Bounded by the policy's `:max-rounds`."
  [{:keys [probe policy] :as system} endpoint]
  (let [policy (merge model/default-policy policy)]
    (loop [rounds [] history []]
      (let [obs  (port/observe probe endpoint)
            plan (model/plan obs history policy)]
        (if (or (not= :heal (:plan/verdict plan))
                (>= (count rounds) (:max-rounds policy)))
          (let [rounds (conj rounds {:observed obs :plan plan :results []})]
            {:outcome (outcome rounds plan)
             :state   (:plan/state plan)
             :rounds  rounds})
          (let [results (run-steps system endpoint (:plan/steps plan))]
            (recur (conj rounds {:observed obs :plan plan :results results})
                   (into history (map :step/kind) results))))))))
