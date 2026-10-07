(ns hive-gimp.lifecycle.model
  "DOMAIN. The link to GIMP's MCP server as a value, and the decision of what
   to do about it as a value.

   Nothing here touches a socket, a process or a bus. An adapter OBSERVES the
   world into an `Observation`; this namespace CLASSIFIES it into a
   `LinkState` and PLANS a `HealPlan`; the executor in
   `hive-gimp.lifecycle.heal` runs the plan's steps through the port. The
   decision is therefore testable with nothing but maps.

   Two open sets, both extended by registration (OCP):

     classify    rules are data in `classification`, checked in order; a new
                 state is a new row.
     plan-for    a multimethod on the state; a new state's remedy is a
                 `defmethod`, never an edit to a `case`.

   Escalation is a function of HISTORY, the step kinds already run in this
   heal. A remedy that was tried and did not take is not tried again the same
   way: `restart-server` before `kill-listener`, one relaunch before giving
   up. That is what keeps `heal!` bounded without a retry counter per state."
  (:require [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Schemas
;; =============================================================================

(def Port [:int {:min 1 :max 65535}])

(def Observation
  "What an adapter saw, all at one instant.

     :listening?  something accepts TCP on the port
     :answering?  the MCP plug-in answered `check_server` with running true
     :gui?        a GUI GIMP owns `org.gimp.GIMP.UI` on the session bus
     :owned?      a headless GIMP that hive-gimp launched is still alive"
  [:map {:closed true}
   [:port       Port]
   [:listening? :boolean]
   [:answering? :boolean]
   [:gui?       :boolean]
   [:owned?     :boolean]])

(def LinkState
  [:enum :link/answering :link/wedged :link/dead-in-gui :link/dead-owned :link/absent])

(def StepKind
  [:enum :restart-server :kill-listener :relaunch-in-gui :stop-owned
   :launch-headless :await-answer])

(def Step
  [:map {:closed true}
   [:step/kind       StepKind]
   [:step/timeout-ms {:optional true} [:int {:min 0}]]
   [:step/poll-ms    {:optional true} [:int {:min 1}]]])

(def History
  "Step kinds already run in this heal, oldest first."
  [:vector StepKind])

(def Verdict [:enum :healthy :heal :give-up])

(def HealPlan
  [:map {:closed true}
   [:plan/state   LinkState]
   [:plan/verdict Verdict]
   [:plan/steps   [:vector Step]]
   [:plan/why     :string]])

(def Policy
  [:map {:closed true}
   [:await-ms   [:int {:min 0}]]
   [:poll-ms    [:int {:min 1}]]
   [:max-rounds [:int {:min 1 :max 16}]]])

(def ProcessHandle
  "A headless GIMP hive-gimp launched: enough to report and to stop it."
  [:map {:closed true}
   [:pid     [:int {:min 1}]]
   [:argv    [:vector :string]]
   [:log     :string]
   [:started :string]])

(def StepResult
  [:map {:closed true}
   [:step/kind StepKind]
   [:ok?       :boolean]
   [:detail    {:optional true} :any]])

(def Round
  [:map {:closed true}
   [:observed Observation]
   [:plan     HealPlan]
   [:results  [:vector StepResult]]])

(def Outcome [:enum :already-healthy :healed :failed])

(def HealReport
  [:map {:closed true}
   [:outcome Outcome]
   [:state   LinkState]
   [:rounds  [:vector Round]]])

(def registered-schemas
  {:hive-gimp.lifecycle/port           Port
   :hive-gimp.lifecycle/observation    Observation
   :hive-gimp.lifecycle/link-state     LinkState
   :hive-gimp.lifecycle/step-kind      StepKind
   :hive-gimp.lifecycle/step           Step
   :hive-gimp.lifecycle/history        History
   :hive-gimp.lifecycle/heal-plan      HealPlan
   :hive-gimp.lifecycle/policy         Policy
   :hive-gimp.lifecycle/process-handle ProcessHandle
   :hive-gimp.lifecycle/step-result    StepResult
   :hive-gimp.lifecycle/round          Round
   :hive-gimp.lifecycle/heal-report    HealReport})

(hs/register-all! registered-schemas)

(def observation?  (m/validator Observation))
(def heal-plan?    (m/validator HealPlan))
(def heal-report?  (m/validator HealReport))
(def process-handle? (m/validator ProcessHandle))

(def default-policy
  "Headless GIMP 3.2 takes 10-25 s to load its plug-ins before the MCP
   procedure runs, so a minute is the honest wait, not a generous one."
  {:await-ms 60000 :poll-ms 500 :max-rounds 5})

;; =============================================================================
;; Classify (rules as data, first match wins)
;; =============================================================================

(def classification
  "Ordered `[state predicate]` rows. Order is meaning: answering beats
   everything, and a listener that does not answer is wedged whoever owns it."
  [[:link/answering   :answering?]
   [:link/wedged      :listening?]
   [:link/dead-in-gui :gui?]
   [:link/dead-owned  :owned?]
   [:link/absent      (constantly true)]])

(defn classify
  "The `LinkState` of an `Observation`."
  [observation]
  (some (fn [[state pred]] (when (pred observation) state)) classification))

;; =============================================================================
;; Plan
;; =============================================================================

(defn- tried? [history kind] (some #{kind} history))

(defn- tries [history kind] (count (filter #{kind} history)))

(defn- await-step
  [policy]
  {:step/kind :await-answer
   :step/timeout-ms (:await-ms policy)
   :step/poll-ms (:poll-ms policy)})

(defn- heal-plan
  [state why steps]
  {:plan/state state :plan/verdict :heal :plan/steps (vec steps) :plan/why why})

(defn- give-up
  [state why]
  {:plan/state state :plan/verdict :give-up :plan/steps [] :plan/why why})

(defmulti plan-for
  "The remedy for one `LinkState`, given what this heal already tried."
  (fn [state _observation _history _policy] state))

(defmethod plan-for :link/answering [state _ _ _]
  {:plan/state state :plan/verdict :healthy :plan/steps []
   :plan/why "The MCP server answers check_server."})

(defmethod plan-for :link/wedged [state _ history policy]
  (cond
    (not (tried? history :restart-server))
    (heal-plan state "Listening but not answering: ask the plug-in to re-bind its socket."
               [{:step/kind :restart-server} (await-step policy)])

    (not (tried? history :kill-listener))
    (heal-plan state "restart_server did not take: stop the process holding the port, then relaunch."
               [{:step/kind :kill-listener}])

    :else
    (give-up state "The port stays wedged after a restart and a kill; something other than the plug-in holds it.")))

(defmethod plan-for :link/dead-in-gui [state _ history policy]
  (if (tried? history :relaunch-in-gui)
    (give-up state "A relaunch inside the running GIMP did not bring the server up. Check that gimp-mcp-plugin is installed in GIMP's plug-ins directory.")
    (heal-plan state "GIMP is running but its MCP server is dead: relaunch the plug-in inside it over D-Bus."
               [{:step/kind :relaunch-in-gui} (await-step policy)])))

(defmethod plan-for :link/dead-owned [state _ history policy]
  (cond
    (empty? history)
    (heal-plan state "A headless GIMP hive-gimp launched is alive but not listening yet: it may still be loading plug-ins."
               [(await-step policy)])

    (< (tries history :launch-headless) 2)
    (heal-plan state "The headless GIMP hive-gimp launched never answered: stop it and launch again."
               [{:step/kind :stop-owned} {:step/kind :launch-headless} (await-step policy)])

    :else
    (give-up state "Two headless launches never answered. Read the launch log in the heal report.")))

(defmethod plan-for :link/absent [state _ history policy]
  (if (< (tries history :launch-headless) 2)
    (heal-plan state "No GIMP is running: launch a headless GIMP that starts the MCP server."
               [{:step/kind :launch-headless} (await-step policy)])
    (give-up state "Headless launches exited without a server. Is the GIMP flatpak installed?")))

(defn plan
  "The `HealPlan` for `observation`, given the `History` of this heal.

   `[observation history]` uses `default-policy`."
  ([observation history] (plan observation history default-policy))
  ([observation history policy]
   (plan-for (classify observation) observation (vec history) policy)))
