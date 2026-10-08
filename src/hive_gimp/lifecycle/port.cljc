(ns hive-gimp.lifecycle.port
  "The lifecycle seams. Three small protocols, split by reason to change (ISP):

     ILinkProbe   look, never touch: one `Observation` of the link.
     ILifecycle   the remedies a `HealPlan` names, one method per step kind
                  that acts on the world.
     IClock       time, so awaiting an answer is testable without sleeping.

   The host adapters live in `hive-gimp.lifecycle.host`; the suite's fakes in
   `test/hive_gimp/lifecycle/fake.clj`. Every method returns data and never
   throws for an expected failure: a remedy that did not work is a
   `{:ok? false}` the planner escalates on, not an exception that ends the
   heal before the next remedy is tried.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defprotocol ILinkProbe
  (observe [probe endpoint]
    "An `Observation` of the MCP link at `endpoint` ({:host :port ...})."))

(defprotocol ILifecycle
  (restart-server! [lc endpoint]
    "Ask a live plug-in to drop and re-bind its socket (`restart_server`).")
  (kill-listener! [lc endpoint]
    "Stop the process holding `endpoint`'s port. GIMP itself survives: the
     holder is the plug-in process, not the GUI.")
  (relaunch-in-gui! [lc endpoint]
    "Start the MCP server procedure inside the running GUI GIMP, no human.")
  (launch-headless! [lc endpoint]
    "Start a headless GIMP, owned by hive-gimp, that runs the MCP server.
     Answers `{:ok? true :detail ProcessHandle}`.")
  (stop-owned! [lc]
    "Stop the headless GIMP this adapter launched, if any."))

(defprotocol IClock
  (now-ms [clock] "Milliseconds, monotonic enough to measure a wait.")
  (sleep! [clock ms] "Wait `ms`."))
