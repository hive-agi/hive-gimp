(ns hive-gimp.lifecycle.tool
  "FACADE. The MCP face of the lifecycle: `gimp_lifecycle` with
   `action` = status | heal.

   A separate tool rather than a `gimp` command because it must work exactly
   when the `gimp` commands cannot: with the plug-in dead there is nothing on
   the other end of the command socket to answer them."
  (:require [clojure.data.json :as json]
            [hive-gimp.lifecycle.heal :as heal]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- text [v] {:content [{:type "text" :text (json/write-str v)}]})

(defn summary
  "A `HealReport` without the per-round observations: what an agent needs to
   decide its next move, short enough to read."
  [report]
  {:outcome (:outcome report)
   :state   (:state report)
   :rounds  (mapv (fn [{:keys [plan results]}]
                    {:state   (:plan/state plan)
                     :verdict (:plan/verdict plan)
                     :why     (:plan/why plan)
                     :steps   (mapv #(select-keys % [:step/kind :ok? :detail]) results)})
                  (:rounds report))})

(defn handler
  [{:keys [system endpoint]} params]
  (let [action (or (get params "action") (get params :action) "status")]
    (case action
      "status" (text (heal/status system endpoint))
      "heal"   (let [report (heal/heal! system endpoint)]
                 (cond-> (text (summary report))
                   (= :failed (:outcome report)) (assoc :isError true)))
      {:isError true
       :content [{:type "text" :text (str "Unknown action " (pr-str action) ". One of: status, heal.")}]})))

(defn lifecycle-tool
  "`lifecycle` is {:system <heal system> :endpoint <Endpoint>}."
  [lifecycle]
  {:name "gimp_lifecycle"
   :description
   (str "Keep GIMP's MCP server up without a human. action=status observes the link "
        "(listening, answering check_server, GUI GIMP on D-Bus, headless GIMP owned by hive-gimp). "
        "action=heal brings it to answering: restart_server when wedged, relaunch the server inside a "
        "running GUI GIMP over D-Bus when it died, or launch a headless GIMP when none runs. "
        "Run heal when a gimp call fails with gimp/not-listening.")
   :inputSchema {:type "object" :additionalProperties false
                 :properties {"action" {:type "string" :enum ["status" "heal"]
                                        :description "status (default) observes; heal repairs."}}}
   :annotations {:readOnlyHint false :destructiveHint false
                 :idempotentHint true :openWorldHint true}
   :handler (partial handler lifecycle)})
