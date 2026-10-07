(ns hive-gimp.lifecycle.heal-test
  "The heal loop over the scripted fake world: every scenario is a story with
   a pinned outcome and remedy sequence, and the loop always terminates with a
   valid report whatever the world does."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-gimp.lifecycle.fake :as fake]
            [hive-gimp.lifecycle.heal :as heal]
            [hive-gimp.lifecycle.model :as model]
            [hive-gimp.lifecycle.tool :as tool]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private heal* @#'heal/heal!)

(defn- case-of [k] [(fake/scenario-system k) fake/endpoint])

(def gen-world
  "Any starting world and any script: every remedy either works or fails in
   one of its scripted ways, or is unscripted."
  (gen/fmap
   (fn [[obs boot script]]
     [(fake/system (assoc (select-keys obs [:listening? :answering? :gui? :owned?]) :boot-ms boot)
                   script)
      fake/endpoint])
   (gen/tuple (gen/hash-map :listening? gen/boolean :answering? gen/boolean
                            :gui? gen/boolean :owned? gen/boolean)
              (gen/elements [0 5000 40000 120000])
              (gen/hash-map
               :restart-server  (gen/elements [:restart-works :restart-noop :restart-refused nil])
               :kill-listener   (gen/elements [:kill-works :kill-nothing nil])
               :relaunch-in-gui (gen/elements [:relaunch-works :relaunch-deaf :relaunch-no-bus nil])
               :launch-headless (gen/elements [:launch-works :launch-slow :launch-dies :launch-fails nil])
               :stop-owned      (gen/elements [:stop-works nil])))))

(defn- bounded-and-valid?
  "Terminates inside the round budget with a schema-valid report, and an
   outcome that agrees with the final state."
  [report]
  (and (model/heal-report? report)
       (<= (count (:rounds report)) (inc (:max-rounds fake/fast-policy)))
       (= (contains? #{:healed :already-healthy} (:outcome report))
          (= :link/answering (:state report)))))

(deftrifecta heal hive-gimp.lifecycle.heal/heal!
  {:golden-path "test/golden/hive_gimp/lifecycle/heal.edn"
   :apply?      true
   :cases       (into {} (map (fn [k] [k (case-of k)])) (keys fake/scenarios))
   :xf          fake/report-summary
   :gen         gen-world
   :pred        bounded-and-valid?
   :num-tests   300
   :mutations   [["one-round-only"
                  (fn [s e] (update (heal* (assoc-in s [:policy :max-rounds] 1) e) :rounds identity))]
                 ["no-wait-for-boot"
                  (fn [s e] (heal* (assoc-in s [:policy :await-ms] 0) e))]
                 ["claims-healed"
                  (fn [s e] (assoc (heal* s e) :outcome :healed))]
                 ["forgets-history"
                  ;; Re-plans every round from scratch: the stubborn wedge
                  ;; restarts forever instead of escalating to a kill.
                  (fn [s e]
                    (loop [i 0]
                      (let [r (heal* (assoc-in s [:policy :max-rounds] 1) e)]
                        (if (or (not= :failed (:outcome r)) (>= i 3)) r (recur (inc i))))))]]})

(deftest the-gui-heal-relaunches-over-the-bus-then-awaits
  (let [s (fake/scenario-system :gui-dead)
        r (heal/heal! s fake/endpoint)]
    (is (= :healed (:outcome r)))
    (is (= [:relaunch-in-gui] (fake/calls s)) "no restart, no kill, no headless GIMP")))

(deftest a-stubborn-wedge-escalates-restart-kill-relaunch
  (let [s (fake/scenario-system :wedged-stubborn)]
    (heal/heal! s fake/endpoint)
    (is (= [:restart-server :kill-listener :relaunch-in-gui] (fake/calls s)))))

(deftest a-slow-boot-is-awaited-not-relaunched
  (let [s (fake/scenario-system :absent-slow-boot)
        r (heal/heal! s fake/endpoint)]
    (is (= :healed (:outcome r)))
    (is (= [:launch-headless] (fake/calls s)))
    (is (= 12000 (-> r :rounds first :results second :detail :waited-ms)))))

(deftest status-observes-without-acting
  (let [s (fake/scenario-system :gui-dead)]
    (is (= {:observed {:port 9877 :listening? false :answering? false :gui? true :owned? false}
            :state    :link/dead-in-gui}
           (heal/status s fake/endpoint)))
    (is (empty? (fake/calls s)))))

(deftest the-mcp-tool-heals-and-reports-failure-as-an-error
  (testing "heal"
    (let [res (tool/handler {:system (fake/scenario-system :gui-dead) :endpoint fake/endpoint}
                            {"action" "heal"})]
      (is (not (:isError res)))
      (is (re-find #"\"outcome\":\"healed\"" (-> res :content first :text)))))
  (testing "a heal that gives up is an MCP error"
    (is (:isError (tool/handler {:system (fake/scenario-system :gui-dead-no-bus) :endpoint fake/endpoint}
                                {"action" "heal"}))))
  (testing "status is the default action"
    (is (re-find #"dead-in-gui"
                 (-> (tool/handler {:system (fake/scenario-system :gui-dead) :endpoint fake/endpoint} {})
                     :content first :text))))
  (testing "an unknown action is refused with the choices"
    (is (:isError (tool/handler {:system (fake/scenario-system :healthy) :endpoint fake/endpoint}
                                {"action" "reboot"})))))
