(ns hive-gimp.lifecycle.model-test
  "The pure decision core: observation -> LinkState -> HealPlan."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-gimp.lifecycle.model :as model]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def gen-observation (hs/generator :hive-gimp.lifecycle/observation))
(def gen-history     (gen/vector (hs/generator :hive-gimp.lifecycle/step-kind) 0 6))

(defn- obs [& {:as flags}]
  (merge {:port 9877 :listening? false :answering? false :gui? false :owned? false} flags))

(def link-state? (set (rest model/LinkState)))

;; =============================================================================
;; classify
;; =============================================================================

(def ^:private classify* @#'model/classify)

(deftrifecta classify hive-gimp.lifecycle.model/classify
  {:golden-path "test/golden/hive_gimp/lifecycle/classify.edn"
   :cases       {:answering        (obs :listening? true :answering? true :gui? true)
                 :answering-owned  (obs :listening? true :answering? true :owned? true)
                 :wedged-in-gui    (obs :listening? true :gui? true)
                 :wedged-headless  (obs :listening? true :owned? true)
                 :dead-in-gui      (obs :gui? true)
                 :dead-gui-and-own (obs :gui? true :owned? true)
                 :dead-owned       (obs :owned? true)
                 :absent           (obs)}
   :gen         gen-observation
   :pred        link-state?
   :mutations   [["gui-beats-wedged"
                  (fn [o] (if (and (:gui? o) (not (:answering? o))) :link/dead-in-gui (classify* o)))]
                 ["listening-is-answering"
                  (fn [o] (classify* (assoc o :answering? (:listening? o))))]
                 ["owned-beats-gui"
                  (fn [o] (if (and (:owned? o) (not (:listening? o))) :link/dead-owned (classify* o)))]
                 ["always-absent" (constantly :link/absent)]]})

;; =============================================================================
;; plan
;; =============================================================================

(def ^:private plan* @#'model/plan)

(defn- coherent-plan?
  "Schema-valid, and the verdict agrees with the steps and the state."
  [p]
  (and (model/heal-plan? p)
       (= (= :healthy (:plan/verdict p)) (= :link/answering (:plan/state p)))
       (= (= :heal (:plan/verdict p)) (boolean (seq (:plan/steps p))))))

(defn- step-kinds [p] (mapv :step/kind (:plan/steps p)))

(deftrifecta plan hive-gimp.lifecycle.model/plan
  {:golden-path "test/golden/hive_gimp/lifecycle/plan.edn"
   :apply?      true
   :cases       {:healthy                [(obs :listening? true :answering? true) []]
                 :wedged-fresh           [(obs :listening? true :gui? true) []]
                 :wedged-after-restart   [(obs :listening? true :gui? true) [:restart-server :await-answer]]
                 :wedged-after-kill      [(obs :listening? true :gui? true) [:restart-server :await-answer :kill-listener]]
                 :gui-dead-fresh         [(obs :gui? true) []]
                 :gui-dead-after-kill    [(obs :gui? true) [:restart-server :await-answer :kill-listener]]
                 :gui-dead-relaunched    [(obs :gui? true) [:relaunch-in-gui :await-answer]]
                 :owned-booting          [(obs :owned? true) []]
                 :owned-after-await      [(obs :owned? true) [:await-answer]]
                 :owned-after-one-launch [(obs :owned? true) [:launch-headless :await-answer]]
                 :owned-after-two        [(obs :owned? true) [:launch-headless :await-answer :stop-owned :launch-headless :await-answer]]
                 :absent-fresh           [(obs) []]
                 :absent-after-one       [(obs) [:launch-headless]]
                 :absent-after-two       [(obs) [:launch-headless :launch-headless]]}
   :gen         (gen/tuple gen-observation gen-history)
   :pred        coherent-plan?
   :mutations   [["ignores-history"
                  (fn [o _h & more] (apply plan* o [] more))]
                 ["always-heals"
                  (fn [o h & more] (assoc (apply plan* o h more) :plan/verdict :heal))]
                 ["absent-relaunches-in-gui"
                  (fn [o h & more]
                    (let [p (apply plan* o h more)]
                      (if (= :link/absent (:plan/state p))
                        (assoc p :plan/steps [{:step/kind :relaunch-in-gui}])
                        p)))]
                 ["no-await-after-remedy"
                  (fn [o h & more]
                    (update (apply plan* o h more) :plan/steps
                            (fn [ss] (filterv #(not= :await-answer (:step/kind %)) ss))))]
                 ["kills-before-restart"
                  (fn [o h & more]
                    (let [p (apply plan* o h more)]
                      (if (and (= :link/wedged (:plan/state p)) (empty? h))
                        (assoc p :plan/steps [{:step/kind :kill-listener}])
                        p)))]]})

;; =============================================================================
;; Relations the output-only property cannot state
;; =============================================================================

(defspec plan-state-is-the-classification 300
  (prop/for-all [[o h] (gen/tuple gen-observation gen-history)]
    (= (model/classify o) (:plan/state (model/plan o h)))))

(defspec a-remedy-is-never-repeated-past-its-budget 300
  ;; Escalation is what bounds heal!: once a remedy's budget is spent the plan
  ;; must not name it again.
  (prop/for-all [o gen-observation]
    (let [spent {:restart-server  [:restart-server]
                 :kill-listener   [:restart-server :kill-listener]
                 :relaunch-in-gui [:relaunch-in-gui]
                 :launch-headless [:launch-headless :launch-headless]}]
      (every? (fn [[kind history]]
                (not-any? #{kind} (step-kinds (model/plan o history))))
              spent))))

(deftest every-state-has-a-remedy
  ;; OCP: a LinkState without a plan-for method would throw here, not in prod.
  (doseq [s (rest model/LinkState)]
    (is (contains? (methods model/plan-for) s) (str s " has no plan-for method"))))
