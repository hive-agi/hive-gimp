(ns hive-gimp.lifecycle.fake
  "A scripted world behind the lifecycle port: no socket, no bus, no process,
   no sleep. The world is one atom; each remedy is a `behaviour`, a pure
   `world -> [world' answer]`, so a scenario is DATA and a test reads as the
   story it tells.

   The clock is virtual: `sleep!` advances `:now`, and a launched GIMP that
   needs `:boot-ms` to bind only starts answering once the clock has passed
   its boot deadline. That is how the await step and the slow-boot escalation
   are tested without waiting."
  (:require [hive-gimp.lifecycle.model :as model]
            [hive-gimp.lifecycle.port :as port]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def endpoint {:host "127.0.0.1" :port 9877 :timeout-ms 3000})

(defn- up   [w] (assoc w :listening? true :answering? true))
(defn- down [w] (assoc w :listening? false :answering? false))

(def behaviours
  "Named remedies. Each answers `[world' {:ok? :detail}]`."
  {:restart-works   (fn [w] [(up w) {:ok? true}])
   :restart-noop    (fn [w] [w {:ok? true :detail "re-bound, still deaf"}])
   :restart-refused (fn [w] [w {:ok? false :detail "connection lost"}])
   :kill-works      (fn [w] [(down w) {:ok? true :detail {:pids [4242]}}])
   :kill-nothing    (fn [w] [w {:ok? false :detail {:pids []}}])
   :relaunch-works  (fn [w] [(up w) {:ok? true :detail "(true,)"}])
   :relaunch-deaf   (fn [w] [w {:ok? true :detail "(true,)"}])
   :relaunch-no-bus (fn [w] [w {:ok? false :detail "no org.gimp.GIMP.UI"}])
   :launch-works    (fn [w] [(-> w up (assoc :owned? true)) {:ok? true :detail {:pid 777}}])
   :launch-slow     (fn [w] [(assoc w :owned? true :boot-at (+ (:now w) (:boot-ms w 0)))
                             {:ok? true :detail {:pid 778}}])
   :launch-dies     (fn [w] [(assoc w :owned? false) {:ok? true :detail {:pid 779}}])
   :launch-fails    (fn [w] [w {:ok? false :detail "flatpak: not found"}])
   :stop-works      (fn [w] [(-> w down (assoc :owned? false) (dissoc :boot-at)) {:ok? true}])})

(defn- unscripted [w] [w {:ok? false :detail :unscripted}])

(defn- booted
  "The world as seen at its own clock: a slow launch binds once booted."
  [w]
  (if (and (:boot-at w) (:owned? w) (>= (:now w) (:boot-at w)))
    (-> w up (dissoc :boot-at))
    w))

(defn- act
  [{:keys [world script calls]} kind]
  (swap! calls conj kind)
  (let [b           (get behaviours (get script kind) unscripted)
        [w' answer] (b @world)]
    (reset! world w')
    answer))

(defrecord FakeLifecycle [world script calls]
  port/ILinkProbe
  (observe [_ endpoint]
    (let [w (swap! world booted)]
      (assoc (select-keys w [:listening? :answering? :gui? :owned?]) :port (:port endpoint))))

  port/ILifecycle
  (restart-server!  [this _] (act this :restart-server))
  (kill-listener!   [this _] (act this :kill-listener))
  (relaunch-in-gui! [this _] (act this :relaunch-in-gui))
  (launch-headless! [this _] (act this :launch-headless))
  (stop-owned!      [this]   (act this :stop-owned))

  port/IClock
  (now-ms [_] (:now @world))
  (sleep! [_ ms] (swap! world update :now + ms) nil))

(def fast-policy
  "The clock is virtual, so this only bounds how many polls an await makes."
  {:await-ms 30000 :poll-ms 500 :max-rounds 5})

(defn system
  "A heal system over a fresh fake world.

   `start`  the world: :listening? :answering? :gui? :owned?, optional
            :boot-ms (a slow launch) or :boot-at (already booting)
   `script` step kind -> behaviour name in `behaviours`"
  ([start script] (system start script fast-policy))
  ([start script policy]
   (let [f (->FakeLifecycle (atom (merge {:listening? false :answering? false
                                          :gui? false :owned? false :now 0}
                                         start))
                            script
                            (atom []))]
     {:probe f :lifecycle f :clock f :policy policy :fake f})))

(defn calls
  "The remedies a system's fake was asked for, in order."
  [system]
  @(:calls (:fake system)))

(def scenarios
  "Named stories: [start script]."
  {:healthy           [{:listening? true :answering? true :gui? true} {}]
   :gui-dead          [{:gui? true} {:relaunch-in-gui :relaunch-works}]
   :gui-dead-no-bus   [{:gui? true} {:relaunch-in-gui :relaunch-no-bus}]
   :gui-dead-deaf     [{:gui? true} {:relaunch-in-gui :relaunch-deaf}]
   :wedged-restart    [{:listening? true :gui? true} {:restart-server :restart-works}]
   :wedged-stubborn   [{:listening? true :gui? true}
                       {:restart-server :restart-noop :kill-listener :kill-works
                        :relaunch-in-gui :relaunch-works}]
   :wedged-unkillable [{:listening? true :gui? true}
                       {:restart-server :restart-noop :kill-listener :kill-nothing}]
   :absent            [{} {:launch-headless :launch-works}]
   :absent-slow-boot  [{:boot-ms 12000} {:launch-headless :launch-slow}]
   :absent-too-slow   [{:boot-ms 90000} {:launch-headless :launch-slow :stop-owned :stop-works}]
   :absent-no-flatpak [{} {:launch-headless :launch-fails}]
   :owned-booting     [{:owned? true :boot-at 4000} {}]})

(defn scenario-system
  "A fresh system for the named scenario."
  [k]
  (let [[start script] (get scenarios k)] (system start script)))

(defn report-summary
  "A `HealReport` reduced to what a golden should pin: the outcome, the final
   state, and per round the state seen and the remedies run with their ok?."
  [report]
  {:outcome (:outcome report)
   :state   (:state report)
   :rounds  (mapv (fn [{:keys [plan results]}]
                    [(:plan/state plan) (:plan/verdict plan)
                     (mapv (juxt :step/kind :ok?) results)])
                  (:rounds report))
   :valid?  (model/heal-report? report)})
