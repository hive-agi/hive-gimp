(ns hive-gimp.lifecycle.host-test
  "The host adapter's pure halves: what it sends GIMP and how it reads the
   answers of ss and gdbus. The effectful halves are proven live
   (dev/heal_live.clj), not here."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-gimp.lifecycle.host :as host]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private pids* @#'host/listener-pids)
(def ^:private argv* @#'host/launch-argv)

(def ss-line
  "One `ss -ltnpH` row, as GIMP's plug-in answers it on this box."
  "LISTEN 0      1          127.0.0.1:9877       0.0.0.0:*    users:((\"python3\",pid=%d,fd=12))")

(deftrifecta listener-pids hive-gimp.lifecycle.host/listener-pids
  {:golden-path "test/golden/hive_gimp/lifecycle/listener_pids.edn"
   :cases       {:none     ""
                 :nil      nil
                 :one      (format ss-line 233990)
                 :two-fds  "LISTEN 0 1 127.0.0.1:9877 0.0.0.0:* users:((\"gimp\",pid=10,fd=3),(\"python3\",pid=11,fd=4))"
                 :repeated (str (format ss-line 7) "\n" (format ss-line 7))
                 :no-users "LISTEN 0 4096 127.0.0.1:9877 0.0.0.0:*"}
   :gen         (gen/fmap (fn [ps] (str/join "\n" (map #(format ss-line %) ps)))
                          (gen/vector (gen/large-integer* {:min 1 :max 4194304}) 0 4))
   :pred        (fn [v] (and (vector? v) (every? pos-int? v) (= v (distinct v))))
   :mutations   [["first-only" (fn [s] (vec (take 1 (pids* s))))]
                 ["keeps-duplicates" (fn [s] (mapv (comp parse-long second) (re-seq #"pid=(\d+)" (or s ""))))]
                 ["strings" (fn [s] (mapv str (pids* s)))]]})

(deftrifecta launch-argv hive-gimp.lifecycle.host/launch-argv
  {:golden-path "test/golden/hive_gimp/lifecycle/launch_argv.edn"
   :apply?      true
   :cases       {:flatpak [host/default-gimp-argv "proc.run(cfg)"]
                 :native  [["gimp-3.2"] "x = 1"]}
   :gen         (gen/tuple (gen/vector gen/string-alphanumeric 1 3) gen/string-ascii)
   :pred        (fn [argv] (and (vector? argv) (every? string? argv)
                                (some #{"-n"} argv) (some #{"-i"} argv)))
   :mutations   [["reuses-the-gui-instance" (fn [g s] (filterv #(not= "-n" %) (argv* g s)))]
                 ["drops-the-batch" (fn [g _] (argv* g ""))]
                 ["script-fu" (fn [g s] (mapv #(if (= "python-fu-eval" %) "plug-in-script-fu-eval" %) (argv* g s)))]]})

(deftrifecta bus-answer-true? hive-gimp.lifecycle.host/bus-answer-true?
  {:golden-path "test/golden/hive_gimp/lifecycle/bus_answer.edn"
   :cases       {:true      "(true,)\n"
                 :false     "(false,)\n"
                 :error     "Error: GDBus.Error:org.freedesktop.DBus.Error.ServiceUnknown"
                 :true-text "Error: true is not a reply"
                 :empty     ""
                 :nil       nil}
   :gen         gen/string-ascii
   :pred        boolean?
   :mutations   [["any-output" (fn [s] (boolean (seq s)))]
                 ["contains-true" (fn [s] (boolean (some-> s (str/includes? "true"))))]]})

(deftest relaunch-source-runs-the-server-procedure-non-interactively
  (let [src (host/relaunch-source)]
    (is (= (str "from gi.repository import Gimp\n"
                "proc = Gimp.get_pdb().lookup_procedure(\"plug-in-mcp-server\")\n"
                "cfg = proc.create_config()\n"
                "cfg.set_property(\"run-mode\", Gimp.RunMode.NONINTERACTIVE)\n"
                "proc.run(cfg)")
           src))
    (is (str/includes? (host/relaunch-source "plug-in-other") "\"plug-in-other\"")
        "the procedure is a spliced literal, never interpolated source")))
