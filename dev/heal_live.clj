(ns heal-live
  "LIVE proof of hive-gimp.core/heal! against a real GIMP, no human.

     clojure -M:test -i dev/heal_live.clj -e '(heal-live/-main)'
     (or load it in a REPL and call (heal-live/-main))

   1. If the MCP plug-in on 9877 is alive, kill it the way a PDB calling
      error does: the plug-in process dies, the GUI stays up. (`--keep` skips
      this when it is already dead.)
   2. `status` must say the link is down.
   3. `heal!` with no human.
   4. `py/eval!` must answer [w h] of an image: the first open one, or a
      scratch 64x48 created and deleted for the purpose."
  (:require [hive-gimp.core :as gimp]
            [hive-gimp.lifecycle.host :as host]
            [hive-gimp.lifecycle.port :as port]
            [hive-gimp.py :as py]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- say [& xs] (apply println "HEAL-LIVE:" xs) (flush))

(defn image-size
  "[w h] of the first open image, or of a scratch image when none is open."
  [g]
  (py/eval! g
    (def imgs (Gimp/get-images))
    (if (> (len imgs) 0)
      (def size [(.get-width (first imgs)) (.get-height (first imgs))])
      (do (def scratch (Gimp.Image/new 64 48 Gimp.ImageBaseType/RGB))
          (def size [(.get-width scratch) (.get-height scratch)])
          (.delete scratch)))
    size))

(defn -main
  [& args]
  (let [g        (gimp/connect)
        endpoint (:endpoint g)
        adapter  (:lifecycle (host/system))]
    (say "before" (gimp/status g))
    (when-not (some #{"--keep"} args)
      (when (:listening? (:observed (gimp/status g)))
        (say "killing the plug-in (simulated PDB crash):" (port/kill-listener! adapter endpoint))
        (Thread/sleep 1000)))
    (let [down (gimp/status g)]
      (say "down" down)
      (assert (not= :link/answering (:state down)) "the link should be down before healing"))
    (let [report (gimp/heal! g)]
      (say "heal outcome" (:outcome report) "state" (:state report))
      (doseq [{:keys [plan results]} (:rounds report)]
        (say "  round" (:plan/state plan) (:plan/verdict plan)
             (mapv (juxt :step/kind :ok?) results)))
      (assert (= :healed (:outcome report)) "heal! should have healed the link"))
    (let [size (image-size g)]
      (say "py/eval! [w h] =" size)
      (assert (and (vector? size) (every? pos-int? size)))
      size)))
