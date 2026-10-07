(ns hive-gimp.lifecycle.host
  "BOUNDARY. The lifecycle port against the real host: a TCP probe, the MCP
   plug-in's own `check_server`/`restart_server`, the session D-Bus, `ss`, and
   a headless flatpak GIMP this adapter launches and owns.

   The no-human channel into a RUNNING GUI GIMP is GIMP's own D-Bus service.
   GIMP 3 owns `org.gimp.GIMP.UI` on the session bus and exports
   `BatchRun(interpreter, command)`, the method `gimp -b` uses to hand a batch
   to an instance that is already open. Running `relaunch-source` through
   `python-fu-eval` there starts `plug-in-mcp-server` inside that GIMP. The
   call is queued by GIMP and answers `(true,)` at once; the server binds a
   moment later, which is why every remedy is followed by `:await-answer`.
   Nothing has to be installed in GIMP for this, and GIMP need not restart.

   Pure helpers (`relaunch-source`, `launch-argv`, `listener-pids`,
   `bus-answer-true?`) are public so the suite pins them without a host."
  (:require [clojure.string :as str]
            [hive-gimp.client :as client]
            [hive-gimp.lifecycle.model :as model]
            [hive-gimp.lifecycle.port :as port]
            [hive-gimp.py :as py]
            [hive-gimp.response :as response]
            [hive-gimp.transport.socket :as socket])
  (:import (java.io File)
           (java.net InetSocketAddress Socket)
           (java.time Instant)
           (java.util.concurrent TimeUnit)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Pure: what gets sent and how answers are read
;; =============================================================================

(def server-procedure
  "The reference plug-in's start procedure."
  "plug-in-mcp-server")

(defn relaunch-source
  "Python, compiled from forms, that runs `procedure` non-interactively.
   `proc.run` blocks for as long as the server lives, which is what keeps a
   headless GIMP up and is harmless in a GUI one (GIMP nests the call)."
  ([] (relaunch-source server-procedure))
  ([procedure]
   (py/->python
    (py/forms
     (import gi.repository [Gimp])
     (def proc (.lookup-procedure (Gimp/get-pdb) ~procedure))
     (def cfg (.create-config proc))
     (.set-property cfg "run-mode" Gimp.RunMode/NONINTERACTIVE)
     (.run proc cfg)))))

(def default-gimp-argv
  "How GIMP is started on this platform. The flatpak is what the README
   documents; override with `:gimp-argv` for a native install.

   `--die-with-parent` is load-bearing. `flatpak run` forks bwrap and the
   sandboxed GIMP under it; destroying only the `flatpak run` process orphans
   that tree to init, which keeps serving the port as a GIMP nobody owns
   (measured 2026-10-07). With it, the sandbox dies with its launcher, and so
   with this JVM."
  ["flatpak" "run" "--die-with-parent" "org.gimp.GIMP"])

(defn launch-argv
  "The full argv of a headless GIMP that runs `source` as its batch: no
   interface (-i), no data (-d), no fonts (-f), a new instance (-n) so it never
   hands the batch to a GUI GIMP that happens to be open."
  [gimp-argv source]
  (into (vec gimp-argv)
        ["-n" "-i" "-d" "-f" "--batch-interpreter" "python-fu-eval" "-b" source]))

(defn listener-pids
  "PIDs `ss -ltnpH` reports as holding a listening socket, distinct, in order."
  [ss-output]
  (->> (re-seq #"pid=(\d+)" (or ss-output ""))
       (map (comp parse-long second))
       distinct
       vec))

(defn bus-answer-true?
  "True when a `gdbus call` printed a boolean tuple `(true,)`."
  [out]
  (boolean (re-find #"^\(true,\)" (str/trim (or out "")))))

(defn- answering-value?
  [value]
  (true? (get-in value ["running"] (get value "running"))))

;; =============================================================================
;; Effects: processes
;; =============================================================================

(defn- run-argv
  "Run `argv`, wait at most `timeout-ms`. Never throws: {:exit :out :err}."
  [argv timeout-ms]
  (try
    (let [p (-> (ProcessBuilder. ^java.util.List (vec argv)) (.start))]
      (if (.waitFor p timeout-ms TimeUnit/MILLISECONDS)
        {:exit (.exitValue p) :out (slurp (.getInputStream p)) :err (slurp (.getErrorStream p))}
        (do (.destroyForcibly p) {:exit -1 :out "" :err (str "timed out after " timeout-ms " ms")})))
    (catch Exception e {:exit -1 :out "" :err (ex-message e)})))

(defn- listening?
  [{:keys [host port]}]
  (try
    (with-open [s (Socket.)]
      (.connect s (InetSocketAddress. ^String host (int port)) 1000)
      true)
    (catch Exception _ false)))

(defn- probe-transport
  "A socket transport with short timeouts: a probe must not wait 30 s on a
   wedged plug-in to learn that it is wedged."
  [endpoint]
  (socket/transport (assoc endpoint :timeout-ms 3000) 1000))

(defn- answering?
  [endpoint]
  (try
    (let [o (client/invoke (probe-transport endpoint) "check_server" {})]
      (and (response/ok? o) (answering-value? (response/value o))))
    (catch Throwable _ false)))

(def ^:private bus ["gdbus" "call" "--session" "--timeout" "5"])

(defn- gui?
  []
  (bus-answer-true?
   (:out (run-argv (into bus ["--dest" "org.freedesktop.DBus"
                              "--object-path" "/org/freedesktop/DBus"
                              "--method" "org.freedesktop.DBus.NameHasOwner"
                              "org.gimp.GIMP.UI"])
                   6000))))

(defn- alive? [^Process p] (boolean (some-> p .isAlive)))

(defn- stop-tree!
  "Stop `p` and every descendant, leaves first, then force what is left.
   A process alone is not enough: see `default-gimp-argv`."
  [^Process p]
  (let [kids (reverse (vec (.toList (.descendants p))))]
    (doseq [^java.lang.ProcessHandle h kids] (.destroy h))
    (.destroy p)
    (when-not (.waitFor p 10 TimeUnit/SECONDS) (.destroyForcibly p))
    (doseq [^java.lang.ProcessHandle h kids] (when (.isAlive h) (.destroyForcibly h)))
    (mapv #(.pid ^java.lang.ProcessHandle %) kids)))

;; =============================================================================
;; The adapter
;; =============================================================================

(defrecord HostLifecycle [opts owned]
  port/ILinkProbe
  (observe [_ endpoint]
    (let [listening (listening? endpoint)]
      {:port       (:port endpoint)
       :listening? listening
       :answering? (and listening (answering? endpoint))
       :gui?       (gui?)
       :owned?     (alive? (:process @owned))}))

  port/ILifecycle
  (restart-server! [_ endpoint]
    (let [o (try (client/invoke (probe-transport endpoint) "restart_server" {})
                 (catch Throwable t {:outcome :error :message (ex-message t)}))]
      {:ok? (boolean (response/ok? o)) :detail (if (response/ok? o) (response/value o) (:message o))}))

  (kill-listener! [_ endpoint]
    (let [ss   (run-argv ["ss" "-ltnpH" (str "sport = :" (:port endpoint))] 5000)
          pids (listener-pids (:out ss))]
      (doseq [pid pids] (run-argv ["kill" "-TERM" (str pid)] 5000))
      {:ok? (boolean (seq pids)) :detail {:pids pids}}))

  (relaunch-in-gui! [_ _endpoint]
    (let [r (run-argv (into bus ["--dest" "org.gimp.GIMP.UI"
                                 "--object-path" "/org/gimp/GIMP/UI"
                                 "--method" "org.gimp.GIMP.UI.BatchRun"
                                 "python-fu-eval" (relaunch-source)])
                      8000)]
      {:ok? (bus-answer-true? (:out r)) :detail (str/trim (str (:out r) (:err r)))}))

  (launch-headless! [_ _endpoint]
    (try
      (let [argv (launch-argv (:gimp-argv opts default-gimp-argv) (relaunch-source))
            log  (File. (System/getProperty "java.io.tmpdir") "hive-gimp-headless.log")
            p    (-> (ProcessBuilder. ^java.util.List argv)
                     (.redirectErrorStream true)
                     (.redirectOutput log)
                     (.start))
            handle {:pid (.pid p) :argv argv :log (str log) :started (str (Instant/now))}]
        (swap! owned assoc :process p :handle handle)
        {:ok? true :detail handle})
      (catch Exception e {:ok? false :detail (ex-message e)})))

  (stop-owned! [_]
    (let [^Process p (:process @owned)
          stopped    (when (alive? p) (stop-tree! p))]
      (swap! owned dissoc :process)
      {:ok? true :detail {:stopped (some-> (:handle @owned) :pid) :descendants (vec stopped)}}))

  port/IClock
  (now-ms [_] (System/currentTimeMillis))
  (sleep! [_ ms] (Thread/sleep (long ms))))

(defonce ^{:doc "One owner per JVM: the headless GIMP outlives any one heal."}
  owned-gimp
  (atom {}))

(defn system
  "A lifecycle system over the real host. `opts` may carry `:gimp-argv` and a
   partial `:policy`."
  ([] (system {}))
  ([opts]
   (let [adapter (->HostLifecycle opts owned-gimp)]
     {:probe adapter :lifecycle adapter :clock adapter
      :policy (merge model/default-policy (:policy opts))})))

(defn owned-process
  "The `ProcessHandle` of the headless GIMP this JVM launched, or nil."
  []
  (when (alive? (:process @owned-gimp)) (:handle @owned-gimp)))
