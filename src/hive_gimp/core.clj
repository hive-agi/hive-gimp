(ns hive-gimp.core
  "FACADE. The public surface for a Clojure caller.

   Thin on purpose: every function here either wires a stratum to its
   collaborator or forwards to one. No behaviour is defined in this namespace,
   because a facade that reimplements a stratum is a second copy of it, and the
   copy is the one that drifts.

   Two ways in, and they are for different callers:

     a hive host   mounts `hive-gimp.addon/addon-ctor` and never sees this ns.
     a program     calls `(connect)` once and passes the result around.

   Usage:

     (require '[hive-gimp.core :as gimp])
     (def g (gimp/connect))
     (gimp/doctor g)
     (gimp/invoke g \"new_canvas\" {:width 800 :height 600})
     (gimp/exec g [\"Gimp.displays_flush()\"])"
  (:require [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.config :as config]
            [hive-gimp.doctor :as doctor]
            [hive-gimp.exec :as exec]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]
            [hive-gimp.transport.python :as python]
            [hive-gimp.transport.socket :as socket]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Wiring
;; =============================================================================

(defn connect
  "A session: a transport plus the host-side Python port.

   Opens no socket. The socket transport connects per command (the plugin hangs
   up after each one), so there is nothing to establish here and nothing to
   close. That is why this returns a plain map rather than something a caller
   must remember to release.

   `overrides` may carry :host, :port, :timeout-ms, :connect-timeout-ms,
   :python-executable and :transport; anything absent resolves from the
   environment.

   `:transport` selects the adapter, and both reach the same GIMP by the same
   protocol:

     :socket  a JVM socket. No Python anywhere. The default.
     :python  the same protocol driven from the embedded interpreter through
              libpython-clj, so GIMP commands and host-side pixel work share
              one Python context and an intermediate file never leaves it."
  ([] (connect {}))
  ([overrides]
   (let [endpoint    (config/endpoint overrides)
         connect-ms  (config/connect-timeout-ms overrides)
         python-exe  (or (:python-executable overrides) (config/python-executable))
         kind        (config/transport-kind overrides)]
     {:endpoint    endpoint
      :transport   (case kind
                     :python (python/transport endpoint connect-ms python-exe)
                     (socket/transport endpoint connect-ms))
      :host-python (python/host-python python-exe)})))

(defn with-transport
  "A session driving `transport`.

   The seam a caller needs to point hive-gimp at something that is not a
   socket: a recorded script, a tunnel, an in-process fake. Nothing downstream
   can tell the difference, which is the point of the port."
  ([transport] (with-transport transport (python/host-python nil)))
  ([transport host-python]
   {:endpoint    nil
    :transport   transport
    :host-python host-python}))

;; =============================================================================
;; Commands
;; =============================================================================

(defn invoke
  "Run a GIMP command. Returns an `Outcome`."
  ([session command] (invoke session command {}))
  ([session command args] (client/invoke (:transport session) command args)))

(defn invoke!
  "Run a GIMP command, returning the value and throwing on failure.

   For REPL and script use. A tool handler wants `invoke`, because an MCP
   client needs the error as data."
  ([session command] (invoke! session command {}))
  ([session command args] (client/invoke! (:transport session) command args)))

(defn exec
  "Run Python inside GIMP's interpreter. Returns an `Outcome`."
  ([session code] (exec/run (:transport session) code))
  ([session code opts] (exec/run (:transport session) code opts)))

(defn flush!
  "Push pending drawing operations to the display.

   Forwarded rather than inlined because the omission is the single most common
   way a correct GIMP script looks broken."
  [session]
  (exec/flush! (:transport session)))

;; =============================================================================
;; Introspection
;; =============================================================================

(defn commands
  "Every command name, sorted."
  []
  (catalog/command-names))

(defn describe
  "One command's parameters, types, defaults and documentation, or nil."
  [command]
  (some-> (catalog/descriptor command) catalog/describe))

(defn search
  "Commands whose name or documentation contains `q`."
  [q]
  (mapv catalog/describe (catalog/search q)))

(defn doctor
  "Staged preflight: contract, plugin, GIMP version, optional Python."
  [session]
  (doctor/report session))

(defn python-status
  "Why the optional host-side Python port is or is not usable."
  [session]
  (ports/python-status (:host-python session)))

;; =============================================================================
;; Outcome helpers
;; =============================================================================
;;
;; Re-exported so a caller does not have to require the promote stratum to
;; branch on a result it was handed.

(def ok?    response/ok?)
(def value  response/value)
(def ->result response/->result)
