(ns hive-gimp.config
  "COLLECT. Environment-driven configuration, resolved once at the boundary.

   Defaults match the reference GIMP plugin as shipped (localhost:9877) so a
   stock install needs no configuration at all. Every field is overridable,
   because a plugin reached over an ssh tunnel or from a container is the same
   protocol at a different address and should not need a code change.

   No literal home directory and no machine-specific path appears here: this
   namespace ships inside a jar to consumers who are not on this box."
  (:require [hive-di.core :refer [defconfig env coalesce]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def default-host "127.0.0.1")
(def default-port 9877)

(def default-timeout-ms
  "30 seconds.

   Not arbitrary: the reference client uses 30s, and GIMP operations are
   synchronous on its main thread, so a scale or an export of a large image
   holds the socket for the whole operation. A shorter timeout does not cancel
   the GIMP-side work, it only makes us stop listening to it, which leaves the
   next request reading the tail of the previous answer."
  30000)

(def default-connect-timeout-ms
  "5 seconds.

   Separate from the read timeout on purpose. Connecting is fast or impossible:
   if the plugin is not listening, the OS refuses immediately, and the only
   case that takes time is an unreachable host. Waiting a read timeout to learn
   that GIMP is not running is 30 seconds of nothing."
  5000)

(defconfig GimpConfig
  :host (env "HIVE_GIMP_HOST"
             :default  default-host
             :type     :string
             :doc      "Host the GIMP MCP plugin socket listens on.")
  :port (env "HIVE_GIMP_PORT"
             :default  default-port
             :type     :int
             :doc      "Port the GIMP MCP plugin socket listens on (plugin default 9877).")
  :timeout-ms (env "HIVE_GIMP_TIMEOUT_MS"
                   :default  default-timeout-ms
                   :type     :int
                   :doc      "Read timeout for one GIMP command, in milliseconds.")
  :connect-timeout-ms (env "HIVE_GIMP_CONNECT_TIMEOUT_MS"
                           :default  default-connect-timeout-ms
                           :type     :int
                           :doc      "Connect timeout, in milliseconds.")
  :transport (env "HIVE_GIMP_TRANSPORT"
                  :default  "socket"
                  :type     :string
                  :doc      "Which adapter reaches the GIMP plugin: \"socket\" (default, a JVM socket, no Python at all) or \"python\" (the same protocol driven from the embedded interpreter through libpython-clj, so GIMP work and host-side pixel work share one Python context). Both satisfy IGimpTransport and every command behaves identically through either.")
  :python-executable (coalesce [(env "HIVE_GIMP_PYTHON")
                                (env "HIVE_PYTHON_EXECUTABLE")]
                               :type     :string
                               :required false
                               :doc      "Python interpreter for the Python transport and the host-side pixel port. Unrelated to GIMP's own interpreter, which no external process can drive."))

(defn endpoint
  "The resolved `Endpoint`, merged with any explicit overrides.

   Overrides win over the environment so a caller holding a config map (a test,
   a second GIMP on another port) does not have to mutate process environment
   to be heard."
  ([] (endpoint {}))
  ([overrides]
   (let [resolved (or (:ok (resolve-GimpConfig))
                      {:host default-host
                       :port default-port
                       :timeout-ms default-timeout-ms
                       :connect-timeout-ms default-connect-timeout-ms})]
     (merge (select-keys resolved [:host :port :timeout-ms])
            (select-keys overrides [:host :port :timeout-ms])))))

(defn connect-timeout-ms
  ([] (connect-timeout-ms {}))
  ([overrides]
   (or (:connect-timeout-ms overrides)
       (:connect-timeout-ms (:ok (resolve-GimpConfig)))
       default-connect-timeout-ms)))

(defn transport-kind
  "Which transport adapter to build: `:socket` or `:python`.

   Anything unrecognised resolves to `:socket` rather than throwing. This is
   read at mount time in a host process, and refusing to start an addon over a
   typo in an optional environment variable trades a working default for an
   outage. The chosen adapter is reported by `transport-id` in health and in
   the doctor, so a typo is visible rather than silent."
  ([] (transport-kind {}))
  ([overrides]
   (let [raw (or (:transport overrides)
                 (:transport (:ok (resolve-GimpConfig)))
                 "socket")]
     (if (= "python" (clojure.string/lower-case (name raw)))
       :python
       :socket))))

(defn python-executable
  "Interpreter for the optional host-side Python port, or nil to let
   libpython-clj autodetect."
  []
  (:python-executable (:ok (resolve-GimpConfig))))
