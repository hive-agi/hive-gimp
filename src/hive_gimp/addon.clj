(ns hive-gimp.addon
  "The IAddon. Wiring only.

   Note what this namespace does NOT require: `hive-mcp`. hive-mcp is the host,
   a runtime rather than a dependency, and a load-time require on it would make
   the published jar unresolvable from a maven fetch. What the host needs from
   this addon is expressed as a contract, `hive-addon.protocol/IAddon`, and the
   host injects itself.

   `initialize!` deliberately does NOT connect to GIMP. Mounting an addon must
   not fail because an unrelated desktop application is closed, and the socket
   transport opens per command anyway, so there is no connection to establish
   at mount time. GIMP being absent is reported by `health` and diagnosed by
   `gimp_doctor`, which is where a user can act on it."
  (:require [hive-addon.protocol :as addon]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.config :as config]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]
            [hive-gimp.tools :as tools]
            [hive-gimp.transport.python :as python]
            [hive-gimp.transport.socket :as socket]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def owner "hive.gimp")

(defrecord GimpAddon [state config]
  addon/IAddon

  (addon-id [_] owner)
  (addon-type [_] :external)
  (capabilities [_] #{:tools :health-reporting})

  (initialize! [_ cfg]
    (try
      (if (:ready? @state)
        {:success? true :already-initialized? true}
        (let [settings    (merge config cfg)
              endpoint    (config/endpoint settings)
              connect-ms  (config/connect-timeout-ms settings)
              python-exe  (or (:python-executable settings) (config/python-executable))
              kind        (config/transport-kind settings)
              transport   (case kind
                            :python (python/transport endpoint connect-ms python-exe)
                            (socket/transport endpoint connect-ms))
              host-python (python/host-python python-exe)
              invalid     (catalog/invalid)]
          ;; A malformed contract is the one thing worth refusing to mount for.
          ;; Every command would fail later with a parameter error that names
          ;; the caller rather than the resource that is actually broken.
          (if (seq invalid)
            {:success? false
             :errors [(str "The GIMP command contract is invalid: "
                           (count invalid) " descriptor(s) do not conform.")]}
            (do
              (reset! state {:ready?      true
                             :endpoint    endpoint
                             :transport   transport
                             :host-python host-python})
              {:success? true :errors []}))))
      (catch Exception e
        {:success? false
         :errors [(str "hive-gimp failed to initialize: " (ex-message e))]})))

  (shutdown! [_]
    (python/reset-cache!)
    (reset! state {:ready? false})
    nil)

  (tools [_]
    (let [{:keys [ready? transport host-python]} @state]
      (if ready?
        (tools/tools transport host-python)
        [])))

  (schema-extensions [_] {})

  (excluded-tools [_] #{})

  (hooks [_] {})

  (health [_]
    (let [{:keys [ready? endpoint transport]} @state]
      (if-not ready?
        {:status :down :details {:initialized? false}}
        ;; `check_server` is answered by the plugin itself, so a success here
        ;; means the plugin is live rather than that a socket accepted us.
        ;; Health must never throw: a health check that dies reports nothing,
        ;; which reads as a broken host rather than a closed GIMP.
        (let [outcome (try
                        (client/invoke transport "check_server")
                        (catch Throwable t
                          {:outcome :error :reason :gimp/transport-error
                           :message (ex-message t)}))
              live?   (response/ok? outcome)]
          {:status (if live? :ok :degraded)
           :details {:initialized? true
                     :endpoint     endpoint
                     :transport    (ports/transport-id transport)
                     :commands     (count (catalog/commands))
                     :gimp         (if live? :reachable :unreachable)
                     :reason       (when-not live? (:reason outcome))
                     :hint         (when-not live?
                                     "GIMP commands will fail until the plugin is running. Run the gimp_doctor tool for a staged diagnosis.")}})))))

(defn addon-ctor
  "Host entry point. `config` may carry :host, :port, :timeout-ms,
   :connect-timeout-ms and :python-executable; anything absent resolves from
   the environment."
  [config]
  (->GimpAddon (atom {:ready? false}) (or config {})))
