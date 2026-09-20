(ns hive-gimp.client
  "PIPELINE. Descriptor lookup, build, encode, send, decode, interpret.

   Six steps, five of them pure. The single impure one is a call through
   `IGimpTransport`, which is why the whole pipeline is testable end to end
   against a scripted double and why no test in this repo redefines a var.

   Every exit is an `Outcome`. A caller cannot receive a naked exception from
   here, and cannot receive nil: a transport that threw, a parameter that would
   not coerce and an error raised inside GIMP all arrive in the same shape,
   distinguished by `:reason`."
  (:require [hive-dsl.result :as r]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.codec :as codec]
            [hive-gimp.command :as command]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]
            [hive-help.core :as help]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- transport-failure
  "An exception from a transport, as an `Outcome`.

   `:hive-gimp/reason` is set by every failure the socket adapter raises. Its
   absence means something unforeseen escaped, and that is reported as
   `:gimp/transport-error` rather than being folded into a known reason: a
   surprise labelled as a familiar failure is worse than an honest surprise."
  [command ^Throwable t]
  {:outcome :error
   :command command
   :reason  (or (:hive-gimp/reason (ex-data t)) :gimp/transport-error)
   :message (ex-message t)
   :detail  (some-> (ex-data t) (dissoc :hive-gimp/reason) not-empty pr-str)})

(defn send-command
  "Send an already-built `GimpCommand` through `transport`. Returns an `Outcome`.

   Separate from `invoke` because the exec escape hatch and the doctor both
   need to send a command that no descriptor describes."
  [transport gimp-command]
  (let [command (:type gimp-command)]
    (try
      (let [frame    (codec/encode gimp-command)
            raw      (ports/round-trip! transport frame)
            decoded  (codec/decode raw)]
        (if (r/err? decoded)
          (response/from-error command decoded)
          (response/interpret command (:ok decoded))))
      (catch Throwable t
        (transport-failure command t)))))

(defn unknown-command-outcome
  "The `Outcome` for a name no descriptor describes.

   The contract ships ~80 commands in two vocabularies (`place_text` and
   `gimp_place_text`), which is exactly the size where a typo is likely and a
   bare refusal is useless. hive-help supplies the fleet's shared shape for
   this, including edit-distance suggestions, so the reader is told what they
   probably meant instead of being sent to go and read a catalog.

   Lives here, at the boundary: `hive-gimp.command` answers the error VALUE
   and is .cljc, so it cannot require hive-help."
  [command-or-tool]
  (let [names (vec (catalog/command-names))]
    {:outcome :error
     :command (str command-or-tool)
     :reason  :gimp/unknown-command
     :message (help/unknown-command
               {:tool           "gimp_exec"
                :command        (str command-or-tool)
                :valid-commands (help/suggest command-or-tool names 12)
                :examples       ["new_canvas" "place_text" "export_image"]})
     :detail  (str "The contract describes " (count names)
                   " commands. gimp_catalog lists or searches all of them.")}))

(defn invoke
  "Run `command-or-tool` with kebab-case `args`. Returns an `Outcome`.

   Accepts either vocabulary for the command name, for the reason given on
   `catalog/descriptor`."
  ([transport command-or-tool] (invoke transport command-or-tool {}))
  ([transport command-or-tool args]
   (if-let [descriptor (catalog/descriptor command-or-tool)]
     (let [built (command/->command descriptor args)]
       (if (r/err? built)
         (response/from-error (:command descriptor) built)
         (send-command transport (:ok built))))
     (unknown-command-outcome command-or-tool))))

(defn invoke!
  "Like `invoke`, but throws on failure and returns the value on success.

   For REPL and script use, where a Result-shaped answer is friction and a
   stack trace is what you wanted anyway. Never call this from a tool handler:
   an MCP client needs the error as data."
  ([transport command-or-tool] (invoke! transport command-or-tool {}))
  ([transport command-or-tool args]
   (let [outcome (invoke transport command-or-tool args)]
     (if (response/ok? outcome)
       (:value outcome)
       (throw (ex-info (:message outcome)
                       {:hive-gimp/reason (:reason outcome)
                        :command          (:command outcome)
                        :detail           (:detail outcome)}))))))
