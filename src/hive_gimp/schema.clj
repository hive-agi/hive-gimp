(ns hive-gimp.schema
  "Value objects for hive-gimp, malli first.

   Everything downstream is defined in terms of these, and the suite is
   synthesized from them rather than hand-written: `deftrifecta-from-schema`
   needs an `:in` schema, so a function that takes a VALUE gets generated
   coverage for free while a function that takes an id or a bag of positional
   arguments gets none. That is the reason the promote layer here is written
   over `Descriptor` and `GimpCommand` rather than over a command name.

   Two vocabularies meet in this namespace and must not be confused:

     WIRE   snake_case strings, the GIMP plugin's own contract. `:wire`,
            `GimpCommand`, `RawResponse`. JSON keys stay strings.
     HIVE   kebab-case keywords, what a Clojure caller and an MCP client see.
            `:name`, `Invocation`.

   `hive-gimp.command` is the only place the two are allowed to touch."
  (:require [malli.core :as m]
            [malli.error :as me]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Names
;; =============================================================================

(def CommandName
  "A GIMP-side command `type`. snake_case, because the plugin dispatches on the
   literal string."
  [:re {:error/message "must be a snake_case GIMP command name"}
   #"^[a-z][a-z0-9_]*$"])

(def ToolName
  "The MCP-facing name of a command. Always `gimp_` prefixed so a host that
   flattens every addon's tools into one namespace cannot collide."
  [:re {:error/message "must be a gimp_-prefixed tool name"}
   #"^gimp_[a-z][a-z0-9_]*$"])

(def ParamName
  "A parameter as a Clojure caller spells it: kebab-case."
  [:re {:error/message "must be a kebab-case parameter name"}
   #"^[a-z][a-z0-9-]*$"])

(def WireName
  "A parameter as the plugin spells it: snake_case."
  [:re {:error/message "must be a snake_case wire parameter name"}
   #"^[a-z][a-z0-9_]*$"])

;; =============================================================================
;; The command contract (resources/hive_gimp/commands.edn)
;; =============================================================================

(def ParamType
  "The scalar shapes the GIMP plugin accepts over JSON. `:any` is the honest
   answer for an unannotated parameter, not a licence to send anything: the
   plugin still validates, and an `:any` simply means this contract cannot say
   more than JSON already does."
  [:enum :long :double :string :boolean :vector :map :any :nil])

(def ParamSpec
  "One parameter of one command.

   `:required?` and `:default` are mutually exclusive by construction: the
   generator emits `:required? true` exactly when the Python signature had no
   default. `:nilable?` is separate and means the plugin accepts an explicit
   JSON null, which is NOT the same as the parameter being absent. That
   distinction is load-bearing: the reference client sends `\"layer_name\": null`
   and the plugin reads it back with `.get`, so dropping the key would change
   behaviour on a plugin that ever starts distinguishing the two."
  [:map {:closed true}
   [:name      ParamName]
   [:wire      WireName]
   [:schema    ParamType]
   [:nilable?  {:optional true} :boolean]
   [:required? {:optional true} :boolean]
   [:default   {:optional true} :any]
   [:doc       {:optional true} :string]])

(def Descriptor
  "One row of the derived command contract. The whole tool surface is a
   sequence of these, which is what makes adding a GIMP command a data change
   rather than a new `defn` (OCP)."
  [:map {:closed true}
   [:command CommandName]
   [:tool    ToolName]
   [:doc     :string]
   [:params  [:sequential ParamSpec]]])

(def Catalog
  "The contract as loaded: descriptors indexed by command."
  [:map-of CommandName Descriptor])

;; =============================================================================
;; Invocation (hive vocabulary) and GimpCommand (wire vocabulary)
;; =============================================================================

(def Invocation
  "What a caller asks for, before defaults and before translation to the wire.
   Argument keys are kebab-case keywords matching `ParamSpec :name`."
  [:map {:closed true}
   [:command CommandName]
   [:args    [:map-of :keyword :any]]])

(def GimpCommand
  "One framed request, exactly as the plugin will parse it. String keys, wire
   spellings, defaults already applied. This is the boundary value: everything
   above it is Clojure, everything below it is JSON on a socket."
  [:map {:closed true}
   [:type   CommandName]
   [:params [:map-of WireName :any]]])

;; =============================================================================
;; Responses
;; =============================================================================

(def RawResponse
  "The plugin's answer, straight off the wire, before interpretation.

   Open on purpose: `:results` carries whatever the command returns, and a
   closed schema here would reject a plugin newer than this contract. The one
   thing we do insist on is that `status` is present and is one of the two
   words the plugin actually emits, because an answer without a status is not a
   response, it is a framing bug wearing one."
  [:map
   ["status" [:enum "success" "error"]]
   ["results"   {:optional true} :any]
   ["error"     {:optional true} :string]
   ["traceback" {:optional true} :string]])

(def Outcome
  "The interpreted answer. A closed two-variant shape rather than a bare map,
   so a caller that forgets to branch fails at the schema instead of silently
   treating an error payload as a result."
  [:multi {:dispatch :outcome}
   [:ok  [:map {:closed true}
          [:outcome [:= :ok]]
          [:command CommandName]
          [:value   :any]]]
   [:error [:map {:closed true}
            [:outcome [:= :error]]
            [:command CommandName]
            [:reason  :keyword]
            [:message :string]
            [:detail  {:optional true} [:maybe :string]]]]])

;; =============================================================================
;; Transport configuration
;; =============================================================================

(def Endpoint
  "Where the GIMP plugin socket lives, and how long we are willing to wait.

   `:host` is deliberately not free-form in practice (the plugin binds
   localhost) but is kept a plain string so an ssh-forwarded or container
   endpoint stays expressible without a code change."
  [:map {:closed true}
   [:host       :string]
   [:port       [:int {:min 1 :max 65535}]]
   [:timeout-ms [:int {:min 1 :max 600000}]]])

(def ExecMode
  "The two things GIMP's Python-Fu bridge can do with a string.

   `:eval` is reached ONLY by the literal marker `python-fu-eval`; every other
   marker falls through to exec. The reference project's own protocol document
   advertises a `pyGObject-eval` marker that does not exist in the plugin's
   dispatch, so a caller following that document gets exec semantics and a
   `[\"None\"]` result instead of a value. `hive-gimp.exec` emits the marker
   that actually works."
  [:enum :exec :eval])

(def ExecRequest
  [:map {:closed true}
   [:mode ExecMode]
   [:code [:sequential :string]]])

;; =============================================================================
;; Validators (compiled once; m/validate recompiles the schema on every call)
;; =============================================================================

(def descriptor?    (m/validator Descriptor))
(def gimp-command?  (m/validator GimpCommand))
(def raw-response?  (m/validator RawResponse))
(def outcome?       (m/validator Outcome))
(def endpoint?      (m/validator Endpoint))
(def exec-request?  (m/validator ExecRequest))

(defn explain
  "Human-readable explanation of why `value` fails `?schema`, or nil when it
   conforms. Used in error paths, never in hot paths."
  [?schema value]
  (some-> (m/explain ?schema value) me/humanize pr-str))
