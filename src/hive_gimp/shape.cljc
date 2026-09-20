(ns hive-gimp.shape
  "The value objects of hive-gimp, as DATA.

   A malli schema is a vector and a map, so every definition below is already
   portable; only compiling one into a function (`m/validator`, `m/explain`)
   needs malli, and that stays in `hive-gimp.schema`, on the JVM.

   That split is the point. Until 2026-09-20 `hive-gimp.schema` held both,
   and `hive-gimp.contract`, `hive-gimp.command` and `hive-gimp.response`
   required it, which pinned the command vocabulary to the JVM for the sake
   of three validity checks. hive-creator's production core and
   hive-kdenlive's MLT core both keep malli strictly at the boundary; this
   namespace is hive-gimp joining them. See docs/portable-core.md.

   Two vocabularies meet here and must not be confused:

     WIRE   snake_case strings, the GIMP plugin's own contract. `:wire`,
            `GimpCommand`, `RawResponse`. JSON keys stay strings.
     HIVE   kebab-case keywords, what a Clojure caller and an MCP client see.
            `:name`, `Invocation`.

   `hive-gimp.command` is the only place the two are allowed to touch.

   Portable: clojure.core only. No reader conditionals, by the fleet rule
   that a host difference means the code belongs at a boundary instead.")

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
;; Portable predicates
;;
;; The same two shape gates `hive-gimp.schema` compiles with m/validator, hand
;; written so the command pipeline can check them on a host without malli.
;; `hive-gimp.schema` keeps the malli versions and the humanized `explain`,
;; which is what an error path shows a reader; these answer only yes or no.
;;
;; They are not a second source of truth: `shape-equivalence-test` asserts
;; they agree with the malli validators over the whole shipped contract and
;; over hand-built counter-examples. If they ever disagree, the malli schema
;; above is right and these are the bug.
;; =============================================================================

(def ^:private command-name-re #"^[a-z][a-z0-9_]*$")
(def ^:private tool-name-re    #"^gimp_[a-z][a-z0-9_]*$")
(def ^:private param-name-re   #"^[a-z][a-z0-9-]*$")
(def ^:private wire-name-re    #"^[a-z][a-z0-9_]*$")

(def ^:private param-types
  #{:long :double :string :boolean :vector :map :any :nil})

(def ^:private param-spec-keys
  #{:name :wire :schema :nilable? :required? :default :doc})

(defn- matches? [re s]
  (and (string? s) (some? (re-matches re s))))

(defn- optional-boolean? [m k]
  (or (not (contains? m k)) (boolean? (get m k))))

(defn param-spec?
  "True when `x` satisfies `ParamSpec`."
  [x]
  (and (map? x)
       (every? param-spec-keys (keys x))
       (matches? param-name-re (:name x))
       (matches? wire-name-re (:wire x))
       (contains? param-types (:schema x))
       (optional-boolean? x :nilable?)
       (optional-boolean? x :required?)
       (or (not (contains? x :doc)) (string? (:doc x)))))

(defn descriptor?
  "True when `x` satisfies `Descriptor`. Closed, like the schema."
  [x]
  (and (map? x)
       (every? #{:command :tool :doc :params} (keys x))
       (matches? command-name-re (:command x))
       (matches? tool-name-re (:tool x))
       (string? (:doc x))
       (sequential? (:params x))
       (every? param-spec? (:params x))))

(defn raw-response?
  "True when `x` satisfies `RawResponse`. Open, like the schema: only the
   status is insisted on, because an answer without one is a framing bug
   rather than a response."
  [x]
  (and (map? x)
       (contains? #{"success" "error"} (get x "status"))
       (or (not (contains? x "error")) (string? (get x "error")))
       (or (not (contains? x "traceback")) (string? (get x "traceback")))))

;; =============================================================================
;; Portable explanations
;;
;; What `hive-gimp.schema/explain` gives is malli's humanized map, pr-str'd.
;; These answer the FIRST clause that failed, in a sentence, and they run
;; anywhere. For a wire error that is the more useful of the two: the reader
;; is looking at one malformed value and wants to know which field, not a
;; nested map mirroring the schema's shape.
;;
;; Both answer nil when the value conforms, so they compose with the
;; predicates above: (when-not (descriptor? d) (explain-descriptor d)).
;; =============================================================================

(defn- param-spec-problem [p]
  (cond
    (not (map? p))                    "is not a map"
    (seq (remove param-spec-keys (keys p)))
    (str "has key(s) the contract does not define: "
         (clojure.string/join ", " (sort (map str (remove param-spec-keys (keys p))))))
    (not (matches? param-name-re (:name p)))
    (str ":name " (pr-str (:name p)) " is not kebab-case")
    (not (matches? wire-name-re (:wire p)))
    (str ":wire " (pr-str (:wire p)) " is not snake_case")
    (not (contains? param-types (:schema p)))
    (str ":schema " (pr-str (:schema p)) " is not one of "
         (clojure.string/join ", " (sort (map str param-types))))
    (not (optional-boolean? p :nilable?))  ":nilable? is not a boolean"
    (not (optional-boolean? p :required?)) ":required? is not a boolean"
    (and (contains? p :doc) (not (string? (:doc p)))) ":doc is not a string"
    :else nil))

(defn explain-descriptor
  "Why `x` is not a `Descriptor`, as one sentence, or nil when it is one."
  [x]
  (cond
    (not (map? x)) (str "not a map: " (pr-str x))

    (seq (remove #{:command :tool :doc :params} (keys x)))
    (str "has key(s) a descriptor does not define: "
         (clojure.string/join ", " (sort (map str (remove #{:command :tool :doc :params} (keys x))))))

    (not (matches? command-name-re (:command x)))
    (str ":command " (pr-str (:command x)) " is not a snake_case GIMP command name")

    (not (matches? tool-name-re (:tool x)))
    (str ":tool " (pr-str (:tool x)) " is not a gimp_-prefixed tool name")

    (not (string? (:doc x))) ":doc is missing or not a string"

    (not (sequential? (:params x))) ":params is not a sequence"

    :else
    (when-let [[i problem] (first (keep-indexed (fn [i p] (when-let [why (param-spec-problem p)] [i why]))
                                                (:params x)))]
      (str ":params[" i "] " problem))))

(defn explain-raw-response
  "Why `x` is not a `RawResponse`, as one sentence, or nil when it is one."
  [x]
  (cond
    (not (map? x))
    (str "not a JSON object: " (pr-str x))

    (not (contains? x "status"))
    "no \"status\" field, so this is a framing bug rather than a response"

    (not (contains? #{"success" "error"} (get x "status")))
    (str "\"status\" was " (pr-str (get x "status"))
         ", expected \"success\" or \"error\"")

    (and (contains? x "error") (not (string? (get x "error"))))
    (str "\"error\" is not a string: " (pr-str (get x "error")))

    (and (contains? x "traceback") (not (string? (get x "traceback"))))
    (str "\"traceback\" is not a string: " (pr-str (get x "traceback")))

    :else nil))
