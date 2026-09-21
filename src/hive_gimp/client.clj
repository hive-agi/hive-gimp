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
            [hive-help.core :as help]
            [hive-gimp.guard :as guard]
            [clojure.string :as str]))

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

(def ^:private font-catalogue-limit
  "How many font names a lookup asks GIMP for.

   The plug-in defaults its own `limit` to 100 and truncates in silence, and a
   truncated list turns an installed font into a refusal. Asking for a number
   no font collection reaches is what makes the answer usable as a universe."
  10000)

(defn- ask
  "Run a catalogued command for the pipeline's own use. Answers the value, or
   nil for any failure at all.

   Routed through the descriptor rather than hand-built, so a lookup the
   pipeline makes on its own behalf speaks the same contract a caller does.
   Collapsing every failure to nil is deliberate: a compensation must never
   turn its own lookup failure into the caller's failure."
  [transport command-name args]
  (when-let [descriptor (catalog/descriptor command-name)]
    (let [built (command/->command descriptor args)]
      (when-not (r/err? built)
        (let [outcome (send-command transport (:ok built))]
          (when (response/ok? outcome)
            (:value outcome)))))))

(defn- font-names
  "Font names GIMP reports for `args`, or nil when the question could not be
   asked.

   A contract that stopped declaring `limit` would make this nil, which reads
   as `:font/unverifiable` and lets the call through. It can never produce a
   refusal built on a silently truncated list."
  [transport args]
  (get (ask transport "list_fonts" args) "fonts"))

(defn unknown-font-outcome
  "The `Outcome` refusing a font GIMP does not have.

   A refusal rather than a reported substitution, for two reasons. The native
   plug-in's `place_text` already refuses, so both paths now answer the same
   way; and the artefact is the layer, which a caller reads long after the
   response that would have carried the notice. A caller who wants any face at
   all leaves `font` out and takes the plug-in's default.

   Lives at the boundary with `unknown-command-outcome`, for the same reason:
   `hive-gimp.guard` answers the VERDICT and is .cljc, so it cannot require
   hive-help."
  [command wanted installed]
  (let [near (seq (help/suggest wanted installed 3))]
    {:outcome :error
     :command command
     :reason  :gimp/unknown-font
     :message (help/expected-message
               {:param    "font"
                :expected "a font GIMP has installed"
                :actual   wanted
                :hint     (str (when near
                                 (str "Did you mean "
                                      (str/join ", " (map help/backtick near))
                                      "? "))
                               "gimp_list_fonts names every font GIMP knows. A font it does not "
                               "have is refused, never substituted: the reference plug-in answers "
                               "success and quietly draws the layer in Sans-serif.")
                :example  (when near
                            {:command command :font (first near)})})
     :detail  (str "GIMP reports " (count installed) " installed font(s).")}))

(defn- font-refusal
  "nil, or the `Outcome` refusing the font this call named.

   Two independent misses are required before refusing, because a false
   refusal is worse than the substitution it prevents. The narrow lookup
   filters server-side and is the only round trip the accepted path pays; the
   whole catalogue is fetched only after it misses, and is also what supplies
   the suggestions.

   nil from the narrow lookup means the question could not be asked at all,
   and the whole catalogue would answer the same way, so it is not asked. That
   is the native plug-in's case: it publishes no `list_fonts` and refuses an
   absent font itself."
  [transport descriptor args]
  (when-let [wanted (guard/font-asked-for descriptor args)]
    (let [narrow (font-names transport {:filter wanted :limit font-catalogue-limit})]
      (when (and (some? narrow)
                 (not= :font/ok (guard/font-verdict wanted narrow)))
        (let [installed (font-names transport {:limit font-catalogue-limit})]
          (when (= :font/unknown (guard/font-verdict wanted installed))
            (unknown-font-outcome (:command descriptor) wanted installed)))))))

(defmulti send-compensated
  "Send a built `GimpCommand`, compensating for a known defect of the plug-in
   that answers it. Returns an `Outcome`.

   Keyed by command name and open by construction: a defect found in a command
   we do not own is a new `defmethod`, never an edit to `invoke`. The default
   is a plain send, so a command with nothing to compensate pays nothing."
  (fn [_transport gimp-command] (:type gimp-command)))

(defn- native-only-aware
  "Send a native-only command, and name the plug-in when the answer says the
   plug-in is the wrong one.

   `KeyError: 'args'` is what the reference plug-in's exec fallback answers for
   a command it does not dispatch. Left alone it reaches the caller as
   `:gimp/command-failed` with the message `'args'`, which names neither the
   command nor the cause."
  [transport gimp-command]
  (let [outcome (send-command transport gimp-command)]
    (if (guard/answered-by-another-plugin? outcome)
      (assoc outcome
             :reason  :gimp/native-only-command
             :message (str "`" (:command outcome) "` is answered by the hive-gimp native plug-in "
                           "only. The plug-in listening here is the reference Python one: it has "
                           "no handler for this command, falls through to its exec branch and "
                           "answers KeyError: 'args'. Start the native plug-in, which listens on "
                           "9878, or use add_text, which has no anchor: place the text yourself "
                           "from the text_width and text_height it returns."))
      outcome)))

(defmethod send-compensated "place_image"
  [transport gimp-command]
  (native-only-aware transport gimp-command))

(defmethod send-compensated "place_text"
  [transport gimp-command]
  (native-only-aware transport gimp-command))

(defmethod send-compensated "new_canvas"
  [transport gimp-command]
  (let [before  (get (ask transport "list_images" {}) "images")
        outcome (send-command transport gimp-command)]
    (if (response/ok? outcome)
      outcome
      (if-let [value (guard/adopted-canvas (:params gimp-command)
                                           before
                                           (get (ask transport "list_images" {}) "images"))]
        {:outcome :ok
         :command (:type gimp-command)
         :value   value}
        outcome))))

(defmethod send-compensated :default
  [transport gimp-command]
  (send-command transport gimp-command))

(defn invoke
  "Run `command-or-tool` with kebab-case `args`. Returns an `Outcome`.

   Accepts either vocabulary for the command name, for the reason given on
   `catalog/descriptor`.

   Two boundary compensations sit between the built command and the wire, and
   both are here rather than in the pipeline because both need a round trip of
   their own: a font the caller named is checked against the fonts GIMP has
   before anything is drawn, and `new_canvas` recovers the image the reference
   plug-in builds and then disowns. `hive-gimp.guard` explains what each one is
   compensating for."
  ([transport command-or-tool] (invoke transport command-or-tool {}))
  ([transport command-or-tool args]
   (if-let [descriptor (catalog/descriptor command-or-tool)]
     (let [built (command/->command descriptor args)]
       (if (r/err? built)
         (response/from-error (:command descriptor) built)
         (or (font-refusal transport descriptor args)
             (send-compensated transport (:ok built)))))
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
