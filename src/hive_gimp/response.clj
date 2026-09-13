(ns hive-gimp.response
  "PROMOTE. A raw plugin answer to an `Outcome`, pure.

   The plugin answers with `{\"status\": \"success\", \"results\": ...}` or
   `{\"status\": \"error\", \"error\": ..., \"traceback\": ...}`. Both reference
   clients read `result[\"status\"]` inline at every call site, which is why the
   same three-line unwrap appears eighty times in their server and why a
   command that forgets it silently returns the envelope instead of the value.

   Interpreting once, here, over a VALUE, is the whole difference."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-gimp.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private truncated-detail 2000)

(defn- clip
  "Long tracebacks are useful to a human and ruinous to a context window."
  [s]
  (when (string? s)
    (if (<= (count s) truncated-detail)
      s
      (str (subs s 0 truncated-detail) "\n... [" (- (count s) truncated-detail) " more characters]"))))

(defn- classify
  "A plugin error message to a reason keyword.

   The plugin reports every failure as a string, so this is pattern matching on
   prose and will never be exhaustive. It is still worth doing: `:gimp/no-image`
   is actionable (open an image) where `:gimp/command-failed` is not, and these
   two account for most of what a caller actually hits. Anything unrecognised
   stays `:gimp/command-failed` rather than being forced into a nearby bucket."
  [message]
  (let [m (str/lower-case (str message))]
    (cond
      (str/includes? m "no images are currently open") :gimp/no-image
      (str/includes? m "no image")                     :gimp/no-image
      (str/includes? m "no such file")                 :gimp/no-such-file
      (str/includes? m "not found")                    :gimp/not-found
      (str/includes? m "index out of range")           :gimp/bad-index
      :else                                            :gimp/command-failed)))

(defn interpret
  "`RawResponse` to `Outcome`.

   `command` is carried through so a caller holding several outcomes can tell
   which one failed without correlating by position."
  [command raw]
  (if-not (schema/raw-response? raw)
    {:outcome :error
     :command command
     :reason  :gimp/malformed-response
     :message "GIMP returned an object without a usable status."
     :detail  (schema/explain schema/RawResponse raw)}
    (if (= "success" (get raw "status"))
      {:outcome :ok
       :command command
       :value   (get raw "results")}
      (let [message (or (get raw "error") "GIMP reported an error with no message.")]
        {:outcome :error
         :command command
         :reason  (classify message)
         :message message
         :detail  (clip (get raw "traceback"))}))))

(defn ok?
  [outcome]
  (= :ok (:outcome outcome)))

(defn value
  "The result of a successful outcome, or nil.

   Returns nil for an error ON PURPOSE and is therefore only safe after `ok?`.
   Callers that cannot branch should use `->result` instead, which makes the
   failure impossible to ignore."
  [outcome]
  (when (ok? outcome) (:value outcome)))

(defn ->result
  "`Outcome` as a hive-dsl Result, for callers composing with `let-ok`."
  [outcome]
  (if (ok? outcome)
    (r/ok (:value outcome))
    (r/err (:reason outcome)
           {:command (:command outcome)
            :message (:message outcome)
            :detail  (:detail outcome)})))

(defn from-error
  "A transport or promote failure rendered as an `Outcome`.

   Everything a caller sees is an `Outcome`, whether it failed inside GIMP,
   on the socket, or before a byte was sent. One shape for all three is what
   lets the tool layer have a single error path instead of three."
  [command result]
  {:outcome :error
   :command (or command "unknown")
   :reason  (:error result)
   :message (or (:message result) "The GIMP request failed.")
   :detail  (clip (:detail result))})
