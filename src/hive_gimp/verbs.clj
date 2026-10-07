(ns hive-gimp.verbs
  "GIMP's base functionality as Clojure. The builders in
   `hive-gimp.verbs.{image,layer,selection,paint,path,io,display}` are PURE:
   each answers hive-gimp.py forms. This namespace composes them and is the
   one BOUNDARY, sending a composed program to GIMP in ONE round trip through
   `py/eval-forms`:

       (require '[hive-gimp.verbs :as verbs]
                '[hive-gimp.verbs.image :as image]
                '[hive-gimp.verbs.layer :as layer])

       (verbs/run g (image/get-by-name \"logo.xcf\")) ; Outcome, :value a Ref

       (verbs/run g
         (layer/new-layer img {:name \"corpo v3\" :as 'body})
         (paint/edit-fill-colour 'body \"#2a4b8d\")
         (layer/set-opacity 'body 80))               ; one round trip

       (verbs/call g :layer/set-name ref \"FRANCANA\")

   `call` goes through the catalogue: it checks the arguments against the
   verb's `:verb/args`, runs it, and checks the answer against
   `:verb/answers`, labelling the `Outcome` `verb_<family>_<verb>`."
  (:refer-clojure :exclude [run!])
  (:require [hive-gimp.py :as py]
            [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.display]
            [hive-gimp.verbs.image]
            [hive-gimp.verbs.io]
            [hive-gimp.verbs.layer]
            [hive-gimp.verbs.paint]
            [hive-gimp.verbs.path]
            [hive-gimp.verbs.selection]
            [hive-gimp.verbs.value]
            [malli.core :as m]
            [malli.error :as me]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def VerbCall
  "One verb applied to its arguments, as a value."
  [:map {:closed true} [:verb :qualified-keyword] [:args [:vector :any]]])

(defn- refuse [reason message data]
  (throw (ex-info message (assoc data :hive-gimp/reason reason))))

(defn- explain [schema value]
  (some-> (m/explain schema value) me/humanize pr-str))

(defn program-of
  "The forms `verb-call` builds. Refuses (`ex-info`, `:hive-gimp/reason`)
   an unknown verb, or arguments its `:verb/args` rejects."
  [{:keys [verb args]}]
  (let [{f :var schema :args} (or (catalog/lookup verb)
                                  (refuse :verbs/unknown-verb
                                          (str verb " is not a verb. Known: " (keys (catalog/catalogue)))
                                          {:verb verb}))]
    (when-let [why (explain schema (vec args))]
      (refuse :verbs/bad-arguments (str verb " rejected its arguments: " why) {:verb verb :args args}))
    (apply f args)))

(defn program
  "The forms of `verb` applied to `args`."
  [verb & args]
  (program-of {:verb verb :args (vec args)}))

(defn compose
  "One program running `programs` in order; it answers the last one's value."
  [& programs]
  (into [] cat programs))

(defn run
  "Send the composition of `programs` to GIMP in one round trip. An `Outcome`
   whose `:value` is the last program's answer as Clojure data."
  [target & programs]
  (py/eval-forms target (apply compose programs)))

(defn run!
  "As `run`, answering the value and throwing on failure."
  [target & programs]
  (let [outcome (apply run target programs)]
    (if (= :ok (:outcome outcome))
      (:value outcome)
      (throw (ex-info (str "GIMP refused the program: " (:message outcome)) outcome)))))

(defn- error-outcome [command reason message]
  {:outcome :error :command command :reason reason :message message})

(defn- checked
  "`outcome` labelled `command`, its value held to `answers`."
  [outcome command answers]
  (let [labelled (-> outcome (assoc :command command) (dissoc :stdout :python))]
    (if-let [why (and (= :ok (:outcome outcome)) (explain answers (:value outcome)))]
      (error-outcome command :verbs/unexpected-answer (str "GIMP answered a value the verb does not promise: " why))
      labelled)))

(defn call
  "Run catalogued `verb` on `args` in GIMP. An `Outcome` labelled
   `verb_<family>_<verb>`; a refused verb or argument, or an answer outside
   `:verb/answers`, is an `:error` Outcome, never a throw."
  [target verb & args]
  (let [command (catalog/command-of verb)]
    (try
      (checked (run target (program-of {:verb verb :args (vec args)}))
               command
               (:answers (catalog/lookup verb)))
      (catch clojure.lang.ExceptionInfo e
        (if-let [reason (:hive-gimp/reason (ex-data e))]
          (error-outcome command reason (ex-message e))
          (throw e))))))
