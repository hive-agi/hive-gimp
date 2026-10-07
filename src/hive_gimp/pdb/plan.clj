(ns hive-gimp.pdb.plan
  "PROMOTE. A `ProcedureSpec` plus the wire params of a built `GimpCommand`
   to the forms that run it, and a `RunAnswer` back to an `Outcome`.

   Pure on both sides. The request side refuses, in Clojure, any value GIMP
   cannot receive (`value-problem`, open by kind), so nothing that would hand
   a procedure a null object ever reaches GIMP."
  (:require [clojure.string :as str]
            [hive-gimp.pdb.forms :as forms]
            [hive-gimp.pdb.spec :as spec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Request side
;; =============================================================================

(defmulti value-problem
  "nil when `value` is one GIMP can receive for `arg`, else a sentence saying
   what was expected."
  (fn [arg _value] (:kind arg)))

(defmethod value-problem :default [_ _] nil)

(defn- id-problem [value]
  (when-not (and (integer? value) (pos? value))
    "a positive integer GIMP object id"))

(defmethod value-problem :image    [_ v] (id-problem v))
(defmethod value-problem :drawable [_ v] (id-problem v))
(defmethod value-problem :layer    [_ v] (id-problem v))
(defmethod value-problem :item     [_ v] (id-problem v))

(defmethod value-problem :drawables [_ v]
  (when-not (and (sequential? v) (seq v) (every? #(and (integer? %) (pos? %)) v))
    "a non-empty list of positive integer drawable ids"))

(defmethod value-problem :file [_ v]
  (when-not (and (string? v) (str/starts-with? v "/"))
    "an absolute file path"))

(defn- refusal
  [command reason message]
  {:outcome :error :command command :reason reason :message message})

(defn plan
  "`{:forms [...]}` running `spec` with wire `params`, or an error `Outcome`
   naming every argument whose value GIMP could not receive."
  [spec params]
  (let [command  (spec/command-of spec)
        assigned (spec/assignments spec params)
        problems (keep (fn [[arg v]]
                         (when-let [p (value-problem arg v)]
                           (str (:name arg) " expects " p ", got " (pr-str v))))
                       assigned)]
    (if (seq problems)
      (refusal command :gimp/invalid-parameter (str/join "; " problems))
      {:forms (forms/run-forms (:procedure spec) assigned)})))

;; =============================================================================
;; Answer side
;; =============================================================================

(defn- named-values [{:keys [names values]}]
  (into (sorted-map) (map (fn [n v] [(keyword n) v]) names values)))

(defn interpret
  "A `RunAnswer` for `procedure` as an `Outcome` labelled `command`."
  [command procedure answer]
  (cond
    (not (spec/run-answer? answer))
    {:outcome :error :command command :reason :gimp/unreadable-answer
     :message (str "GIMP ran " procedure " but its answer is not a RunAnswer.")
     :detail  (pr-str answer)}

    (not (:found answer))
    (refusal command :gimp/unknown-procedure
             (str "GIMP has no PDB procedure named " procedure "."))

    (seq (:missing answer))
    (refusal command :gimp/invalid-parameter
             (str "No live GIMP object for: " (str/join ", " (:missing answer))
                  ". The ids must name an open image, layer or drawable."))

    (= "success" (:status answer))
    {:outcome :ok :command command
     :value   {:procedure procedure :values (named-values answer)}}

    :else
    (refusal command :gimp/procedure-failed
             (str procedure " answered " (:status answer)
                  (when-let [e (not-empty (:error answer))] (str ": " e))))))
