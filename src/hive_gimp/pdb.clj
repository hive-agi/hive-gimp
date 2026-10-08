(ns hive-gimp.pdb
  "BOUNDARY. Introspect and run any GIMP PDB procedure, through hive-gimp.py
   forms only.

     (pdb/procedures g :match \"plug-in-.*\")   ; Outcome, :value the names
     (pdb/describe g \"plug-in-despeckle\")     ; Outcome, :value ProcedureInfo
     (pdb/run! g \"gimp-drawable-invert\" {:drawable 12})

   `target` is a session `{:transport t}` or a transport. Every function
   answers an `Outcome`. The forms come from `hive-gimp.pdb.forms`, the
   decisions from `hive-gimp.pdb.plan` and `hive-gimp.pdb.derive`; this
   namespace only sends and labels.

   `run!` takes kebab-case keyword args, describes the procedure, and runs it
   through the same coercion a catalogued command gets
   (`hive-gimp.command/->command` over the derived descriptor)."
  (:refer-clojure :exclude [run!])
  (:require [hive-dsl.result :as r]
            [hive-gimp.command :as command]
            [hive-gimp.pdb.derive :as derive]
            [hive-gimp.pdb.forms :as forms]
            [hive-gimp.pdb.plan :as plan]
            [hive-gimp.pdb.spec :as spec]
            [hive-gimp.py :as py]
            [hive-gimp.response :as response]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- labelled
  [outcome command]
  (-> outcome (assoc :command command) (dissoc :stdout :python)))

(defn procedures
  "PDB procedure names, sorted; `:match` is a regex over the name."
  [target & {:keys [match]}]
  (labelled (py/eval-forms target (forms/procedures-forms match)) "pdb_procedures"))

(defn describe
  "The `ProcedureInfo` of procedure `name`. An unknown name is
   `:gimp/unknown-procedure`."
  [target name]
  (let [outcome (labelled (py/eval-forms target (forms/describe-forms name)) "pdb_describe")]
    (cond
      (not (response/ok? outcome)) outcome
      (nil? (:value outcome))      {:outcome :error :command "pdb_describe"
                                    :reason  :gimp/unknown-procedure
                                    :message (str "GIMP has no PDB procedure named " name ".")}
      :else                        outcome)))

(defn run-spec
  "Run `spec` with the wire `params` of a built `GimpCommand`."
  [target spec params]
  (let [command (spec/command-of spec)
        planned (plan/plan spec params)]
    (if (:outcome planned)
      planned
      (let [outcome (py/eval-forms target (:forms planned))]
        (if (response/ok? outcome)
          (plan/interpret command (:procedure spec) (:value outcome))
          (labelled outcome command))))))

(defn run!
  "Run procedure `name` with kebab-case keyword `args`, coerced against the
   procedure's live description. Run mode is always non-interactive."
  [target name args]
  (let [described (describe target name)]
    (if-not (response/ok? described)
      described
      (let [spec  (derive/info->spec (:value described))
            built (command/->command (spec/spec->descriptor spec) args)]
        (if (r/err? built)
          (response/from-error (spec/command-of spec) built)
          (run-spec target spec (:params (:ok built))))))))
