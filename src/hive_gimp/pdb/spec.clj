(ns hive-gimp.pdb.spec
  "DOMAIN. A GIMP PDB procedure as a value, and its projection onto the
   command contract.

   `ProcedureSpec` is what `resources/hive_gimp/pdb/*.edn` holds;
   `ProcedureInfo` is what a live `describe` answers; `RunAnswer` is what the
   run forms print back. All are malli schemas registered in the hive
   registry under `:hive-gimp.pdb/*` at load. Nothing here touches GIMP.

   The argument KIND is an open set: a kind exists because `param-type` has a
   method for it, and `hive-gimp.pdb.forms/set-forms` says how GIMP receives
   it."
  (:require [clojure.string :as str]
            [hive-gimp.schema :as schema]
            [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Kinds (open set)
;; =============================================================================

(defmulti param-type
  "The contract `ParamType` a caller sends for an argument of `kind`."
  identity)

(defmethod param-type :int       [_] :long)
(defmethod param-type :enum      [_] :long)
(defmethod param-type :double    [_] :double)
(defmethod param-type :boolean   [_] :boolean)
(defmethod param-type :string    [_] :string)
(defmethod param-type :image     [_] :long)
(defmethod param-type :drawable  [_] :long)
(defmethod param-type :layer     [_] :long)
(defmethod param-type :item      [_] :long)
(defmethod param-type :drawables [_] :vector)
(defmethod param-type :file      [_] :string)
(defmethod param-type :color     [_] :string)

(defn kind?
  "True when `k` is a registered argument kind."
  [k]
  (and (keyword? k) (contains? (methods param-type) k)))

(defn kinds
  "Every registered kind, sorted."
  []
  (vec (sort (filter keyword? (keys (methods param-type))))))

;; =============================================================================
;; Schemas
;; =============================================================================

(def Kind
  [:and {:gen/elements (kinds)}
   :keyword
   [:fn {:error/message "must be a registered pdb kind"} kind?]])

(def ProcedureName
  [:re {:error/message "must be a PDB procedure name like plug-in-despeckle"
        :gen/elements  ["plug-in-despeckle" "gimp-drawable-invert" "file-png-export" "a" "x-1"]}
   #"^[a-z][a-z0-9-]*$"])

(def ArgName
  "`schema/ParamName`, generatable without test.chuck."
  [:re {:error/message "must be a kebab-case parameter name"
        :gen/elements  ["image" "drawable" "drawables" "run-mode" "file" "lum-threshold" "x1"]}
   #"^[a-z][a-z0-9-]*$"])

(def Command
  "`schema/CommandName`, generatable without test.chuck."
  [:re {:error/message "must be a snake_case GIMP command name"
        :gen/elements  ["pdb_custom" "my_blur" "x"]}
   #"^[a-z][a-z0-9_]*$"])

(def ArgSpec
  "`:fixed` is always sent and never exposed; otherwise `:required?` and
   `:default` are exclusive, as in `ParamSpec`."
  [:map {:closed true}
   [:name      ArgName]
   [:kind      Kind]
   [:fixed     {:optional true} [:or :int :string :boolean]]
   [:required? {:optional true} :boolean]
   [:nilable?  {:optional true} :boolean]
   [:default   {:optional true} [:maybe [:or :int :double :string :boolean]]]
   [:doc       {:optional true} :string]])

(def ValueSpec
  [:map {:closed true} [:name :string] [:type {:optional true} :string]])

(def OmittedArg
  [:map {:closed true} [:name :string] [:type :string]])

(def ProcedureSpec
  [:map {:closed true}
   [:procedure ProcedureName]
   [:command   {:optional true} Command]
   [:fn        {:optional true} :symbol]
   [:doc       :string]
   [:args      [:vector ArgSpec]]
   [:values    {:optional true} [:vector ValueSpec]]
   [:omitted   {:optional true} [:vector OmittedArg]]])

(def ArgInfo
  [:map
   [:name ArgName]
   [:type :string]
   [:fundamental {:optional true} [:maybe :string]]
   [:default {:optional true} :any]
   [:blurb {:optional true} [:maybe :string]]
   [:min {:optional true} [:maybe number?]]
   [:max {:optional true} [:maybe number?]]])

(def ProcedureInfo
  [:map
   [:name   ProcedureName]
   [:blurb  [:maybe :string]]
   [:help   [:maybe :string]]
   [:args   [:vector ArgInfo]]
   [:values [:vector ArgInfo]]])

(def RunAnswer
  [:map
   [:found   :boolean]
   [:missing [:vector :string]]
   [:status  [:maybe :string]]
   [:values  [:vector :any]]
   [:names   [:vector :string]]
   [:error   {:optional true} [:maybe :string]]])

(def registered-schemas
  {:hive-gimp.pdb/kind           Kind
   :hive-gimp.pdb/procedure-name ProcedureName
   :hive-gimp.pdb/arg-spec       ArgSpec
   :hive-gimp.pdb/value-spec     ValueSpec
   :hive-gimp.pdb/omitted-arg    OmittedArg
   :hive-gimp.pdb/procedure-spec ProcedureSpec
   :hive-gimp.pdb/arg-info       ArgInfo
   :hive-gimp.pdb/procedure-info ProcedureInfo
   :hive-gimp.pdb/run-answer     RunAnswer})

(hs/register-all! registered-schemas)

(def procedure-spec? (m/validator ProcedureSpec))
(def procedure-info? (m/validator ProcedureInfo))
(def run-answer?     (m/validator RunAnswer))

(defn explain
  "Why `value` fails `?schema`, or nil."
  [?schema value]
  (schema/explain ?schema value))

;; =============================================================================
;; Names
;; =============================================================================

(defn snake
  "`a-b` -> `a_b`."
  [s]
  (str/replace s "-" "_"))

(defn command-of
  "The catalogue command for `spec`: its `:command`, else `pdb_<procedure>`."
  [spec]
  (or (:command spec) (str "pdb_" (snake (:procedure spec)))))

(defn fn-name
  "The Clojure fn `defprocedure` defines for `spec`."
  [spec]
  (or (:fn spec) (symbol (:procedure spec))))

(defn exposed-args
  "Arguments a caller supplies: every arg that is not `:fixed`."
  [spec]
  (vec (remove #(contains? % :fixed) (:args spec))))

(defn required-args
  "Exposed arguments without a default, in PDB order."
  [spec]
  (filterv :required? (exposed-args spec)))

;; =============================================================================
;; Projection onto the command contract
;; =============================================================================

(defn arg->param
  "One exposed `ArgSpec` as a contract `ParamSpec`."
  [{:keys [name kind required? nilable? doc] :as arg}]
  (cond-> {:name name :wire (snake name) :schema (param-type kind)}
    required?                (assoc :required? true)
    nilable?                 (assoc :nilable? true)
    (contains? arg :default) (assoc :default (:default arg))
    (seq doc)                (assoc :doc doc)))

(defn spec->descriptor
  "The catalogue `Descriptor` the MCP `gimp` tool lists and runs for `spec`."
  [spec]
  (let [command (command-of spec)]
    {:command command
     :tool    (str "gimp_" command)
     :doc     (str (:doc spec) " (GIMP PDB procedure " (:procedure spec) ".)")
     :params  (mapv arg->param (exposed-args spec))}))

(defn assignments
  "`[arg value]` pairs to set on the procedure config, in PDB order, from the
   wire `params` of a built `GimpCommand`. Fixed args always; an absent or nil
   value is left to GIMP's own default."
  [spec params]
  (vec (keep (fn [arg]
               (if (contains? arg :fixed)
                 [arg (:fixed arg)]
                 (let [v (get params (snake (:name arg)))]
                   (when (some? v) [arg v]))))
             (:args spec))))
