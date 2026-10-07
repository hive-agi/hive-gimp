(ns hive-gimp.pdb.derive
  "PROMOTE. A live `ProcedureInfo` to the `ProcedureSpec` row a spec EDN
   holds. What `dev/pdb_snapshot.clj` writes, and what `hive-gimp.pdb/run!`
   uses for a procedure no spec describes.

   GType to kind is DATA: `type-kinds` by exact GType name, then
   `fundamental-kinds` by fundamental type. A GType neither maps is reported
   under `:omitted` and left to GIMP's default. `fixed-args` names arguments
   that are always sent and never exposed."
  (:require [hive-gimp.pdb.spec :as spec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def type-kinds
  {"GimpImage"           :image
   "GimpDrawable"        :drawable
   "GimpLayer"           :layer
   "GimpItem"            :item
   "GimpCoreObjectArray" :drawables
   "GFile"               :file
   "GeglColor"           :color})

(def fundamental-kinds
  {"gint"       :int
   "guint"      :int
   "gint64"     :int
   "guint64"    :int
   "guchar"     :int
   "gdouble"    :double
   "gfloat"     :double
   "gboolean"   :boolean
   "gchararray" :string
   "GEnum"      :enum})

(def fixed-args
  "Run mode NONINTERACTIVE: an interactive run would open a dialog and block
   the plug-in's socket."
  {"run-mode" 1})

(def nil-default-kinds
  "Kinds whose nil default is a usable 'unset' rather than a missing input."
  #{:string})

(defn kind-of
  "The kind for one `ArgInfo`, or nil."
  [{:keys [type fundamental]}]
  (or (get type-kinds type) (get fundamental-kinds fundamental)))

(defn- range-doc [{:keys [min max]}]
  (when (and (number? min) (number? max))
    (str " (" min ".." max ")")))

(defn- scalar-default [default]
  (when (or (int? default) (double? default) (string? default) (boolean? default))
    default))

(defn info->arg
  "One `ArgInfo` as an `ArgSpec`, or nil when its type maps to no kind."
  [info]
  (when-let [kind (kind-of info)]
    (let [doc     (str (or (:blurb info) (:name info)) (range-doc info))
          default (scalar-default (:default info))
          base    {:name (:name info) :kind kind :doc doc}]
      (cond
        (contains? fixed-args (:name info)) (assoc base :fixed (get fixed-args (:name info)))
        (some? default)                     (assoc base :default default)
        (nil-default-kinds kind)            (assoc base :nilable? true :default nil)
        :else                               (assoc base :required? true)))))

(defn info->spec
  "A `ProcedureInfo` as a `ProcedureSpec`."
  [info]
  (let [args (:args info)]
    (cond-> {:procedure (:name info)
             :doc       (or (not-empty (:blurb info)) (:name info))
             :args      (into [] (keep info->arg) args)}
      (seq (:values info))
      (assoc :values (mapv #(select-keys % [:name :type]) (:values info)))

      (seq (remove kind-of args))
      (assoc :omitted (mapv #(select-keys % [:name :type]) (remove kind-of args))))))

(defn valid-spec
  "`spec` when it conforms to `ProcedureSpec`, else throws naming why."
  [spec]
  (if (spec/procedure-spec? spec)
    spec
    (throw (ex-info (str "Not a ProcedureSpec: " (spec/explain spec/ProcedureSpec spec))
                    {:hive-gimp/reason :pdb/invalid-spec :spec spec}))))
