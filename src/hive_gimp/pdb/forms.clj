(ns hive-gimp.pdb.forms
  "PURE. The hive-gimp.py forms a PDB call needs: list, describe, run.

   Every program here is a vector of Clojure forms built with `py/forms`;
   Clojure values enter only through `~x`, as Python literals. How an
   argument of a kind reaches the procedure config is `set-forms`, an open
   multimethod: a new kind is a `defmethod`.

   Python names the programs bind all start `hg-pdb-`, so they cannot collide
   with a caller's own state in the shared exec context."
  (:require [hive-gimp.py :as py]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Listing and describing
;; =============================================================================

(defn procedures-forms
  "Forms answering the sorted PDB procedure names matching regex `match`
   (every procedure when blank)."
  [match]
  (py/forms
   (sorted (.query-procedures (Gimp/get-pdb) ~(or match "") "" "" "" "" "" "" ""))))

(def ^:private param-spec-fns
  (py/forms
   (defn hg-pdb-default [p]
     (try (.get-default-value p) (catch Exception e nil)))
   (defn hg-pdb-param [p]
     {"name"        (.-name p)
      "type"        (py.. p -value-type -name)
      "fundamental" (py.. p -value-type -fundamental -name)
      "default"     (hg-pdb-default p)
      "blurb"       (.get-blurb p)
      "min"         (getattr p "minimum" nil)
      "max"         (getattr p "maximum" nil)})))

(defn describe-forms
  "Forms answering `ProcedureInfo` for procedure `procedure`, or nil when GIMP
   has no such procedure."
  [procedure]
  (into param-spec-fns
        (py/forms
         (def hg-pdb-proc (.lookup-procedure (Gimp/get-pdb) ~procedure))
         (if (is hg-pdb-proc nil)
           nil
           {"name"   ~procedure
            "blurb"  (.get-blurb hg-pdb-proc)
            "help"   (.get-help hg-pdb-proc)
            "args"   (for [p (.get-arguments hg-pdb-proc)] (hg-pdb-param p))
            "values" (for [p (.get-return-values hg-pdb-proc)] (hg-pdb-param p))}))))

;; =============================================================================
;; Setting one argument (open set, keyed by kind)
;; =============================================================================

(defmulti set-forms
  "Statements setting `value` for `arg` on `hg-pdb-config`. Object kinds look
   their object up by id and record the arg name in `hg-pdb-missing` when the
   id names no live object, instead of handing GIMP a null."
  (fn [arg _value] (:kind arg)))

(defmethod set-forms :default
  [{:keys [name]} value]
  (py/forms (.set-property hg-pdb-config ~name ~value)))

(defn- object-forms
  "Statements setting an object looked up by `lookup-form` (bound to
   `hg-pdb-object`), or recording `name` as missing."
  [name lookup-forms]
  (into lookup-forms
        (py/forms
         (if (is hg-pdb-object nil)
           (.append hg-pdb-missing ~name)
           (.set-property hg-pdb-config ~name hg-pdb-object)))))

(defmethod set-forms :image
  [{:keys [name]} id]
  (object-forms name (py/forms (def hg-pdb-object (Gimp.Image/get-by-id ~id)))))

(defmethod set-forms :drawable
  [{:keys [name]} id]
  (object-forms name (py/forms (def hg-pdb-object (Gimp.Drawable/get-by-id ~id)))))

(defmethod set-forms :layer
  [{:keys [name]} id]
  (object-forms name (py/forms (def hg-pdb-object (Gimp.Layer/get-by-id ~id)))))

(defmethod set-forms :item
  [{:keys [name]} id]
  (object-forms name (py/forms (def hg-pdb-object (Gimp.Item/get-by-id ~id)))))

(defmethod set-forms :drawables
  [{:keys [name]} ids]
  (py/forms
   (def hg-pdb-objects (for [i ~(vec ids)] (Gimp.Drawable/get-by-id i)))
   (if (or (= 0 (count hg-pdb-objects)) (contains? hg-pdb-objects nil))
     (.append hg-pdb-missing ~name)
     (.set-core-object-array hg-pdb-config ~name hg-pdb-objects))))

(defmethod set-forms :file
  [{:keys [name]} path]
  (py/forms (.set-property hg-pdb-config ~name (Gio.File/new-for-path ~path))))

(defmethod set-forms :color
  [{:keys [name]} color]
  (py/forms (.set-property hg-pdb-config ~name (Gegl.Color/new ~color))))

;; =============================================================================
;; Running
;; =============================================================================

(defn- guarded
  "`stmts` run only when the procedure was found."
  [stmts]
  [(apply list 'when '(not (is hg-pdb-proc nil)) stmts)])

(defn run-forms
  "Forms running `procedure` with `assignments` (`[arg value]` pairs) and
   answering a `RunAnswer`. The procedure runs only when it exists and every
   object argument named a live object."
  [procedure assignments]
  (let [setters (into [] (mapcat (fn [[arg v]] (set-forms arg v))) assignments)
        body    (-> (py/forms (def hg-pdb-config (.create-config hg-pdb-proc)))
                    (into setters)
                    (into (py/forms
                           (when (= 0 (count hg-pdb-missing))
                             (set! hg-pdb-result (.run hg-pdb-proc hg-pdb-config))))))]
    (-> (py/forms
         (import gi.repository [Gio Gegl])
         (def hg-pdb-proc (.lookup-procedure (Gimp/get-pdb) ~procedure))
         (def hg-pdb-missing [])
         (def hg-pdb-result nil))
        (into (guarded body))
        (into (py/forms
               {"found"   (not (is hg-pdb-proc nil))
                "missing" hg-pdb-missing
                "status"  (if (is hg-pdb-result nil) nil (.-value-nick (.index hg-pdb-result 0)))
                "values"  (if (is hg-pdb-result nil)
                            []
                            (for [i (range 1 (.length hg-pdb-result))] (.index hg-pdb-result i)))
                "names"   (if (is hg-pdb-proc nil)
                            []
                            (for [p (.get-return-values hg-pdb-proc)] (.-name p)))
                "error"   (if (or (is hg-pdb-result nil)
                                  (= "success" (.-value-nick (.index hg-pdb-result 0))))
                            nil
                            (.get-last-error (Gimp/get-pdb)))})))))
