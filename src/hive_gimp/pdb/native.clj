(ns hive-gimp.pdb.native
  "BOUNDARY (wiring). `defprocedure` makes one PDB procedure native to
   hive-gimp; `defprocedures` does it for every row of a spec EDN resource.

   For a `ProcedureSpec` the expansion installs three things, all keyed by
   the spec's catalogue command so a re-evaluation replaces rather than
   duplicates:

     1. a catalogue row (`hive-gimp.catalog/register!`), so the MCP `gimp`
        tool and `gimp_catalog` list and run it;
     2. a `client/send-compensated` method answering that command over exec
        (`hive-gimp.pdb/run-spec`);
     3. a Clojure fn whose arglists are the procedure's required arguments,
        with an optional trailing map for the rest, going through
        `client/invoke` exactly as an MCP call does.

   Adding a procedure is a row in `resources/hive_gimp/pdb/*.edn`."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.pdb :as pdb]
            [hive-gimp.pdb.derive :as derive]
            [hive-gimp.pdb.spec :as spec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn invoke
  "Run catalogued `command` with kebab-case `args` on `target` (a session or
   a transport). Returns an `Outcome`."
  [target command args]
  (client/invoke (or (:transport target) target) command args))

(defn docstring
  "The docstring a native fn carries for `spec`."
  [spec]
  (str (:doc spec) "\n\n"
       "GIMP PDB procedure " (:procedure spec) ", catalogued as `"
       (spec/command-of spec) "`. Returns an Outcome.\n\n"
       (str/join "\n" (map (fn [{:keys [name kind doc] :as arg}]
                             (str "  " name " (" (clojure.core/name kind)
                                  (cond (:required? arg) ", required"
                                        (contains? arg :default) (str ", default " (pr-str (:default arg))))
                                  ")" (when doc (str ": " doc))))
                           (spec/exposed-args spec)))))

(defn expansion
  "The forms `defprocedure` expands to for `fn-sym` and a validated `spec`.
   Arglists read `[target <required args>]` and `[target <required args> opts]`."
  [fn-sym spec]
  (let [command  (spec/command-of spec)
        required (spec/required-args spec)
        syms     (mapv (comp symbol :name) required)
        kws      (mapv (comp keyword :name) required)]
    `(do
       (catalog/register! '~(spec/spec->descriptor spec))
       (defmethod client/send-compensated ~command
         [transport# gimp-command#]
         (pdb/run-spec transport# '~spec (:params gimp-command#)))
       (defn ~fn-sym
         ~(docstring spec)
         ([~'target ~@syms] (~fn-sym ~'target ~@syms {}))
         ([~'target ~@syms ~'opts]
          (invoke ~'target ~command (merge ~'opts ~(zipmap kws syms))))))))

(defmacro defprocedure
  "Make the PDB procedure described by `spec-form` (a `ProcedureSpec`,
   evaluated at expansion) native, as the fn `fn-sym`."
  [fn-sym spec-form]
  (expansion fn-sym (derive/valid-spec (eval spec-form))))

(defn read-specs
  "The `ProcedureSpec`s in classpath resource `path`, validated."
  [path]
  (if-let [url (io/resource path)]
    (mapv derive/valid-spec (edn/read-string (slurp url)))
    (throw (ex-info (str "No PDB spec resource at " path)
                    {:hive-gimp/reason :pdb/missing-spec-resource :path path}))))

(defmacro defprocedures
  "`defprocedure` for every row of spec EDN resource `path`. Evaluates to the
   catalogue commands it registered."
  [path]
  (let [specs (read-specs path)]
    `(do ~@(map (fn [s] `(defprocedure ~(spec/fn-name s) '~s)) specs)
         ~(mapv spec/command-of specs))))
