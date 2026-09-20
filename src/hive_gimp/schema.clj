(ns hive-gimp.schema
  "Value objects for hive-gimp, malli first — the JVM face of them.

   The DEFINITIONS live in `hive-gimp.shape`, which is portable: a malli
   schema is a vector and a map, so it needs no host. This namespace is the
   part that needs malli, which is compiling a schema into a function
   (`m/validator`) and rendering a failure (`m/explain` + humanize).

   Every schema is re-exported here under its old name, so `schema/Descriptor`
   and `schema/descriptor?` mean exactly what they meant before the split and
   nothing downstream had to move.

   Everything downstream is defined in terms of these, and the suite is
   synthesized from them rather than hand-written: `deftrifecta-from-schema`
   needs an `:in` schema, so a function that takes a VALUE gets generated
   coverage for free while a function that takes an id or a bag of positional
   arguments gets none. That is the reason the promote layer here is written
   over `Descriptor` and `GimpCommand` rather than over a command name.

   Two vocabularies meet in these value objects and must not be confused:

     WIRE   snake_case strings, the GIMP plugin's own contract. `:wire`,
            `GimpCommand`, `RawResponse`. JSON keys stay strings.
     HIVE   kebab-case keywords, what a Clojure caller and an MCP client see.
            `:name`, `Invocation`.

   `hive-gimp.command` is the only place the two are allowed to touch."
  (:require [hive-gimp.shape :as shape]
            [malli.core :as m]
            [malli.error :as me]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; The schemas, re-exported from hive-gimp.shape
;;
;; Deliberate re-exports, not duplicates: the names are this namespace's
;; published surface and predate the portable split. `carto` reports a
;; re-export as orphaned when nothing else points at it; these are pointed at
;; by every caller of hive-gimp.schema.
;; =============================================================================

(def CommandName shape/CommandName)
(def ToolName    shape/ToolName)
(def ParamName   shape/ParamName)
(def WireName    shape/WireName)
(def ParamType   shape/ParamType)
(def ParamSpec   shape/ParamSpec)
(def Descriptor  shape/Descriptor)
(def Catalog     shape/Catalog)
(def Invocation  shape/Invocation)
(def GimpCommand shape/GimpCommand)
(def RawResponse shape/RawResponse)
(def Outcome     shape/Outcome)
(def Endpoint    shape/Endpoint)
(def ExecMode    shape/ExecMode)
(def ExecRequest shape/ExecRequest)

;; =============================================================================
;; Validators (compiled once; m/validate recompiles the schema on every call)
;; =============================================================================

(def descriptor?    (m/validator Descriptor))
(def gimp-command?  (m/validator GimpCommand))
(def raw-response?  (m/validator RawResponse))
(def outcome?       (m/validator Outcome))
(def endpoint?      (m/validator Endpoint))
(def exec-request?  (m/validator ExecRequest))

(defn explain
  "Human-readable explanation of why `value` fails `?schema`, or nil when it
   conforms. Used in error paths, never in hot paths."
  [?schema value]
  (some-> (m/explain ?schema value) me/humanize pr-str))
