(ns hive-gimp.catalog
  "COLLECT. Reads the command contract off the classpath, and nothing else.

   Two resources, merged, and the split between them is deliberate:

     commands.edn        DERIVED from the reference project by
                         dev/extract_gimp_contract.py. Regenerated wholesale,
                         so nothing hand-written may live in it.
     commands_extra.edn  HAND-MAINTAINED additions, which a regeneration must
                         not erase.

   Every function here either returns what the resources say or delegates to
   `hive-gimp.contract`, which owns every decision ABOUT the contract. The
   split matters because this namespace is the only one that cannot be driven
   from a generator: its input is a file on the classpath."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [hive-gimp.contract :as contract]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def derived-resource "hive_gimp/commands.edn")
(def extra-resource   "hive_gimp/commands_extra.edn")

(defn read-resource
  "Descriptors from a classpath EDN resource, or [] when it is absent.

   Absent is not an error: the extra resource is optional, and a consumer
   repackaging this jar may drop it."
  [path]
  (if-let [url (io/resource path)]
    (vec (edn/read-string (slurp url)))
    []))

(defn load-descriptors
  "Every descriptor from both resources, derived first so a hand-written row is
   the later one and can win a tie it is entitled to win."
  []
  (into (read-resource derived-resource)
        (read-resource extra-resource)))

(defn- build
  []
  (let [descriptors (load-descriptors)]
    {:descriptors descriptors
     :commands    (contract/index descriptors)
     :aliases     (contract/aliases descriptors)}))

(defonce ^{:doc "The contract, read once.

                 Resources do not change under a running JVM, and re-reading
                 per call would put a file read in the path of every command."}
  catalog
  (delay (build)))

;; =============================================================================
;; Accessors
;; =============================================================================
;;
;; Thin by design. Each is the loaded contract handed to the corresponding pure
;; function, so the behaviour under test is the pure one.

(defn commands
  "Command name to `Descriptor`."
  []
  (:commands @catalog))

(defn descriptor
  "The `Descriptor` for a command name or a published tool name, or nil."
  [name-or-tool]
  (let [{:keys [commands aliases]} @catalog]
    (contract/lookup commands aliases name-or-tool)))

(defn command-names
  "Every command, sorted."
  []
  (contract/command-names (commands)))

(defn search
  "Commands whose name or description contains `q`."
  [q]
  (contract/search (commands) q))

(defn describe
  "The published projection of one descriptor."
  [descriptor]
  (contract/describe descriptor))

(defn invalid
  "Descriptors in the SHIPPED resources that do not conform.

   Kept as a function rather than a test-only assertion so the doctor can
   report a bad contract in the field, where no test runner is present."
  []
  (contract/invalid (load-descriptors)))
