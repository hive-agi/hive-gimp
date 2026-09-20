(ns hive-gimp.doctor.verdict
  "PROMOTE. The doctor's judgements, as pure functions over values.

   Separated from `hive-gimp.doctor`, which runs the stages and touches the
   transport. The judgements are the part worth pinning: whether GIMP 2.10 is
   acceptable, what an unreadable version means, whether a newer major is a
   refusal or a warning. Behind an I/O call those cases are awkward to reach
   and end up untested; over a value they are three lines each.

   The version gate is the reason this namespace exists. The command contract
   is derived from a GIMP 3.x plugin (`Gimp.get_images`, `Gegl.Color`,
   PyGObject). GIMP 2.10 exposes a different Python API entirely (`pdb.gimp_*`,
   `gimpfu`), so against a 2.10 host the failures surface deep inside GIMP and
   read as nonsense. Saying so before the first command is the whole point."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def supported-major
  "The GIMP major version this command contract describes."
  3)

(def ^:private version-pattern #"(\d+)\.(\d+)(?:\.(\d+))?")

(defn parse-version
  "A version string to `{:major :minor :patch :raw}`, or nil.

   Takes the first thing that looks like a version anywhere in the string. The
   plugin reports its version through several different accessors depending on
   the GIMP release, and none of them is guaranteed to be present, so matching
   a fixed prefix would fail on exactly the hosts that need diagnosing."
  [s]
  (when-let [[raw major minor patch] (re-find version-pattern (str s))]
    {:raw   raw
     :major (parse-long major)
     :minor (parse-long minor)
     :patch (some-> patch parse-long)}))

(defn find-version
  "A version out of whatever shape `get_gimp_info` answered with.

   Walks the payload rather than reading a fixed path, for the reason above:
   the useful field moves between GIMP releases, and a doctor that cannot find
   the version is a doctor that reports `unknown` on a perfectly good install."
  [payload]
  (->> (tree-seq coll? #(if (map? %) (vals %) (seq %)) payload)
       (filter string?)
       (some parse-version)))

(defn version-verdict
  "Whether a detected version is one this contract can drive.

   Four outcomes, not two. `unknown` and `too old` are both `not ok` but call
   for opposite responses: one says proceed and treat failures as suspect, the
   other says stop and install a different GIMP."
  [version]
  (cond
    (nil? version)
    {:ok?     false
     :status  :gimp/version-unknown
     :message "GIMP answered but did not report a version this library recognises. Commands may still work; treat failures as suspect."}

    (< (:major version) supported-major)
    {:ok?     false
     :status  :gimp/version-too-old
     :message (str "GIMP " (:raw version) " is running, but this command contract is derived from the GIMP "
                   supported-major ".x API (Gimp.get_images, Gegl.Color, PyGObject). GIMP 2.10 exposes a "
                   "different Python API (pdb.gimp_*, gimpfu) and these commands will not work against it. "
                   "Install GIMP " supported-major ".x, for example: flatpak install flathub org.gimp.GIMP")}

    (> (:major version) supported-major)
    {:ok?     true
     :status  :gimp/version-newer
     :message (str "GIMP " (:raw version) " is newer than the " supported-major
                   ".x API this contract was derived from. Most commands should work; regenerate the "
                   "contract if new procedures are missing.")}

    :else
    {:ok?     true
     :status  :gimp/version-ok
     :message (str "GIMP " (:raw version) ".")}))

;; =============================================================================
;; Remediation
;; =============================================================================

(defn connection-hint
  "What to do about a failed connection, keyed by the reason the pipeline
   reported.

   A pure table rather than a `case` inside the stage, so adding a reason is a
   data change and so the mapping can be asserted exhaustively."
  [reason]
  (get {:gimp/not-listening
        "Open GIMP, then run Tools > MCP > Start MCP Server. If the menu is absent, the plugin is not installed. The native (clojurust) plug-in listens on 9878, not the default 9877: set HIVE_GIMP_PORT=9878 or connect with {:port 9878}."

        :gimp/connect-timeout
        "The host is reachable but the port is not answering. Check for a firewall or an ssh tunnel that died."

        :gimp/timeout
        "Something is holding the port but not answering. GIMP may be busy in a modal dialog."

        :gimp/unparseable-response
        "Something other than the GIMP MCP plugin is listening on this port."

        :gimp/unknown-host
        "The configured host does not resolve. Check HIVE_GIMP_HOST."}
       reason))

(defn summarize
  "The one line a reader should act on: the first required stage that failed.

   Optional stages are excluded, because a doctor that reports red for an
   optional capability trains its reader to ignore it."
  [stages]
  (let [required (remove :optional? stages)]
    (if (every? :ok? required)
      "hive-gimp is ready."
      (let [{:keys [stage message]} (first (remove :ok? required))]
        (str (name stage) ": " message)))))
