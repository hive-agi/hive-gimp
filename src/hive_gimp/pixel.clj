(ns hive-gimp.pixel
  "PIPELINE. Host-side pixel work, and its composition with GIMP.

   This is what the optional Python port is FOR. GIMP has no procedure for
   learned background removal, and the reference project's answer is
   bg_remove_iterative.py: sixteen kilobytes that samples corner colours, runs
   `select_by_color` at a threshold, measures what it removed, adjusts the
   threshold, and goes round again. It is a careful piece of work and it is
   fighting the tool. `rembg` is one call and a segmentation model.

   The composition is export, operate, re-open:

     GIMP  ---export_image--->  PNG on disk
     PNG   ---rembg/PIL----->   PNG on disk
     PNG   ---open_image---->   GIMP

   Two round trips through the plugin and one Python call. Note that this reads
   well with `hive-gimp.transport.python`, where the GIMP socket and the pixel
   work happen in the SAME interpreter and the intermediate file never leaves
   Python's own view of the filesystem.

   Every operation degrades honestly: without the port they return an error
   naming `:python/no-libpython` and the remediation, never a silent no-op."
  (:require [clojure.java.io :as io]
            [hive-dsl.result :as r]
            [hive-gimp.client :as client]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- unavailable
  [command {:keys [status hint]}]
  {:outcome :error
   :command command
   :reason  (or status :python/no-libpython)
   :message (str "This operation needs the optional host-side Python port, which is unavailable"
                 (when status (str " (" (name status) ")")) ".")
   :detail  hint})

(defn- missing-module
  [command module]
  {:outcome :error
   :command command
   :reason  :python/missing-module
   :message (str "The Python package `" module "` is not installed in the interpreter hive-gimp is using.")
   :detail  (str "Install it, for example: pip install " module)})

(defn- ok
  [command value]
  {:outcome :ok :command command :value value})

(defn- from-throwable
  [command ^Throwable t]
  {:outcome :error
   :command command
   :reason  (or (:hive-gimp/reason (ex-data t)) :python/call-failed)
   :message (ex-message t)
   :detail  (some-> (ex-data t) (dissoc :hive-gimp/reason) not-empty pr-str)})

(defn- with-python
  "Run `f` when `module` is importable, else answer with the honest failure.

   Checking the module BEFORE calling is what turns `ModuleNotFoundError: rembg`
   deep in a stack trace into one line naming the pip command."
  [host-python command module f]
  (let [{:keys [status modules] :as st} (ports/python-status host-python)]
    (cond
      (not= :python/available status) (unavailable command st)
      (false? (get modules module))   (missing-module command module)
      :else (try (f) (catch Throwable t (from-throwable command t))))))

;; =============================================================================
;; Operations
;; =============================================================================

(defn image-info
  "Width, height, mode and format of an image file, via PIL.

   Useful next to `gimp_get_image_metadata`, which describes what GIMP has
   OPEN. This describes a file on disk, including one GIMP has not opened and
   one this library just wrote."
  [host-python path]
  (with-python
    host-python "image_info" "PIL"
    (fn []
      (ok "image_info"
          (ports/call-python host-python "hive_gimp_pixel" "image_info"
                             [(str path)] {})))))

(defn remove-background
  "Remove the background of `in-path`, writing a transparent PNG to `out-path`.

   One rembg call, made on the Python side: read, remove, write all happen in
   the interpreter, so the image bytes cross the bridge zero times. The
   alternative, marshalling the buffer into the JVM and back, would be slower
   and would turn a segmentation result into a byte array for no reason.

   The model downloads on first use, so the first invocation is slow and the
   rest are not. That is rembg's behaviour and it is surfaced rather than
   hidden behind a timeout that would make it look like a hang."
  [host-python in-path out-path]
  (with-python
    host-python "remove_background" "rembg"
    (fn []
      (ok "remove_background"
          (ports/call-python host-python "hive_gimp_pixel" "remove_background"
                             [(str in-path) (str out-path)] {})))))

;; =============================================================================
;; Composition with GIMP
;; =============================================================================

(def ^:private temp-prefix "hive-gimp-")

(defn- temp-png
  [suffix]
  (let [f (java.io.File/createTempFile temp-prefix (str suffix ".png"))]
    (.deleteOnExit f)
    (.getAbsolutePath f)))

(defn remove-background-in-gimp
  "Export the current GIMP image, remove its background, and open the result.

   The workflow bg_remove_iterative.py exists to approximate, done in one pass
   by a model that was trained for it.

   Returns an `Outcome` whose value names both temp files, so a caller can
   inspect the intermediate when the result is not what they expected. The
   files are marked delete-on-exit rather than removed eagerly, for the same
   reason."
  ([transport host-python] (remove-background-in-gimp transport host-python {}))
  ([transport host-python {:keys [image-index] :or {image-index 0}}]
   (let [exported (temp-png "-export")
         cleaned  (temp-png "-nobg")
         export   (client/invoke transport "export_image"
                                 {:file-path exported :image-index image-index})]
     (if-not (response/ok? export)
       export
       (let [removal (remove-background host-python exported cleaned)]
         (if-not (response/ok? removal)
           removal
           (let [opened (client/invoke transport "open_image" {:file-path cleaned})]
             (if-not (response/ok? opened)
               opened
               (ok "remove_background_in_gimp"
                   {"exported" exported
                    "cleaned"  cleaned
                    "opened"   (:value opened)})))))))))

(defn available?
  "True when the host-side Python port can run these operations."
  [host-python]
  (= :python/available (:status (ports/python-status host-python))))
