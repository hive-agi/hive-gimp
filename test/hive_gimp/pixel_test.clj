(ns hive-gimp.pixel-test
  "The host-side Python port and its composition with GIMP.

   Driven entirely through `IHostPython` doubles, so these run with no Python
   installed at all. That is the point of the port: the capability is optional,
   and the assertions about how it DEGRADES matter more than the ones about how
   it succeeds, because degradation is what most deployments will see."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-gimp.pixel :as pixel]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]
            [hive-gimp.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defrecord ScriptedPython [status modules answers calls]
  ports/IHostPython
  (python-available? [_] (= :python/available status))
  (python-status [_] {:status status :hint "install it" :modules modules})
  (call-python [_ module attr args kwargs]
    (swap! calls conj {:module module :attr attr :args (vec args) :kwargs kwargs})
    (let [answer (get answers [module attr] ::none)]
      (if (= ::none answer)
        (throw (ex-info (str "no scripted answer for " module "/" attr) {}))
        answer))))

(defn scripted-python
  ([answers] (scripted-python answers {"PIL" true "rembg" true}))
  ([answers modules] (->ScriptedPython :python/available modules answers (atom []))))

;; =============================================================================
;; Degradation
;; =============================================================================

(deftest without-python-the-answer-names-the-remediation
  (testing "not a silent no-op, and not a stack trace"
    (let [out (pixel/image-info (stub/host-python :python/no-libpython) "/tmp/x.png")]
      (is (not (response/ok? out)))
      (is (= :python/no-libpython (:reason out)))
      (is (some? (:detail out)) "the hint is what the reader acts on"))))

(deftest a-missing-package-is-distinguished-from-a-missing-runtime
  (testing "rembg absent is a pip command; Python absent is a different afternoon"
    (let [host (stub/host-python :python/available {"PIL" true "rembg" false})
          out  (pixel/remove-background host "/tmp/in.png" "/tmp/out.png")]
      (is (= :python/missing-module (:reason out)))
      (is (re-find #"pip install rembg" (:detail out))))))

(deftest a-module-check-happens-before-the-call
  (testing "checking first turns a ModuleNotFoundError deep in a stack into one actionable line"
    (let [calls (atom [])
          host  (->ScriptedPython :python/available {"rembg" false} {} calls)]
      (pixel/remove-background host "/tmp/in.png" "/tmp/out.png")
      (is (empty? @calls) "no call should be attempted when the package is known absent"))))

(deftest a-python-exception-becomes-an-outcome-not-a-throw
  (let [host (scripted-python {})]                    ; every call throws
    (let [out (pixel/image-info host "/tmp/missing.png")]
      (is (not (response/ok? out)))
      (is (= :python/call-failed (:reason out))))))

;; =============================================================================
;; Success
;; =============================================================================

(deftest image-info-goes-through-the-embedded-module
  (let [calls (atom [])
        host  (->ScriptedPython :python/available {"PIL" true}
                                {["hive_gimp_pixel" "image_info"]
                                 {"width" 640 "height" 480 "mode" "RGBA" "format" "PNG"}}
                                calls)
        out   (pixel/image-info host "/tmp/x.png")]
    (is (response/ok? out))
    (is (= 640 (get (:value out) "width")))
    (is (= "hive_gimp_pixel" (:module (first @calls)))
        "reached through the PORT, so pixel never requires the transport namespace")))

(deftest remove-background-passes-both-paths-to-python
  (let [calls (atom [])
        host  (->ScriptedPython :python/available {"rembg" true}
                                {["hive_gimp_pixel" "remove_background"]
                                 {"input" "/tmp/in.png" "output" "/tmp/out.png" "bytes" 1234}}
                                calls)
        out   (pixel/remove-background host "/tmp/in.png" "/tmp/out.png")]
    (is (response/ok? out))
    (is (= ["/tmp/in.png" "/tmp/out.png"] (:args (first @calls)))
        "the bytes never cross the bridge; only the paths do")))

;; =============================================================================
;; Composition with GIMP
;; =============================================================================

(deftest the-composition-is-export-operate-reopen
  (let [transport (stub/recording
                   (stub/scripted (stub/success-frame {"exported" true})
                                  (stub/success-frame {"opened" true})))
        host      (scripted-python {["hive_gimp_pixel" "remove_background"]
                                    {"bytes" 999}})
        out       (pixel/remove-background-in-gimp transport host)]
    (is (response/ok? out))
    (let [[export open] (stub/sent-requests transport)]
      (is (= "export_image" (get export "type")))
      (is (= "open_image" (get open "type")))
      (is (= (get-in export ["params" "file_path"])
             (get-in out [:value "exported"]))
          "the exported path is reported so a caller can inspect the intermediate"))))

(deftest a-failed-export-stops-before-python
  (let [calls     (atom [])
        transport (stub/always (stub/error-frame "No images are currently open in GIMP"))
        host      (->ScriptedPython :python/available {"rembg" true} {} calls)
        out       (pixel/remove-background-in-gimp transport host)]
    (is (= :gimp/no-image (:reason out)))
    (is (empty? @calls) "no point removing the background of an image that was never exported")))

(deftest a-failed-removal-stops-before-reopening
  (let [transport (stub/recording (stub/always (stub/success-frame {"ok" true})))
        host      (stub/host-python :python/available {"rembg" false})
        out       (pixel/remove-background-in-gimp transport host)]
    (is (= :python/missing-module (:reason out)))
    (is (= 1 (count (stub/sent-frames transport)))
        "the export happened; the re-open must not")))

(deftest available?-reflects-the-port
  (is (true?  (pixel/available? (stub/host-python :python/available))))
  (is (false? (pixel/available? (stub/host-python :python/no-libpython)))))
