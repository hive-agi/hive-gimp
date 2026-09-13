(ns hive-gimp.stub
  "Test doubles for `IGimpTransport`, as records.

   Not `with-redefs`, and not because redefinition is inelegant. A test that
   must redefine somebody else's var is reporting that the subject depends on a
   concretion, and here it would also be redefining the wrong thing: the socket
   adapter is not what the pipeline calls, the PORT is. Reaching past the port
   to patch the adapter would test a path production does not take.

   Three doubles, and the third is the one that earns the design:

     scripted    answers from a queue of frames. The base case.
     recording   a DECORATOR over any other transport that captures what it
                 was asked. This is how request FRAMING is asserted, and no
                 redefinition of the codec can do it.
     failing     raises the exact ex-info shape the socket adapter raises, so
                 the pipeline's error path is exercised without a socket."
  (:require [clojure.data.json :as json]
            [hive-gimp.ports :as ports]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Frames
;; =============================================================================

(defn success-frame
  "A plugin success frame carrying `results`."
  [results]
  (json/write-str {"status" "success" "results" results}))

(defn error-frame
  ([message] (error-frame message nil))
  ([message traceback]
   (json/write-str (cond-> {"status" "error" "error" message}
                     traceback (assoc "traceback" traceback)))))

;; =============================================================================
;; Scripted
;; =============================================================================

(defrecord ScriptedTransport [frames]
  ports/IGimpTransport
  (transport-id [_] :scripted)
  (round-trip! [_ _frame]
    (let [[next & rest] @frames]
      (when (nil? next)
        (throw (ex-info "ScriptedTransport ran out of frames. The subject sent more requests than the script anticipated."
                        {:hive-gimp/reason :test/script-exhausted})))
      (reset! frames (vec rest))
      next)))

(defn scripted
  "A transport answering `frames` in order."
  [& frames]
  (->ScriptedTransport (atom (vec frames))))

(defn always
  "A transport answering `frame` to every request."
  [frame]
  (->ScriptedTransport (atom (repeat 1000 frame))))

;; =============================================================================
;; Recording
;; =============================================================================

(defrecord RecordingTransport [inner sent]
  ports/IGimpTransport
  (transport-id [_] (ports/transport-id inner))
  (round-trip! [_ frame]
    (swap! sent conj frame)
    (ports/round-trip! inner frame)))

(defn recording
  "Wrap `inner`, capturing every frame sent through it."
  [inner]
  (->RecordingTransport inner (atom [])))

(defn sent-frames
  "Raw frames a recording transport was asked to send."
  [t]
  @(:sent t))

(defn sent-requests
  "Frames a recording transport was asked to send, parsed."
  [t]
  (mapv json/read-str @(:sent t)))

;; =============================================================================
;; Failing
;; =============================================================================

(defrecord FailingTransport [reason message]
  ports/IGimpTransport
  (transport-id [_] :failing)
  (round-trip! [_ _frame]
    (throw (ex-info message {:hive-gimp/reason reason}))))

(defn failing
  "A transport that raises the ex-info shape the socket adapter raises."
  ([reason] (failing reason "Simulated transport failure."))
  ([reason message] (->FailingTransport reason message)))

;; =============================================================================
;; Host Python
;; =============================================================================

(defrecord StubHostPython [status modules]
  ports/IHostPython
  (python-available? [_] (= :python/available status))
  (python-status [_]
    ;; A `:hint` accompanies every non-available status, because the real port
    ;; guarantees that and a double that quietly omits it lets an assertion
    ;; about remediation pass against a `nil` the production path never
    ;; produces. A stub is only useful while it is faithful to the contract.
    {:status  status
     :hint    (when-not (= :python/available status)
                (str "Stub remediation for " (name status) "."))
     :modules modules})
  (call-python [_ _module _attr _args _kwargs]
    (if (= :python/available status)
      :stubbed
      (throw (ex-info "Host-side Python is unavailable." {:hive-gimp/reason status})))))

(defn host-python
  ([] (host-python :python/available {"PIL" true "rembg" true}))
  ([status] (host-python status {}))
  ([status modules] (->StubHostPython status modules)))
