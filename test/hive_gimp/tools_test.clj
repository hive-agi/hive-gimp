(ns hive-gimp.tools-test
  "The MCP surface: four tools carrying eighty commands, and the promotions
   that make the results usable to a model."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.stub :as stub]
            [hive-gimp.tools :as tools]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- surface [transport]
  (tools/tools transport (stub/host-python)))

(defn- tool-named [transport nm]
  (first (filter #(= nm (:name %)) (surface transport))))

(defn- call [transport nm params]
  ((:handler (tool-named transport nm)) params))

(defn- text-of [result]
  (-> result :content first :text))

;; =============================================================================
;; The surface itself
;; =============================================================================

(deftest five-tools-carry-every-command
  (let [t (stub/always (stub/success-frame "ok"))]
    (is (= #{"gimp" "gimp_exec" "gimp_catalog" "gimp_pixel" "gimp_doctor"}
           (set (map :name (surface t))))
        "one tool definition per GIMP command would be a permanent context tax on every client")))

(deftest pixel-is-its-own-tool-not-a-gimp-command
  (testing "folding it into the gimp enum would advertise it as available whenever GIMP is"
    (let [t (stub/always (stub/success-frame "ok"))]
      (is (nil? (catalog/descriptor "remove_background"))
          "it is not a GIMP command and must not appear in the contract")
      (is (= ["image_info" "remove_background" "remove_background_in_gimp"]
             (get-in (tool-named t "gimp_pixel") [:inputSchema :properties "command" :enum]))))))

(deftest pixel-reports-the-missing-port-rather-than-failing-opaquely
  (let [t (stub/always (stub/success-frame "ok"))
        result ((:handler (tool-named t "gimp_pixel")) {"command" "image_info" "path" "/tmp/x.png"})]
    ;; `surface` wires a StubHostPython that reports :python/available with PIL
    ;; present, so this exercises the call path rather than the refusal; the
    ;; refusal itself is pinned in hive-gimp.pixel-test.
    (is (map? result))
    (is (contains? result :content))))

(deftest an-unknown-pixel-command-lists-the-real-ones
  (let [t (stub/always (stub/success-frame "ok"))
        result ((:handler (tool-named t "gimp_pixel")) {"command" "nope"})]
    (is (true? (:isError result)))
    (is (re-find #"remove_background_in_gimp" (text-of result)))))

(deftest every-tool-is-well-formed
  (let [t (stub/always (stub/success-frame "ok"))]
    (doseq [tool (surface t)]
      (is (string? (:name tool)))
      (is (seq (:description tool)))
      (is (= "object" (get-in tool [:inputSchema :type])))
      (is (ifn? (:handler tool))))))

;; =============================================================================
;; gimp
;; =============================================================================

(deftest both-key-spellings-reach-the-same-parameter
  (testing "a client that read the wire contract and one that read the catalog must both work"
    (let [t (stub/recording (stub/scripted (stub/success-frame "a") (stub/success-frame "b")))]
      (call t "gimp" {"command" "auto_levels" "params" {"image_index" 1}})
      (call t "gimp" {"command" "auto_levels" "params" {"image-index" 1}})
      (is (= [1 1] (mapv #(get-in % ["params" "image_index"]) (stub/sent-requests t)))))))

(deftest a-missing-command-points-at-the-discovery-path
  (let [t (stub/always (stub/success-frame "ok"))
        result (call t "gimp" {"params" {}})]
    (is (true? (:isError result)))
    (is (re-find #"gimp_catalog" (text-of result)))))

(deftest an-error-carries-the-reason-keyword
  (testing "the reason is the only stable handle an agent has for deciding what to do next"
    (let [t (stub/always (stub/error-frame "No images are currently open in GIMP"))
          result (call t "gimp" {"command" "auto_levels"})]
      (is (true? (:isError result)))
      (is (re-find #"gimp/no-image" (text-of result))))))

;; =============================================================================
;; Image promotion
;; =============================================================================

(deftest a-bitmap-result-becomes-mcp-image-content
  (testing "base64 in a text field is both unusable to the model and enormous"
    (let [t (stub/always (stub/success-frame {"image_data" "aGVsbG8=" "format" "png" "width" 4}))
          result (call t "gimp" {"command" "get_image_bitmap"})
          part   (-> result :content first)]
      (is (= "image" (:type part)))
      (is (= "aGVsbG8=" (:data part)))
      (is (= "image/png" (:mimeType part))))))

(deftest an-ordinary-result-stays-text
  (let [t (stub/always (stub/success-frame {"width" 640 "height" 480}))
        result (call t "gimp" {"command" "get_image_metadata"})]
    (is (= "text" (-> result :content first :type)))
    (is (= {"width" 640 "height" 480} (json/read-str (text-of result))))))

;; =============================================================================
;; gimp_catalog
;; =============================================================================

(deftest catalog-lists-searches-and-describes
  (let [t (stub/recording (stub/always (stub/success-frame "unreachable")))]
    (testing "no arguments lists every command"
      (let [body (json/read-str (text-of (call t "gimp_catalog" {})))]
        (is (= (count (catalog/commands)) (get body "count")))))

    (testing "query searches"
      (let [body (json/read-str (text-of (call t "gimp_catalog" {"query" "layer"})))]
        (is (pos? (get body "count")))))

    (testing "command describes, with types and defaults"
      (let [body (json/read-str (text-of (call t "gimp_catalog" {"command" "new_canvas"})))
            width (first (filter #(= "width" (get % "name")) (get body "params")))]
        (is (= "new_canvas" (get body "command")))
        (is (= "long" (get width "type")))
        (is (true? (get width "required")))))

    (testing "an unknown command is an error, not an empty list"
      (is (true? (:isError (call t "gimp_catalog" {"command" "nope_not_real"})))))

    (is (empty? (stub/sent-frames t))
        "discovery must never touch GIMP")))

;; =============================================================================
;; gimp_exec
;; =============================================================================

(deftest exec-accepts-a-bare-string-and-an-array
  (let [t (stub/recording (stub/scripted (stub/success-frame ["1"]) (stub/success-frame ["2"])))]
    (call t "gimp_exec" {"code" "print(1)"})
    (call t "gimp_exec" {"code" ["print(1)" "print(2)"]})
    (is (= [1 2] (mapv #(count (second (get-in % ["params" "args"]))) (stub/sent-requests t))))))

(deftest exec-without-code-is-refused-before-the-socket
  (let [t (stub/recording (stub/always (stub/success-frame "unreachable")))]
    (is (true? (:isError (call t "gimp_exec" {}))))
    (is (empty? (stub/sent-frames t)))))

;; =============================================================================
;; gimp_doctor
;; =============================================================================

(deftest doctor-reports-without-throwing-when-gimp-is-absent
  (let [t (stub/failing :gimp/not-listening "Nothing is listening.")
        body (json/read-str (text-of (call t "gimp_doctor" {})))]
    (is (false? (get body "ok?")))
    (is (seq (get body "stages")))))
