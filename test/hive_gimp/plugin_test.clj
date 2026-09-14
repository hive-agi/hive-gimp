(ns hive-gimp.plugin-test
  "The native plug-in's portable core, on the JVM.

   Three claims are held here, and none of them needs GIMP:

   - the hand-written JSON codec agrees with clojure.data.json on every
     generated value, in both directions, so the plug-in and the JVM client
     cannot drift apart on the wire;
   - the dispatch table answers the wire vocabulary hive-gimp's client speaks,
     in the reference plug-in's result shapes, over a fake port;
   - the fake port, the real port main.cljrs builds from gimp.native/*, and
     dispatch/port-keys name the same functions. The fake is only evidence
     about the table if it is the same port the real one is.

   The same core runs on ClojureWasm and clojurust in
   dev/native_portability.cljc, and against a real GIMP in
   dev/verify_native_plugin.sh."
  (:require [clojure.data.json :as data.json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-gimp.plugin.color :as color]
            [hive-gimp.plugin.dispatch :as dispatch]
            [hive-gimp.plugin.fake :as fake]
            [hive-gimp.plugin.json :as json]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; ---------------------------------------------------------------------------
;; JSON: a differential oracle against data.json

(def ^:private bmp-string
  "Strings over the BMP minus surrogates, which the codec refuses by design."
  (gen/fmap (fn [codes] (apply str (map char codes)))
            (gen/vector (gen/one-of [(gen/choose 0 0x7f) (gen/choose 0x80 0xd7ff) (gen/choose 0xe000 0xfffd)]))))

(def ^:private json-value
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/vector inner 0 4)
                  (gen/map bmp-string inner {:max-elements 4})]))
   (gen/one-of [gen/large-integer
                (gen/double* {:infinite? false :NaN? false})
                gen/boolean
                (gen/return nil)
                bmp-string])))

(defspec our-emit-is-read-back-by-data-json 300
  (prop/for-all [v json-value]
    (= v (data.json/read-str (json/emit v)))))

(defspec data-json-output-is-parsed-by-ours 300
  (prop/for-all [v json-value]
    (= {:ok v} (json/parse (data.json/write-str v)))))

(defspec a-proper-prefix-of-an-object-is-never-a-complete-object 100
  (prop/for-all [m (gen/map bmp-string json-value {:min-elements 1 :max-elements 3})]
    (let [text (json/emit m)]
      (and (json/complete-object? text)
           (every? #(not (json/complete-object? (subs text 0 %))) (range (count text)))))))

(deftest parse-reports-malformed-input-instead-of-throwing
  (doseq [bad ["" "{" "[1,]" "{\"a\" 1}" "01" "\"\\ud800\"" "nul" "{} {}" "\"a\u0001\""]]
    (is (string? (:error (json/parse bad))) (pr-str bad))))

;; ---------------------------------------------------------------------------
;; Colour

(deftest colours-reach-gimp-as-hex-or-not-at-all
  (is (= 148 (count color/named)))
  (is (every? #(re-matches #"#[0-9a-f]{6}" %) (vals color/named)))
  (testing "the two spellings GEGL itself gets wrong"
    (is (= "#ffa500" (color/normalize "orange")) "GEGL paints this transparent cyan")
    (is (= "#008000" (color/normalize "rgb(0,128,0)")) "GEGL reads these channels as 0..1"))
  (is (= "#ff8000" (color/normalize "rgb(100%, 50%, 0%)")))
  (is (= "#aabbcc" (color/normalize " #ABC ")))
  (is (nil? (color/normalize "rgb(256,0,0)")))
  (is (nil? (color/normalize "blurple"))))

;; ---------------------------------------------------------------------------
;; Dispatch over the fake port

(defn- run [port t params] (dispatch/handle port {:port 9878} {"type" t "params" params}))

(deftest a-session-answers-in-the-reference-shapes
  (let [p (fake/port)]
    (is (= {"status" "success" "results" {"images" [] "count" 0}} (run p "list_images" {"image_index" 0}))
        "no image open is an empty list, not an error, and an extra param is ignored")
    (let [canvas (run p "new_canvas" {"width" 64 "height" 48 "fill" "orange"})]
      (is (= {"image_id" 1 "width" 64 "height" 48 "color_mode" "RGB" "fill" "orange" "display_opened" false}
             (get canvas "results")))
      (is (= "#ffa500" (get-in @(:state p) [:layer 2 :fill]))))
    (is (= {"layer_id" 3 "name" "top" "width" 64 "height" 48 "opacity" 50.0}
           (get (run p "create_layer" {"name" "top" "opacity" 50 "fill" "transparent"}) "results")))
    (is (= ["top" "Untitled"] (mapv #(get % "name") (get-in (run p "list_layers" {}) ["results" "layers"]))))
    (testing "export goes through a duplicate, so the open image keeps its layers"
      (is (= true (get-in (run p "export_image" {"file_path" "/tmp/a.png"}) ["results" "flattened"])))
      (is (= [1] (:images @(:state p))))
      (is (= 2 (count (get-in @(:state p) [:image 1 :layers])))))
    (is (= "success" (get (run p "close_image" {}) "status")))
    (is (= [] (:images @(:state p))))))

(deftest client-defaults-arrive-as-explicit-nulls-and-are-handled
  (let [p (fake/port)]
    (run p "new_canvas" {"width" 8 "height" 8 "name" "Untitled" "color_mode" "RGB" "fill" "white" "resolution" 72})
    (is (= "success" (get (run p "create_layer" {"name" "L" "width" nil "height" nil "fill" "transparent"
                                                 "opacity" 100 "blend_mode" "NORMAL" "position" -1 "image_index" 0})
                          "status")))
    (is (= "success" (get (run p "fill_layer" {"color" "red" "layer_name" nil "image_index" 0}) "status")))))

(deftest failures-are-answers-not-exceptions
  (let [p (fake/port)]
    (is (re-find #"^Unknown command \"nope\"" (get (run p "nope" {}) "error")))
    (run p "new_canvas" {"width" 8 "height" 8})
    (is (= "image_index 5 is out of range; 1 image(s) open" (get (run p "list_layers" {"image_index" 5}) "error")))
    (is (re-find #"^Not a colour" (get (run p "fill_layer" {"color" "blurple"}) "error")))
    (is (= "export_image requires file_path" (get (run p "export_image" {}) "error")))
    (is (re-find #"^list_images failed: " (get (run (dissoc p :image-ids) "list_images" {}) "error"))
        "an unexpected failure names the command")))

;; ---------------------------------------------------------------------------
;; The fake is the real port's shape

(defn- main-port-keys
  "The keys of the `gimp` map main.cljrs builds from gimp.native/*, read as
   data: main.cljrs is a clojurust file and is not loaded on the JVM."
  []
  (with-open [r (java.io.PushbackReader. (io/reader (io/file "native/src/hive_gimp/plugin/main.cljrs")))]
    (let [forms (take-while some? (repeatedly #(read {:eof nil :read-cond :allow} r)))]
      (some (fn [f] (when (and (seq? f) (= 'def (first f)) (= 'gimp (second f)))
                      (set (keys (last f)))))
            forms))))

(deftest the-fake-real-and-declared-ports-agree
  (is (= dispatch/port-keys (main-port-keys)))
  (is (= dispatch/port-keys (disj (set (keys (fake/port))) :state))))

(deftest every-command-the-table-answers-is-in-the-client-catalog
  (let [catalog (set (map :command (concat (read-string (slurp (io/resource "hive_gimp/commands.edn")))
                                           (read-string (slurp (io/resource "hive_gimp/commands_extra.edn"))))))]
    (is (every? catalog (keys dispatch/commands))
        "a command the plug-in answers but the client refuses to send is unreachable")))
