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
      (is (= {"image_id" 1 "width" 64 "height" 48 "color_mode" "RGB" "fill" "orange" "resolution" 72.0
              "display_opened" false}
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

(deftest transforms-and-layer-edits
  (let [p (fake/port)
        ok (fn [t params] (let [r (run p t params)]
                            (is (= "success" (get r "status")) (pr-str t params r))
                            (get r "results")))]
    (ok "new_canvas" {"width" 200 "height" 100})
    (ok "create_layer" {"name" "top"})
    (testing "rotation: quarter turns swap the sides, other angles are refused, not approximated"
      (is (= {"width" 100 "height" 200} (ok "rotate_image" {"angle" 90})))
      (is (= {"width" 100 "height" 200} (ok "rotate_image" {"angle" 180})))
      (is (= {"width" 200 "height" 100} (ok "rotate_image" {"angle" -90})))
      (is (= {"width" 200 "height" 100} (ok "rotate_image" {"angle" 360})))
      (is (re-find #"multiples of 90" (get (run p "rotate_image" {"angle" 45}) "error")))
      (is (re-find #"multiples of 90" (get (run p "rotate_image" {"angle" 90.5}) "error"))))
    (is (= {"width" 50 "height" 25} (ok "scale_image" {"width" 50 "height" 25})))
    (is (re-find #"not inside" (get (run p "crop_to_rect" {"x" 40 "y" 0 "width" 20 "height" 10}) "error")))
    (is (= {"width" 10 "height" 5} (ok "crop_to_rect" {"x" 10 "y" 0 "width" 10 "height" 5})))
    (is (re-find #"horizontal or vertical" (get (run p "flip_image" {"direction" "diagonal"}) "error")))
    (is (= {"direction" "vertical"} (ok "flip_image" {"direction" "Vertical"})))
    (testing "layer edits address layers by name or index, and never fall back silently"
      (is (= {"layer_id" 4 "name" "top copy"} (ok "duplicate_layer" {"layer_name" "top"})))
      (is (= ["top copy" "top" "Untitled"]
             (mapv #(get % "name") (get (ok "list_layers" {}) "layers"))))
      (is (= {"old_name" "top copy" "new_name" "halo"} (ok "rename_layer" {"new_name" "halo" "layer_index" 0})))
      (is (re-find #"No layer named" (get (run p "delete_layer" {"layer_name" "ghost"}) "error")))
      (is (re-find #"out of range" (get (run p "delete_layer" {"layer_index" 9}) "error")))
      (is (= {"deleted" "halo" "num_layers" 2} (ok "delete_layer" {"layer_name" "halo"})))
      (let [props (ok "set_layer_properties" {"layer_name" "top" "opacity" 40 "visible" false})]
        (is (= [40.0 false] [(get props "opacity") (get props "visible")])))
      (is (re-find #"only NORMAL" (get (run p "set_layer_properties" {"blend_mode" "MULTIPLY"}) "error")))
      (is (re-find #"0-100" (get (run p "set_layer_properties" {"opacity" 140}) "error"))))
    (is (re-find #"must end in .xcf" (get (run p "save_xcf" {"file_path" "/tmp/a.png"}) "error")))
    (is (= {"file_path" "/tmp/a.xcf" "image_id" 1} (ok "save_xcf" {"file_path" "/tmp/a.xcf"})))
    (is (= {"num_layers" 1} (ok "flatten_image" {})))
    (let [opened (ok "open_image" {"file_path" "/tmp/in.png"})]
      (is (= "/tmp/in.png" (get opened "file_path")))
      (is (= 2 (get (ok "list_images" {}) "count"))))))

;; ---------------------------------------------------------------------------
;; The fake is the real port's shape

(deftest composition-places-by-anchor-and-refuses-rather-than-substitutes
  (let [p  (fake/port)
        ok (fn [t params] (let [r (run p t params)]
                            (is (= "success" (get r "status")) (pr-str t params r))
                            (get r "results")))
        layer (fn [id] (get-in @(:state p) [:layer id]))]
    (testing "resolution is stored on the image and reported"
      (is (= 300.0 (get (ok "new_canvas" {"width" 1080 "height" 1920 "fill" "transparent" "resolution" 300})
                        "resolution")))
      (is (re-find #"positive dpi" (get (run p "new_canvas" {"width" 8 "height" 8 "resolution" -1}) "error"))))
    (testing "place_text: the anchor point lands on (x, y); the font is the one asked for"
      ;; the fake measures size/2 per character: "Any video." at 100 is 500 x 100
      (let [r (ok "place_text" {"text" "Any video." "font" "Lato Black" "size" 100 "color" "orange"
                                "x" 540 "y" 960 "anchor" "center" "justify" "center" "name" "line"})]
        (is (= {"x" 290 "y" 910 "text_width" 500 "text_height" 100 "font" "Lato Black" "color" "#ffa500"
                "layer_name" "line"}
               (select-keys r ["x" "y" "text_width" "text_height" "font" "color" "layer_name"])))
        (is (= [290 910] (:offsets (layer (get r "layer_id")))))
        (is (= 2 (:justify (layer (get r "layer_id")))) "center is GimpTextJustification 2"))
      (is (= [-20 -70] (dispatch/anchor-origin [2 2] 100 30 120 100)) "bottom-right")
      (is (= [100 30] (dispatch/anchor-origin [0 0] 100 30 120 100)) "top-left"))
    (testing "a missing font, anchor or colour is refused and creates no layer"
      (let [before (count (get-in @(:state p) [:image 1 :layers]))]
        (is (re-find #"No font named \"Montserrat\"" (get (run p "place_text" {"text" "x" "font" "Montserrat"}) "error")))
        (is (re-find #"anchor must be one of" (get (run p "place_text" {"text" "x" "anchor" "middle"}) "error")))
        (is (re-find #"^Not a colour" (get (run p "place_text" {"text" "x" "color" "blurple"}) "error")))
        (is (re-find #"non-empty text" (get (run p "place_text" {"text" ""}) "error")))
        (is (= before (count (get-in @(:state p) [:image 1 :layers]))))))
    (testing "add_text keeps the reference result shape, and gains the refusal"
      (is (= [3 4] (get (ok "add_text" {"text" "Hi" "x" 3 "y" 4 "font" "Sans-serif" "size" 24
                                        "color" "black" "image_index" 0})
                        "position")))
      (is (= "error" (get (run p "add_text" {"text" "Hi" "font" "Sans"}) "status"))))
    (testing "gradient_fill: far corner by default, transparent end gains alpha, other shapes refused"
      (ok "create_layer" {"name" "glow" "fill" "white"})
      (swap! (:state p) assoc-in [:layer (some #(when (= "glow" (:name (layer %))) %)
                                               (get-in @(:state p) [:image 1 :layers])) :alpha?] false)
      (let [r (ok "gradient_fill" {"color1" "#b8f34a" "color2" "transparent" "gradient_type" "radial"
                                   "x1" 540 "y1" 600 "layer_name" "glow"})
            g (first (filter #(= "glow" (:name %)) (vals (:layer @(:state p)))))]
        (is (= {"from" [540.0 600.0] "to" [1080.0 1920.0] "color2" "transparent" "gradient_type" "radial"}
               (select-keys r ["from" "to" "color2" "gradient_type"])))
        (is (= {:gradient 2 :from "#b8f34a" :to "transparent" :line [540.0 600.0 1080.0 1920.0]} (:fill g)))
        (is (true? (:alpha? g))))
      (testing "a transparent color1 fades color2 in, and the layer gains alpha the same way"
        (ok "create_layer" {"name" "vignette" "fill" "white"})
        (swap! (:state p) assoc-in [:layer (some #(when (= "vignette" (:name (layer %))) %)
                                                 (get-in @(:state p) [:image 1 :layers])) :alpha?] false)
        (ok "gradient_fill" {"color1" "transparent" "color2" "#000000" "gradient_type" "radial"
                             "layer_name" "vignette"})
        (let [g (first (filter #(= "vignette" (:name %)) (vals (:layer @(:state p)))))]
          (is (= {:from "transparent" :to "#000000"} (select-keys (:fill g) [:from :to])))
          (is (true? (:alpha? g)))))
      (is (re-find #"both transparent"
                   (get (run p "gradient_fill" {"color1" "transparent" "color2" "transparent"}) "error")))
      (is (re-find #"linear or radial" (get (run p "gradient_fill" {"gradient_type" "conical"}) "error"))))
    (testing "place_image: one dimension keeps the aspect ratio, then the anchor places the scaled box"
      ;; the fake loads every file as 64 x 32
      (is (= {"width" 640 "height" 320 "x" 220 "y" 800 "source_width" 64 "source_height" 32}
             (select-keys (ok "place_image" {"file_path" "/in/logo.png" "x" 540 "y" 960 "anchor" "center"
                                             "width" 640})
                          ["width" "height" "x" "y" "source_width" "source_height"])))
      (is (= [60 30] ((juxt #(get % "width") #(get % "height"))
                      (ok "place_image" {"file_path" "/in/logo.png" "height" 30}))))
      (is (re-find #"must be positive" (get (run p "place_image" {"file_path" "/in/a.png" "width" 0}) "error")))
      (is (re-find #"0-100" (get (run p "place_image" {"file_path" "/in/a.png" "opacity" 120}) "error"))))
    (testing "list_layers reports offsets"
      (is (= [220 800] (get (first (filter #(= 640 (get % "width")) (get (ok "list_layers" {}) "layers")))
                            "offsets"))))))

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
