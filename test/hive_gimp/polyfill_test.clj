(ns hive-gimp.polyfill-test
  "The programs `hive-gimp.polyfill` writes, asserted as data, with no GIMP.

   What a test here protects is the ORDER and the ARITHMETIC: a placed layer is
   scaled, then mirrored, then offset from its final size; a missing parameter
   leaves its statement out instead of sending a None; a string reaches Python
   as a literal no quote or backslash can break out of."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.polyfill :as polyfill]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- index-of
  [program pred]
  (first (keep-indexed (fn [i s] (when (pred s) i)) program)))

(defn- has?
  [program fragment]
  (boolean (some #(str/includes? % fragment) program)))

(deftest py-str-is-a-closed-literal
  (is (= "'roses.png'" (polyfill/py-str "roses.png")))
  (is (= "'it\\'s'" (polyfill/py-str "it's")))
  (is (= "'C:\\\\x'" (polyfill/py-str "C:\\x")) "a backslash is doubled once, not again by a later escape")
  (is (= "'a\\nb'" (polyfill/py-str "a\nb")) "a newline cannot end the statement"))

(deftest anchors-cover-the-place-text-vocabulary
  (is (= 9 (count polyfill/anchors)))
  (is (= [0 0] (polyfill/anchors "top-left")))
  (is (= [1 1] (polyfill/anchors "bottom-right")))
  (is (= [0.5 0.5] (polyfill/anchors "center"))))

(deftest size-statement-keeps-the-aspect-ratio-for-one-side
  (is (= "_hg_w, _hg_h = _hg_w0, _hg_h0" (polyfill/size-statement nil nil)))
  (is (= "_hg_w, _hg_h = 300, 200" (polyfill/size-statement 300 200)))
  (is (str/includes? (polyfill/size-statement 600 nil) "round(_hg_h0 * 600 / _hg_w0)"))
  (is (str/includes? (polyfill/size-statement nil 400) "round(_hg_w0 * 400 / _hg_h0)")))

(deftest place-image-scales-then-mirrors-then-offsets
  (let [program (polyfill/place-image-program
                 {"file_path" "/tmp/spray.png" "x" 1664 "y" 2512
                  "anchor" "bottom-right" "width" 600 "flip" "both" "image_index" 2})
        scale   (index-of program #(str/includes? % "_hg_lyr.scale("))
        flip    (index-of program #(str/includes? % "transform_flip_simple"))
        offset  (index-of program #(str/includes? % "set_offsets"))]
    (is (< scale flip offset))
    (is (= 2 (count (filter #(str/includes? % "transform_flip_simple") program)))
        "both = a horizontal and a vertical mirror")
    (is (has? program "Gimp.get_images()[2]"))
    (is (has? program "1664 - 1.0 * _hg_lyr.get_width()") "bottom-right anchors the far corner")
    (is (has? program "Gio.File.new_for_path('/tmp/spray.png')"))
    (is (str/starts-with? (peek program) "print(json.dumps(") "the answer is the last statement")))

(deftest place-image-leaves-out-what-was-not-asked
  (let [program (polyfill/place-image-program {"file_path" "/tmp/a.png"})]
    (is (not (has? program "transform_flip_simple")))
    (is (not (has? program "set_name")))
    (is (not (has? program "set_opacity")))
    (is (has? program "Gimp.get_images()[0]") "image index defaults to the first image")
    (is (has? program "set_offsets(int(round(0 - 0.0 *") "top-left at the origin by default")))

(deftest place-image-names-and-fades-when-asked
  (let [program (polyfill/place-image-program
                 {"file_path" "/tmp/a.png" "name" "rose top-left" "opacity" 85})]
    (is (has? program "_hg_lyr.set_name('rose top-left')"))
    (is (has? program "_hg_lyr.set_opacity(85.0)"))))

(deftest place-image-problem-refuses-unknown-words
  (is (nil? (polyfill/place-image-problem {"anchor" "top" "flip" "horizontal"})))
  (is (nil? (polyfill/place-image-problem {})))
  (testing "a misspelled anchor is a refusal, never a silent top-left"
    (let [p (polyfill/place-image-problem {"anchor" "upper-left"})]
      (is (= :gimp/invalid-parameter (:error p)))
      (is (str/includes? (:message p) "upper-left"))))
  (is (some? (polyfill/place-image-problem {"flip" "diagonal"}))))

(deftest color-to-alpha-adds-alpha-before-the-filter
  (let [program (polyfill/color-to-alpha-program {"layer_name" "rose" "color" "#ffffff"})
        alpha   (index-of program #(str/includes? % "add_alpha"))
        filt    (index-of program #(str/includes? % "gegl:color-to-alpha"))
        merge   (index-of program #(str/includes? % "merge_filter"))]
    (is (< alpha filt merge))
    (is (has? program "get_layer_by_name('rose')"))
    (is (has? program "Gegl.Color.new('#ffffff')"))))

(deftest color-to-alpha-defaults
  (let [program (polyfill/color-to-alpha-program {})]
    (is (has? program "_hg_img.get_layers()[0]") "the top layer when none is named")
    (is (has? program "Gegl.Color.new('white')"))
    (is (has? program "'transparency-threshold', 0.08"))
    (is (has? program "'opacity-threshold', 0.35"))))
