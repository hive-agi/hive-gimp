(ns hive-gimp.polyfill
  "PROMOTE. Commands the listening plug-in cannot answer, written as the GIMP 3
   Python that answers them, as DATA.

   The reference Python plug-in has no handler for `place_image` (only the
   native plug-in does) and nothing at all for `color_to_alpha`. Both are a few
   calls of GIMP 3's own PyGObject API, which the plug-in's exec fallback runs
   unchanged. This namespace turns a built command's wire params into those
   statements; `hive-gimp.compose` is the boundary that sends them.

   Pure, and .cljc, so every program this emits is asserted on the JVM without a
   GIMP anywhere: the anchor arithmetic, the size rule, which statements a
   missing parameter leaves out, and how a string reaches Python.

   Two facts about the exec channel shape the output, both measured against
   GIMP 3.2.6:

   1. Each string is exec'd as its own statement, in one long-lived context. So
      a program is a vector of single statements, and its names carry a `_hg_`
      prefix to stay clear of whatever a caller left in that context.
   2. `eval` runs in a DIFFERENT context from `exec` and cannot see what exec
      bound. The only way a result comes back is stdout, so every program ends
      by printing one JSON object, which the boundary reads as the value."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Value objects
;; =============================================================================

(def anchors
  "Anchor name -> the fraction of the layer's width and height that sits at
   (x, y). The same nine names `place_text` documents, so one vocabulary places
   both text and images."
  {"top-left"    [0   0]   "top"    [0.5 0]   "top-right"    [1 0]
   "left"        [0   0.5] "center" [0.5 0.5] "right"        [1 0.5]
   "bottom-left" [0   1]   "bottom" [0.5 1]   "bottom-right" [1 1]})

(def flips
  "Flip name -> the GIMP orientations applied, in order. `both` is a half turn
   by two mirrors, which keeps the layer's own centre where it was."
  {"horizontal" ["HORIZONTAL"]
   "vertical"   ["VERTICAL"]
   "both"       ["HORIZONTAL" "VERTICAL"]})

;; =============================================================================
;; Python literals
;; =============================================================================

(defn py-str
  "`s` as a Python string literal.

   Hand-escaped rather than borrowed from a JSON writer because this namespace
   is .cljc. `str/escape` maps each character once, so the backslash an escape
   adds is never escaped again, on either platform."
  [s]
  (str "'"
       (str/escape (str s) {\\ "\\\\" \' "\\'" \newline "\\n" \return "\\r"})
       "'"))

(defn- py-num
  [n]
  (str (double n)))

;; =============================================================================
;; Validation
;; =============================================================================

(defn- invalid
  [command param expected actual]
  {:error   :gimp/invalid-parameter
   :command command
   :message (str "`" param "` must be one of " expected ", not " (pr-str actual) ".")})

(defn place-image-problem
  "nil, or the reason these `place_image` wire params cannot be composed."
  [{:strs [anchor flip]}]
  (cond
    (and anchor (not (contains? anchors anchor)))
    (invalid "place_image" "anchor" (vec (sort (keys anchors))) anchor)

    (and flip (not (contains? flips flip)))
    (invalid "place_image" "flip" (vec (sort (keys flips))) flip)))

;; =============================================================================
;; Programs
;; =============================================================================

(defn- image-statement
  [image-index]
  (str "_hg_img = Gimp.get_images()[" (long (or image-index 0)) "]"))

(defn size-statement
  "The statement binding `_hg_w`/`_hg_h`, the layer's final size.

   Width or height alone keeps the aspect ratio; both stretch to exactly that
   box; neither keeps the file's own size. Decided here, in Clojure, so the
   Python that runs is one assignment and never a branch."
  [width height]
  (cond
    (and width height)
    (str "_hg_w, _hg_h = " (long width) ", " (long height))

    width
    (str "_hg_w, _hg_h = " (long width) ", max(1, round(_hg_h0 * " (long width) " / _hg_w0))")

    height
    (str "_hg_w, _hg_h = max(1, round(_hg_w0 * " (long height) " / _hg_h0)), " (long height))

    :else
    "_hg_w, _hg_h = _hg_w0, _hg_h0"))

(def ^:private report-layer
  (str "print(json.dumps({'layer': _hg_lyr.get_name(),"
       " 'x': _hg_lyr.get_offsets()[1], 'y': _hg_lyr.get_offsets()[2],"
       " 'width': _hg_lyr.get_width(), 'height': _hg_lyr.get_height(),"
       " 'opacity': _hg_lyr.get_opacity()}))"))

(defn place-image-program
  "The statements that place the file in `params` as a new top layer.

   Order matters and is the point of writing it down: scale, then mirror, then
   offset, because the offset is computed from the FINAL size and an anchor
   other than top-left moves with it."
  [{:strs [file_path x y anchor width height opacity name flip image_index]}]
  (let [[fx fy] (get anchors (or anchor "top-left"))]
    (-> ["import json"
         "from gi.repository import Gio"
         (image-statement image_index)
         (str "_hg_lyr = Gimp.file_load_layer(Gimp.RunMode.NONINTERACTIVE, _hg_img,"
              " Gio.File.new_for_path(" (py-str file_path) "))")
         "_hg_img.insert_layer(_hg_lyr, None, 0)"
         "_hg_w0, _hg_h0 = _hg_lyr.get_width(), _hg_lyr.get_height()"
         (size-statement width height)
         "(_hg_w, _hg_h) == (_hg_w0, _hg_h0) or _hg_lyr.scale(_hg_w, _hg_h, False)"]
        (into (for [o (get flips flip)]
                (str "_hg_lyr.transform_flip_simple(Gimp.OrientationType." o ", True, 0.0)")))
        (conj (str "_hg_lyr.set_offsets(int(round(" (long (or x 0)) " - " (py-num fx) " * _hg_lyr.get_width())),"
                   " int(round(" (long (or y 0)) " - " (py-num fy) " * _hg_lyr.get_height())))"))
        (cond-> name    (conj (str "_hg_lyr.set_name(" (py-str name) ")"))
                opacity (conj (str "_hg_lyr.set_opacity(" (py-num opacity) ")")))
        (conj "Gimp.displays_flush()" report-layer))))

(defn color-to-alpha-program
  "The statements that make `color` transparent on a layer, through GEGL.

   The layer gains an alpha channel first: color-to-alpha on a layer without
   one has nowhere to put the transparency and changes nothing."
  [{:strs [color transparency_threshold opacity_threshold layer_name image_index]}]
  [ "import json"
   "from gi.repository import Gegl"
   (image-statement image_index)
   (if layer_name
     (str "_hg_lyr = _hg_img.get_layer_by_name(" (py-str layer_name) ")")
     "_hg_lyr = _hg_img.get_layers()[0]")
   "_hg_lyr.has_alpha() or _hg_lyr.add_alpha()"
   "_hg_f = Gimp.DrawableFilter.new(_hg_lyr, 'gegl:color-to-alpha', 'color to alpha')"
   "_hg_c = _hg_f.get_config()"
   (str "_hg_c.set_property('color', Gegl.Color.new(" (py-str (or color "white")) "))")
   (str "_hg_c.set_property('transparency-threshold', " (py-num (or transparency_threshold 0.08)) ")")
   (str "_hg_c.set_property('opacity-threshold', " (py-num (or opacity_threshold 0.35)) ")")
   "_hg_lyr.merge_filter(_hg_f)"
   "Gimp.displays_flush()"
   "print(json.dumps({'layer': _hg_lyr.get_name(), 'has_alpha': _hg_lyr.has_alpha()}))"])
