(ns hive-gimp.verbs.value
  "DOMAIN. The values GIMP verbs take and answer, as malli schemas registered
   under `:hive-gimp.verbs/*`, and their projection onto hive-gimp.py forms.

   A `Handle` names a GIMP object inside a program: a `Ref` that came back
   from GIMP (`{:id :type :name}`), or a symbol naming a Python variable an
   earlier verb of the same program bound with `:as`. `handle-form` turns
   either into the expression that reaches the object; how a `Ref` is looked
   up is the open multimethod `lookup-form`, keyed by its `:type`.

   GIMP's enumerations are DATA tables (`channel-ops`, `fill-types`,
   `image-types`): a new member is a map entry. Pure; nothing here touches
   GIMP."
  (:require [clojure.walk :as walk]
            [hive-gimp.py :as py]
            [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Identity
;; =============================================================================

(def ItemId [:int {:min 1 :max 1000000}])

(def TypeName
  [:re {:error/message "must be a GIMP class name like Layer"
        :gen/elements  ["Image" "Layer" "GroupLayer" "TextLayer" "Path" "Channel" "Display"]}
   #"^[A-Z][A-Za-z]*$"])

(def Ref
  "A GIMP object as GIMP answers it."
  [:map [:id ItemId] [:type TypeName] [:name [:maybe :string]]])

(defn py-name?
  "True when `x` is an unqualified symbol usable as a Python variable."
  [x]
  (and (symbol? x)
       (nil? (namespace x))
       (some? (re-matches #"[A-Za-z_][A-Za-z0-9_-]*" (name x)))))

(def PyName
  [:fn {:error/message "must be an unqualified symbol naming a Python variable"
        :gen/elements  '[img lyr group hg-layer hg-path]}
   py-name?])

(def Handle
  "How a verb is told which GIMP object to act on."
  [:or Ref PyName])

;; =============================================================================
;; Quantities
;; =============================================================================

(def Name [:string {:min 1 :max 64}])

(def Dimension [:int {:min 1 :max 65536}])

(def Coord
  [:or [:int {:min -100000 :max 100000}] [:double {:min -100000.0 :max 100000.0}]])

(def Size [:map [:width Dimension] [:height Dimension]])

(def Offsets [:map [:x :int] [:y :int]])

(def Opacity
  "Percent, as GIMP's `set_opacity` takes it."
  [:or [:int {:min 0 :max 100}] [:double {:min 0.0 :max 100.0}]])

(def Unit [:or [:int {:min 0 :max 1}] [:double {:min 0.0 :max 1.0}]])

(def Steps [:int {:min 1 :max 1000}])

(def Position
  "Stack position for an insert: 0 is the top, -1 above the active item."
  [:int {:min -1 :max 10000}])

(def Point [:tuple Coord Coord])

(def Points [:vector {:min 2 :max 16} Point])

(def Bounds
  "The selection's bounding box; `:empty?` true when nothing is selected."
  [:map [:empty? :boolean] [:x :int] [:y :int]
   [:width [:int {:min 0}]] [:height [:int {:min 0}]]])

;; =============================================================================
;; Colour
;; =============================================================================

(def Css
  [:re {:error/message "must be a CSS colour name or #rgb, #rrggbb, #rrggbbaa"
        :gen/elements  ["black" "white" "red" "#fff" "#2a4b8d" "#2a4b8d80"]}
   #"^(#[0-9a-fA-F]{3}|#[0-9a-fA-F]{6}|#[0-9a-fA-F]{8}|[a-z]+)$"])

(def Rgba
  "Channels in 0..1; an absent `:a` is opaque."
  [:map [:r Unit] [:g Unit] [:b Unit] [:a {:optional true} Unit]])

(def Colour [:or Css Rgba])

;; =============================================================================
;; Files
;; =============================================================================

(def FilePath
  [:re {:error/message "must be an absolute path"
        :gen/elements  ["/tmp/a.xcf" "/tmp/logo.png" "/srv/art/in.jpg"]}
   #"^/.+"])

(def XcfPath
  [:re {:error/message "must be an absolute path ending in .xcf"
        :gen/elements  ["/tmp/a.xcf" "/srv/art/logo v3.xcf"]}
   #"^/.*\.xcf$"])

(def PngPath
  [:re {:error/message "must be an absolute path ending in .png"
        :gen/elements  ["/tmp/a.png" "/srv/art/logo v3.png"]}
   #"^/.*\.png$"])

;; =============================================================================
;; Enumerations (closed in GIMP, data here)
;; =============================================================================

(def channel-ops
  {:replace   'Gimp.ChannelOps/REPLACE
   :add       'Gimp.ChannelOps/ADD
   :subtract  'Gimp.ChannelOps/SUBTRACT
   :intersect 'Gimp.ChannelOps/INTERSECT})

(def fill-types
  {:foreground  'Gimp.FillType/FOREGROUND
   :background  'Gimp.FillType/BACKGROUND
   :white       'Gimp.FillType/WHITE
   :transparent 'Gimp.FillType/TRANSPARENT
   :pattern     'Gimp.FillType/PATTERN})

(def image-types
  {:rgb   'Gimp.ImageType/RGB_IMAGE
   :rgba  'Gimp.ImageType/RGBA_IMAGE
   :gray  'Gimp.ImageType/GRAY_IMAGE
   :graya 'Gimp.ImageType/GRAYA_IMAGE})

(defn- enum-of [table] (into [:enum] (sort (keys table))))

(def ChannelOp (enum-of channel-ops))
(def FillType  (enum-of fill-types))
(def ImageType (enum-of image-types))

(defn enum-form
  "The GIMP enum member `k` names in `table`."
  [table k]
  (or (get table k)
      (throw (ex-info (str k " is not one of " (sort (keys table)))
                      {:hive-gimp/reason :verbs/unknown-enum :value k}))))

;; =============================================================================
;; Registration
;; =============================================================================

(def registered-schemas
  {:hive-gimp.verbs/ref        Ref
   :hive-gimp.verbs/py-name    PyName
   :hive-gimp.verbs/handle     Handle
   :hive-gimp.verbs/name       Name
   :hive-gimp.verbs/size       Size
   :hive-gimp.verbs/offsets    Offsets
   :hive-gimp.verbs/opacity    Opacity
   :hive-gimp.verbs/point      Point
   :hive-gimp.verbs/points     Points
   :hive-gimp.verbs/bounds     Bounds
   :hive-gimp.verbs/rgba       Rgba
   :hive-gimp.verbs/colour     Colour
   :hive-gimp.verbs/file-path  FilePath
   :hive-gimp.verbs/xcf-path   XcfPath
   :hive-gimp.verbs/png-path   PngPath
   :hive-gimp.verbs/channel-op ChannelOp
   :hive-gimp.verbs/fill-type  FillType
   :hive-gimp.verbs/image-type ImageType})

(hs/register-all! registered-schemas)

(def ref?    (m/validator Ref))
(def handle? (m/validator Handle))
(def colour? (m/validator Colour))

;; =============================================================================
;; Projection onto forms
;; =============================================================================

(defmulti lookup-form
  "The expression reaching the GIMP object `ref` names, keyed by `:type`."
  :type)

(defmethod lookup-form "Image"   [{:keys [id]}] (first (py/forms (Gimp.Image/get-by-id ~id))))
(defmethod lookup-form "Display" [{:keys [id]}] (first (py/forms (Gimp.Display/get-by-id ~id))))
(defmethod lookup-form :default  [{:keys [id]}] (first (py/forms (Gimp.Item/get-by-id ~id))))

(defprotocol IHandle
  (handle-form [h] "The expression reaching the object `h` names. A form
   (a list) is already an expression and stands for itself."))

(extend-protocol IHandle
  clojure.lang.IPersistentMap
  (handle-form [ref] (lookup-form ref))
  clojure.lang.Symbol
  (handle-form [sym] sym)
  clojure.lang.ISeq
  (handle-form [form] form))

(defn plug
  "`forms` with every `?hole` symbol replaced by the FORM `holes` maps it to
   (a form, unlike `~x`, which splices a value as a literal)."
  [forms holes]
  (walk/postwalk-replace holes forms))

(defmacro with-holes
  "`py/forms` of `body`, with each `?hole` symbol of the `holes` map replaced
   by the form its value evaluates to."
  [holes & body]
  `(plug (py/forms ~@body) ~(into {} (map (fn [[k v]] [(list 'quote k) v])) holes)))

(defmulti colour-forms
  "Statements binding the Python name `hg-colour` to a `Gegl.Color` for
   `colour`, a CSS string or an `Rgba` map."
  class)

(defmethod colour-forms String
  [css]
  (py/forms
   (import gi.repository [Gegl])
   (def hg-colour (Gegl.Color/new ~css))))

(defmethod colour-forms clojure.lang.IPersistentMap
  [{:keys [r g b a] :or {a 1.0}}]
  (py/forms
   (import gi.repository [Gegl])
   (def hg-colour (Gegl.Color/new "black"))
   (.set-rgba hg-colour ~(double r) ~(double g) ~(double b) ~(double a))))
