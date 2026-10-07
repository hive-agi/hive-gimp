(ns hive-gimp.verbs.layer
  "PURE. Verbs over GIMP layers and groups: find, create, insert, name,
   visibility, opacity and offsets. Non-destructive by construction: nothing
   here merges, clears or deletes; `supersede` is how a retry replaces a
   layer (a new layer above it, the old one hidden)."
  (:require [hive-gimp.py :as py]
            [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def Placement
  [:map
   [:parent   {:optional true} [:maybe v/Handle]]
   [:position {:optional true} v/Position]])

(def NewLayer
  [:map {:closed true}
   [:name     v/Name]
   [:width    {:optional true} v/Dimension]
   [:height   {:optional true} v/Dimension]
   [:type     {:optional true} v/ImageType]
   [:opacity  {:optional true} v/Opacity]
   [:parent   {:optional true} [:maybe v/Handle]]
   [:position {:optional true} v/Position]
   [:as       {:optional true} v/PyName]])

(def NewGroup
  [:map {:closed true}
   [:name     v/Name]
   [:parent   {:optional true} [:maybe v/Handle]]
   [:position {:optional true} v/Position]
   [:as       {:optional true} v/PyName]])

(defn- parent-form [parent] (some-> parent v/handle-form))

(defn get-by-id
  "The layer with id `id`, or nil."
  {:verb/args [:cat v/ItemId] :verb/answers [:maybe v/Ref]}
  [id]
  (py/forms (Gimp.Layer/get-by-id ~id)))

(defn insert
  "Insert `layer` into `image` under `:parent` (nil: top level) at
   `:position` (default 0, the top). Answers the layer."
  {:verb/args [:cat v/Handle v/Handle Placement] :verb/answers v/Ref}
  [image layer {:keys [parent position] :or {position 0}}]
  (v/with-holes {?image    (v/handle-form image)
                 ?layer    (v/handle-form layer)
                 ?parent   (parent-form parent)
                 ?position position}
    (.insert-layer ?image ?layer ?parent ?position)
    ?layer))

(defn new-layer
  "A new transparent layer in `image`, sized to the image unless `:width` and
   `:height` say otherwise, inserted at `:position` under `:parent` and bound
   to `:as` (default `hg-layer`). Answers its `Ref`."
  {:verb/args [:cat v/Handle NewLayer] :verb/answers v/Ref}
  [image {:keys [name width height type opacity as] :or {type :rgba opacity 100 as 'hg-layer} :as opts}]
  (let [img (v/handle-form image)]
    (into (v/with-holes {?image img
                         ?as    as
                         ?w     (or width (list '.get-width img))
                         ?h     (or height (list '.get-height img))
                         ?type  (v/enum-form v/image-types type)}
            (def ?as (Gimp.Layer/new ?image ~name ?w ?h ?type ~(double opacity) Gimp.LayerMode/NORMAL)))
          (insert image as (select-keys opts [:parent :position])))))

(defn new-group
  "A new layer group in `image`, inserted at `:position` under `:parent` and
   bound to `:as` (default `hg-group`). Answers its `Ref`."
  {:verb/args [:cat v/Handle NewGroup] :verb/answers v/Ref}
  [image {:keys [name as] :or {as 'hg-group} :as opts}]
  (into (v/with-holes {?image (v/handle-form image) ?as as}
          (def ?as (Gimp.GroupLayer/new ?image ~name)))
        (insert image as (select-keys opts [:parent :position]))))

(defn set-name
  "Rename `item`. GIMP appends \" #1\" when the name is taken anywhere in the
   image. Answers the item."
  {:verb/args [:cat v/Handle v/Name] :verb/answers v/Ref}
  [item item-name]
  (v/with-holes {?item (v/handle-form item)}
    (.set-name ?item ~item-name)
    ?item))

(defn set-visible
  "Show or hide `item`. Answers the item."
  {:verb/args [:cat v/Handle :boolean] :verb/answers v/Ref}
  [item visible?]
  (v/with-holes {?item (v/handle-form item)}
    (.set-visible ?item ~visible?)
    ?item))

(defn set-opacity
  "Set `layer`'s opacity, in percent. Answers the layer."
  {:verb/args [:cat v/Handle v/Opacity] :verb/answers v/Ref}
  [layer opacity]
  (v/with-holes {?layer (v/handle-form layer)}
    (.set-opacity ?layer ~(double opacity))
    ?layer))

(defn set-offsets
  "Move `layer` so its top-left corner is at `:x`, `:y`. Answers the layer."
  {:verb/args [:cat v/Handle v/Offsets] :verb/answers v/Ref}
  [layer {:keys [x y]}]
  (v/with-holes {?layer (v/handle-form layer)}
    (.set-offsets ?layer ~x ~y)
    ?layer))

(defn get-offsets
  "The `Offsets` of `layer`'s top-left corner."
  {:verb/args [:cat v/Handle] :verb/answers v/Offsets}
  [layer]
  (v/with-holes {?layer (v/handle-form layer)}
    (def hg-offsets (.get-offsets ?layer))
    {"x" (nth hg-offsets 1) "y" (nth hg-offsets 2)}))

(defn supersede
  "The non-destructive retry: a new layer (`NewLayer` options, `:parent` and
   `:position` ignored) directly above `old` in `image`, then `old` hidden.
   Answers the new layer."
  {:verb/args [:cat v/Handle v/Handle NewLayer] :verb/answers v/Ref}
  [image old opts]
  (let [img (v/handle-form image)
        o   (v/handle-form old)
        new (:as opts 'hg-layer)]
    (-> (new-layer image (assoc opts
                                :parent   (list '.get-parent o)
                                :position (list '.get-item-position img o)))
        (into (v/with-holes {?old o ?new new}
                (.set-visible ?old false)
                ?new)))))

(catalog/register-ns! 'hive-gimp.verbs.layer)
