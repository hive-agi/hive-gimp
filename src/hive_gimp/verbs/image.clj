(ns hive-gimp.verbs.image
  "PURE. Verbs over GIMP images: create, list, find by id or name, size, and
   their layers. Each answers a vector of hive-gimp.py forms whose last form
   is the value the program answers."
  (:require [hive-gimp.py :as py]
            [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def NewImage
  [:map {:closed true}
   [:width  v/Dimension]
   [:height v/Dimension]
   [:as {:optional true} v/PyName]])

(defn new-image
  "A new RGB image of `:width` x `:height`, bound to `:as` (default
   `hg-image`). Answers its `Ref`."
  {:verb/args [:cat NewImage] :verb/answers v/Ref}
  [{:keys [width height as] :or {as 'hg-image}}]
  (v/with-holes {?as as}
    (def ?as (Gimp.Image/new ~width ~height Gimp.ImageBaseType/RGB))
    ?as))

(defn get-images
  "Every open image, as `Ref`s."
  {:verb/args [:cat] :verb/answers [:vector v/Ref]}
  []
  (py/forms (Gimp/get-images)))

(defn get-by-id
  "The image with id `id`, or nil."
  {:verb/args [:cat v/ItemId] :verb/answers [:maybe v/Ref]}
  [id]
  (py/forms (Gimp.Image/get-by-id ~id)))

(defn get-by-name
  "The first open image named `image-name`, or nil."
  {:verb/args [:cat v/Name] :verb/answers [:maybe v/Ref]}
  [image-name]
  (py/forms (next (iter (for [i (Gimp/get-images) :when (= (.get-name i) ~image-name)] i)) nil)))

(defn get-size
  "The canvas `Size` of `image`."
  {:verb/args [:cat v/Handle] :verb/answers v/Size}
  [image]
  (v/with-holes {?image (v/handle-form image)}
    {"width" (.get-width ?image) "height" (.get-height ?image)}))

(defn get-layers
  "The top-level layers of `image`, top first."
  {:verb/args [:cat v/Handle] :verb/answers [:vector v/Ref]}
  [image]
  (v/with-holes {?image (v/handle-form image)}
    (.get-layers ?image)))

(defn get-selected-layers
  "The layers selected in `image`."
  {:verb/args [:cat v/Handle] :verb/answers [:vector v/Ref]}
  [image]
  (v/with-holes {?image (v/handle-form image)}
    (.get-selected-layers ?image)))

(defn get-layer-by-name
  "The layer of `image` named `layer-name`, at any depth, or nil."
  {:verb/args [:cat v/Handle v/Name] :verb/answers [:maybe v/Ref]}
  [image layer-name]
  (v/with-holes {?image (v/handle-form image)}
    (.get-layer-by-name ?image ~layer-name)))

(catalog/register-ns! 'hive-gimp.verbs.image)
