(ns hive-gimp.verbs.selection
  "PURE. Verbs over an image's selection: rectangle, ellipse, polygon, an
   item's alpha, invert, all, none, grow and shrink. Every verb answers the
   selection's `Bounds` afterwards, so a program can check what it selected.
   `:op` combines with the current selection (`v/channel-ops`, default
   `:replace`)."
  (:require [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def Box
  [:map {:closed true}
   [:x v/Coord] [:y v/Coord] [:width v/Dimension] [:height v/Dimension]
   [:op {:optional true} v/ChannelOp]])

(def Combine
  [:map {:closed true} [:op {:optional true} v/ChannelOp]])

(defn- bounds-of
  "Forms answering the `Bounds` of `img`'s selection (`img` a form)."
  [img]
  (v/with-holes {?image img}
    (def hg-bounds (Gimp.Selection/bounds ?image))
    {"empty?" (not (nth hg-bounds 1))
     "x"      (nth hg-bounds 2)
     "y"      (nth hg-bounds 3)
     "width"  (- (nth hg-bounds 4) (nth hg-bounds 2))
     "height" (- (nth hg-bounds 5) (nth hg-bounds 3))}))

(defn- then-bounds
  "`forms` acting on `image`, followed by the selection's `Bounds`."
  [image forms]
  (into forms (bounds-of (v/handle-form image))))

(defn- op-form [op] (v/enum-form v/channel-ops (or op :replace)))

(defn bounds
  "The `Bounds` of `image`'s selection."
  {:verb/args [:cat v/Handle] :verb/answers v/Bounds}
  [image]
  (bounds-of (v/handle-form image)))

(defn select-rectangle
  "Select the box `:x :y :width :height` of `image`."
  {:verb/args [:cat v/Handle Box] :verb/answers v/Bounds}
  [image {:keys [x y width height op]}]
  (then-bounds image (v/with-holes {?image (v/handle-form image) ?op (op-form op)}
                       (.select-rectangle ?image ?op ~(double x) ~(double y) ~(double width) ~(double height)))))

(defn select-ellipse
  "Select the ellipse inscribed in the box `:x :y :width :height` of `image`."
  {:verb/args [:cat v/Handle Box] :verb/answers v/Bounds}
  [image {:keys [x y width height op]}]
  (then-bounds image (v/with-holes {?image (v/handle-form image) ?op (op-form op)}
                       (.select-ellipse ?image ?op ~(double x) ~(double y) ~(double width) ~(double height)))))

(defn select-polygon
  "Select the polygon through `points` (`[[x y] ...]`, closed implicitly)."
  {:verb/args [:cat v/Handle v/Points Combine] :verb/answers v/Bounds}
  [image points {:keys [op]}]
  (then-bounds image (v/with-holes {?image (v/handle-form image) ?op (op-form op)}
                       (.select-polygon ?image ?op ~(vec (mapcat #(map double %) points))))))

(defn select-item
  "Select the alpha of `item` (a layer's opaque pixels, a path's interior)."
  {:verb/args [:cat v/Handle v/Handle Combine] :verb/answers v/Bounds}
  [image item {:keys [op]}]
  (then-bounds image (v/with-holes {?image (v/handle-form image) ?item (v/handle-form item) ?op (op-form op)}
                       (.select-item ?image ?op ?item))))

(defn invert
  "Invert `image`'s selection."
  {:verb/args [:cat v/Handle] :verb/answers v/Bounds}
  [image]
  (then-bounds image (v/with-holes {?image (v/handle-form image)}
                       (Gimp.Selection/invert ?image))))

(defn all
  "Select all of `image`."
  {:verb/args [:cat v/Handle] :verb/answers v/Bounds}
  [image]
  (then-bounds image (v/with-holes {?image (v/handle-form image)}
                       (Gimp.Selection/all ?image))))

(defn none
  "Select nothing in `image`."
  {:verb/args [:cat v/Handle] :verb/answers v/Bounds}
  [image]
  (then-bounds image (v/with-holes {?image (v/handle-form image)}
                       (Gimp.Selection/none ?image))))

(defn grow
  "Grow `image`'s selection by `steps` pixels."
  {:verb/args [:cat v/Handle v/Steps] :verb/answers v/Bounds}
  [image steps]
  (then-bounds image (v/with-holes {?image (v/handle-form image)}
                       (Gimp.Selection/grow ?image ~steps))))

(defn shrink
  "Shrink `image`'s selection by `steps` pixels."
  {:verb/args [:cat v/Handle v/Steps] :verb/answers v/Bounds}
  [image steps]
  (then-bounds image (v/with-holes {?image (v/handle-form image)}
                       (Gimp.Selection/shrink ?image ~steps))))

(catalog/register-ns! 'hive-gimp.verbs.selection)
