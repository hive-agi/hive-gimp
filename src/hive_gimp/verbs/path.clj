(ns hive-gimp.verbs.path
  "PURE. Verbs over GIMP paths (`Gimp.Path`, GIMP 3.2): create one, add a
   stroke through points, and stroke it onto a drawable. A polyline through
   `[[x y] ...]` is a Bezier stroke whose every anchor is its own two
   handles (`control-points`)."
  (:require [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.paint :as paint]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def NewPath
  [:map {:closed true}
   [:name v/Name]
   [:as {:optional true} v/PyName]])

(def StrokeOpts
  [:map {:closed true} [:closed? {:optional true} :boolean]])

(def FromPoints
  [:map {:closed true}
   [:name v/Name]
   [:closed? {:optional true} :boolean]
   [:as {:optional true} v/PyName]])

(defn control-points
  "The flat Bezier control points of the polyline through `points`: each
   anchor repeated as its own in and out handle."
  [points]
  (vec (mapcat (fn [[x y]] (let [x (double x) y (double y)] [x y x y x y])) points)))

(defn new-path
  "A new empty path in `image`, inserted at the top of its paths and bound to
   `:as` (default `hg-path`). Answers its `Ref`."
  {:verb/args [:cat v/Handle NewPath] :verb/answers v/Ref}
  [image {:keys [name as] :or {as 'hg-path}}]
  (v/with-holes {?image (v/handle-form image) ?as as}
    (def ?as (Gimp.Path/new ?image ~name))
    (.insert-path ?image ?as nil 0)
    ?as))

(defn stroke-new-from-points
  "Add a stroke through `points` to `path`, closed when `:closed?`. Answers
   the path."
  {:verb/args [:cat v/Handle v/Points StrokeOpts] :verb/answers v/Ref}
  [path points {:keys [closed?] :or {closed? false}}]
  (v/with-holes {?path (v/handle-form path)}
    (.stroke-new-from-points ?path Gimp.PathStrokeType/BEZIER ~(control-points points) ~closed?)
    ?path))

(defn from-points
  "A new path named `:name` in `image` through `points`. Answers its `Ref`."
  {:verb/args [:cat v/Handle v/Points FromPoints] :verb/answers v/Ref}
  [image points {:keys [as] :or {as 'hg-path} :as opts}]
  (into (new-path image (assoc (select-keys opts [:name]) :as as))
        (stroke-new-from-points as points (select-keys opts [:closed?]))))

(defn stroke
  "Stroke `path` onto `drawable` with `:colour` and `:width` when given.
   Answers the drawable."
  {:verb/args [:cat v/Handle v/Handle paint/Stroke] :verb/answers v/Ref}
  [drawable path opts]
  (paint/edit-stroke-item drawable path opts))

(catalog/register-ns! 'hive-gimp.verbs.path)
