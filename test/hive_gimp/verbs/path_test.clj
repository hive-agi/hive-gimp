(ns hive-gimp.verbs.path-test
  "hive-gimp.verbs.path as a trifecta over `program-of`, and `control-points`
   as its own trifecta: the polyline-to-Bezier rule is pure arithmetic."
  (:require [clojure.test :refer [deftest is]]
            [hive-gimp.py :as py]
            [hive-gimp.verbs.path]
            [hive-gimp.verbs.support-test :as s :refer [call]]
            [hive-gimp.verbs.value :as v]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def img   {:id 1 :type "Image" :name "logo.xcf"})
(def layer {:id 12 :type "Layer" :name "corpo azul"})
(def path  {:id 40 :type "Path" :name "contorno"})

(def cases
  {:new-path                (call :path/new-path img {:name "contorno"})
   :new-path-as             (call :path/new-path 'scratch {:name "guia" :as 'guide})
   :stroke-new-from-points  (call :path/stroke-new-from-points path [[0 0] [10 5]] {})
   :stroke-closed           (call :path/stroke-new-from-points 'guide [[0 0] [10 0] [5 8.5]] {:closed? true})
   :from-points             (call :path/from-points img [[1 2] [3 4] [5 6]] {:name "linha"})
   :from-points-closed      (call :path/from-points img [[0 0] [9 0] [9 9]] {:name "tri" :closed? true :as 'tri})
   :stroke                  (call :path/stroke layer path {})
   :stroke-styled           (call :path/stroke 'bg 'tri {:colour "#ffffff" :width 2})})

(deftest every-path-verb-has-a-golden-case
  (is (empty? (s/uncovered "path" cases))))

(deftrifecta path-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/path.edn"
   :cases         cases
   :xf            py/->python
   :gen           (s/gen-call "path")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     200
   :mutations
   [["new-path never inserted"     (s/drop-forms :path/new-path '.insert-path)]
    ["new-path ignores the name"   (s/with-args :path/new-path (fn [[i o]] [i (assoc o :name "Path")]))]
    ["stroke never closes"         (s/with-args :path/stroke-new-from-points (fn [[p ps _]] [p ps {}]))]
    ["stroke always closes"        (s/with-args :path/stroke-new-from-points (fn [[p ps _]] [p ps {:closed? true}]))]
    ["stroke reversed"             (s/with-args :path/stroke-new-from-points (fn [[p ps o]] [p (vec (reverse ps)) o]))]
    ["from-points has no stroke"   (s/drop-forms :path/from-points '.stroke-new-from-points)]
    ["from-points never closes"    (s/with-args :path/from-points (fn [[i ps o]] [i ps (dissoc o :closed?)]))]
    ["stroke strokes selection"    (s/on :path/stroke #(let [[d _ o] (:args %)] (s/original (call :paint/edit-stroke-selection d o))))]]})

(deftrifecta control-points hive-gimp.verbs.path/control-points
  {:golden-path   "test/golden/hive_gimp/verbs/control_points.edn"
   :cases         {:segment  [[0 0] [10 5]]
                   :triangle [[0 0] [10 0] [5 8.5]]
                   :negative [[-3 -4] [0.25 1e3]]}
   :gen           (hs/generator v/Points)
   :property-type :pred-io
   :pred          (fn [points cps]
                    (and (= (* 6 (count points)) (count cps))
                         (every? double? cps)
                         (every? (fn [[i [x y]]]
                                   (= (repeat 3 [(double x) (double y)])
                                      (partition 2 (subvec cps (* 6 i) (* 6 (inc i))))))
                                 (map-indexed vector points))))
   :num-tests     200
   :mutations     [["anchor only"        (fn [ps] (vec (mapcat (fn [[x y]] [(double x) (double y)]) ps)))]
                   ["handles swap x y"   (fn [ps] (vec (mapcat (fn [[x y]] (let [x (double x) y (double y)] [x y y x x y])) ps)))]
                   ["longs, not doubles" (fn [ps] (vec (mapcat (fn [[x y]] [x y x y x y]) ps)))]]})
