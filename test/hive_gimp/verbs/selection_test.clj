(ns hive-gimp.verbs.selection-test
  "hive-gimp.verbs.selection as a trifecta over `program-of`."
  (:require [clojure.test :refer [deftest is]]
            [hive-gimp.py :as py]
            [hive-gimp.verbs.support-test :as s :refer [call]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def img  {:id 1 :type "Image" :name "logo.xcf"})
(def star {:id 14 :type "Layer" :name "P e estrela"})

(def cases
  {:bounds              (call :selection/bounds img)
   :select-rectangle    (call :selection/select-rectangle img {:x 10 :y 20 :width 300 :height 40})
   :select-rectangle-op (call :selection/select-rectangle 'scratch {:x 0.5 :y 1 :width 2 :height 3 :op :subtract})
   :select-ellipse      (call :selection/select-ellipse img {:x 0 :y 0 :width 64 :height 64 :op :add})
   :select-polygon      (call :selection/select-polygon img [[0 0] [100 0] [50 80]] {})
   :select-polygon-op   (call :selection/select-polygon img [[1.5 2] [3 4]] {:op :intersect})
   :select-item         (call :selection/select-item img star {})
   :select-item-op      (call :selection/select-item img 'body {:op :add})
   :invert              (call :selection/invert img)
   :all                 (call :selection/all img)
   :none                (call :selection/none img)
   :grow                (call :selection/grow img 5)
   :shrink              (call :selection/shrink img 3)})

(deftest every-selection-verb-has-a-golden-case
  (is (empty? (s/uncovered "selection" cases))))

(deftrifecta selection-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/selection.edn"
   :cases         cases
   :xf            py/->python
   :gen           (s/gen-call "selection")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     200
   :mutations
   [["bounds height from x"        (s/on :selection/bounds #(mapv (fn [f] (if (map? f) (assoc f "height" (get f "width")) f))
                                                                  (s/original %)))]
    ["bounds empty? not negated"   (s/on :selection/bounds #(mapv (fn [f] (if (map? f) (update f "empty?" second) f))
                                                                  (s/original %)))]
    ["rectangle ignores op"        (s/with-args :selection/select-rectangle (fn [[i b]] [i (dissoc b :op)]))]
    ["rectangle swaps the size"    (s/with-args :selection/select-rectangle (fn [[i b]] [i (assoc b :width (:height b) :height (:width b))]))]
    ["rectangle forgets bounds"    (s/on :selection/select-rectangle #(vec (take 1 (s/original %))))]
    ["ellipse draws a rectangle"   (s/on :selection/select-ellipse #(s/original (assoc % :verb :selection/select-rectangle)))]
    ["polygon drops the last point" (s/with-args :selection/select-polygon (fn [[i ps o]] [i (vec (butlast ps)) o]))]
    ["polygon ignores op"          (s/with-args :selection/select-polygon (fn [[i ps _]] [i ps {}]))]
    ["item ignores op"             (s/with-args :selection/select-item (fn [[i it _]] [i it {}]))]
    ["invert selects all"          (s/on :selection/invert #(s/original (assoc % :verb :selection/all)))]
    ["all selects none"            (s/on :selection/all #(s/original (assoc % :verb :selection/none)))]
    ["none inverts"                (s/on :selection/none #(s/original (assoc % :verb :selection/invert)))]
    ["grow shrinks"                (s/on :selection/grow #(s/original (assoc % :verb :selection/shrink)))]
    ["shrink by one"               (s/with-args :selection/shrink (fn [[i _]] [i 1]))]]})
