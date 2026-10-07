(ns hive-gimp.verbs.image-test
  "hive-gimp.verbs.image as a trifecta over `program-of`: one golden case per
   verb (rendered Python, byte-exact), calls generated from `:verb/args`,
   and one mutant per plausible builder bug."
  (:require [clojure.test :refer [deftest is]]
            [hive-gimp.py :as py]
            [hive-gimp.verbs.support-test :as s :refer [call]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def img {:id 1 :type "Image" :name "logo.xcf"})

(def cases
  {:new-image           (call :image/new-image {:width 640 :height 480})
   :new-image-as        (call :image/new-image {:width 8 :height 8 :as 'scratch})
   :get-images          (call :image/get-images)
   :get-by-id           (call :image/get-by-id 7)
   :get-by-name         (call :image/get-by-name "logo.xcf")
   :get-size-ref        (call :image/get-size img)
   :get-size-bound      (call :image/get-size 'scratch)
   :get-layers          (call :image/get-layers img)
   :get-selected-layers (call :image/get-selected-layers img)
   :get-layer-by-name   (call :image/get-layer-by-name img "FRANCANA")})

(deftest every-image-verb-has-a-golden-case
  (is (empty? (s/uncovered "image" cases))))

(deftrifecta image-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/image.edn"
   :cases         cases
   :xf            py/->python
   :gen           (s/gen-call "image")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     200
   :mutations
   [["new-image swaps width and height"
     (s/with-args :image/new-image (fn [[o]] [(assoc o :width (:height o) :height (:width o))]))]
    ["new-image forgets to answer"    (s/on :image/new-image #(vec (butlast (s/original %))))]
    ["get-by-id looks up an item"     (s/on :image/get-by-id #(py/forms (Gimp.Item/get-by-id ~(first (:args %)))))]
    ["get-by-name takes the first"    (s/on :image/get-by-name (fn [_] (py/forms (next (iter (Gimp/get-images)) nil))))]
    ["get-size reports width twice"   (s/on :image/get-size #(mapv (fn [f] (if (map? f) (assoc f "height" (get f "width")) f))
                                                                  (s/original %)))]
    ["get-layers answers selected"    (s/on :image/get-layers #(s/original (assoc % :verb :image/get-selected-layers)))]
    ["selected answers all layers"    (s/on :image/get-selected-layers #(s/original (assoc % :verb :image/get-layers)))]
    ["layer-by-name ignores the name" (s/with-args :image/get-layer-by-name (fn [[i _]] [i "Background"]))]
    ["get-images answers a count"     (s/on :image/get-images (fn [_] (py/forms (len (Gimp/get-images)))))]]})
