(ns hive-gimp.verbs.layer-test
  "hive-gimp.verbs.layer as a trifecta over `program-of`, plus the
   non-destructive rule as an assertion over every generated program."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [hive-gimp.py :as py]
            [hive-gimp.verbs :as verbs]
            [hive-gimp.verbs.support-test :as s :refer [call]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def img   {:id 1 :type "Image" :name "logo.xcf"})
(def body  {:id 12 :type "Layer" :name "corpo azul"})
(def group {:id 30 :type "GroupLayer" :name "logo v2"})

(def cases
  {:get-by-id         (call :layer/get-by-id 12)
   :new-layer         (call :layer/new-layer img {:name "corpo v3"})
   :new-layer-full    (call :layer/new-layer img {:name "estrela" :width 64 :height 32 :type :graya
                                                  :opacity 40 :parent group :position 2 :as 'star})
   :new-layer-bound   (call :layer/new-layer 'scratch {:name "fundo" :parent 'grp :as 'bg})
   :new-group         (call :layer/new-group img {:name "logo v3"})
   :new-group-nested  (call :layer/new-group img {:name "texto" :parent group :position 1 :as 'words})
   :insert            (call :layer/insert img body {})
   :insert-placed     (call :layer/insert img 'bg {:parent group :position 3})
   :set-name          (call :layer/set-name body "corpo azul (old)")
   :set-visible       (call :layer/set-visible body false)
   :set-opacity       (call :layer/set-opacity body 55)
   :set-offsets       (call :layer/set-offsets body {:x -4 :y 9})
   :get-offsets       (call :layer/get-offsets 'bg)
   :supersede         (call :layer/supersede img body {:name "corpo azul v2"})
   :supersede-as      (call :layer/supersede img 'old {:name "retry" :opacity 80 :as 'retry})})

(deftest every-layer-verb-has-a-golden-case
  (is (empty? (s/uncovered "layer" cases))))

(def ^:private destructive #"\.(merge_down|merge_visible_layers|flatten|edit_clear|delete|remove_layer|merge_filter)\(")

(defspec layer-programs-are-non-destructive 200
  (prop/for-all [c (s/gen-call "layer")]
    (not (re-find destructive (py/->python (verbs/program-of c))))))

(deftrifecta layer-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/layer.edn"
   :cases         cases
   :xf            py/->python
   :gen           (s/gen-call "layer")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     200
   :mutations
   [["get-by-id looks up an image"  (s/on :layer/get-by-id #(py/forms (Gimp.Image/get-by-id ~(first (:args %)))))]
    ["new-layer never inserted"     (s/drop-forms :layer/new-layer '.insert-layer)]
    ["new-layer opaque regardless"  (s/with-args :layer/new-layer (fn [[i o]] [i (dissoc o :opacity)]))]
    ["new-layer always rgba"        (s/with-args :layer/new-layer (fn [[i o]] [i (dissoc o :type)]))]
    ["new-layer ignores parent"     (s/with-args :layer/new-layer (fn [[i o]] [i (dissoc o :parent)]))]
    ["new-layer swaps the size"     (s/with-args :layer/new-layer (fn [[i o]] [i (assoc o :width (:height o) :height (:width o))]))]
    ["new-group never inserted"     (s/drop-forms :layer/new-group '.insert-layer)]
    ["insert ignores position"      (s/with-args :layer/insert (fn [[i l o]] [i l (dissoc o :position)]))]
    ["set-name is a no-op"          (s/drop-forms :layer/set-name '.set-name)]
    ["set-visible inverted"         (s/with-args :layer/set-visible (fn [[l v]] [l (not v)]))]
    ["opacity as a fraction"        (s/with-args :layer/set-opacity (fn [[l o]] [l (/ o 100.0)]))]
    ["offsets swapped"              (s/with-args :layer/set-offsets (fn [[l {:keys [x y]}]] [l {:x y :y x}]))]
    ["offsets read from index 0"    (s/on :layer/get-offsets #(mapv (fn [f] (if (map? f) (into {} (map (fn [[k v]] [k (if (seq? v) (concat (butlast v) [0]) v)])) f) f))
                                                                    (s/original %)))]
    ["supersede keeps old visible"  (s/drop-forms :layer/supersede '.set-visible)]
    ["supersede lands on top"       (s/on :layer/supersede #(let [[i _ o] (:args %)] (s/original (s/call :layer/new-layer i o))))]]})
