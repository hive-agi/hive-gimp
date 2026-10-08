(ns hive-gimp.verbs.paint-test
  "hive-gimp.verbs.paint as a trifecta over `program-of`, plus the rule that
   a one-shot colour or width never leaks into GIMP's context."
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

(def layer {:id 12 :type "Layer" :name "corpo azul"})
(def path  {:id 40 :type "Path" :name "contorno"})

(def cases
  {:context-get-foreground     (call :paint/context-get-foreground)
   :context-get-background     (call :paint/context-get-background)
   :context-set-foreground     (call :paint/context-set-foreground "#2a4b8d")
   :context-set-foreground-rgb (call :paint/context-set-foreground {:r 1 :g 0.5 :b 0})
   :context-set-background     (call :paint/context-set-background {:r 0 :g 0 :b 0 :a 0.25})
   :context-set-line-width     (call :paint/context-set-line-width 3)
   :edit-fill                  (call :paint/edit-fill layer :foreground)
   :edit-fill-white            (call :paint/edit-fill 'bg :white)
   :edit-fill-colour           (call :paint/edit-fill-colour layer "white")
   :edit-fill-colour-rgba      (call :paint/edit-fill-colour 'bg {:r 0.1 :g 0.2 :b 0.3})
   :edit-stroke-selection      (call :paint/edit-stroke-selection layer {})
   :edit-stroke-selection-full (call :paint/edit-stroke-selection layer {:colour "red" :width 2.5})
   :edit-stroke-item           (call :paint/edit-stroke-item layer path {:width 4})
   :edit-stroke-item-colour    (call :paint/edit-stroke-item 'bg 'p {:colour {:r 1 :g 1 :b 1}})})

(deftest every-paint-verb-has-a-golden-case
  (is (empty? (s/uncovered "paint" cases))))

(def ^:private one-shot
  #{:paint/edit-fill-colour :paint/edit-stroke-selection :paint/edit-stroke-item})

(defspec one-shot-settings-are-pushed-and-popped 200
  (prop/for-all [c (s/gen-call "paint")]
    (let [source (py/->python (verbs/program-of c))]
      (or (not (one-shot (:verb c)))
          (and (str/includes? source "Gimp.context_push()")
               (str/includes? source "finally:\n    Gimp.context_pop()"))))))

(deftrifecta paint-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/paint.edn"
   :cases         cases
   :xf            py/->python
   :gen           (s/gen-call "paint")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     200
   :mutations
   [["get-foreground reads background" (s/on :paint/context-get-foreground #(s/original (assoc % :verb :paint/context-get-background)))]
    ["get-background reads foreground" (s/on :paint/context-get-background #(s/original (assoc % :verb :paint/context-get-foreground)))]
    ["set-foreground sets background"  (s/on :paint/context-set-foreground #(s/original (assoc % :verb :paint/context-set-background)))]
    ["set-background is a no-op"       (s/drop-forms :paint/context-set-background 'Gimp/context-set-background)]
    ["rgba drops alpha"                (s/with-args :paint/context-set-background (fn [[c]] [(if (map? c) (dissoc c :a) c)]))]
    ["line width doubled"              (s/with-args :paint/context-set-line-width (fn [[w]] [(* 2 w)]))]
    ["fill always foreground"          (s/with-args :paint/edit-fill (fn [[d _]] [d :foreground]))]
    ["fill-colour leaks the context"   (s/on :paint/edit-fill-colour #(vec (mapcat (fn [f] (if (and (seq? f) (= 'try (first f)))
                                                                                            (remove (fn [g] (and (seq? g) (= 'finally (first g)))) (rest f))
                                                                                            [f]))
                                                                               (s/original %))))]
    ["fill-colour ignores the colour"  (s/with-args :paint/edit-fill-colour (fn [[d _]] [d "black"]))]
    ["stroke ignores width"            (s/with-args :paint/edit-stroke-selection (fn [[d o]] [d (dissoc o :width)]))]
    ["stroke ignores colour"           (s/with-args :paint/edit-stroke-selection (fn [[d o]] [d (dissoc o :colour)]))]
    ["stroke-item strokes selection"   (s/on :paint/edit-stroke-item #(let [[d _ o] (:args %)] (s/original (call :paint/edit-stroke-selection d o))))]]})
