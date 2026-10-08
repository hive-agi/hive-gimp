(ns hive-gimp.verbs.paint
  "PURE. Verbs that set GIMP's context colours and line width and paint into
   a drawable: fill (with a fill type or a colour) and stroke (the selection
   or an item such as a path). Painting changes the drawable it is given, so
   give it a fresh layer (`hive-gimp.verbs.layer/new-layer`). A verb that
   needs a colour or width for one paint pushes and pops GIMP's context, so
   the GUI's own colours survive it."
  (:require [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def LineWidth [:or [:int {:min 1 :max 10000}] [:double {:min 0.1 :max 10000.0}]])

(def Stroke
  [:map {:closed true}
   [:colour {:optional true} v/Colour]
   [:width  {:optional true} LineWidth]])

(defn- rgba-of
  "Forms answering the `Rgba` of the `Gegl.Color` `colour` (a form)."
  [colour]
  (v/with-holes {?colour colour}
    (def hg-rgba (.get-rgba ?colour))
    {"r" (nth hg-rgba 0) "g" (nth hg-rgba 1) "b" (nth hg-rgba 2) "a" (nth hg-rgba 3)}))

(defn- in-context
  "`settings` then `paint` between a context push and pop."
  [settings paint]
  [(apply list 'try (concat '[(Gimp/context-push)] settings paint '[(finally (Gimp/context-pop))]))])

(defn- stroke-settings
  [{:keys [colour width]}]
  (cond-> []
    colour (into (v/colour-forms colour))
    colour (conj '(Gimp/context-set-foreground hg-colour))
    width  (conj (list 'Gimp/context-set-line-width (double width)))))

(defn context-get-foreground
  "The context's foreground colour."
  {:verb/args [:cat] :verb/answers v/Rgba}
  []
  (rgba-of '(Gimp/context-get-foreground)))

(defn context-get-background
  "The context's background colour."
  {:verb/args [:cat] :verb/answers v/Rgba}
  []
  (rgba-of '(Gimp/context-get-background)))

(defn context-set-foreground
  "Set the context's foreground colour. Answers it as GIMP stored it."
  {:verb/args [:cat v/Colour] :verb/answers v/Rgba}
  [colour]
  (-> (v/colour-forms colour)
      (conj '(Gimp/context-set-foreground hg-colour))
      (into (context-get-foreground))))

(defn context-set-background
  "Set the context's background colour. Answers it as GIMP stored it."
  {:verb/args [:cat v/Colour] :verb/answers v/Rgba}
  [colour]
  (-> (v/colour-forms colour)
      (conj '(Gimp/context-set-background hg-colour))
      (into (context-get-background))))

(defn context-set-line-width
  "Set the context's stroke line width, in pixels. Answers it."
  {:verb/args [:cat LineWidth] :verb/answers number?}
  [width]
  [(list 'Gimp/context-set-line-width (double width))
   '(Gimp/context-get-line-width)])

(defn edit-fill
  "Fill `drawable`'s selected area (all of it when nothing is selected) with
   `fill-type` (`v/fill-types`). Answers the drawable."
  {:verb/args [:cat v/Handle v/FillType] :verb/answers v/Ref}
  [drawable fill-type]
  (v/with-holes {?d (v/handle-form drawable) ?fill (v/enum-form v/fill-types fill-type)}
    (.edit-fill ?d ?fill)
    ?d))

(defn edit-fill-colour
  "Fill `drawable`'s selected area with `colour`, leaving the context's
   foreground as it was. Answers the drawable."
  {:verb/args [:cat v/Handle v/Colour] :verb/answers v/Ref}
  [drawable colour]
  (let [d (v/handle-form drawable)]
    (into (in-context (stroke-settings {:colour colour})
                      [(list '.edit-fill d 'Gimp.FillType/FOREGROUND)])
          [d])))

(defn edit-stroke-selection
  "Stroke the selection's outline on `drawable`, with `:colour` and `:width`
   when given (else the context's). Answers the drawable."
  {:verb/args [:cat v/Handle Stroke] :verb/answers v/Ref}
  [drawable opts]
  (let [d (v/handle-form drawable)]
    (into (in-context (stroke-settings opts) [(list '.edit-stroke-selection d)])
          [d])))

(defn edit-stroke-item
  "Stroke `item` (a path, or a layer's outline) on `drawable`, with `:colour`
   and `:width` when given. Answers the drawable."
  {:verb/args [:cat v/Handle v/Handle Stroke] :verb/answers v/Ref}
  [drawable item opts]
  (let [d (v/handle-form drawable)]
    (into (in-context (stroke-settings opts) [(list '.edit-stroke-item d (v/handle-form item))])
          [d])))

(catalog/register-ns! 'hive-gimp.verbs.paint)
