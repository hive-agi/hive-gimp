(ns hive-gimp.verbs.display
  "PURE. Verbs over GIMP's displays: open a window on an image, and flush
   pending changes to every open display."
  (:require [hive-gimp.py :as py]
            [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn display-new
  "Open a display on `image`. Answers the display's `Ref` (type `Display`)."
  {:verb/args [:cat v/Handle] :verb/answers v/Ref}
  [image]
  (v/with-holes {?image (v/handle-form image)}
    (Gimp.Display/new ?image)))

(defn displays-flush
  "Redraw every open display with the changes made so far."
  {:verb/args [:cat] :verb/answers :nil}
  []
  (py/forms (Gimp/displays-flush)))

(catalog/register-ns! 'hive-gimp.verbs.display)
