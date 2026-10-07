(ns hive-gimp.verbs.io
  "PURE. Verbs that move images between GIMP and files: load any format GIMP
   reads, save the `.xcf` (the editable source), export a `.png`. All run
   non-interactively."
  (:require [hive-gimp.verbs.catalog :as catalog]
            [hive-gimp.verbs.value :as v]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def Load [:map {:closed true} [:as {:optional true} v/PyName]])

(defn- save-forms
  "Forms saving `image` to `path` through `Gimp.file_save`, which picks the
   format from the extension. Answers GIMP's success flag."
  [image path]
  (v/with-holes {?image (v/handle-form image)}
    (import gi.repository [Gio])
    (Gimp/file-save Gimp.RunMode/NONINTERACTIVE ?image (Gio.File/new-for-path ~path) nil)))

(defn file-load
  "Open the image at `path`, bound to `:as` (default `hg-image`). Answers its
   `Ref`."
  {:verb/args [:cat v/FilePath Load] :verb/answers v/Ref}
  [path {:keys [as] :or {as 'hg-image}}]
  (v/with-holes {?as as}
    (import gi.repository [Gio])
    (def ?as (Gimp/file-load Gimp.RunMode/NONINTERACTIVE (Gio.File/new-for-path ~path)))
    ?as))

(defn save-xcf
  "Save `image`, every layer and path intact, as the `.xcf` at `path`."
  {:verb/args [:cat v/Handle v/XcfPath] :verb/answers :boolean}
  [image path]
  (save-forms image path))

(defn export-png
  "Export `image` as the `.png` at `path`. The image itself is unchanged."
  {:verb/args [:cat v/Handle v/PngPath] :verb/answers :boolean}
  [image path]
  (save-forms image path))

(catalog/register-ns! 'hive-gimp.verbs.io)
