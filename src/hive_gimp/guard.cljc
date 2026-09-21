(ns hive-gimp.guard
  "PIPELINE. Pure decisions that stop a reference-plug-in defect from reaching
   the caller as a plausible-looking answer.

   Two behaviours of the Python plug-in are compensated here, and both share a
   shape: the response alone cannot show that anything went wrong.

     FONT   `_resolve_font` falls back through an alias chain to
            `Sans-serif`, then to the first font in the list, and the
            `add_text` response never echoes the font it used. A caller asking
            for a font GIMP does not have is told `success` and gets a layer in
            another face.

     CANVAS `_new_canvas` builds the image, inserts and fills the layer, and
            only then calls `Gimp.Display.new`, which answers NULL headless.
            The blanket `except` reports failure for a fully formed image whose
            id is never returned: a false failure and an orphan at once.

   Every function here is a decision over values. The round trips that gather
   those values live in `hive-gimp.client`, which is the only namespace allowed
   to talk to a transport.

   Portable (.cljc): the same judgements are wanted in the native plug-in,
   which answers on cljrs."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Fonts
;; =============================================================================

(def font-parameter
  "The parameter name a command uses for a font, in the hive vocabulary."
  "font")

(defn takes-a-font?
  "Whether `descriptor` declares a font parameter."
  [descriptor]
  (boolean (some #(= font-parameter (:name %)) (:params descriptor))))

(defn font-asked-for
  "The font the caller NAMED, or nil.

   Read off the raw arguments rather than the built command, because
   `command/->command` has already applied the descriptor's default by then and
   a default nobody typed must not be refused. `add_text` defaults to `Sans`,
   which GIMP 3.2 no longer resolves; refusing it would break every call that
   simply left the font out."
  [descriptor args]
  (when (takes-a-font? descriptor)
    (let [named (or (get args :font) (get args "font"))]
      (when (and (string? named) (not (str/blank? named)))
        named))))

(defn font-verdict
  "`:font/ok`, `:font/unknown` or `:font/unverifiable` for `wanted` against the
   font names GIMP reported.

   An empty or absent catalogue is `:font/unverifiable`, never `:font/unknown`.
   A gate whose universe is empty rejects every input, and \"I could not look\"
   is a different answer from \"it is not there\"."
  [wanted installed]
  (cond
    (empty? installed)                  :font/unverifiable
    (contains? (set installed) wanted)  :font/ok
    :else                               :font/unknown))

;; =============================================================================
;; The canvas the plug-in disowns
;; =============================================================================

(defn image-ids
  "The set of image ids in a `list_images` result."
  [images]
  (into #{} (keep #(get % "image_id")) images))

(defn adopted-canvas
  "The `new_canvas` value for an image the plug-in built and then disowned, or
   nil when there is nothing to adopt.

   `before` and `after` are `list_images` results, and nil for either means the
   count was never taken: no adoption then, because the alternative is to guess
   which image is ours.

   Adoption requires exactly one image to have appeared AND its size to match
   what was asked for. An unrelated image opened by someone else is the failure
   mode this guards against, and handing a caller the wrong image is worse than
   reporting the error the plug-in reported."
  [params before after]
  (when (and (some? before) (some? after))
    (let [known (image-ids before)
          fresh (remove #(contains? known (get % "image_id")) after)
          image (first fresh)]
      (when (and (= 1 (count fresh))
                 (= (get params "width") (get image "width"))
                 (= (get params "height") (get image "height")))
        {"image_id"       (get image "image_id")
         "image_index"    (get image "index")
         "width"          (get image "width")
         "height"         (get image "height")
         "color_mode"     (get params "color_mode")
         "fill"           (get params "fill")
         "resolution"     (get params "resolution")
         "display_opened" false
         "recovered"      true
         "note"           (str "The plug-in reported a failure after building this image: it opens a "
                               "display last, and there is none headless. The image is complete and is "
                               "reported here rather than left orphaned inside GIMP.")}))))

(def native-only-commands
  "Commands only the hive-gimp native plug-in answers.

   They exist because the reference `add_text` has no anchor: centring a line
   there needs a second call, and the plug-in publishes no way to move a layer
   once it is placed."
  #{"place_text" "place_image"})

(defn answered-by-another-plugin?
  "Whether this failure is the reference Python plug-in saying, badly, that it
   has never heard of the command.

   Its dispatch has no branch for these names, so they reach the exec fallback,
   which reads `params[\"args\"]` unguarded: the call dies as `KeyError: 'args'`
   with no mention of the command that was sent. Measured 2026-09-21 against
   GIMP 3.2.4.

   Narrow on purpose. The native plug-in refuses these same commands for real
   reasons, a font it does not have or an anchor it does not know, and those
   answers must reach the caller untouched."
  [outcome]
  (boolean (and (contains? native-only-commands (:command outcome))
                (or (= "'args'" (str/trim (str (:message outcome))))
                    (str/includes? (str (:detail outcome)) "KeyError: 'args'")))))
