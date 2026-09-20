(ns hive-gimp.plugin.dispatch
  "The GIMP-side command table: one wire request in, one wire response out.

   This is what the reference plug-in spends an 80-branch elif chain on. Here a
   command is a ROW in `commands`, and a row is a function of the request's
   params and a GIMP PORT. The port is a map of plain functions over GIMP
   object ids (see `port-keys`); the native plug-in supplies it from
   `gimp.native/*` on clojurust, a test supplies a fake. Nothing in this
   namespace touches GIMP, a socket or JSON text, so the whole table runs on
   the JVM, ClojureWasm and clojurust alike.

   The wire vocabulary is the one hive-gimp's client already speaks
   (resources/hive_gimp/commands.edn): snake_case params, and responses shaped
   {\"status\" \"success\" \"results\" ...} or {\"status\" \"error\" \"error\" msg}.
   Result keys follow the reference plug-in's, so a caller cannot tell which
   plug-in answered except by asking `check_server`."
  (:require [clojure.string :as str]
            [hive-gimp.plugin.color :as color]))

(def port-keys
  "The functions a GIMP port must provide. Ids are GIMP's integer object ids."
  #{:version :image-ids :image-width :image-height :image-base-type :image-layer-ids
    :image-file :image-new :image-duplicate :image-flatten :image-delete
    :image-scale :image-crop :image-rotate :image-flip :image-remove-layer
    :image-set-resolution :image-resolution
    :layer-new :layer-insert :layer-name :layer-visible? :layer-opacity :layer-add-alpha
    :layer-copy :layer-set-opacity :layer-set-offsets :layer-scale :item-set-name :item-set-visible
    :drawable-width :drawable-height :drawable-has-alpha? :drawable-offsets
    :fill-color :fill-transparent :gradient-fill
    :font-name :text-layer-new :text-layer-style
    :display-new :displays-flush :file-save :file-load :file-load-layer})

(defn- call
  [port k & args]
  (if-let [f (get port k)]
    (apply f args)
    (throw (ex-info (str "the GIMP port provides no " (name k)) {}))))

(defn- fail [msg] (throw (ex-info msg {::user true})))

;; ---------------------------------------------------------------------------
;; Param coercion. A JSON number may arrive as a long or a double, and a
;; client may send a numeric string; the reference plug-in int()s everything.

(defn- ->long
  [v default]
  (cond
    (nil? v) default
    (integer? v) v
    (number? v) (long v)
    (string? v) (or (parse-long v) (fail (str "not an integer: " v)))
    :else (fail (str "not an integer: " (pr-str v)))))

(defn- ->double
  [v default]
  (cond
    (nil? v) default
    (number? v) (double v)
    (string? v) (or (parse-double v) (fail (str "not a number: " v)))
    :else (fail (str "not a number: " (pr-str v)))))

(defn- param [params k default] (let [v (get params k)] (if (nil? v) default v)))

(def ^:private base-type-names {0 "RGB" 1 "Grayscale" 2 "Indexed"})

(defn- image-id-at
  "GIMP's id for the image at INDEX in the open-image list, or a user error."
  [port index]
  (let [ids (vec (call port :image-ids))]
    (cond
      (empty? ids) (fail "No images are open in GIMP")
      (not (< -1 index (count ids)))
      (fail (str "image_index " index " is out of range; " (count ids) " image(s) open"))
      :else (nth ids index))))

(defn- basename [path] (last (str/split (str path) #"/")))

;; ---------------------------------------------------------------------------
;; Commands

(defn- image-summary
  [port index id]
  (let [file (call port :image-file id)]
    {"index"      index
     "image_id"   id
     "name"       (if file (basename file) (str "Untitled_" index))
     "width"      (call port :image-width id)
     "height"     (call port :image-height id)
     "color_mode" (get base-type-names (call port :image-base-type id) "Unknown")
     "num_layers" (count (call port :image-layer-ids id))
     "file_path"  (or file "Untitled")}))

(defn- list-images
  [port _ _]
  (let [images (vec (map-indexed #(image-summary port %1 %2) (call port :image-ids)))]
    {"images" images "count" (count images)}))

(defn- colour!
  "COLOUR as #rrggbb, or a user error naming what is accepted."
  [colour]
  (or (color/normalize colour)
      (fail (str "Not a colour: " (pr-str colour)
                 ". Use a CSS colour name, #rgb, #rrggbb, or rgb(r, g, b) with 0-255 channels."))))

(defn- transparent? [v] (= "transparent" (str/lower-case (str/trim (str v)))))

(defn- fill!
  "Fill drawable ID with FILL: \"transparent\", or a colour `color/normalize`
   accepts. The colour reaches GIMP as #rrggbb, because GEGL itself paints an
   unknown name transparent cyan and reads rgb() channels as 0..1, both while
   reporting success."
  [port id fill]
  (if (transparent? fill)
    (do (when-not (call port :drawable-has-alpha? id) (call port :layer-add-alpha id))
        (call port :fill-transparent id))
    (let [hex (colour! fill)]
      (when-not (call port :fill-color id hex)
        (fail (str "GIMP could not fill with " (pr-str fill)))))))

(defn- new-canvas
  "RESOLUTION (dpi, both axes) is stored on the image, so an exported PNG
   carries it as pHYs."
  [port params _]
  (let [width      (->long (param params "width" 1024) 1024)
        height     (->long (param params "height" 1024) 1024)
        name       (str (param params "name" "Untitled"))
        mode       (str/upper-case (str (param params "color_mode" "RGB")))
        fill       (param params "fill" "white")
        resolution (->double (get params "resolution") nil)
        gray?      (contains? #{"GRAY" "GRAYA"} mode)
        _          (when (and resolution (not (< 0 resolution 65536)))
                     (fail (str "resolution must be a positive dpi, got " resolution)))
        image      (call port :image-new width height (if gray? 1 0))
        layer      (call port :layer-new image name width height (if gray? 2 0) 100.0)]
    (when resolution
      (when-not (call port :image-set-resolution image resolution)
        (fail (str "GIMP refused the resolution " resolution))))
    (call port :layer-insert image layer 0)
    (fill! port layer fill)
    (let [display? (boolean (call port :display-new image))]
      (call port :displays-flush)
      {"image_id" image "width" width "height" height "color_mode" mode
       "fill" fill "resolution" (call port :image-resolution image) "display_opened" display?})))

(defn- layer-summary
  [port index id]
  {"index"     index
   "name"      (call port :layer-name id)
   "id"        id
   "visible"   (boolean (call port :layer-visible? id))
   "opacity"   (call port :layer-opacity id)
   "width"     (call port :drawable-width id)
   "height"    (call port :drawable-height id)
   "offsets"   (vec (call port :drawable-offsets id))
   "has_alpha" (boolean (call port :drawable-has-alpha? id))})

(defn- list-layers
  [port params _]
  (let [image  (image-id-at port (->long (get params "image_index") 0))
        layers (vec (map-indexed #(layer-summary port %1 %2) (call port :image-layer-ids image)))]
    {"layers" layers "count" (count layers)}))

(defn- create-layer
  [port params _]
  (let [image    (image-id-at port (->long (get params "image_index") 0))
        name     (str (param params "name" "New Layer"))
        width    (->long (get params "width") (call port :image-width image))
        height   (->long (get params "height") (call port :image-height image))
        opacity  (->double (get params "opacity") 100.0)
        position (->long (get params "position") -1)
        layer    (call port :layer-new image name width height 1 opacity)]
    (call port :layer-insert image layer (if (neg? position) 0 position))
    (fill! port layer (param params "fill" "transparent"))
    (call port :displays-flush)
    {"layer_id" layer "name" name "width" width "height" height "opacity" opacity}))

(defn- fill-layer
  [port params _]
  (let [image  (image-id-at port (->long (get params "image_index") 0))
        layers (vec (call port :image-layer-ids image))
        wanted (get params "layer_name")
        layer  (if wanted
                 (or (some #(when (= wanted (call port :layer-name %)) %) layers)
                     (fail (str "No layer named " (pr-str wanted))))
                 (or (first layers) (fail "The image has no layers")))]
    (fill! port layer (or (get params "color") (fail "fill_layer requires color")))
    (call port :displays-flush)
    {"layer" (call port :layer-name layer) "color" (get params "color")}))

(defn- image-metadata
  [port _ _]
  (let [id (image-id-at port 0)]
    (assoc (image-summary port 0 id)
           "layers" (vec (map-indexed #(layer-summary port %1 %2) (call port :image-layer-ids id))))))

(defn- export-image
  "Export through a flattened DUPLICATE, so the open image keeps its layers.
   Exporting the image itself would merge the caller's layers as a side
   effect of asking for a file."
  [port params _]
  (let [path   (or (get params "file_path") (fail "export_image requires file_path"))
        image  (image-id-at port (->long (get params "image_index") 0))
        ;; `=`, not `false?`: see hive-gimp.plugin.json/emit.
        flat?  (not (= false (get params "flatten")))
        target (if flat? (call port :image-duplicate image) image)]
    (try
      (when flat? (call port :image-flatten target))
      (when-not (call port :file-save target path)
        (fail (str "GIMP could not export to " path)))
      {"file_path" path "width" (call port :image-width target)
       "height" (call port :image-height target) "flattened" flat?}
      (finally
        (when flat? (call port :image-delete target))))))

(defn- close-image
  [port params _]
  (let [image (image-id-at port (->long (get params "image_index") 0))]
    (if (call port :image-delete image)
      {"closed" image}
      (fail (str "GIMP refused to close image " image)))))

;; ---------------------------------------------------------------------------
;; Files and whole-image transforms

(defn- open-image
  [port params _]
  (let [path  (or (get params "file_path") (fail "open_image requires file_path"))
        image (call port :file-load path)]
    (call port :display-new image)
    (call port :displays-flush)
    {"image_id" image "file_path" path
     "width" (call port :image-width image) "height" (call port :image-height image)
     "num_layers" (count (call port :image-layer-ids image))}))

(defn- save-xcf
  [port params _]
  (let [path  (or (get params "file_path") (fail "save_xcf requires file_path"))
        _     (when-not (str/ends-with? (str/lower-case path) ".xcf")
                (fail (str "save_xcf writes XCF; file_path must end in .xcf, got " (pr-str path))))
        image (image-id-at port (->long (get params "image_index") 0))]
    (when-not (call port :file-save image path)
      (fail (str "GIMP could not save " path)))
    {"file_path" path "image_id" image}))

(defn- dimensions [port image]
  {"width" (call port :image-width image) "height" (call port :image-height image)})

(defn- scale-image
  [port params _]
  (let [image  (image-id-at port (->long (get params "image_index") 0))
        width  (->long (get params "width") nil)
        height (->long (get params "height") nil)]
    (when-not (and width height (pos? width) (pos? height))
      (fail "scale_image requires positive width and height"))
    (when-not (call port :image-scale image width height)
      (fail (str "GIMP could not scale to " width "x" height)))
    (call port :displays-flush)
    (dimensions port image)))

(defn- crop-to-rect
  [port params _]
  (let [image  (image-id-at port (->long (get params "image_index") 0))
        [x y w h] (map #(->long (get params %) nil) ["x" "y" "width" "height"])
        iw     (call port :image-width image)
        ih     (call port :image-height image)]
    (when-not (and x y w h (<= 0 x) (<= 0 y) (pos? w) (pos? h) (<= (+ x w) iw) (<= (+ y h) ih))
      (fail (str "crop rectangle " [x y w h] " is not inside the " iw "x" ih " image")))
    (when-not (call port :image-crop image w h x y)
      (fail "GIMP refused the crop"))
    (call port :displays-flush)
    (dimensions port image)))

(defn- rotate-image
  "Quarter turns only: GIMP's image rotation takes 90, 180 or 270 degrees, and
   an arbitrary angle is a per-layer transform with interpolation and a new
   canvas size, which this plug-in does not pretend to do."
  [port params _]
  (let [image (image-id-at port (->long (get params "image_index") 0))
        angle (->double (get params "angle") nil)
        turn  (when angle (mod (long angle) 360))
        kind  (get {90 0 180 1 270 2} turn)]
    (cond
      (nil? angle) (fail "rotate_image requires angle")
      (not (== angle (long angle))) (fail (str "rotate_image supports multiples of 90 degrees, got " angle))
      (= 0 turn) (dimensions port image)
      (nil? kind) (fail (str "rotate_image supports multiples of 90 degrees, got " angle))
      :else (do (when-not (call port :image-rotate image kind) (fail "GIMP refused the rotation"))
                (call port :displays-flush)
                (dimensions port image)))))

(defn- flip-image
  [port params _]
  (let [image     (image-id-at port (->long (get params "image_index") 0))
        direction (str/lower-case (str (param params "direction" "horizontal")))
        kind      (get {"horizontal" 0 "vertical" 1} direction)]
    (when-not kind (fail (str "direction must be horizontal or vertical, got " (pr-str direction))))
    (when-not (call port :image-flip image kind) (fail "GIMP refused the flip"))
    (call port :displays-flush)
    {"direction" direction}))

(defn- flatten-image
  [port params _]
  (let [image (image-id-at port (->long (get params "image_index") 0))]
    (when-not (call port :image-flatten image) (fail "GIMP could not flatten the image"))
    (call port :displays-flush)
    {"num_layers" (count (call port :image-layer-ids image))}))

;; ---------------------------------------------------------------------------
;; Layers

(defn- layer-at
  "The layer PARAMS name: by NAME-KEY, else by INDEX-KEY (0 = top), else the
   top layer. A name or index that matches nothing is a user error, never a
   silent fall back to the top layer."
  [port image params name-key index-key]
  (let [layers (vec (call port :image-layer-ids image))
        wanted (get params name-key)
        index  (->long (get params index-key) nil)]
    (cond
      (empty? layers) (fail "The image has no layers")
      wanted (or (some #(when (= wanted (call port :layer-name %)) %) layers)
                 (fail (str "No layer named " (pr-str wanted))))
      index (if (< -1 index (count layers))
              (nth layers index)
              (fail (str "layer_index " index " is out of range; " (count layers) " layer(s)")))
      :else (first layers))))

(defn- delete-layer
  [port params _]
  (let [image (image-id-at port (->long (get params "image_index") 0))
        layer (layer-at port image params "layer_name" "layer_index")
        name  (call port :layer-name layer)]
    (when-not (call port :image-remove-layer image layer) (fail (str "GIMP could not remove " (pr-str name))))
    (call port :displays-flush)
    {"deleted" name "num_layers" (count (call port :image-layer-ids image))}))

(defn- rename-layer
  [port params _]
  (let [image    (image-id-at port (->long (get params "image_index") 0))
        new-name (or (get params "new_name") (fail "rename_layer requires new_name"))
        layer    (layer-at port image params "old_name" "layer_index")
        old-name (call port :layer-name layer)]
    (when-not (call port :item-set-name layer (str new-name)) (fail "GIMP refused the rename"))
    {"old_name" old-name "new_name" (call port :layer-name layer)}))

(defn- duplicate-layer
  "The copy goes directly above the original. Position is found by a scan, not
   `.indexOf`, which is JVM interop and does not exist on clojurust or cljw."
  [port params _]
  (let [image  (image-id-at port (->long (get params "image_index") 0))
        layer  (layer-at port image params "layer_name" "layer_index")
        copy   (call port :layer-copy layer)
        above  (or (first (keep-indexed (fn [i id] (when (= id layer) i))
                                        (call port :image-layer-ids image)))
                   0)]
    (call port :layer-insert image copy above)
    (call port :displays-flush)
    {"layer_id" copy "name" (call port :layer-name copy)}))

(defn- set-layer-properties
  "opacity (0-100) and visible. blend_mode is refused unless NORMAL: GIMP's
   layer mode enum has ~60 members and a wrong mapping would be a silent
   wrong picture."
  [port params _]
  (let [image   (image-id-at port (->long (get params "image_index") 0))
        layer   (layer-at port image params "layer_name" "layer_index")
        opacity (->double (get params "opacity") nil)
        visible (get params "visible")
        mode    (get params "blend_mode")]
    (when (and mode (not= "NORMAL" (str/upper-case (str mode))))
      (fail (str "blend_mode " (pr-str mode) " is not supported by the native plug-in; only NORMAL")))
    (when opacity
      (when-not (<= 0 opacity 100) (fail (str "opacity must be 0-100, got " opacity)))
      (call port :layer-set-opacity layer opacity))
    (when (some? visible)
      (call port :item-set-visible layer (= true visible)))
    (call port :displays-flush)
    (layer-summary port 0 layer)))

;; ---------------------------------------------------------------------------
;; Composition: text, gradients, placed images

(def ^:private anchor-halves
  "Anchor name -> [kx ky]: the anchor point sits at k/2 of the box on each axis.
   Halves, so an origin is integer arithmetic on every host."
  {"top-left"    [0 0] "top"    [1 0] "top-right"    [2 0]
   "left"        [0 1] "center" [1 1] "right"        [2 1]
   "bottom-left" [0 2] "bottom" [1 2] "bottom-right" [2 2]})

(defn- anchor!
  "The [kx ky] of ANCHOR, or a user error listing the accepted names."
  [anchor]
  (or (get anchor-halves (str/lower-case (str/trim (str anchor))))
      (fail (str "anchor must be one of " (str/join ", " (sort (keys anchor-halves)))
                 ", got " (pr-str anchor)))))

(defn anchor-origin
  "The top-left corner that puts the anchor point [kx ky] of a W x H box at (X, Y)."
  [[kx ky] x y w h]
  [(- x (quot (* w kx) 2)) (- y (quot (* h ky) 2))])

(def ^:private justifications {"left" 0 "right" 1 "center" 2 "fill" 3})

(defn- place-text
  "A text layer, placed by ANCHOR at (x, y). The font is resolved by exact GIMP
   name and REFUSED when absent: the reference plug-in substitutes Sans-serif
   and reports success, so a typo renders as a different typeface."
  [port params _]
  (let [image   (image-id-at port (->long (get params "image_index") 0))
        text    (let [t (get params "text")]
                  (if (or (nil? t) (= "" (str t))) (fail "place_text requires non-empty text") (str t)))
        wanted  (str (param params "font" "Sans-serif"))
        size    (->double (get params "size") 24.0)
        hex     (colour! (param params "color" "black"))
        justify (let [j (str/lower-case (str (param params "justify" "left")))]
                  (or (get justifications j)
                      (fail (str "justify must be left, center, right or fill, got " (pr-str j)))))
        k       (anchor! (param params "anchor" "top-left"))
        x       (->long (get params "x") 0)
        y       (->long (get params "y") 0)
        letter  (->double (get params "letter_spacing") 0.0)
        line    (->double (get params "line_spacing") 0.0)
        _       (when-not (pos? size) (fail (str "size must be positive, got " size)))
        font    (or (call port :font-name wanted)
                    (fail (str "No font named " (pr-str wanted) " is installed in GIMP; list_fonts shows"
                               " the names it knows. A missing font is refused, not substituted.")))
        layer   (call port :text-layer-new image text font size)]
    (call port :layer-insert image layer 0)
    (when-not (call port :text-layer-style layer hex justify letter line)
      (fail "GIMP refused the text style"))
    (let [w       (call port :drawable-width layer)
          h       (call port :drawable-height layer)
          [ox oy] (anchor-origin k x y w h)]
      (call port :layer-set-offsets layer ox oy)
      (when-let [n (get params "name")] (call port :item-set-name layer (str n)))
      (call port :displays-flush)
      {"layer_id" layer "layer_name" (call port :layer-name layer) "font" font "size" size
       "color" hex "x" ox "y" oy "text_width" w "text_height" h})))

(defn- add-text
  "The reference contract's add_text, on place_text: top-left at (x, y)."
  [port params ctx]
  (let [r (place-text port (dissoc params "anchor" "justify" "name") ctx)]
    (assoc r "position" [(get r "x") (get r "y")])))

(def ^:private gradient-kinds {"linear" 0 "radial" 2})

(defn- gradient-fill
  "A two-colour gradient over a layer, from (x1, y1) to (x2, y2) in the layer's
   own pixels; x2/y2 default to its far corner. Either colour may be
   \"transparent\": color2 fades color1 out, color1 fades color2 in (the layer
   gains alpha). Linear or radial only."
  [port params _]
  (let [image (image-id-at port (->long (get params "image_index") 0))
        kind  (let [t (str/lower-case (str (param params "gradient_type" "linear")))]
                (or (get gradient-kinds t)
                    (fail (str "gradient_type must be linear or radial, got " (pr-str t)))))
        c1    (let [c (param params "color1" "black")] (if (transparent? c) "transparent" (colour! c)))
        c2    (let [c (param params "color2" "white")] (if (transparent? c) "transparent" (colour! c)))
        layer (layer-at port image params "layer_name" "layer_index")
        w     (call port :drawable-width layer)
        h     (call port :drawable-height layer)
        x1    (->double (get params "x1") 0.0)
        y1    (->double (get params "y1") 0.0)
        x2    (->double (get params "x2") (double w))
        y2    (->double (get params "y2") (double h))]
    (when (and (= "transparent" c1) (= "transparent" c2))
      (fail "a gradient needs one colour: color1 and color2 are both transparent"))
    (when (and (or (= "transparent" c1) (= "transparent" c2)) (not (call port :drawable-has-alpha? layer)))
      (call port :layer-add-alpha layer))
    (when-not (call port :gradient-fill layer kind c1 c2 x1 y1 x2 y2)
      (fail "GIMP refused the gradient"))
    (call port :displays-flush)
    {"layer" (call port :layer-name layer) "gradient_type" (if (= 2 kind) "radial" "linear")
     "color1" c1 "color2" c2 "from" [x1 y1] "to" [x2 y2]}))

(defn- place-image
  "A file loaded as a NEW layer of an open image, optionally scaled to width
   and/or height (one alone keeps the aspect ratio), placed by ANCHOR at (x, y)."
  [port params _]
  (let [image   (image-id-at port (->long (get params "image_index") 0))
        path    (or (get params "file_path") (fail "place_image requires file_path"))
        k       (anchor! (param params "anchor" "top-left"))
        x       (->long (get params "x") 0)
        y       (->long (get params "y") 0)
        width   (->long (get params "width") nil)
        height  (->long (get params "height") nil)
        opacity (->double (get params "opacity") nil)]
    (when (or (and width (not (pos? width))) (and height (not (pos? height))))
      (fail "place_image width and height must be positive"))
    (when (and opacity (not (<= 0 opacity 100)))
      (fail (str "opacity must be 0-100, got " opacity)))
    (let [layer (call port :file-load-layer image path)
          w0    (call port :drawable-width layer)
          h0    (call port :drawable-height layer)
          [w h] (cond (and width height) [width height]
                      width  [width (max 1 (quot (* h0 width) w0))]
                      height [(max 1 (quot (* w0 height) h0)) height]
                      :else  [w0 h0])]
      (call port :layer-insert image layer 0)
      (when (and (not= [w h] [w0 h0]) (not (call port :layer-scale layer w h)))
        (fail (str "GIMP could not scale the placed image to " w "x" h)))
      (let [[ox oy] (anchor-origin k x y w h)]
        (call port :layer-set-offsets layer ox oy)
        (when opacity (call port :layer-set-opacity layer opacity))
        (when-let [n (get params "name")] (call port :item-set-name layer (str n)))
        (call port :displays-flush)
        {"layer_id" layer "layer_name" (call port :layer-name layer) "file_path" path
         "x" ox "y" oy "width" w "height" h "source_width" w0 "source_height" h0}))))

(defn- check-server
  [_ _ ctx]
  {"running" true "port" (:port ctx) "implementation" "hive-gimp-native"})

(defn- gimp-info
  [port _ ctx]
  {"version" (call port :version) "implementation" "hive-gimp-native"
   "runtime" (:runtime ctx "clojurust") "port" (:port ctx)})

(def commands
  "Wire command name -> (fn [port params ctx] results). Adding a command is a
   row here and a descriptor in commands.edn, nothing else."
  {"check_server"         check-server
   "get_gimp_info"        gimp-info
   "list_images"          list-images
   "get_image_metadata"   image-metadata
   "new_canvas"           new-canvas
   "open_image"           open-image
   "save_xcf"             save-xcf
   "export_image"         export-image
   "close_image"          close-image
   "scale_image"          scale-image
   "crop_to_rect"         crop-to-rect
   "rotate_image"         rotate-image
   "flip_image"           flip-image
   "flatten_image"        flatten-image
   "create_layer"         create-layer
   "list_layers"          list-layers
   "fill_layer"           fill-layer
   "delete_layer"         delete-layer
   "rename_layer"         rename-layer
   "duplicate_layer"      duplicate-layer
   "set_layer_properties" set-layer-properties
   "add_text"             add-text
   "place_text"           place-text
   "gradient_fill"        gradient-fill
   "place_image"          place-image})

(defn handle
  "REQUEST (parsed wire map) -> response map. Never throws.

   A failure the command raised on purpose keeps its message; anything else is
   reported with the command name, because an unclassified error from inside
   GIMP is useless without knowing what was being asked."
  [port ctx request]
  (let [command (get request "type")
        params  (let [p (get request "params")] (if (map? p) p {}))]
    (if-let [f (get commands command)]
      (try
        {"status" "success" "results" (f port params ctx)}
        (catch Exception e
          {"status" "error"
           "error"  (if (::user (ex-data e))
                      (ex-message e)
                      (str command " failed: " (or (ex-message e) "unknown error")))}))
      {"status" "error"
       "error"  (str "Unknown command " (pr-str command) ". The native plug-in implements: "
                     (str/join ", " (sort (keys commands))))})))
