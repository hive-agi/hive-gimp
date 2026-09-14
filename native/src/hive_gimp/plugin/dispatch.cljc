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
    :layer-new :layer-insert :layer-name :layer-visible? :layer-opacity :layer-add-alpha
    :drawable-width :drawable-height :drawable-has-alpha? :fill-color :fill-transparent
    :display-new :displays-flush :file-save})

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

(defn- fill!
  "Fill drawable ID with FILL: \"transparent\", or a colour `color/normalize`
   accepts. The colour reaches GIMP as #rrggbb, because GEGL itself paints an
   unknown name transparent cyan and reads rgb() channels as 0..1, both while
   reporting success."
  [port id fill]
  (if (= "transparent" (str/lower-case (str/trim (str fill))))
    (do (when-not (call port :drawable-has-alpha? id) (call port :layer-add-alpha id))
        (call port :fill-transparent id))
    (let [hex (or (color/normalize fill)
                  (fail (str "Not a colour: " (pr-str fill)
                             ". Use a CSS colour name, #rgb, #rrggbb, or rgb(r, g, b) with 0-255 channels.")))]
      (when-not (call port :fill-color id hex)
        (fail (str "GIMP could not fill with " (pr-str fill)))))))

(defn- new-canvas
  [port params _]
  (let [width  (->long (param params "width" 1024) 1024)
        height (->long (param params "height" 1024) 1024)
        name   (str (param params "name" "Untitled"))
        mode   (str/upper-case (str (param params "color_mode" "RGB")))
        fill   (param params "fill" "white")
        gray?  (contains? #{"GRAY" "GRAYA"} mode)
        image  (call port :image-new width height (if gray? 1 0))
        layer  (call port :layer-new image name width height (if gray? 2 0) 100.0)]
    (call port :layer-insert image layer 0)
    (fill! port layer fill)
    (let [display? (boolean (call port :display-new image))]
      (call port :displays-flush)
      {"image_id" image "width" width "height" height "color_mode" mode
       "fill" fill "display_opened" display?})))

(defn- layer-summary
  [port index id]
  {"index"     index
   "name"      (call port :layer-name id)
   "id"        id
   "visible"   (boolean (call port :layer-visible? id))
   "opacity"   (call port :layer-opacity id)
   "width"     (call port :drawable-width id)
   "height"    (call port :drawable-height id)
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
  {"check_server"       check-server
   "get_gimp_info"      gimp-info
   "list_images"        list-images
   "get_image_metadata" image-metadata
   "new_canvas"         new-canvas
   "create_layer"       create-layer
   "list_layers"        list-layers
   "fill_layer"         fill-layer
   "export_image"       export-image
   "close_image"        close-image})

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
