;; The JVM half of the native plug-in's live gate: hive-gimp's OWN client,
;; unchanged, against the clojurust plug-in running inside a real GIMP.
;;
;; Run by dev/verify_native_plugin.sh, which starts GIMP and afterwards checks
;; the exported pixels with ffmpeg. On its own:
;;
;;   clojure -M dev/verify_native_plugin.clj <port> <export.png>
;;
;; The point is the word UNCHANGED: every call below goes through
;; hive-gimp.core, the catalog, the command builder, the codec and the socket
;; transport exactly as the addon's MCP tools do. If the native plug-in speaks
;; the wire contract differently from the Python one, this is where it shows.

(require '[hive-gimp.core :as gimp]
         '[hive-gimp.response :as response])

(def failures (atom 0))

(defn check [label pred outcome]
  (let [ok (and (response/ok? outcome) (pred (response/value outcome)))]
    (println (if ok "  OK  " "  FAIL") label (pr-str (if (response/ok? outcome) (response/value outcome) outcome)))
    (when-not ok (swap! failures inc))))

(let [[port png png2 xcf] *command-line-args*
      g (gimp/connect {:port (parse-long port)})]
  (println "== hive-gimp JVM client -> native plug-in on port" port)
  (check "check_server names the native implementation"
         #(= "hive-gimp-native" (get % "implementation"))
         (gimp/invoke g "check_server"))
  (check "get_gimp_info reports GIMP 3"
         #(.startsWith (str (get % "version")) "3.")
         (gimp/invoke g "get_gimp_info"))
  (check "new_canvas 320x200 filled with a CSS name GEGL cannot parse"
         #(= [320 200] [(get % "width") (get % "height")])
         (gimp/invoke g "new_canvas" {:width 320 :height 200 :name "hive" :fill "orange"}))
  (check "create_layer 100x80 filled with an rgb() GEGL misreads"
         #(= [100 80] [(get % "width") (get % "height")])
         (gimp/invoke g "create_layer" {:name "overlay" :width 100 :height 80 :fill "rgb(0, 128, 0)"}))
  (check "list_layers: overlay on top of the background"
         #(= ["overlay" "hive"] (mapv (fn [l] (get l "name")) (get % "layers")))
         (gimp/invoke g "list_layers"))
  (check "list_images: one image, two layers"
         #(= [1 2] [(get % "count") (get-in % ["images" 0 "num_layers"])])
         (gimp/invoke g "list_images"))
  (check "export_image writes the file through a flattened duplicate"
         #(= true (get % "flattened"))
         (gimp/invoke g "export_image" {:file-path png}))
  (check "the open image still has both layers after export"
         #(= 2 (get-in % ["images" 0 "num_layers"]))
         (gimp/invoke g "list_images"))
  (let [bad (gimp/invoke g "fill_layer" {:color "blurple"})]
    (println (if (response/ok? bad) "  FAIL" "  OK  ") "an unknown colour is refused, not painted cyan" (pr-str (:message bad)))
    (when (or (response/ok? bad) (not (re-find #"Not a colour" (str (:message bad)))))
      (swap! failures inc)))
  ;; Layer edits, then transforms whose effect on the overlay's position is
  ;; known exactly, so the shell can check the pixels of the second export.
  (check "duplicate_layer puts the copy above the original"
         #(= "overlay copy" (get % "name"))
         (gimp/invoke g "duplicate_layer" {:layer-name "overlay"}))
  (check "rename_layer by name"
         #(= "halo" (get % "new_name"))
         (gimp/invoke g "rename_layer" {:new-name "halo" :old-name "overlay copy"}))
  (check "set_layer_properties opacity 50, hidden"
         #(= [50.0 false] [(get % "opacity") (get % "visible")])
         (gimp/invoke g "set_layer_properties" {:layer-name "halo" :opacity 50 :visible false}))
  (check "delete_layer removes the copy"
         #(= ["halo" 2] [(get % "deleted") (get % "num_layers")])
         (gimp/invoke g "delete_layer" {:layer-name "halo"}))
  (check "save_xcf"
         #(= xcf (get % "file_path"))
         (gimp/invoke g "save_xcf" {:file-path xcf}))
  (check "rotate_image 90: 320x200 becomes 200x320"
         #(= {"width" 200 "height" 320} %)
         (gimp/invoke g "rotate_image" {:angle 90}))
  (check "flip_image horizontal"
         #(= "horizontal" (get % "direction"))
         (gimp/invoke g "flip_image" {:direction "horizontal"}))
  (check "crop_to_rect to the top-left 100x100"
         #(= {"width" 100 "height" 100} %)
         (gimp/invoke g "crop_to_rect" {:x 0 :y 0 :width 100 :height 100}))
  (check "scale_image to 50x50"
         #(= {"width" 50 "height" 50} %)
         (gimp/invoke g "scale_image" {:width 50 :height 50}))
  (check "export the transformed image"
         #(= [50 50] [(get % "width") (get % "height")])
         (gimp/invoke g "export_image" {:file-path png2}))
  (check "close_image"
         some?
         (gimp/invoke g "close_image"))
  (check "no images left"
         #(= 0 (get % "count"))
         (gimp/invoke g "list_images"))
  (check "open_image reads the exported PNG back in"
         #(= [320 200] [(get % "width") (get % "height")])
         (gimp/invoke g "open_image" {:file-path png}))
  (check "close the reopened image"
         some?
         (gimp/invoke g "close_image"))
  (println (if (zero? @failures) "JVM client: all checks passed" (str "JVM client: " @failures " check(s) failed")))
  (shutdown-agents)
  (System/exit (if (zero? @failures) 0 1)))
