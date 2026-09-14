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

(let [[port png] *command-line-args*
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
  (check "close_image"
         some?
         (gimp/invoke g "close_image"))
  (check "no images left"
         #(= 0 (get % "count"))
         (gimp/invoke g "list_images"))
  (println (if (zero? @failures) "JVM client: all checks passed" (str "JVM client: " @failures " check(s) failed")))
  (shutdown-agents)
  (System/exit (if (zero? @failures) 0 1)))
