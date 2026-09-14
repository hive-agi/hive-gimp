;; The native plug-in's portable core, run on every host that has to agree:
;;
;;   clojure -M dev/native_portability.cljc                          (JVM, with native/src on the path)
;;   cljw -cp native/src dev/native_portability.cljc                 (ClojureWasm)
;;   cljrs run --src-path native/src dev/native_portability.cljc     (clojurust)
;;
;; Every expected value is spelled out, not computed from the implementation:
;; a check derived from the code agrees with broken code just as happily.
;;
;; The whole script runs its checks 60 times. On clojurust a fn is compiled
;; after 50 calls and the compiled tier has answered differently from the
;; interpreter (identical?/true?/false? on booleans, measured 2026-09-13), so
;; one pass through the interpreted tier proves nothing about the compiled one.
;;
;; Failure exits non-zero by THROWING: cljrs cannot resolve System/exit.

(require '[clojure.string :as str]
         '[hive-gimp.plugin.color :as color]
         '[hive-gimp.plugin.dispatch :as dispatch]
         '[hive-gimp.plugin.fake :as fake]
         '[hive-gimp.plugin.json :as json])

(def failures (atom []))

(defn check [label expected actual]
  (when-not (= expected actual)
    (swap! failures conj [label expected actual])))

(defn run-checks []
  ;; json
  (check "emit object" "{\"a\":1,\"b\":[true,false,null],\"c\":\"x\\\"y\"}"
         (json/emit (array-map "a" 1 "b" [true false nil] "c" "x\"y")))
  (check "emit control char" "\"a\\u0001b\\n\"" (json/emit (str "a" (char 1) "b\n")))
  (check "emit keyword key" "{\"k\":\"v\"}" (json/emit {:k :v}))
  (check "emit double" "1.5" (json/emit 1.5))
  (check "parse object" {:ok {"type" "new_canvas" "params" {"width" 64 "fill" "red"}}}
         (json/parse "{\"type\":\"new_canvas\", \"params\": {\"width\": 64, \"fill\":\"red\"}}"))
  (check "parse numbers" {:ok [0 -12 1.5 2.0E3 true false nil]}
         (json/parse "[0,-12,1.5,2e3,true,false,null]"))
  (check "parse escapes" {:ok "a\"b\\c/\né"} (json/parse "\"a\\\"b\\\\c\\/\\n\\u00e9\""))
  (check "parse round-trips emit" {:ok {"s" "q\"\n" "n" [1 2.5]}}
         (json/parse (json/emit {"s" "q\"\n" "n" [1 2.5]})))
  (check "parse trailing" "trailing input" (:error (json/parse "{} x")))
  (check "parse bad number" true (some? (:error (json/parse "[01]"))))
  (check "parse unterminated" true (some? (:error (json/parse "{\"a\":\"b"))))
  (check "complete-object? prefix" false (json/complete-object? "{\"a\":1"))
  (check "complete-object? array" false (json/complete-object? "[1]"))
  (check "complete-object? object" true (json/complete-object? "{\"a\":[1]}"))
  ;; color
  (check "named" "#ffa500" (color/normalize "Orange"))
  (check "named count" 148 (count color/named))
  (check "hex3" "#aabbcc" (color/normalize "#abc"))
  (check "hex8 drops alpha" "#112233" (color/normalize "#112233ff"))
  (check "rgb 0-255" "#008000" (color/normalize "rgb(0, 128, 0)"))
  (check "rgb percent" "#ff8000" (color/normalize "rgb(100%,50%,0%)"))
  (check "unknown refused" nil (color/normalize "not-a-colour"))
  (check "out of range refused" nil (color/normalize "rgb(256,0,0)"))
  ;; dispatch over the fake port
  (let [p   (fake/port)
        ctx {:port 9878}
        run (fn [t params] (dispatch/handle p ctx {"type" t "params" params}))]
    (check "no image" {"status" "error" "error" "No images are open in GIMP"} (run "list_layers" {}))
    (check "canvas" "success" (get (run "new_canvas" {"width" 64 "height" 48 "fill" "orange"}) "status"))
    (check "canvas fill normalised" "#ffa500" (get-in @(:state p) [:layer 2 :fill]))
    (check "bad colour refused" "error" (get (run "fill_layer" {"color" "nope"}) "status"))
    (check "layer" {"layer_id" 3 "name" "top" "width" 64 "height" 48 "opacity" 50.0}
           (get (run "create_layer" {"name" "top" "opacity" 50}) "results"))
    (check "layer names top first" ["top" "Untitled"]
           (mapv #(get % "name") (get-in (run "list_layers" {}) ["results" "layers"])))
    (check "export flattened is a boolean" true
           (get-in (run "export_image" {"file_path" "/tmp/x.png"}) ["results" "flattened"]))
    (check "export keeps the open image" 1 (count (:images @(:state p))))
    (check "unknown command" "error" (get (run "nope" {}) "status"))
    (check "out of range index" "image_index 3 is out of range; 1 image(s) open"
           (get (run "list_layers" {"image_index" 3}) "error"))))

(dotimes [_ 60] (run-checks))

(if (empty? @failures)
  (println "native plug-in core: all checks passed (60 passes)")
  (do (doseq [[label expected actual] (distinct @failures)]
        (println "FAIL" label "expected" (pr-str expected) "got" (pr-str actual)))
      (throw (ex-info (str (count (distinct @failures)) " portability check(s) failed") {}))))
