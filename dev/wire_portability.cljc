;; The portable wire codec (hive-gimp.wire), run on every host that has to agree:
;;
;;   clojure -M dev/wire_portability.cljc                             (JVM)
;;   cljw -cp src dev/wire_portability.cljc                           (ClojureWasm)
;;   cljrs run --src-path src dev/wire_portability.cljc               (clojurust)
;;
;; Same rules as dev/native_portability.cljc: every expected value is spelled
;; out rather than computed from the implementation, and failure exits non-zero
;; by THROWING, because cljrs cannot resolve System/exit. The checks run 60
;; times so clojurust's tiers are exercised, not only its interpreter: a fn is
;; lowered to IR after 50 calls and JIT-compiled after 1000, and the readers
;; inside `parse` cross both thresholds here. The compiled tier has answered
;; differently from the interpreter on this codec (see the README section).

(require '[hive-gimp.wire :as wire])

(def failures (atom []))

(defn check [label expected actual]
  (when-not (= expected actual)
    (swap! failures conj [label expected actual])))

(def smile
  "U+1F600, built from its UTF-8 spelling in the source file, so no host has
   to agree on how to build it from a number."
  "😀")

(defn run-checks []
  ;; emit
  (check "emit object" "{\"a\":1,\"b\":[true,false,null],\"c\":\"x\\\"y\"}"
         (wire/emit (array-map "a" 1 "b" [true false nil] "c" "x\"y")))
  (check "emit control char" "\"a\\u0001b\\n\\t\"" (wire/emit (str "a" (char 1) "b\n\t")))
  (check "emit keyword key and value" "{\"k\":\"v\"}" (wire/emit {:k :v}))
  (check "emit double" "1.5" (wire/emit 1.5))
  (check "emit negative long" "-42" (wire/emit -42))
  (check "emit set as array" "[1]" (wire/emit #{1}))
  (check "emit non-ascii raw" "\"é\"" (wire/emit "é"))
  (check "emit astral raw" (str "\"" smile "\"") (wire/emit smile))
  ;; parse
  (check "parse object" {:ok {"type" "new_canvas" "params" {"width" 64 "fill" "red"}}}
         (wire/parse "{\"type\":\"new_canvas\", \"params\": {\"width\": 64, \"fill\":\"red\"}}"))
  (check "parse numbers" {:ok [0 -12 1.5 2000.0 -0.25 true false nil]}
         (wire/parse "[0,-12,1.5,2e3,-2.5E-1,true,false,null]"))
  (check "parse long beyond range is a double" {:ok [1.0E20]} (wire/parse "[100000000000000000000]"))
  (check "parse escapes" {:ok "a\"b\\c/\né\t"} (wire/parse "\"a\\\"b\\\\c\\/\\n\\u00e9\\t\""))
  (check "parse surrogate pair" {:ok smile} (wire/parse "\"\\ud83d\\ude00\""))
  (check "parse surrogate pair upper hex" {:ok (str "x" smile "y")} (wire/parse "\"x\\uD83D\\uDE00y\""))
  (check "parse raw astral" {:ok smile} (wire/parse (str "\"" smile "\"")))
  (check "parse lone high surrogate" "unpaired surrogate \\u escape" (:error (wire/parse "\"\\ud83d\"")))
  (check "parse lone low surrogate" "unpaired surrogate \\u escape" (:error (wire/parse "\"\\ude00\"")))
  (check "parse whitespace around" {:ok {}} (wire/parse " \n\t{ } \r\n"))
  (check "parse empty containers" {:ok {"a" [] "b" {}}} (wire/parse "{\"a\":[],\"b\":{}}"))
  (check "parse duplicate key keeps last" {:ok {"a" 2}} (wire/parse "{\"a\":1,\"a\":2}"))
  (check "parse round-trips emit" {:ok {"s" "q\"\n" "n" [1 2.5] "u" smile}}
         (wire/parse (wire/emit {"s" "q\"\n" "n" [1 2.5] "u" smile})))
  (check "parse trailing" "trailing input" (:error (wire/parse "{} x")))
  (check "parse trailing at" 3 (:at (wire/parse "{} x")))
  (check "parse leading zero" true (some? (:error (wire/parse "[01]"))))
  (check "parse bare dot" true (some? (:error (wire/parse "[1.]"))))
  (check "parse plus sign" true (some? (:error (wire/parse "[+1]"))))
  (check "parse unterminated" "unterminated string" (:error (wire/parse "{\"a\":\"b")))
  (check "parse raw control char" "control character in string"
         (:error (wire/parse (str "\"a" (char 10) "b\""))))
  (check "parse trailing comma" true (some? (:error (wire/parse "[1,]"))))
  (check "parse single quotes" true (some? (:error (wire/parse "{'a':1}"))))
  (check "parse empty" "unexpected end of input" (:error (wire/parse "")))
  (check "parse nil" "not a string" (:error (wire/parse nil)))
  ;; framing
  (check "frame: object" true (wire/complete-frame? "{\"status\":\"success\"}"))
  (check "frame: object with trailing ws" true (wire/complete-frame? "{\"a\":[1]}\n"))
  (check "frame: prefix" false (wire/complete-frame? "{\"a\":1"))
  (check "frame: prefix ending in brace" false (wire/complete-frame? "{\"a\":{}"))
  (check "frame: array" false (wire/complete-frame? "[1]"))
  (check "frame: number" false (wire/complete-frame? "123"))
  (check "frame: empty" false (wire/complete-frame? ""))
  (check "frame: nil" false (wire/complete-frame? nil))
  (check "frame: two objects" false (wire/complete-frame? "{}{}"))
  ;; envelopes
  (check "encode request" "{\"type\":\"auto_levels\",\"params\":{\"layer_name\":null}}\n"
         (wire/encode-request {:type "auto_levels" :params {"layer_name" nil}}))
  (check "encode request parses back"
         {:ok {"type" "new_canvas" "params" {"width" 64 "fill" "red"}}}
         (wire/parse (wire/encode-request {:type "new_canvas" :params {"width" 64 "fill" "red"}})))
  (check "decode success" {:ok {"status" "success" "results" {"a" 1}}}
         (wire/decode-response "{\"status\":\"success\",\"results\":{\"a\":1}}"))
  (check "decode plugin error" {:ok {"status" "error" "error" "boom"}}
         (wire/decode-response "{\"status\":\"error\",\"error\":\"boom\"}"))
  (check "decode truncated" :gimp/unparseable-response (:error (wire/decode-response "{\"status\":")))
  (check "decode array" {:error :gimp/unparseable-response
                         :message "GIMP returned JSON that is not an object."
                         :detail "array"}
         (wire/decode-response "[1,2,3]"))
  (check "decode no status" :gimp/malformed-response (:error (wire/decode-response "{\"nope\":1}")))
  (check "decode weird status" "\"status\" was \"weird\", expected \"success\" or \"error\""
         (:detail (wire/decode-response "{\"status\":\"weird\"}"))))

(dotimes [_ 60] (run-checks))

(if (empty? @failures)
  (println "wire codec: all checks passed (60 passes)")
  (do (doseq [[label expected actual] (distinct @failures)]
        (println "FAIL" label "expected" (pr-str expected) "got" (pr-str actual)))
      (throw (ex-info (str (count (distinct @failures)) " wire portability check(s) failed") {}))))
