(ns hive-gimp.wire
  "The plug-in socket protocol, portable: JSON text <-> data, framing, and the
   request/response envelopes, in plain Clojure over strings.

   `hive-gimp.codec` is the JVM face of the same protocol and stays on
   clojure.data.json, because the socket read loop asks `complete-frame?` on
   every chunk and a compiled parser is what that loop wants. This namespace is
   for hosts that have no JSON library: ClojureWasm (cljw) and clojurust
   (cljrs). The two are held to each other by hive-gimp.wire-test, a
   differential test against clojure.data.json, and the portable build is
   held to itself by dev/wire_portability.cljc, run on all three hosts.

   Contract
     parse           string -> {:ok value} | {:error message :at index}.
                     Objects -> maps with STRING keys; arrays -> vectors;
                     integers -> longs (doubles past the long range);
                     fractions and exponents -> doubles. Never throws.
     emit            value -> JSON text. Keyword/symbol keys and values emit
                     by name, nil is null, sets emit as arrays, non-finite
                     doubles emit as null. Non-ASCII is written raw, not as
                     \\u escapes.
     complete-frame? true only for exactly one JSON OBJECT (whitespace
                     around it allowed). nil, \"\" and any proper prefix are
                     not frames.
     encode-request  GimpCommand -> one request frame, newline-terminated.
                     Params are written verbatim, nulls included.
     decode-response frame -> hive-dsl Result as a plain map:
                     {:ok raw-response} | {:error reason :message .. :detail ..}
                     with reason one of :gimp/unparseable-response or
                     :gimp/malformed-response. Built literally, so this
                     namespace needs no hive-dsl on the host.

   Refused input, stated rather than hidden: a lone (unpaired) surrogate
   \\u escape is refused, as is a raw control character inside a string.

   Portable: clojure.core, clojure.string and hive-gimp.shape only; no reader
   conditionals, no host interop. Every scan works on one-char STRINGS,
   because the hosts disagree on what a char is."
  (:require [clojure.string :as str]
            [hive-gimp.shape :as shape]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Emit
;; =============================================================================

(def ^:private escapes
  {"\"" "\\\"" "\\" "\\\\" "\n" "\\n" "\r" "\\r" "\t" "\\t" "\b" "\\b" "\f" "\\f"})

(def ^:private hex "0123456789abcdef")

(defn- control-escape
  "\\u00XX for a code below 0x20."
  [code]
  (str "\\u00" (subs hex (quot code 16) (inc (quot code 16)))
       (subs hex (rem code 16) (inc (rem code 16)))))

(defn- escape-char
  [c]
  (let [c (str c)]
    (or (get escapes c)
        (let [code (int (first c))]
          (if (< code 0x20) (control-escape code) c)))))

(defn- escape-string
  [s]
  (str "\"" (apply str (map escape-char s)) "\""))

(defn- finite?
  "Whether double X has a JSON spelling. Decided on the printed form, which
   is the one test that agrees on every host."
  [x]
  (not (contains? #{"NaN" "##NaN" "Infinity" "-Infinity" "##Inf" "##-Inf"} (str x))))

(defn- key-name
  [k]
  (if (or (keyword? k) (symbol? k)) (name k) (str k)))

(defn emit
  "Value to JSON text. Booleans are tested with `=`, never `true?`/`false?`,
   which answer wrongly for booleans on clojurust once the caller is hot."
  [v]
  (cond
    (nil? v) "null"
    (= true v) "true"
    (= false v) "false"
    (string? v) (escape-string v)
    (keyword? v) (escape-string (name v))
    (symbol? v) (escape-string (name v))
    (integer? v) (str v)
    (number? v) (if (finite? v) (str (double v)) "null")
    (map? v) (str "{"
                  (str/join "," (map (fn [[k val]]
                                       (str (escape-string (key-name k)) ":" (emit val)))
                                     v))
                  "}")
    (or (sequential? v) (set? v)) (str "[" (str/join "," (map emit v)) "]")
    :else (escape-string (str v))))

;; =============================================================================
;; Parse
;;
;; Every reader is (fn [cs i] -> [value next-i] | failure), over the text as a
;; vector of one-char strings. A failure is a value (`fail`), never a throw.
;; =============================================================================

(defn- ch
  "The one-char string at I of the char vector CS, or nil past the end."
  [cs i]
  (get cs i))

(defn- fail
  "A reader's failure VALUE. Readers return [value next-i] or this map, and
   never throw: see `parse`."
  [i msg]
  {::error msg ::at i})

(defn- failed? [r] (map? r))

(def ^:private whitespace #{" " "\n" "\r" "\t"})

(defn- skip-ws
  [s i]
  (if (contains? whitespace (ch s i)) (recur s (inc i)) i))

(defn- span
  "The text of CS from I (inclusive) to J (exclusive), clamped to its end."
  [cs i j]
  (let [n (count cs)]
    (apply str (subvec cs (min n i) (min n j)))))

(defn- expect
  "[value next-i] when TOKEN is spelled at I, else a failure."
  [cs i token value]
  (if (= token (span cs i (+ i (count token))))
    [value (+ i (count token))]
    (fail i (str "expected " token))))

(def ^:private unescapes
  {"\"" "\"" "\\" "\\" "/" "/" "b" "\b" "f" "\f" "n" "\n" "r" "\r" "t" "\t"})

(defn- hex4
  "The four hex digits at I as a number, or nil."
  [cs i]
  (let [digits (span cs i (+ i 4))]
    (when (= 4 (count digits))
      (reduce (fn [acc c]
                (let [d (str/index-of hex (str/lower-case (str c)))]
                  (if (and acc d) (+ (* acc 16) d) (reduced nil))))
              0
              digits))))

(defn- high-surrogate? [code] (<= 0xD800 code 0xDBFF))
(defn- low-surrogate? [code] (<= 0xDC00 code 0xDFFF))

(defn- astral
  "The string for a code point above the BMP, given its surrogate pair. A host
   whose char is a Unicode scalar (clojurust) builds it from the code point; a
   host whose char is a UTF-16 unit (the JVM) refuses that and builds it from
   the two halves instead. Trying one then the other is what keeps this free
   of reader conditionals."
  [hi lo]
  (let [cp (+ 0x10000 (* (- hi 0xD800) 1024) (- lo 0xDC00))]
    (try
      (str (char cp))
      (catch Exception _
        (str (char hi) (char lo))))))

(defn- read-unicode-escape
  "Reads the \\uXXXX escape at I (pointing at the backslash), and its low
   surrogate partner when XXXX is a high surrogate. -> [string next-i]."
  [cs i]
  (let [code (hex4 cs (+ i 2))]
    (cond
      (nil? code) (fail i "bad \\u escape")

      (high-surrogate? code)
      (let [lo (when (= "\\u" (span cs (+ i 6) (+ i 8)))
                 (hex4 cs (+ i 8)))]
        (if (and lo (low-surrogate? lo))
          [(astral code lo) (+ i 12)]
          (fail i "unpaired surrogate \\u escape")))

      (low-surrogate? code) (fail i "unpaired surrogate \\u escape")
      :else [(str (char code)) (+ i 6)])))

(defn- control-char? [c] (< (int c) 0x20))

(defn- read-string*
  [cs i]
  (loop [i (inc i) acc []]
    (let [c (ch cs i)]
      (cond
        (nil? c) (fail i "unterminated string")
        (= c "\"") [(apply str acc) (inc i)]
        (= c "\\")
        (let [e (ch cs (inc i))]
          (cond
            (contains? unescapes e) (recur (+ i 2) (conj acc (get unescapes e)))
            (= e "u") (let [r (read-unicode-escape cs i)]
                        (if (failed? r)
                          r
                          (recur (second r) (conj acc (first r)))))
            :else (fail i "bad escape")))
        (control-char? (first c)) (fail i "control character in string")
        :else
        ;; Copy the plain run up to the next quote, backslash or control char.
        (let [end (loop [j (inc i)]
                    (let [d (ch cs j)]
                      (if (or (nil? d) (= d "\"") (= d "\\") (control-char? (first d)))
                        j
                        (recur (inc j)))))]
          (recur end (conj acc (span cs i end))))))))

(def ^:private number-chars (set (map str "0123456789+-.eE")))

(defn- read-number
  [cs i]
  (let [end (loop [j i] (if (contains? number-chars (ch cs j)) (recur (inc j)) j))
        tok (span cs i end)]
    (cond
      (= "" tok)
      (fail i (str "unexpected character " (pr-str (ch cs i))))

      (not (re-matches #"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?" tok))
      (fail i (str "bad number " (pr-str tok)))

      (re-matches #"-?(0|[1-9][0-9]*)" tok)
      [(or (parse-long tok) (parse-double tok)) end]

      :else [(parse-double tok) end])))

(declare read-value)

(defn- read-array
  [cs i]
  (let [i (skip-ws cs (inc i))]
    (if (= "]" (ch cs i))
      [[] (inc i)]
      (loop [i i acc []]
        (let [r (read-value cs i)]
          (if (failed? r)
            r
            (let [[v i] r
                  i     (skip-ws cs i)]
              (case (ch cs i)
                "," (recur (skip-ws cs (inc i)) (conj acc v))
                "]" [(conj acc v) (inc i)]
                (fail i "expected , or ]")))))))))

(defn- read-object
  [cs i]
  (let [i (skip-ws cs (inc i))]
    (if (= "}" (ch cs i))
      [{} (inc i)]
      (loop [i i acc {}]
        (if-not (= "\"" (ch cs i))
          (fail i "expected a string key")
          (let [kr (read-string* cs i)]
            (if (failed? kr)
              kr
              (let [[k i] kr
                    i     (skip-ws cs i)]
                (if-not (= ":" (ch cs i))
                  (fail i "expected :")
                  (let [vr (read-value cs (skip-ws cs (inc i)))]
                    (if (failed? vr)
                      vr
                      (let [[v i] vr
                            i     (skip-ws cs i)]
                        (case (ch cs i)
                          "," (recur (skip-ws cs (inc i)) (assoc acc k v))
                          "}" [(assoc acc k v) (inc i)]
                          (fail i "expected , or }"))))))))))))))

(defn- read-value
  [cs i]
  (let [i (skip-ws cs i)
        c (ch cs i)]
    (cond
      (nil? c) (fail i "unexpected end of input")
      (= c "{") (read-object cs i)
      (= c "[") (read-array cs i)
      (= c "\"") (read-string* cs i)
      (= c "t") (expect cs i "true" true)
      (= c "f") (expect cs i "false" false)
      (= c "n") (expect cs i "null" nil)
      :else (read-number cs i))))

(defn parse
  "JSON text to {:ok value} | {:error message :at index}. Never throws.
   The text is split once into a vector of one-char strings and every reader
   indexes that vector; `:at` counts those chars. Readers report failure as a
   value, never by throwing; the catch is only a backstop for a host error."
  [s]
  (if-not (string? s)
    {:error "not a string" :at 0}
    (try
      (let [cs (mapv str s)
            r  (read-value cs 0)]
        (if (failed? r)
          {:error (::error r) :at (::at r)}
          (let [i (skip-ws cs (second r))]
            (if (= i (count cs))
              {:ok (first r)}
              {:error "trailing input" :at i}))))
      (catch Exception e
        {:error (or (ex-message e) "malformed") :at 0}))))

;; =============================================================================
;; Framing
;; =============================================================================

(def request-terminator
  "Appended to every request frame. The plug-in accumulates until its buffer
   parses, so this is a courtesy rather than a delimiter."
  "\n")

(defn- ends-like-an-object?
  "Cheap necessary condition for a whole object: the last non-blank char is
   `}`. Lets a read loop skip the full parse for most partial buffers."
  [s]
  (str/ends-with? (str/trimr s) "}"))

(defn complete-frame?
  "True when `buffer` holds exactly one whole JSON object. A response has no
   length prefix and no terminator, so `parses as an object` IS the frame
   boundary; an object, not any value, so a truncated answer whose prefix
   happens to parse on its own is never mistaken for a whole one."
  [buffer]
  (and (string? buffer)
       (pos? (count buffer))
       (ends-like-an-object? buffer)
       (map? (:ok (parse buffer)))))

;; =============================================================================
;; Envelopes
;; =============================================================================

(defn encode-request
  "`GimpCommand` ({:type string :params {string any}}) to one request frame.
   Nil params are sent as null: the reference client sends an omitted
   optional that way and the plug-in reads it with `.get`."
  [command]
  (str (emit (array-map "type" (:type command) "params" (:params command)))
       request-terminator))

(defn- json-type
  [v]
  (cond
    (nil? v) "null"
    (string? v) "string"
    (number? v) "number"
    (or (= true v) (= false v)) "boolean"
    (sequential? v) "array"
    :else "value"))

(defn- clip
  [s n]
  (if (> (count s) n) (subs s 0 n) s))

(defn decode-response
  "One response frame to a hive-dsl Result, spelled as a plain map.

   Two failure reasons, kept distinct because they call for different actions:
   :gimp/unparseable-response (truncated frame, desynced socket, or something
   other than the plug-in answering) and :gimp/malformed-response (the plug-in
   answered in a shape this contract does not know)."
  [buffer]
  (let [{:keys [ok error at] :as parsed} (parse buffer)]
    (cond
      (contains? parsed :error)
      {:error   :gimp/unparseable-response
       :message "GIMP returned a frame that is not JSON."
       :detail  (str error " at " at ": " (if (string? buffer) (clip buffer 200) (pr-str buffer)))}

      (not (map? ok))
      {:error   :gimp/unparseable-response
       :message "GIMP returned JSON that is not an object."
       :detail  (json-type ok)}

      (not (shape/raw-response? ok))
      {:error   :gimp/malformed-response
       :message "GIMP returned an object without a usable status."
       :detail  (shape/explain-raw-response ok)}

      :else {:ok ok})))
