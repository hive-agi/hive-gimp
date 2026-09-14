(ns hive-gimp.plugin.json
  "JSON text <-> data for the plug-in's socket wire, written by hand.

   The native plug-in runs on clojurust inside GIMP, where no JSON library
   exists, and the same codec is proven on ClojureWasm and the JVM, so it is
   plain Clojure over strings: clojure.core and clojure.string only, no reader
   conditionals, no host interop, no characters (every scan works on one-char
   STRINGS, because hosts disagree on what a char is).

   `parse`: string -> {:ok value} | {:error string :at int}.
     objects -> maps with STRING keys, the wire vocabulary is not keywordised;
     arrays -> vectors; integers -> longs; fractions/exponents -> doubles.
   `emit` : value -> string. Keyword and symbol keys emit by name. nil is null.

   Limits, stated rather than hidden: \\u escapes outside the BMP (surrogate
   pairs) are refused by parse, and non-finite doubles emit as null."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Emit

(def ^:private escapes
  {"\"" "\\\"" "\\" "\\\\" "\n" "\\n" "\r" "\\r" "\t" "\\t" "\b" "\\b" "\f" "\\f"})

(def ^:private hex "0123456789abcdef")

(defn- control-escape
  "\\u00XX for a code below 0x20."
  [code]
  (str "\\u00" (subs hex (quot code 16) (inc (quot code 16)))
       (subs hex (rem code 16) (inc (rem code 16)))))

(defn- escape-string
  [s]
  (str "\""
       (apply str
              (map (fn [c]
                     (let [c (str c)]
                       (or (get escapes c)
                           (let [code (int (first c))]
                             (if (< code 0x20) (control-escape code) c)))))
                   s))
       "\""))

(defn- finite?
  "Whether double X has a JSON spelling. Decided on the PRINTED form, because
   `(== x x)` is not a NaN test everywhere: clojurust answers true for NaN."
  [x]
  (not (contains? #{"NaN" "##NaN" "Infinity" "-Infinity" "##Inf" "##-Inf"} (str x))))

(defn emit
  "Booleans are tested with `=`, never `true?`/`false?`: on clojurust those
   go through `identical?`, which answers wrongly for booleans once the
   calling fn is hot (measured: wrong from the 51st call on)."
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
                  (str/join ","
                            (map (fn [[k val]]
                                   (str (escape-string (if (or (keyword? k) (symbol? k)) (name k) (str k)))
                                        ":" (emit val)))
                                 v))
                  "}")
    (sequential? v) (str "[" (str/join "," (map emit v)) "]")
    (set? v) (str "[" (str/join "," (map emit v)) "]")
    :else (escape-string (str v))))

;; ---------------------------------------------------------------------------
;; Parse
;;
;; Every reader is (fn [s i] -> [value next-i]) and throws ex-info carrying :at
;; on malformed input; `parse` is the only place that catches.

(defn- ch [s i] (when (< i (count s)) (subs s i (inc i))))

(defn- fail [i msg] (throw (ex-info msg {::at i})))

(def ^:private whitespace #{" " "\n" "\r" "\t"})

(defn- skip-ws
  [s i]
  (if (contains? whitespace (ch s i)) (recur s (inc i)) i))

(defn- expect
  [s i token]
  (if (= token (subs s i (min (count s) (+ i (count token)))))
    (+ i (count token))
    (fail i (str "expected " token))))

(def ^:private unescapes
  {"\"" "\"" "\\" "\\" "/" "/" "b" "\b" "f" "\f" "n" "\n" "r" "\r" "t" "\t"})

(defn- hex4
  [s i]
  (let [digits (subs s i (min (count s) (+ i 4)))]
    (when (= 4 (count digits))
      (reduce (fn [acc c]
                (let [d (str/index-of hex (str/lower-case (str c)))]
                  (if (and acc d) (+ (* acc 16) d) (reduced nil))))
              0
              digits))))

(defn- read-string*
  [s i]
  (loop [i (inc i) acc []]
    (let [c (ch s i)]
      (cond
        (nil? c) (fail i "unterminated string")
        (= c "\"") [(apply str acc) (inc i)]
        (= c "\\")
        (let [e (ch s (inc i))]
          (cond
            (contains? unescapes e) (recur (+ i 2) (conj acc (get unescapes e)))
            (= e "u")
            (let [code (hex4 s (+ i 2))]
              (cond
                (nil? code) (fail i "bad \\u escape")
                (<= 0xD800 code 0xDFFF) (fail i "surrogate \\u escapes are not supported")
                :else (recur (+ i 6) (conj acc (str (char code))))))
            :else (fail i "bad escape")))
        (< (int (first c)) 0x20) (fail i "control character in string")
        :else
        ;; Copy the run up to the next quote or backslash in one subs.
        (let [q   (or (str/index-of s "\"" i) (count s))
              b   (or (str/index-of s "\\" i) (count s))
              end (min q b)]
          (if (> end i)
            (let [run (subs s i end)]
              (if (some #(< (int %) 0x20) run)
                (fail i "control character in string")
                (recur end (conj acc run))))
            (recur (inc i) (conj acc c))))))))

(def ^:private number-chars (set (map str "0123456789+-.eE")))

(defn- read-number
  [s i]
  (let [end (loop [j i] (if (contains? number-chars (ch s j)) (recur (inc j)) j))
        tok (subs s i end)]
    (if-not (re-matches #"-?(0|[1-9][0-9]*)(\.[0-9]+)?([eE][+-]?[0-9]+)?" tok)
      (fail i (str "bad number " tok))
      [(if (re-matches #"-?(0|[1-9][0-9]*)" tok) (parse-long tok) (parse-double tok)) end])))

(declare read-value)

(defn- read-array
  [s i]
  (let [i (skip-ws s (inc i))]
    (if (= "]" (ch s i))
      [[] (inc i)]
      (loop [i i acc []]
        (let [[v i] (read-value s i)
              i     (skip-ws s i)]
          (case (ch s i)
            "," (recur (skip-ws s (inc i)) (conj acc v))
            "]" [(conj acc v) (inc i)]
            (fail i "expected , or ]")))))))

(defn- read-object
  [s i]
  (let [i (skip-ws s (inc i))]
    (if (= "}" (ch s i))
      [{} (inc i)]
      (loop [i i acc {}]
        (when-not (= "\"" (ch s i)) (fail i "expected a string key"))
        (let [[k i] (read-string* s i)
              i     (skip-ws s i)
              _     (when-not (= ":" (ch s i)) (fail i "expected :"))
              [v i] (read-value s (skip-ws s (inc i)))
              i     (skip-ws s i)]
          (case (ch s i)
            "," (recur (skip-ws s (inc i)) (assoc acc k v))
            "}" [(assoc acc k v) (inc i)]
            (fail i "expected , or }")))))))

(defn- read-value
  [s i]
  (let [i (skip-ws s i)
        c (ch s i)]
    (cond
      (nil? c) (fail i "unexpected end of input")
      (= c "{") (read-object s i)
      (= c "[") (read-array s i)
      (= c "\"") (read-string* s i)
      (= c "t") [true (expect s i "true")]
      (= c "f") [false (expect s i "false")]
      (= c "n") [nil (expect s i "null")]
      :else (read-number s i))))

(defn parse
  [s]
  (if-not (string? s)
    {:error "not a string" :at 0}
    (try
      (let [[v i] (read-value s 0)
            i     (skip-ws s i)]
        (if (= i (count s))
          {:ok v}
          {:error "trailing input" :at i}))
      (catch Exception e
        {:error (or (ex-message e) "malformed") :at (get (ex-data e) ::at 0)}))))

(defn complete-object?
  "True when S is exactly one complete JSON object. The framing rule the
   JVM client already holds its side to (hive-gimp.codec/complete-frame?)."
  [s]
  (map? (:ok (parse s))))
