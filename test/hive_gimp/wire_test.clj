(ns hive-gimp.wire-test
  "hive-gimp.wire against an ORACLE: clojure.data.json, which the JVM side of
   this library already trusts (hive-gimp.codec).

   A codec checked only against its own round trip agrees with itself even
   when it is wrong in both directions at once. Each property below pits one
   direction against the oracle instead: the oracle writes and wire reads,
   wire writes and the oracle reads, and on damaged text both judge and must
   agree.

   The oracle is made STRICT first. data.json's `read-str` reads one value and
   ignores whatever follows it, which wire (and RFC 8259) treat as an error, so
   `strict-oracle` reads through a reader and refuses trailing input.

   Where the two still differ, the difference is deliberate and pinned in
   `documented-divergences-test`; a property never silently skips a case it
   does not understand."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-gimp.codec :as codec]
            [hive-gimp.wire :as wire]
            [clojure.walk :as walk])
  (:import (java.io PushbackReader StringReader)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Oracle
;; =============================================================================

(def ^:private json-whitespace #{\space \tab \newline \return})

(defn strict-oracle
  "{:ok value} when data.json reads TEXT as exactly one JSON value with only
   JSON whitespace after it, else {:error kind}."
  [^String text]
  (try
    (let [r (PushbackReader. (StringReader. text) 64)
          v (json/read r)
          tail (slurp r)]
      (if (every? json-whitespace tail)
        {:ok v}
        {:error :trailing}))
    (catch Exception _ {:error :rejected})))

;; =============================================================================
;; Generators
;; =============================================================================

(def gen-code-point
  "Any Unicode scalar value, astral included, surrogates excluded: every string
   a well-formed JSON text can carry."
  (gen/such-that #(not (<= 0xD800 % 0xDFFF))
                 (gen/frequency [[6 (gen/choose 0x20 0x7e)]
                                 [2 (gen/choose 0 0x1f)]
                                 [2 (gen/choose 0x80 0xFFFF)]
                                 [1 (gen/choose 0x10000 0x10FFFF)]])
                 100))

(def gen-text
  (gen/fmap (fn [cps] (apply str (map #(String. (Character/toChars (int %))) cps)))
            (gen/vector gen-code-point 0 12)))

(def gen-scalar
  (gen/one-of [(gen/return nil)
               gen/boolean
               gen/large-integer
               (gen/double* {:infinite? false :NaN? false})
               gen-text]))

(def gen-json
  "A JSON-shaped Clojure value: string-keyed maps, vectors, scalars."
  (gen/recursive-gen (fn [inner]
                       (gen/one-of [(gen/vector inner 0 4)
                                    (gen/map gen-text inner {:max-elements 4})]))
                     gen-scalar))

(def gen-frame
  "A response object as the plug-in would send it."
  (gen/let [status  (gen/elements ["success" "error"])
            results gen-json
            error   gen-text]
    (if (= "success" status)
      {"status" status "results" results}
      {"status" status "error" error})))

(def ^:private damage-chars
  ["{" "}" "[" "]" "," ":" "\"" "\\" "0" "-" "." "e" "u" " " "n" "t"])

(def gen-damaged-text
  "Valid JSON text with one edit: truncated, one char deleted, or one
   structurally significant char inserted. Most results are invalid, and those
   are the cases that decide whether two parsers agree on what JSON IS."
  (gen/let [v    gen-json
            kind (gen/elements [:truncate :delete :insert])
            n    gen/nat
            c    (gen/elements damage-chars)]
    (let [t (json/write-str v)
          i (mod n (inc (count t)))]
      (case kind
        :truncate (subs t 0 i)
        :delete   (if (< i (count t)) (str (subs t 0 i) (subs t (inc i))) t)
        :insert   (str (subs t 0 i) c (subs t i))))))

;; =============================================================================
;; Values: the oracle writes, wire reads, and the reverse
;; =============================================================================

(defspec wire-reads-what-the-oracle-writes 300
  (prop/for-all [v gen-json]
    (= {:ok v} (wire/parse (json/write-str v)))))

(defspec wire-reads-what-the-oracle-writes-unescaped 200
  ;; data.json escapes every non-ASCII char by default, which only ever
  ;; exercises wire's \u path. Written raw, the same values exercise the other.
  (prop/for-all [v gen-json]
    (= {:ok v} (wire/parse (json/write-str v :escape-unicode false)))))

(defspec the-oracle-reads-what-wire-writes 300
  (prop/for-all [v gen-json]
    (= {:ok v} (strict-oracle (wire/emit v)))))

(defspec wire-round-trips-itself 200
  (prop/for-all [v gen-json]
    (= {:ok v} (wire/parse (wire/emit v)))))

;; =============================================================================
;; Damaged text: both judge, and must agree
;; =============================================================================

(defn- lone-surrogate?
  "Whether S holds a UTF-16 surrogate not paired with its partner."
  [^String s]
  (let [n (.length s)]
    (loop [i 0]
      (cond
        (>= i n) false
        (Character/isHighSurrogate (.charAt s i))
        (if (and (< (inc i) n) (Character/isLowSurrogate (.charAt s (inc i))))
          (recur (+ i 2))
          true)
        (Character/isLowSurrogate (.charAt s i)) true
        :else (recur (inc i))))))

(defn- carries-lone-surrogate? [v]
  (boolean (some #(and (string? %) (lone-surrogate? %))
                 (tree-seq coll? #(if (map? %) (mapcat identity %) (seq %)) v))))

(defn- bigints->doubles
  "The oracle's value as wire spells it: an integer past the long range is a
   double in wire and a BigInt in data.json (a documented divergence)."
  [v]
  (walk/postwalk #(if (instance? clojure.lang.BigInt %) (double %) %) v))

(defspec wire-and-the-oracle-agree-on-damaged-text 1000
  (prop/for-all [t gen-damaged-text]
    (let [w (wire/parse t)
          o (strict-oracle t)]
      (cond
        ;; A deletion that splits an escaped surrogate pair: data.json accepts
        ;; the lone half, wire refuses it. Wire may also name the surrogate as
        ;; the reason for a text the oracle rejects for another one.
        (= "unpaired surrogate \\u escape" (:error w))
        (or (contains? o :error) (carries-lone-surrogate? (:ok o)))

        ;; An inserted sign inside a \u escape: data.json reads the four chars
        ;; with Integer/parseInt, which takes a sign, so it accepts \u-000.
        (and (= "bad \\u escape" (:error w)) (contains? o :ok))
        (boolean (re-find #"\\u[+-]" t))

        (contains? w :ok) (and (contains? o :ok)
                               (= (:ok w) (bigints->doubles (:ok o))))
        :else (contains? o :error)))))

;; =============================================================================
;; Framing and envelopes, against the JVM codec
;; =============================================================================

(defspec wire-and-codec-agree-on-every-prefix-of-a-frame 200
  (prop/for-all [frame gen-frame]
    (let [text (json/write-str frame)]
      (every? (fn [n]
                (let [prefix (subs text 0 n)]
                  (= (codec/complete-frame? prefix) (wire/complete-frame? prefix))))
              (range 0 (inc (count text)))))))

(defspec wire-and-codec-decode-alike 300
  (prop/for-all [frame gen-frame
                 cut   gen/nat]
    (let [text   (json/write-str frame)
          buffer (subs text 0 (- (count text) (mod cut 3)))
          c      (codec/decode buffer)
          w      (wire/decode-response buffer)]
      (and (= (:error c) (:error w))
           (= (:ok c) (:ok w))))))

(deftest decode-response-reasons-match-the-codec
  (doseq [buffer ["{\"status\":" "[1,2,3]" "\"s\"" "{\"nope\":1}" "{\"status\":\"weird\"}"
                  "{\"status\":\"error\",\"error\":7}"
                  "{\"status\":\"success\",\"results\":{\"a\":1}}"
                  "{\"status\":\"error\",\"error\":\"boom\",\"traceback\":\"tb\"}"]]
    (testing buffer
      (is (= (:error (codec/decode buffer)) (:error (wire/decode-response buffer))))
      (is (= (:ok (codec/decode buffer)) (:ok (wire/decode-response buffer)))))))

(defspec encode-request-says-what-codec-encode-says 200
  (prop/for-all [type   (gen/elements ["auto_levels" "select_all" "export_image" "new_canvas"])
                 params (gen/map gen-text gen-json {:max-elements 4})]
    (let [command {:type type :params params}
          w       (wire/encode-request command)]
      (and (str/ends-with? w "\n")
           (= (json/read-str (codec/encode command)) (json/read-str w))
           (wire/complete-frame? (str/trimr w))))))

(deftest encode-request-keeps-explicit-nulls
  (is (= "{\"type\":\"auto_levels\",\"params\":{\"image_index\":0,\"layer_name\":null}}\n"
         (wire/encode-request {:type "auto_levels"
                               :params (array-map "image_index" 0 "layer_name" nil)}))))

;; =============================================================================
;; Divergences from the oracle, each on purpose
;; =============================================================================

(deftest documented-divergences-test
  (testing "trailing input: data.json read-str ignores it, wire refuses it"
    (is (= {} (json/read-str "{} x")))
    (is (= {:error "trailing input" :at 3} (wire/parse "{} x")))
    (is (= {:error :trailing} (strict-oracle "{} x")) "the strict oracle agrees with wire"))

  (testing "codec and wire agree on it: an object followed by more text is not a whole frame"
    (is (false? (codec/complete-frame? "{\"status\":\"success\"}{\"sta")))
    (is (false? (wire/complete-frame? "{\"status\":\"success\"}{\"sta")))
    (is (= (:error (codec/decode "{\"status\":\"success\"} x"))
           (:error (wire/decode-response "{\"status\":\"success\"} x")))))

  (testing "a lone surrogate escape: data.json accepts it, wire refuses it"
    (is (lone-surrogate? (json/read-str "\"\\ud83d\"")))
    (is (= "unpaired surrogate \\u escape" (:error (wire/parse "\"\\ud83d\"")))))

  (testing "a raw control char inside a string: data.json accepts it, wire refuses it (RFC 8259 s7)"
    (is (= "a\tb" (json/read-str "\"a\tb\"")))
    (is (= "control character in string" (:error (wire/parse "\"a\tb\"")))))

  (testing "an integer past the long range: data.json reads a BigInt, wire a double"
    (is (= 100000000000000000000N (json/read-str "100000000000000000000")))
    (is (= {:ok 1.0E20} (wire/parse "100000000000000000000"))))

  (testing "non-ASCII: data.json escapes it on write, wire writes it raw; both read either"
    (is (= "\"\\u00e9\"" (json/write-str "é")))
    (is (= "\"é\"" (wire/emit "é")))
    (is (= {:ok "é"} (wire/parse (json/write-str "é"))))
    (is (= "é" (json/read-str (wire/emit "é")))))

  (testing "an unknown escape: both refuse, data.json with a non-JSON exception"
    (is (thrown? IllegalArgumentException (json/read-str "\"\\x\"")))
    (is (= "bad escape" (:error (wire/parse "\"\\x\""))))))

(deftest documented-divergence-signed-unicode-escape-test
  (testing "a sign inside a \\u escape: data.json accepts it (Integer/parseInt takes a sign), wire refuses it"
    (is (= (str "a" (char 0)) (json/read-str "\"a\\u-000\"")))
    (is (= "bad \\u escape" (:error (wire/parse "\"a\\u-000\""))))
    (is (= "bad \\u escape" (:error (wire/parse "\"a\\u+00f\""))))))

(deftest parse-never-throws
  (doseq [x [nil 1 :k "" "{" "\"\\u" "\"\\ud83d\\u" "[1e]" (apply str (repeat 500 "["))]]
    (is (map? (wire/parse x)) (pr-str x))))
