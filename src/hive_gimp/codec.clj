(ns hive-gimp.codec
  "Framing and JSON, pure.

   The GIMP plugin's wire protocol is asymmetric, and both directions are
   captured here so that neither is buried behind an I/O call where no
   generator can reach it.

   REQUEST   one JSON object followed by a newline. The plugin accumulates
             until the buffer parses, so the newline is a courtesy rather than
             a delimiter, but the reference client sends it and the plugin's
             own logging assumes it.

   RESPONSE  one JSON object and NOTHING else. No length prefix, no
             terminator, no newline. `parses as JSON` IS the frame boundary,
             which is why `complete-frame?` exists as a named, tested predicate
             instead of a try/catch buried in a read loop.

   One deliberate strengthening over the reference clients: a frame is complete
   only when the buffer parses as a JSON OBJECT. Both Python clients accept any
   parse, so a response truncated at a point where the prefix happens to be
   valid JSON on its own gets treated as a whole message and the rest of the
   real answer is read as the head of the NEXT one. Requiring an object costs
   nothing (every plugin answer is an object) and removes the failure mode."
  (:require [clojure.data.json :as json]
            [hive-dsl.result :as r]
            [hive-gimp.schema :as schema])
(:import [java.io PushbackReader]
[java.io StringReader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def request-terminator
  "Appended to every request frame. See the ns docstring: a courtesy, not a
   delimiter."
  "\n")

;; =============================================================================
;; Encode
;; =============================================================================

(defn encode
  "`GimpCommand` to one complete request frame.

   Params are written verbatim, nulls included. The reference client sends
   `\"layer_name\": null` for an omitted optional and the plugin reads it back
   with `.get`, so an encoder that helpfully dropped nil keys would be sending
   a different message than every other client of this plugin."
  [command]
  (str (json/write-str {"type"   (:type command)
                        "params" (:params command)})
       request-terminator))

;; =============================================================================
;; Decode
;; =============================================================================

(defn- only-json-whitespace-left?
  "True when nothing but JSON whitespace (space, tab, LF, CR) remains in `r`."
  [^PushbackReader r]
  (loop []
    (let [c (.read r)]
      (cond
        (= -1 c) true
        (contains? #{32 9 10 13} c) (recur)
        :else false))))

(defn- parse
  "Parsed JSON value, or ::unparseable. Never throws: callers use this to ASK
   whether a buffer is complete, and an exception is the normal answer for
   `not yet`.

   Exactly one value: anything but JSON whitespace after it is ::unparseable,
   so an object followed by the head of the next one is never read as whole."
  [^String buffer]
  (try
    (let [r (PushbackReader. (StringReader. buffer) 64)
          v (json/read r)]
      (if (only-json-whitespace-left? r) v ::unparseable))
    (catch Exception _ ::unparseable)))

(defn complete-frame?
  "True when `buffer` holds a whole response object.

   This is the read loop's only stopping condition, so it carries the whole
   framing contract. `nil` and the empty string are never complete."
  [buffer]
  (and (string? buffer)
       (pos? (count buffer))
       (map? (parse buffer))))

(defn decode
  "One response frame to a `RawResponse`.

   Three distinct failures, kept distinct because they call for different
   actions: unparseable means the frame was truncated or the socket desynced,
   not-an-object means something other than the plugin answered, and
   malformed means the plugin answered in a shape this contract does not know."
  [buffer]
  (let [parsed (parse buffer)]
    (cond
      (= ::unparseable parsed)
      (r/err :gimp/unparseable-response
             {:message "GIMP returned a frame that is not JSON."
              :detail  (some-> buffer (subs 0 (min 200 (count buffer))))})

      (not (map? parsed))
      (r/err :gimp/unparseable-response
             {:message "GIMP returned JSON that is not an object."
              :detail  (pr-str (type parsed))})

      (not (schema/raw-response? parsed))
      (r/err :gimp/malformed-response
             {:message "GIMP returned an object without a usable status."
              :detail  (schema/explain schema/RawResponse parsed)})

      :else (r/ok parsed))))
