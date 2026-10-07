(ns hive-gimp.py.names
  "Clojure symbols as Python dotted names, and the one refusal every stratum
   above raises for a form it cannot lower."
  (:require [clojure.string :as str]
            [hive-schemas.schema :as hs]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private python-name
  #"[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*")

;; =============================================================================
;; Schemas
;; =============================================================================

(def PyName
  "A Python identifier, or a dotted path of them."
  [:re {:error/message "a Python (dotted) name"
        :gen/elements  ["a" "x1" "_hg" "get_images" "Gimp.get_images" "Gimp.ChannelOps.REPLACE"]}
   #"^[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*$"])

(def Refusal
  "The `ex-data` of a form lowering refuses."
  [:map
   [:hive-gimp/reason [:= :py/unsupported-form]]
   [:form :any]])

(hs/register-all! {::py-name PyName
                   ::refusal Refusal})

;; =============================================================================
;; Names
;; =============================================================================

(defn refuse
  "Throw the unsupported-form refusal: `message` names the fix, `form` is the
   offending input."
  [message form]
  (throw (ex-info message {:hive-gimp/reason :py/unsupported-form :form form})))

(defn ident
  "`s` with `-` spelled `_`, when it is a Python (dotted) name; refuses `form`
   otherwise."
  [s form]
  (let [s (str/replace (str s) "-" "_")]
    (if (re-matches python-name s)
      s
      (refuse (str "`" form "` is not a Python name.") form))))

(defn sym->py
  "`Gimp/get-images` -> `Gimp.get_images`, `a.b-c` -> `a.b_c`."
  [sym]
  (if (symbol? sym)
    (ident (if-let [ns (namespace sym)] (str ns "." (name sym)) (name sym)) sym)
    (refuse (str "`" (pr-str sym) "` is not a Python name: a name is a symbol.") sym)))

(defn keyword->py
  "A keyword argument's name: `:fill-type` -> `fill_type`."
  [kw]
  (ident (name kw) kw))

(m/=> ident [:=> [:cat :any :any] PyName])
(m/=> sym->py [:=> [:cat :symbol] PyName])
(m/=> keyword->py [:=> [:cat :keyword] PyName])
