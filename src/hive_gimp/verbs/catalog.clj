(ns hive-gimp.verbs.catalog
  "The verb catalogue. A verb is a public fn of a registered verbs namespace
   whose metadata carries `:verb/args`, a malli `:cat` over its arguments, and
   `:verb/answers`, the schema of the value its program answers. A verbs
   namespace registers itself with `register-ns!`; the catalogue is derived
   from the registered namespaces' publics, keyed `:<ns-leaf>/<fn>`.

   Adding a verb is a `defn` with that metadata; adding a family is a new
   namespace that registers itself."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defonce ^:private namespaces (atom (sorted-set)))

(defn register-ns!
  "Add verbs namespace `ns-sym` to the catalogue."
  [ns-sym]
  (swap! namespaces conj ns-sym)
  ns-sym)

(defn registered-namespaces
  "Every registered verbs namespace, sorted."
  []
  (vec @namespaces))

(def Entry
  [:map {:closed true}
   [:verb    :qualified-keyword]
   [:var     [:fn var?]]
   [:doc     [:maybe :string]]
   [:args    :any]
   [:answers :any]])

(defn verb?
  "True when var `v` declares itself a verb."
  [v]
  (contains? (meta v) :verb/args))

(defn verb-key
  "`:layer/set-name` for `#'hive-gimp.verbs.layer/set-name`."
  [v]
  (let [{:keys [ns name]} (meta v)]
    (keyword (last (str/split (str (ns-name ns)) #"\.")) (str name))))

(defn entry
  "The catalogue `Entry` of verb var `v`."
  [v]
  (let [md (meta v)]
    {:verb (verb-key v) :var v :doc (:doc md) :args (:verb/args md) :answers (:verb/answers md)}))

(defn catalogue-of
  "The catalogue of the verbs namespaces `ns-syms`, by verb key."
  [ns-syms]
  (into (sorted-map)
        (for [n ns-syms, v (vals (ns-publics n)) :when (verb? v)]
          [(verb-key v) (entry v)])))

(defn catalogue
  "The catalogue of every registered verbs namespace."
  []
  (catalogue-of @namespaces))

(defn lookup
  "The `Entry` of `verb`, or nil."
  [verb]
  (get (catalogue) verb))

(defn command-of
  "The snake_case command an Outcome of `verb` is labelled with."
  [verb]
  (str "verb_" (str/replace (str (namespace verb) "_" (name verb)) #"[^a-z0-9_]" "_")))
