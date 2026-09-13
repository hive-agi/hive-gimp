(ns hive-gimp.command
  "PROMOTE. Descriptor plus arguments to a `GimpCommand`, pure.

   Everything here takes a VALUE and returns a VALUE. That is not tidiness:
   `deftrifecta-from-schema` synthesizes coverage from an `:in` schema, and a
   function whose first argument is a `Descriptor` has one while a function
   whose first argument is a command NAME does not. Writing this layer over
   the descriptor is what buys the suite.

   This is also the ONLY namespace allowed to touch both vocabularies. Above
   it, arguments are kebab-case keywords; below it, JSON object keys are
   snake_case strings. A leak in either direction shows up here or nowhere."
  (:require [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-gimp.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Descriptor projections
;; =============================================================================

(defn param-index
  "Parameter name (keyword) to `ParamSpec`."
  [descriptor]
  (into {} (map (juxt (comp keyword :name) identity)) (:params descriptor)))

(defn required-params
  "Parameters a caller must supply."
  [descriptor]
  (into #{} (comp (filter :required?) (map (comp keyword :name))) (:params descriptor)))

(defn defaulted-params
  "Parameters carrying a default, as name to default value.

   `contains?` rather than a truthy check, because `nil` and `false` are both
   legitimate defaults in this contract and both are falsey."
  [descriptor]
  (into {}
        (comp (filter #(contains? % :default))
              (map (juxt (comp keyword :name) :default)))
        (:params descriptor)))

(defn nil-allowed?
  "True when the plugin accepts an explicit null for this parameter.

   Distinct from `not required?`: a parameter can be optional (it has a
   default) and still reject null once that default is applied."
  [param]
  (boolean (:nilable? param)))

;; =============================================================================
;; Coercion
;; =============================================================================
;;
;; MCP clients are not uniformly typed. Some send `5`, some send `\"5\"`, and a
;; few send `\"[1,2,3]\"` for an array parameter. The reference server leans on
;; FastMCP's signature coercion, which we do not have, so the same leniency is
;; reproduced here as a pure, testable function instead of being discovered one
;; failed GIMP call at a time.
;;
;; Leniency has a floor: a value that cannot be read as the declared type is an
;; error, never a silent pass-through. Sending a string where the plugin wants
;; an int produces a Python TypeError inside GIMP, which surfaces as an opaque
;; traceback with no mention of the parameter that caused it.

(defn- parse-long* [s]
  (try (Long/parseLong (str/trim s)) (catch Exception _ nil)))

(defn- parse-double* [s]
  (try (Double/parseDouble (str/trim s)) (catch Exception _ nil)))

(defn- read-json-ish
  "Read `s` as JSON when it looks like a JSON array or object, else nil.

   Deliberately shallow: this exists for clients that stringify structured
   arguments, not as a general escape hatch for embedding JSON in strings."
  [s pred]
  (let [t (str/trim (str s))]
    (when (or (str/starts-with? t "[") (str/starts-with? t "{"))
      (try
        (let [v (clojure.data.json/read-str t)]
          (when (pred v) v))
        (catch Exception _ nil)))))

(defn- coerce-scalar
  "Value coerced to `kind`, or ::fail."
  [kind value]
  (case kind
    :long    (cond
               (integer? value) (long value)
               ;; A whole double is an integer that survived a JSON round trip.
               (and (number? value) (== value (long value))) (long value)
               (string? value) (or (parse-long* value) ::fail)
               :else ::fail)
    :double  (cond
               (number? value) (double value)
               (string? value) (or (parse-double* value) ::fail)
               :else ::fail)
    :string  (cond
               (string? value)  value
               (keyword? value) (name value)
               :else ::fail)
    :boolean (cond
               (boolean? value) value
               (= "true" value)  true
               (= "false" value) false
               :else ::fail)
    :vector  (cond
               (sequential? value) (vec value)
               (string? value) (or (read-json-ish value sequential?) ::fail)
               :else ::fail)
    :map     (cond
               (map? value) value
               (string? value) (or (read-json-ish value map?) ::fail)
               :else ::fail)
    :nil     (if (nil? value) nil ::fail)
    :any     value
    ::fail))

(defn coerce
  "One argument coerced to what its `ParamSpec` declares.

   Returns a Result so the failure carries the parameter name. A bare nil or a
   thrown exception here would reach the caller as `GIMP rejected your call`,
   which is true and useless."
  [param value]
  (cond
    (and (nil? value) (nil-allowed? param))
    (r/ok nil)

    (nil? value)
    (r/err :gimp/invalid-parameter
           {:parameter (:name param)
            :message   (str "Parameter " (:name param) " does not accept null.")})

    :else
    (let [coerced (coerce-scalar (:schema param) value)]
      (if (= ::fail coerced)
        (r/err :gimp/invalid-parameter
               {:parameter (:name param)
                :message   (str "Parameter " (:name param) " expects "
                                (name (:schema param)) ", got " (pr-str value) ".")})
        (r/ok coerced)))))

;; =============================================================================
;; Build
;; =============================================================================

(defn- unknown-arguments
  "Supplied argument names this descriptor does not declare."
  [descriptor args]
  (let [known (set (keys (param-index descriptor)))]
    (into (sorted-set) (remove known) (keys args))))

(defn- missing-arguments
  "Required parameters the caller did not supply."
  [descriptor args]
  (into (sorted-set) (remove #(contains? args %)) (required-params descriptor)))

(defn- effective-args
  "Supplied arguments with defaults filled in for everything absent.

   Defaults are applied HERE rather than left to the plugin, because the plugin
   reads absent parameters with `.get` and a default that disagrees with this
   contract would be invisible: the call would succeed and do something else."
  [descriptor args]
  (merge (defaulted-params descriptor) args))

(defn ->command
  "`Descriptor` plus kebab-case arguments to a `GimpCommand`.

   Fails, rather than guessing, on: an unknown parameter, a missing required
   parameter, or a value that cannot be read as its declared type. Each failure
   names the parameter, because the alternative is a Python traceback from
   inside GIMP that names a line number in someone else's file."
  [descriptor args]
  (let [args (or args {})]
    (cond
      (not (schema/descriptor? descriptor))
      (r/err :gimp/unknown-command
             {:message "No such GIMP command."
              :detail  (schema/explain schema/Descriptor descriptor)})

      (seq (unknown-arguments descriptor args))
      (r/err :gimp/unknown-parameter
             {:message (str "Unknown parameter(s) for " (:command descriptor) ": "
                            (str/join ", " (map name (unknown-arguments descriptor args)))
                            ". Accepted: "
                            (str/join ", " (sort (map :name (:params descriptor)))) ".")})

      (seq (missing-arguments descriptor args))
      (r/err :gimp/missing-parameter
             {:message (str "Missing required parameter(s) for " (:command descriptor) ": "
                            (str/join ", " (map name (missing-arguments descriptor args))) ".")})

      :else
      (let [index (param-index descriptor)
            coerced (reduce-kv
                     (fn [acc k v]
                       (if (r/err? acc)
                         acc
                         (let [res (coerce (get index k) v)]
                           (if (r/err? res)
                             res
                             (assoc-in acc [:ok (:wire (get index k))] (:ok res))))))
                     (r/ok {})
                     (effective-args descriptor args))]
        (if (r/err? coerced)
          coerced
          (r/ok {:type   (:command descriptor)
                 :params (:ok coerced)}))))))
