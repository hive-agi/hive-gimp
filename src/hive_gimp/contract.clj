(ns hive-gimp.contract
  "PROMOTE. The descriptor algebra: pure functions over a collection of
   `Descriptor` values.

   Split out of `hive-gimp.catalog` on purpose. Indexing, collision resolution,
   search and projection are decisions ABOUT the contract, and every one of
   them was previously reachable only through a classpath resource. A pure
   function over a vector of descriptors can be driven from a generator and a
   REPL; the same logic behind a `delay` over `io/resource` cannot.

   `hive-gimp.catalog` now does one thing, which is read the resources.
   Everything that INTERPRETS what it read lives here."
  (:require [clojure.string :as str]
            [hive-gimp.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Naming
;; =============================================================================

(defn canonical-tool
  "The tool name a command would carry if nobody had renamed it."
  [command]
  (str "gimp_" command))

(defn canonical?
  "True when this descriptor's tool name matches its own command."
  [descriptor]
  (= (:tool descriptor) (canonical-tool (:command descriptor))))

;; =============================================================================
;; Collision resolution
;; =============================================================================

(defn prefer
  "Which of two descriptors for the same command wins.

   The derived contract contains one genuine collision: the reference server's
   `check_server` tool sends `get_gimp_info`, so two tools claim that command.
   The tie breaks toward the canonical claimant, whose parameters actually
   describe the command rather than borrowing it for a connection probe.

   COMMUTATIVE, and that is a requirement rather than a nicety. `index` folds
   this over a sequence read from two resource files, so any argument-order
   dependence here becomes a catalog that differs by file order: a command with
   parameters on one machine and without them on another, with nothing in the
   output to say so.

   Getting that right needs each rung to be EXCLUSIVE. An earlier version
   asked `(canonical? a)` and then `(canonical? b)`, which answers `a` for
   (a, b) and `b` for (b, a) whenever BOTH are canonical, which is precisely
   what a row duplicated across the derived and hand-written files looks like.
   `prefer-is-commutative` found it on the third generated case.

   So the rungs form a total order: canonical beats non-canonical, then more
   parameters wins, then a deterministic order over the values themselves. The
   last rung is never reached by real data, and exists so that `index` cannot
   depend on argument order for ANY input rather than only for the inputs we
   thought of."
  [a b]
  (let [canon-a (canonical? a)
        canon-b (canonical? b)
        n-a     (count (:params a))
        n-b     (count (:params b))]
    (cond
      (= a b) a

      (and canon-a (not canon-b)) a
      (and canon-b (not canon-a)) b

      (> n-a n-b) a
      (< n-a n-b) b

      ;; Fully tied on every meaningful axis. Order by the printed value, which
      ;; is total and independent of which argument arrived first.
      :else (if (neg? (compare (pr-str a) (pr-str b))) a b))))

(defn index
  "Descriptors to a `Catalog`, resolving duplicates through `prefer`.

   Not `(into {} (map (juxt :command identity)))`: that is last-write-wins, and
   the only symptom of losing the tie would be a command that rejects arguments
   it ought to accept."
  [descriptors]
  (reduce (fn [acc d]
            (if-let [existing (get acc (:command d))]
              (assoc acc (:command d) (prefer existing d))
              (assoc acc (:command d) d)))
          {}
          descriptors))

(defn aliases
  "Tool name to command, for EVERY descriptor including those `index` dropped.

   A tool that lost the tie-break is still a name this contract publishes, and
   answering `unknown command` for a name we ourselves advertise is worse than
   resolving it to the surviving descriptor."
  [descriptors]
  (into {} (map (juxt :tool :command)) descriptors))

;; =============================================================================
;; Queries
;; =============================================================================

(defn lookup
  "The descriptor for `name-or-tool` in `catalog`, consulting `alias-map`.

   Accepts either vocabulary: a wire command (`auto_levels`) or a published
   tool name (`gimp_auto_levels`). An MCP client that echoes back the tool name
   it was given should not be punished for it."
  [catalog alias-map name-or-tool]
  (or (get catalog name-or-tool)
      (some->> (get alias-map name-or-tool) (get catalog))))

(defn command-names
  "Every command in `catalog`, sorted.

   Sorted rather than left in file order: this is published in a tool schema,
   and a surface that reorders itself between builds is a diff every time."
  [catalog]
  (vec (sort (keys catalog))))

(defn search
  "Descriptors in `catalog` whose command or documentation contains `q`.

   Present because eighty commands do not fit in a tool description, and an
   agent that cannot find `unsharp` should not have to guess at `sharpen`."
  [catalog q]
  (let [needle (str/lower-case (str q))]
    (->> (vals catalog)
         (filter (fn [{:keys [command doc]}]
                   (or (str/includes? command needle)
                       (str/includes? (str/lower-case (str doc)) needle))))
         (sort-by :command)
         vec)))

;; =============================================================================
;; Projection
;; =============================================================================

(defn describe
  "One descriptor as the compact map the catalog tool publishes.

   An identity projection in the DDD sense: the contract's internal shape is
   not the shape a client should depend on, and this is the single place the
   two are related. It lived in the facade before, which put a promote decision
   where a wiring namespace could not be generatively tested."
  [{:keys [command tool doc params]}]
  {:command command
   :tool    tool
   :doc     doc
   :params  (mapv (fn [p]
                    (cond-> {:name (:name p) :type (name (:schema p))}
                      (:required? p)         (assoc :required true)
                      (contains? p :default) (assoc :default (:default p))
                      (:nilable? p)          (assoc :nullable true)
                      (seq (:doc p))         (assoc :doc (:doc p))))
                  params)})

;; =============================================================================
;; Validation
;; =============================================================================

(defn invalid
  "Descriptors that do not conform, with an explanation each.

   A contract file is generated by a script against someone else's source, so
   `it parsed` is not the same as `it is usable`."
  [descriptors]
  (->> descriptors
       (remove schema/descriptor?)
       (mapv (fn [d]
               {:command (:command d)
                :problem (schema/explain schema/Descriptor d)}))))
