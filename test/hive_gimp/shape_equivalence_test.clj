(ns hive-gimp.shape-equivalence-test
  "`hive-gimp.shape` carries hand-written predicates so the command pipeline
   can check a shape on a host without malli. Two implementations of one rule
   is the classic way to grow a silent disagreement, so this suite pins them
   together: over the WHOLE shipped contract, and over counter-examples built
   to break each clause of each schema.

   If one ever disagrees with the other, the malli schema is the source of
   truth and the portable predicate is the bug."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.schema :as schema]
            [hive-gimp.shape :as shape]))

;; ---------------------------------------------------------------------------
;; Over the real contract

(deftest the-two-descriptor-checks-agree-on-every-shipped-descriptor-test
  (let [descriptors (catalog/load-descriptors)]
    (is (seq descriptors) "no contract loaded, so this proves nothing")
    (is (< 50 (count descriptors))
        "the shipped contract is ~80 rows; a handful means the resource did not load")
    (doseq [d descriptors]
      (is (= (schema/descriptor? d) (shape/descriptor? d))
          (str (:command d) ": malli says " (schema/descriptor? d)
               ", the portable predicate says " (shape/descriptor? d))))))

(deftest every-shipped-descriptor-actually-passes-both-test
  (doseq [d (catalog/load-descriptors)]
    (is (shape/descriptor? d) (str (:command d) " fails the portable check"))))

;; ---------------------------------------------------------------------------
;; Counter-examples, one per clause

(def ^:private good-descriptor
  {:command "new_canvas"
   :tool    "gimp_new_canvas"
   :doc     "Make a canvas."
   :params  [{:name "width" :wire "width" :schema :long :required? true :doc "px"}
             {:name "layer-name" :wire "layer_name" :schema :string :nilable? true :default nil}]})

(def ^:private bad-descriptors
  {"command not snake_case"   (assoc good-descriptor :command "NewCanvas")
   "tool not gimp_ prefixed"  (assoc good-descriptor :tool "new_canvas")
   "doc missing"              (dissoc good-descriptor :doc)
   "doc not a string"         (assoc good-descriptor :doc 42)
   "params not sequential"    (assoc good-descriptor :params {:name "width"})
   "an extra key"             (assoc good-descriptor :extra true)
   "param name snake_case"    (assoc good-descriptor :params [{:name "layer_name" :wire "layer_name" :schema :string}])
   "param wire kebab-case"    (assoc good-descriptor :params [{:name "layer-name" :wire "layer-name" :schema :string}])
   "param type unknown"       (assoc good-descriptor :params [{:name "w" :wire "w" :schema :int}])
   "param extra key"          (assoc good-descriptor :params [{:name "w" :wire "w" :schema :long :nope 1}])
   "required? not boolean"    (assoc good-descriptor :params [{:name "w" :wire "w" :schema :long :required? "yes"}])
   "not a map"                "new_canvas"
   "nil"                      nil})

(deftest the-two-descriptor-checks-agree-on-counter-examples-test
  (testing "the good one passes both, or the counter-examples prove nothing"
    (is (schema/descriptor? good-descriptor))
    (is (shape/descriptor? good-descriptor)))
  (doseq [[why d] bad-descriptors]
    (testing why
      (is (false? (boolean (schema/descriptor? d))) "malli should reject this")
      (is (= (boolean (schema/descriptor? d)) (boolean (shape/descriptor? d)))))))

(def ^:private raw-responses
  {"success"                  {"status" "success" "results" [1 2]}
   "error with a message"     {"status" "error" "error" "boom" "traceback" "..."}
   "open to unknown keys"     {"status" "success" "whatever" 1}
   "status missing"           {"results" []}
   "status not one of two"    {"status" "ok"}
   "error not a string"       {"status" "error" "error" 500}
   "traceback not a string"   {"status" "error" "traceback" 7}
   "not a map"                "success"
   "nil"                      nil})

(deftest the-two-raw-response-checks-agree-test
  (doseq [[why r] raw-responses]
    (testing why
      (is (= (boolean (schema/raw-response? r)) (boolean (shape/raw-response? r)))
          (str "malli says " (boolean (schema/raw-response? r))
               ", the portable predicate says " (boolean (shape/raw-response? r)))))))

;; ---------------------------------------------------------------------------
;; The re-exports are the same objects, not copies

(deftest schema-re-exports-the-shape-definitions-test
  (doseq [[nm a b] [["Descriptor"  schema/Descriptor  shape/Descriptor]
                    ["ParamSpec"   schema/ParamSpec   shape/ParamSpec]
                    ["RawResponse" schema/RawResponse shape/RawResponse]
                    ["GimpCommand" schema/GimpCommand shape/GimpCommand]
                    ["Outcome"     schema/Outcome     shape/Outcome]
                    ["Endpoint"    schema/Endpoint    shape/Endpoint]
                    ["ExecRequest" schema/ExecRequest shape/ExecRequest]]]
    (testing nm
      (is (identical? a b)
          (str nm " was copied into hive-gimp.schema instead of re-exported;"
               " a copy is the one that rots")))))
