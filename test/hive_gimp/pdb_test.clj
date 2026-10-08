(ns hive-gimp.pdb-test
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.pdb :as pdb]
            [hive-gimp.pdb.derive :as derive]
            [hive-gimp.pdb.forms :as forms]
            [hive-gimp.pdb.native :as native]
            [hive-gimp.pdb.plan :as plan]
            [hive-gimp.pdb.spec :as spec]
            [hive-gimp.pdb.starter :as starter]
            [hive-gimp.py :as py]
            [hive-gimp.schema :as schema]
            [hive-gimp.stub :as stub]
            [hive-gimp.tools :as tools]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Data: the shipped specs, and describe answers captured from GIMP 3.2
;; =============================================================================

(def starter-specs
  (into {} (map (juxt (comp keyword name spec/fn-name) identity))
        (native/read-specs "hive_gimp/pdb/starter.edn")))

(def despeckle (:despeckle starter-specs))
(def invert    (:invert-drawable starter-specs))
(def export    (:export-png starter-specs))

(defn- arg-info [n t f d b & [mn mx]]
  {:name n :type t :fundamental f :default d :blurb b :min mn :max mx})

(def invert-info
  {:name "gimp-drawable-invert" :blurb "Invert the contents of the specified drawable." :help "" :values []
   :args [(arg-info "drawable" "GimpDrawable" "GObject" nil "The drawable")
          (arg-info "linear" "gboolean" "gboolean" false "Whether to invert in linear space")]})

(def png-info
  {:name "file-png-export" :blurb "Exports files in PNG file format" :help "" :values []
   :args [(arg-info "run-mode" "GimpRunMode" "GEnum" 1 "The run mode")
          (arg-info "image" "GimpImage" "GObject" nil "The image to export")
          (arg-info "file" "GFile" "GInterface" nil "The file to export to")
          (arg-info "options" "GimpExportOptions" "GObject" nil "Export options")
          (arg-info "compression" "gint" "gint" 9 "Deflate Compression factor" 0 9)
          (arg-info "format" "gchararray" "gchararray" "auto" "PNG export format")
          (arg-info "comment" "gchararray" "gchararray" nil "Comment")]})

(def new-layer-info
  {:name "gimp-layer-new" :blurb "Create a new layer." :help nil
   :values [(arg-info "layer" "GimpLayer" "GObject" nil "The new layer")]
   :args [(arg-info "image" "GimpImage" "GObject" nil "The image")
          (arg-info "color" "GeglColor" "GObject" nil "Fill colour")]})

(defn- answer
  "A run answer frame, exactly as GIMP's exec prints one."
  [m]
  (stub/success-frame [(str "\u001ehive-gimp.py " (json/write-str m) "\n")]))

(def ok-answer {"found" true "missing" [] "status" "success" "values" [] "names" []})

(def gen-wire-params
  (hs/generator [:map-of :string [:or :int :double :string :boolean [:vector :int]]]))

;; =============================================================================
;; Trifectas over the pure core
;; =============================================================================

(def ^:private real-spec->descriptor spec/spec->descriptor)

(deftrifecta spec->descriptor hive-gimp.pdb.spec/spec->descriptor
  {:golden-path "test/golden/pdb/spec_to_descriptor.edn"
   :cases       {:despeckle despeckle :invert invert :export export}
   :gen         (hs/generator :hive-gimp.pdb/procedure-spec)
   :pred        schema/descriptor?
   :num-tests   100
   :mutations   [["exposes fixed args"
                  (fn [s] (assoc (real-spec->descriptor s) :params (mapv spec/arg->param (:args s))))]
                 ["forgets required"
                  (fn [s] (update (real-spec->descriptor s) :params (partial mapv #(dissoc % :required?))))]
                 ["kebab on the wire"
                  (fn [s] (update (real-spec->descriptor s) :params (partial mapv #(assoc % :wire (:name %)))))]]})

(def ^:private real-info->spec derive/info->spec)

(deftrifecta info->spec hive-gimp.pdb.derive/info->spec
  {:golden-path "test/golden/pdb/info_to_spec.edn"
   :cases       {:invert invert-info :png png-info :new-layer new-layer-info}
   :gen         (hs/generator :hive-gimp.pdb/procedure-info)
   :pred        spec/procedure-spec?
   :num-tests   100
   :mutations   [["run-mode exposed"
                  (fn [i] (update (real-info->spec i) :args
                                  (partial mapv #(if (contains? % :fixed)
                                                   (-> % (dissoc :fixed) (assoc :default 1))
                                                   %))))]
                 ["every object optional"
                  (fn [i] (update (real-info->spec i) :args
                                  (partial mapv #(if (:required? %)
                                                   (-> % (dissoc :required?) (assoc :nilable? true :default nil))
                                                   %))))]
                 ["hides omitted args" (fn [i] (dissoc (real-info->spec i) :omitted))]]})

(defn- rendered [r] (if (:forms r) (py/->python (:forms r)) r))

(deftrifecta plan hive-gimp.pdb.plan/plan
  {:golden-path "test/golden/pdb/plan.edn"
   :apply?      true
   :cases       {:invert        [invert {"drawable" 7 "linear" true}]
                 :despeckle     [despeckle {"image" 1 "drawables" [2 3] "radius" 4}]
                 :export        [export {"image" 1 "file" "/tmp/x.png" "compression" 3}]
                 :empty-list    [despeckle {"image" 1 "drawables" []}]
                 :relative-file [export {"image" 1 "file" "x.png"}]
                 :zero-id       [invert {"drawable" 0}]}
   :xf          rendered
   :gen         (gen/tuple (hs/generator :hive-gimp.pdb/procedure-spec) gen-wire-params)
   :pred        #(or (vector? (:forms %)) (schema/outcome? %))
   :num-tests   100
   :mutations   [["skips value checks"
                  (fn [s p] {:forms (forms/run-forms (:procedure s) (spec/assignments s p))})]
                 ["drops fixed args"
                  (fn [s p] {:forms (forms/run-forms (:procedure s)
                                                     (remove (comp #(contains? % :fixed) first)
                                                             (spec/assignments s p)))})]]})

(def ^:private real-interpret plan/interpret)

(deftrifecta interpret hive-gimp.pdb.plan/interpret
  {:golden-path "test/golden/pdb/interpret.edn"
   :apply?      true
   :cases       {:ok        ["pdb_x" "x" {:found true :missing [] :status "success"
                                          :values [{:id 9 :type "Layer" :name "l"}] :names ["layer"]}]
                 :unknown   ["pdb_x" "x" {:found false :missing [] :status nil :values [] :names []}]
                 :missing   ["pdb_x" "x" {:found true :missing ["image"] :status nil :values [] :names []}]
                 :failed    ["pdb_x" "x" {:found true :missing [] :status "execution-error"
                                          :values ["boom"] :names [] :error "boom"}]
                 :garbage   ["pdb_x" "x" {:found "yes"}]}
   :gen         (gen/tuple (gen/return "pdb_x") (gen/return "x")
                           (hs/generator :hive-gimp.pdb/run-answer))
   :pred        schema/outcome?
   :num-tests   100
   :mutations   [["always ok" (fn [c p _] {:outcome :ok :command c :value {:procedure p}})]
                 ["ignores missing"
                  (fn [c p a] (real-interpret c p (assoc a :missing [])))]
                 ["string value keys"
                  (fn [c p a] (let [o (real-interpret c p a)]
                                (if (= :ok (:outcome o))
                                  (assoc-in o [:value :values] (zipmap (:names a) (:values a)))
                                  o)))]]})

(deftrifecta describe-forms hive-gimp.pdb.forms/describe-forms
  {:golden-path "test/golden/pdb/describe_forms.edn"
   :cases       {:despeckle "plug-in-despeckle"}
   :xf          py/->python
   :gen         (hs/generator :hive-gimp.pdb/procedure-name)
   :pred        #(and (vector? %) (not-any? string? %))
   :num-tests   50
   :mutations   [["no unknown-procedure guard"
                  (fn [n] (py/forms (.lookup-procedure (Gimp/get-pdb) ~n)))]]})

;; =============================================================================
;; Registration: catalogue row, send-compensated method, native fn
;; =============================================================================

(use-fixtures :each
  (fn [f]
    (let [saved @catalog/registered]
      (try (f)
           (finally (reset! catalog/registered saved))))))

(def test-spec
  {:procedure "plug-in-test-blur"
   :doc       "A test blur."
   :args      [{:name "run-mode" :kind :enum :fixed 1}
               {:name "image" :kind :image :required? true}
               {:name "radius" :kind :double :default 1.5}]})

(deftest defprocedure-registers-idempotently
  (try
    (dotimes [_ 2]
      (binding [*ns* (the-ns 'hive-gimp.pdb-test)]
        (eval `(native/defprocedure ~'test-blur '~test-spec))))
    (testing "one catalogue row, listed and described by the MCP catalogue"
      (is (= 1 (count (filter #{"pdb_plug_in_test_blur"} (catalog/command-names)))))
      (is (= "gimp_pdb_plug_in_test_blur" (:tool (catalog/descriptor "pdb_plug_in_test_blur")))))
    (testing "a send-compensated method keyed by the command"
      (is (contains? (methods client/send-compensated) "pdb_plug_in_test_blur")))
    (testing "a fn whose arglists are the required args"
      (is (= '([target image] [target image opts])
             (:arglists (meta (resolve 'hive-gimp.pdb-test/test-blur))))))
    (finally
      (remove-method client/send-compensated "pdb_plug_in_test_blur")
      (ns-unmap 'hive-gimp.pdb-test 'test-blur))))

(deftest defprocedure-refuses-an-invalid-spec
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Not a ProcedureSpec"
        (native/expansion 'x (derive/valid-spec (assoc test-spec :procedure "Bad Name"))))))

(deftest starter-set-is-native
  (is (= ["pdb_plug_in_despeckle" "pdb_gimp_drawable_invert" "pdb_file_png_export"]
         starter/commands))
  (doseq [c starter/commands]
    (is (some? (catalog/descriptor c)) c)
    (is (contains? (methods client/send-compensated) c) c))
  (is (= '([target image drawables] [target image drawables opts])
         (:arglists (meta #'starter/despeckle))))
  (is (= '([target image file] [target image file opts])
         (:arglists (meta #'starter/export-png)))))

(deftest schemas-are-registered-in-the-hive-registry
  (is (every? (set (keys (hs/registered))) (keys spec/registered-schemas)))
  (is (hs/validate :hive-gimp.pdb/procedure-spec despeckle)))

;; =============================================================================
;; The MCP-facing path, end to end over a stub transport
;; =============================================================================

(defn- sent-code [t]
  (-> (stub/sent-requests t) first (get-in ["params" "args" 1]) first))

(deftest mcp-gimp-tool-runs-a-catalogued-procedure
  (let [t      (stub/recording (stub/scripted (answer ok-answer)))
        result (tools/gimp-handler t {"command" "gimp_pdb_gimp_drawable_invert"
                                      "params"  {"drawable" 5}})]
    (is (not (:isError result)))
    (is (= 1 (count (stub/sent-requests t))))
    (is (str/includes? (sent-code t) "lookup_procedure(\"gimp-drawable-invert\")"))
    (is (str/includes? (sent-code t) "Gimp.Drawable.get_by_id(5)"))))

(deftest native-fn-and-mcp-share-one-path
  (let [t (stub/recording (stub/scripted (answer ok-answer)))]
    (is (= {:outcome :ok :command "pdb_plug_in_despeckle"
            :value   {:procedure "plug-in-despeckle" :values {}}}
           (starter/despeckle t 1 [2] {:radius 4})))
    (is (str/includes? (sent-code t) "set_core_object_array(\"drawables\""))
    (is (str/includes? (sent-code t) "set_property(\"run-mode\", 1)"))))

(deftest a-missing-object-never-reaches-gimp
  (testing "required object absent: refused before any frame"
    (let [t (stub/recording (stub/scripted))]
      (is (= :gimp/missing-parameter (:reason (client/invoke t "pdb_gimp_drawable_invert" {}))))
      (is (empty? (stub/sent-requests t)))))
  (testing "malformed ids: refused before any frame"
    (let [t (stub/recording (stub/scripted))]
      (is (= :gimp/invalid-parameter
             (:reason (client/invoke t "pdb_plug_in_despeckle" {:image 1 :drawables []}))))
      (is (empty? (stub/sent-requests t)))))
  (testing "an id naming no live object comes back named, not as a crash"
    (let [t (stub/scripted (answer (assoc ok-answer "missing" ["drawable"] "status" nil)))]
      (is (= :gimp/invalid-parameter (:reason (starter/invert-drawable t 99)))))))

;; =============================================================================
;; The facade
;; =============================================================================

(deftest describe-and-run!-through-the-facade
  (let [info-frame (answer (-> invert-info (update :args vec)))
        t          (stub/recording (stub/scripted info-frame (answer ok-answer)))]
    (is (= {:outcome :ok :command "pdb_gimp_drawable_invert"
            :value   {:procedure "gimp-drawable-invert" :values {}}}
           (pdb/run! t "gimp-drawable-invert" {:drawable 3})))
    (is (= 2 (count (stub/sent-requests t)))))
  (is (= :gimp/unknown-procedure
         (:reason (pdb/describe (stub/scripted (answer nil)) "no-such-proc"))))
  (is (= :gimp/missing-parameter
         (:reason (pdb/run! (stub/scripted (answer invert-info)) "gimp-drawable-invert" {}))))
  (is (= ["a" "b"]
         (:value (pdb/procedures (stub/scripted (answer ["a" "b"])) :match "x")))))
