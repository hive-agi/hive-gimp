(ns hive-gimp.tools
  "FACADE. The MCP surface.

   FOUR tools, not eighty, and the arithmetic is the reason. The reference
   project publishes one MCP tool per GIMP command; eighty tool definitions
   with their descriptions and schemas is a large, permanent tax on the context
   of every client that mounts this addon, whether or not it ever touches GIMP.
   The commands are DATA here, so one dispatching tool carries all of them and
   the catalog is queried on demand instead of being recited up front.

     gimp          run any catalogued command
     gimp_exec     arbitrary Python-Fu, the escape hatch
     gimp_catalog  list, search and describe commands (the discovery path that
                   replaces eighty descriptions)
     gimp_doctor   why is this not working

   Adding a GIMP command adds a row to commands_extra.edn and changes nothing
   here (OCP)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.doctor :as doctor]
            [hive-gimp.exec :as exec]
            [hive-gimp.response :as response]
            [hive-gimp.pixel :as pixel]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Rendering
;; =============================================================================

(defn- text
  [s]
  {:content [{:type "text" :text (if (string? s) s (json/write-str s))}]})

(defn- failure
  "An `Outcome` error as an MCP error result.

   The reason keyword is rendered QUALIFIED (`gimp/no-image`, not `no-image`).
   It is the only stable handle an agent has for deciding whether to retry,
   open an image, or give up, and the namespace is the half that makes it
   unambiguous; the human-readable message is not stable across GIMP versions."
  [outcome]
  {:isError true
   :content [{:type "text"
              :text (str (symbol (:reason outcome)) ": " (:message outcome)
                         (when-let [d (:detail outcome)] (str "\n" d)))}]})

(defn- image-payload
  "The base64 image inside a `get_image_bitmap` result, or nil.

   Promoted to MCP image content rather than handed back as a JSON blob: a
   base64 string in a text field is both unusable to the model and enormous."
  [value]
  (when (and (map? value) (string? (get value "image_data")))
    {:content [{:type "image"
                :data (get value "image_data")
                :mimeType (str "image/" (or (get value "format") "png"))}]}))

(defn- render
  [outcome]
  (if-not (response/ok? outcome)
    (failure outcome)
    (let [value (:value outcome)]
      (or (image-payload value)
          (text value)))))

;; =============================================================================
;; Argument normalization
;; =============================================================================

(defn normalize-args
  "A JSON params object to the kebab-case keyword map the promote layer wants.

   Accepts either spelling for every key. An MCP client that read the wire
   contract sends `image_index`; one that read the catalog sends `image-index`.
   Punishing either would be a puzzle, not a contract."
  [params]
  (into {}
        (map (fn [[k v]] [(keyword (str/replace (name k) "_" "-")) v]))
        (or params {})))

(defn- str-keys
  "MCP hands params with string keys; internal callers use keywords."
  [params]
  (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) v])) (or params {})))

;; =============================================================================
;; gimp
;; =============================================================================

(defn gimp-handler
  [transport params]
  (let [p       (str-keys params)
        command (get p "command")
        args    (normalize-args (get p "params"))]
    (if (str/blank? (str command))
      {:isError true
       :content [{:type "text"
                  :text "gimp requires a command. Use gimp_catalog to list or search the available commands."}]}
      (render (client/invoke transport command args)))))

(defn gimp-tool
  [transport]
  {:name "gimp"
   :description
   (str "Run a GIMP image-editing command. " (count (catalog/commands))
        " commands cover files, adjustments, transforms, selections, layers, drawing, text, filters and export. "
        "Pass the command name and its arguments as `params`. "
        "Use gimp_catalog to discover commands and their parameters, gimp_exec for anything not catalogued, "
        "and gimp_doctor if a call fails for reasons that do not name a parameter.")
   :inputSchema {:type "object"
                 :additionalProperties false
                 :properties {"command" {:type "string"
                                         :description "The GIMP command to run, for example auto_levels or export_image."}
                              "params"  {:type "object"
                                         :description "Arguments for the command. Keys accept either snake_case or kebab-case."}}
                 :required ["command"]}
   :annotations {:readOnlyHint false :destructiveHint true
                 :idempotentHint false :openWorldHint true}
   :handler (partial gimp-handler transport)})

;; =============================================================================
;; gimp_exec
;; =============================================================================

(defn exec-handler
  [transport params]
  (let [p    (str-keys params)
        code (get p "code")
        code (cond
               (string? code) [code]
               (sequential? code) (vec code)
               :else nil)
        mode (if (= "eval" (get p "mode")) :eval :exec)]
    (if (or (nil? code) (empty? code) (not (every? string? code)))
      {:isError true
       :content [{:type "text" :text "gimp_exec requires `code`: a Python string or an array of Python strings."}]}
      (render (exec/run transport code {:mode mode
                                        :preamble? (boolean (get p "preamble"))})))))

(defn exec-tool
  [transport]
  {:name "gimp_exec"
   :description
   (str "Execute Python inside GIMP's own interpreter (GIMP 3 PyGObject API). The escape hatch for anything "
        "the catalogued commands do not cover. State persists between calls, so imports and variables survive. "
        "mode=exec returns captured stdout; mode=eval returns str() of each expression. "
        "Set preamble=true to have image/layer/drawable bound to the first open image before your code runs. "
        "Drawing operations do not appear until Gimp.displays_flush() runs.")
   :inputSchema {:type "object"
                 :additionalProperties false
                 :properties {"code" {:type "array"
                                      :items {:type "string"}
                                      :description "Python statements, executed in order in one shared context."}
                              "mode" {:type "string" :enum ["exec" "eval"]
                                      :description "exec (default) runs statements; eval returns the value of each expression."}
                              "preamble" {:type "boolean"
                                          :description "Bind images/image/layers/layer/drawable to the first open image first."}}
                 :required ["code"]}
   :annotations {:readOnlyHint false :destructiveHint true
                 :idempotentHint false :openWorldHint true}
   :handler (partial exec-handler transport)})

;; =============================================================================
;; gimp_catalog
;; =============================================================================

(defn catalog-handler
  "Discovery. Never touches GIMP: every answer comes from the loaded contract.

   The projection itself lives in `hive-gimp.contract/describe`, reached here
   through the catalog accessor. Keeping it out of this namespace is what lets
   it be generatively tested; a wiring namespace is a poor place for a decision
   about what clients may depend on."
  [params]
  (let [p       (str-keys params)
        command (get p "command")
        query   (get p "query")]
    (cond
      (seq (str command))
      (if-let [d (catalog/descriptor command)]
        (text (catalog/describe d))
        {:isError true
         :content [{:type "text"
                    :text (str "No GIMP command named " (pr-str command) ". Search with `query` instead.")}]})

      (seq (str query))
      (let [hits (catalog/search query)]
        (text {:query    query
               :count    (count hits)
               :commands (mapv catalog/describe hits)}))

      :else
      (text {:count    (count (catalog/commands))
             :commands (catalog/command-names)
             :hint     "Pass `query` to search names and descriptions, or `command` for one command's full parameter list."}))))

(defn catalog-tool
  []
  {:name "gimp_catalog"
   :description
   (str "Discover GIMP commands. No arguments lists every command name; `query` searches names and descriptions; "
        "`command` returns one command's parameters, types, defaults and documentation. "
        "This is the discovery path for the `gimp` tool.")
   :inputSchema {:type "object"
                 :additionalProperties false
                 :properties {"query"   {:type "string" :description "Substring to search command names and descriptions."}
                              "command" {:type "string" :description "A command name, to get its full parameter list."}}}
   :annotations {:readOnlyHint true :destructiveHint false
                 :idempotentHint true :openWorldHint false}
   :handler (fn [params] (catalog-handler params))})

;; =============================================================================
;; gimp_doctor
;; =============================================================================

(defn doctor-tool
  [transport host-python]
  {:name "gimp_doctor"
   :description
   (str "Diagnose the GIMP connection: is the contract loaded, is the plugin listening and answering, "
        "is GIMP a version this contract supports, and is the optional host-side Python available. "
        "Run this when a gimp call fails for a reason that does not name a parameter.")
   :inputSchema {:type "object" :additionalProperties false :properties {}}
   :annotations {:readOnlyHint true :destructiveHint false
                 :idempotentHint true :openWorldHint true}
   :handler (fn [_] (text (doctor/report {:transport transport :host-python host-python})))})

(defn pixel-handler
  [transport host-python params]
  (let [p       (str-keys params)
        command (get p "command")]
    (case command
      "image_info"
      (if-let [path (get p "path")]
        (render (pixel/image-info host-python path))
        {:isError true :content [{:type "text" :text "image_info requires `path`."}]})

      "remove_background"
      (let [in (get p "path") out (get p "output")]
        (if (and in out)
          (render (pixel/remove-background host-python in out))
          {:isError true
           :content [{:type "text" :text "remove_background requires `path` and `output`."}]}))

      "remove_background_in_gimp"
      (render (pixel/remove-background-in-gimp
               transport host-python
               {:image-index (or (get p "image_index") 0)}))

      {:isError true
       :content [{:type "text"
                  :text (str "Unknown pixel command " (pr-str command)
                             ". One of: image_info, remove_background, remove_background_in_gimp.")}]})))

(defn pixel-tool
  "Host-side pixel work, and its composition with GIMP.

   A separate tool from `gimp` because these are NOT GIMP commands: they run in
   the host's Python, on files, using packages GIMP does not ship. Folding them
   into the `gimp` command enum would tell a client they are available whenever
   GIMP is, which is exactly wrong: they need the optional Python port and GIMP
   commands do not."
  [transport host-python]
  {:name "gimp_pixel"
   :description
   (str "Pixel operations GIMP has no procedure for, run in the host's Python (rembg, PIL). "
        "remove_background_in_gimp is the useful one: it exports the open image, removes its "
        "background with a segmentation model, and opens the result back in GIMP, replacing the "
        "iterative fuzzy-select threshold search this otherwise requires. "
        "Needs the optional Python port; gimp_doctor reports whether it is available.")
   :inputSchema {:type "object"
                 :additionalProperties false
                 :properties {"command" {:type "string"
                                         :enum ["image_info" "remove_background" "remove_background_in_gimp"]}
                              "path"    {:type "string" :description "Input image file."}
                              "output"  {:type "string" :description "Output file, for remove_background."}
                              "image_index" {:type "integer" :description "Which open GIMP image, for remove_background_in_gimp."}}
                 :required ["command"]}
   :annotations {:readOnlyHint false :destructiveHint false
                 :idempotentHint false :openWorldHint true}
   :handler (partial pixel-handler transport host-python)})

;; =============================================================================
;; Surface
;; =============================================================================

(defn tools
  "Every tool this addon publishes.

   Five, carrying eighty GIMP commands plus the host-side pixel operations.
   The count is the point: one tool definition per GIMP command would be a
   large permanent tax on the context of every client that mounts this addon,
   whether or not it ever opens an image."
  [transport host-python]
  [(gimp-tool transport)
   (exec-tool transport)
   (catalog-tool)
   (pixel-tool transport host-python)
   (doctor-tool transport host-python)])
