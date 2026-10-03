(ns hive-gimp.compose
  "BOUNDARY. Commands the listening plug-in cannot answer, answered over exec.

   `hive-gimp.polyfill` writes the GIMP 3 Python as data; this namespace sends
   it and reads the answer back. It plugs into `client/send-compensated`, the
   multimethod that already exists for exactly this (a defect of a plug-in we
   do not own is a new defmethod, never an edit to `invoke`), so nothing in the
   pipeline changes to gain these commands.

   A namespace of its own because the exec channel lives in `hive-gimp.exec`,
   which requires `hive-gimp.client`: the client cannot require exec back. The
   addon requires this namespace, which is what installs the methods.

   `place_image` is tried natively FIRST: the native plug-in answers it with its
   own implementation, and the composed program is the fallback for the
   reference Python plug-in, recognised by the same `KeyError: 'args'` the
   native-only diagnosis already reads."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-gimp.client :as client]
            [hive-gimp.exec :as exec]
            [hive-gimp.guard :as guard]
            [hive-gimp.polyfill :as polyfill]
            [hive-gimp.response :as response]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- printed-value
  "The JSON object a polyfill program printed last, from an exec `Outcome`.

   Exec answers one stdout string per statement; every program ends with one
   `print(json.dumps(...))`, so the last non-blank entry is the value."
  [value]
  (some->> (if (sequential? value) value [value])
           (map str)
           (remove str/blank?)
           last
           str/trim
           json/read-str))

(defn run-program
  "Run a polyfill `program` for `command`. Returns an `Outcome`.

   An exec failure keeps its own reason (a GIMP traceback is the most useful
   thing a caller can get) but is relabelled with the command it was running,
   so the caller is not told that `exec` failed when it asked for
   `place_image`."
  [transport command program]
  (let [outcome (exec/run transport program)]
    (if-not (response/ok? outcome)
      (assoc outcome :command command)
      (try
        {:outcome :ok
         :command command
         :value   (printed-value (:value outcome))}
        (catch Exception e
          {:outcome :error
           :command command
           :reason  :gimp/unreadable-answer
           :message (str "GIMP ran " command " but its answer was not the JSON the program prints: "
                         (ex-message e))
           :detail  (pr-str (:value outcome))})))))

(defn- refuse
  [{:keys [command message]}]
  {:outcome :error
   :command command
   :reason  :gimp/invalid-parameter
   :message message})

(defmethod client/send-compensated "place_image"
  [transport gimp-command]
  (let [params (:params gimp-command)]
    (if-let [problem (polyfill/place-image-problem params)]
      (refuse problem)
      (let [outcome (client/send-command transport gimp-command)]
        (if (guard/answered-by-another-plugin? outcome)
          (run-program transport "place_image" (polyfill/place-image-program params))
          outcome)))))

(defmethod client/send-compensated "color_to_alpha"
  [transport gimp-command]
  (run-program transport "color_to_alpha"
               (polyfill/color-to-alpha-program (:params gimp-command))))
