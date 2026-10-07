(ns hive-gimp.verbs.io-test
  "hive-gimp.verbs.io and hive-gimp.verbs.display as trifectas over
   `program-of`, and the file-path contracts as refusals."
  (:require [clojure.test :refer [deftest is]]
            [clojure.walk :as walk]
            [hive-gimp.py :as py]
            [hive-gimp.verbs :as verbs]
            [hive-gimp.verbs.support-test :as s :refer [call]]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def img {:id 1 :type "Image" :name "logo.xcf"})

(def io-cases
  {:file-load    (call :io/file-load "/srv/art/logo.png" {})
   :file-load-as (call :io/file-load "/srv/art/logo v3.xcf" {:as 'logo})
   :save-xcf     (call :io/save-xcf img "/srv/art/logo v3.xcf")
   :export-png   (call :io/export-png 'logo "/srv/art/logo.png")})

(def display-cases
  {:display-new    (call :display/display-new img)
   :displays-flush (call :display/displays-flush)})

(deftest every-io-and-display-verb-has-a-golden-case
  (is (empty? (s/uncovered "io" io-cases)))
  (is (empty? (s/uncovered "display" display-cases))))

(deftest file-paths-are-held-to-their-extension
  (doseq [[verb path] [[:io/save-xcf "/tmp/a.png"] [:io/export-png "/tmp/a.xcf"] [:io/save-xcf "relative.xcf"]]]
    (is (= :verbs/bad-arguments
           (try (verbs/program verb img path) nil
                (catch clojure.lang.ExceptionInfo e (:hive-gimp/reason (ex-data e))))))))

(deftrifecta io-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/io.edn"
   :cases         io-cases
   :xf            py/->python
   :gen           (s/gen-call "io")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     200
   :mutations
   [["load interactive"        (s/on :io/file-load #(walk/postwalk-replace {'Gimp.RunMode/NONINTERACTIVE 'Gimp.RunMode/INTERACTIVE}
                                                                           (s/original %)))]
    ["load answers nothing"    (s/on :io/file-load #(vec (butlast (s/original %))))]
    ["save forgets Gio"        (s/on :io/save-xcf #(vec (rest (s/original %))))]
    ["save ignores the path"   (s/with-args :io/save-xcf (fn [[i _]] [i "/tmp/untitled.xcf"]))]
    ["export saves the image"  (s/with-args :io/export-png (fn [[i p]] [i (str p ".xcf")]))]]})

(deftrifecta display-verbs hive-gimp.verbs/program-of
  {:golden-path   "test/golden/hive_gimp/verbs/display.edn"
   :cases         display-cases
   :xf            py/->python
   :gen           (s/gen-call "display")
   :property-type :pred-io
   :pred          s/program-holds?
   :num-tests     100
   :mutations
   [["display of the first image" (s/on :display/display-new (fn [_] (py/forms (Gimp.Display/new (first (Gimp/get-images))))))]
    ["flush is a no-op"           (s/on :display/displays-flush (fn [_] (py/forms nil)))]]})
