(ns hive-gimp.python-interop-test
  "The host-side Python in hive-gimp.transport.python is driven through
   libpython-clj interop, with no Python source text. These run without
   libpython-clj on the classpath: they check the registry and the source
   file, not a live interpreter (dev/verify_python_transport.clj and a REPL on
   the :python alias do that)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.transport.python :as tp]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(deftest every-pixel-operation-is-an-interop-fn-test
  (testing "the [module attr] pairs hive-gimp.pixel calls through the port"
    (doseq [path [["hive_gimp_pixel" "image_info"]
                  ["hive_gimp_pixel" "remove_background"]]]
      (let [op (get-in tp/embedded-modules path)]
        (is (var? op) (str path " is not registered"))
        (is (fn? (some-> op deref)) (str path " is not a fn"))))))

(deftest no-python-source-text-is-executed-test
  (let [src (slurp (io/file "src/hive_gimp/transport/python.clj"))]
    (doseq [builtin ["\"exec\"" "\"eval\"" "\"compile\""]]
      (is (not (str/includes? src builtin))
          (str "transport.python calls the Python builtin " builtin
               "; drive Python through interop instead of source text")))))
