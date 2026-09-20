(ns hive-gimp.actionable-errors-test
  "A refusal has to tell the reader what to do next.

   The contract ships ~80 commands in two vocabularies (`place_text` and
   `gimp_place_text`), which is the size where a typo is likely and where
   `No GIMP command named \"place_txt\".` is of no use at all. The message is
   built with hive-help, the fleet's shared error vocabulary, so the shape is
   the same one hive-carto and hive-mcp use.

   Boundary only. `hive-gimp.command` and `hive-gimp.response` are .cljc and
   answer error VALUES; they must never require hive-help, and
   hive-gimp.portability-test enforces that."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]))

(deftest a-mistyped-command-suggests-the-real-one-test
  (let [{:keys [outcome reason message detail]}
        (client/unknown-command-outcome "place_txt")]
    (is (= :error outcome))
    (is (= :gimp/unknown-command reason))
    (testing "it names what was asked for"
      (is (str/includes? message "place_txt") message))
    (testing "it offers the nearest real command"
      (is (str/includes? message "place_text") message))
    (testing "it says where the whole list is, with its size"
      (is (str/includes? detail (str (count (catalog/command-names)))) detail)
      (is (str/includes? detail "gimp_catalog") detail))))

(deftest the-suggestion-is-edit-distance-not-prefix-test
  (testing "a transposition still finds it"
    (is (str/includes? (:message (client/unknown-command-outcome "new_cavnas"))
                       "new_canvas")))
  (testing "a missing underscore still finds it"
    (is (str/includes? (:message (client/unknown-command-outcome "exportimage"))
                       "export_image"))))

(deftest the-tool-vocabulary-is-suggested-too-test
  (testing "a caller using gimp_-prefixed names gets those back"
    (let [message (:message (client/unknown-command-outcome "gimp_place_txt"))]
      (is (str/includes? message "place_text") message))))

(deftest a-known-command-is-not-refused-test
  (doseq [c ["new_canvas" "place_text" "export_image" "close_image"]]
    (is (some? (catalog/descriptor c))
        (str c " stopped resolving, so the refusal path would be reached for a real command"))))
