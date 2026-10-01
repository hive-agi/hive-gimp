(ns hive-gimp.root-tool-contract-test
  "Every hive.gimp tool must be operable from its schema alone."
  (:require [clojure.test :refer [deftest is]]
            [hive-addon.protocol :as addon]
            [hive-addon.tool-contract :as contract]
            [hive-addon.tool-contract.test :refer [assert-root-tools]]
            [hive-gimp.addon :as gimp]))

(deftest every-gimp-tool-satisfies-the-root-contract
  (let [a (gimp/addon-ctor {})]
    (try
      (is (:success? (addon/initialize! a {})))
      (assert-root-tools (addon/tools a))
      (finally (addon/shutdown! a)))))

(deftest gimp-doctor-declares-its-host-python-switch
  (let [a (gimp/addon-ctor {})]
    (try
      (addon/initialize! a {})
      (let [doctor (first (filter #(= "gimp_doctor" (:name %)) (addon/tools a)))]
        (is (contract/conforms? doctor))
        (is (= "boolean" (get-in doctor [:inputSchema :properties "host_python" :type]))))
      (finally (addon/shutdown! a)))))
