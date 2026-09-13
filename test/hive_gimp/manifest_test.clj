(ns hive-gimp.manifest-test
  "The mount manifest is a claim about the addon it names. These tests hold it
   to that addon: the constructor it points at must exist and build an IAddon
   whose identity, type and capabilities are the ones the manifest declares, and
   the tool names its description advertises must be the tools that addon
   publishes. A manifest is read by hosts that never load this namespace, so
   nothing else would notice it drifting."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.lifecycle.policy :as policy]
            [hive-addon.mount.boundary :as boundary]
            [hive-addon.protocol :as addon]
            [hive-dsl.result :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def manifest-resource "META-INF/hive-addons/hive-gimp.edn")

(defn- manifest-text [] (slurp (io/resource manifest-resource)))

(deftest the-manifest-is-a-valid-mount-spec
  (let [parsed (boundary/parse-spec (manifest-text))]
    (is (r/ok? parsed) (pr-str (:explanation parsed)))))

(deftest the-classpath-scan-finds-exactly-one-hive-gimp-spec
  (let [{:keys [specs]} (boundary/discover-specs)]
    (is (= 1 (count (filter #(= "hive.gimp" (:addon/id %)) specs))))))

(deftest the-constructor-builds-the-addon-the-manifest-describes
  (let [spec (:ok (boundary/parse-spec (manifest-text)))
        res  (boundary/resolve-constructor spec)]
    (is (= :resolved (:constructor/status res)) (:constructor/error res))
    (let [a ((:constructor res) (:addon/config spec))]
      (testing "identity, type and capabilities agree with the manifest"
        (is (satisfies? addon/IAddon a))
        (is (= (:addon/id spec) (addon/addon-id a)))
        (is (= (:addon/type spec) (addon/addon-type a)))
        (is (= (:addon/capabilities spec) (addon/capabilities a))))
      (testing "the tools named in the description are the tools published"
        (try
          (is (:success? (addon/initialize! a {})))
          (let [published (set (map :name (addon/tools a)))
                advertised (set (re-seq #"gimp(?:_[a-z]+)?"
                                        (second (re-find #"\(([^)]*)\)" (:addon/description spec)))))]
            (is (= 5 (count published)))
            (is (= published advertised)))
          (finally (addon/shutdown! a)))))))

(deftest the-lifecycle-resolves-to-lazy-with-a-fifteen-minute-idle
  (let [spec (edn/read-string (manifest-text))]
    (is (= {:policy :lazy :idle-ms 900000}
           (policy/resolve-lifecycle spec nil nil)))
    (testing "no surface is declared, so the host learns it from the first mount"
      (is (nil? (:addon/surface spec))))
    (testing "a host override still wins over the manifest"
      (is (= :pinned (:policy (policy/resolve-lifecycle spec nil {:policy :pinned})))))))

(deftest the-description-mentions-no-tool-twice
  (let [names (re-seq #"gimp(?:_[a-z]+)?"
                      (second (re-find #"\(([^)]*)\)"
                                       (:addon/description (edn/read-string (manifest-text))))))]
    (is (= (count names) (count (distinct names))))
    (is (every? (complement str/blank?) names))))
