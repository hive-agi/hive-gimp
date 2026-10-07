(ns hive-gimp.verbs-test
  "The verbs BOUNDARY through the transport port's doubles: composition is
   ONE round trip, `call` holds arguments and answers to the verb's schemas
   and answers an `Outcome` instead of throwing. Plus the value objects'
   schemas against the shapes GIMP 3.2 answered live."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.py :as py]
            [hive-gimp.schema :as schema]
            [hive-gimp.stub :as stub]
            [hive-gimp.verbs :as verbs]
            [hive-gimp.verbs.image :as image]
            [hive-gimp.verbs.layer :as layer]
            [hive-gimp.verbs.paint :as paint]
            [hive-gimp.verbs.selection :as selection]
            [hive-gimp.verbs.value :as v]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- answer
  "The frame GIMP's exec sends back for an eval answering `value`."
  [value]
  (stub/success-frame [(str "\u001ehive-gimp.py " (json/write-str value) "\n")]))

(def img {:id 1 :type "Image" :name "logo.xcf"})

;; Captured from GIMP 3.2 on 2026-10-07, on a scratch image.
(def live-ref     {"id" 4993 "type" "Layer" "name" nil})
(def live-size    {"width" 64 "height" 48})
(def live-offsets {"x" 5 "y" 7})
(def live-bounds  {"empty?" false "x" 1 "y" 2 "width" 12 "height" 10})
(def live-rgba    {"r" 1.0 "g" 1.0 "b" 1.0 "a" 1.0})

(deftest live-answers-satisfy-the-value-objects
  (let [kw (fn [m] (into {} (map (fn [[k x]] [(keyword k) x])) m))]
    (is (m/validate v/Ref (kw live-ref)))
    (is (m/validate v/Size (kw live-size)))
    (is (m/validate v/Offsets (kw live-offsets)))
    (is (m/validate v/Bounds (kw live-bounds)))
    (is (m/validate v/Rgba (kw live-rgba)))))

(deftest a-composed-program-is-one-round-trip
  (let [t   (stub/recording (stub/scripted (answer live-ref)))
        out (verbs/run {:transport t}
                       (layer/new-layer img {:name "corpo v3" :as 'body})
                       (paint/edit-fill-colour 'body "#2a4b8d")
                       (layer/set-opacity 'body 80))]
    (is (= :ok (:outcome out)))
    (is (= {:id 4993 :type "Layer" :name nil} (:value out)))
    (is (= 1 (count (stub/sent-frames t))) "three verbs, one frame")
    (let [sent (str/join (stub/sent-frames t))]
      (is (str/includes? sent "body = Gimp.Layer.new("))
      (is (str/includes? sent "body.edit_fill(Gimp.FillType.FOREGROUND)"))
      (is (str/includes? sent "body.set_opacity(80.0)")))))

(deftest compose-concatenates-in-order
  (let [a (image/get-images) b (image/get-size img)]
    (is (= (into a b) (verbs/compose a b)))
    (is (= [] (verbs/compose)))))

(deftest call-answers-a-labelled-outcome
  (let [out (verbs/call (stub/scripted (answer live-size)) :image/get-size img)]
    (is (schema/outcome? out))
    (is (= {:outcome :ok :command "verb_image_get_size" :value {:width 64 :height 48}} out))))

(deftest call-refuses-an-answer-the-verb-does-not-promise
  (let [out (verbs/call (stub/scripted (answer {"width" 64})) :image/get-size img)]
    (is (schema/outcome? out))
    (is (= :verbs/unexpected-answer (:reason out)))))

(deftest call-refuses-bad-arguments-before-sending
  (let [t   (stub/recording (stub/scripted))
        out (verbs/call t :layer/set-opacity img 250)]
    (is (schema/outcome? out))
    (is (= :verbs/bad-arguments (:reason out)))
    (is (empty? (stub/sent-frames t)) "nothing reached GIMP")))

(deftest call-refuses-an-unknown-verb
  (let [out (verbs/call (stub/scripted) :layer/merge-down img)]
    (is (= :verbs/unknown-verb (:reason out)))
    (is (= "verb_layer_merge_down" (:command out)))))

(deftest a-transport-failure-is-an-error-outcome
  (let [out (verbs/call (stub/failing :gimp/connection-refused) :image/get-images)]
    (is (= :error (:outcome out)))
    (is (schema/outcome? out))))

(deftest run!-answers-the-value-or-throws
  (is (= [{:id 1 :type "Image" :name nil}]
         (verbs/run! (stub/scripted (answer [{"id" 1 "type" "Image" "name" nil}])) (image/get-images))))
  (is (thrown? clojure.lang.ExceptionInfo
               (verbs/run! (stub/scripted (stub/error-frame "boom")) (selection/none img)))))

(deftest program-and-program-of-agree
  (is (= (verbs/program :layer/set-name img "x")
         (verbs/program-of {:verb :layer/set-name :args [img "x"]})
         (layer/set-name img "x"))))

(deftest handles-reach-objects-by-type
  (is (= "Gimp.Image.get_by_id(1)"   (py/->python [(v/handle-form img)])))
  (is (= "Gimp.Item.get_by_id(9)"    (py/->python [(v/handle-form {:id 9 :type "TextLayer" :name nil})])))
  (is (= "Gimp.Display.get_by_id(2)" (py/->python [(v/handle-form {:id 2 :type "Display" :name nil})])))
  (is (= "body"                      (py/->python [(v/handle-form 'body)]))))

(deftest enumerations-refuse-a-stranger
  (is (= 'Gimp.ChannelOps/ADD (v/enum-form v/channel-ops :add)))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not one of"
                        (v/enum-form v/fill-types :gradient))))
