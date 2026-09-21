(ns hive-gimp.guard-test
  "The two compensations for reference-plug-in defects: the pure verdicts, and
   the pipeline that spends round trips on them.

   Both defects were measured against a live headless GIMP 3.2.4 before any of
   this was written (dev/verify_python_defects.clj re-measures them). What is
   asserted here is what hive-gimp DOES about them, against doubles, so the
   suite needs no GIMP."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.catalog :as catalog]
            [hive-gimp.client :as client]
            [hive-gimp.guard :as guard]
            [hive-gimp.response :as response]
            [hive-gimp.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private add-text (catalog/descriptor "add_text"))

(defn- fonts-frame
  [& names]
  (stub/success-frame {"fonts" (vec names) "count" (count names)}))

(defn- images-frame
  [& images]
  (stub/success-frame {"images" (vec images) "count" (count images)}))

(defn- image
  [index id width height]
  {"index" index "image_id" id "width" width "height" height
   "name" (str "Untitled_" index) "num_layers" 1})

(defn- types-sent
  [t]
  (mapv #(get % "type") (stub/sent-requests t)))

;; =============================================================================
;; The verdicts
;; =============================================================================

(deftest only-a-font-the-caller-named-is-judged-test
  (testing "a font that was typed is the one we check"
    (is (= "Lato Black" (guard/font-asked-for add-text {:font "Lato Black"})))
    (is (= "Lato Black" (guard/font-asked-for add-text {"font" "Lato Black"}))))
  (testing "a default nobody typed is not a request"
    (is (nil? (guard/font-asked-for add-text {:text "hi"}))
        "add_text defaults to Sans, which GIMP 3.2 no longer resolves; refusing it would break every call that left the font out")
    (is (nil? (guard/font-asked-for add-text {:font "   "}))))
  (testing "a command with no font parameter is never judged"
    (is (nil? (guard/font-asked-for (catalog/descriptor "new_canvas") {:font "Lato Black"})))))

(deftest an-empty-font-catalogue-is-unverifiable-not-a-refusal-test
  (testing "a gate whose universe is empty would reject every input"
    (is (= :font/unverifiable (guard/font-verdict "Lato Black" nil)))
    (is (= :font/unverifiable (guard/font-verdict "Lato Black" []))))
  (is (= :font/ok (guard/font-verdict "Lato Black" ["Sans-serif" "Lato Black"])))
  (is (= :font/unknown (guard/font-verdict "Lato Black" ["Sans-serif"]))))

(deftest a-canvas-is-adopted-only-when-it-is-unambiguously-ours-test
  (let [params {"width" 400 "height" 300 "color_mode" "RGB" "fill" "white" "resolution" 72}]
    (testing "one image appeared, at the size that was asked for"
      (let [adopted (guard/adopted-canvas params [] [(image 0 7 400 300)])]
        (is (= 7 (get adopted "image_id")))
        (is (= 0 (get adopted "image_index")))
        (is (true? (get adopted "recovered")))
        (is (false? (get adopted "display_opened"))
            "the display is the one thing that genuinely failed, and the caller is told so")
        (is (= "white" (get adopted "fill")) "the request's own parameters are carried through")))

    (testing "images that were already open are not adopted"
      (is (nil? (guard/adopted-canvas params
                                      [(image 0 7 400 300)]
                                      [(image 0 7 400 300)]))))

    (testing "two new images is an ambiguity, not a recovery"
      (is (nil? (guard/adopted-canvas params [] [(image 0 7 400 300) (image 1 8 400 300)]))))

    (testing "a new image of another size belongs to somebody else"
      (is (nil? (guard/adopted-canvas params [] [(image 0 7 640 480)]))))

    (testing "a count that was never taken cannot be differenced"
      (is (nil? (guard/adopted-canvas params nil [(image 0 7 400 300)])))
      (is (nil? (guard/adopted-canvas params [] nil))))))

;; =============================================================================
;; The pipeline
;; =============================================================================

(deftest a-font-gimp-does-not-have-is-refused-before-anything-is-drawn-test
  (let [t (stub/recording (stub/scripted (fonts-frame)
                                         (fonts-frame "Sans-serif" "Lato Black")))
        outcome (client/invoke t "add_text" {:text "Any video." :font "Lato Blakc"})]
    (is (= :gimp/unknown-font (:reason outcome)))
    (is (= ["list_fonts" "list_fonts"] (types-sent t))
        "the refusal costs two lookups and no add_text: nothing is drawn")
    (testing "the refusal names what was asked for and what to do instead"
      (is (str/includes? (:message outcome) "Lato Blakc") (:message outcome))
      (is (str/includes? (:message outcome) "Lato Black") (:message outcome))
      (is (str/includes? (:message outcome) "gimp_list_fonts") (:message outcome))
      (is (str/includes? (:detail outcome) "2 installed font(s)") (:detail outcome)))))

(deftest an-installed-font-goes-through-on-one-lookup-test
  (let [t (stub/recording (stub/scripted (fonts-frame "Lato Black")
                                         (stub/success-frame {"layer_id" 3})))
        outcome (client/invoke t "add_text" {:text "Any video." :font "Lato Black"})]
    (is (response/ok? outcome))
    (is (= ["list_fonts" "add_text"] (types-sent t))
        "the narrow lookup filters server-side, so the accepted path pays one round trip")))

(deftest a-font-nobody-named-costs-nothing-test
  (let [t (stub/recording (stub/scripted (stub/success-frame {"layer_id" 3})))]
    (is (response/ok? (client/invoke t "add_text" {:text "Any video."})))
    (is (= ["add_text"] (types-sent t))
        "no font was requested, so there is no substitution to prevent")))

(deftest a-lookup-that-could-not-be-made-lets-the-call-through-test
  (testing "the native plug-in answers no list_fonts, and refuses missing fonts itself"
    (let [t (stub/recording (stub/scripted (stub/error-frame "Unknown command: list_fonts")
                                           (stub/success-frame {"layer_id" 3})))
          outcome (client/invoke t "add_text" {:text "Any video." :font "Lato Black"})]
      (is (response/ok? outcome)
          "'I could not look' must not be reported as 'it is not there'")
      (is (= ["list_fonts" "add_text"] (types-sent t))
          "a lookup that cannot be made is not retried against the whole catalogue: the answer would be the same")))

  (testing "a catalogue that comes back empty is not a universe"
    (let [t (stub/recording (stub/scripted (fonts-frame) (fonts-frame)
                                           (stub/success-frame {"layer_id" 3})))]
      (is (response/ok? (client/invoke t "add_text" {:text "x" :font "Lato Black"}))))))

(deftest the-narrow-lookup-is-not-trusted-to-refuse-on-its-own-test
  (testing "a filter that answers nothing is overruled by the whole catalogue"
    (let [t (stub/recording (stub/scripted (fonts-frame)
                                           (fonts-frame "Lato Black")
                                           (stub/success-frame {"layer_id" 3})))
          outcome (client/invoke t "add_text" {:text "x" :font "Lato Black"})]
      (is (response/ok? outcome)
          "two independent misses are required, because a false refusal is worse than the substitution it prevents")
      (is (= ["list_fonts" "list_fonts" "add_text"] (types-sent t))))))

(deftest the-font-universe-is-not-capped-at-the-plugins-hundred-test
  (let [limit (->> (catalog/descriptor "list_fonts") :params (filter #(= "limit" (:name %))) first)]
    (is (some? limit)
        "the plugin defaults limit to 100 and truncates in silence; without this parameter the check would refuse fonts GIMP has")
    (is (= :long (:schema limit)))))

(deftest a-canvas-the-plugin-disowns-is-recovered-test
  (let [t (stub/recording (stub/scripted (images-frame)
                                         (stub/error-frame "new_canvas failed: constructor returned NULL")
                                         (images-frame (image 0 7 400 300))))
        outcome (client/invoke t "new_canvas" {:width 400 :height 300})]
    (is (response/ok? outcome)
        "the image is whole; only the display failed, and headless there is none to open")
    (is (= 7 (get (:value outcome) "image_id")))
    (is (true? (get (:value outcome) "recovered")))
    (is (= ["list_images" "new_canvas" "list_images"] (types-sent t)))))

(deftest a-canvas-that-really-failed-still-fails-test
  (let [t (stub/recording (stub/scripted (images-frame)
                                         (stub/error-frame "new_canvas failed: bad fill colour")
                                         (images-frame)))
        outcome (client/invoke t "new_canvas" {:width 400 :height 300})]
    (is (not (response/ok? outcome))
        "nothing was created, so there is nothing to adopt and the error is the truth")
    (is (str/includes? (:message outcome) "bad fill colour"))))

(deftest a-canvas-that-succeeded-is-passed-through-untouched-test
  (let [t (stub/recording (stub/scripted (images-frame)
                                         (stub/success-frame {"image_id" 7 "display_opened" true})))
        outcome (client/invoke t "new_canvas" {:width 400 :height 300})]
    (is (response/ok? outcome))
    (is (= {"image_id" 7 "display_opened" true} (:value outcome))
        "a working plug-in is not second-guessed")
    (is (= ["list_images" "new_canvas"] (types-sent t)))))

(deftest a-canvas-recovery-is-not-attempted-blind-test
  (let [t (stub/recording (stub/scripted (stub/error-frame "Unknown command: list_images")
                                         (stub/error-frame "new_canvas failed: constructor returned NULL")
                                         (stub/error-frame "Unknown command: list_images")))
        outcome (client/invoke t "new_canvas" {:width 400 :height 300})]
    (is (not (response/ok? outcome))
        "without a before-and-after count there is no way to tell which image is ours, and handing back the wrong one is worse than the error")))

(deftest a-native-only-command-names-the-plugin-that-answers-it-test
  (let [t (stub/recording
           (stub/scripted (stub/error-frame "'args'"
                                            "Traceback (most recent call last):\n  File \"gimp-mcp-plugin.py\", line 426, in execute_command\n    a = p['args']\nKeyError: 'args'\n")))
        outcome (client/invoke t "place_text" {:text "Any video."})]
    (is (= :gimp/native-only-command (:reason outcome))
        "`'args'` names neither the command nor the cause")
    (is (str/includes? (:message outcome) "place_text") (:message outcome))
    (is (str/includes? (:message outcome) "9878") (:message outcome))
    (is (str/includes? (:message outcome) "add_text") (:message outcome))))

(deftest a-real-refusal-from-the-native-plugin-survives-test
  (let [t (stub/recording
           (stub/scripted (stub/error-frame "No font named \"Montserrat\" is installed in GIMP; list_fonts shows the names it knows.")))
        outcome (client/invoke t "place_text" {:text "Any video."})]
    (is (not= :gimp/native-only-command (:reason outcome))
        "the plug-in that HAS this command refuses it for real reasons, and those must reach the caller")
    (is (str/includes? (:message outcome) "Montserrat"))))
