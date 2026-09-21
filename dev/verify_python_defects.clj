(ns verify-python-defects
  "Live measurement of the two reference-plug-in defects hive-gimp guards against.

   Run it:  clojure -M -i dev/verify_python_defects.clj

   Preconditions: a GIMP 3.x with the reference Python plug-in listening, started
   HEADLESS, because both defects only exist headless:

     flatpak run org.gimp.GIMP -n -i -d --batch-interpreter python-fu-eval \\
       -b \"exec(open('dev/start_mcp.py').read())\"

   (No -f: -f loads no fonts, and a run with no fonts cannot tell a refusal
   earned by a missing font from one earned by an empty font list.)

   Why a live probe and not a unit test: both defects are behaviours of a
   plug-in we do not ship. The suite proves what hive-gimp DOES with those
   behaviours, against a double. This file proves the behaviours are still
   there, i.e. that the double is not describing a plug-in that no longer
   exists, and it measures the guard against the real thing.

   Everything is measured, never asserted from the response alone:

     font     read back off the text layer inside GIMP, because the plug-in's
              add_text response does not echo the font it used. A silent
              substitution is INVISIBLE in the response by construction.
     leak     Gimp.get_images() counted before and after, because the plug-in
              reports failure and keeps the image, so the outcome alone cannot
              distinguish a failed call from a leaking one.

   Exits non-zero when a guard does not hold."
  (:require [clojure.string :as str]
            [hive-gimp.core :as gimp]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private missing-font
  "A font no system installs, so the probe cannot pass by accident on a box that
   happens to have the name we asked for."
  "Nonexistent Hive Probe Face")

(def session (gimp/connect {:port 9877 :timeout-ms 120000}))

(def results (atom []))

(defn- record!
  [label pass? detail]
  (swap! results conj {:label label :pass? pass?})
  (println (format "  %-46s %-4s %s" label (if pass? "PASS" "FAIL") detail)))

(defn- printed
  "Read a value out of GIMP. exec captures stdout, so this is the only way to
   observe state the command responses do not carry."
  [code]
  (let [o (gimp/exec session [code])]
    (when (gimp/ok? o) (some-> (last (gimp/value o)) str/trim not-empty))))

(defn- image-count [] (some-> (printed "print(len(Gimp.get_images()))") parse-long))

(defn- purge-images!
  "Headless only: the images have no display, so delete() is safe and is the
   only thing that clears them (close_image is dead code against GIMP 3.2)."
  []
  (gimp/exec session ["for _i in list(Gimp.get_images()): _i.delete()"])
  (image-count))

(defn- layer-font
  [layer-index]
  (printed (format "print(Gimp.get_images()[0].get_layers()[%d].get_font().get_name())"
                   layer-index)))

(println)
(println "hive-gimp: reference-plug-in defect probe")
(println "doctor:" (:summary (gimp/doctor session)))
(purge-images!)

;; ---------------------------------------------------------------------------
(println)
(println "DEFECT (b)  headless new_canvas: the plug-in reports failure and keeps the image")

(def canvas
  (let [before  (image-count)
        outcome (gimp/invoke session "new_canvas" {:width 400 :height 300 :fill "white"})
        after   (image-count)]
    {:outcome outcome :before before :after after}))

(println (format "  raw plug-in behaviour: images %s -> %s" (:before canvas) (:after canvas)))

(record! "new_canvas does not orphan an image"
         (or (gimp/ok? (:outcome canvas))
             (= (:before canvas) (:after canvas)))
         (if (gimp/ok? (:outcome canvas))
           (str "adopted image_index " (get (gimp/value (:outcome canvas)) "image_index")
                ", display_opened " (get (gimp/value (:outcome canvas)) "display_opened"))
           (str (:reason (:outcome canvas)) " and " (- (:after canvas) (:before canvas))
                " image(s) left behind")))

(record! "the recovered canvas is usable"
         (let [o (gimp/invoke session "get_image_metadata" {})]
           (and (gimp/ok? o)
                (= 400 (get-in (gimp/value o) ["basic" "width"]))
                (= 300 (get-in (gimp/value o) ["basic" "height"]))))
         "get_image_metadata reports 400x300")

;; Layers are counted as a DELTA. A fresh canvas already carries its background
;; layer, so asserting zero would fail for a reason that has nothing to do with
;; the refusal under test.
(def layers-before-refusal
  (printed "print(len(Gimp.get_images()[0].get_layers()))"))


;; ---------------------------------------------------------------------------
(println)
(println "DEFECT (a)  add_text: an uninstalled font is substituted, and the response hides it")

(def fonts
  (let [o (gimp/invoke session "list_fonts" {:limit 5000})]
    (when (gimp/ok? o) (get (gimp/value o) "fonts"))))

(record! "list_fonts answers a non-empty catalogue"
         (boolean (seq fonts))
         (str (count fonts) " fonts, e.g. " (pr-str (vec (take 3 fonts)))))

(def refused (gimp/invoke session "add_text"
                          {:text "probe" :x 10 :y 10 :font missing-font :size 24}))

(record! "add_text with an uninstalled font is refused"
         (and (not (gimp/ok? refused)) (= :gimp/unknown-font (:reason refused)))
         (str (:reason refused) " " (pr-str (some-> (:message refused) (subs 0 (min 90 (count (:message refused))))))))

(record! "the refusal created no layer"
         (= layers-before-refusal
            (printed "print(len(Gimp.get_images()[0].get_layers()))"))
         (str "layers " layers-before-refusal " -> "
              (printed "print(len(Gimp.get_images()[0].get_layers()))")))


;; The control: an INSTALLED font must still go through, or the guard would be
;; passing by refusing everything.
(def installed-font (first fonts))

(def accepted (gimp/invoke session "add_text"
                           {:text "probe" :x 10 :y 10 :font installed-font :size 24}))

(record! "an installed font still goes through"
         (gimp/ok? accepted)
         (str (pr-str installed-font) " -> " (:outcome accepted)))

(record! "the layer carries the font that was asked for"
         (= installed-font (layer-font 0))
         (str "asked " (pr-str installed-font) ", layer reports " (pr-str (layer-font 0))))

;; ---------------------------------------------------------------------------
(println)
(println "DEFECT (c)  the reference add_text has no anchor, and its stand-in is native-only")

(def layers-before-place
  (printed "print(len(Gimp.get_images()[0].get_layers()))"))

(def placed (gimp/invoke session "place_text" {:text "probe" :x 200 :y 150 :anchor "center"}))

(record! "place_text says which plug-in answers it"
         (= :gimp/native-only-command (:reason placed))
         (str (:reason placed) " "
              (pr-str (some-> (:message placed)
                              (subs 0 (min 70 (count (str (:message placed)))))))))

(record! "and the reference plug-in drew nothing"
         (= layers-before-place
            (printed "print(len(Gimp.get_images()[0].get_layers()))"))
         (str "layers " layers-before-place " -> "
              (printed "print(len(Gimp.get_images()[0].get_layers()))")))


;; ---------------------------------------------------------------------------
(println)
(purge-images!)
(let [rs @results
      failed (remove :pass? rs)]
  (println (format "%d passed, %d failed" (- (count rs) (count failed)) (count failed)))
  (doseq [r failed] (println "  failed:" (:label r)))
  (flush)
  (System/exit (if (seq failed) 1 0)))
