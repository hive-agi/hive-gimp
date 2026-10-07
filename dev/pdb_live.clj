(ns pdb-live
  "LIVE end-to-end proof of the starter PDB procedures, through the
   MCP-facing path (`client/invoke` on each catalogued command), against a
   running GIMP. Every proof works on a scratch image it creates and deletes;
   no other image is read for pixels, modified, saved or closed.

     clojure -Sdeps '{:aliases {:live {:extra-paths [\"dev\"]}}}' \\
       -M:live -m pdb-live [port] [out-dir]

   or from a REPL: (load-file \"dev/pdb_live.clj\") (pdb-live/prove! g out-dir).

   Proofs:
     :invert     pdb_gimp_drawable_invert moves the VALUE mean m to 255-m and
                 pixel (0,0) from white to black.
     :despeckle  pdb_plug_in_despeckle removes isolated specks: std > 0 before,
                 0 after.
     :export     pdb_file_png_export writes a PNG whose IHDR names the scratch
                 size.
     :refusal    a missing or malformed object argument is refused in Clojure:
                 zero frames reach GIMP.
   Every proof counts the frames it sent through a recording decorator over
   the live transport; the pixel proofs are the positive control that the
   decorator sees traffic. Exits 0 only when every proof passes and the open
   image ids after equal those before."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [hive-gimp.client :as client]
            [hive-gimp.core :as gimp]
            [hive-gimp.pdb.starter]
            [hive-gimp.ports :as ports]
            [hive-gimp.py :as py]
            [hive-gimp.response :as response]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; A recording decorator over the transport port
;; =============================================================================

(defrecord Recording [inner frames]
  ports/IGimpTransport
  (round-trip! [_ frame]
    (swap! frames conj frame)
    (ports/round-trip! inner frame))
  (transport-id [_] (ports/transport-id inner)))

(defn recording
  "`transport` wrapped so every frame it is handed is counted."
  [transport]
  (->Recording transport (atom [])))

(defn sent
  "How many frames `rec` has sent."
  [rec]
  (count @(:frames rec)))

;; =============================================================================
;; Forms (pure)
;; =============================================================================

(def size 32)
(def speck-step 7)

(defn image-ids-forms
  "Forms answering the ids of every open image."
  []
  (py/forms (for [i (Gimp/get-images)] (.get-id i))))

(defn scratch-forms
  "Forms creating a `w`x`h` RGB image with one white layer carrying black
   specks every `step` pixels, answering [image-id layer-id]. GIMP's context
   is not touched."
  [w h step]
  (py/forms
   (import gi.repository [Gegl])
   (def hg-live-img (Gimp.Image/new ~w ~h Gimp.ImageBaseType/RGB))
   (def hg-live-layer (Gimp.Layer/new hg-live-img "hg-live-scratch" ~w ~h
                                      Gimp.ImageType/RGB-IMAGE 100.0 Gimp.LayerMode/NORMAL))
   (.insert-layer hg-live-img hg-live-layer nil 0)
   (.fill hg-live-layer Gimp.FillType/WHITE)
   (doseq [xy ~(vec (for [x (range 3 w step) y (range 3 h step)] [x y]))]
     (.set-pixel hg-live-layer (nth xy 0) (nth xy 1) (Gegl.Color/new "black")))
   [(.get-id hg-live-img) (.get-id hg-live-layer)]))

(defn probe-forms
  "Forms answering the VALUE histogram mean and std of drawable `id`, and the
   RGBA of its pixel (0,0)."
  [id]
  (py/forms
   (def hg-live-d (Gimp.Drawable/get-by-id ~id))
   (def hg-live-h (.histogram hg-live-d Gimp.HistogramChannel/VALUE 0.0 1.0))
   {"mean"  (nth hg-live-h 1)
    "std"   (nth hg-live-h 2)
    "pixel" (.get-rgba (.get-pixel hg-live-d 0 0))}))

(defn delete-forms
  "Forms deleting image `id` when it is open, answering the open ids after."
  [id]
  (into (py/forms
         (def hg-live-i (Gimp.Image/get-by-id ~id))
         (when (not (is hg-live-i nil)) (.delete hg-live-i)))
        (image-ids-forms)))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn eval!
  "The value of `forms` evaluated on `transport`; throws on any refusal."
  [transport forms]
  (let [outcome (py/eval-forms transport forms)]
    (if (response/ok? outcome)
      (:value outcome)
      (throw (ex-info (str "GIMP refused: " (:message outcome))
                      (dissoc outcome :python))))))

(defn with-scratch
  "Call `(f image-id layer-id)` on a fresh scratch image, deleting it after."
  [transport f]
  (let [[img layer] (eval! transport (scratch-forms size size speck-step))]
    (try (f img layer)
         (finally (eval! transport (delete-forms img))))))

(defn png-size
  "[w h] from the IHDR of the PNG at `path`, or nil when it is not a PNG."
  [path]
  (let [bs    (with-open [in (io/input-stream path)] (.readNBytes in 24))
        magic [-119 80 78 71 13 10 26 10]
        int32 (fn [o] (reduce (fn [acc i] (+ (* acc 256) (bit-and 0xff (aget bs (+ o i))))) 0 (range 4)))]
    (when (and (= 24 (alength bs)) (= magic (vec (take 8 bs))))
      [(int32 16) (int32 20)])))

;; =============================================================================
;; Proofs
;; =============================================================================

(defn- close? [a b] (< (abs (- a b)) 0.5))

(defn prove-invert
  [transport]
  (with-scratch transport
    (fn [_img layer]
      (let [rec    (recording transport)
            before (eval! transport (probe-forms layer))
            ran    (client/invoke rec "pdb_gimp_drawable_invert" {:drawable layer})
            after  (eval! transport (probe-forms layer))]
        {:proof    :invert
         :pass?    (and (response/ok? ran)
                        (close? (:mean after) (- 255.0 (:mean before)))
                        (= [1.0 1.0 1.0] (vec (take 3 (:pixel before))))
                        (= [0.0 0.0 0.0] (vec (take 3 (:pixel after)))))
         :frames   (sent rec)
         :evidence {:outcome (:outcome ran) :before before :after after}}))))

(defn prove-despeckle
  [transport]
  (with-scratch transport
    (fn [img layer]
      (let [rec    (recording transport)
            before (eval! transport (probe-forms layer))
            ran    (client/invoke rec "pdb_plug_in_despeckle"
                                  {:image img :drawables [layer] :radius 2
                                   :type "median" :black -1 :white 256})
            after  (eval! transport (probe-forms layer))]
        {:proof    :despeckle
         :pass?    (and (response/ok? ran)
                        (pos? (:std before))
                        (zero? (:std after))
                        (close? 255.0 (:mean after)))
         :frames   (sent rec)
         :evidence {:outcome (:outcome ran) :before before :after after}}))))

(defn prove-export
  [transport out-dir]
  (let [path (str (io/file out-dir "pdb-live-export.png"))]
    (io/make-parents path)
    (io/delete-file path true)
    (with-scratch transport
      (fn [img _layer]
        (let [rec   (recording transport)
              ran   (client/invoke rec "pdb_file_png_export" {:image img :file path})
              exist (.exists (io/file path))
              dims  (when exist (png-size path))]
          {:proof    :export
           :pass?    (and (response/ok? ran) (= [size size] dims))
           :frames   (sent rec)
           :evidence {:outcome (:outcome ran) :file path
                      :bytes (when exist (.length (io/file path))) :png-size dims}})))))

(def refusal-cases
  [["pdb_gimp_drawable_invert" {}                              :gimp/missing-parameter]
   ["pdb_plug_in_despeckle"    {:drawables [1]}                 :gimp/missing-parameter]
   ["pdb_file_png_export"      {:image 1}                       :gimp/missing-parameter]
   ["pdb_gimp_drawable_invert" {:drawable 0}                    :gimp/invalid-parameter]
   ["pdb_plug_in_despeckle"    {:image 1 :drawables []}         :gimp/invalid-parameter]
   ["pdb_file_png_export"      {:image 1 :file "relative.png"}  :gimp/invalid-parameter]])

(defn prove-refusal
  [transport]
  (let [rec     (recording transport)
        answers (mapv (fn [[command args expected]]
                        {:command command :args args :expected expected
                         :reason (:reason (client/invoke rec command args))})
                      refusal-cases)]
    {:proof    :refusal
     :pass?    (and (zero? (sent rec)) (every? #(= (:expected %) (:reason %)) answers))
     :frames   (sent rec)
     :evidence answers}))

(defn prove!
  "Run every proof on `session`, writing the export under `out-dir`."
  [session out-dir]
  (let [t       (or (:transport session) session)
        before  (eval! t (image-ids-forms))
        results [(prove-invert t) (prove-despeckle t) (prove-export t out-dir) (prove-refusal t)]
        after   (eval! t (image-ids-forms))]
    {:pass?   (and (every? :pass? results) (= before after))
     :images  {:before before :after after}
     :results results}))

;; =============================================================================
;; Entry point
;; =============================================================================

(defn ensure-link!
  "The session's link state, healing it first when it is not answering."
  [session]
  (let [status (gimp/status session)]
    (if (= :link/answering (:state status))
      status
      (do (gimp/heal! session)
          (gimp/status session)))))

(defn -main
  [& [port out-dir]]
  (let [session (gimp/connect {:port (or (some-> port parse-long) 9877)})
        out-dir (or out-dir (System/getProperty "java.io.tmpdir"))
        link    (ensure-link! session)]
    (println "PDB-LIVE link" (:state link))
    (let [report (prove! session out-dir)]
      (binding [pprint/*print-right-margin* 120] (pprint/pprint report))
      (shutdown-agents)
      (System/exit (if (:pass? report) 0 1)))))
