;; Live end-to-end against a REAL GIMP 3.x, over the real plugin socket.
;;
;; Everything else in this repo is proved against a scripted double. This file
;; is the one place the double is not allowed: it is the proof that the wire
;; contract we derived from the reference project matches the plugin GIMP
;; actually loads.
;;
;; Preconditions, each of which the run reports rather than assumes:
;;   1. GIMP 3.x is running. Headless is fine; a display unlocks more (below).
;;   2. gimp-mcp-plugin.py is in <Gimp.directory()>/plug-ins/gimp-mcp-plugin/,
;;      executable. ASK GIMP for that path, do not guess it. A flatpak GIMP
;;      answers with the HOST ~/.config/GIMP/<ver>, NOT the sandbox's private
;;      ~/.var/app/org.gimp.GIMP/config/GIMP/<ver>, because the manifest grants
;;      xdg-config/GIMP:create. Installing into the sandbox path leaves the
;;      plugin unregistered and every command answering :gimp/unknown-command.
;;   3. The plugin's socket server is started. run() blocks in a GLib main loop,
;;      so a batch needs no keep-alive:
;;
;;        HEADLESS   flatpak run org.gimp.GIMP -n -i -d -f \
;;                     --batch-interpreter python-fu-eval \
;;                     -b "exec(open('dev/start_mcp.py').read())"
;;
;;        HEADED     same without -i (and without -d -f). A GUI window opens
;;                   and the batch still runs, so no menu click is needed.
;;
;;   clojure -M -i dev/verify_live_gimp.clj
;;
;; Exits non-zero when any CONTRACT check fails, so it can gate a release.
;;
;; Three sections, and the split is the point:
;;
;;   CONTRACT   what hive-gimp promises, over a transport that needs no
;;              display. Always runs. Gates.
;;   DISPLAY    the commands that open, export or flush a display. Runs only
;;              when GIMP has one, SKIPS otherwise, and gates when it runs.
;;              Skipped checks are neither passes nor failures: counting a
;;              skip as a pass is how a headless CI comes to believe it covers
;;              the display path.
;;   FINDINGS   what the reference plugin does, defects included. Printed,
;;              never gates: a release of THIS library must not be blocked by
;;              a defect in a plugin we do not ship.

(require '[hive-gimp.core :as gimp]
         '[hive-gimp.config :as config]
         '[hive-gimp.exec :as exec]
         '[clojure.java.io :as io]
         '[clojure.string :as str])

(def results (atom []))
(def findings (atom []))

(def work-dir
  "Shared between the JVM and GIMP. The flatpak manifest grants /tmp, so a file
   GIMP writes here is a file this process can stat. That is itself part of
   what the export round trip proves."
  (io/file "/tmp/hive-gimp-verify"))

(defn- record!
  [label status detail]
  (swap! results conj {:label label :status status :detail detail})
  (println (format "  %-40s %-4s %s"
                   label
                   (case status :pass "PASS" :fail "FAIL" :skip "SKIP")
                   (let [s (str detail)]
                     (if (> (count s) 84) (str (subs s 0 84) " ...") s)))))

(defn- check!
  "Run thunk, judge its Outcome with ok-pred. An exception is a FAIL, never a
   crash: one broken command must not hide the checks after it."
  [label ok-pred thunk]
  (try
    (let [outcome (thunk)]
      (record! label (if (ok-pred outcome) :pass :fail) outcome))
    (catch Throwable t
      (record! label :fail (str (.getSimpleName (class t)) ": " (.getMessage t))))))

(defn- skip!
  [label why]
  (record! label :skip why))

(defn- finding!
  [label detail]
  (swap! findings conj {:label label :detail detail})
  (println (format "  %-40s %s" label detail)))

(defn- value-contains?
  [outcome & fragments]
  (let [s (str/lower-case (str (gimp/value outcome)))]
    (every? #(str/includes? s (str/lower-case %)) fragments)))

(defn- printed
  "mode=exec captures stdout, so this is how a value comes back out of GIMP.
   Returns the trimmed text of the last statement, or nil."
  [transport code]
  (let [outcome (gimp/exec transport [code])]
    (when (gimp/ok? outcome)
      (some-> (last (gimp/value outcome)) str/trim not-empty))))

(defn- image-count
  [transport]
  (some-> (printed transport "print(len(Gimp.get_images()))") parse-long))

(defn- purge-images!
  "Drop every open image, by deleting its DISPLAY. Called at both ends: once so
   a previous run's leftovers cannot be mistaken for this run's work, and once
   after, so a long-lived GIMP does not accumulate canvases across runs.

   Neither obvious route works, and both failures are informative:

   * The catalogued `close_image` is dead code against GIMP 3.2.4. It calls
     `Gimp.get_displays()`, which the PyGObject API does not have, so EVERY
     call fails and no image is ever closed.

   * `Gimp.Image.delete()` is valid only for an image with no display attached.
     Calling it on a displayed one kills the plug-in PROCESS and takes the
     socket server with it:

       LibGimpBase: ERROR: gimp_wire_read_msg: could not find handler for
                    message: 1734962544
       LibGimpBase-CRITICAL: gimp_value_array_index: assertion
                    'index < value_array->n_values' failed

     which reaches this side as :gimp/timeout on the NEXT command, one step
     away from the call that caused it.

   What does work is reaching each display by id and deleting it; the image
   goes with it. Ids are small and monotonic, so a bounded sweep finds them
   all, and get_by_id answers None for a gap. Measured 2026-09-10 against
   headed GIMP 3.2.4."
  [transport]
  (gimp/exec transport
             [(str "for _d in range(1, 512):\n"
                   "    _x = Gimp.Display.get_by_id(_d)\n"
                   "    if _x: Gimp.Display.delete(_x)")])
  (image-count transport))

;; ---------------------------------------------------------------------------

(println)
(println "hive-gimp live verification")
(println "endpoint:  " (pr-str (config/endpoint)))
(println "transport: " (name (config/transport-kind)))
(println)

(def transport (gimp/connect))

(println "CONTRACT")

;; The doctor first, because it is the only check that is SUPPOSED to survive a
;; dead GIMP. If it cannot reach the plugin, every check below fails for one
;; reason and the report should say so once.
(let [report (gimp/doctor transport)]
  (record! "doctor reaches the plugin"
           (if (:ok? report) :pass :fail)
           (:summary report))
  (doseq [{:keys [stage ok? message optional?]} (:stages report)]
    (println (format "    %-10s %-5s %s"
                     (name stage)
                     (cond ok? "ok" optional? "skip" :else "FAIL")
                     message))))

;; check_server is one of the two commands the reference plugin dispatches but
;; whose Python server never exposed. Answering here confirms that finding
;; against a LIVE plugin rather than against its source.
(check! "check_server (never before reachable)"
        gimp/ok?
        #(gimp/invoke transport "check_server" {}))

(check! "get_gimp_info"
        #(and (gimp/ok? %) (value-contains? % "version"))
        #(gimp/invoke transport "get_gimp_info" {}))

(check! "get_context_state"
        gimp/ok?
        #(gimp/invoke transport "get_context_state" {}))

(check! "exec reaches the interpreter"
        #(and (gimp/ok? %) (= ["42\n"] (gimp/value %)))
        #(gimp/exec transport ["print(6*7)"]))

;; An unknown command must come back as a clean Outcome: not an exception, not
;; a hang, and carrying the QUALIFIED reason keyword an agent can dispatch on.
;; This is the failure path the whole Outcome type exists for.
(check! "unknown command fails cleanly"
        #(and (not (gimp/ok? %)) (= :gimp/unknown-command (:reason %)))
        #(gimp/invoke transport "no_such_command_xyz" {}))

;; A parameter the catalogue rejects must never reach the wire. Both required
;; params are supplied, so the refusal can only be about the VALUE: a missing
;; one would answer :gimp/missing-parameter and prove nothing about coercion.
(check! "uncoercible parameter refused locally"
        #(and (not (gimp/ok? %))
              (= :gimp/invalid-parameter (:reason %)))
        #(gimp/invoke transport "new_canvas" {:width "not-a-number" :height 600}))

;; ---------------------------------------------------------------------------
;; Is there a display? Decided by MEASUREMENT, not by reading DISPLAY out of an
;; environment, because what matters is whether GIMP can open one, and this
;; process is not GIMP. new_canvas is the probe: it is the cheapest command
;; that must open a display. The probe cleans up after itself either way.
;;
;; A SUCCESSFUL new_canvas no longer implies a display. hive-gimp adopts the
;; image the plug-in builds and then disowns headless, so the command now
;; succeeds either way and the two cases are told apart by `display_opened`,
;; which the plug-in hardcodes true and the recovery sets false. Reading
;; success as "there is a display" would run the whole DISPLAY suite against a
;; headless GIMP and report its failures as defects of this library.

(println)
(purge-images! transport)

(def display-probe
  (let [before (image-count transport)
        outcome (gimp/invoke transport "new_canvas" {:width 320 :height 240})
        after (image-count transport)
        value (when (gimp/ok? outcome) (gimp/value outcome))]
    {:outcome outcome
     :display? (true? (get value "display_opened"))
     :recovered? (true? (get value "recovered"))
     :leaked? (and (not (gimp/ok? outcome)) before after (> after before))
     :before before
     :after after}))


(purge-images! transport)

(println (format "DISPLAY  (%s)"
                 (if (:display? display-probe)
                   "GIMP has a display; the display suite runs and gates"
                   "GIMP is headless; the display suite is SKIPPED, not passed")))

(if-not (:display? display-probe)
  (doseq [label ["new_canvas opens an image"
                 "export_image writes a real file"
                 "open_image reads it back"
                 "get_image_bitmap returns base64"
                 "scale_image changes the dimensions"
                 "save_xcf writes a project file"]]
    (skip! label "no display"))

  (let [png (io/file work-dir "roundtrip.png")
        xcf (io/file work-dir "roundtrip.xcf")]
    (.mkdirs work-dir)
    (doseq [f [png xcf]] (.delete f))

    ;; Counts are asserted as DELTAS, never as absolutes. close_image is broken
    ;; (see purge-images!), so a GIMP that has been up a while carries images
    ;; this run did not make, and an absolute assertion would fail for a reason
    ;; that has nothing to do with the command under test.
    (let [before (image-count transport)]
      (check! "new_canvas opens an image"
              #(and (gimp/ok? %) (= (inc before) (image-count transport)))
              #(gimp/invoke transport "new_canvas"
                            {:width 320 :height 240 :fill "white"})))

    ;; The round trip leaves the process: GIMP writes the file, and the JVM
    ;; stats it. A command that reports success while writing nothing would
    ;; pass a response-shape assertion and fail this one.
    (check! "export_image writes a real file"
            #(and (gimp/ok? %) (.exists png) (pos? (.length png)))
            #(gimp/invoke transport "export_image"
                          {:file-path (.getAbsolutePath png) :format "png"}))

    (let [before (image-count transport)]
      (check! "open_image reads it back"
              #(and (gimp/ok? %) (= (inc before) (image-count transport)))
              #(gimp/invoke transport "open_image"
                            {:file-path (.getAbsolutePath png)})))

    ;; base64 image data is what tools/render promotes to MCP image content, so
    ;; this is the one command whose VALUE the tool layer reshapes.
    ;;
    ;; :region is declared OPTIONAL by the contract and is required in fact:
    ;; _get_current_image_bitmap reads region.get("origin_x") unguarded, so
    ;; omitting it raises AttributeError on None inside GIMP. Same shape as the
    ;; call_api KeyError. Passing one is the workaround, and the FINDINGS
    ;; section records the defect.
    (check! "get_image_bitmap returns base64"
            #(and (gimp/ok? %)
                  (let [v (gimp/value %)]
                    (and (map? v)
                         (string? (get v "image_data"))
                         (> (count (get v "image_data")) 100))))
            #(gimp/invoke transport "get_image_bitmap"
                          {:max-width 64
                           :region {:origin_x 0 :origin_y 0
                                    :width 320 :height 240}}))

    (check! "scale_image changes the dimensions"
            #(and (gimp/ok? %)
                  (= "(160, 120)"
                     (printed transport
                              "print((Gimp.get_images()[0].get_width(), Gimp.get_images()[0].get_height()))")))
            #(gimp/invoke transport "scale_image" {:width 160 :height 120}))

    (check! "save_xcf writes a project file"
            #(and (gimp/ok? %) (.exists xcf) (pos? (.length xcf)))
            #(gimp/invoke transport "save_xcf"
                          {:file-path (.getAbsolutePath xcf)}))))

;; ---------------------------------------------------------------------------

(println)
(println "FINDINGS (reference plugin behaviour, never gates)")

;; The two modes are genuinely different, and the DEFAULT is exec, so a bare
;; expression sent without :mode answers "" with no error. Worth printing every
;; run: that is indistinguishable from a command that legitimately printed
;; nothing, and it is the shape an agent will hit first.
(doseq [mode [:exec :eval]]
  (let [expr (gimp/exec transport ["1 + 1"] {:mode mode})
        prn* (gimp/exec transport ["print(6*7)"] {:mode mode})]
    (finding! (str "exec semantics, mode=" (name mode))
              (format "marker %s: `1 + 1` -> %-8s `print(6*7)` -> %s"
                      (pr-str (if (= :eval mode) exec/eval-marker exec/exec-marker))
                      (pr-str (gimp/value expr))
                      (pr-str (gimp/value prn*))))))

;; _new_canvas builds the image and its layer, fills it, and only THEN calls
;; Gimp.Display.new(image), which returns NULL when GIMP runs headless. The
;; blanket `except` turns that into {"status": "error"}, so the plug-in reports
;; a failure for a fully formed image whose id it never returns: a false
;; failure report AND an orphan, on every headless call.
;;
;; hive-gimp compensates, so what this prints is now the plug-in's behaviour
;; SEEN THROUGH the recovery: `recovered` means the defect is still there and
;; was caught. The day the plug-in opens no display and says so honestly, this
;; line will say `succeeded` with display_opened false and no recovery.
(finding! "new_canvas"
          (cond
            (:display? display-probe)
            (format "succeeded (a display exists); images %s -> %s"
                    (:before display-probe) (:after display-probe))

            (:recovered? display-probe)
            (format "reported failure headless; images %s -> %s, image adopted by hive-gimp"
                    (:before display-probe) (:after display-probe))

            :else
            (format "reported %s, images %s -> %s%s"
                    (:reason (:outcome display-probe))
                    (:before display-probe) (:after display-probe)
                    (if (:leaked? display-probe)
                      "  <- ORPHANED IMAGE LEAKED"
                      ""))))


;; close_image calls Gimp.get_displays(), which the GIMP 3.2 PyGObject API does
;; not have. Every call fails, so nothing can close an image through this
;; plugin. Probed rather than asserted from the traceback, so the day it is
;; fixed the run says so.
(finding! "close_image"
          (let [o (gimp/invoke transport "close_image" {:image-index 0 :save-first false})]
            (if (gimp/ok? o)
              "works (upstream fixed it)"
              (format "%s: %s" (:reason o) (:message o)))))

;; :region is contracted OPTIONAL and is required in fact.
(finding! "get_image_bitmap without :region"
          (let [o (gimp/invoke transport "get_image_bitmap" {:max-width 64})]
            (if (gimp/ok? o)
              "works (upstream fixed it)"
              (format "%s: %s" (:reason o) (:message o)))))

;; ---------------------------------------------------------------------------
;; Cleanup, and the asymmetry is not an oversight.
;;
;; HEADLESS: new_canvas left orphans with no display attached, so Gimp.Image
;; delete() is both safe and the only thing that clears them. purge-images!
;; would sweep displays that are not there.
;;
;; HEADED: do NOT sweep. Deleting every display closes the last window and GIMP
;; EXITS, so the run kills the GIMP it was testing and the next invocation finds
;; nothing listening. Measured the hard way: a 13/13 run was followed by a
;; 2 passed / 5 failed / 6 skipped one, for no reason other than this.
;; A headed GIMP is also somebody's session, and closing their windows to tidy
;; up after a test is presumptuous even when it works.

(if (:display? display-probe)
  (println (format "left %s image(s) open; a headed GIMP is a live session and closing"
                   (image-count transport))
           "\n         its last display would quit it")
  (gimp/exec transport ["for _i in list(Gimp.get_images()): _i.delete()"]))

(println)
(let [rs @results
      by (group-by :status rs)
      passed (count (:pass by))
      failed (count (:fail by))
      skipped (count (:skip by))]
  (println (format "%d passed, %d failed, %d skipped, %d findings"
                   passed failed skipped (count @findings)))
  (when (pos? failed)
    (println)
    (println "failed:")
    (doseq [r (:fail by)]
      (println "  " (:label r) "->" (pr-str (:detail r)))))
  (flush)
  (System/exit (if (zero? failed) 0 1)))
