(ns hive-gimp.exec
  "The Python-Fu escape hatch: run arbitrary code inside GIMP's interpreter.

   Eighty typed commands cover the common ground and will never cover GIMP.
   This is the way out, and it is also the sharpest thing in the library, so
   the shape it sends is documented rather than copied.

   HOW THE PLUGIN ACTUALLY DISPATCHES

   `execute_command` is one long chain of `elif j[\"type\"] == ...`. Anything
   that matches no branch falls through to:

       elif \"cmds\" in j:  a = ['python-fu-exec', j[\"cmds\"]]
       else:               a = j[\"params\"]['args']

   and then the only test that matters:

       if a[0] == 'python-fu-eval':  return [str(eval(e)) for e in a[1]]
       else:                         exec each string in a[1]

   Two consequences a caller has to know:

   1. The marker `python-fu-eval` is the ONLY thing that produces a VALUE.
      Every other marker execs. The reference project's own protocol document
      advertises `pyGObject-eval` for this, which matches no branch, so a
      caller following that document gets exec semantics and a list of
      \"None\" back. `:eval` here sends the marker that works.

   2. The `else` branch reads `j[\"params\"]` unguarded, so a request with no
      params raises KeyError inside the plugin. Both shapes below always send
      params.

   State persists between calls: the plugin execs into one long-lived context
   dict, so imports and variables set by an earlier call are still there. That
   is useful and it is also why an `exec` that leaves a half-built image around
   is visible to the next caller."
  (:require [hive-gimp.client :as client]
            [hive-gimp.schema :as schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def eval-marker
  "The literal the plugin compares against. Not a name we chose, and not one we
   may spell differently."
  "python-fu-eval")

(def exec-marker
  "Any marker that is not `eval-marker` execs. This one is the spelling the
   reference documentation uses, so a plugin log line stays recognisable to
   someone reading that project's docs."
  "pyGObject-console")

(def preamble
  "The bindings nearly every GIMP snippet opens by establishing.

   Offered rather than imposed: `with-preamble` is opt-in, because a snippet
   that creates its own image must not be handed one, and a snippet run against
   an empty GIMP would fail on the preamble rather than on its own first line."
  ["images = Gimp.get_images()"
   "image = images[0] if images else None"
   "layers = image.get_layers() if image else []"
   "layer = layers[0] if layers else None"
   "drawable = layer"])

(defn with-preamble
  "`code` prefixed with `preamble`."
  [code]
  (into (vec preamble) code))

(defn ->command
  "An `ExecRequest` as a `GimpCommand`.

   Pure, and separated from `run` so the exact bytes sent to a very sharp
   interface can be asserted without a GIMP anywhere."
  [{:keys [mode code]}]
  {:type   "exec"
   :params {"args"   [(if (= :eval mode) eval-marker exec-marker) (vec code)]
            "kwargs" {}}})

(defn run
  "Execute `code` inside GIMP. Returns an `Outcome`.

   `:exec` returns the captured stdout of each statement; `:eval` returns
   `str()` of each expression's value. Note that `:eval` stringifies on the
   GIMP side, so a Python object comes back as its repr, not as data."
  ([transport code] (run transport code {}))
  ([transport code {:keys [mode preamble?] :or {mode :exec preamble? false}}]
   (let [code    (if (string? code) [code] (vec code))
         code    (if preamble? (with-preamble code) code)
         request {:mode mode :code code}]
     (if-not (schema/exec-request? request)
       {:outcome :error
        :command "exec"
        :reason  :gimp/invalid-parameter
        :message "exec expects a mode of :exec or :eval and a sequence of code strings."
        :detail  (schema/explain schema/ExecRequest request)}
       (client/send-command transport (->command request))))))

(defn flush!
  "Push pending drawing operations to the display.

   GIMP does not repaint until `displays_flush()` runs, so a drawing snippet
   that omits it appears to have done nothing. Named as its own function
   because the omission is the single most common way a correct GIMP script
   looks broken."
  [transport]
  (run transport ["Gimp.displays_flush()"]))
