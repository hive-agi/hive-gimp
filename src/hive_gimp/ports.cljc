(ns hive-gimp.ports
  "The seams. Every namespace above this one depends on these protocols and on
   no concretion, which is what lets the suite inject a scripted double instead
   of reaching for `with-redefs` on a socket.

   Two ports, deliberately separate, because they have different clients and
   different reasons to change (ISP):

     IGimpTransport   one framed round trip to the GIMP plugin. The ONLY thing
                      the command pipeline needs, and the only thing it may
                      touch. A socket satisfies it; so does a recorded script,
                      a Python client, or an ssh tunnel.

     IHostPython      the host JVM's Python, for pixel work GIMP has no
                      procedure for. Nothing in the command pipeline depends on
                      it, so a deployment without libpython-clj loses this port
                      and keeps every GIMP command.

   Collapsing these into one port would be the mistake: it would make the
   command pipeline unusable without a Python runtime it never needed.

   Portable (.cljc since 2026-09-20): a protocol declaration names no host.
   The ADAPTERS are the host-specific part and stay .clj, which is the whole
   reason the seam is worth having."
  (:refer-clojure :exclude [flush]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defprotocol IGimpTransport
  "One request in, one response out, both as already-framed strings.

   The port is deliberately at the STRING boundary rather than at the map
   boundary. Encoding and decoding are pure and belong in `hive-gimp.codec`
   where they can be property-tested; an adapter that also encoded would put
   that logic behind an I/O call where no generator can reach it."
  (round-trip!
    [transport frame]
    "Send `frame` (a complete request line, newline included) and return the
     raw response string. Throws `ex-info` with `:hive-gimp/reason` on any
     transport failure, never returns a sentinel: a transport that answers
     `nil` for `could not connect` is indistinguishable from one that answers
     `nil` for `GIMP said nothing`.")
  (transport-id
    [transport]
    "A keyword naming this adapter, e.g. `:socket` or `:python`. Reported by
     health and doctor so a surprising result can be attributed."))

(defprotocol IHostPython
  "Host-side Python, reached through libpython-clj. Optional by construction.

   This is NOT how hive-gimp talks to GIMP. GIMP's Python is its own embedded
   PyGObject interpreter, living in another process, and it is unreachable from
   this JVM at any price. What this port offers is the packages GIMP does not
   ship: background removal, numpy pixel maths, scikit-image, PIL."
  (python-available?
    [host]
    "True when a Python runtime is initialized and importable. Never throws.")
  (python-status
    [host]
    "A map describing why Python is or is not usable, with a `:hint` a human
     can act on. Never throws, so a health check cannot be what breaks health.")
  (call-python
    [host module attr args kwargs]
    "Call `module.attr(*args, **kwargs)` and return the result converted to
     JVM values. Throws `ex-info` when Python is unavailable rather than
     returning nil, for the reason given on `round-trip!`."))
