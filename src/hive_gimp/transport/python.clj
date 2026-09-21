(ns hive-gimp.transport.python
  "BOUNDARY. libpython-clj. TWO things live here, and confusing them is the
   whole difficulty of this integration.

   ## What libpython-clj CANNOT do

   It cannot import GIMP. GIMP 3 plugins run in GIMP's own embedded CPython,
   linked against that process's GObject introspection typelibs and attached to
   its main loop and PDB. Pointing libpython-clj at that same interpreter and
   running `gi.require_version('Gimp', '3.0'); from gi.repository import Gimp`
   SUCCEEDS, which is the trap: the module imports and is inert.
   `Gimp.get_images()` answers for a GIMP that this process is not part of.
   There is no in-process route to a running GIMP, from a JVM or from anything
   else that GIMP did not itself launch.

   ## What libpython-clj CAN do, and does here

   Two useful things, and both are real.

   `PythonTransport` drives the GIMP plugin socket FROM the embedded
   interpreter. Python opens the socket, frames the request and reads the
   answer; the JVM never touches a file descriptor. This is precisely the path
   the reference project's own bg_remove.py takes, executed in-process instead
   of as a subprocess. It satisfies `IGimpTransport`, so it is a drop-in
   alternative to `hive-gimp.transport.socket` and every command works through
   it unchanged.

   Why have it when the native socket exists: it puts GIMP work and host-side
   Python work in ONE interpreter, sharing state. A workflow that exports a
   layer, runs rembg over it, and re-imports the result is then one Python
   context rather than two runtimes passing files, and the reference project's
   Python helpers can be imported and driven directly.

   `HostPython` is the other half: the Python image ecosystem GIMP does not
   ship (rembg, PIL, numpy, scikit-image). See `hive-gimp.pixel`.

   Every libpython-clj var is resolved SOFTLY, at call time. The dependency
   lives behind the `:python` alias, so a deployment without it loads this
   namespace, reports `:python/no-libpython`, and keeps the native socket
   transport and every GIMP command working."
  (:require [clojure.java.io :as io]
            [hive-gimp.ports :as ports]
            [taoensso.timbre :as log]
            [hive-gimp.codec :as codec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defonce ^:private initialized? (atom false))
(defonce ^:private cached-status (atom nil))

(defn- py-var
  "Resolve a libpython-clj var, or nil when the library is absent.

   `requiring-resolve` rather than a `:require`: a load-time require would make
   this namespace, and therefore the whole addon, unloadable without the
   optional dependency."
  [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn- libpython-present? []
  (some? (py-var 'libpython-clj2.python/initialize!)))

;; =============================================================================
;; Status
;; =============================================================================

(def status-hints
  {:python/available    nil

   :python/no-libpython
   "libpython-clj is not on the classpath. It is optional and lives behind the :python alias:
  clojure -M:python ...
or add clj-python/libpython-clj {:mvn/version \"2.026\"} to your deps."

   :python/no-interpreter
   "No Python interpreter was found. Point HIVE_GIMP_PYTHON at one, for example:
  export HIVE_GIMP_PYTHON=$HOME/miniconda3/envs/hive/bin/python"

   :python/init-failed
   "libpython-clj could not initialize Python. Usual causes:
  1. PYTHONHOME or PYTHONPATH pointing at a different interpreter
  2. the interpreter has no shared libpython (needs a --enable-shared build)
  3. a version mismatch between the interpreter and its site-packages"})

(defn- patch-io-encoding!
  "Give the JVM stdout/stderr bridge an `encoding` attribute.

   Several Python packages read `sys.stdout.encoding` at import time and raise
   AttributeError against libpython-clj's bridge, which does not define it. Set
   through `set-attr!` rather than by running a Python string, because the
   string path recurses through the same IO bridge and overflows the stack."
  []
  (try
    (let [import-module (py-var 'libpython-clj2.python/import-module)
          get-attr      (py-var 'libpython-clj2.python/get-attr)
          set-attr!     (py-var 'libpython-clj2.python/set-attr!)
          sys           (import-module "sys")]
      (doseq [stream ["stdout" "stderr"]]
        (let [s (get-attr sys stream)]
          (when-not (try (get-attr s "encoding") (catch Throwable _ nil))
            (set-attr! s "encoding" "utf-8")))))
    (catch Throwable t
      (log/debug "[hive-gimp] could not patch python io encoding" {:cause (ex-message t)}))))

(defn- ensure-site-packages!
  "Put the interpreter's own package directories on sys.path.

   libpython-clj initializes an interpreter without running site.py in some
   configurations, so a conda env's packages are installed and invisible. Adds
   only directories that exist and are not already present, so it is idempotent
   and cannot shadow anything."
  []
  (try
    (let [import-module (py-var 'libpython-clj2.python/import-module)
          get-attr      (py-var 'libpython-clj2.python/get-attr)
          call-attr     (py-var 'libpython-clj2.python/call-attr)
          ->jvm         (py-var 'libpython-clj2.python/->jvm)
          sys           (import-module "sys")
          sysconfig     (import-module "sysconfig")
          path          (get-attr sys "path")
          present       (set (->jvm path))]
      (doseq [kind ["purelib" "platlib"]]
        (let [dir (->jvm (call-attr sysconfig "get_path" kind))]
          (when (and (string? dir) (.isDirectory (io/file dir)) (not (present dir)))
            (call-attr path "append" dir)))))
    (catch Throwable t
      (log/debug "[hive-gimp] could not extend python sys.path" {:cause (ex-message t)}))))

(defn- initialize-python!
  "Initialize Python once. Returns a status keyword."
  [python-executable]
  (cond
    (not (libpython-present?)) :python/no-libpython
    @initialized?              :python/available
    :else
    (let [init! (py-var 'libpython-clj2.python/initialize!)]
      (try
        (if python-executable
          (init! :python-executable python-executable)
          (init!))
        (ensure-site-packages!)
        (patch-io-encoding!)
        (reset! initialized? true)
        :python/available
        (catch Throwable t
          (log/warn "[hive-gimp] python initialization failed" {:cause (ex-message t)})
          :python/init-failed)))))

(defn- module-available?
  [module]
  (boolean
   (try
     (when-let [import-module (py-var 'libpython-clj2.python/import-module)]
       (import-module module)
       true)
     (catch Throwable _ false))))

(defn status
  "Why host-side Python is or is not usable. Never throws."
  [python-executable]
  (or @cached-status
      (let [s      (initialize-python! python-executable)
            answer {:status s
                    :hint   (get status-hints s)
                    :python-executable python-executable
                    :modules (when (= :python/available s)
                               (into {} (map (juxt identity module-available?))
                                     ["PIL" "numpy" "rembg" "skimage"]))}]
        ;; Only a success is cached. A failure is usually something the user is
        ;; in the middle of fixing, and caching it would make the fix invisible
        ;; until the JVM restarts.
        (when (= :python/available s) (reset! cached-status answer))
        answer)))

;; =============================================================================
;; The embedded Python client
;; =============================================================================

(defn- errno-reason
  "Python's own errno, lifted to this library's vocabulary.

   `connect_ex` is why this is a table of INTEGERS rather than a table of
   exception-message substrings. It answers with the errno instead of raising,
   so the classification never leaves Python's own vocabulary and never depends
   on the wording, or the locale, of a message: a `Connection refused`
   translated into another language still arrives here as 111."
  [errno-mod rc]
  (let [get-attr (py-var 'libpython-clj2.python/get-attr)
        ->jvm    (py-var 'libpython-clj2.python/->jvm)
        e        #(->jvm (get-attr errno-mod %))]
    (condp = rc
      (e "ECONNREFUSED") :gimp/not-listening
      (e "ETIMEDOUT")    :gimp/connect-timeout
      (e "EAGAIN")       :gimp/connect-timeout
      (e "EINPROGRESS")  :gimp/connect-timeout
      (e "EWOULDBLOCK")  :gimp/connect-timeout
      :gimp/transport-error)))

(defn- round-trip*
  "One request and its response against the GIMP plugin socket, driven from the
   embedded interpreter through libpython-clj INTEROP.

   Python is used as a LANGUAGE here, not as a string: modules are imported,
   attributes are read, and objects are called. There is no Python source text
   in this namespace to keep in sync with the Clojure around it, nothing is
   `exec`d, and nothing is compiled at runtime.

   Three deliberate choices, each removing a failure mode the source-string
   version had:

   * `connect_ex` instead of `connect`. It answers with an errno rather than
     raising, so `errno-reason` classifies against Python's own integers and
     never parses an exception message. See `errno-reason`.

   * `select` instead of a bare blocking `recv`. Readiness is reported as an
     empty list rather than by raising `socket.timeout`, so the timeout path is
     an ordinary value.

   * The accumulating buffer stays a PYTHON bytes object, concatenated with
     `operator.add`. `->jvm` on Python bytes yields a vector of boxed integers,
     so marshalling a partial buffer on every chunk would turn a base64 image
     into millions of Longs. The bytes cross the bridge exactly once, decoded,
     when the frame is complete.

   Completeness is decided by `hive-gimp.codec/complete-frame?`, the same
   predicate the JVM socket transport uses, so the framing rule has ONE
   definition and both transports cannot drift. A decode that fails mid
   multibyte character is a partial read, not an error, and simply continues.

   Returns a map: `{:ok true :frame s}` or
   `{:ok false :reason <qualified-keyword> :message s :partial n}`. It does not
   throw; the record decides what is worth raising."
  [host port frame timeout-ms connect-timeout-ms]
  (let [import-module (py-var 'libpython-clj2.python/import-module)
        call-attr     (py-var 'libpython-clj2.python/call-attr)
        get-attr      (py-var 'libpython-clj2.python/get-attr)
        ->jvm         (py-var 'libpython-clj2.python/->jvm)
        ->python      (py-var 'libpython-clj2.python/->python)
        socket-mod (import-module "socket")
        select-mod (import-module "select")
        errno-mod  (import-module "errno")
        os-mod     (import-module "os")
        operator   (import-module "operator")
        builtins   (import-module "builtins")
        py-len     #(->jvm (call-attr builtins "len" %))
        empty-bytes #(call-attr (->python "") "encode" "utf-8")
        sock (call-attr socket-mod "socket"
                        (get-attr socket-mod "AF_INET")
                        (get-attr socket-mod "SOCK_STREAM"))]
    (try
      (call-attr sock "settimeout" (double (/ connect-timeout-ms 1000)))
      (let [addr (call-attr builtins "tuple" [host (int port)])
            ;; The one place an exception is unavoidable: resolution failure
            ;; raises socket.gaierror before connect_ex can return anything.
            ;; Catching broadly is exact here, because every way this call can
            ;; fail IS a failure to resolve the host.
            rc (try (->jvm (call-attr sock "connect_ex" addr))
                    (catch Exception _ ::unresolvable))]
        (cond
          (= ::unresolvable rc)
          {:ok false :reason :gimp/unknown-host
           :message (str "Cannot resolve host " host ".")}

          (not (zero? rc))
          {:ok false :reason (errno-reason errno-mod rc) :errno rc
           :message (->jvm (call-attr os-mod "strerror" rc))}

          :else
          (do
            (call-attr sock "settimeout" (double (/ timeout-ms 1000)))
            (call-attr sock "sendall" (call-attr (->python frame) "encode" "utf-8"))
            (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
              (loop [buf (empty-bytes)]
                (let [remaining (/ (- deadline (System/currentTimeMillis)) 1000.0)
                      seen (py-len buf)]
                  (if (neg? remaining)
                    {:ok false :reason :gimp/timeout :partial seen
                     :message "GIMP did not answer before the timeout."}
                    (let [ready (->jvm (call-attr select-mod "select"
                                                  [sock] [] [] (double remaining)))]
                      (if (empty? (first ready))
                        {:ok false :reason :gimp/timeout :partial seen
                         :message "GIMP did not answer before the timeout."}
                        (let [chunk (call-attr sock "recv" 8192)]
                          (if (zero? (py-len chunk))
                            {:ok false :reason :gimp/connection-lost :partial seen
                             :message "GIMP closed the connection while answering."}
                            (let [buf' (call-attr operator "add" buf chunk)
                                  text (try (->jvm (call-attr buf' "decode" "utf-8"))
                                            (catch Exception _ nil))]
                              (if (and text (codec/complete-frame? text))
                                {:ok true :frame text}
                                (recur buf'))))))))))))))
      (finally
        (try (call-attr sock "close") (catch Exception _ nil))))))

;; =============================================================================
;; Host-side pixel operations, through interop
;; =============================================================================

(defn- image-info
  "PIL's reading of the image file at `path`:
   {\"width\" int \"height\" int \"mode\" str \"format\" str-or-nil}."
  [path]
  (let [import-module (py-var 'libpython-clj2.python/import-module)
        get-attr      (py-var 'libpython-clj2.python/get-attr)
        call-attr     (py-var 'libpython-clj2.python/call-attr)
        ->jvm         (py-var 'libpython-clj2.python/->jvm)
        im            (call-attr (import-module "PIL.Image") "open" path)
        attr          #(->jvm (get-attr im %))]
    (try
      {"width"  (attr "width")
       "height" (attr "height")
       "mode"   (attr "mode")
       "format" (attr "format")}
      (finally (call-attr im "close")))))

(defn- remove-background
  "rembg over the file at `in-path`, written to `out-path`:
   {\"input\" in-path \"output\" out-path \"bytes\" written}.

   The image bytes stay Python objects from `read` to `write`; only the
   written length crosses into the JVM."
  [in-path out-path]
  (let [import-module (py-var 'libpython-clj2.python/import-module)
        call-attr     (py-var 'libpython-clj2.python/call-attr)
        ->jvm         (py-var 'libpython-clj2.python/->jvm)
        builtins      (import-module "builtins")
        rembg         (import-module "rembg")
        with-file     (fn [path mode f]
                        (let [handle (call-attr builtins "open" path mode)]
                          (try (f handle) (finally (call-attr handle "close")))))
        data          (with-file in-path "rb" #(call-attr % "read"))
        out           (call-attr rembg "remove" data)]
    (with-file out-path "wb" #(call-attr % "write" out))
    {"input"  in-path
     "output" out-path
     "bytes"  (->jvm (call-attr builtins "len" out))}))

(def embedded-modules
  "This library's own host-side Python operations, by module and attribute
   name, as Clojure fns that drive Python through interop. `call-python`
   answers a [module attr] found here without importing anything named
   `module`, so `hive-gimp.pixel` reaches them through the port like any other
   Python call. Args are positional; kwargs are refused."
  {"hive_gimp_pixel" {"image_info"        #'image-info
                      "remove_background" #'remove-background}})

;; =============================================================================
;; PythonTransport
;; =============================================================================

(defn- fail!
  [reason message data]
  (throw (ex-info message (merge {:hive-gimp/reason reason} data))))

(defrecord PythonTransport [endpoint connect-timeout-ms python-executable]
  ports/IGimpTransport
  (transport-id [_] :python)

  (round-trip! [_ frame]
    (let [{:keys [status hint]} (status python-executable)]
      (when-not (= :python/available status)
        (fail! status
               (str "The Python transport is unavailable: " (name status)
                    ". The native socket transport needs no Python.")
               {:hint hint})))
    (let [{:keys [host port timeout-ms]} endpoint
          result (round-trip* host port frame timeout-ms connect-timeout-ms)]
      (if (:ok result)
        (:frame result)
        (let [reason (:reason result)]
          (fail! reason
                 (case reason
                   :gimp/not-listening
                   (str "Nothing is listening on " host ":" port
                        ". Open GIMP and run Tools > MCP > Start MCP Server.")
                   :gimp/unknown-host
                   (str "Cannot resolve host " host ".")
                   :gimp/timeout
                   "GIMP did not answer before the timeout. A long operation may still be running."
                   :gimp/connection-lost
                   "GIMP closed the connection while answering."
                   (str "The Python transport failed: " (:message result)))
                 {:host host :port port
                  :partial (:partial result)
                  :errno (:errno result)
                  :python-message (:message result)}))))))

(defn transport
  "An `IGimpTransport` that reaches GIMP THROUGH the embedded Python
   interpreter rather than through a JVM socket.

   Interchangeable with `hive-gimp.transport.socket/transport`: same port, same
   commands, same outcomes. Choose it when you also want host-side Python in
   the same interpreter, so a GIMP export can be handed straight to rembg
   without leaving the process."
  ([endpoint] (transport endpoint 5000 nil))
  ([endpoint connect-timeout-ms] (transport endpoint connect-timeout-ms nil))
  ([endpoint connect-timeout-ms python-executable]
   (->PythonTransport endpoint connect-timeout-ms python-executable)))

;; =============================================================================
;; HostPython
;; =============================================================================

(defrecord HostPython [python-executable]
  ports/IHostPython

  (python-available? [_] (= :python/available (:status (status python-executable))))

  (python-status [_] (status python-executable))

  (call-python [this module attr args kwargs]
    (let [{:keys [status hint]} (ports/python-status this)]
      (when-not (= :python/available status)
        (throw (ex-info (str "Host-side Python is unavailable: " (name status))
                        {:hive-gimp/reason status :hint hint})))
      (if-let [op (get-in embedded-modules [module attr])]
        (if (seq kwargs)
          (throw (ex-info (str module "." attr " takes positional arguments only")
                          {:hive-gimp/reason :python/bad-arguments :kwargs kwargs}))
          (apply op args))
        (let [import-module (py-var 'libpython-clj2.python/import-module)
              call-attr-kw  (py-var 'libpython-clj2.python/call-attr-kw)
              ->jvm         (py-var 'libpython-clj2.python/->jvm)
              m             (import-module module)]
          (->jvm (call-attr-kw m attr (vec args) (or kwargs {}))))))))

(defn host-python
  "An `IHostPython` bound to `python-executable` (nil to autodetect)."
  ([] (host-python nil))
  ([python-executable] (->HostPython python-executable)))

(defn reset-cache!
  "Forget the cached status. For tests, and for a user who just installed the
   interpreter this process failed to find.

   There is no compiled client to forget any more: `round-trip*` resolves its
   modules through interop on each call, so nothing is compiled and held."
  []
  (reset! cached-status nil))
