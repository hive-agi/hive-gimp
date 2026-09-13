(ns verify-python-transport
  "Live proof that the libpython-clj transport drives the GIMP protocol.

   Run it:  clojure -M:python -i dev/verify_python_transport.clj

   Not a unit test, and it deliberately does not live in `test/`. The suite runs
   without libpython-clj and must keep doing so; this needs the real thing. What
   it proves is the part a double cannot:

     1. libpython-clj initializes a real interpreter
     2. the embedded client source compiles inside it
     3. Python opens a real socket and speaks the plugin's framing
     4. a tagged failure crosses back and is classified correctly
     5. a full command round trip works end to end

   GIMP is not required. Step 5 runs against a FAKE plugin: a JVM ServerSocket
   that implements the same wire protocol the real plugin implements, including
   the detail that makes it interesting, which is that responses carry no
   terminator. If the transport can talk to that, it can talk to GIMP, because
   the wire is the entire contract between them."
  (:require [clojure.data.json :as json]
            [hive-gimp.client :as client]
            [hive-gimp.ports :as ports]
            [hive-gimp.response :as response]
            [hive-gimp.transport.python :as python])
  (:import (java.net InetSocketAddress ServerSocket Socket)
           (java.nio.charset StandardCharsets)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; A fake GIMP plugin
;; =============================================================================

(defn- handle-one!
  "Serve one request the way the real plugin does.

   Two fidelities matter. The response carries NO terminator, so the client must
   stop on `parses as a JSON object` or hang. And the connection is closed after
   answering, because `auto_disconnect_client` starts true in the real plugin."
  [^Socket client responses]
  (with-open [client client]
    (let [in  (.getInputStream client)
          buf (byte-array 8192)
          n   (.read in buf)
          req (json/read-str (String. buf 0 (max n 0) StandardCharsets/UTF_8))
          out (.getOutputStream client)
          rsp (responses (get req "type")
                         {"status" "error" "error" (str "fake plugin has no " (get req "type"))})]
      (.write out (.getBytes (json/write-str rsp) StandardCharsets/UTF_8))
      (.flush out))))

(defn start-fake-plugin!
  "A ServerSocket speaking the GIMP plugin protocol. Returns {:port :stop!}."
  [responses]
  (let [server  (ServerSocket.)
        _       (.bind server (InetSocketAddress. "127.0.0.1" 0))
        running (atom true)
        thread  (doto (Thread.
                       #(while @running
                          (try (handle-one! (.accept server) responses)
                               (catch Throwable _ nil)))
                       "fake-gimp-plugin")
                  (.setDaemon true)
                  (.start))]
    {:port  (.getLocalPort server)
     :stop! (fn [] (reset! running false) (.close server) (.interrupt thread))}))

;; =============================================================================
;; Checks
;; =============================================================================

(defn- check [label ok? detail]
  (println (format "  %-4s %s" (if ok? "PASS" "FAIL") label))
  (when detail (println (str "       " detail)))
  ok?)

(defn run []
  (println "\nlibpython-clj GIMP transport, live verification\n")

  (let [{:keys [status hint modules]} (python/status nil)]
    (if-not (= :python/available status)
      (do (check "libpython-clj initializes a real interpreter" false (str status ": " hint))
          (println "\nCannot continue without a working interpreter.\n")
          false)

      (let [results
            (atom [(check "libpython-clj initializes a real interpreter" true
                          (str "modules seen: " (pr-str modules)))])
            add! #(swap! results conj %)]

        ;; 2 + 3 + 4: the embedded client compiles, opens a real socket, and a
        ;; tagged failure crosses back classified. A closed port is the cheapest
        ;; way to exercise all three at once.
        (let [dead (python/transport {:host "127.0.0.1" :port 1 :timeout-ms 2000} 1000 nil)
              out  (try (ports/round-trip! dead "{\"type\":\"check_server\",\"params\":{}}\n")
                        (catch Throwable t t))]
          (add! (check "embedded Python client compiles and opens a real socket"
                       (instance? Throwable out)
                       nil))
          (add! (check "a refused connection is classified as :gimp/not-listening"
                       (= :gimp/not-listening (:hive-gimp/reason (ex-data out)))
                       (str "reason: " (:hive-gimp/reason (ex-data out))))))

        ;; 5: a full command round trip against a fake plugin.
        (let [{:keys [port stop!]}
              (start-fake-plugin!
               {"check_server"  {"status" "success" "results" {"running" true "port" 9877}}
                "get_gimp_info" {"status" "success" "results" {"version" "3.0.4"}}
                "auto_levels"   {"status" "error" "error" "No images are currently open in GIMP"}})]
          (try
            (let [transport (python/transport {:host "127.0.0.1" :port port :timeout-ms 5000} 2000 nil)]
              (add! (check "transport-id reports :python"
                           (= :python (ports/transport-id transport)) nil))

              (let [out (client/invoke transport "check_server")]
                (add! (check "a full command round trip succeeds through Python"
                             (and (response/ok? out) (true? (get (:value out) "running")))
                             (pr-str (:value out)))))

              (let [out (client/invoke transport "get_gimp_info")]
                (add! (check "a second round trip succeeds (the plugin hangs up between commands)"
                             (response/ok? out) (pr-str (:value out)))))

              (let [out (client/invoke transport "auto_levels")]
                (add! (check "a plugin error is classified, not thrown"
                             (= :gimp/no-image (:reason out))
                             (:message out)))))
            (finally (stop!))))

        (let [passed (count (filter true? @results))
              total  (count @results)]
          (println (format "\n%d/%d checks passed\n" passed total))
          (= passed total))))))

(run)
