(ns hive-gimp.transport.socket
  "BOUNDARY. The only namespace that touches a socket.

   One connection per command, deliberately. That is not a simplification, it
   is the plugin's actual behaviour: `_handle_client` ends with

       if self.auto_disconnect_client: client.close()

   and `auto_disconnect_client` starts true. The plugin hangs up after every
   command unless a client first sends the bare string `disable_auto_disconnect`,
   so a client that assumes a persistent connection is really reconnecting on
   every other call and discovering it at the least convenient moment. Opening
   per command makes that explicit and costs a loopback TCP handshake.

   Everything above this namespace speaks `IGimpTransport` and therefore never
   learns that any of this happened."
  (:require [hive-gimp.codec :as codec]
            [hive-gimp.ports :as ports]
            [taoensso.timbre :as log])
  (:import (java.io InputStreamReader OutputStreamWriter)
           (java.net ConnectException InetSocketAddress Socket SocketTimeoutException)
           (java.nio.charset StandardCharsets)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private read-buffer-size 8192)

(defn- fail!
  [reason message data]
  (throw (ex-info message (merge {:hive-gimp/reason reason} data))))

(defn- read-frame
  "Read until the buffer holds a complete response object.

   The plugin sends no terminator, so the stopping condition is
   `codec/complete-frame?`. Three ways out, and each is a different diagnosis:

     complete    the normal path
     EOF         GIMP closed mid-answer, so what we hold is a truncated frame
     timeout     GIMP is still working, or died holding the lock

   A timeout that had already accumulated bytes is reported separately, because
   `GIMP sent nothing` and `GIMP sent half an answer` call for different
   responses from a human."
  [^Socket socket command]
  (let [reader (InputStreamReader. (.getInputStream socket) StandardCharsets/UTF_8)
        buf    (char-array read-buffer-size)]
    (loop [acc (StringBuilder.)]
      (if (codec/complete-frame? (.toString acc))
        (.toString acc)
        (let [n (try
                  (.read reader buf 0 read-buffer-size)
                  (catch SocketTimeoutException _
                    (fail! :gimp/timeout
                           (if (pos? (.length acc))
                             "GIMP started answering and then stopped. The command may still be running inside GIMP."
                             "GIMP did not answer before the timeout. A long operation may still be running; raise HIVE_GIMP_TIMEOUT_MS if this command is legitimately slow.")
                           {:command command :partial (.length acc)})))]
          (cond
            (neg? n)
            (fail! :gimp/connection-lost
                   (if (pos? (.length acc))
                     "GIMP closed the connection while answering, leaving a truncated response."
                     "GIMP closed the connection without answering.")
                   {:command command :partial (.length acc)})

            :else
            (recur (.append acc buf 0 n))))))))

(defrecord SocketTransport [endpoint connect-timeout-ms]
  ports/IGimpTransport
  (transport-id [_] :socket)

  (round-trip! [_ frame]
    (let [{:keys [host port timeout-ms]} endpoint]
      (with-open [socket (Socket.)]
        (try
          (.connect socket (InetSocketAddress. ^String host ^int (int port))
                    (int connect-timeout-ms))
          (catch ConnectException e
            (fail! :gimp/not-listening
                   (str "Nothing is listening on " host ":" port
                        ". Open GIMP and run Tools > MCP > Start MCP Server.")
                   {:host host :port port :cause (ex-message e)}))
          (catch SocketTimeoutException _
            (fail! :gimp/connect-timeout
                   (str "Timed out connecting to " host ":" port ".")
                   {:host host :port port}))
          (catch java.net.UnknownHostException _
            (fail! :gimp/unknown-host
                   (str "Cannot resolve host " host ".")
                   {:host host})))
        (.setSoTimeout socket (int timeout-ms))
        (let [writer (OutputStreamWriter. (.getOutputStream socket) StandardCharsets/UTF_8)]
          (.write writer ^String frame)
          (.flush writer))
        (log/debug "[hive-gimp] sent frame" {:bytes (count frame) :host host :port port})
        (read-frame socket frame)))))

(defn transport
  "A `SocketTransport` for `endpoint`."
  ([endpoint] (transport endpoint 5000))
  ([endpoint connect-timeout-ms]
   (->SocketTransport endpoint connect-timeout-ms)))
