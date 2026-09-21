(ns hive-gimp.portability-test
  "hive-gimp grew a portable core on 2026-09-20 (`shape`, `ports`,
   `doctor.verdict`). This keeps it portable.

   Two invariants, read off the files rather than off loaded namespaces,
   because a namespace that requires malli loads perfectly well on the JVM,
   which is exactly where the breakage does not show up:

     1. no .cljc REQUIRES or IMPORTS a host-only library;
     2. no .cljc contains a reader conditional.

   A portable core in this fleet is not `Clojure with #?(:clj ...)` escapes;
   it is plain Clojure that happens to run on JVM, cljw and cljrs, with every
   host-specific thing pushed out to a boundary. hive-creator and
   hive-kdenlive carry the same test over their own cores.

   `codec` stays JVM on clojure.data.json; `wire` is its portable twin, held
   to it by hive-gimp.wire-test and run on cljw and cljrs by
   dev/wire_portability.cljc."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk])
  (:import (java.io File PushbackReader)))

(def banned-prefixes
  "Namespace prefixes that tie a file to one host."
  ["malli" "hive-addon" "hive-di" "clojure.java." "clojure.data.json"
   "taoensso.timbre" "libpython-clj" "java."])

(defn- portable-files []
  (->> (file-seq (io/file "src"))
       (filter #(.isFile ^File %))
       (filter #(str/ends-with? (.getName ^File %) ".cljc"))
       (sort-by #(.getPath ^File %))))

(defn- ns-form [^File file]
  (with-open [r (PushbackReader. (io/reader file))]
    (read {:read-cond :preserve} r)))

(defn- referenced-namespaces [form]
  (let [found (atom [])]
    (walk/postwalk (fn [x] (when (symbol? x) (swap! found conj (str x))) x) form)
    @found))

(defn- violations [^File file]
  (for [referenced (referenced-namespaces (ns-form file))
        banned     banned-prefixes
        :when      (str/starts-with? referenced banned)]
    {:referenced referenced :banned banned}))

(deftest there-is-a-portable-core-test
  (let [files (portable-files)
        names (set (map #(.getName ^File %) files))]
    (is (seq files) "no .cljc under src, so the portable core is gone")
    (doseq [n ["shape.cljc"     ; the value objects, as data
               "ports.cljc"     ; the seams
               "verdict.cljc"   ; the doctor's judgements
               "command.cljc"   ; descriptor + args -> GimpCommand
               "response.cljc"  ; raw answer -> Outcome
               "wire.cljc"]     ; JSON + framing, for hosts without a JSON library
            ]
      (is (contains? names n) (str n " left the portable core")))
    (testing "the command vocabulary is the point: building a GIMP script and
              reading its answer are what a cljw or cljrs host needs"
      (is (contains? names "command.cljc"))
      (is (contains? names "response.cljc")))))

(deftest no-portable-namespace-requires-a-host-only-library-test
  (doseq [^File file (portable-files)]
    (testing (.getName file)
      (is (= [] (vec (violations file)))
          (str (.getName file) " names a host-only library in its ns form."
               " If it belongs here, it belongs at a boundary instead"
               " (hive-gimp.schema / transport.* / addon)")))))

(deftest no-portable-namespace-hides-behind-a-reader-conditional-test
  (doseq [^File file (portable-files)]
    (testing (.getName file)
      (is (not (str/includes? (slurp file) "#?("))
          (str (.getName file) " contains a reader conditional. A host"
               " difference means the code belongs at a boundary, not that it"
               " needs an escape hatch")))))

(deftest the-boundary-namespaces-stay-on-the-jvm-test
  (doseq [path ["src/hive_gimp/schema.clj"
                "src/hive_gimp/addon.clj"
                "src/hive_gimp/catalog.clj"
                "src/hive_gimp/transport/socket.clj"]]
    (is (.exists (io/file path)) (str path " is a boundary and must stay .clj"))
    (is (not (.exists (io/file (str/replace path ".clj" ".cljc"))))
        (str path " became .cljc; it needs malli, the classpath, a socket or"
             " the MCP surface and cannot be portable"))))

(deftest the-check-would-actually-catch-a-violation-test
  (testing "a ns form that requires malli is caught"
    (let [tmp (File/createTempFile "portability" ".cljc")]
      (try
        (spit tmp "(ns probe (:require [malli.core :as m] [clojure.string :as str]))\n")
        (is (= [{:referenced "malli.core" :banned "malli"}] (vec (violations tmp))))
        (finally (.delete tmp)))))
  (testing "a ns form that only names portable things is clean"
    (let [tmp (File/createTempFile "portability" ".cljc")]
      (try
        (spit tmp "(ns probe (:require [clojure.string :as str]))\n")
        (is (= [] (vec (violations tmp))))
        (finally (.delete tmp))))))
