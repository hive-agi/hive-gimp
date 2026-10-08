(ns hive-gimp.py-test
  "Clojure forms compiled to the Python GIMP runs. The compiler is pure, so most
   of this needs no transport; the round trips run against the doubles."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-gimp.py :as py]
            [hive-gimp.stub :as stub]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- src [& lines] (str/join "\n" lines))

;; =============================================================================
;; Names, calls, values
;; =============================================================================

(deftest names-follow-pygobject-spelling
  (is (= "Gimp.get_images()" (py/->python (py/forms (Gimp/get-images)))))
  (is (= "Gimp.ChannelOps.REPLACE" (py/->python (py/forms Gimp.ChannelOps/REPLACE))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not a Python name"
                        (py/->python (py/forms (ok? 1))))
      "a name Python cannot spell is refused here, not inside GIMP"))

(deftest calls-methods-attributes-and-constructors
  (is (= "img.select_ellipse(Gimp.ChannelOps.REPLACE, 10, 20, w=32)"
         (py/->python (py/forms (.select-ellipse img Gimp.ChannelOps/REPLACE 10 20 :w 32)))))
  (testing "libpython-clj's spellings compile to the same call"
    (is (= (py/->python (py/forms (.get-name l)))
           (py/->python (py/forms (py. l get-name)))
           (py/->python (py/forms (. l get-name))))))
  (is (= "h.count" (py/->python (py/forms (.-count h)))))
  (is (= "h.count" (py/->python (py/forms (py.- h count)))))
  (is (= "img.get_layers()[0].get_name().upper()"
         (py/->python (py/forms (py.. (first (.get-layers img)) get-name (upper))))))
  (is (= "img.__class__" (py/->python (py/forms (py.. img -__class__)))))
  (is (= "Gegl.Color(\"#1a20cf\")" (py/->python (py/forms (Gegl.Color. "#1a20cf"))))))

(deftest values-become-python-literals
  (is (= "x = {\"a\": [1, 2.5, None, True, False], \"b\": \"s/t\"}"
         (py/->python (py/forms (def x {"a" [1 2.5 nil true false] :b "s/t"}))))
      "keywords are strings, and \"/\" is not escaped into a stray backslash")
  (is (= "t = (1,)" (py/->python (py/forms (def t (tuple 1)))))))

(deftest unquote-splices-clojure-values
  (let [n 3 xs [1 2] label "logo v2"]
    (is (= "f(3, \"logo v2\", 1, 2)" (py/->python (py/forms (f ~n ~label ~@xs)))))
    (is (= "f([1, 2])" (py/->python (py/forms (f ~xs))))
        "an unquoted vector is a value, never syntax")))

;; =============================================================================
;; Expressions and statements
;; =============================================================================

(deftest operators-and-sequence-helpers
  (is (= "((a + 1) * 2)" (py/->python (py/forms (* (inc a) 2)))))
  (is (= "(-a)" (py/->python (py/forms (- a)))))
  (is (= "((a == b) and (not c))" (py/->python (py/forms (and (= a b) (not c))))))
  (is (= "(x in xs)" (py/->python (py/forms (contains? xs x)))))
  (is (= "xs[1:]" (py/->python (py/forms (slice xs 1 nil)))))
  (is (= "y = (b if a else None)" (py/->python (py/forms (def y (if a b)))))
      "inside an expression, if is Python's conditional expression")
  (is (= (src "if a:" "    b") (py/->python (py/forms (if a b))))
      "as a statement, it is an if block"))

(deftest comprehension-and-lambda
  (is (= "[(x ** 2) for x in range(5) if ((x % 2) == 0)]"
         (py/->python (py/forms (for [x (range 5) :when (= (mod x 2) 0)] (** x 2))))))
  (is (= "(lambda a, *r: (a + 1))" (py/->python (py/forms (fn [a & r] (inc a))))))
  (is (thrown? clojure.lang.ExceptionInfo (py/->python (py/forms (fn [a] (f a) (g a)))))
      "a lambda holds one expression; a body belongs in defn"))

(deftest blocks-indent-and-a-defn-returns-its-tail
  (is (= (src "for (i, l) in enumerate(ls):"
              "    if (i > 0):"
              "        print(i)")
         (py/->python (py/forms (doseq [[i l] (enumerate ls)] (when (> i 0) (print i)))))))
  (is (= (src "def half(a, b):"
              "    if (a > b):"
              "        return (a / 2)"
              "    else:"
              "        c = (b * 2)"
              "        return c")
         (py/->python (py/forms (defn half [a b] (if (> a b) (/ a 2) (let [c (* b 2)] c)))))))
  (is (= (src "if a:" "    pass") (py/->python (py/forms (when a))))
      "an empty block is still valid Python"))

(deftest imports-try-and-raw
  (is (= "from gi.repository import Gio, Gegl" (py/->python (py/forms (import gi.repository [Gio Gegl])))))
  (is (= "import numpy as np" (py/->python (py/forms (import [numpy :as np])))))
  (is (= (src "try:" "    f()" "except Exception as e:" "    print(e)" "finally:" "    g()")
         (py/->python (py/forms (try (f) (catch Exception e (print e)) (finally (g)))))))
  (is (= (src "x = 1" "y = 2") (py/->python (py/forms (raw "x = 1\ny = 2"))))))

;; =============================================================================
;; Round trips
;; =============================================================================

(deftest exec-sends-one-block-through-the-exec-channel
  (let [t (stub/recording (stub/always (stub/success-frame ["0\n1\n"])))
        out (py/exec t (doseq [i (range 2)] (print i)))]
    (is (= "0\n1\n" (:value out)))
    (let [[marker code] (get-in (first (stub/sent-requests t)) ["params" "args"])]
      (is (= "pyGObject-console" marker))
      (is (= [(src "for i in range(2):" "    print(i)")] code)
          "one round trip, whatever the number of forms"))))

(deftest eval-answers-data-not-a-repr
  (let [t (stub/always (stub/success-frame
                        ["hello\n\u001ehive-gimp.py [2168, 2096, {\"id\": 3, \"type\": \"Layer\", \"name\": \"P\"}]\n"]))
        out (py/eval t (def img (first (Gimp/get-images))) [(.get-width img) (.get-height img) layer])]
    (is (= [2168 2096 {:id 3 :type "Layer" :name "P"}] (:value out)))
    (is (= "hello" (:stdout out)) "what the forms printed is kept apart from the value")))

(deftest a-refusal-carries-the-python-that-was-sent
  (let [t (stub/always (stub/error-frame "name 'undefined_thing' is not defined"))
        out (py/exec t (undefined-thing 1))]
    (is (= :error (:outcome out)))
    (is (= "undefined_thing(1)" (:python out))))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"GIMP refused"
                        (py/eval! (stub/always (stub/error-frame "boom")) (f)))))
