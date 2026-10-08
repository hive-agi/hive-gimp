(ns hive-gimp.py.render-test
  "Every byte-exact rendering rule in docs/py-strata.md, node by node."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-gimp.py.ast :as ast]
            [hive-gimp.py.render :as r]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- n [k & kvs] (apply array-map :py/node k kvs))
(defn- nm [id] (n :name :id id))
(defn- lit [v] (n :literal :value v))
(defn- src [& lines] (str/join "\n" lines))
(defn- stmt [e] (n :expr-stmt :expr e))

(def ^:private a (nm "a"))
(def ^:private b (nm "b"))

;; =============================================================================
;; Literals
;; =============================================================================

(deftest literals
  (doseq [[v expected] [[nil "None"] [true "True"] [false "False"]
                        ["s/t" "\"s/t\""] ["q\"\n" "\"q\\\"\\n\""]
                        [:a "\"a\""] [:a/b "\"a/b\""]
                        [42 "42"] [(bigint 9) "9"] [1/2 "0.5"] [2.5 "2.5"]
                        [##NaN "float('nan')"] [##Inf "float('inf')"] [##-Inf "float('-inf')"]
                        [[1 [2]] "[1, [2]]"] ['(1 2) "[1, 2]"]
                        [{"a" 1 :b nil} "{\"a\": 1, \"b\": None}"]
                        [#{} "set()"] [#{1} "{1}"]]]
    (is (= expected (r/literal v)) (pr-str v))
    (is (= expected (r/expr (lit v))) "a :literal node renders its value")))

(deftest a-value-with-no-literal-is-refused
  (let [e (try (r/literal (Object.)) (catch clojure.lang.ExceptionInfo e e))]
    (is (= :py/unsupported-form (:hive-gimp/reason (ex-data e))))))

(defspec string-literals-are-json-strings-python-reads-back 100
  (prop/for-all [s gen/string]
    (= s (json/read-str (r/literal s)))))

;; =============================================================================
;; Expressions
;; =============================================================================

(deftest names-raw-and-attributes
  (is (= "Gimp.get_images" (r/expr (nm "Gimp.get_images"))))
  (is (= "1 + 1" (r/expr (n :raw :code "1 + 1"))))
  (is (= "img.get_name" (r/expr (n :attr :object (nm "img") :attr "get_name")))))

(deftest calls-subscripts-and-slices
  (is (= "f(a, 1, w=32)" (r/expr (n :call :fn (nm "f") :args [a (lit 1)] :kwargs [["w" (lit 32)]]))))
  (is (= "f()" (r/expr (n :call :fn (nm "f") :args [] :kwargs []))))
  (is (= "xs[0]" (r/expr (n :subscript :object (nm "xs") :index (lit 0)))))
  (is (= "xs[1:]" (r/expr (n :slice :object (nm "xs") :lower (lit 1) :upper nil))))
  (is (= "xs[:2]" (r/expr (n :slice :object (nm "xs") :upper (lit 2)))))
  (is (= "xs[1:2]" (r/expr (n :slice :object (nm "xs") :lower (lit 1) :upper (lit 2))))))

(deftest operators
  (is (= "(a + b + 1)" (r/expr (n :binop :op "+" :operands [a b (lit 1)]))))
  (is (= "(a and b)" (r/expr (n :binop :op "and" :operands [a b]))))
  (is (= "(-a)" (r/expr (n :unop :op "-" :operand a))))
  (is (= "(not a)" (r/expr (n :unop :op "not" :operand a))))
  (is (= "(b if a else None)" (r/expr (n :cond-expr :test a :then b :else (lit nil))))))

(deftest lambdas-and-comprehensions
  (is (= "(lambda a, *r: (a + 1))"
         (r/expr (n :lambda :params {:fixed ["a"] :rest "r"} :body (n :binop :op "+" :operands [a (lit 1)])))))
  (is (= "(lambda : a)" (r/expr (n :lambda :params {:fixed []} :body a))))
  (is (= "[(x ** 2) for x in range(5) if ((x % 2) == 0)]"
         (r/expr (n :comp
                    :element (n :binop :op "**" :operands [(nm "x") (lit 2)])
                    :clauses [{:for (nm "x") :in (n :call :fn (nm "range") :args [(lit 5)] :kwargs [])}
                              {:if (n :binop :op "==" :operands [(n :binop :op "%" :operands [(nm "x") (lit 2)]) (lit 0)])}]))))
  (is (= "[l for (i, l) in xs]"
         (r/expr (n :comp :element (nm "l") :clauses [{:for (n :tuple :items [(nm "i") (nm "l")]) :in (nm "xs")}])))))

(deftest collections
  (is (= "[a, b]" (r/expr (n :list :items [a b]))))
  (is (= "[]" (r/expr (n :list :items []))))
  (is (= "(a, b)" (r/expr (n :tuple :items [a b]))))
  (is (= "(a,)" (r/expr (n :tuple :items [a]))) "a one-item tuple keeps its comma")
  (is (= "{a}" (r/expr (n :set :items [a]))))
  (is (= "set()" (r/expr (n :set :items []))) "{} would be a dict")
  (is (= "{\"k\": a}" (r/expr (n :dict :entries [[(lit "k") a]])))))

(deftest an-unregistered-node-is-refused
  (doseq [render [#(r/expr %) #(r/stmt %)]]
    (let [e (try (render (n :walrus)) (catch clojure.lang.ExceptionInfo e e))]
      (is (= :py/unsupported-form (:hive-gimp/reason (ex-data e)))))))

;; =============================================================================
;; Statements
;; =============================================================================

(deftest simple-statements
  (is (= "t = (1,)" (r/module [(n :assign :target (nm "t") :value (n :tuple :items [(lit 1)]))])))
  (is (= "(i, l) = x" (r/module [(n :assign :target (n :tuple :items [(nm "i") (nm "l")]) :value (nm "x"))])))
  (is (= "f()" (r/module [(stmt (n :call :fn (nm "f") :args [] :kwargs []))])))
  (is (= "return a" (r/module [(n :return :value a)])))
  (is (= "return" (r/module [(n :return :value nil)])))
  (is (= "import json" (r/module [(n :import :module "json")])))
  (is (= "import numpy as np" (r/module [(n :import :module "numpy" :as "np")])))
  (is (= "from gi.repository import Gio, Gegl"
         (r/module [(n :from-import :module "gi.repository" :names ["Gio" "Gegl"])])))
  (is (= (src "x = 1" "y = 2") (r/module [(n :raw-stmt :code "x = 1\ny = 2")]))))

(deftest blocks-indent-four-and-an-empty-block-is-pass
  (is (= (src "if a:" "    pass") (r/module [(n :if :test a :body [])])))
  (is (= (src "while a:" "    b") (r/module [(n :while :test a :body [(stmt b)])])))
  (is (= (src "for (i, l) in enumerate(ls):"
              "    if (i > 0):"
              "        print(i)")
         (r/module [(n :for
                       :target (n :tuple :items [(nm "i") (nm "l")])
                       :iter (n :call :fn (nm "enumerate") :args [(nm "ls")] :kwargs [])
                       :body [(n :if :test (n :binop :op ">" :operands [(nm "i") (lit 0)])
                                 :body [(stmt (n :call :fn (nm "print") :args [(nm "i")] :kwargs []))])])]))))

(deftest else-renders-only-when-orelse-is-present
  (is (= (src "if a:" "    b") (r/module [(n :if :test a :body [(stmt b)] :orelse nil)])))
  (is (= (src "if a:" "    b" "else:" "    pass") (r/module [(n :if :test a :body [(stmt b)] :orelse [])]))
      "an empty orelse is still an else"))

(deftest def-renders-params-and-body
  (is (= (src "def half(a, b):"
              "    if (a > b):"
              "        return (a / 2)"
              "    else:"
              "        c = (b * 2)"
              "        return c")
         (r/module [(n :def :name "half" :params {:fixed ["a" "b"] :rest nil}
                       :body [(n :if :test (n :binop :op ">" :operands [a b])
                                 :body [(n :return :value (n :binop :op "/" :operands [a (lit 2)]))]
                                 :orelse [(n :assign :target (nm "c") :value (n :binop :op "*" :operands [b (lit 2)]))
                                          (n :return :value (nm "c"))])])])))
  (is (= (src "def f(*r):" "    pass") (r/module [(n :def :name "f" :params {:fixed [] :rest "r"} :body [])]))))

(deftest try-renders-handlers-and-finally
  (let [call (fn [f & args] (stmt (n :call :fn (nm f) :args (vec args) :kwargs [])))]
    (is (= (src "try:" "    f()" "except Exception as e:" "    print(e)" "finally:" "    g()")
           (r/module [(n :try :body [(call "f")]
                         :handlers [{:type (nm "Exception") :name "e" :body [(call "print" (nm "e"))]}]
                         :finally [(call "g")])])))
    (is (= (src "try:" "    pass" "except Exception as e:" "    pass")
           (r/module [(n :try :body [] :handlers [{:type (nm "Exception") :name "e" :body []}] :finally nil)]))
        "no finally: clause without :finally")))

(deftest a-module-is-statements-joined-by-newlines
  (let [m [(n :import :module "json") (n :assign :target a :value (lit 1)) (stmt a)]]
    (is (ast/valid? ast/Module m))
    (is (= (src "import json" "a = 1" "a") (r/module m)))
    (is (= ["import json" "a = 1" "a"] (r/stmts m)))
    (is (= "" (r/module [])))))
