(ns hive-gimp.py.template-test
  "Forms as data with `~x` / `~@xs`: golden, property and mutation over the
   code `template` builds, and the `forms` macro itself."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk :as walk]
            [hive-gimp.py.template :as template :refer [forms]]
            [hive-gimp.py.value :as value]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- built
  "Evaluate the code `template` returned, spliced values shown as `[:lit v]`."
  [code]
  (walk/postwalk #(if (value/lit? %) [:lit (:value %)] %) (eval code)))

(def ^:private gen-literal (mg/generator value/LiteralValue))

(deftrifecta template hive-gimp.py.template/template
  {:golden-path "test/golden/hive_gimp/py/template.edn"
   :cases       {:symbol   'a
                 :call     '(f a 1 :k "s")
                 :unquote  '(f ~(+ 1 2))
                 :splice   '(f ~@[1 2] b)
                 :vector   '[a ~(str "x" "y")]
                 :map      '{:k ~(inc 1)}
                 :set      '#{a}
                 :nested   '(do (def x ~[1 2]) (.m o ~@(range 2)))}
   :xf          built
   :gen         gen-literal
   :pred        (fn [code] (hs/validate value/LiteralValue (eval code)))
   :mutations   [["quotes-everything" (fn [form] (list 'quote form))]
                 ["unquote-ignored" (fn [form] (if (seq? form) (list 'quote form) form))]
                 ["splice-as-one-value" (fn [form]
                                          (list 'quote (walk/postwalk
                                                        #(if (and (seq? %) (= 'clojure.core/unquote-splicing (first %)))
                                                           (value/lit (eval (second %)))
                                                           %)
                                                        form)))]]})

(defspec a-splice-yields-one-lit-per-value 200
  (prop/for-all [vs (gen/vector gen-literal 0 4)]
    (= (cons 'f (map value/lit vs))
       (eval (template/template (list 'f (list 'clojure.core/unquote-splicing (list 'quote vs))))))))

(deftest forms-splices-clojure-values
  (let [n 3 xs [1 2] label "logo v2"]
    (is (= ['(f a)] (forms (f a))))
    (is (= [(list 'f (value/lit 3) (value/lit "logo v2") (value/lit 1) (value/lit 2))]
           (forms (f ~n ~label ~@xs))))
    (is (= [(list 'f (value/lit [1 2]))] (forms (f ~xs))) "an unquoted vector is one value")
    (is (= 2 (count (forms (a) (b)))))))

(deftest every-spliced-literal-round-trips
  (let [vs (mg/sample value/LiteralValue {:size 30})]
    (is (= (map value/lit vs) (map #(first (forms ~%)) vs)))))
