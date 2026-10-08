(ns hive-gimp.py.lower-test
  "Clojure forms lowered to the Python AST of docs/py-strata.md, as data."
  (:require [clojure.string]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.walk]
            [hive-gimp.py.lower :as lower]
            [hive-gimp.py.lower.core]
            [hive-gimp.py.names :as names]
            [hive-gimp.py.template :refer [forms]]
            [hive-gimp.py.value :as value]
            [hive-schemas.schema :as hs]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(defn- n [id] {:py/node :name :id id})
(defn- l [v] {:py/node :literal :value v})
(defn- attr [o a] {:py/node :attr :object o :attr a})
(defn- call
  ([f args] (call f args []))
  ([f args kwargs] {:py/node :call :fn f :args args :kwargs kwargs}))
(defn- binop [op & xs] {:py/node :binop :op op :operands (vec xs)})
(defn- sub [o i] {:py/node :subscript :object o :index i})
(defn- stmt [e] {:py/node :expr-stmt :expr e})
(defn- ret [e] {:py/node :return :value e})

(defn- expr [form] (lower/lower-expr form))
(defmacro ^:private e [form] `(expr (first (forms ~form))))
(defmacro ^:private s [& body] `(lower/lower-stmts (forms ~@body)))

(defn- refusal [thunk]
  (try (thunk) nil (catch clojure.lang.ExceptionInfo x (ex-data x))))

;; =============================================================================
;; Atoms, collections, names
;; =============================================================================

(deftest atoms-and-collections
  (is (= (n "Gimp.get_images") (e Gimp/get-images)))
  (is (= (n "Gimp.ChannelOps.REPLACE") (e Gimp.ChannelOps/REPLACE)))
  (is (= (l 1) (e 1)))
  (is (= (l "s") (e "s")))
  (is (= (l :k) (e :k)))
  (is (= (l nil) (e nil)))
  (is (= {:py/node :list :items [(l 1) (n "a")]} (e [1 a])))
  (is (= {:py/node :dict :entries [[(l :a) (n "b")]]} (e {:a b})))
  (is (= {:py/node :set :items [(n "a")]} (e #{a})))
  (is (= {:py/node :tuple :items []} (e ()))))

(deftest spliced-values-are-literals-never-syntax
  (let [xs [1 2] m {:a 1}]
    (is (= (l [1 2]) (expr (first (forms ~xs)))))
    (is (= (call (n "f") [(l {:a 1}) (l 1) (l 2)]) (expr (first (forms (f ~m ~@xs))))))))

;; =============================================================================
;; Calls, keyword arguments and sugar
;; =============================================================================

(deftest calls-and-keyword-arguments
  (is (= (call (n "f") [(n "a") (l 1)] [["fill_type" (l 2)]]) (e (f a 1 :fill-type 2))))
  (is (= (call (call (n "f") []) [(n "x")]) (e ((f) x))) "a non-symbol head is called"))

(deftest method-attribute-chain-and-constructor
  (let [m (call (attr (n "l") "get_name") [])]
    (is (= m (e (.get-name l))))
    (is (= m (e (py. l get-name))))
    (is (= m (e (. l get-name)))))
  (is (= (call (attr (n "img") "select_ellipse") [(n "Gimp.ChannelOps.REPLACE") (l 10)] [["w" (l 32)]])
         (e (.select-ellipse img Gimp.ChannelOps/REPLACE 10 :w 32))))
  (is (= (attr (n "h") "count") (e (.-count h))))
  (is (= (attr (n "h") "count") (e (py.- h count))))
  (is (= (call (attr (call (attr (sub (call (attr (n "img") "get_layers") []) (l 0)) "get_name") []) "upper") [])
         (e (py.. (first (.get-layers img)) get-name (upper)))))
  (is (= (attr (n "img") "__class__") (e (.. img -__class__))))
  (is (= (call (attr (n "o") "m") [(l 1)]) (e (py.. o (m 1)))))
  (is (= (call (n "Gegl.Color") [(l "#1a20cf")]) (e (Gegl.Color. "#1a20cf"))))
  (is (= (call (n "Gimp.Image") [(l 1)]) (e (Gimp/Image. 1))))
  (is (= (call (n "C") []) (e (new C)))))

;; =============================================================================
;; Operators and expression helpers
;; =============================================================================

(deftest infix-operators-are-registered-as-data
  (doseq [{:keys [head op]} @#'hive-gimp.py.lower.expr/infix-operators]
    (is (= (binop op (n "a") (n "b") (n "c")) (expr (list head 'a 'b 'c))) (str head))))

(deftest unary-forms
  (is (= {:py/node :unop :op "-" :operand (n "a")} (e (- a))))
  (is (= {:py/node :unop :op "not" :operand (n "c")} (e (not c))))
  (is (= :py/unsupported-form (:hive-gimp/reason (refusal #(e (+ a)))))))

(deftest expression-helpers
  (is (= {:py/node :cond-expr :test (n "a") :then (n "b") :else (l nil)} (e (if a b))))
  (is (= {:py/node :lambda :params {:fixed ["a"] :rest "r"} :body (binop "+" (n "a") (l 1))}
         (e (fn [a & r] (inc a)))))
  (is (= {:py/node :comp :element (binop "**" (n "x") (l 2))
          :clauses [{:for (n "x") :in (call (n "range") [(l 5)])}
                    {:if (binop "==" (binop "%" (n "x") (l 2)) (l 0))}]}
         (e (for [x (range 5) :when (= (mod x 2) 0)] (** x 2)))))
  (is (= {:py/node :tuple :items [(l 1)]} (e (tuple 1))))
  (is (= (sub (n "xs") (l 0)) (e (get xs 0))))
  (is (= (sub (n "xs") (n "i")) (e (nth xs i))))
  (is (= (call (attr (n "d") "get") [(l :k) (l nil)]) (e (get d :k nil))))
  (is (= (sub (sub (n "d") (l "a")) (l 0)) (e (get-in d ["a" 0]))))
  (is (= (sub (n "xs") (l 0)) (e (first xs))))
  (is (= (sub (n "xs") (l 1)) (e (second xs))))
  (is (= (sub (n "xs") (l -1)) (e (last xs))))
  (is (= {:py/node :slice :object (n "xs") :lower (l 1) :upper nil} (e (slice xs 1 nil))))
  (is (= {:py/node :slice :object (n "xs") :lower nil :upper (n "b")} (e (slice xs nil b))))
  (is (= (call (n "len") [(n "xs")]) (e (count xs))))
  (is (= (binop "in" (n "x") (n "xs")) (e (contains? xs x))))
  (is (= (binop "+" (n "a") (l 1)) (e (inc a))))
  (is (= (binop "-" (n "a") (l 1)) (e (dec a))))
  (is (= (call (attr (l "") "join") [(call (n "map") [(n "str") {:py/node :list :items [(n "a") (l 1)]}])])
         (e (str a 1))))
  (is (= {:py/node :raw :code "x.y"} (e (raw "x.y")))))

;; =============================================================================
;; Statements
;; =============================================================================

(deftest bindings-and-blocks
  (is (= [{:py/node :assign :target (n "x") :value (l 1)}] (s (def x 1))))
  (is (= [{:py/node :assign :target {:py/node :tuple :items [(n "a") (n "b")]} :value (n "p")}]
         (s (def [a b] p))))
  (is (= [{:py/node :assign :target (attr (n "o") "x") :value (l 1)}] (s (set! (.-x o) 1))))
  (is (= [(stmt (call (n "f") [])) (stmt (call (n "g") []))] (s (do (f) (g)))))
  (is (= [{:py/node :assign :target (n "a") :value (l 1)} (stmt (call (n "f") [(n "a")]))]
         (s (let [a 1] (f a)))))
  (is (= [{:py/node :if :test (n "a") :body [(stmt (n "b"))] :orelse nil}] (s (if a b))))
  (is (= [{:py/node :if :test (n "a") :body [(stmt (n "b"))] :orelse [(stmt (n "c"))]}] (s (if a b c))))
  (is (= [{:py/node :if :test (n "a") :body [] :orelse nil}] (s (when a))))
  (is (= [{:py/node :if :test {:py/node :unop :op "not" :operand (n "a")} :body [(stmt (n "b"))] :orelse nil}]
         (s (when-not a b))))
  (is (= [{:py/node :while :test (n "a") :body [(stmt (n "b"))]}] (s (while a b)))))

(deftest loops
  (is (= [{:py/node :for :target {:py/node :tuple :items [(n "i") (n "l")]}
           :iter (call (n "enumerate") [(n "ls")])
           :body [{:py/node :if :test (binop ">" (n "i") (l 0))
                   :body [(stmt (call (n "print") [(n "i")]))] :orelse nil}]}]
         (s (doseq [[i l] (enumerate ls)] (when (> i 0) (print i))))))
  (is (= [{:py/node :for :target (n "i") :iter (call (n "range") [(l 2)])
           :body [{:py/node :for :target (n "j") :iter (call (n "range") [(n "js")]) :body [(stmt (n "j"))]}]}]
         (s (dotimes [i 2 j js] j)))
      "dotimes ranges every binding, nested"))

(deftest defn-returns-its-tail
  (is (= [{:py/node :def :name "half" :params {:fixed ["a" "b"] :rest nil}
           :body [{:py/node :if :test (binop ">" (n "a") (n "b"))
                   :body [(ret (binop "/" (n "a") (l 2)))]
                   :orelse [{:py/node :assign :target (n "c") :value (binop "*" (n "b") (l 2))}
                            (ret (n "c"))]}]}]
         (s (defn half [a b] (if (> a b) (/ a 2) (let [c (* b 2)] c))))))
  (is (= [{:py/node :def :name "f" :params {:fixed [] :rest nil}
           :body [(stmt (call (n "g") [])) (ret (n "x"))]}]
         (s (defn f [] (do (g) x)))))
  (is (= [{:py/node :def :name "f" :params {:fixed [] :rest nil}
           :body [{:py/node :assign :target (n "y") :value (l 1)}]}]
         (s (defn f [] (def y 1))))
      "a statement-only tail returns nothing")
  (is (= [{:py/node :def :name "f" :params {:fixed [] :rest nil} :body [{:py/node :raw-stmt :code "pass"}]}]
         (s (defn f [] (raw "pass")))))
  (is (= [{:py/node :def :name "f" :params {:fixed ["c"] :rest nil}
           :body [{:py/node :if :test (n "c") :body [(ret (n "x"))] :orelse nil}]}]
         (s (defn f [c] (when c x))))
      "a `when` tail returns its body's value")
  (is (= [{:py/node :def :name "f" :params {:fixed [] :rest nil} :body []}] (s (defn f [])))))

(deftest return-import-try-raw
  (is (= [(ret nil)] (s (return))))
  (is (= [(ret (n "x"))] (s (return x))))
  (is (= [{:py/node :import :module "json" :as nil}] (s (import json))))
  (is (= [{:py/node :import :module "numpy" :as "np"}] (s (import [numpy :as np]))))
  (is (= [{:py/node :from-import :module "gi.repository" :names ["Gio" "Gegl"]}]
         (s (import gi.repository [Gio Gegl]))))
  (is (= [{:py/node :try :body [(stmt (call (n "f") []))]
           :handlers [{:type (n "Exception") :name "e" :body [(stmt (call (n "print") [(n "e")]))]}]
           :finally [(stmt (call (n "g") []))]}]
         (s (try (f) (catch Exception e (print e)) (finally (g))))))
  (is (= [{:py/node :try :body [(stmt (n "a"))] :handlers [] :finally nil}] (s (try a))))
  (is (= [{:py/node :raw-stmt :code "x = 1\ny = 2"}] (s (raw "x = 1\ny = 2"))))
  (is (= [(stmt (call (n "f") []))] (s (f))) "any other form is an expression statement"))

(deftest try-in-tail-position-returns-from-each-branch
  (is (= {:py/node :try :body [(ret (n "a"))]
          :handlers [{:type (n "E") :name "e" :body [(ret (n "b"))]}]
          :finally [(stmt (call (n "g") []))]}
         (first (lower/lower-tail (forms (try a (catch E e b) (finally (g)))))))))

;; =============================================================================
;; Refusals
;; =============================================================================

(deftest refusals-name-the-form
  (doseq [[label thunk] {"bad name"          #(e (ok? 1))
                         "kwarg without value" #(e (f :k))
                         "multi-form lambda" #(e (fn [a] (f a) (g a)))
                         "bad target"        #(s (def 1 2))
                         "bad import"        #(s (import "x"))
                         "bad chain step"    #(e (py.. o 1))
                         "non-symbol method" #(e (py. o "m"))}]
    (let [data (refusal thunk)]
      (is (= :py/unsupported-form (:hive-gimp/reason data)) label)
      (is (contains? data :form) label))))

;; =============================================================================
;; Registries are open
;; =============================================================================

(deftest every-contract-head-is-a-registration
  (let [contract {lower/expr-form '[if fn for tuple get nth get-in first second last slice count
                                    contains? inc dec str raw not . py. py.- .. py..
                                    + - * / mod quot ** < > <= >= = not= is and or bit-and bit-or]
                  lower/stmt-form '[def set! do let if when when-not while doseq dotimes defn
                                    return import try raw]
                  lower/tail-form '[do let if try]}]
    (doseq [[mm heads] contract h heads]
      (is (contains? (methods mm) h) (str h)))))

(deftest a-new-form-is-one-defmethod
  (let [h 'hive-gimp-test-probe]
    (try
      (.addMethod ^clojure.lang.MultiFn lower/expr-form h (fn [[_ x]] (l [:probe x])))
      (is (= (l [:probe 7]) (expr (list h 7))))
      (finally (remove-method lower/expr-form h)))))

;; =============================================================================
;; Trifectas: golden + property + mutation
;; =============================================================================

(def ^:private gen-name-symbol
  (gen/fmap symbol (mg/generator names/PyName)))

(def ^:private gen-atom
  (gen/one-of [gen-name-symbol
               (gen/elements [nil true false 0 1 -2 2.5 "s" :k :fill-type])
               (gen/fmap value/lit (mg/generator value/LiteralValue))]))

(def ^:private heads
  "Every registered head plus sugar spellings and plain callables."
  (into ['f 'Gimp/get-images '.get-name '.-count 'Gegl.Color. 'ok?]
        (concat (keys (methods lower/expr-form)) (keys (methods lower/stmt-form)))))

(def ^:private gen-form
  (gen/recursive-gen
   (fn [inner]
     (gen/one-of [(gen/vector inner 0 3)
                  (gen/fmap (fn [[h args]] (apply list h args))
                            (gen/tuple (gen/elements (remove #{:default} heads)) (gen/vector inner 0 3)))]))
   gen-atom))

(declare lowered-or-refused?)

(def ^:private infix-heads (mapv :head @#'hive-gimp.py.lower.expr/infix-operators))

(def ^:private gen-expr
  "Well-formed expression forms: every one lowers."
  (gen/recursive-gen
   (fn [x]
     (let [xs (gen/vector x 0 3)]
       (gen/one-of
        [(gen/vector x 0 3)
         (gen/fmap (fn [[h a b more]] (apply list h a b more))
                   (gen/tuple (gen/elements infix-heads) x x (gen/vector x 0 2)))
         (gen/fmap (fn [[h a]] (list h a))
                   (gen/tuple (gen/elements '[not - count inc dec first second last .-attr]) x))
         (gen/fmap (fn [[h a b]] (list h a b))
                   (gen/tuple (gen/elements '[get nth contains? slice]) x x))
         (gen/fmap (fn [[c t e]] (list 'if c t e)) (gen/tuple x x x))
         (gen/fmap (fn [[h args]] (apply list h args))
                   (gen/tuple (gen/elements '[f Gimp/get-images Gegl.Color. str tuple]) xs))
         (gen/fmap (fn [[o args kw v]] (apply list '.get-name o (concat args [kw v])))
                   (gen/tuple x xs (gen/elements [:w :fill-type]) x))
         (gen/fmap (fn [[s coll body]] (list 'for [s coll :when body] body))
                   (gen/tuple gen-name-symbol x x))
         (gen/fmap (fn [[ps body]] (list 'fn (vec ps) body))
                   (gen/tuple (gen/vector gen-name-symbol 0 2) x))])))
   (gen/one-of [gen-name-symbol
                (gen/elements [nil true false 0 1 -2 2.5 "s"])
                (gen/fmap value/lit (mg/generator value/LiteralValue))])))

(def ^:private gen-stmt
  "Well-formed statement forms: every one lowers."
  (gen/recursive-gen
   (fn [s]
     (let [ss (gen/vector s 0 2)]
       (gen/one-of
        [(gen/fmap (fn [[t v]] (list 'def t v)) (gen/tuple gen-name-symbol gen-expr))
         (gen/fmap (fn [[c body]] (apply list 'when c body)) (gen/tuple gen-expr ss))
         (gen/fmap (fn [[c t e]] (list 'if c t e)) (gen/tuple gen-expr s s))
         (gen/fmap (fn [[v coll body]] (apply list 'doseq [v coll] body)) (gen/tuple gen-name-symbol gen-expr ss))
         (gen/fmap (fn [[n ps body]] (apply list 'defn n (vec ps) body))
                   (gen/tuple gen-name-symbol (gen/vector gen-name-symbol 0 2) (gen/vector gen-expr 0 3)))
         (gen/fmap (fn [body] (apply list 'try (concat body [(list 'catch 'Exception 'e '(print e))])))
                   ss)
         (gen/fmap (fn [v] (list 'return v)) gen-expr)])))
   gen-expr))

(defspec any-form-lowers-or-is-refused 300
  (prop/for-all [form gen-form]
    (and (lowered-or-refused? lower/lower-expr hive-gimp.py.lower.core/Node form)
         (lowered-or-refused? lower/lower-stmts hive-gimp.py.lower.core/Module [form]))))

(defn- lowered-or-refused?
  "`lower` either returns a value of `schema` or refuses with the contract's
   ex-data; nothing else escapes."
  [lower schema input]
  (try
    (hs/validate schema (lower input))
    (catch clojure.lang.ExceptionInfo x (hs/validate names/Refusal (ex-data x)))))

(def ^:private lower-expr* lower/lower-expr)
(def ^:private lower-stmts* lower/lower-stmts)

(defn- map-nodes
  "`f` over every node map in `x`."
  [f x]
  (clojure.walk/postwalk #(if (and (map? %) (:py/node %)) (f %) %) x))

(deftrifecta lower-expr hive-gimp.py.lower/lower-expr
  {:golden-path "test/golden/hive_gimp/py/lower-expr.edn"
   :cases       {:name        'Gimp/get-images
                 :literal     (value/lit {"a" [1 nil]})
                 :collections '[1 {:a b} #{c}]
                 :call-kwargs '(f a 1 :fill-type 2)
                 :method      '(.select-ellipse img REPLACE 10 :w 32)
                 :attribute   '(.-count h)
                 :chain       '(py.. (first (.get-layers img)) get-name (upper))
                 :constructor '(Gegl.Color. "#1a20cf")
                 :infix       '(* (inc a) (- b) (mod c 2))
                 :logic       '(and (= a b) (not c) (contains? xs x))
                 :cond-expr   '(if a b)
                 :lambda      '(fn [a & r] (inc a))
                 :comp        '(for [x (range 5) :when (= (mod x 2) 0)] (** x 2))
                 :access      '[(get d :k nil) (nth xs 1) (get-in d ["a" 0]) (last xs) (slice xs 1 nil)]
                 :helpers     '[(count xs) (dec a) (str a 1) (tuple 1) (raw "x.y")]}
   :gen         gen-expr
   :pred        #(hs/validate hive-gimp.py.lower.core/Node %)
   :num-tests   300
   :mutations   [["unary-as-binop" (fn [f] (map-nodes #(cond-> % (= :unop (:py/node %)) (assoc :py/node :binop)) (lower-expr* f)))]
                 ["kwargs-dropped" (fn [f] (map-nodes #(cond-> % (= :call (:py/node %)) (assoc :kwargs [])) (lower-expr* f)))]
                 ["cond-expr-swapped" (fn [f] (map-nodes #(cond-> % (= :cond-expr (:py/node %)) (assoc :then (:else %) :else (:then %))) (lower-expr* f)))]
                 ["dashes-kept" (fn [f] (map-nodes #(cond-> % (= :name (:py/node %)) (update :id clojure.string/replace "_" "-")) (lower-expr* f)))]
                 ["spliced-value-read-as-syntax" (fn [f] (lower-expr* (if (value/lit? f) (:value f) f)))]
                 ["operands-reversed" (fn [f] (map-nodes #(cond-> % (= :binop (:py/node %)) (update :operands (comp vec reverse))) (lower-expr* f)))]]})

(deftrifecta lower-stmts hive-gimp.py.lower/lower-stmts
  {:golden-path "test/golden/hive_gimp/py/lower-stmts.edn"
   :cases       {:bindings '[(def x 1) (def [a b] p) (set! (.-x o) 1) (let [c 2] (f c))]
                 :blocks   '[(do (f) (g)) (if a b) (if a b c) (when a) (when-not a b) (while a b)]
                 :loops    '[(doseq [[i l] (enumerate ls)] (when (> i 0) (print i))) (dotimes [i 2] i)]
                 :defn     '[(defn half [a b] (if (> a b) (/ a 2) (let [c (* b 2)] c)))
                             (defn f [c] (when c x)) (defn g [] (def y 1)) (defn h [] (try a (catch E e b)))]
                 :misc     '[(return) (return x) (import json) (import [numpy :as np])
                             (import gi.repository [Gio Gegl]) (raw "x = 1\ny = 2")
                             (try (f) (catch Exception e (print e)) (finally (g))) (f)]}
   :gen         (gen/vector gen-stmt 0 4)
   :pred        #(hs/validate hive-gimp.py.lower.core/Module %)
   :num-tests   300
   :mutations   [["last-statement-dropped" (fn [fs] (vec (butlast (lower-stmts* fs))))]
                 ["orelse-dropped" (fn [fs] (map-nodes #(cond-> % (= :if (:py/node %)) (assoc :orelse nil)) (lower-stmts* fs)))]
                 ["returns-dropped" (fn [fs] (map-nodes #(cond-> % (= :return (:py/node %)) (-> (assoc :py/node :expr-stmt :expr (:value %)) (dissoc :value))) (lower-stmts* fs)))]
                 ["import-alias-lost" (fn [fs] (map-nodes #(cond-> % (= :import (:py/node %)) (assoc :as nil)) (lower-stmts* fs)))]
                 ["handlers-dropped" (fn [fs] (map-nodes #(cond-> % (= :try (:py/node %)) (assoc :handlers [])) (lower-stmts* fs)))]]})

(deftest malformed-forms-refuse-instead-of-crashing
  (doseq [form '[(get-in d 5) (fn 1 2) (for 1 x) (new)]]
    (is (lowered-or-refused? lower/lower-expr hive-gimp.py.lower.core/Node form) (pr-str form))
    (is (thrown? clojure.lang.ExceptionInfo (lower/lower-expr form)) (pr-str form)))
  (is (thrown? clojure.lang.ExceptionInfo (lower/lower-stmts '[(doseq 5 x)]))))

(deftest a-keyword-is-a-keyword-argument-only-in-a-call
  (is (= (call (n "len") [(l :k)]) (e (count :k))))
  (is (= (call (attr (n "d") "get") [(l :k) (l nil)]) (e (get d :k nil))))
  (is (= (call (n "f") [] [["k" (l :v)]]) (e (f :k :v)))))
