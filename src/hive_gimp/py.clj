(ns hive-gimp.py
  "Drive GIMP's Python with Clojure forms instead of Python strings.

   WHY NOT libpython-clj

   libpython-clj embeds a CPython in the JVM. The Python that owns GIMP's
   objects is a different interpreter: GIMP's own PyGObject, inside a plug-in
   process GIMP launched and wired to itself. No interpreter outside that
   process can import `Gimp` and get a live image, so a JVM-side libpython-clj
   has nothing to hold. (The optional `:python` alias is for HOST-side pixel
   work, see `hive-gimp.transport.python`.)

   WHAT THIS DOES INSTEAD

   It keeps libpython-clj's call vocabulary (`py.`, `py..`, `py.-`, keyword
   arguments) and compiles the forms to ONE Python block, sent through
   `hive-gimp.exec` in a single round trip. GIMP still runs Python. The author
   writes and composes Clojure data, and `~x` splices a Clojure value in as a
   Python literal:

       (py/eval g
         (def img (first (Gimp/get-images)))
         [(.get-width img) (.get-height img) ~label])
       ;; => [2168 2096 \"logo\"]

   `eval` makes GIMP print its last expression as JSON and parses it back, so
   the answer is Clojure data, not a repr. GIMP objects come back as maps with
   their id, type and name.

   THE FORMS

   Names    `Gimp/get-images` is `Gimp.get_images`; `-` becomes `_`.
   Calls    `(f a :k v)` is `f(a, k=v)`; a keyword in an argument list always
            opens a keyword argument. `(Cls. a)` constructs.
   Methods  `(.m obj a)`, `(py. obj m a)`, `(. obj m a)`; attributes
            `(.-attr obj)`, `(py.- obj attr)`; chains `(py.. obj -attr (m a) n)`.
   Values   strings, numbers, nil/true/false, keywords (as strings), vectors
            (lists), maps (dicts), sets, `(tuple a b)`.
   Exprs    arithmetic and comparison operators, `and or not`, `get nth first
            second last slice count contains? inc dec str`, `(if c a b)`,
            `(fn [x] expr)` (a lambda), `(for [x xs :when c] expr)`.
   Stmts    `def set! do let if when when-not doseq dotimes while defn return
            import try raw`.
   Imports  `(import json)`, `(import [numpy :as np])`,
            `(import gi.repository [Gio Gegl])`.
   Escape   `(raw \"any python\")` emits its string unchanged.

   State persists between calls (the plugin execs into one context), exactly
   as with `hive-gimp.exec`: a `def` here is visible to the next call."
  (:refer-clojure :exclude [eval])
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-gimp.exec :as exec]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

;; =============================================================================
;; Literals and names
;; =============================================================================

(defrecord Lit [value])

(defn- refuse [message form]
  (throw (ex-info message {:hive-gimp/reason :py/unsupported-form :form form})))

(declare literal)

(defn- string-literal [s]
  ;; JSON string syntax is valid Python string syntax once "/" is left bare
  ;; (Python keeps the backslash of an unknown escape like "\/").
  (json/write-str s :escape-slash false))

(defn literal
  "A Clojure value as a Python literal."
  [v]
  (cond
    (instance? Lit v) (literal (:value v))
    (nil? v)          "None"
    (true? v)         "True"
    (false? v)        "False"
    (string? v)       (string-literal v)
    (keyword? v)      (string-literal (subs (str v) 1))
    (integer? v)      (str v)
    (ratio? v)        (str (double v))
    (number? v)       (let [d (double v)]
                        (cond (Double/isNaN d)      "float('nan')"
                              (Double/isInfinite d) (if (pos? d) "float('inf')" "float('-inf')")
                              :else                 (str d)))
    (map? v)          (str "{" (str/join ", " (map (fn [[k x]] (str (literal k) ": " (literal x))) v)) "}")
    (set? v)          (if (empty? v) "set()" (str "{" (str/join ", " (map literal v)) "}"))
    (sequential? v)   (str "[" (str/join ", " (map literal v)) "]")
    :else             (refuse (str "No Python literal for a " (type v) ".") v)))

(defn- ident [s form]
  (let [s (str/replace s "-" "_")]
    (if (re-matches #"[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*" s)
      s
      (refuse (str "`" form "` is not a Python name.") form))))

(defn sym->py
  "`Gimp/get-images` -> `Gimp.get_images`, `a.b-c` -> `a.b_c`."
  [sym]
  (ident (if-let [ns (namespace sym)] (str ns "." (name sym)) (name sym)) sym))

;; =============================================================================
;; Expressions
;; =============================================================================

(declare emit-expr)

(defn- args->py
  "Positional arguments, then `:k v` pairs as keyword arguments."
  [args]
  (loop [[a & more :as args] args out []]
    (cond
      (empty? args) (str/join ", " out)
      (keyword? a)  (if (empty? more)
                      (refuse (str "Keyword argument " a " has no value.") a)
                      (recur (rest more) (conj out (str (ident (name a) a) "=" (emit-expr (first more))))))
      :else         (recur more (conj out (emit-expr a))))))

(defn- target->py
  "A binding target: a symbol, or a vector destructured as a tuple."
  [t]
  (cond (symbol? t) (sym->py t)
        (vector? t) (str "(" (str/join ", " (map target->py t)) (when (= 1 (count t)) ",") ")")
        :else       (refuse "A binding target is a symbol or a vector of them." t)))

(defn- params->py [params]
  (let [[fixed [_ rest-param]] (split-with #(not= '& %) params)]
    (str/join ", " (cond-> (mapv sym->py fixed)
                     rest-param (conj (str "*" (sym->py rest-param)))))))

(defn- call [f args] (str f "(" (args->py args) ")"))

(defn- subscript [x k] (str (emit-expr x) "[" (emit-expr k) "]"))

(def ^:private infix
  {'+ "+" '- "-" '* "*" '/ "/" 'mod "%" 'quot "//" '** "**"
   '< "<" '> ">" '<= "<=" '>= ">=" '= "==" 'not= "!=" 'is "is"
   'and "and" 'or "or" 'bit-and "&" 'bit-or "|"})

(defn- chain
  "`(py.. obj -attr (m a) n)`: `-x` reads an attribute, a list or a bare
   symbol calls a method."
  [obj steps]
  (reduce (fn [acc step]
            (cond
              (and (symbol? step) (str/starts-with? (name step) "-"))
              (str acc "." (sym->py (symbol (subs (name step) 1))))
              (symbol? step) (str acc "." (call (sym->py step) []))
              (seq? step)    (str acc "." (call (sym->py (first step)) (rest step)))
              :else          (refuse "A py.. step is -attr, method or (method args)." step)))
          (emit-expr obj) steps))

(defn- comprehension [[bindings body]]
  (let [clauses (loop [[b x & more] bindings out []]
                  (cond (nil? b)    out
                        (= :when b) (recur more (conj out (str "if " (emit-expr x))))
                        :else       (recur more (conj out (str "for " (target->py b) " in " (emit-expr x))))))]
    (str "[" (emit-expr body) " " (str/join " " clauses) "]")))

(defn- emit-seq [form]
  (let [[h & args] form
        hn (when (symbol? h) (name h))]
    (cond
      (not (symbol? h)) (call (str "(" (emit-expr h) ")") args)

      (contains? infix h)
      (cond (and (= '- h) (= 1 (count args))) (str "(-" (emit-expr (first args)) ")")
            (< (count args) 2) (refuse (str h " needs two or more operands.") form)
            :else (str "(" (str/join (str " " (infix h) " ") (map emit-expr args)) ")"))

      :else
      (case h
        (. py.)  (str (emit-expr (first args)) "." (call (sym->py (second args)) (nnext args)))
        py.-     (str (emit-expr (first args)) "." (sym->py (second args)))
        (.. py..) (chain (first args) (rest args))
        not      (str "(not " (emit-expr (first args)) ")")
        if       (str "(" (emit-expr (second args)) " if " (emit-expr (first args))
                      " else " (emit-expr (nth args 2 nil)) ")")
        fn       (if (= 2 (count args))
                   (str "(lambda " (params->py (first args)) ": " (emit-expr (second args)) ")")
                   (refuse "A fn compiles to a Python lambda: one parameter vector and one expression." form))
        for      (comprehension args)
        tuple    (str "(" (str/join ", " (map emit-expr args)) (when (= 1 (count args)) ",") ")")
        (get nth) (if (= 3 (count args))
                    (str (emit-expr (first args)) ".get(" (emit-expr (second args)) ", " (emit-expr (nth args 2)) ")")
                    (subscript (first args) (second args)))
        get-in   (reduce (fn [acc k] (str acc "[" (emit-expr k) "]")) (emit-expr (first args)) (second args))
        first    (str (emit-expr (first args)) "[0]")
        second   (str (emit-expr (first args)) "[1]")
        last     (str (emit-expr (first args)) "[-1]")
        slice    (let [[x a b] args]
                   (str (emit-expr x) "[" (when (some? a) (emit-expr a)) ":" (when (some? b) (emit-expr b)) "]"))
        count    (str "len(" (emit-expr (first args)) ")")
        contains? (str "(" (emit-expr (second args)) " in " (emit-expr (first args)) ")")
        inc      (str "(" (emit-expr (first args)) " + 1)")
        dec      (str "(" (emit-expr (first args)) " - 1)")
        str      (str "''.join(map(str, [" (str/join ", " (map emit-expr args)) "]))")
        raw      (first args)
        (cond
          (str/starts-with? hn ".-") (str (emit-expr (first args)) "." (sym->py (symbol (subs hn 2))))
          (and (str/starts-with? hn ".") (not= "." hn))
          (str (emit-expr (first args)) "." (call (sym->py (symbol (subs hn 1))) (rest args)))
          (and (str/ends-with? hn ".") (not= "." hn))
          (call (sym->py (symbol (namespace h) (subs hn 0 (dec (count hn))))) args)
          :else (call (sym->py h) args))))))

(defn emit-expr
  "One form as a Python expression."
  [form]
  (cond
    (instance? Lit form) (literal (:value form))
    (symbol? form)       (sym->py form)
    (seq? form)          (if (empty? form) "()" (emit-seq form))
    (vector? form)       (str "[" (str/join ", " (map emit-expr form)) "]")
    (map? form)          (str "{" (str/join ", " (map (fn [[k v]] (str (emit-expr k) ": " (emit-expr v))) form)) "}")
    (set? form)          (if (empty? form) "set()" (str "{" (str/join ", " (map emit-expr form)) "}"))
    :else                (literal form)))

;; =============================================================================
;; Statements
;; =============================================================================

(declare emit-stmts emit-tail)

(defn- indent [lines] (mapv #(str "    " %) (if (seq lines) lines ["pass"])))

(defn- head [form] (when (and (seq? form) (symbol? (first form))) (first form)))

(defn- import->py [[a b]]
  (cond
    (and (symbol? a) (vector? b)) [(str "from " (sym->py a) " import " (str/join ", " (map sym->py b)))]
    (symbol? a)                   [(str "import " (sym->py a))]
    (vector? a)                   (let [[m _ alias] a] [(str "import " (sym->py m) " as " (sym->py alias))])
    :else                         (refuse "import takes a module, [module :as alias] or module [names]." a)))

(defn- loops [bindings body-lines kw]
  (if-let [[b x & more] (seq bindings)]
    (into [(str "for " (target->py b) " in " (if (= kw :dotimes) (str "range(" (emit-expr x) ")") (emit-expr x)) ":")]
          (indent (loops more body-lines kw)))
    body-lines))

(defn- try->py [body emit-body]
  (let [clause? #(#{'catch 'finally} (head %))
        main    (remove clause? body)
        catches (filter #(= 'catch (head %)) body)
        fin     (first (filter #(= 'finally (head %)) body))]
    (cond-> (into ["try:"] (indent (emit-body main)))
      true (into (mapcat (fn [[_ cls e & cbody]]
                           (into [(str "except " (sym->py cls) " as " (sym->py e) ":")] (indent (emit-body cbody))))
                         catches))
      fin  (into (into ["finally:"] (indent (emit-stmts (rest fin))))))))

(defn- emit-stmt [form]
  (case (head form)
    def      [(str (target->py (second form)) " = " (emit-expr (nth form 2)))]
    set!     [(str (emit-expr (second form)) " = " (emit-expr (nth form 2)))]
    do       (emit-stmts (rest form))
    let      (into (vec (mapcat (fn [[t x]] [(str (target->py t) " = " (emit-expr x))]) (partition 2 (second form))))
                   (emit-stmts (nnext form)))
    if       (let [[_ c t e] form]
               (cond-> (into [(str "if " (emit-expr c) ":")] (indent (emit-stmts [t])))
                 (> (count form) 3) (into (into ["else:"] (indent (emit-stmts [e]))))))
    when     (into [(str "if " (emit-expr (second form)) ":")] (indent (emit-stmts (nnext form))))
    when-not (into [(str "if not " (emit-expr (second form)) ":")] (indent (emit-stmts (nnext form))))
    while    (into [(str "while " (emit-expr (second form)) ":")] (indent (emit-stmts (nnext form))))
    doseq    (loops (second form) (emit-stmts (nnext form)) :doseq)
    dotimes  (loops (second form) (emit-stmts (nnext form)) :dotimes)
    defn     (let [[_ n params & body] form]
               (into [(str "def " (sym->py n) "(" (params->py params) "):")] (indent (emit-tail body))))
    return   [(if (next form) (str "return " (emit-expr (second form))) "return")]
    import   (import->py (rest form))
    try      (try->py (rest form) emit-stmts)
    raw      (str/split-lines (second form))
    [(emit-expr form)]))

(defn emit-stmts
  "Forms as Python statement lines."
  [forms]
  (vec (mapcat emit-stmt forms)))

(def ^:private statement-heads
  '#{def set! doseq dotimes while defn return import raw})

(defn- emit-tail
  "A function body whose last expression is returned."
  [body]
  (let [lead (butlast body) lst (last body)]
    (into (emit-stmts lead)
          (case (head lst)
            nil      (if (some? lst) [(str "return " (emit-expr lst))] [])
            do       (emit-tail (rest lst))
            let      (into (vec (mapcat (fn [[t x]] [(str (target->py t) " = " (emit-expr x))]) (partition 2 (second lst))))
                           (emit-tail (nnext lst)))
            if       (let [[_ c t e] lst]
                       (-> [(str "if " (emit-expr c) ":")]
                           (into (indent (emit-tail [t])))
                           (into ["else:"])
                           (into (indent (emit-tail [e])))))
            try      (try->py (rest lst) emit-tail)
            (if (statement-heads (head lst))
              (emit-stmt lst)
              [(str "return " (emit-expr lst))])))))

(defn ->python
  "Forms as one Python source block."
  [forms]
  (str/join "\n" (emit-stmts forms)))

;; =============================================================================
;; Templates: forms as data, with ~x splicing Clojure values
;; =============================================================================

(defn- unquote? [f] (and (seq? f) (= 'clojure.core/unquote (first f))))
(defn- splice? [f] (and (seq? f) (= 'clojure.core/unquote-splicing (first f))))

(defn- template [form]
  (letfn [(items [fs] `(concat ~@(map (fn [f] (if (splice? f) `(map ->Lit ~(second f)) [(template f)])) fs)))]
    (cond
      (unquote? form) `(->Lit ~(second form))
      (seq? form)     `(apply list ~(items form))
      (vector? form)  `(vec ~(items form))
      (map? form)     `(apply array-map ~(items (mapcat identity form)))
      (set? form)     `(set ~(items form))
      (symbol? form)  `'~form
      :else           form)))

(defmacro forms
  "`body` as data the compiler reads. `~x` is the VALUE of Clojure `x`, sent as
   a Python literal; `~@xs` splices each value of `xs`."
  [& body]
  `(vector ~@(map template body)))

;; =============================================================================
;; Running inside GIMP
;; =============================================================================

(def ^:private sentinel "\u001ehive-gimp.py ")

(def ^:private json-default
  ;; GIMP objects come back as {id, type, name}; anything else as its repr.
  ["import json as __hg_json"
   "def __hg_default(o):"
   "    if hasattr(o, 'get_id'):"
   "        return {'id': o.get_id(), 'type': type(o).__name__, 'name': o.get_name() if hasattr(o, 'get_name') else None}"
   "    return repr(o)"])

(defn eval-source
  "The Python an `eval` of `forms` sends: every form but the last as a
   statement, then the last one's value printed as JSON after a sentinel."
  [forms]
  (let [lead (butlast forms) lst (last forms)]
    (str/join "\n" (concat json-default
                           (emit-stmts lead)
                           [(str "__hg_r = " (if (some? lst) (emit-expr lst) "None"))
                            (str "print(" (string-literal sentinel) " + __hg_json.dumps(__hg_r, default=__hg_default))")]))))

(defn- transport-of [target] (or (:transport target) target))

(defn- with-source [outcome source]
  (cond-> outcome (not= :ok (:outcome outcome)) (assoc :python source)))

(defn exec-forms
  "Run `forms` in GIMP. An `Outcome` whose `:value` is the captured stdout."
  [target forms]
  (let [source (->python forms)
        out    (exec/run (transport-of target) [source])]
    (with-source (cond-> out (= :ok (:outcome out)) (update :value #(apply str %))) source)))

(defn- parse-result [stdout]
  (let [lines  (str/split-lines (or stdout ""))
        result (last (filter #(str/starts-with? % sentinel) lines))]
    {:data   (when result (json/read-str (subs result (count sentinel)) :key-fn keyword))
     :stdout (str/join "\n" (remove #(str/starts-with? % sentinel) lines))}))

(defn eval-forms
  "Run `forms` in GIMP. An `Outcome` whose `:value` is the last form's value as
   Clojure data, and whose `:stdout` is anything the forms printed."
  [target forms]
  (let [source (eval-source forms)
        out    (exec/run (transport-of target) [source])]
    (with-source
      (if (= :ok (:outcome out))
        (let [{:keys [data stdout]} (parse-result (apply str (:value out)))]
          (assoc out :value data :stdout stdout))
        out)
      source)))

(defn- value! [outcome]
  (if (= :ok (:outcome outcome))
    (:value outcome)
    (throw (ex-info (str "GIMP refused the forms: " (:message outcome)) outcome))))

(defmacro exec
  "Run Clojure forms as Python in GIMP. Returns the `Outcome`."
  [target & body]
  `(exec-forms ~target (forms ~@body)))

(defmacro eval
  "Run Clojure forms as Python in GIMP. Returns the `Outcome`, `:value` being
   the last form's value as data."
  [target & body]
  `(eval-forms ~target (forms ~@body)))

(defmacro exec!
  "As `exec`, returning the stdout and throwing on failure."
  [target & body]
  `(#'value! (exec ~target ~@body)))

(defmacro eval!
  "As `eval`, returning the value and throwing on failure."
  [target & body]
  `(#'value! (eval ~target ~@body)))
