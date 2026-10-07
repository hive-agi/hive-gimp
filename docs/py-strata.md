# hive-gimp.py strata: Clojure forms to GIMP, and GIMP to Clojure

The contract every lane of the `feat/py-strata` swarm builds against. If your
code and this file disagree, your code is wrong; if this file is wrong, shout
`blocked` with the correction, do not improvise.

Design canon (refresh it from memory before designing): Stratified Design,
CPPB (Collect -> Promote -> Pipeline -> Boundary, effects only at the
Boundary), DDD (the domain values are named and schema'd), SOLID (small
ports, substitutable fakes, OCP by REGISTRATION: adding a form, a node, a verb
or a PDB procedure is a `defmethod` or a map entry, never an edit to a `case`,
`cond` or set literal). Every namespace stays under 400 lines and speaks only
the language of the stratum below it. No `with-redefs` in tests: use the
records in `test/hive_gimp/stub.clj`.

## Strata (bottom to top)

```
value      hive-gimp.py.value     Lit record: a Clojure value spliced with ~x        (pure)
names      hive-gimp.py.names     symbol -> Python dotted name, refusal on bad names  (pure)
ast        hive-gimp.py.ast       the Python AST as data + malli schemas               (pure, DOMAIN)
render     hive-gimp.py.render    AST -> Python source text                            (pure)
lower      hive-gimp.py.lower     Clojure forms -> AST, form registry (multimethods)  (pure)
template   hive-gimp.py.template  the `forms` macro: forms as data, ~x / ~@xs          (pure, macro)
result     hive-gimp.py.result    eval protocol: wrap AST to print JSON, parse stdout  (pure)
facade     hive-gimp.py           ->python, forms, exec/eval(!), *-forms, eval-source (BOUNDARY via hive-gimp.exec)
pdb        hive-gimp.pdb(.*)      PDB introspection, run!, defprocedure macro          (pure core + boundary)
verbs      hive-gimp.verbs.*      base GIMP verbs as form builders (image, layer, ...) (pure builders)
```

CPPB of one call: `template` COLLECTS forms; `lower` PROMOTES them to AST
(validated by `ast` schemas); `render` and `result` are the PIPELINE; the
facade's `exec-forms`/`eval-forms` are the only BOUNDARY (`hive-gimp.exec/run`
over the `IGimpTransport` port).

## Frozen public API of `hive-gimp.py`

These names, arities and behaviours stay exactly as on staging `bd24906`;
`test/hive_gimp/py_test.clj` passes UNCHANGED at the end:

`->python [forms]`, macro `forms [& body]`, `exec-forms [target forms]`,
`eval-forms [target forms]`, `eval-source [forms]`, macros `exec`, `eval`,
`exec!`, `eval!` (`[target & body]`). `target` is a session `{:transport t}`
or a transport.

## The AST (the domain value)

Every node is a map with `:py/node`. Children are nodes unless stated. Exact
keys; `ast` owns the malli schemas `Expr`, `Stmt`, `Params`, `Module` (a
vector of Stmt) and `valid?`/`explain`.

Expressions:

```clojure
{:py/node :name      :id "Gimp.get_images"}            ; dotted path, already validated
{:py/node :literal   :value v}                          ; v = any Clojure value render can spell
{:py/node :attr      :object E :attr "get_name"}
{:py/node :call      :fn E :args [E] :kwargs [["k" E]]} ; kwargs ordered pairs
{:py/node :subscript :object E :index E}
{:py/node :slice     :object E :lower E-or-nil :upper E-or-nil}
{:py/node :binop     :op "+" :operands [E E ...]}       ; n-ary, op is the Python spelling
{:py/node :unop      :op "-" :operand E}                ; "-" or "not"
{:py/node :cond-expr :test E :then E :else E}
{:py/node :lambda    :params P :body E}
{:py/node :comp      :element E :clauses [{:for T :in E} {:if E}]}
{:py/node :list      :items [E]}
{:py/node :tuple     :items [E]}                        ; also the destructuring target
{:py/node :set       :items [E]}
{:py/node :dict      :entries [[E E]]}
{:py/node :raw       :code "any python expression"}
P = {:fixed ["a" "b"] :rest "r-or-nil"}
T = a :name or a :tuple of targets
```

Statements:

```clojure
{:py/node :assign      :target E :value E}
{:py/node :expr-stmt   :expr E}
{:py/node :if          :test E :body [S] :orelse [S]-or-nil}
{:py/node :for         :target T :iter E :body [S]}
{:py/node :while       :test E :body [S]}
{:py/node :def         :name "half" :params P :body [S]}
{:py/node :return      :value E-or-nil}
{:py/node :import      :module "numpy" :as "np-or-nil"}
{:py/node :from-import :module "gi.repository" :names ["Gio" "Gegl"]}
{:py/node :try         :body [S] :handlers [{:type E :name "e" :body [S]}] :finally [S]-or-nil}
{:py/node :raw-stmt    :code "x = 1\ny = 2"}
```

Rendering rules (byte-exact, these are what `py_test.clj` asserts):

- Indent 4 spaces; an empty block renders `pass`.
- `:call` -> `f(a, b, k=v)`; `:attr` -> `o.attr`; `:subscript` -> `o[i]`;
  `:slice` -> `o[a:b]` with an absent bound left empty.
- `:binop` -> `(a OP b OP c)`; `:unop "-"` -> `(-a)`; `:unop "not"` -> `(not a)`.
- `:cond-expr` -> `(then if test else else)`; `:lambda` -> `(lambda a, *r: body)`.
- `:comp` -> `[elem for t in xs if c]`; `:list` `[a, b]`; `:tuple` `(a, b)`,
  one item `(a,)`; `:set` `{a}` and empty `set()`; `:dict` `{k: v}`.
- Literals: nil `None`, booleans `True`/`False`, strings as JSON string syntax
  with `/` NOT escaped, keywords as the string of their name (`:a/b` is
  `"a/b"`), integers as is, ratios as doubles, NaN `float('nan')`, infinities
  `float('inf')`/`float('-inf')`, maps/sets/sequentials recursively.
- `:if` renders `else:` only when `:orelse` is non-nil; `:try` renders
  `except T as e:` per handler and `finally:` when present; `:raw-stmt` splits
  on newlines.

## Lowering (forms -> AST), the registration points

`lower` exposes `lower-expr`, `lower-stmts`, `lower-tail` and three
multimethods, each dispatching on the head symbol of a list form (default:
plain call / expression statement / return):

- `expr-form` : `if fn for tuple get nth get-in first second last slice count
  contains? inc dec str raw not . py. py.- .. py..` plus the infix table
  (`+ - * / mod quot ** < > <= >= = not= is and or bit-and bit-or`) as DATA
  registered through one helper.
- `stmt-form` : `def set! do let if when when-not while doseq dotimes defn
  return import try raw`.
- `tail-form` : `do let if try` (a `defn` returns its last expression).

Sugar resolved before dispatch: `(.m o a)` method, `(.-a o)` attribute,
`(Cls. a)` constructor, `Ns/name` and `a-b` names (via `names`). A keyword in
an argument list opens a keyword argument. `value/Lit` lowers to `:literal`.
Refusals are `ex-info` with `{:hive-gimp/reason :py/unsupported-form :form f}`
and a message naming the fix.

## PDB bridge and native plug-ins (lane D)

`hive-gimp.pdb`: `procedures [target & {:keys [match]}]` (names),
`describe [target name]` -> `{:name :blurb :help :args [{:name :type :default
:blurb}] :values [...]}`, `run! [target name args-map]` (lookup_procedure,
create_config, set_property per arg, run, values back as data), all through
`hive-gimp.py` forms, never Python strings. Macro `defprocedure` (and a
`defprocedures` over a spec EDN) makes a PDB procedure NATIVE: a Clojure fn
with real arglists, a catalogue row so the MCP `gimp` tool lists and runs it,
and a `client/send-compensated` defmethod that answers it over exec. Specs live
as data in `resources/hive_gimp/pdb/*.edn`, generated by a Clojure dev task
from a live `describe`, so adding a plug-in is a row, not code.

## Base verbs (lane E)

`hive-gimp.verbs.image|layer|selection|paint|path|io|display`: pure builders
returning forms (built with `py/forms`), composed by the caller and run in one
round trip through `py/eval!`/`py/exec!`. Values that come back are DDD value
objects (`{:id :type :name}` refs, sizes, colours) with malli schemas.

## Lifecycle (lane F)

`hive-gimp.lifecycle.*` keeps the MCP server up without a human.
`model` (pure, DOMAIN): `Observation` -> `classify` -> `LinkState`, and
`plan [observation history]` -> `HealPlan` (multimethod `plan-for` per state;
escalation is a function of the step kinds already run). `port`:
`ILinkProbe`, `ILifecycle`, `IClock`. `heal` (PIPELINE): `status`, `heal!`,
`run-step` multimethod per step kind. `host` (BOUNDARY): TCP + `check_server`
probe, `restart_server`, `ss`+kill, GIMP's D-Bus `org.gimp.GIMP.UI.BatchRun`
for a running GUI, and an owned headless `flatpak run --die-with-parent`.
`tool`: the MCP `gimp_lifecycle` tool. Facade: `hive-gimp.core/status`,
`hive-gimp.core/heal!`. Schemas register under `:hive-gimp.lifecycle/*`.
