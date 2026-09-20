# A portable core for hive-gimp (cljw / cljrs)

Written 2026-09-20, from measurements of the three media addons as they stand.

**Status: partly applied, same day.** `shape.cljc`, `ports.cljc` and
`doctor/verdict.cljc` exist, `schema.clj` is the thin JVM face over `shape`,
and two suites guard the result (`shape_equivalence_test`,
`portability_test`). `command` and `response` are still `.clj`; that is the
open card. The table below describes the state BEFORE the split, and the plan
at the end now reads as steps 1 to 3 done, 4 to 5 remaining.

## The house pattern, measured not assumed

The two siblings that already run on more than the JVM agree on a strict shape:

| repo | portable core | requires | reader conditionals |
|---|---|---|---|
| hive-creator | `srt ass snap taste ffmpeg still finish plan export copy format campaign vocabulary` (`.cljc`) | `clojure.core`, `clojure.string`, own nses | **0** |
| hive-kdenlive | `mlt/*` + `kdenlive/routes` (`.cljc`) | `clojure.string`, own nses | **0** |
| hive-gimp | — none, every namespace is `.clj` | — | — |

Two things stand out.

**Zero reader conditionals.** A portable core in this fleet is not
"Clojure with `#?(:clj ...)` escapes". It is plain Clojure that happens to run
on three hosts, and every host-specific thing has been pushed out to a
boundary namespace. That is a checkable invariant, not a style preference.

**Malli never reaches the core.** hive-creator's `deps.edn` says it outright —
malli and hive-addon "serve the JVM boundary (schema, run, addon); the
portable namespaces never require them" — and the measurement agrees: no
`.cljc` in hive-creator requires either.

hive-gimp inverts that second rule, and that is the whole of what blocks it.

## Where hive-gimp actually stands

Classifying every namespace by what it requires:

**Already portable in substance** (only `clojure.string`, `hive-dsl.result` —
itself `.cljc` — or own nses):

- `ports` — no requires at all; protocols only
- `doctor/verdict` — `clojure.string` only
- `contract`, `command`, `response` — `clojure.string` + `hive-dsl.result`
  + **`hive-gimp.schema`**, which is the only thing keeping them on the JVM

**Correctly JVM** (boundary; should stay `.clj`):

- `transport/socket` — `java.net`, `java.io`, `java.nio.charset`
- `transport/python` — libpython-clj, timbre
- `config` — hive-di
- `catalog` — `clojure.java.io` + `clojure.edn`; reads a classpath resource,
  which is a host capability by definition
- `addon`, `tools` — hive-addon, the MCP surface

**The two real blockers:**

1. `schema` requires `malli.core` / `malli.error`, and `contract`, `command`
   and `response` require `schema`.
2. `codec` requires `clojure.data.json`.

## Blocker 1: malli, and why it is smaller than it looks

Malli reaches the would-be portable core at exactly **three call sites**, and
all three are the same shape — a validity check plus an explanation:

| namespace | site | what it does |
|---|---|---|
| `command` | `validate` | `(not (schema/descriptor? descriptor))` -> `:gimp/unknown-command` with `(schema/explain schema/Descriptor descriptor)` |
| `contract` | `invalid` | `(remove schema/descriptor?)`, then `(schema/explain schema/Descriptor d)` per failure |
| `response` | `outcome` | `(not (schema/raw-response? raw))` -> `:gimp/malformed-response` with `(schema/explain schema/RawResponse raw)` |

None is business logic. Each is a shape gate.

The key fact: **a malli schema is data.** `Descriptor`, `RawResponse` and the
rest of `hive-gimp.schema` are plain vectors and maps. Only `m/validator` and
`m/explain` — the compilation of that data into functions — need malli.

So the split is clean:

- `hive_gimp/shape.cljc` — the schema *definitions*, moved verbatim. Portable
  today, because they are data.
- `hive_gimp/schema.clj` — stays JVM, and becomes thin:
  `(def descriptor? (m/validator shape/Descriptor))`, `explain`, and the rest.

That leaves the three call sites. Two options, and they are not equal:

**Preferred — split the gate from the explanation.** `descriptor?` and
`raw-response?` are structural checks that can be written portably in a few
lines (a descriptor has a string `:command` and a vector `:params`; a raw
response has a `"status"`). Write those portably in `shape.cljc` and keep
malli for the rich `explain`, which the boundary supplies. `contract/invalid`
is a build-time audit of a generated contract file — it runs on the JVM, at
the boundary, and should simply stay there rather than be made portable.

**Alternative — inject the validator.** Pass `{:valid? f :explain f}` into
`command/validate` and `response/outcome` (DIP, the same way
`hive-creator.run` takes a runner map). Purer, but it changes three
signatures and every caller, for a gain the first option already gets.

Recommend the first.

## Blocker 2: JSON

`codec` is the wire codec for the plug-in socket, so it is arguably boundary
work that belongs with `transport/socket` and can stay JVM. The question is
whether a cljw/cljrs host ever needs to *speak* the GIMP protocol, or only to
*build* command scripts.

For the stated purpose — hive-creator planning GIMP scripts natively in a
cljw/cljrs pipeline — only the second is needed. `hive-creator.still/script`
produces `[[command params] ...]` as data and never encodes JSON; encoding
happens when a JVM boundary sends it. So **JSON is not on the critical path**
and `codec` should stay `.clj` until something actually needs it portable.

## The move, in order

1. `hive_gimp/shape.cljc` — schema definitions moved out of `schema.clj`
   verbatim, plus portable `descriptor?` / `raw-response?` predicates.
2. `schema.clj` keeps the malli validators and `explain`, now built over
   `shape`. Public surface unchanged, so nothing downstream moves.
3. `contract.clj` stays JVM (it audits a generated file; that is boundary work).
4. `command` -> `command.cljc`, `response` -> `response.cljc`,
   `doctor/verdict` -> `verdict.cljc`, `ports` -> `ports.cljc`.
5. Add the invariant as a test, the way the fleet tests everything else:
   **no `.cljc` in this repo may require malli, hive-addon, hive-di,
   `clojure.java.*`, or contain a reader conditional.** That is what actually
   keeps a portable core portable; without it the next change quietly
   re-couples it, which is how hive-gimp got here.

Step 5 is the one that matters most, and it is worth adding to hive-creator
and hive-kdenlive too — both satisfy it today by discipline alone, and
neither has a test that would notice if they stopped.

## What this buys

hive-gimp's command vocabulary and response handling become usable from a
cljw/cljrs pipeline, which is what makes the open cards
`20260919140248-16e5e273` (creator's native leg via cljrs media) and
`20260919140248-62aa7550` (native plug-in verbs) reachable without dragging a
JVM behind them. It also gives all three media addons the same stratification,
which is the point: today two of them share a shape and the third does not.
