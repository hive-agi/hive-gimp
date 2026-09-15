# hive-gimp

An `IAddon` exposing **GIMP 3.x** as MCP tools, plus an optional host-side Python
port through libpython-clj.

```clojure
(require '[hive-gimp.core :as gimp])

(def g (gimp/connect))
(gimp/doctor g)
(gimp/invoke g "new_canvas" {:width 800 :height 600})
(gimp/exec g ["Gimp.displays_flush()"])
```

---

## The one thing to know before reading further

**GIMP is reached over a TCP socket.** What differs between the two transports is
*who opens the socket*, and both are supported:

| `HIVE_GIMP_TRANSPORT` | who opens the socket | needs libpython-clj |
|---|---|---|
| `socket` (default) | the JVM | no |
| `python` | the embedded CPython, via libpython-clj | yes |

Both satisfy `IGimpTransport`; every command behaves identically through either.

The distinction that matters, and the one that trips people up:

- libpython-clj **can** drive GIMP, by speaking the plugin's socket protocol from
  the embedded interpreter. That is what `hive-gimp.transport.python` does, and
  it is exactly the path the reference project's own `bg_remove.py` takes,
  in-process instead of as a subprocess. Verified end to end against a real
  interpreter (`clojure -M:python -i dev/verify_python_transport.clj`, 7/7) and
  against a real GIMP 3.2.4 (`HIVE_GIMP_TRANSPORT=python`, 13/13).

  Python is used there as a **language, not as a string**. Modules are
  imported, attributes read and objects called through libpython-clj interop;
  no Python source text is `exec`d and nothing is compiled at runtime. That is
  what lets `connect_ex` classify failures by **errno** rather than by parsing
  an exception message, and it keeps the read buffer on the Python side (a
  `->jvm` of Python `bytes` is a vector of boxed integers, so marshalling per
  chunk would turn a base64 image into millions of `Long`s).
- libpython-clj **cannot** `import gi.repository.Gimp` and get a working GIMP.
  The import *succeeds*, which is the trap: GIMP 3 plugins run in GIMP's own
  embedded CPython, attached to its main loop and PDB, so the module is live and
  inert. `Gimp.get_images()` answers for a GIMP this process is not part of.
  There is no in-process route to a running GIMP from anything GIMP did not
  itself launch.

Choose `python` when you also want host-side pixel work in the same interpreter,
so a GIMP export can be handed straight to `rembg` without leaving the process.
Choose `socket` (the default) when you want no Python at all.

The other half of the Python story is `hive-gimp.pixel`: the image ecosystem GIMP
does not ship. `rembg` does in one call what the reference project's
`bg_remove_iterative.py` spends sixteen kilobytes doing through iterative
fuzzy-select. Exposed as the `gimp_pixel` tool; `remove_background_in_gimp`
exports, removes, and re-opens in one step.

## Layout

```
schema          value objects, malli first. The WIRE / HIVE vocabulary split.
ports           IGimpTransport, IHostPython. Role-sized (ISP), injected (DIP).
catalog         COLLECT   reads the contract off the classpath, nothing else
contract        PROMOTE   the descriptor algebra, pure
command         PROMOTE   descriptor + args -> GimpCommand, pure
response        PROMOTE   raw plugin answer -> Outcome, pure
codec           PROMOTE   framing and JSON, pure
doctor.verdict  PROMOTE   the doctor's judgements, pure
client          PIPELINE  lookup, build, encode, send, decode, interpret
doctor          PIPELINE  staged preflight
pixel           PIPELINE  host-side pixel work, composed with GIMP
transport/*     BOUNDARY  the only namespaces that touch a socket or Python
tools           FACADE    the five MCP tools
core            FACADE    the public Clojure surface
addon           wiring    IAddon
```

Five of the pipeline's six steps are pure; the impure one is a call through a
port. That is why the whole thing is tested end to end against a scripted double
and why **no test in this repo redefines a var**.

## The tool surface is data

The GIMP command contract lives in `resources/hive_gimp/commands.edn`, derived
from the reference project by `dev/extract_gimp_contract.py` (78 commands, 286
parameters, read with Python's `ast`, never regex). Hand-written additions go in
`commands_extra.edn`, which a regeneration will not erase.

Adding a GIMP command is a row of EDN. No new `defn`.

That is also why this addon publishes **five** MCP tools rather than eighty:

| tool | |
|---|---|
| `gimp` | run any catalogued command |
| `gimp_exec` | arbitrary Python-Fu, the escape hatch |
| `gimp_catalog` | list, search, describe (the discovery path) |
| `gimp_pixel` | host-side pixel work (rembg, PIL) and its composition with GIMP |
| `gimp_doctor` | staged diagnosis |

Eighty tool definitions with descriptions and schemas is a permanent context tax
on every client that mounts the addon, whether or not it ever touches GIMP.

## Things found in the reference project

Two passes found these. The generator cross-checks the commands the Python
tools *send* against the commands the plugin *handles*, which is a source-only
reading. `dev/verify_live_gimp.clj` then drives a real GIMP 3.2.4, which is
where the behavioural ones showed up: reading the source told us `new_canvas`
opens a display, and only running it showed what happens when it cannot.

- **`call_api` is not dispatched by the plugin.** It falls through to the
  `else` branch and is executed as raw Python-Fu. The branch reads
  `j["params"]` unguarded, so a request without `params` raises `KeyError`
  inside GIMP.
- **`pyGObject-eval` does not exist.** `GIMP_MCP_PROTOCOL.md` advertises it for
  evaluating expressions, but the plugin compares against the literal
  `python-fu-eval` and everything else execs. A caller following that document
  gets exec semantics and a list of `"None"`. `hive-gimp.exec` sends the marker
  that works.

  Confirmed live against GIMP 3.2.4, and the two modes are genuinely different,
  so pick deliberately:

  ```
  mode=exec (default)   ["1 + 1"]      -> [""]        value discarded
                        ["print(6*7)"] -> ["42\n"]    stdout captured
  mode=eval             ["1 + 1"]      -> ["2"]       value returned
                        ["print(6*7)"] -> ["None"]    print's own return value
  ```

  The default is `exec`, so a bare expression sent without `mode` comes back as
  an empty string and no error. Use `mode=eval` for a value, or `print` it.

- **A headless `new_canvas` reports failure and leaks the image it created.**
  `_new_canvas` builds the image, inserts its layer and fills it, and only then
  calls `Gimp.Display.new(image)`, which returns NULL when GIMP runs with `-i`.
  A blanket `except` turns that into `{"status": "error"}`. The caller is told
  the command failed; a fully formed image is left inside GIMP with its id
  never returned. Measured, not inferred, because the count moves: two
  "failed" calls took `len(Gimp.get_images())` from 1 to 3, at exactly the
  requested dimensions. The same handler also hardcodes
  `"display_opened": True` in its success payload.
- **`close_image` is dead code.** It calls `Gimp.get_displays()`, which the GIMP
  3.2 PyGObject API does not have, so every call fails on every image and
  nothing can close an image through this plugin. What works is reaching each
  display by id and deleting it, which takes the image with it; that is the
  sweep `dev/verify_live_gimp.clj` uses to clean up, and it is also the fix.

  Related trap while working around it: `Gimp.Image.delete()` is valid only for
  an image with **no display attached**. Called on a displayed one it kills the
  plug-in process, and this side then sees `:gimp/timeout` on the *next*
  command, one step away from the cause.

- **`get_image_bitmap` requires `region`, which the contract calls optional.**
  `region.get("origin_x")` is read unguarded, so omitting it raises
  `AttributeError` on `None` inside GIMP. Same shape as the `call_api`
  `KeyError`. Pass `{:origin_x 0 :origin_y 0 :width w :height h}`.

- **`add_text` substitutes a font and reports success.** `_resolve_font` walks
  aliases down to `Sans-serif` and then the first installed font. Measured
  against GIMP 3.2.4: `add_text` with `"Montserrat ExtraBold"` (not installed)
  answered success and the layer's font read back `Sans-serif`. The native
  plug-in's `place_text` and `add_text` refuse the name instead.

- **`check_server` and `restart_server` are handled by the plugin but were never
  exposed.** The Python server spends both names on host-side connection
  management. They are in `commands_extra.edn`.
- **Response framing has no terminator.** Both reference clients accumulate
  until the buffer parses as *any* JSON value, so a response truncated where the
  prefix happens to be valid JSON is treated as whole and the rest is read as the
  head of the next message. `codec/complete-frame?` requires a JSON **object**,
  which costs nothing and removes the failure mode.

## Requirements

**GIMP 3.x.** This contract is derived from the GIMP 3 API (`Gimp.get_images()`,
`Gegl.Color`, PyGObject). GIMP 2.10 exposes a different Python API entirely
(`pdb.gimp_*`, `gimpfu`) and these commands will not work against it.
`gimp_doctor` detects and reports this rather than letting it surface as a
traceback from inside GIMP.

```bash
flatpak install flathub org.gimp.GIMP
```

Then install the plugin from the reference project into GIMP's own user
directory, and start it with **Tools > MCP > Start MCP Server**.

**Ask GIMP where that directory is; do not guess it.** On a flatpak install the
manifest grants `xdg-config/GIMP:create`, so GIMP writes to the **host**
`~/.config/GIMP/<ver>` and *not* to the sandbox's private
`~/.var/app/org.gimp.GIMP/config/GIMP/<ver>`, which is the path the rest of the
sandbox makes look right. A plugin in the wrong directory is silent: it simply
never registers, and every command then answers `:gimp/unknown-command` far
from the real cause.

```bash
flatpak run org.gimp.GIMP -n -i -d -f --batch-interpreter python-fu-eval \
  -b 'from gi.repository import Gimp; print("USER-DIR:", Gimp.directory())'

DEST=~/.config/GIMP/3.2/plug-ins/gimp-mcp-plugin     # whatever it answered
mkdir -p "$DEST" && cp gimp-mcp-plugin.py "$DEST/" && chmod +x "$DEST"/*.py
```

The directory and the file must share a name (`gimp-mcp-plugin/gimp-mcp-plugin.py`)
and the file must be executable, or GIMP 3 skips it without a word.

## Configuration

Every field has a working default; a stock install needs none.

| env | default | |
|---|---|---|
| `HIVE_GIMP_HOST` | `127.0.0.1` | plugin socket host |
| `HIVE_GIMP_PORT` | `9877` | plugin socket port |
| `HIVE_GIMP_TIMEOUT_MS` | `30000` | read timeout for one command |
| `HIVE_GIMP_CONNECT_TIMEOUT_MS` | `5000` | connect timeout |
| `HIVE_GIMP_TRANSPORT` | `socket` | `socket` or `python` |
| `HIVE_GIMP_PYTHON` | autodetect | interpreter for the Python transport and pixel port |

An unrecognised `HIVE_GIMP_TRANSPORT` resolves to `socket` rather than throwing:
this is read at mount time in a host process, and refusing to start over a typo
in an optional variable trades a working default for an outage. The adapter
actually chosen is reported by health and by `gimp_doctor`, so a typo is visible
rather than silent.

## Development

```bash
clojure -M:test                                     # the suite (no Python needed)
clojure -M:test:nrepl --port 7920                   # interactive
clojure -M:python -i dev/verify_python_transport.clj # live libpython-clj proof
clojure -M -i dev/verify_live_gimp.clj              # live proof against real GIMP
python3 dev/extract_gimp_contract.py resources/hive_gimp/commands.edn
```

`verify_live_gimp.clj` is the one place a double is not allowed: it drives a
real GIMP over the real plugin socket. Bring one up first. No GUI click is
needed either way, because the plugin's `run()` blocks in a GLib main loop and
so keeps its process alive by itself:

```bash
# headless: fine for CI, and the display suite reports itself as SKIPPED
flatpak run org.gimp.GIMP -n -i -d -f --batch-interpreter python-fu-eval \
  -b "exec(open('dev/start_mcp.py').read())"

# headed: same command without -i. A window opens; the batch still runs.
DISPLAY=:1 flatpak run org.gimp.GIMP -n --batch-interpreter python-fu-eval \
  -b "exec(open('dev/start_mcp.py').read())"
```

The run has three sections and the split is deliberate:

| | |
|---|---|
| **CONTRACT** | what hive-gimp promises, over a transport needing no display. Always runs, gates the exit code. |
| **DISPLAY** | the commands that open, export or flush a display. Runs only when GIMP has one, **SKIPS** otherwise, gates when it runs. |
| **FINDINGS** | what the reference plugin does, defects included. Printed every run, never gates. |

```
headed:    13 passed, 0 failed, 0 skipped, 5 findings
headless:   7 passed, 0 failed, 6 skipped, 5 findings
```

A skip is not a pass, and the count is on the summary line so a headless run
cannot be mistaken for coverage of the display path. Findings never gate
because a release of this library must not be blocked by a bug in a plugin it
does not ship, and each one is phrased to report `works (upstream fixed it)`
the day it starts working.

Whether a display exists is decided by **measurement**, not by reading
`DISPLAY` out of the environment: this process is not GIMP, and the question is
whether GIMP can open one.

Both transports are verified against the same live GIMP:

```bash
clojure -M -i dev/verify_live_gimp.clj                            # socket
HIVE_GIMP_TRANSPORT=python clojure -M:python -i dev/verify_live_gimp.clj  # libpython-clj
```

`verify_python_transport.clj` needs no GIMP. It runs the Python transport against
a fake plugin: a JVM `ServerSocket` implementing the same wire protocol, including
the detail that makes it interesting, which is that responses carry no
terminator. If the transport can talk to that, it can talk to GIMP, because the
wire is the entire contract between them.

## The native plug-in: GIMP's side in Clojure, on clojurust

`native/` is a GIMP 3 plug-in with no Python in it. GIMP execs a launcher that
runs `cljrs` (clojurust) on `native/src/hive_gimp/plugin/main.cljrs`; a small
Rust cdylib registers the GimpPlugIn subclass, enters libgimp's `gimp_main`,
owns the socket and wraps the libgimp calls; the command table, the JSON codec
and the colour normalisation are portable Clojure that run the same on the JVM,
ClojureWasm and clojurust. It speaks the socket contract above, so this side
reaches it with nothing but a port:

```clojure
(def g (gimp/connect {:port 9878}))
(gimp/invoke g "new_canvas" {:width 320 :height 200 :fill "orange"})
```

It implements 25 of the catalogued commands today: composition (`place_text`,
`add_text`, `gradient_fill`, `place_image`; see `native/README.md`), server and info
(`check_server`, `get_gimp_info`), files (`new_canvas`, `open_image`,
`save_xcf`, `export_image`, `close_image`, `list_images`,
`get_image_metadata`), whole-image transforms (`scale_image`, `crop_to_rect`,
`rotate_image` in quarter turns, `flip_image`, `flatten_image`) and layers
(`create_layer`, `list_layers`, `fill_layer`, `delete_layer`, `rename_layer`,
`duplicate_layer`, `set_layer_properties`). The Python reference plug-in still
covers the rest, on its own port, and both can be installed at once.

```bash
native/build.sh && native/install.sh
dev/verify_native_plugin.sh     # live: headless flatpak GIMP 3.2.4 + this JVM client + ffmpeg pixel checks
```

Measured along the way, all in `native/README.md`: a Clojure callback from the
cdylib breaks the moment it builds a vector past 32 elements, so no Clojure runs
inside one; `true?`/`false?`/`identical?` answer wrongly on clojurust once a fn
is hot; and GEGL paints an unknown colour name transparent cyan and reads
`rgb()` channels as 0..1, both while reporting success, which the Python
reference plug-in passes straight through.

## A note on Basilisp

The GIMP-side plug-in now exists in Clojure, on clojurust (above), which is what
this section used to propose Basilisp for. Basilisp remains the other route: it
runs in GIMP's own Python, where `gi.repository.Gimp` is plain interop, and it
needs no Rust. The seam is still the socket contract, so either drops in behind
it without one line changing on the JVM side.

## License

MIT. The command contract is derived from
[gimp-mcp](https://github.com/) (MIT).
