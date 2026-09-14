# hive-gimp native plug-in

A GIMP 3 plug-in written in Clojure, run by **clojurust** (cljrs) inside GIMP,
speaking the same socket protocol as the Python reference plug-in. The JVM side
of hive-gimp talks to it unchanged: `(gimp/connect {:port 9878})`.

    GIMP 3 (flatpak) ── execs ──► launcher ── exec ──► cljrs run main.cljrs
                                                         │
                          gimp-main thread (Rust)        │ cljrs main thread
                          gimp_main ─► run_procedure     │ accept ─► dispatch ─► respond
                                       parks on condvar  │    libgimp calls through the port
                                                         │
    hive-gimp JVM client ── TCP 127.0.0.1:9878, one JSON object per connection ──┘

## Layout

| piece | language | role |
|---|---|---|
| `src/hive_gimp/plugin/json.cljc` | portable Clojure | the wire codec (no JSON library exists on cljrs) |
| `src/hive_gimp/plugin/color.cljc` | portable Clojure | CSS colour -> `#rrggbb` before GEGL sees it |
| `src/hive_gimp/plugin/dispatch.cljc` | portable Clojure | the command table over a GIMP **port** (a map of fns) |
| `src/hive_gimp/plugin/fake.cljc` | portable Clojure | an in-memory port for tests and the host smoke run |
| `src/hive_gimp/plugin/main.cljrs` | cljrs | entry: start gimp_main, serve, finish |
| `src/hive_gimp/plugin/smoke.cljrs` | cljrs | the server over the fake port, no GIMP |
| `rust/src/lib.rs` | Rust cdylib | GimpPlugIn subclass + gimp_main, the socket, libgimp wrappers |

The portable half runs identically on the JVM, ClojureWasm (cljw) and cljrs:
`dev/native_portability.cljc` checks it on all three, 60 passes each.

## Commands

`check_server`, `get_gimp_info`, `list_images`, `get_image_metadata`,
`new_canvas`, `create_layer`, `list_layers`, `fill_layer`, `export_image`,
`close_image`, plus `quit_server` (stops serving and lets GIMP's procedure
return). Every other catalogued command answers `Unknown command`, naming the
ones implemented. Adding one is a row in `dispatch/commands`, plus any libgimp
wrapper it needs in `lib.rs` and the `gimp` port map in `main.cljrs`
(`plugin-test/the-fake-real-and-declared-ports-agree` keeps the three in step).

## Build, install, verify

    ./build.sh                          # the cdylib (debug profile, see below)
    ./install.sh                        # launcher into ~/.config/GIMP/3.2/plug-ins
    ../dev/verify_native_plugin.sh      # live gate against a real headless GIMP

Host smoke without GIMP:

    cljrs run src/hive_gimp/plugin/smoke.cljrs -- 9879

Start it by hand:

    flatpak run org.gimp.GIMP -n -i -d -f --batch-interpreter plug-in-script-fu-eval \
      -b '(plug-in-hive-gimp-native #:run-mode RUN-NONINTERACTIVE)' -b '(gimp-quit 0)'

With a GUI, drop `-i -d -f`: the batch still runs and GIMP keeps its window.

## Things that were measured, not assumed

**No Clojure runs inside a callback from the cdylib.** The obvious design has
libgimp's run function call back into a Clojure handler. A Clojure fn invoked
from the cdylib through `cljrs_runtime::env::callback::invoke` panics in rpds
("cannot have a branch at this height") as soon as it builds a vector past 32
elements; called directly from cljrs the same fn is fine. The cdylib links its
own copy of the runtime crates. So gimp_main runs on a Rust thread and parks
inside the procedure while the host interpreter serves, and only scalars cross
the boundary (id lists as comma-separated strings).

**`true?`, `false?` and `identical?` are wrong on cljrs once a fn is hot** (from
the 51st call). The codec tests booleans with `=`. Portable code for cljrs must
be exercised past that threshold, which is why the gates repeat.

**GEGL parses fewer colours than the contract promises.** `"orange"` becomes
transparent cyan with only a warning; `rgb(0,128,0)` is read as 0..1 floats and
fills full green. The reference Python plug-in passes both through. Here
`color/normalize` sends `#rrggbb` or refuses by name.

**The profile trap.** The cdylib statically links its own cljrs runtime crates,
so it must be built in the same cargo profile as the cljrs binary loading it.
cljrs loads project libraries from `target/debug`: pair with the DEBUG binary.

**Flatpak facts.** GIMP 3.2.4 runs on the GNOME 50 runtime (glibc 2.42). It
reads plug-ins from the HOST `~/.config/GIMP/3.2/plug-ins`, and the sandbox has
`filesystem=host`, so the launcher's absolute paths resolve inside it. The debug
cljrs binary links only libc, libm and libgcc_s. libgimp is dlopened by soname
from `/app/lib`, so nothing GIMP is needed at build time. Inside the sandbox
`cargo` is absent; cljrs then falls back to `rust/target` to find the cdylib.

**Layouts relied on**: `GimpPlugInClass` is `GObjectClass` (136 bytes on LP64)
followed by `query_procedures`, `init_procedures`, `create_procedure`, `quit`,
`set_i18n`, then ten reserved slots; `start` checks the queried class size is
large enough before writing vfuncs. Enum values are from the 3.2 headers
(`GIMP_PDB_SUCCESS` 3, `GIMP_FILL_BACKGROUND` 1, `GIMP_LAYER_MODE_NORMAL` 28).
