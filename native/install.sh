#!/usr/bin/env bash
# Install the native plug-in into GIMP 3's user plug-in directory.
#
# What gets installed is a LAUNCHER, not a binary: GIMP 3 runs a plug-in by
# executing <plug-ins>/<name>/<name>, and this one execs the clojurust binary
# on native/src/hive_gimp/plugin/main.cljrs, from native/ so that cljrs.edn
# loads the cdylib. Nothing is copied, so a rebuild needs no reinstall.
#
# The flip side of copying nothing is that the launcher holds two absolute
# paths into checkouts this script does not own. Both are checked here, and
# again by the launcher at run time, because GIMP reports an exec of a missing
# file as nothing more informative than "Plug-in crashed".
#
# Flatpak GIMP reads plug-ins from the HOST ~/.config/GIMP/<ver>/plug-ins, not
# the sandbox's ~/.var/app/org.gimp.GIMP/config, because its manifest grants
# xdg-config/GIMP:create. The sandbox has filesystem=host, so the launcher's
# absolute paths into this checkout resolve inside it, and the debug cljrs
# binary links only libc, libm and libgcc_s, which the GNOME runtime provides.
#
# The Python reference plug-in (gimp-mcp-plugin) is left where it is. The
# native one listens on its own port (9878 unless HIVE_GIMP_NATIVE_PORT is set
# at install time), so both can be registered at once.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CLJRS="${CLJRS:-/home/leibniz/PP/clojurust/target/debug/cljrs}"
GIMP_VERSION="${GIMP_VERSION:-3.2}"
PORT="${HIVE_GIMP_NATIVE_PORT:-9878}"
name="hive-gimp-native"
dir="$HOME/.config/GIMP/$GIMP_VERSION/plug-ins/$name"
lib="$here/rust/target/debug/libhive_gimp_native.so"

# Why the DEBUG build, stated once and quoted by both refusals below:
# cljrs loads a project's own :rust cdylib from target/DEBUG unconditionally
# (crates/cljrs/src/native/mod.rs, load_project_lib passes release=false), and
# that cdylib statically links its own copy of the cljrs runtime crates, so a
# cljrs binary from a different profile segfaults on the first call into a
# registered fn. See build.sh.
debug_note="It must be a DEBUG cljrs build: cljrs loads this plug-in's cdylib from
target/debug, and the cdylib statically links its own copy of the cljrs
runtime, so a RELEASE cljrs segfaults on the first native call. A release
binary is not a substitute; see $here/build.sh."

if [[ ! -e "$CLJRS" ]]; then
  cat >&2 <<MSG
REFUSED: no cljrs binary at
    $CLJRS
That path would be baked into the launcher this script writes, and a launcher
that execs a file which is not there is all GIMP can report as "Plug-in
crashed". Nothing was installed.

Point this script at an existing binary:
    CLJRS=/path/to/clojurust/target/debug/cljrs $here/install.sh

$debug_note
MSG
  exit 2
fi

if [[ ! -x "$CLJRS" ]]; then
  cat >&2 <<MSG
REFUSED: the cljrs at
    $CLJRS
exists but is not executable. Nothing was installed.
    chmod +x $CLJRS
or point this script elsewhere:
    CLJRS=/path/to/clojurust/target/debug/cljrs $here/install.sh

$debug_note
MSG
  exit 2
fi

case "$CLJRS" in
  */target/release/*)
    cat >&2 <<MSG
WARNING: $CLJRS looks like a RELEASE build.
$debug_note
Installing anyway, because the path is only a hint about the profile -- but if
the plug-in dies on its first native call, this is why.
MSG
    ;;
esac

if [[ ! -f "$lib" ]]; then
  cat >&2 <<MSG
REFUSED: no plug-in cdylib at
    $lib
Nothing was installed. Build it first:
    $here/build.sh
MSG
  exit 2
fi

mkdir -p "$dir"
cat > "$dir/$name" <<EOF
#!/bin/sh
# Installed by hive-gimp/native/install.sh. GIMP passes its wire-protocol
# arguments; they are forwarded after the port and this launcher's own path,
# which gimp_main reads as argv[0].
#
# The two paths below were baked in at install time, into checkouts this
# launcher does not own. If either has gone since (a deleted or rebuilt
# clojurust target/, a cargo clean here), say so on stderr -- which is where
# GIMP's plug-in log comes from -- rather than exec'ing a missing file, which
# GIMP reports only as "Plug-in crashed", naming nothing.
cljrs="$CLJRS"
lib="$lib"
if [ ! -x "\$cljrs" ]; then
  echo "hive-gimp-native: NOT STARTING: no executable cljrs at \$cljrs" >&2
  echo "hive-gimp-native: it must be a DEBUG cljrs build (a release one segfaults on the first native call)." >&2
  echo "hive-gimp-native: fix: CLJRS=/path/to/clojurust/target/debug/cljrs $here/install.sh" >&2
  exit 1
fi
if [ ! -f "\$lib" ]; then
  echo "hive-gimp-native: NOT STARTING: no plug-in cdylib at \$lib" >&2
  echo "hive-gimp-native: fix: $here/build.sh" >&2
  exit 1
fi
cd "$here" || exit 1
exec "\$cljrs" run src/hive_gimp/plugin/main.cljrs -- "$PORT" "\$0" "\$@"
EOF
chmod +x "$dir/$name"
echo "installed $dir/$name (port $PORT)"
