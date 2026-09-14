#!/usr/bin/env bash
# Install the native plug-in into GIMP 3's user plug-in directory.
#
# What gets installed is a LAUNCHER, not a binary: GIMP 3 runs a plug-in by
# executing <plug-ins>/<name>/<name>, and this one execs the clojurust binary
# on native/src/hive_gimp/plugin/main.cljrs, from native/ so that cljrs.edn
# loads the cdylib. Nothing is copied, so a rebuild needs no reinstall.
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

[[ -x "$CLJRS" ]] || { echo "no cljrs at $CLJRS (set CLJRS; it must be the DEBUG build, see build.sh)"; exit 2; }
[[ -f "$here/rust/target/debug/libhive_gimp_native.so" ]] || { echo "build first: $here/build.sh"; exit 2; }

mkdir -p "$dir"
cat > "$dir/$name" <<EOF
#!/bin/sh
# Installed by hive-gimp/native/install.sh. GIMP passes its wire-protocol
# arguments; they are forwarded after the port and this launcher's own path,
# which gimp_main reads as argv[0].
cd "$here" || exit 1
exec "$CLJRS" run src/hive_gimp/plugin/main.cljrs -- "$PORT" "\$0" "\$@"
EOF
chmod +x "$dir/$name"
echo "installed $dir/$name (port $PORT)"
