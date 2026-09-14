#!/usr/bin/env bash
# Build the native plug-in's cdylib, the library cljrs loads.
#
# PROFILE MATTERS. The cdylib statically links its own copy of the cljrs
# runtime crates, so it must be built in the SAME cargo profile as the cljrs
# binary that loads it, or the first call into a registered fn segfaults. cljrs
# loads a project library from target/debug, so this pairs with the DEBUG cljrs
# binary. Same trap, same fix, as hive-k8s/native.
#
# No GIMP headers or libraries are needed: libgimp and friends are dlopened
# when the plug-in runs inside GIMP.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

avail=$(free -g | awk '/^Mem:/ {print $7}')
if (( avail < 3 )); then
  echo "REFUSED: ${avail} GB available, a cargo build here wants at least 3" >&2
  exit 2
fi

cd "$here/rust"
cargo build -j 4
ls -la "$here/rust/target/debug/libhive_gimp_native.so"
