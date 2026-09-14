#!/usr/bin/env bash
# Live gate for the native (clojurust) GIMP plug-in, against a real GIMP 3.
#
#   dev/verify_native_plugin.sh            (from the hive-gimp checkout)
#
# 1. the portable core on the three hosts that must agree (cljrs, cljw, JVM)
# 2. install the launcher, start flatpak GIMP HEADLESS, and have Script-Fu run
#    plug-in-hive-gimp-native, which blocks serving the socket
# 3. hive-gimp's own JVM client, unchanged, drives it (dev/verify_native_plugin.clj)
# 4. ffmpeg reads the exported PNG back: size and pixels, an oracle that is
#    neither GIMP nor the plug-in
# 5. quit_server, and GIMP is stopped by the PID this script started
#
# Exits non-zero when any stage fails. Refuses to start when little RAM is free
# or the port is already taken (a leftover server would be graded instead).
set -uo pipefail

repo="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CLJRS="${CLJRS:-/home/leibniz/PP/clojurust/target/debug/cljrs}"
PORT="${HIVE_GIMP_NATIVE_PORT:-9878}"
out="$(mktemp -d)"
png="$out/native.png"
png2="$out/transformed.png"
xcf="$out/native.xcf"
fail=0

avail=$(free -g | awk '/^Mem:/ {print $7}')
(( avail >= 3 )) || { echo "REFUSED: ${avail} GB available"; exit 2; }
if ss -ltn | grep -q ":$PORT "; then echo "REFUSED: port $PORT is already listening"; exit 2; fi
for tool in flatpak ffmpeg ffprobe ncat cljw clojure; do
  command -v "$tool" >/dev/null || { echo "REFUSED: $tool not on PATH"; exit 2; }
done

echo "== 1. portable core on cljrs, cljw and the JVM"
cd "$repo"
"$CLJRS" run --src-path native/src dev/native_portability.cljc 2>&1 | tail -1 || fail=1
cljw -cp native/src dev/native_portability.cljc 2>&1 | tail -1 || fail=1
clojure -J-Xmx512m -Sdeps '{:paths ["native/src"]}' -M dev/native_portability.cljc 2>&1 | tail -1 || fail=1

echo "== 2. install and start GIMP headless"
HIVE_GIMP_NATIVE_PORT="$PORT" "$repo/native/install.sh"
flatpak run org.gimp.GIMP -n -i -d -f \
  --batch-interpreter plug-in-script-fu-eval \
  -b '(plug-in-hive-gimp-native #:run-mode RUN-NONINTERACTIVE)' \
  -b '(gimp-quit 0)' > "$out/gimp.log" 2>&1 &
gimp_pid=$!
up=0
for _ in $(seq 1 120); do
  if ss -ltn | grep -q ":$PORT "; then up=1; break; fi
  kill -0 "$gimp_pid" 2>/dev/null || break
  sleep 1
done
if (( ! up )); then
  echo "  FAIL the plug-in never listened on $PORT"; tail -20 "$out/gimp.log"
  kill "$gimp_pid" 2>/dev/null; exit 1
fi

echo "== 3. hive-gimp's JVM client against the native plug-in"
clojure -J-Xmx512m -M dev/verify_native_plugin.clj "$PORT" "$png" "$png2" "$xcf" || fail=1

echo "== 4. the exported PNGs, read by ffmpeg"
check_size() { # file expected
  local size; size=$(ffprobe -v error -show_entries stream=width,height -of csv=p=0 "$1" 2>/dev/null)
  [[ "$size" == "$2" ]] && echo "  OK   $(basename "$1") size $size" || { echo "  FAIL $(basename "$1") size: expected $2 got $size"; fail=1; }
}
pixel() { ffmpeg -v error -i "$1" -vf "crop=1:1:$2:$3" -f rawvideo -pix_fmt rgb24 - | od -An -tu1 | xargs; }
check_pixel() { # label file x y expected
  local got; got=$(pixel "$2" "$3" "$4")
  [[ "$got" == "$5" ]] && echo "  OK   $1 ($3,$4) = $got" || { echo "  FAIL $1 ($3,$4): expected $5 got $got"; fail=1; }
}
check_size "$png" "320,200"
check_pixel "overlay, rgb(0, 128, 0)" "$png" 10 10 "0 128 0"
check_pixel "background, orange"      "$png" 300 190 "255 165 0"
# The overlay covered x 0-99, y 0-79 of 320x200. Rotated 90 degrees clockwise
# (200x320) it covers x 120-199, y 0-99; flipped horizontally, x 0-79, y 0-99;
# cropped to the top-left 100x100 it is x 0-79 of 100; scaled to 50x50, x 0-39.
check_size "$png2" "50,50"
check_pixel "after rotate/flip/crop/scale: overlay" "$png2" 10 25 "0 128 0"
check_pixel "after rotate/flip/crop/scale: orange"  "$png2" 47 25 "255 165 0"
[[ -s "$xcf" ]] && echo "  OK   save_xcf wrote $(stat -c %s "$xcf") bytes" || { echo "  FAIL save_xcf wrote nothing"; fail=1; }

echo "== 5. quit_server and stop GIMP"
printf '{"type":"quit_server"}' | ncat -w 10 127.0.0.1 "$PORT"; echo
for _ in $(seq 1 30); do kill -0 "$gimp_pid" 2>/dev/null || break; sleep 1; done
if kill -0 "$gimp_pid" 2>/dev/null; then kill "$gimp_pid"; echo "  GIMP did not exit on its own; stopped pid $gimp_pid"; fi
grep -E "hive-gimp-native:|GEGL-WARNING" "$out/gimp.log" | sed 's/^/  gimp: /'

if (( fail )); then echo "native plug-in gate FAILED"; exit 1; fi
echo "native plug-in gate passed"
