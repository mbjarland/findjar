#!/usr/bin/env bash
# Regenerate doc/img/demo.svg — the colored hero image at the top of
# README.md. Captures findjar's ANSI output via FORCE_COLOR=1, wraps
# it in a synthetic asciinema cast, and renders to SVG via svg-term-cli.
#
# Requirements:
#   - target/findjar built (clj -T:build native-image)
#   - svg-term-cli on PATH (npm install -g svg-term-cli)
#   - python3
set -euo pipefail

cd "$(dirname "$0")/.."

BIN="target/findjar"
M2="$HOME/.m2/repository/org/clojure/clojure/1.11.1"
OUT="doc/img/demo.svg"
CAST="$(mktemp -t findjar-demo.XXXXXX.cast)"
TXT="$(mktemp -t findjar-demo.XXXXXX.txt)"
trap 'rm -f "$CAST" "$TXT"' EXIT

if [[ ! -x "$BIN" ]]; then
  echo "error: $BIN not built. Run: clj -T:build native-image" >&2
  exit 1
fi

if [[ ! -d "$M2" ]]; then
  echo "error: $M2 not found. Pull a copy of org.clojure/clojure first." >&2
  exit 1
fi

if ! command -v svg-term >/dev/null 2>&1; then
  echo "error: svg-term not on PATH. Install via: npm install -g svg-term-cli" >&2
  exit 1
fi

mkdir -p "$(dirname "$OUT")"

FORCE_COLOR=1 "$BIN" "$M2" -n core.clj -g 'Rich Hickey' -t j -x 1 > "$TXT"

python3 - "$TXT" "$CAST" <<'PY'
import json, sys, time
inp, outp = sys.argv[1], sys.argv[2]
with open(inp, 'rb') as f:
    body = f.read().decode('utf-8', errors='replace')
# xterm.js (used by svg-term) runs in raw mode: a bare LF moves the
# cursor down one row but does NOT return it to col 0. Convert to CRLF
# so each new row starts at the left edge.
body = body.replace('\n', '\r\n')
ESC = '\x1b'
prompt = (f'{ESC}[1;36m~{ESC}[m {ESC}[33m❯{ESC}[m '
          "findjar ~/.m2 -n core.clj -g 'Rich Hickey' -t j -x 1\r\n")
header = {"version": 2, "width": 130, "height": 16,
          "timestamp": int(time.time()),
          "env": {"SHELL": "/bin/bash", "TERM": "xterm-256color"}}
with open(outp, 'w') as f:
    f.write(json.dumps(header) + "\n")
    f.write(json.dumps([0.0, "o", prompt]) + "\n")
    f.write(json.dumps([0.4, "o", body]) + "\n")
PY

svg-term --in "$CAST" --out "$OUT" \
  --width 130 --height 7 --window --no-cursor --at 1000 --padding 14

echo "wrote $OUT"
