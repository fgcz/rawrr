#!/usr/bin/env bash
# Verify a rawrr binary against inst/extdata/sample.raw.
# usage: verify-rawrr.sh <rawrr executable> <sample.raw> <expected version>
# Runs on Linux, macOS, and Windows (git bash). Exits non-zero on the first failure.
set -euo pipefail

exe=$1
raw=$2
expected=$3

fail() { echo "FAIL: $*" >&2; exit 1; }
ok() { echo "ok:   $*"; }

[ -f "$exe" ] || fail "no such file: $exe"
chmod +x "$exe" 2>/dev/null || true

# health check used by rawrr:::.isAssemblyWorking()
out=$("$exe" | tr -d '\r' | head -n 1)
[ "$out" = "No RAW file specified!" ] || fail "health check printed '$out'"
ok "health check"

version=$("$exe" version | tr -d '\r')
[ "$version" = "$expected" ] || fail "version '$version', expected '$expected'"
ok "version $version"

n=$("$exe" "$raw" index | tr -d '\r' | grep -vc '^#')
[ "$n" -eq 575 ] || fail "index has $n lines, expected 575 (header + 574 scans)"
ok "index ($n lines)"

"$exe" help --markdown | tr -d '\r' | grep -q '^| `statusLog` |' || fail "help --markdown lists no statusLog option"
ok "help --markdown"

"$exe" "$raw" devices | tr -d '\r' | grep -q '^MS;1;' || fail "devices lists no MS device"
ok "devices"

# status log values must be unformatted (full precision): 274.9595..., not 274.96
value=$("$exe" "$raw" statusLog MS 1 7 | tr -d '\r' | sed -n 2p | cut -d';' -f2)
decimals=${value#*.}
[ "${#decimals}" -gt 2 ] || fail "statusLog value '$value' is rounded (binary predates the unformatted status log values)"
ok "statusLog ($value)"

echo "all checks passed: $exe"
