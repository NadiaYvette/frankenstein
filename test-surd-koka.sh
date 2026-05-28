#!/bin/bash
# Regression guard for surd-koka byte-identity-with-native.
#
# Compiles each of the 4 surd demos written in Koka through the
# Frankenstein Koka bridge (which uses Koka's own compiler API for
# elaboration), then byte-diffs the output against native Koka.
#
# Usage:
#   bash test-surd-koka.sh           # runs all, reports counts
#   bash test-surd-koka.sh --strict  # nonzero exit if any non-zero diff
#
# Targets: trig-table, euler-integral, elliptic-integral, solvable-quintic.
#
# Status note (as of 2026-05-28): the Koka demos depend on the full
# surd/koka library (~38 modules).  Frankenstein currently emits
# warnings for unresolved symbols (sin, pow, max, etc.) and the
# compiled binaries segfault on call to those NULL stubs.  This
# script captures the regression baseline; closing the gap will
# require shim coverage for the missing Koka runtime functions.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SURD_DIR="${SURD_DIR:-/home/nyc/src/surd/koka}"
DEMO_DIR="$SURD_DIR/surd/demo"
FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
STRICT="no"
[ "${1:-}" = "--strict" ] && STRICT="yes"

if [ ! -d "$DEMO_DIR" ]; then
    echo "DEMO_DIR=$DEMO_DIR not found; set SURD_DIR env var to point to surd/koka" >&2
    exit 2
fi

# Each entry is "<demo-base> <native-binary-suffix>"
# Native koka mangles file names: dashes become "_dash_" in the binary name.
TARGETS=(
    "trig-table        surd_demo_trig_dash_table__main"
    "euler-integral    surd_demo_euler_dash_integral__main"
    "elliptic-integral surd_demo_elliptic_dash_integral__main"
    "solvable-quintic  surd_demo_solvable_dash_quintic__main"
)

PASS=0
FAIL=0
FAIL_LIST=""

echo "========================================"
echo " Frankenstein surd-koka regression"
echo "========================================"

# Build all native references in one pass (Koka shares incremental compile state)
cd "$SURD_DIR"
for entry in "${TARGETS[@]}"; do
    demo="${entry%% *}"
    timeout 300 koka --compile -i. "surd/demo/$demo.kk" > /dev/null 2>&1 || true
done

# Locate the latest koka build dir (version number varies)
NATIVE_DIR=$(ls -dt "$SURD_DIR/.koka"/*/gcc-debug-* 2>/dev/null | head -1)

for entry in "${TARGETS[@]}"; do
    demo=$(echo "$entry" | awk '{print $1}')
    nat_suffix=$(echo "$entry" | awk '{print $2}')
    printf "  %-22s" "$demo"

    native="$NATIVE_DIR/$nat_suffix"
    if [ ! -x "$native" ]; then
        echo "SKIP (no native binary at $native)"
        continue
    fi

    bin="/tmp/k-$demo-regression"
    rm -f "$bin"
    (cd "$SCRIPT_DIR" && \
        KOKA_INCLUDE="$SURD_DIR" \
        $FRANK "$DEMO_DIR/$demo.kk" --compile -o "$bin") \
        > /tmp/frank-koka-$demo.log 2>&1

    if [ ! -x "$bin" ]; then
        echo "FAIL (Frank compile)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $demo(compile)"
        continue
    fi

    DL=$(diff <(timeout 90 "$native" 2>&1) <(timeout 90 "$bin" 2>&1) 2>&1 | wc -l)
    if [ "$DL" -eq 0 ]; then
        echo "PASS  (byte-identical)"
        PASS=$((PASS + 1))
    else
        echo "FAIL  ($DL diff lines)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $demo($DL)"
    fi
done

echo "========================================"
echo "  $PASS passed, $FAIL failed"
if [ -n "$FAIL_LIST" ]; then
    echo "  failing: $FAIL_LIST"
fi
echo "========================================"

if [ "$STRICT" = "yes" ] && [ "$FAIL" -gt 0 ]; then
    exit 1
fi
