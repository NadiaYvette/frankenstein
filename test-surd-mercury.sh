#!/bin/bash
# Regression guard for surd-mercury byte-identity-with-native.
#
# As of session ending 2026-05-26, all 8 demos + test_surd produce
# byte-identical output to native Mercury.  This script asserts that
# property: any non-zero diff is a regression to fix before merging.
#
# Usage:
#   bash test-surd-mercury.sh           # runs all, reports counts
#   bash test-surd-mercury.sh --strict  # nonzero exit if any non-zero diff
#
# Targets: demo_elliptic_integral, demo_euler_integral, demo_factor_sf,
# demo_lead_coeff, demo_rational_smoke, demo_simplify_rad,
# demo_solvable_quintic, demo_trig_table, test_surd.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SURD_DIR="${SURD_DIR:-/home/nyc/src/surd/mercury}"
FRANK="cabal-3.16.1.0 -v0 run frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
STRICT="no"
[ "${1:-}" = "--strict" ] && STRICT="yes"

if [ ! -d "$SURD_DIR" ]; then
    echo "SURD_DIR=$SURD_DIR not found; set SURD_DIR env var to point to surd/mercury" >&2
    exit 2
fi

PROGS=(
    demo_elliptic_integral
    demo_euler_integral
    demo_factor_sf
    demo_lead_coeff
    demo_rational_smoke
    demo_simplify_rad
    demo_solvable_quintic
    demo_trig_table
    test_surd
)

PASS=0
FAIL=0
FAIL_LIST=""

echo "========================================"
echo " Frankenstein surd-mercury regression"
echo "========================================"

cd "$SURD_DIR"

# Build native references if missing
for prog in "${PROGS[@]}"; do
    if [ ! -x "./$prog" ]; then
        echo "Building native $prog..."
        mmc --make "$prog" > /dev/null 2>&1 || echo "  WARN: native build failed"
    fi
done

cd "$SCRIPT_DIR"

for prog in "${PROGS[@]}"; do
    printf "  %-30s" "$prog"
    bin="/tmp/m-$prog-regression"
    rm -f "$bin"
    $FRANK "$SURD_DIR/$prog.m" --compile -o "$bin" > /dev/null 2>&1
    if [ ! -x "$bin" ]; then
        echo "FAIL (Frank compile)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $prog(compile)"
        continue
    fi
    if [ ! -x "$SURD_DIR/$prog" ]; then
        echo "SKIP (no native binary)"
        continue
    fi
    DL=$(diff <(timeout 90 "$SURD_DIR/$prog" 2>&1) <(timeout 90 "$bin" 2>&1) 2>&1 | wc -l)
    if [ "$DL" -eq 0 ]; then
        echo "PASS  (byte-identical)"
        PASS=$((PASS + 1))
    else
        echo "FAIL  ($DL diff lines)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $prog($DL)"
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
