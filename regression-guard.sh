#!/bin/bash
# Umbrella regression guard for the achievements established before
# bootstrap-restoration work began.  Refuses if any baseline degrades.
#
# Current baseline (session ending 2026-05-28):
#   test-hellos.sh           — 25/26 PASS (1 known FAIL)
#   surd-mercury             — 9/9 byte-identical with native
#   surd-idris2              — 3/4 byte-identical (quintic Galois-group bug)
#   surd-koka                — 0/4 (missing runtime shims for sin/pow/etc.)
#   surd-haskell             — 0/4 (multi-module library imports unresolved)
#
# Bootstrap (self-host/build.sh) was restored to fixed-point in commits
# d…→8d9e6ca→776d4a3→3da8f6f; included separately in CI rather than
# this script because it takes ~30 minutes.
#
# Usage:
#   bash regression-guard.sh         # report counts only
#   bash regression-guard.sh --strict  # nonzero exit on any degradation

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
STRICT="no"
[ "${1:-}" = "--strict" ] && STRICT="yes"

# Baselines: minimum acceptable PASS counts.  Increment when achievements grow.
HELLOS_MIN=25
SURD_MERCURY_MIN=9
SURD_IDRIS2_MIN=3
SURD_KOKA_MIN=0
SURD_HASKELL_MIN=0

cd "$SCRIPT_DIR"

echo "########################################"
echo "# Frankenstein regression guard"
echo "########################################"

echo
echo "=== Hello-worlds ==="
HELLOS_OUT=$(bash test-hellos.sh 2>&1)
# Match either "X passed, Y failed" (mixed run) or "ALL HELLOS PASS (X/Y)" (clean).
HELLOS_PASS=$(echo "$HELLOS_OUT" | grep -oE '[0-9]+ passed' | head -1 | awk '{print $1}')
if [ -z "$HELLOS_PASS" ]; then
    HELLOS_PASS=$(echo "$HELLOS_OUT" | grep -oE 'ALL HELLOS PASS \([0-9]+/[0-9]+\)' | head -1 | grep -oE '\([0-9]+' | grep -oE '[0-9]+')
fi
HELLOS_PASS="${HELLOS_PASS:-0}"
echo "  $HELLOS_PASS passed (baseline: $HELLOS_MIN)"

# Helper: run a test script, extract "<N> passed" count
run_suite() {
    local label="$1"
    local script="$2"
    local minvar="$3"
    local out
    out=$(bash "$script" 2>&1)
    local pass
    pass=$(echo "$out" | grep -oE '[0-9]+ passed' | head -1 | awk '{print $1}')
    pass="${pass:-0}"
    local min="${!minvar}"
    echo "  $pass passed (baseline: $min)"
    if [ "$pass" -lt "$min" ]; then
        echo "  details:"
        echo "$out" | grep 'failing:' | sed 's/^/    /'
    fi
    eval "${label}_PASS=$pass"
}

echo
echo "=== surd-mercury demos ==="
run_suite SURD_MERCURY  test-surd-mercury.sh  SURD_MERCURY_MIN

echo
echo "=== surd-idris2 demos ==="
run_suite SURD_IDRIS2   test-surd-idris2.sh   SURD_IDRIS2_MIN

echo
echo "=== surd-koka demos ==="
run_suite SURD_KOKA     test-surd-koka.sh     SURD_KOKA_MIN

echo
echo "=== surd-haskell demos ==="
run_suite SURD_HASKELL  test-surd-haskell.sh  SURD_HASKELL_MIN

echo
echo "########################################"
DEGRADED=0
[ "$HELLOS_PASS" -lt "$HELLOS_MIN" ] && {
    echo "# REGRESSION: hello-worlds $HELLOS_PASS < baseline $HELLOS_MIN"
    DEGRADED=1
}
[ "$SURD_MERCURY_PASS" -lt "$SURD_MERCURY_MIN" ] && {
    echo "# REGRESSION: surd-mercury $SURD_MERCURY_PASS < baseline $SURD_MERCURY_MIN"
    DEGRADED=1
}
[ "$SURD_IDRIS2_PASS" -lt "$SURD_IDRIS2_MIN" ] && {
    echo "# REGRESSION: surd-idris2 $SURD_IDRIS2_PASS < baseline $SURD_IDRIS2_MIN"
    DEGRADED=1
}
[ "$SURD_KOKA_PASS" -lt "$SURD_KOKA_MIN" ] && {
    echo "# REGRESSION: surd-koka $SURD_KOKA_PASS < baseline $SURD_KOKA_MIN"
    DEGRADED=1
}
[ "$SURD_HASKELL_PASS" -lt "$SURD_HASKELL_MIN" ] && {
    echo "# REGRESSION: surd-haskell $SURD_HASKELL_PASS < baseline $SURD_HASKELL_MIN"
    DEGRADED=1
}
if [ "$DEGRADED" = "0" ]; then
    echo "# OK: all baselines met or exceeded"
fi
echo "########################################"

if [ "$STRICT" = "yes" ] && [ "$DEGRADED" = "1" ]; then
    exit 1
fi
