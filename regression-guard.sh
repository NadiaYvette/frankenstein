#!/bin/bash
# Umbrella regression guard for the achievements established before
# bootstrap-restoration work began.  Refuses if any baseline degrades.
#
# Current baseline (session ending 2026-05-26):
#   test-hellos.sh       — 25/26 PASS (1 known FAIL)
#   surd-mercury         — 9/9 byte-identical with native
#
# Usage:
#   bash regression-guard.sh         # report counts only
#   bash regression-guard.sh --strict  # nonzero exit on any degradation
#
# The bootstrap (self-host/build.sh) is intentionally NOT here — it's
# currently regressed (Phase 8: 0/21, fixed-point: 2/26) and restoring
# it IS the upcoming work.  This guard ensures the restoration arc
# doesn't accidentally undo what's already passing.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
STRICT="no"
[ "${1:-}" = "--strict" ] && STRICT="yes"

# Baselines: minimum acceptable PASS counts.  Increment when achievements grow.
HELLOS_MIN=25
SURD_MIN=9

cd "$SCRIPT_DIR"

echo "########################################"
echo "# Frankenstein regression guard"
echo "########################################"

echo
echo "=== Hello-worlds ==="
HELLOS_OUT=$(bash test-hellos.sh 2>&1)
HELLOS_PASS=$(echo "$HELLOS_OUT" | grep -oE '[0-9]+ passed' | head -1 | awk '{print $1}')
HELLOS_PASS="${HELLOS_PASS:-0}"
echo "  $HELLOS_PASS passed (baseline: $HELLOS_MIN)"

echo
echo "=== surd-mercury demos ==="
SURD_OUT=$(bash test-surd-mercury.sh 2>&1)
SURD_PASS=$(echo "$SURD_OUT" | grep -oE '[0-9]+ passed' | head -1 | awk '{print $1}')
SURD_PASS="${SURD_PASS:-0}"
echo "  $SURD_PASS passed (baseline: $SURD_MIN)"
if [ "$SURD_PASS" -lt "$SURD_MIN" ]; then
    echo "  details:"
    echo "$SURD_OUT" | grep 'failing:' | sed 's/^/    /'
fi

echo
echo "########################################"
DEGRADED=0
[ "$HELLOS_PASS" -lt "$HELLOS_MIN" ] && {
    echo "# REGRESSION: hello-worlds $HELLOS_PASS < baseline $HELLOS_MIN"
    DEGRADED=1
}
[ "$SURD_PASS" -lt "$SURD_MIN" ] && {
    echo "# REGRESSION: surd-mercury $SURD_PASS < baseline $SURD_MIN"
    DEGRADED=1
}
if [ "$DEGRADED" = "0" ]; then
    echo "# OK: all baselines met or exceeded"
fi
echo "########################################"

if [ "$STRICT" = "yes" ] && [ "$DEGRADED" = "1" ]; then
    exit 1
fi
