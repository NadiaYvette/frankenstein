#!/bin/bash
# Regression guard for surd-haskell byte-identity-with-native.
#
# Compiles each of the 4 surd demos written in Haskell through the
# Frankenstein GHC bridge, then byte-diffs the output against native
# GHC (cabal-built) binaries.
#
# Usage:
#   bash test-surd-haskell.sh           # runs all, reports counts
#   bash test-surd-haskell.sh --strict  # nonzero exit if any non-zero diff
#
# Targets: surd-trig-table, surd-euler-integral, surd-elliptic-integral,
# surd-solvable-quintic.
#
# Status note (as of 2026-05-28): the Haskell demos depend on the full
# Surd haskell library (50+ modules) which lives in surd/haskell/src/.
# Frankenstein's GHC bridge runs a single .hs file through GHC API, so
# it cannot directly resolve the cross-module imports.  For now this
# script captures the regression baseline (compilation will fail with
# "Could not find module Surd.*").  Closing the gap requires either:
#   (a) passing the full source tree to frankenstein, or
#   (b) a GHC-package-db integration that points at surd's dist-newstyle.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SURD_DIR="${SURD_DIR:-/home/nyc/src/surd/haskell}"
FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
STRICT="no"
[ "${1:-}" = "--strict" ] && STRICT="yes"

if [ ! -d "$SURD_DIR" ]; then
    echo "SURD_DIR=$SURD_DIR not found; set SURD_DIR env var to point to surd/haskell" >&2
    exit 2
fi

# Each entry is "<demo-source> <native-exe> <stable-args>"
# TrigTable takes CLI args, so pick a small stable invocation for reproducibility.
TARGETS=(
    "TrigTable          surd-trig-table        12"
    "EulerIntegral      surd-euler-integral    "
    "EllipticIntegral   surd-elliptic-integral "
    "SolvableQuintic    surd-solvable-quintic  "
)

PASS=0
FAIL=0
FAIL_LIST=""

echo "========================================"
echo " Frankenstein surd-haskell regression"
echo "========================================"

# Build native references via cabal once (capped so a broken environment
# can't hang the regression run forever — GHC 9.15 vs surd haskell pkg
# compatibility issues are currently breaking the cabal build).
cd "$SURD_DIR"
echo "Building native binaries via cabal (timeout 300s)..."
timeout 300 cabal build surd-trig-table surd-euler-integral surd-elliptic-integral surd-solvable-quintic \
    > /tmp/surd-haskell-cabal.log 2>&1
case "$?" in
    0)   echo "  cabal: OK" ;;
    124) echo "  cabal: TIMEOUT after 300s (see /tmp/surd-haskell-cabal.log)" ;;
    *)   echo "  cabal: errors (see /tmp/surd-haskell-cabal.log)" ;;
esac

for entry in "${TARGETS[@]}"; do
    demo=$(echo "$entry" | awk '{print $1}')
    exe=$(echo "$entry" | awk '{print $2}')
    args=$(echo "$entry" | awk '{$1=""; $2=""; print substr($0,3)}')
    printf "  %-26s" "$demo"

    native=$(cabal list-bin "$exe" 2>/dev/null)
    if [ -z "$native" ] || [ ! -x "$native" ]; then
        echo "SKIP (no native binary for $exe)"
        continue
    fi

    bin="/tmp/h-$demo-regression"
    rm -f "$bin"
    (cd "$SCRIPT_DIR" && $FRANK "$SURD_DIR/demo/$demo.hs" --compile -o "$bin") \
        > /tmp/frank-haskell-$demo.log 2>&1

    if [ ! -x "$bin" ]; then
        echo "FAIL (Frank compile)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $demo(compile)"
        continue
    fi

    DL=$(diff <(timeout 90 "$native" $args 2>&1) <(timeout 90 "$bin" $args 2>&1) 2>&1 | wc -l)
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
