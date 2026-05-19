#!/bin/bash
# Phase A test driver for hello-world per language.
#
# Compiles and runs the easy-five hello programs through their respective
# frontend bridges, verifying expected output.  This is the file-layout
# pattern that Phases B (stdlib coverage) and C (cross-shim audit) will
# inherit — keep the structure parallel to examples/polyglot-demo/test-polyglot.sh.
#
# Bridges with string-ABI gaps degrade to integer output (length 13 of
# "Hello, World!").  Native string output is currently only available
# from the Koka and Python bridges.  See docs/test-coverage-plan.md and
# ROADMAP's "Phase 9 Outstanding Issues" → BRIDGE_<lang>_strings entries.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
BUILD_DIR="$SCRIPT_DIR/build/hellos"
PASS=0
FAIL=0

cd "$SCRIPT_DIR"

rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
# Mercury bridge leaves temp dirs; clean them so stale state can't poison the run
rm -rf /tmp/frankenstein-mercury-*/

echo "========================================"
echo " Frankenstein Phase A Hello Test Suite"
echo "========================================"
echo ""

run_hello() {
    local name="$1"
    local expected="$2"
    shift 2
    local sources=("$@")
    local output_bin="$BUILD_DIR/${name}"

    printf "  %-40s" "$name"

    local compile_output
    compile_output=$($FRANK "${sources[@]}" --compile -o "$output_bin" 2>&1)
    local compile_rc=$?

    if [ $compile_rc -ne 0 ] || [ ! -x "$output_bin" ]; then
        echo "FAIL (compile)"
        echo "$compile_output" | tail -5 | sed 's/^/    /'
        FAIL=$((FAIL + 1))
        return
    fi

    local actual
    actual=$("$output_bin" 2>&1)
    local run_rc=$?

    if [ $run_rc -ne 0 ]; then
        echo "FAIL (exit $run_rc)"
        FAIL=$((FAIL + 1))
        return
    fi

    if [ "$actual" = "$expected" ]; then
        echo "PASS  ($actual)"
        PASS=$((PASS + 1))
    else
        echo "FAIL  (expected '$expected', got '$actual')"
        FAIL=$((FAIL + 1))
    fi
}

echo "--- Native string output (full string ABI) ---"
run_hello "hello-koka"        "Hello, World!" "examples/hello.kk"
run_hello "hello-python"      "Hello, world"  "examples/hello.py"
run_hello "hello-haskell"     "Hello, World!" "examples/hello.hs"

echo ""
echo "--- Degraded to Int (string-ABI gap; routes through printf wrapper) ---"
run_hello "hello-rust"        "13"            "examples/hello.rs"

# Clean Mercury temp dirs between runs
rm -rf /tmp/frankenstein-mercury-*/
run_hello "hello-mercury"     "13"            "examples/hello.m"

echo ""
echo "========================================"
if [ "$FAIL" -eq 0 ]; then
    echo "  ALL HELLOS PASS ($PASS/$((PASS+FAIL)))"
else
    echo "  $PASS passed, $FAIL failed"
fi
echo "========================================"
exit $FAIL
