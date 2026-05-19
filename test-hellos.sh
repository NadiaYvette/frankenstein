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
run_hello "hello-rust"        "Hello, World!" "examples/hello.rs"
# Clean Mercury temp dirs between runs
rm -rf /tmp/frankenstein-mercury-*/
run_hello "hello-mercury"     "Hello, World!" "examples/hello.m"

echo ""
echo "--- Chained IO actions ---"
run_hello "chained-haskell"   $'first\nsecond\nthird' "examples/chained_io.hs"
run_hello "chained-rust"      $'first\nsecond\nthird' "examples/chained_io.rs"
# Clean Mercury temp dirs between runs
rm -rf /tmp/frankenstein-mercury-*/
run_hello "chained-mercury"   $'first\nsecond\nthird' "examples/chained_io.m"

echo ""
echo "--- Show / print integers ---"
run_hello "show-haskell"      $'0\n42\n-7\n210' "examples/show_int.hs"

echo ""
echo "--- Show / print compound types ---"
run_hello "show-compound"     $'[]\n[42]\n[1,2,3,4,5]\n[-7,0,100]\nJust 42\nNothing' "examples/show_compound.hs"

echo ""
echo "--- Show / print derived ADTs ---"
run_hello "show-derived"      "Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)" "examples/show_derived.hs"
run_hello "show-tuple"        $'(1,2)\n(1,2,3)\n(-5,100)\n(42,0,-7,13)\n((7,13),42)' "examples/show_tuple.hs"
run_hello "show-mixed"        $'Red\nGreen\nBlue\nPoint 3 4\nCircle 10\nRectangle 7 13\nSquare 5' "examples/show_mixed.hs"

echo ""
echo "--- Rust formatted println! ---"
run_hello "rust-fmt"          $'a=41 b=101\nanswer = 142\nhello, frankenstein\nfrankenstein = 142' "examples/rust_fmt.rs"
run_hello "rust-dbg"          $'Display: 42\nDebug:   42\nDisplay: hello\tworld\nDebug:   "hello\\tworld"' "examples/rust_dbg.rs"
run_hello "rust-radix"        $'dec: 255\nhex: ff\nHEX: FF\noct: 377\nbin: 11111111' "examples/rust_radix.rs"
run_hello "rust-spec"         $'[   42]\n[42   ]\n[   42]\n[ 42  ]\n[00042]\n[-0007]\n[xxx42]\n[000000ff]\n[hi   ]\n[frank]\n[00042]\n[frank     ]\n[+42]\n[-7]\n[+0042]' "examples/rust_spec.rs"

echo ""
echo "========================================"
if [ "$FAIL" -eq 0 ]; then
    echo "  ALL HELLOS PASS ($PASS/$((PASS+FAIL)))"
else
    echo "  $PASS passed, $FAIL failed"
fi
echo "========================================"
exit $FAIL
