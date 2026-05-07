#!/bin/bash
# Frankenstein Phase 1b — Polyglot Test Suite
#
# Compiles and runs all demo configurations, verifying expected output.
# Each test compiles source files from 1-4 languages through their bridges,
# links into a single native binary via MLIR, and checks the result.
#
# Prerequisites:
#   - GHC 9.14.1 at /usr/lib64/ghc-9.14.1/bin/ghc
#   - cabal 3.16+ at ~/.ghcup/bin/cabal-3.16.1.0
#   - mmc (Mercury rotd-2024-06-15)
#   - rustc nightly (for MIR output)
#   - MLIR tools: mlir-opt, mlir-translate
#   - clang

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
BUILD_DIR="$SCRIPT_DIR/build"
PASS=0
FAIL=0
SKIP=0

cd "$PROJECT_DIR"

# Clean build directory and stale Mercury temp files
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"
rm -rf /tmp/frankenstein-mercury-*/

echo "========================================"
echo " Frankenstein Polyglot Test Suite"
echo "========================================"
echo ""

run_test() {
    local name="$1"
    local expected="$2"
    shift 2
    local files=("$@")
    local output_bin="$BUILD_DIR/${name}"

    printf "  %-40s" "$name"

    # Compile
    local compile_output
    compile_output=$($FRANK "${files[@]}" --compile -o "$output_bin" 2>&1)
    local compile_rc=$?

    if [ $compile_rc -ne 0 ]; then
        echo "FAIL (compile)"
        echo "    $compile_output" | head -5
        FAIL=$((FAIL + 1))
        return
    fi

    # Run
    if [ ! -x "$output_bin" ]; then
        echo "FAIL (no binary)"
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

    # Verify
    if [ "$actual" = "$expected" ]; then
        echo "PASS  ($actual)"
        PASS=$((PASS + 1))
    else
        echo "FAIL  (expected $expected, got $actual)"
        FAIL=$((FAIL + 1))
    fi
}

# -------------------------------------------------------
# Test 1: Built-in factorial demo (single language, Koka)
# -------------------------------------------------------
echo "--- Single language ---"

printf "  %-40s" "factorial-demo"
compile_output=$($FRANK --demo --compile -o "$BUILD_DIR/factorial-demo" 2>&1)
if [ $? -eq 0 ] && [ -x "$BUILD_DIR/factorial-demo" ]; then
    actual=$("$BUILD_DIR/factorial-demo" 2>&1)
    if [ "$actual" = "3628800" ]; then
        echo "PASS  (3628800)"
        PASS=$((PASS + 1))
    else
        echo "FAIL  (expected 3628800, got $actual)"
        FAIL=$((FAIL + 1))
    fi
else
    echo "FAIL (compile)"
    FAIL=$((FAIL + 1))
fi

# -------------------------------------------------------
# Test 2: 3-language demo (Haskell + Rust + Koka)
# -------------------------------------------------------
echo ""
echo "--- Multi-language (pure functions) ---"

run_test "3-lang-haskell-rust-koka" "69" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/double.rs" \
    "$SCRIPT_DIR/three-lang.kk"

# -------------------------------------------------------
# Tests 3-4: 4-language semidet (exn effect)
# -------------------------------------------------------
echo ""
echo "--- 4-language with semidet effect (exn) ---"

run_test "4-lang-semidet-success" "69" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/double.rs" \
    "$SCRIPT_DIR/check.m" \
    "$SCRIPT_DIR/four-lang.kk"

# Clean Mercury temp between runs
rm -rf /tmp/frankenstein-mercury-*/

run_test "4-lang-semidet-failure" "1" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/double.rs" \
    "$SCRIPT_DIR/check.m" \
    "$SCRIPT_DIR/four-lang-fail.kk"

# -------------------------------------------------------
# Test 5: 4-language choice (multi-shot effect)
# -------------------------------------------------------
echo ""
echo "--- 4-language with choice effect (multi) ---"

# Clean Mercury temp between runs
rm -rf /tmp/frankenstein-mercury-*/

run_test "4-lang-choice-multi" "144" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/double.rs" \
    "$SCRIPT_DIR/pick.m" \
    "$SCRIPT_DIR/four-lang-choice.kk"

# -------------------------------------------------------
# Test 6: Cross-language Haskell multi-module (2 .hs + 1 .kk)
# -------------------------------------------------------
echo ""
echo "--- Cross-language multi-module (Haskell×2 + Koka) ---"

run_test "cross-lang-haskell-multi" "75" \
    "$SCRIPT_DIR/CrossModuleMain.hs" \
    "$SCRIPT_DIR/cross-lang.kk"

# -------------------------------------------------------
# Test 7: Haskell stdlib (map/filter/sum) called from Koka
# -------------------------------------------------------
echo ""
echo "--- Haskell stdlib + Koka ---"

run_test "stdlib-cross-lang" "220" \
    "$SCRIPT_DIR/StdlibHaskell.hs" \
    "$SCRIPT_DIR/stdlib-cross.kk"

# -------------------------------------------------------
# Test 8: 7-language demo (Haskell + Rust + Mercury + Python + Go + Futhark + Koka)
# -------------------------------------------------------
echo ""
echo "--- 7-language (all bridges) ---"

# Clean Mercury temp between runs
rm -rf /tmp/frankenstein-mercury-*/

run_test "7-lang-all-bridges" "147" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/double.rs" \
    "$SCRIPT_DIR/check.m" \
    "$SCRIPT_DIR/square.py" \
    "$SCRIPT_DIR/gcd.go" \
    "$SCRIPT_DIR/sum_to.fut" \
    "$SCRIPT_DIR/seven-lang.kk"

# -------------------------------------------------------
# Test 9: 7-language multi-module demo (multi-module Haskell + 5 other languages + Koka)
# -------------------------------------------------------
echo ""
echo "--- 7-language multi-module (Haskell×2 + Rust + Mercury + Python + Go + Futhark + Koka) ---"

# Clean Mercury temp between runs
rm -rf /tmp/frankenstein-mercury-*/

run_test "7-lang-multi-module" "175" \
    "$SCRIPT_DIR/CrossModuleMain.hs" \
    "$SCRIPT_DIR/negate.rs" \
    "$SCRIPT_DIR/check.m" \
    "$SCRIPT_DIR/square.py" \
    "$SCRIPT_DIR/gcd.go" \
    "$SCRIPT_DIR/sum_to.fut" \
    "$SCRIPT_DIR/seven-lang-multi.kk"

# -------------------------------------------------------
# Test 10: 12-language demo (all direct-style in-tree bridges)
# -------------------------------------------------------
echo ""
echo "--- 12-language (Haskell + Rust + Mercury + Python + Go + Futhark + Swift + OCaml + Erlang + F# + Idris + Koka) ---"

# Clean Mercury temp between runs
rm -rf /tmp/frankenstein-mercury-*/

run_test "12-lang-all-bridges" "440" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/negate.rs" \
    "$SCRIPT_DIR/check.m" \
    "$SCRIPT_DIR/square.py" \
    "$SCRIPT_DIR/gcd.go" \
    "$SCRIPT_DIR/sum_to.fut" \
    "$SCRIPT_DIR/pow4.swift" \
    "$SCRIPT_DIR/halve.ml" \
    "$SCRIPT_DIR/modulo.erl" \
    "$SCRIPT_DIR/abs_val.fsx" \
    "$SCRIPT_DIR/clamp.idr" \
    "$SCRIPT_DIR/twelve-lang.kk"

# -------------------------------------------------------
# Test 11: Haskell foreign import ccall (FFI cross-language)
# -------------------------------------------------------
echo ""
echo "--- Haskell foreign import ccall (Haskell FFI → Python + Go + Koka) ---"

run_test "haskell-ffi-cross" "157" \
    "$SCRIPT_DIR/FfiImport.hs" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/square.py" \
    "$SCRIPT_DIR/gcd.go" \
    "$SCRIPT_DIR/ffi-cross.kk"

# -------------------------------------------------------
# Test 12: Rust extern "C" (FFI cross-language)
# -------------------------------------------------------
echo ""
echo "--- Rust extern \"C\" (Rust FFI → Python + Haskell + Koka) ---"

run_test "rust-extern-cross" "69" \
    "$SCRIPT_DIR/RustExtern.rs" \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/square.py" \
    "$SCRIPT_DIR/rust-extern-cross.kk"

# -------------------------------------------------------
# Summary
# -------------------------------------------------------
echo ""
echo "========================================"
TOTAL=$((PASS + FAIL + SKIP))
echo " Results: $PASS passed, $FAIL failed, $SKIP skipped ($TOTAL total)"
echo "========================================"

if [ $FAIL -gt 0 ]; then
    exit 1
fi
