#!/bin/bash
# Frankenstein Phase 1 Simple Demo — 2-language pure function calling
#
# Haskell defines fibonacci :: Int -> Int
# Koka defines main, calls fibonacci(10), prints the result
#
# This is the minimal polyglot test: no effects, no backtracking, just
# cross-language pure function composition through the linker.
#
# Expected output: fibonacci(10) = 55

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
FRANK="cabal -v0 run frankenstein --"
BUILD_DIR="$SCRIPT_DIR/build"

cd "$PROJECT_DIR"

echo "========================================"
echo " Frankenstein Simple Demo (2 languages)"
echo "========================================"
echo ""

# Clean build directory
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# -------------------------------------------------------
# Phase 1: Individual compilation to Core IR
# -------------------------------------------------------
echo "=== Phase 1: Individual bridge compilation ==="
echo ""

echo "[1/2] Haskell: fib.hs (GHC bridge)..."
$FRANK "$SCRIPT_DIR/fib.hs" --emit-core 2>&1 | tee "$BUILD_DIR/fib.core"
echo ""

echo "[2/2] Koka: fib-main.kk (Koka bridge)..."
$FRANK "$SCRIPT_DIR/fib-main.kk" --emit-core 2>&1 | tee "$BUILD_DIR/fib-main.core"
echo ""

# -------------------------------------------------------
# Phase 2: Link + compile to native
# -------------------------------------------------------
echo "=== Phase 2: Link + compile ==="
echo ""

echo "Linking Haskell + Koka and compiling to native..."
$FRANK \
    "$SCRIPT_DIR/fib.hs" \
    "$SCRIPT_DIR/fib-main.kk" \
    --compile -o "$BUILD_DIR/fib-demo" 2>&1

echo ""

# -------------------------------------------------------
# Phase 3: Run and verify
# -------------------------------------------------------
echo "=== Phase 3: Run ==="
echo ""

if [ -x "$BUILD_DIR/fib-demo" ]; then
    OUTPUT=$("$BUILD_DIR/fib-demo" 2>&1)
    echo "Output: $OUTPUT"
    echo ""

    # fibonacci(10) = 55
    EXPECTED="fibonacci(10) = 55"
    if echo "$OUTPUT" | grep -q "55"; then
        echo "=== PASS: fibonacci(10) = 55 ==="
    else
        echo "=== FAIL: Expected '$EXPECTED' but got: $OUTPUT ==="
        exit 1
    fi
else
    echo "ERROR: $BUILD_DIR/fib-demo not found or not executable"
    exit 1
fi

echo ""
echo "========================================"
echo " Simple demo passed!"
echo "========================================"
