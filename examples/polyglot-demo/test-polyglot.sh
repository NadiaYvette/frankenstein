#!/bin/bash
# Frankenstein Phase 1 Polyglot Demo — Full 4-language test
#
# Compiles Haskell + Mercury + Rust + Koka through their respective bridges,
# links them into a single binary, and runs the result.
#
# Prerequisites:
#   - GHC 9.14.1 (for Haskell bridge)
#   - mmc (Mercury rotd-2024-06-15, for Mercury bridge)
#   - rustc nightly (for Rust bridge / MIR)
#   - Koka compiler (for Koka bridge)
#   - MLIR tools: mlir-opt, mlir-translate
#   - clang (for final native linking)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
FRANK="cabal -v0 run frankenstein --"
BUILD_DIR="$SCRIPT_DIR/build"

cd "$PROJECT_DIR"

echo "========================================"
echo " Frankenstein Polyglot Demo (4 languages)"
echo "========================================"
echo ""

# Clean build directory
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

# -------------------------------------------------------
# Phase 1: Individual bridge compilation (--emit-core)
# -------------------------------------------------------
echo "=== Phase 1: Individual bridge compilation ==="
echo ""

echo "[1/4] Haskell (GHC bridge)..."
$FRANK "$SCRIPT_DIR/Scoring.hs" --emit-core > "$BUILD_DIR/scoring.core" 2>&1 || {
    echo "  WARN: GHC bridge failed (expected if GHC API not available)"
    echo "  Continuing with demo fallback..."
}
echo "  -> $BUILD_DIR/scoring.core"

echo "[2/4] Mercury (HLDS bridge)..."
$FRANK "$SCRIPT_DIR/search.m" --emit-core > "$BUILD_DIR/search.core" 2>&1 || {
    echo "  WARN: Mercury bridge failed (expected if mmc not available)"
    echo "  Continuing with demo fallback..."
}
echo "  -> $BUILD_DIR/search.core"

echo "[3/4] Rust (MIR bridge)..."
$FRANK "$SCRIPT_DIR/accumulate.rs" --emit-core > "$BUILD_DIR/accumulate.core" 2>&1 || {
    echo "  WARN: Rust bridge failed (expected if rustc nightly not available)"
    echo "  Continuing with demo fallback..."
}
echo "  -> $BUILD_DIR/accumulate.core"

echo "[4/4] Koka (Koka bridge)..."
$FRANK "$SCRIPT_DIR/main.kk" --emit-core > "$BUILD_DIR/main.core" 2>&1 || {
    echo "  WARN: Koka bridge failed (expected if Koka library not linked)"
    echo "  Continuing with demo fallback..."
}
echo "  -> $BUILD_DIR/main.core"

echo ""
echo "Individual core files:"
ls -la "$BUILD_DIR"/*.core 2>/dev/null || echo "  (no core files generated)"
echo ""

# -------------------------------------------------------
# Phase 2: Polyglot link + compile to native
# -------------------------------------------------------
echo "=== Phase 2: Polyglot link + compile ==="
echo ""

echo "Linking 4 modules and compiling to native..."
$FRANK \
    "$SCRIPT_DIR/Scoring.hs" \
    "$SCRIPT_DIR/search.m" \
    "$SCRIPT_DIR/accumulate.rs" \
    "$SCRIPT_DIR/main.kk" \
    --compile -o "$BUILD_DIR/polyglot-demo" 2>&1 || {
    echo ""
    echo "WARN: Full polyglot compilation failed."
    echo "This is expected if effect codegen is not yet complete."
    echo "Try simple-demo.sh for a 2-language pure-function demo."
    echo ""
    exit 1
}

echo ""

# -------------------------------------------------------
# Phase 3: Run and verify
# -------------------------------------------------------
echo "=== Phase 3: Run ==="
echo ""

if [ -x "$BUILD_DIR/polyglot-demo" ]; then
    OUTPUT=$("$BUILD_DIR/polyglot-demo" 2>&1)
    echo "Output:"
    echo "$OUTPUT"
    echo ""

    # Check for expected output patterns
    if echo "$OUTPUT" | grep -q "Total score:"; then
        echo "=== PASS: Output contains total score ==="
    else
        echo "=== FAIL: Expected 'Total score:' in output ==="
        exit 1
    fi

    if echo "$OUTPUT" | grep -q "4 languages, 1 binary"; then
        echo "=== PASS: Polyglot banner present ==="
    else
        echo "=== WARN: Banner line not found (non-fatal) ==="
    fi
else
    echo "ERROR: $BUILD_DIR/polyglot-demo not found or not executable"
    exit 1
fi

echo ""
echo "========================================"
echo " All checks passed!"
echo "========================================"
