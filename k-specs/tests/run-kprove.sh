#!/usr/bin/env bash
# run-kprove.sh — Verify Perceus claims with kprove (Haskell backend)
#
# kprove requires a definition compiled with --backend haskell.
# This script will compile one if it does not exist, then run kprove.
#
# Usage:
#   cd k-specs/tests && bash run-kprove.sh
#   -- or --
#   bash k-specs/tests/run-kprove.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SPEC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

KOMPILE="${KOMPILE:-$HOME/src/k/result/bin/kompile}"
KPROVE="${KPROVE:-$HOME/src/k/result/bin/kprove}"

HASKELL_DEF="$SPEC_DIR/organ-ir-haskell-kompiled"
CLAIMS="$SPEC_DIR/perceus-claims.k"
SOURCE="$SPEC_DIR/organ-ir.k"

# --- Sanity checks ---

if [ ! -x "$KPROVE" ]; then
  echo "ERROR: kprove not found at $KPROVE"
  echo "Set KPROVE=/path/to/kprove or install K framework."
  exit 1
fi

echo "Using kprove: $KPROVE"
"$KPROVE" --version 2>/dev/null || echo "(version check failed, continuing anyway)"
echo ""

# --- Compile with Haskell backend if needed ---

if [ ! -d "$HASKELL_DEF" ] || [ "$SOURCE" -nt "$HASKELL_DEF/timestamp" ] 2>/dev/null; then
  echo "=== Compiling organ-ir.k with Haskell backend ==="
  echo "This may take several minutes on first run..."
  echo ""
  "$KOMPILE" "$SOURCE" --backend haskell -d "$HASKELL_DEF"
  echo "Compilation complete."
  echo ""
else
  echo "=== Using existing Haskell-backend definition ==="
  echo "  $HASKELL_DEF"
  echo ""
fi

# --- Run kprove ---

echo "=== Running kprove on perceus-claims.k ==="
echo ""

if "$KPROVE" "$CLAIMS" --definition "$HASKELL_DEF"; then
  echo ""
  echo "=== ALL CLAIMS VERIFIED ==="
  exit 0
else
  RC=$?
  echo ""
  echo "=== SOME CLAIMS FAILED (exit code $RC) ==="
  echo ""
  echo "To debug individual claims, try:"
  echo "  $KPROVE $CLAIMS --definition $HASKELL_DEF --debugger"
  echo ""
  echo "Or verify claims one at a time by commenting out others."
  exit $RC
fi
