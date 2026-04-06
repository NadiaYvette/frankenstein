#!/usr/bin/env bash
# run-kprove.sh — Verify all kprove claims (Haskell backend)
#
# kprove requires a definition compiled with --backend haskell.
# This script compiles two definitions if needed:
#   1. organ-ir-haskell-kompiled (for perceus-claims.k)
#   2. all-claims-haskell-kompiled (for bridge, evidence, linker claims)
#
# Usage:
#   cd k-specs/tests && bash run-kprove.sh
#   -- or --
#   bash k-specs/tests/run-kprove.sh
#   bash k-specs/tests/run-kprove.sh perceus       # run only perceus claims
#   bash k-specs/tests/run-kprove.sh bridge         # run only bridge claims
#   bash k-specs/tests/run-kprove.sh evidence       # run only evidence claims
#   bash k-specs/tests/run-kprove.sh linker         # run only linker claims

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SPEC_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

KOMPILE="${KOMPILE:-$HOME/src/k/result/bin/kompile}"
KPROVE="${KPROVE:-$HOME/src/k/result/bin/kprove}"

# Definitions
PERCEUS_DEF="$SPEC_DIR/organ-ir-haskell-kompiled"
ALL_CLAIMS_DEF="$SPEC_DIR/all-claims-haskell-kompiled"

# Source files
ORGAN_IR="$SPEC_DIR/organ-ir.k"
ALL_CLAIMS_SRC="$SPEC_DIR/all-claims-def.k"
BRIDGE_PROPS="$SPEC_DIR/bridge-properties.k"

# Claim files
PERCEUS_CLAIMS="$SPEC_DIR/perceus-claims.k"
BRIDGE_CLAIMS="$SPEC_DIR/bridge-claims.k"
EVIDENCE_CLAIMS="$SPEC_DIR/evidence-claims.k"
LINKER_CLAIMS="$SPEC_DIR/linker-claims.k"

# --- Sanity checks ---

if [ ! -x "$KPROVE" ]; then
  echo "ERROR: kprove not found at $KPROVE"
  echo "Set KPROVE=/path/to/kprove or install K framework."
  exit 1
fi

echo "Using kprove: $KPROVE"
"$KPROVE" --version 2>/dev/null || echo "(version check failed, continuing anyway)"
echo ""

# --- Helper: compile a definition if needed ---

compile_def() {
  local DEF_DIR="$1"
  local SRC_FILE="$2"
  local MAIN_MOD="$3"
  local SYNTAX_MOD="$4"
  shift 4
  local DEPS=("$@")

  local NEEDS_RECOMPILE=false
  if [ ! -d "$DEF_DIR" ]; then
    NEEDS_RECOMPILE=true
  elif [ "$SRC_FILE" -nt "$DEF_DIR/timestamp" ] 2>/dev/null; then
    NEEDS_RECOMPILE=true
  else
    for dep in "${DEPS[@]}"; do
      if [ "$dep" -nt "$DEF_DIR/timestamp" ] 2>/dev/null; then
        NEEDS_RECOMPILE=true
        break
      fi
    done
  fi

  if [ "$NEEDS_RECOMPILE" = true ]; then
    echo "=== Compiling $SRC_FILE with Haskell backend ==="
    echo "This may take several minutes..."
    echo ""
    "$KOMPILE" "$SRC_FILE" --backend haskell \
      --main-module "$MAIN_MOD" --syntax-module "$SYNTAX_MOD" \
      -o "$DEF_DIR"
    echo "Compilation complete: $DEF_DIR"
    echo ""
  else
    echo "=== Using existing definition: $DEF_DIR ==="
    echo ""
  fi
}

# --- Determine which claim files to run ---

FILTER="${1:-all}"
FAILED=0
PASSED=0

run_claims() {
  local label="$1"
  local file="$2"
  local def="$3"

  if [ ! -f "$file" ]; then
    echo "WARNING: $file not found, skipping"
    return
  fi

  echo "=== Running kprove on $label ==="
  echo ""

  if "$KPROVE" "$file" --definition "$def"; then
    echo ""
    echo "  $label: ALL CLAIMS VERIFIED"
    echo ""
    PASSED=$((PASSED + 1))
  else
    echo ""
    echo "  $label: SOME CLAIMS FAILED"
    echo ""
    FAILED=$((FAILED + 1))
  fi
}

# --- Compile definitions as needed ---

case "$FILTER" in
  perceus)
    compile_def "$PERCEUS_DEF" "$ORGAN_IR" "ORGAN-IR" "ORGAN-IR-SYNTAX"
    run_claims "perceus-claims.k (20 claims)" "$PERCEUS_CLAIMS" "$PERCEUS_DEF"
    ;;
  bridge|evidence|linker)
    compile_def "$ALL_CLAIMS_DEF" "$ALL_CLAIMS_SRC" "ALL-CLAIMS-DEF" "ALL-CLAIMS-DEF-SYNTAX" "$ORGAN_IR" "$BRIDGE_PROPS"
    case "$FILTER" in
      bridge)   run_claims "bridge-claims.k (67 claims)" "$BRIDGE_CLAIMS" "$ALL_CLAIMS_DEF" ;;
      evidence) run_claims "evidence-claims.k (15 claims)" "$EVIDENCE_CLAIMS" "$ALL_CLAIMS_DEF" ;;
      linker)   run_claims "linker-claims.k (18 claims)" "$LINKER_CLAIMS" "$ALL_CLAIMS_DEF" ;;
    esac
    ;;
  all)
    compile_def "$PERCEUS_DEF" "$ORGAN_IR" "ORGAN-IR" "ORGAN-IR-SYNTAX"
    compile_def "$ALL_CLAIMS_DEF" "$ALL_CLAIMS_SRC" "ALL-CLAIMS-DEF" "ALL-CLAIMS-DEF-SYNTAX" "$ORGAN_IR" "$BRIDGE_PROPS"
    run_claims "perceus-claims.k (20 claims)" "$PERCEUS_CLAIMS" "$PERCEUS_DEF"
    run_claims "bridge-claims.k (67 claims)" "$BRIDGE_CLAIMS" "$ALL_CLAIMS_DEF"
    run_claims "evidence-claims.k (15 claims)" "$EVIDENCE_CLAIMS" "$ALL_CLAIMS_DEF"
    run_claims "linker-claims.k (18 claims)" "$LINKER_CLAIMS" "$ALL_CLAIMS_DEF"
    ;;
  *)
    echo "Unknown filter: $FILTER"
    echo "Usage: $0 [perceus|bridge|evidence|linker|all]"
    exit 1
    ;;
esac

# --- Summary ---

TOTAL=$((PASSED + FAILED))
echo "==========================================="
echo "  PASSED: $PASSED / $TOTAL  FAILED: $FAILED"
echo "==========================================="

if [ "$FAILED" -gt 0 ]; then
  echo ""
  echo "To debug individual claims:"
  echo "  $KPROVE <claims-file> --definition <def-dir> --debugger"
  exit 1
fi

exit 0
