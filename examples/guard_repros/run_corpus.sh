#!/usr/bin/env bash
# Run the guard-shape repro corpus.
#
# For each .hs file in this directory:
#   1. Host CLI emits MLIR directly (control).
#   2. Host CLI emits .organ.json (input to stage 1).
#   3. Stage 1 binary consumes .organ.json + emits MLIR (test).
#   4. Diff the two MLIR outputs, report whether they agree.
#
# Expectation: 21/21 E2E PASS but some MLIR diffs reveal which pattern
# class trips the self-host divergence.

set -e
cd "$(dirname "$0")/../.."  # repo root

FRKN_RUN="cabal-3.16.1.0 -v0 exec -w /usr/lib64/ghc-9.14.1/bin/ghc frankenstein --"
STAGE1=./self-host/frankenstein-self-compiler

if [ ! -x "$STAGE1" ]; then
  echo "Stage 1 binary missing: $STAGE1"
  echo "Run: bash self-host/build.sh"
  exit 1
fi

OUT=/tmp/guard_corpus
rm -rf "$OUT"
mkdir -p "$OUT"

echo "=== Guard-shape corpus repro ==="
echo

PASS_AGREES=0
FAIL_DIVERGES=0
FAIL_OTHER=0

for hs in examples/guard_repros/*.hs; do
  name=$(basename "$hs" .hs)
  echo -n "  $name: "

  # 1. Host emit
  if ! $FRKN_RUN "$hs" --no-simplify --evidence=plotkin --emit-mlir \
       > "$OUT/$name-host.mlir" 2>"$OUT/$name-host.err"; then
    echo "FAIL (host emit)"
    FAIL_OTHER=$((FAIL_OTHER + 1))
    continue
  fi

  # 2. Host organ.json (pre-passes)
  if ! $FRKN_RUN "$hs" --no-simplify --emit-organ \
       > "$OUT/$name.organ.json" 2>"$OUT/$name-organ.err"; then
    echo "FAIL (host organ)"
    FAIL_OTHER=$((FAIL_OTHER + 1))
    continue
  fi

  # 3. Stage 1 binary emit
  if ! cat "$OUT/$name.organ.json" | $STAGE1 - -o "$OUT/$name-stage1.mlir" \
       2>"$OUT/$name-stage1.err"; then
    echo "FAIL (stage 1 emit)"
    FAIL_OTHER=$((FAIL_OTHER + 1))
    continue
  fi

  # 4. Diff
  if cmp -s "$OUT/$name-host.mlir" "$OUT/$name-stage1.mlir"; then
    echo "AGREE"
    PASS_AGREES=$((PASS_AGREES + 1))
  else
    diff_lines=$(diff "$OUT/$name-host.mlir" "$OUT/$name-stage1.mlir" | wc -l)
    echo "DIVERGE ($diff_lines diff lines)"
    FAIL_DIVERGES=$((FAIL_DIVERGES + 1))
  fi
done

echo
echo "=== Summary ==="
echo "  Agree:    $PASS_AGREES"
echo "  Diverge:  $FAIL_DIVERGES"
echo "  Other:    $FAIL_OTHER"
echo
echo "MLIR outputs in $OUT/"
