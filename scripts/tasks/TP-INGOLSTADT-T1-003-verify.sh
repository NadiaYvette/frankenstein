#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T1-003] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Verify.SynquidHole"'
test -x ./bin/track1-bench && ./bin/track1-bench
test -f docs/TRACK1_OPT_BENCHMARKS.md
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T1-003] Verification passed ==="
