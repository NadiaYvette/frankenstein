#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T2-003] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Backpack.HsigBridge"'
test -x ./bin/track2-bench && ./bin/track2-bench
test -f docs/TRACK2_MODULE_BENCHMARKS.md
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T2-003] Verification passed ==="
