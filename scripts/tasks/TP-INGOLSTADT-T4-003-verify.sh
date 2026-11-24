#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T4-003] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Ide.LiveSession"'
test -x ./bin/track4-bench && ./bin/track4-bench
test -f docs/TRACK4_IDE_BENCHMARKS.md
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T4-003] Verification passed ==="
