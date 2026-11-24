#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-MVP-004] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "PolyglotLink"'
test -x ./bin/mvp-demo && ./bin/mvp-demo
test -f docs/MVP_BENCHMARKS.md
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-MVP-004] Verification passed ==="
