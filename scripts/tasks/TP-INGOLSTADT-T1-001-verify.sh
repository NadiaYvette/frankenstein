#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T1-001] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "LiquidProfile"'
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T1-001] Verification passed ==="
