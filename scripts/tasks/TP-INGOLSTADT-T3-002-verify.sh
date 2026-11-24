#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T3-002] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='--test-show-details=always -p "Style.Profile"'
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T3-002] Verification passed ==="
