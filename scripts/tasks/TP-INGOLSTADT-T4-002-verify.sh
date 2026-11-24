#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T4-002] Running verification gates ==="

cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Db.ResilientEditCone"'
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T4-002] Verification passed ==="
