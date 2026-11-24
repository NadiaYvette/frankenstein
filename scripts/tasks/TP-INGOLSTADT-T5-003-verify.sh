#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T5-003] Running verification gates ==="

test $(ls drivers/*.dhall | wc -l) -ge 15
for f in drivers/*.dhall; do dhall type --file "$f" >/dev/null; done
cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Driver.Roster15"'
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T5-003] Verification passed ==="
