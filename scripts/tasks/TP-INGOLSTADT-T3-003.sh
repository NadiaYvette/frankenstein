#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T3-003] Deploying Emit Idiomatic ==="

mkdir -p src/Frankenstein/Emit
cp -f /tmp/frankenstein-t3-003-staging/src/Frankenstein/Emit/Idiomatic.hs src/Frankenstein/Emit/
cp -f /tmp/frankenstein-t3-003-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t3-003-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T3-003] Deployment complete ==="
