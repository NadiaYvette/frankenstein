#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T2-002] Deploying StageInstantiate ==="

mkdir -p src/Frankenstein/Backpack
cp -f /tmp/frankenstein-t2-002-staging/src/Frankenstein/Backpack/StageInstantiate.hs src/Frankenstein/Backpack/
cp -f /tmp/frankenstein-t2-002-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t2-002-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T2-002] Deployment complete ==="
