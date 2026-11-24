#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T2-001] Deploying OpenRecordSig ==="

mkdir -p src/Frankenstein/Backpack
cp -f /tmp/frankenstein-track2-staging/src/Frankenstein/Backpack/OpenRecordSig.hs src/Frankenstein/Backpack/
cp -f /tmp/frankenstein-track2-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-track2-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T2-001] Deployment complete ==="
