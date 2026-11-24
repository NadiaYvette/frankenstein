#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T2-003] Deploying HsigBridge and track2-bench ==="

mkdir -p src/Frankenstein/Backpack test/fixtures/hsig bin docs
cp -f /tmp/frankenstein-t2-003-staging/src/Frankenstein/Backpack/HsigBridge.hs src/Frankenstein/Backpack/
cp -f /tmp/frankenstein-t2-003-staging/test/fixtures/hsig/*.hsig test/fixtures/hsig/
cp -f /tmp/frankenstein-t2-003-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t2-003-staging/test/Main.hs test/Main.hs
cp -f /tmp/frankenstein-t2-003-staging/bin/track2-bench bin/track2-bench
chmod +x bin/track2-bench

echo "=== [TP-INGOLSTADT-T2-003] Deployment complete ==="
