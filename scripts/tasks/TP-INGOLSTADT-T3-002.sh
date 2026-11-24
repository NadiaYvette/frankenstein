#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T3-002] Deploying Style Profile ==="

mkdir -p src/Frankenstein/Style
cp -f /tmp/frankenstein-t3-002-staging/src/Frankenstein/Style/Profile.hs src/Frankenstein/Style/
cp -f /tmp/frankenstein-t3-002-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t3-002-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T3-002] Deployment complete ==="
