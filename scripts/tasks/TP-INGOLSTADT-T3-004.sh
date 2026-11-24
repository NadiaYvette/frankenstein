#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T3-004] Deploying Transform Api ==="

mkdir -p src/Frankenstein/Transform
cp -f /tmp/frankenstein-t3-004-staging/src/Frankenstein/Transform/Api.hs src/Frankenstein/Transform/
cp -f /tmp/frankenstein-t3-004-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t3-004-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T3-004] Deployment complete ==="
