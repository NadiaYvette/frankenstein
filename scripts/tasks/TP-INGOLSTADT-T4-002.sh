#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T4-002] Deploying Resilient EditCone ==="

mkdir -p src/Frankenstein/Db
cp -f /tmp/frankenstein-t4-002-staging/src/Frankenstein/Db/ResilientEditCone.hs src/Frankenstein/Db/
cp -f /tmp/frankenstein-t4-002-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t4-002-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T4-002] Deployment complete ==="
