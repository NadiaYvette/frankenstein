#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T4-001] Deploying Incremental CodeDb ==="

mkdir -p src/Frankenstein/Db
cp -f /tmp/frankenstein-t4-001-staging/src/Frankenstein/Db/Incremental.hs src/Frankenstein/Db/
cp -f /tmp/frankenstein-t4-001-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t4-001-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T4-001] Deployment complete ==="
