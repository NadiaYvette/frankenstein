#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T1-002] Deploying Frankenstein Opt.EGraph Rewrite Kernel ==="

mkdir -p src/Frankenstein/Opt
cp -f /tmp/frankenstein-egraph-staging/src/Frankenstein/Opt/EGraph.hs src/Frankenstein/Opt/
cp -f /tmp/frankenstein-egraph-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-egraph-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T1-002] Deployment complete ==="
