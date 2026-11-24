#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-MVP-002] Deploying Frankenstein.GhcBridge.Plugin ==="

mkdir -p src/Frankenstein/GhcBridge test/fixtures/mvp-haskell
cp -f /tmp/frankenstein-plugin-staging/src/Frankenstein/GhcBridge/Plugin.hs src/Frankenstein/GhcBridge/
cp -f /tmp/frankenstein-plugin-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-plugin-staging/test/Main.hs test/
cp -rf /tmp/frankenstein-plugin-staging/test/fixtures/mvp-haskell/* test/fixtures/mvp-haskell/

echo "=== [TP-INGOLSTADT-MVP-002] Deployment complete ==="
