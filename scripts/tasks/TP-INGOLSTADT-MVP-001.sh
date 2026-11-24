#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-MVP-001] Deploying Frankenstein.Core.Interface ==="

mkdir -p src/Frankenstein/Core test
cp -f /tmp/frankenstein-interface-staging/src/Frankenstein/Core/Interface.hs src/Frankenstein/Core/
cp -f /tmp/frankenstein-interface-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-interface-staging/test/Main.hs test/

echo "=== [TP-INGOLSTADT-MVP-001] Deployment complete ==="
