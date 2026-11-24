#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T1-001] Deploying Frankenstein LiquidProfile SMT-LIB2 Pass ==="

mkdir -p src/Frankenstein/Verify
cp -f /tmp/frankenstein-liquidprofile-staging/src/Frankenstein/Verify/LiquidProfile.hs src/Frankenstein/Verify/
cp -f /tmp/frankenstein-liquidprofile-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-liquidprofile-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T1-001] Deployment complete ==="
