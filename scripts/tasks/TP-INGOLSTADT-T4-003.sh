#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T4-003] Deploying LiveSession and Benchmark ==="

mkdir -p src/Frankenstein/Ide bin docs
cp -f /tmp/frankenstein-t4-003-staging/src/Frankenstein/Ide/LiveSession.hs src/Frankenstein/Ide/
cp -f /tmp/frankenstein-t4-003-staging/bin/track4-bench bin/track4-bench
chmod +x bin/track4-bench
cp -f /tmp/frankenstein-t4-003-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t4-003-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T4-003] Deployment complete ==="
