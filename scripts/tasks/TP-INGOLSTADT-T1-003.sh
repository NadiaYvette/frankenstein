#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T1-003] Deploying SynquidHole and Track 1 Benchmarks ==="

mkdir -p src/Frankenstein/Verify bin docs
cp -f /tmp/frankenstein-track1-staging/src/Frankenstein/Verify/SynquidHole.hs src/Frankenstein/Verify/
cp -f /tmp/frankenstein-track1-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-track1-staging/test/Main.hs test/Main.hs
cp -f /tmp/frankenstein-track1-staging/bin/track1-bench bin/track1-bench
chmod +x bin/track1-bench

echo "=== [TP-INGOLSTADT-T1-003] Deployment complete ==="
