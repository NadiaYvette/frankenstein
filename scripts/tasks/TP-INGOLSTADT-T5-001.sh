#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T5-001] Deploying io_uring RTS Bridge and Driver ==="

mkdir -p rts/include rts/test src/Frankenstein/RTS bin docs

cp -f /tmp/frankenstein-t5-001-staging/rts/include/organ_uring_bridge.h rts/include/organ_uring_bridge.h
cp -f /tmp/frankenstein-t5-001-staging/rts/test/test_organ_uring_bridge.c rts/test/test_organ_uring_bridge.c
cp -f /tmp/frankenstein-t5-001-staging/rts/test/bench_organ_uring.c rts/test/bench_organ_uring.c
cp -f /tmp/frankenstein-t5-001-staging/src/Frankenstein/RTS/UringDriver.hs src/Frankenstein/RTS/UringDriver.hs
cp -f /tmp/frankenstein-t5-001-staging/bin/track5-uring-bench bin/track5-uring-bench
chmod +x bin/track5-uring-bench
cp -f /tmp/frankenstein-t5-001-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t5-001-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T5-001] Deployment complete ==="
