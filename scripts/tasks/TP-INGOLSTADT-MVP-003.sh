#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-MVP-003] Deploying Frankenstein C11 Runtime and Emit ==="

mkdir -p rts/include rts/test src/Frankenstein/Emit
cp -f /tmp/frankenstein-organrt-staging/rts/include/organ_rt.h rts/include/
cp -f /tmp/frankenstein-organrt-staging/rts/test/test_freestanding_linear.c rts/test/
cp -f /tmp/frankenstein-organrt-staging/rts/test/test_organ_rt.c rts/test/
cp -f /tmp/frankenstein-organrt-staging/src/Frankenstein/Emit/C11.hs src/Frankenstein/Emit/
cp -f /tmp/frankenstein-organrt-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-organrt-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-MVP-003] Deployment complete ==="
