#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T5-003] Deploying 15-Language Drivers and Roster15 ==="

mkdir -p drivers src/Frankenstein/Driver

cp -f /tmp/frankenstein-t5-003-staging/drivers/*.dhall drivers/
cp -f /tmp/frankenstein-t5-003-staging/src/Frankenstein/Driver/Roster15.hs src/Frankenstein/Driver/Roster15.hs
cp -f /tmp/frankenstein-t5-003-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t5-003-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T5-003] Deployment complete ==="
