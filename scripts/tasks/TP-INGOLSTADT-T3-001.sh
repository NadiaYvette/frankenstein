#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T3-001] Deploying Driver Registry and Dhall manifests ==="

mkdir -p src/Frankenstein/Driver drivers/schema
cp -f /tmp/frankenstein-t3-001-staging/src/Frankenstein/Driver/Registry.hs src/Frankenstein/Driver/
cp -f /tmp/frankenstein-t3-001-staging/drivers/schema/DriverSchema.dhall drivers/schema/
cp -f /tmp/frankenstein-t3-001-staging/drivers/*.dhall drivers/
cp -f /tmp/frankenstein-t3-001-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t3-001-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T3-001] Deployment complete ==="
