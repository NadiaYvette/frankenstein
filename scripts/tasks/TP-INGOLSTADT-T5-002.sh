#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T5-002] Deploying DSO-as-Plugin Linker and Tests ==="

mkdir -p rts/include rts/test src/Frankenstein/Linker

cp -f /tmp/frankenstein-t5-002-staging/rts/include/organ_dso.h rts/include/organ_dso.h
cp -f /tmp/frankenstein-t5-002-staging/rts/test/sample_plugin.c rts/test/sample_plugin.c
cp -f /tmp/frankenstein-t5-002-staging/rts/test/test_organ_dso.c rts/test/test_organ_dso.c
cp -f /tmp/frankenstein-t5-002-staging/src/Frankenstein/Linker/DsoPlugin.hs src/Frankenstein/Linker/DsoPlugin.hs
cp -f /tmp/frankenstein-t5-002-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-t5-002-staging/test/Main.hs test/Main.hs

echo "=== [TP-INGOLSTADT-T5-002] Deployment complete ==="
