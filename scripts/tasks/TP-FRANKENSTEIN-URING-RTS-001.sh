#!/bin/sh
set -e
mkdir -p rts/include rts/test src/Frankenstein/RTS test
cp -f /tmp/frankenstein-staging/rts/include/organ_uring_rts.h rts/include/
cp -f /tmp/frankenstein-staging/rts/test/test_organ_uring_rts.c rts/test/
cp -f /tmp/frankenstein-staging/src/Frankenstein/RTS/UringLower.hs src/Frankenstein/RTS/
cp -f /tmp/frankenstein-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-staging/test/FunnelSpec.hs test/FunnelSpec.hs
echo "Staged TP-FRANKENSTEIN-URING-RTS-001 files into worktree"
