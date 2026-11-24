#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-MVP-004] Deploying Frankenstein PolyglotLink & MVP Demo ==="

mkdir -p src/Frankenstein/Linker bin docs examples/mvp-polyglot
cp -f /tmp/frankenstein-polyglotlink-staging/src/Frankenstein/Linker/PolyglotLink.hs src/Frankenstein/Linker/
cp -f /tmp/frankenstein-polyglotlink-staging/frankenstein.cabal frankenstein.cabal
cp -f /tmp/frankenstein-polyglotlink-staging/test/Main.hs test/Main.hs
cp -f /tmp/frankenstein-polyglotlink-staging/bin/mvp-demo bin/mvp-demo
chmod +x bin/mvp-demo
cp -f /tmp/frankenstein-polyglotlink-staging/examples/mvp-polyglot/* examples/mvp-polyglot/

echo "=== [TP-INGOLSTADT-MVP-004] Deployment complete ==="
