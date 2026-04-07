#!/bin/bash
# Build pre-compiled Wasm demos for the browser.
# Run from the frankenstein project root:
#   bash web/build.sh
set -euo pipefail

FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"

echo "Building Wasm demos for browser..."

# Compile demo factorial to Wasm
$FRANK --demo --compile --target wasm32 -o web/factorial.wasm 2>&1

# Show results
ls -la web/factorial.wasm
echo ""
echo "Done. To serve locally:"
echo "  cd web && python3 -m http.server 8080"
echo "  open http://localhost:8080"
