#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007E-verify] Verifying frankenstein funnel ==="
cabal v2-test frankenstein:test:funnel-tests
echo "=== [TP-007E-verify] Verification complete ==="
