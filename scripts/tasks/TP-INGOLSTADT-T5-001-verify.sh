#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T5-001] Running verification gates ==="

cc -std=c11 -Wall -Wextra -Werror -fsanitize=address,undefined -I rts/include rts/test/test_organ_uring_bridge.c -o /tmp/test_organ_uring_bridge && /tmp/test_organ_uring_bridge
cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "RTS.UringDriver"'
test -x ./bin/track5-uring-bench && ./bin/track5-uring-bench
test -f docs/TRACK5_URING_BENCHMARKS.md
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T5-001] Verification passed ==="
