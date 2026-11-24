#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-T5-002] Running verification gates ==="

cc -std=c11 -Wall -Wextra -Werror -fPIC -fvisibility=hidden -shared -I rts/include rts/test/sample_plugin.c -o /tmp/libsample_organ_plugin.so
cc -std=c11 -Wall -Wextra -Werror -I rts/include rts/test/test_organ_dso.c -ldl -o /tmp/test_organ_dso && /tmp/test_organ_dso /tmp/libsample_organ_plugin.so
cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Linker.DsoPlugin"'
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-T5-002] Verification passed ==="
