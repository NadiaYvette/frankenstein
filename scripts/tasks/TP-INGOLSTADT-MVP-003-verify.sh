#!/bin/sh
set -e
echo "=== [TP-INGOLSTADT-MVP-003] Running verification gates ==="

cc -std=c11 -Wall -Wextra -Werror -DORGAN_FREESTANDING -c rts/test/test_freestanding_linear.c -I rts/include -o /tmp/test_freestanding_linear.o && ! nm -u /tmp/test_freestanding_linear.o | grep -Eq ' (malloc|free|stg_|rts_|hs_init)$'
cc -std=c11 -Wall -Wextra -Werror -fsanitize=address,undefined -I rts/include rts/test/test_organ_rt.c -o /tmp/test_organ_rt && /tmp/test_organ_rt
cabal --with-compiler=ghc-9.14.1 v2-build lib:frankenstein
cabal --with-compiler=ghc-9.14.1 v2-test frankenstein:test:frankenstein-tests --test-options='-p "Emit.C11"'
if [ -x ./bin/frankenstein-verify ]; then ./bin/frankenstein-verify; fi

echo "=== [TP-INGOLSTADT-MVP-003] Verification passed ==="
