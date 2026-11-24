#!/bin/sh
set -e
cc -std=c11 -Wall -Wextra -Werror -I rts/include rts/test/test_organ_uring_rts.c -o /tmp/test_organ_uring_rts
/tmp/test_organ_uring_rts
cabal v2-test funnel-tests -j20 --semaphore
echo "TP-FRANKENSTEIN-URING-RTS-001 verification passed!"
