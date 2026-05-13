/* Test: PAP (partially-applied function) round-trip end-to-end.
 *
 * Simulates the HOF scenario the plotkin self-host bootstrap hits.
 * A plotkin'd function `plus_evv(evv, a)` has been transformed to take
 * evv as its first arg. A HOF (a hand-written "map_one" that calls a
 * 1-arg fn with one element) was compiled against the original
 * 1-arg-fn ABI and invokes its callback as `f(self, a)`. Without PAP
 * wrapping, calling plus_evv that way would skip evv and produce
 * garbage.
 *
 * With PAP wrapping:
 *   1. The "compiler" wraps plus_evv in a PAP that pre-supplies evv=0.
 *      The PAP's field-0 trampoline is kk_pap_call_1.
 *   2. The HOF calls field-0 as f(self_pap, a), reaching kk_pap_call_1.
 *   3. kk_pap_call_1 extracts wrapped_fn and evv from self_pap and
 *      calls wrapped_fn(evv, a) — invoking plus_evv with the right
 *      signature.
 *
 * Build (run from runtime/):
 *   cc -o test_pap test_pap.c kk_runtime.c kk_arena.c kk_cycle.c -I.
 *
 * Expected output:
 *   PASS: PAP dispatch through closure ABI invokes wrapped fn -> 42
 *   PASS: PAP with non-zero evv passes the right value through -> 99
 *   PASS: PAP for 2-arg fn dispatches correctly -> 5
 */
#include <stdio.h>
#include "kk_runtime.h"

/* A plotkin'd 1-arg function. Takes (evv, a) and returns a + evv * 100. */
static int64_t plus_evv(int64_t evv, int64_t a) {
    return a + evv * 100;
}

/* A plotkin'd 2-arg function. Takes (evv, a, b) and returns a + b. */
static int64_t plus2_evv(int64_t evv, int64_t a, int64_t b) {
    (void)evv;
    return a + b;
}

/* A HOF that calls a 1-arg-callback closure with `x`. Mirrors what
 * map / fromList / etc. would compile to: load field-0, llvm.call as
 * fn(self, x). */
static int64_t call_with(int64_t callback_closure, int64_t x) {
    int64_t fn_word = kk_field(callback_closure, 0);
    int64_t (*fn)(int64_t, int64_t) =
        (int64_t (*)(int64_t, int64_t))(intptr_t)fn_word;
    return fn(callback_closure, x);
}

/* A HOF that calls a 2-arg-callback closure with (x, y). */
static int64_t call_with_2(int64_t callback_closure, int64_t x, int64_t y) {
    int64_t fn_word = kk_field(callback_closure, 0);
    int64_t (*fn)(int64_t, int64_t, int64_t) =
        (int64_t (*)(int64_t, int64_t, int64_t))(intptr_t)fn_word;
    return fn(callback_closure, x, y);
}

int main(void) {
    int passed = 1;

    /* Case 1: PAP wrapping plus_evv with evv=0, called with a=42.
     * Expected: plus_evv(0, 42) = 42. */
    {
        int64_t pap = kk_pap_alloc(
            (int64_t)(intptr_t)kk_pap_call_1,
            (int64_t)(intptr_t)plus_evv,
            0);
        int64_t result = call_with(pap, 42);
        if (result == 42) {
            printf("PASS: PAP dispatch through closure ABI invokes wrapped fn -> 42\n");
        } else {
            printf("FAIL: expected 42, got %ld\n", (long)result);
            passed = 0;
        }
    }

    /* Case 2: PAP wrapping plus_evv with evv=99, called with a=99-9900.
     * Expected: plus_evv(99, -9801) = -9801 + 99*100 = 99. */
    {
        int64_t pap = kk_pap_alloc(
            (int64_t)(intptr_t)kk_pap_call_1,
            (int64_t)(intptr_t)plus_evv,
            99);
        int64_t result = call_with(pap, -9801);
        if (result == 99) {
            printf("PASS: PAP with non-zero evv passes the right value through -> 99\n");
        } else {
            printf("FAIL: expected 99, got %ld\n", (long)result);
            passed = 0;
        }
    }

    /* Case 3: PAP wrapping plus2_evv with evv=0, called with (2, 3).
     * Expected: plus2_evv(0, 2, 3) = 5. */
    {
        int64_t pap = kk_pap_alloc(
            (int64_t)(intptr_t)kk_pap_call_2,
            (int64_t)(intptr_t)plus2_evv,
            0);
        int64_t result = call_with_2(pap, 2, 3);
        if (result == 5) {
            printf("PASS: PAP for 2-arg fn dispatches correctly -> 5\n");
        } else {
            printf("FAIL: expected 5, got %ld\n", (long)result);
            passed = 0;
        }
    }

    return passed ? 0 : 1;
}
