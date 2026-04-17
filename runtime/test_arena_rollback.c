/* Test: arena checkpoint/rollback for abort effect cleanup.
 *
 * Verifies that kk_handler_exec + kk_handler_abort correctly rolls
 * back the arena, reclaiming constructor cells allocated in the
 * abandoned continuation.
 *
 * Build:
 *   cc -o test_arena_rollback test_arena_rollback.c kk_runtime.c \
 *      kk_arena.c kk_cycle.c -I.
 *
 * Expected output:
 *   PASS: abort result is 42
 *   PASS: arena reclaimed (before=..., after=..., delta >= ...)
 */
#include <stdio.h>
#include <stdlib.h>
#include "kk_runtime.h"
#include "kk_arena.h"

/* Body function: allocates many constructor cells, then aborts.
 * Signature matches closure ABI: int64_t (*)(int64_t closure). */
static int64_t abort_body(int64_t closure) {
    (void)closure;
    /* Allocate 1000 constructor cells (each 3 fields = 40 bytes). */
    for (int i = 0; i < 1000; i++) {
        int64_t cell = kk_alloc_con(99, 3);
        kk_set_field(cell, 0, i);
        kk_set_field(cell, 1, i * 2);
        kk_set_field(cell, 2, i * 3);
        (void)cell;
    }
    /* Abort with value 42. */
    kk_handler_abort(0, 42);
    /* Never reached. */
    return 99;
}

int main(void) {
    int passed = 1;

    /* Build a minimal closure for abort_body: field 0 = fptr. */
    int64_t closure = kk_alloc_con(1129074515 /* CLOS */, 1);
    kk_set_field(closure, 0, (int64_t)(void*)abort_body);

    /* Record arena state before handler. */
    int64_t before = kk_arena_bytes_allocated();

    /* Execute with handler — body will abort. */
    int64_t result = kk_handler_exec(0, closure);

    /* Record arena state after handler (should have rolled back). */
    int64_t after = kk_arena_bytes_allocated();

    /* Check result. */
    if (result == 42) {
        printf("PASS: abort result is 42\n");
    } else {
        printf("FAIL: abort result is %ld (expected 42)\n", (long)result);
        passed = 0;
    }

    /* Check arena reclamation.
     * The 1000 cells * 40 bytes = 40000 bytes should have been reclaimed.
     * After rollback, `after` should be <= `before` + small overhead. */
    int64_t delta = after - before;
    if (delta < 1000) {
        printf("PASS: arena reclaimed (before=%ld, after=%ld, delta=%ld)\n",
               (long)before, (long)after, (long)delta);
    } else {
        printf("FAIL: arena NOT reclaimed (before=%ld, after=%ld, delta=%ld, expected < 1000)\n",
               (long)before, (long)after, (long)delta);
        passed = 0;
    }

    return passed ? 0 : 1;
}
