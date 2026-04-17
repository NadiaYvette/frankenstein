/* Test: abort result rescue from arena rollback region.
 *
 * Verifies that when an abort handler returns a heap-allocated value
 * (constructor cell), it is rescued from the arena before rollback
 * so the caller can still use it.
 *
 * Build:
 *   cc -o test_arena_rescue test_arena_rescue.c kk_runtime.c \
 *      kk_arena.c kk_cycle.c -I.
 *
 * Expected output:
 *   PASS: rescued result tag=7, field0=100, field1=200
 *   PASS: arena reclaimed
 */
#include <stdio.h>
#include "kk_runtime.h"
#include "kk_arena.h"

/* Body function: allocates garbage, then aborts with a heap value. */
static int64_t abort_body_heap(int64_t closure) {
    (void)closure;
    /* Allocate garbage that should be reclaimed. */
    for (int i = 0; i < 500; i++) {
        kk_alloc_con(0, 2);
    }
    /* Build the abort result: a 2-field constructor. */
    int64_t result = kk_alloc_con(7, 2);
    kk_set_field(result, 0, 100);
    kk_set_field(result, 1, 200);
    kk_handler_abort(0, result);
    return 0;  /* unreachable */
}

int main(void) {
    int passed = 1;

    int64_t closure = kk_alloc_con(1129074515, 1);
    kk_set_field(closure, 0, (int64_t)(void*)abort_body_heap);

    int64_t before = kk_arena_bytes_allocated();
    int64_t result = kk_handler_exec(0, closure);
    int64_t after = kk_arena_bytes_allocated();

    /* Check the rescued result is still valid. */
    if (kk_is_heap_ptr(result) && kk_tag(result) == 7
        && kk_field(result, 0) == 100 && kk_field(result, 1) == 200) {
        printf("PASS: rescued result tag=7, field0=100, field1=200\n");
    } else {
        printf("FAIL: result not valid (ptr=%ld, tag=%ld)\n",
               (long)result, kk_is_heap_ptr(result) ? (long)kk_tag(result) : -1);
        passed = 0;
    }

    int64_t delta = after - before;
    if (delta < 1000) {
        printf("PASS: arena reclaimed (delta=%ld)\n", (long)delta);
    } else {
        printf("FAIL: arena NOT reclaimed (delta=%ld)\n", (long)delta);
        passed = 0;
    }

    return passed ? 0 : 1;
}
