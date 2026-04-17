/* Test: deep rescue of nested heap values from arena rollback region.
 *
 * Verifies that when an abort handler returns a constructor cell whose
 * fields themselves point to arena-allocated constructor cells, the
 * entire transitive closure is rescued (deep-copied to malloc) before
 * rollback, so all nested values remain valid.
 *
 * Build:
 *   cc -o test_deep_rescue test_deep_rescue.c kk_runtime.c \
 *      kk_arena.c kk_cycle.c -I.
 *
 * Expected output:
 *   PASS: outer tag=1, nfields=2
 *   PASS: inner_a tag=2, field0=111
 *   PASS: inner_b tag=3, field0=222, field1=333
 *   PASS: arena reclaimed
 */
#include <stdio.h>
#include "kk_runtime.h"
#include "kk_arena.h"

/* Body function: allocates garbage, then aborts with a nested structure.
 *
 * Result structure:
 *   outer (tag=1, 2 fields) → [ inner_a (tag=2, 1 field) → [111],
 *                                inner_b (tag=3, 2 fields) → [222, 333] ]
 *
 * All three cells are arena-allocated. Deep rescue must copy all of them. */
static int64_t abort_body_nested(int64_t closure) {
    (void)closure;
    /* Allocate garbage that should be reclaimed. */
    for (int i = 0; i < 200; i++) {
        kk_alloc_con(99, 1);
    }
    /* Build nested abort result. */
    int64_t inner_a = kk_alloc_con(2, 1);
    kk_set_field(inner_a, 0, 111);

    int64_t inner_b = kk_alloc_con(3, 2);
    kk_set_field(inner_b, 0, 222);
    kk_set_field(inner_b, 1, 333);

    int64_t outer = kk_alloc_con(1, 2);
    kk_set_field(outer, 0, inner_a);
    kk_set_field(outer, 1, inner_b);

    kk_handler_abort(0, outer);
    return 0;  /* unreachable */
}

int main(void) {
    int passed = 1;

    int64_t closure = kk_alloc_con(1129074515, 1);
    kk_set_field(closure, 0, (int64_t)(void*)abort_body_nested);

    int64_t before = kk_arena_bytes_allocated();
    int64_t result = kk_handler_exec(0, closure);
    int64_t after = kk_arena_bytes_allocated();

    /* Check outer cell. */
    if (kk_is_heap_ptr(result) && kk_tag(result) == 1 && kk_nfields(result) == 2) {
        printf("PASS: outer tag=1, nfields=2\n");
    } else {
        printf("FAIL: outer not valid (ptr=%ld, tag=%ld)\n",
               (long)result, kk_is_heap_ptr(result) ? (long)kk_tag(result) : -1);
        passed = 0;
    }

    /* Check inner_a (field 0 of outer). */
    int64_t inner_a = kk_field(result, 0);
    if (kk_is_heap_ptr(inner_a) && kk_tag(inner_a) == 2 && kk_field(inner_a, 0) == 111) {
        printf("PASS: inner_a tag=2, field0=111\n");
    } else {
        printf("FAIL: inner_a not valid (ptr=%ld, tag=%ld, f0=%ld)\n",
               (long)inner_a,
               kk_is_heap_ptr(inner_a) ? (long)kk_tag(inner_a) : -1,
               kk_is_heap_ptr(inner_a) ? (long)kk_field(inner_a, 0) : -1);
        passed = 0;
    }

    /* Check inner_b (field 1 of outer). */
    int64_t inner_b = kk_field(result, 1);
    if (kk_is_heap_ptr(inner_b) && kk_tag(inner_b) == 3
        && kk_field(inner_b, 0) == 222 && kk_field(inner_b, 1) == 333) {
        printf("PASS: inner_b tag=3, field0=222, field1=333\n");
    } else {
        printf("FAIL: inner_b not valid (ptr=%ld, tag=%ld, f0=%ld, f1=%ld)\n",
               (long)inner_b,
               kk_is_heap_ptr(inner_b) ? (long)kk_tag(inner_b) : -1,
               kk_is_heap_ptr(inner_b) ? (long)kk_field(inner_b, 0) : -1,
               kk_is_heap_ptr(inner_b) ? (long)kk_field(inner_b, 1) : -1);
        passed = 0;
    }

    /* Check arena reclamation (garbage + original arena cells freed). */
    int64_t delta = after - before;
    if (delta < 1000) {
        printf("PASS: arena reclaimed (delta=%ld)\n", (long)delta);
    } else {
        printf("FAIL: arena NOT reclaimed (delta=%ld)\n", (long)delta);
        passed = 0;
    }

    return passed ? 0 : 1;
}
