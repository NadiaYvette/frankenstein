/*
 * test_organ_rt.c — Perceus Profile 1 runtime & first-order lazy constructor test
 * Compiles with -fsanitize=address,undefined.
 */
#include "organ_rt.h"
#include <assert.h>
#include <stdio.h>

static int g_drop_count = 0;

static void test_drop_fields(organ_obj_t *obj) {
    (void)obj;
    g_drop_count++;
}

/* Thunk evaluator: squares captured argument in env[0] */
static int64_t test_eval_square(organ_obj_t *thunk_obj) {
    organ_thunk_t *th = (organ_thunk_t *)thunk_obj;
    int64_t x = th->env[0];
    return x * x;
}

int main(void) {
    /* 1. Test Profile 1 constructor allocation & RC */
    organ_obj_t *obj = organ_alloc_ctor(0x12345678ULL, 2, test_drop_fields);
    assert(obj != NULL);
    assert(organ_is_unique(obj));
    assert(obj->rc == 1);
    assert(obj->content_tag == 0x12345678ULL);

    /* 2. Test organ_dup */
    organ_dup(obj);
    assert(!organ_is_unique(obj));
    assert(obj->rc == 2);

    /* 3. Test organ_drop (decrement) */
    organ_drop(obj);
    assert(organ_is_unique(obj));
    assert(obj->rc == 1);

    /* 4. Test in-place reuse */
    organ_obj_t *reused = organ_reuse_or_alloc(obj, 0x87654321ULL, 2, test_drop_fields);
    assert(reused == obj); /* Exact pointer identity preserved! */
    assert(reused->content_tag == 0x87654321ULL);

    /* Drop the reused object */
    organ_drop(reused);
    assert(g_drop_count == 1);

    /* 5. Test first-order lazy constructor thunk forcing & memoization */
    organ_thunk_t *th = organ_alloc_thunk(0xabcdefULL, 1, test_eval_square, test_drop_fields);
    assert(th != NULL);
    th->env[0] = 7;

    /* First force: computes 7 * 7 = 49 */
    int64_t val1 = organ_force(th);
    assert(val1 == 49);
    assert(th->kind == ORGAN_KIND_EVALUATED);
    assert(g_drop_count == 2); /* Captured env dropped after eval */

    /* Second force: returns memoized value without re-eval */
    int64_t val2 = organ_force(th);
    assert(val2 == 49);

    /* Clean up thunk */
    organ_drop((organ_obj_t *)th);

    printf("=== All organ_rt tests passed successfully ===\n");
    return 0;
}
