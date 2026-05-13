/* Test: Plotkin-style evidence-vector dispatch end-to-end.
 *
 * Builds a tiny scenario by hand (no compiler involvement):
 *   - effect "ask" has one op "get" returning a constant
 *   - construct an op_table with a single closure (returns 42)
 *   - extend the empty evv with (ask_id, op_table)
 *   - look the handler up by id, fetch op 0, invoke through the closure ABI
 *   - confirm we get 42 back
 *
 * Also exercises kk_evv_extend stacking (innermost-wins) and a
 * cross-effect lookup that returns 0 (unhandled).
 *
 * Build (run from runtime/):
 *   cc -o test_evv_plotkin test_evv_plotkin.c kk_runtime.c \
 *      kk_arena.c kk_cycle.c -I.
 *
 * Expected output:
 *   PASS: single-handler lookup invokes closure -> 42
 *   PASS: nested extend shadows outer handler -> 100
 *   PASS: unrelated effect lookup returns 0
 */
#include <stdio.h>
#include "kk_runtime.h"

#define ASK_ID    0x6173686B  /* "ask\0" as a 32-bit literal; arbitrary */
#define LOG_ID    0x6C6F676B  /* "logk" — different effect */
#define CLOS_TAG  0x434C4F53  /* "CLOS" */

/* Closure ABI: field 0 is raw fn ptr; arg 0 to the fn is the closure itself. */
static int64_t ask_get_returns_42(int64_t closure) {
    (void)closure;
    return 42;
}

static int64_t ask_get_returns_100(int64_t closure) {
    (void)closure;
    return 100;
}

static int64_t make_nullary_closure(int64_t (*fn)(int64_t)) {
    int64_t c = kk_alloc_con(CLOS_TAG, 1);
    kk_set_field(c, 0, (int64_t)(intptr_t)fn);
    return c;
}

static int64_t invoke_op(int64_t op_tab, int64_t op_idx) {
    int64_t clos = kk_optab_get(op_tab, op_idx);
    int64_t fn_word = kk_field(clos, 0);
    int64_t (*fn)(int64_t) = (int64_t (*)(int64_t))(intptr_t)fn_word;
    return fn(clos);
}

int main(void) {
    int passed = 1;

    /* --- Case 1: single handler in evv, lookup + invoke --- */
    {
        int64_t closure  = make_nullary_closure(ask_get_returns_42);
        int64_t op_table = kk_optab_create(1);
        kk_optab_set(op_table, 0, closure);

        int64_t evv = kk_evv_extend(0, ASK_ID, op_table);
        int64_t tab = kk_evv_lookup(evv, ASK_ID);
        if (tab == 0) {
            printf("FAIL: lookup returned 0 for installed handler\n");
            passed = 0;
        } else {
            int64_t result = invoke_op(tab, 0);
            if (result == 42) {
                printf("PASS: single-handler lookup invokes closure -> 42\n");
            } else {
                printf("FAIL: invoke returned %ld, expected 42\n", (long)result);
                passed = 0;
            }
        }
    }

    /* --- Case 2: nested extend, innermost wins --- */
    {
        int64_t c_outer  = make_nullary_closure(ask_get_returns_42);
        int64_t t_outer  = kk_optab_create(1);
        kk_optab_set(t_outer, 0, c_outer);

        int64_t c_inner  = make_nullary_closure(ask_get_returns_100);
        int64_t t_inner  = kk_optab_create(1);
        kk_optab_set(t_inner, 0, c_inner);

        int64_t evv_outer = kk_evv_extend(0,         ASK_ID, t_outer);
        int64_t evv_inner = kk_evv_extend(evv_outer, ASK_ID, t_inner);

        int64_t tab = kk_evv_lookup(evv_inner, ASK_ID);
        int64_t result = invoke_op(tab, 0);
        if (result == 100) {
            printf("PASS: nested extend shadows outer handler -> 100\n");
        } else {
            printf("FAIL: nested lookup returned %ld, expected 100\n",
                   (long)result);
            passed = 0;
        }
    }

    /* --- Case 3: unrelated lookup returns 0 --- */
    {
        int64_t c    = make_nullary_closure(ask_get_returns_42);
        int64_t tab  = kk_optab_create(1);
        kk_optab_set(tab, 0, c);
        int64_t evv  = kk_evv_extend(0, ASK_ID, tab);

        int64_t hit  = kk_evv_lookup(evv, LOG_ID);
        if (hit == 0) {
            printf("PASS: unrelated effect lookup returns 0\n");
        } else {
            printf("FAIL: unrelated lookup returned %ld, expected 0\n",
                   (long)hit);
            passed = 0;
        }
    }

    return passed ? 0 : 1;
}
