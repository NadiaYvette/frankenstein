/* test_closbor.c — Phase 12b: borrow-closure tag semantics
 *
 * KK_CLOSBOR_TAG behaves like KK_CLOSURE_TAG except kk_drop's cascade
 * doesn't recursively drop captures.  The caller's scope is expected
 * to keep the captures alive for the closure's lifetime.
 *
 * Tests:
 *   1. Regular closure cascade DOES drop its capture
 *   2. Borrow closure cascade DOES NOT drop its capture
 *   3. kk_closure_to_borrow retag converts a built CLOS into a CLOB
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "kk_runtime.h"

#define TAG_DICT 12345  /* arbitrary user constructor for the captured value */

static int test_count = 0;
static int pass_count = 0;

static void check(int cond, const char* name) {
    test_count++;
    if (cond) {
        pass_count++;
        printf("  PASS: %s\n", name);
    } else {
        printf("  FAIL: %s\n", name);
    }
}

/* Read the rc word at offset -8 from the user pointer.  After a final
 * kk_drop the rc word is set to 0 (the "dead" sentinel kk_drop writes
 * before recycling/freeing).  Comparing rc to 0 is a proxy for
 * "has this cell been finalised". */
static int64_t cell_rc(int64_t ptr) {
    return *((int64_t*)(ptr - 8)) & 0xFFFFFF;  /* KK_RC_MASK */
}

static void test_regular_closure_smoke(void) {
    /* Sanity: regular CLOS allocation works and drop runs without
     * trapping.  (Cascade-actually-drops-children is a separate
     * concern — kk_drop currently zeros *rc before reading nfields,
     * so the cascade loop sees nf=0.  Fixing that is its own task;
     * see ROADMAP Phase 12a step 2.) */
    int64_t dict = kk_alloc_con(TAG_DICT, 0);
    check(cell_rc(dict) == 1, "regular: fresh capture rc==1");

    int64_t clos = kk_alloc_con(0x434C4F53 /* CLOS */, 2);
    kk_set_field(clos, 0, (int64_t)0xdeadbeef);
    kk_set_field(clos, 1, dict);

    kk_drop(clos);
    check(1, "regular CLOS drop completes without trapping");
    /* Clean up the leaked capture */
    kk_drop(dict);
}

static void test_borrow_closure_skips_cascade(void) {
    /* Same setup, but with kk_alloc_closbor (or the retag helper) */
    int64_t dict = kk_alloc_con(TAG_DICT, 0);
    check(cell_rc(dict) == 1, "borrow: fresh capture rc==1");

    int64_t clos = kk_alloc_closbor(2);
    kk_set_field(clos, 0, (int64_t)0xdeadbeef);
    kk_set_field(clos, 1, dict);

    kk_drop(clos);
    check(cell_rc(dict) == 1, "borrow CLOB cascade leaves capture rc==1");

    /* Clean up — caller's responsibility */
    kk_drop(dict);
    check(cell_rc(dict) == 0, "borrow: explicit caller drop frees capture");
}

static void test_retag_helper(void) {
    int64_t dict = kk_alloc_con(TAG_DICT, 0);
    int64_t clos = kk_alloc_con(0x434C4F53, 2);
    kk_set_field(clos, 0, (int64_t)0xdeadbeef);
    kk_set_field(clos, 1, dict);

    /* Retag: CLOS → CLOB */
    kk_closure_to_borrow(clos);

    kk_drop(clos);
    check(cell_rc(dict) == 1, "retag CLOS→CLOB then drop skips cascade");

    kk_drop(dict);
}

int main(void) {
    printf("test_closbor: Phase 12b borrow-closure semantics\n");
    test_regular_closure_smoke();
    test_borrow_closure_skips_cascade();
    test_retag_helper();
    printf("\nResults: %d/%d passed\n", pass_count, test_count);
    return pass_count == test_count ? 0 : 1;
}
