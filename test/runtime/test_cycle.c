/* test_cycle.c — Test the Bacon-Rajan cycle collector
 *
 * Tests:
 *   1. Acyclic data: allocate, drop, verify freed (no cycle collector needed)
 *   2. Cyclic pair: A -> B -> A, drop external refs, verify cycle collector frees both
 *   3. Cyclic triple: A -> B -> C -> A, verify all three collected
 *   4. Mixed: acyclic + cyclic data, verify only cyclic collected
 *   5. Self-referencing: A -> A, verify collected
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include "kk_cycle.h"

/* Runtime functions from kk_runtime.c */
extern int64_t kk_alloc_con(int64_t tag, int64_t nfields);
extern void    kk_set_field(int64_t ptr, int64_t idx, int64_t value);
extern int64_t kk_field(int64_t ptr, int64_t idx);
extern void    kk_retain(int64_t ptr);
extern void    kk_drop(int64_t ptr);
extern int64_t kk_tag(int64_t ptr);

#define TAG_PAIR 1
#define TAG_TRIPLE 2
#define TAG_NODE 3

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

/* Test 1: Acyclic data — normal RC frees it */
static void test_acyclic(void) {
    printf("Test 1: Acyclic data\n");

    /* Allocate a pair: (42, 99) — no pointers, no cycles */
    int64_t pair = kk_alloc_con(TAG_PAIR, 2);
    kk_set_field(pair, 0, 42);
    kk_set_field(pair, 1, 99);

    /* Verify fields */
    check(kk_field(pair, 0) == 42, "field 0 == 42");
    check(kk_field(pair, 1) == 99, "field 1 == 99");

    /* Drop — should free immediately (rc goes to 0) */
    kk_drop(pair);

    /* Run collector — should find nothing */
    int64_t freed = kk_cycle_collect();
    check(freed == 0, "no cycles to collect");
}

/* Test 2: Cyclic pair — A -> B -> A */
static void test_cyclic_pair(void) {
    printf("Test 2: Cyclic pair (A -> B -> A)\n");

    /* Allocate two nodes: A has 1 field (-> B), B has 1 field (-> A) */
    int64_t a = kk_alloc_con(TAG_NODE, 1);
    int64_t b = kk_alloc_con(TAG_NODE, 1);

    /* Create cycle: A.field0 = B, B.field0 = A */
    kk_set_field(a, 0, b);
    kk_retain(b);  /* A holds a ref to B */
    kk_set_field(b, 0, a);
    kk_retain(a);  /* B holds a ref to A */

    /* Now A has rc=2 (1 from alloc + 1 from B), B has rc=2 (1 from alloc + 1 from A) */
    /* Drop external references */
    kk_drop(a);  /* A: rc=1 (only B's ref remains) — becomes purple candidate */
    kk_drop(b);  /* B: rc=1 (only A's ref remains) — becomes purple candidate */

    /* Both are now unreachable but rc=1 due to the cycle */
    int64_t roots_before = kk_cycle_roots_count();
    check(roots_before >= 2, "at least 2 cycle candidates");

    /* Run cycle collector */
    int64_t freed = kk_cycle_collect();
    check(freed == 2, "cycle collector freed 2 objects");
}

/* Test 3: Cyclic triple — A -> B -> C -> A */
static void test_cyclic_triple(void) {
    printf("Test 3: Cyclic triple (A -> B -> C -> A)\n");

    int64_t a = kk_alloc_con(TAG_NODE, 1);
    int64_t b = kk_alloc_con(TAG_NODE, 1);
    int64_t c = kk_alloc_con(TAG_NODE, 1);

    /* Create cycle: A->B->C->A */
    kk_set_field(a, 0, b);
    kk_retain(b);
    kk_set_field(b, 0, c);
    kk_retain(c);
    kk_set_field(c, 0, a);
    kk_retain(a);

    /* Drop external refs */
    kk_drop(a);
    kk_drop(b);
    kk_drop(c);

    int64_t freed = kk_cycle_collect();
    check(freed == 3, "cycle collector freed 3 objects");
}

/* Test 4: Self-referencing node — A -> A */
static void test_self_cycle(void) {
    printf("Test 4: Self-referencing (A -> A)\n");

    int64_t a = kk_alloc_con(TAG_NODE, 1);
    kk_set_field(a, 0, a);
    kk_retain(a);  /* self-reference adds rc */

    /* A has rc=2: 1 from alloc, 1 from self */
    kk_drop(a);  /* rc=1, becomes candidate */

    int64_t freed = kk_cycle_collect();
    check(freed == 1, "cycle collector freed self-referencing object");
}

/* Test 5: Mixed — acyclic tree + cyclic pair, only cycle collected */
static void test_mixed(void) {
    printf("Test 5: Mixed (acyclic tree + cyclic pair)\n");

    /* Acyclic: simple node with integer children (not pointers) */
    int64_t tree = kk_alloc_con(TAG_PAIR, 2);
    kk_set_field(tree, 0, 10);
    kk_set_field(tree, 1, 20);

    /* Cyclic: A <-> B */
    int64_t a = kk_alloc_con(TAG_NODE, 1);
    int64_t b = kk_alloc_con(TAG_NODE, 1);
    kk_set_field(a, 0, b);
    kk_retain(b);
    kk_set_field(b, 0, a);
    kk_retain(a);

    /* Drop cyclic pair's external refs */
    kk_drop(a);
    kk_drop(b);

    /* tree is still live (we hold a ref) */
    check(kk_field(tree, 0) == 10, "tree field 0 still accessible");

    /* Collect cycles — should only free the pair */
    int64_t freed = kk_cycle_collect();
    check(freed == 2, "only cyclic pair collected (not tree)");

    /* tree still works */
    check(kk_field(tree, 1) == 20, "tree field 1 still accessible after collect");

    /* Now drop the tree normally */
    kk_drop(tree);
}

int main(void) {
    printf("=== Cycle Collector Tests ===\n\n");

    test_acyclic();
    test_cyclic_pair();
    test_cyclic_triple();
    test_self_cycle();
    test_mixed();

    printf("\n=== Results: %d/%d passed ===\n", pass_count, test_count);
    printf("Total objects collected by cycle collector: %ld\n",
           (long)kk_cycle_collected_count());

    return (pass_count == test_count) ? 0 : 1;
}
