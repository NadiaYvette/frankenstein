/*
 * test_freestanding_linear.c — Freestanding linear state transition
 * Compiles with -DORGAN_FREESTANDING and verified via nm -u to have 0 malloc/free/rts symbols.
 */
#ifndef ORGAN_FREESTANDING
#define ORGAN_FREESTANDING
#endif
#include "organ_rt.h"

typedef struct {
    uint64_t page_id;
    uint32_t cluster_order;
    uint32_t flags;
} page_cluster_t;

/* Profile 0 in-place state transition: mutates struct linearly without allocations */
void page_cluster_promote(page_cluster_t *cluster) {
    if (cluster != NULL) {
        cluster->cluster_order += 1u;
        cluster->flags |= 0x1u;
    }
}

int test_linear_transition(void) {
    page_cluster_t cluster = { .page_id = 42, .cluster_order = 0, .flags = 0 };
    page_cluster_promote(&cluster);
    return (cluster.cluster_order == 1u) ? 0 : 1;
}
