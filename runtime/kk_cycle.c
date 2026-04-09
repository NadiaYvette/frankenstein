/* kk_cycle.c — Bacon-Rajan synchronous cycle collector for Perceus RC
 *
 * Implements trial deletion cycle collection as described in:
 *   Bacon & Rajan, "Concurrent Cycle Collection in Reference Counted Systems"
 *   ECOOP 2001
 *
 * The algorithm has three phases:
 *   1. MarkRoots: For each candidate root, trial-delete internal references
 *   2. ScanRoots: Check which objects reached rc=0 (garbage) vs rc>0 (live)
 *   3. CollectRoots: Free all white (garbage) objects
 */

#include "kk_cycle.h"
#include "kk_arena.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ---- Object layout helpers (must match kk_runtime.c) ---- */

static inline int kk_is_heap_ptr(int64_t ptr) {
    return ptr != 0 && (ptr & 7) == 0 && ptr > 4096;
}

static inline int64_t* kk_rc_ptr(int64_t ptr) {
    return (int64_t*)(ptr - 8);
}

static inline int64_t kk_get_rc(int64_t ptr) {
    return *kk_rc_ptr(ptr) & KK_RC_MASK;
}

static inline int64_t kk_get_color(int64_t ptr) {
    return *kk_rc_ptr(ptr) & KK_COLOR_MASK;
}

static inline void kk_set_color(int64_t ptr, int64_t color) {
    int64_t* rc = kk_rc_ptr(ptr);
    *rc = (*rc & KK_RC_MASK) | color;
}

static inline void kk_rc_decrement(int64_t ptr) {
    int64_t* rc = kk_rc_ptr(ptr);
    int64_t count = *rc & KK_RC_MASK;
    if (count > 0) count--;
    *rc = (*rc & KK_COLOR_MASK) | count;
}

static inline void kk_rc_increment(int64_t ptr) {
    int64_t* rc = kk_rc_ptr(ptr);
    int64_t count = *rc & KK_RC_MASK;
    count++;
    *rc = (*rc & KK_COLOR_MASK) | count;
}

/* ---- Roots buffer ---- */

#define KK_ROOTS_INITIAL_CAP 256

static int64_t* roots_buf = NULL;
static int64_t  roots_len = 0;
static int64_t  roots_cap = 0;

static int64_t total_collected = 0;

static void roots_ensure_cap(void) {
    if (roots_buf == NULL) {
        roots_cap = KK_ROOTS_INITIAL_CAP;
        roots_buf = (int64_t*)malloc((size_t)roots_cap * sizeof(int64_t));
    } else if (roots_len >= roots_cap) {
        roots_cap *= 2;
        roots_buf = (int64_t*)realloc(roots_buf, (size_t)roots_cap * sizeof(int64_t));
    }
}

/* ---- Field scanning ---- */

/* Known special tags that have fixed layouts */
#define KK_EVV_TAG     0x45565630  /* "EVV0" */
#define KK_THUNK_TAG   0x4C415A59  /* "LAZY" */
#define KK_CLOSURE_TAG 0x434C4F53  /* "CLOS" — field 0 is a raw fn ptr */

int kk_can_be_cyclic(int64_t tag) {
    /* Thunks, evidence vectors, and closures have acyclic layouts */
    if (tag == KK_THUNK_TAG || tag == KK_EVV_TAG || tag == KK_CLOSURE_TAG) return 0;
    /* Constructor tags below 0x10000 are user-defined ADT constructors
     * that could potentially contain back-references */
    return 1;
}

/* Get the number of fields.
 * We don't store nfields in the object header (to keep the layout minimal),
 * so we need another way to determine it. We use the allocation size:
 * total_bytes = (2 + nfields) * 8, so nfields = total_bytes/8 - 2.
 *
 * Since malloc doesn't store the size portably, we add a size field.
 * ALTERNATIVE: We encode nfields in the tag's high bits.
 *
 * For now, we use a simpler approach: scan fields until we find a
 * non-plausible pointer. This is conservative but works for our use case
 * where fields are either valid heap pointers or small integers.
 *
 * UPDATE: We store nfields in a side table populated by kk_alloc_con_tracked.
 */

/* Side table: ptr -> nfields (simple linear probe hash table) */
#define KK_NFIELDS_TABLE_SIZE 4096
static struct { int64_t ptr; int64_t nfields; } nfields_table[KK_NFIELDS_TABLE_SIZE];

void kk_register_nfields(int64_t ptr, int64_t nfields) {
    int64_t idx = (ptr >> 3) & (KK_NFIELDS_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_NFIELDS_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_NFIELDS_TABLE_SIZE - 1);
        if (nfields_table[probe].ptr == 0 || nfields_table[probe].ptr == ptr) {
            nfields_table[probe].ptr = ptr;
            nfields_table[probe].nfields = nfields;
            return;
        }
    }
    /* Table full — shouldn't happen in practice */
}

void kk_unregister_nfields(int64_t ptr) {
    int64_t idx = (ptr >> 3) & (KK_NFIELDS_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_NFIELDS_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_NFIELDS_TABLE_SIZE - 1);
        if (nfields_table[probe].ptr == ptr) {
            nfields_table[probe].ptr = 0;
            nfields_table[probe].nfields = 0;
            return;
        }
        if (nfields_table[probe].ptr == 0) return;
    }
}

int64_t kk_nfields(int64_t ptr) {
    int64_t idx = (ptr >> 3) & (KK_NFIELDS_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_NFIELDS_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_NFIELDS_TABLE_SIZE - 1);
        if (nfields_table[probe].ptr == ptr) {
            return nfields_table[probe].nfields;
        }
        if (nfields_table[probe].ptr == 0) return 0;
    }
    return 0;
}

/* Iterate over the children (fields that are heap pointers) of an object.
 * For closures we skip field 0, which holds a raw function pointer. */
static void for_each_child(int64_t ptr, void (*fn)(int64_t child)) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t nf = kk_nfields(ptr);
    int64_t* fields = (int64_t*)(ptr + 8);
    int64_t start = (*(int64_t*)ptr == KK_CLOSURE_TAG) ? 1 : 0;
    for (int64_t i = start; i < nf; i++) {
        int64_t child = fields[i];
        if (kk_is_heap_ptr(child)) {
            fn(child);
        }
    }
}

/* ---- Phase 1: MarkRoots — trial-delete internal references ---- */

static void mark_gray(int64_t ptr);

static void mark_gray_child(int64_t child) {
    kk_rc_decrement(child);
    if (kk_get_color(child) != KK_COLOR_GRAY) {
        mark_gray(child);
    }
}

static void mark_gray(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    if (kk_get_color(ptr) == KK_COLOR_GRAY) return;
    kk_set_color(ptr, KK_COLOR_GRAY);
    for_each_child(ptr, mark_gray_child);
}

/* ---- Phase 2: ScanRoots — identify garbage (white) vs live (black) ---- */

static void scan(int64_t ptr);
static void scan_black(int64_t ptr);

static void scan_black_child(int64_t child) {
    kk_rc_increment(child);
    if (kk_get_color(child) != KK_COLOR_BLACK) {
        scan_black(child);
    }
}

static void scan_black(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    kk_set_color(ptr, KK_COLOR_BLACK);
    for_each_child(ptr, scan_black_child);
}

static void scan_child(int64_t child) {
    scan(child);
}

static void scan(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    if (kk_get_color(ptr) != KK_COLOR_GRAY) return;
    if (kk_get_rc(ptr) > 0) {
        /* External reference exists — this object is live */
        scan_black(ptr);
    } else {
        /* No external references — this is garbage */
        kk_set_color(ptr, KK_COLOR_WHITE);
        for_each_child(ptr, scan_child);
    }
}

/* ---- Phase 3: CollectRoots — free white objects ---- */

static void collect_white(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    if (kk_get_color(ptr) != KK_COLOR_WHITE) return;

    /* Mark black to prevent double-free */
    kk_set_color(ptr, KK_COLOR_BLACK);

    /* Recursively collect children first */
    int64_t nf = kk_nfields(ptr);
    int64_t* fields = (int64_t*)(ptr + 8);
    int64_t start = (*(int64_t*)ptr == KK_CLOSURE_TAG) ? 1 : 0;
    for (int64_t i = start; i < nf; i++) {
        int64_t child = fields[i];
        collect_white(child);
    }

    /* Unregister and free. Arena-owned cells are not individually
     * reclaimed; kk_arena_free is a no-op for them. */
    kk_unregister_nfields(ptr);
    total_collected++;
    kk_arena_free((void*)(ptr - 8));
}

/* ---- Public API ---- */

void kk_cycle_candidate(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    /* Only track objects that can participate in cycles */
    int64_t tag = *(int64_t*)ptr;
    if (!kk_can_be_cyclic(tag)) return;
    /* Mark as purple (candidate) and add to roots buffer */
    kk_set_color(ptr, KK_COLOR_PURPLE);
    roots_ensure_cap();
    roots_buf[roots_len++] = ptr;
}

int64_t kk_cycle_collect(void) {
    if (roots_len == 0) return 0;

    int64_t freed_before = total_collected;

    /* Phase 1: MarkRoots — trial-delete internal references */
    for (int64_t i = 0; i < roots_len; i++) {
        int64_t ptr = roots_buf[i];
        if (kk_is_heap_ptr(ptr) && kk_get_color(ptr) == KK_COLOR_PURPLE) {
            mark_gray(ptr);
        } else {
            /* No longer a candidate (already freed or re-colored) */
            roots_buf[i] = 0;
        }
    }

    /* Phase 2: ScanRoots — identify live (black) vs garbage (white) */
    for (int64_t i = 0; i < roots_len; i++) {
        int64_t ptr = roots_buf[i];
        if (ptr != 0) {
            scan(ptr);
        }
    }

    /* Phase 3: CollectRoots — free white objects */
    for (int64_t i = 0; i < roots_len; i++) {
        int64_t ptr = roots_buf[i];
        if (ptr != 0) {
            collect_white(ptr);
        }
    }

    /* Reset roots buffer */
    roots_len = 0;

    return total_collected - freed_before;
}

int64_t kk_cycle_roots_count(void) {
    return roots_len;
}

int64_t kk_cycle_collected_count(void) {
    return total_collected;
}
