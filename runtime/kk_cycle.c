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
    *rc = (*rc & (KK_RC_MASK | KK_NFIELDS_MASK)) | color;
}

static inline void kk_rc_decrement(int64_t ptr) {
    int64_t* rc = kk_rc_ptr(ptr);
    int64_t count = *rc & KK_RC_MASK;
    if (count > 0) count--;
    *rc = (*rc & (KK_COLOR_MASK | KK_NFIELDS_MASK)) | count;
}

static inline void kk_rc_increment(int64_t ptr) {
    int64_t* rc = kk_rc_ptr(ptr);
    int64_t count = *rc & KK_RC_MASK;
    count++;
    *rc = (*rc & (KK_COLOR_MASK | KK_NFIELDS_MASK)) | count;
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
#define KK_EVV2_TAG    0x45565632  /* "EVV2" — Plotkin evv stack */
#define KK_OPTAB_TAG   0x4F505442  /* "OPTB" — op table */
#define KK_PAP_TAG     0x50415030  /* "PAP0" — partially applied fn */
#define KK_THUNK_TAG   0x4C415A59  /* "LAZY" */
#define KK_CLOSURE_TAG 0x434C4F53  /* "CLOS" — field 0 is a raw fn ptr */
#define KK_CLOSBOR_TAG 0x434C4F42  /* "CLOB" — closure w/ borrowed captures (Phase 12b) */

int kk_can_be_cyclic(int64_t tag) {
    /* Thunks, evidence vectors, op tables, PAPs, and closures have acyclic layouts */
    if (tag == KK_THUNK_TAG || tag == KK_EVV_TAG || tag == KK_EVV2_TAG
        || tag == KK_OPTAB_TAG || tag == KK_PAP_TAG
        || tag == KK_CLOSURE_TAG || tag == KK_CLOSBOR_TAG) return 0;
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

/* nfields is now packed into the RC word (bits 55-48) by kk_alloc_con.
 * These functions read/write the RC word directly — O(1) instead of
 * the old O(n) linear-probe hash table. */

void kk_register_nfields(int64_t ptr, int64_t nfields) {
    /* Write nfields into the RC word's nfields bits */
    int64_t* rc = (int64_t*)(ptr - 8);
    int64_t nf = (nfields > 255 ? 255 : nfields);
    *rc = (*rc & ~KK_NFIELDS_MASK) | (nf << KK_NFIELDS_SHIFT);
}

void kk_unregister_nfields(int64_t ptr) {
    /* Clear nfields bits in RC word */
    int64_t* rc = (int64_t*)(ptr - 8);
    *rc = *rc & ~KK_NFIELDS_MASK;
}

int64_t kk_nfields(int64_t ptr) {
    int64_t* rc = (int64_t*)(ptr - 8);
    return (*rc & KK_NFIELDS_MASK) >> KK_NFIELDS_SHIFT;
}

/* Iterate over the children (fields that are heap pointers) of an object.
 * For closures we skip field 0, which holds a raw function pointer. */
static void for_each_child(int64_t ptr, void (*fn)(int64_t child)) {
    if (!kk_is_heap_ptr(ptr)) return;
    /* Bound nf: kk_alloc_con clamps to 255.  A recycled-or-corrupt
     * cell can have rc-word bits that decode to an absurdly large
     * nfields and the loop walks into unmapped memory.  Reject
     * implausible values silently. */
    int64_t nf = kk_nfields(ptr);
    if (nf < 0 || nf > 255) return;
    int64_t* fields = (int64_t*)(ptr + 8);
    int64_t tag_w = *(int64_t*)ptr;
    if (tag_w == KK_CLOSBOR_TAG) return;
    int64_t start = (tag_w == KK_CLOSURE_TAG) ? 1 : 0;
    for (int64_t i = start; i < nf; i++) {
        int64_t child = fields[i];
        if (!kk_is_heap_ptr(child)) continue;
        /* Reject children that aren't arena-owned: malloc'd or
         * unmapped addresses don't participate in cycles via the
         * candidate API and dereferencing them segfaults. */
        if (!kk_arena_maybe_owns((const void*)(uintptr_t)child)) continue;
        fn(child);
    }
}

/* ---- Phase 1: MarkRoots — trial-delete internal references ---- */

/* Reject pointers that look like freelist entries — kk_arena_recycle_put
 * leaves block[0]=0 (rc word) and block[1]'s high bit set (KK_RECYCLE_FLAG
 * on the freelist next-ptr).  If we traverse into one of those during
 * mark_gray/scan/collect_white the recursion processes garbage and
 * sometimes infinite-loops the stack.  ptr points to block[1] (the tag
 * slot) — bit 63 of *ptr is the recycle flag. */
#define KK_RECYCLE_FLAG ((int64_t)1 << 63)
static inline int kk_is_freelist_cell(int64_t ptr) {
    int64_t* rc = (int64_t*)(ptr - 8);
    if ((*rc & 0xFFFFFFFFFFFFFFLL) != 0) return 0;  /* live: rc/nfields/color nonzero */
    return (*(int64_t*)ptr & KK_RECYCLE_FLAG) != 0;
}

/* Iterative mark_gray.  Surd-quintic builds polynomial coefficient
 * lists that recurse deeper than the default 8 MiB pthread stack
 * — recursive mark_gray overflows and segfaults.  Use a heap-grown
 * work queue instead.
 *
 * mark_gray's job is two-fold:
 *  - decrement the child's rc (trial deletion)
 *  - recurse into child's children if it's not already GRAY
 * The decrement happens once per (parent, child) edge — push every
 * child we encounter onto the queue.  Setting GRAY before pushing
 * doesn't help because we want the decrement for every edge.
 *
 * Two-stack pattern:
 *  - rc_queue: cells to decrement-and-recurse (children to visit)
 *  - grey_queue: cells to mark gray and walk THEIR children
 * Avoids duplicating the per-edge decrement vs. per-cell grey-mark. */
static int64_t* mg_queue = NULL;
static int64_t  mg_queue_cap = 0;
static int64_t  mg_queue_len = 0;
static void mg_queue_push(int64_t ptr) {
    if (mg_queue_len >= mg_queue_cap) {
        int64_t new_cap = mg_queue_cap < 1024 ? 1024 : mg_queue_cap * 2;
        int64_t* p = (int64_t*)realloc(mg_queue, (size_t)new_cap * sizeof(int64_t));
        if (!p) return;  /* OOM: abandon; collection becomes incomplete */
        mg_queue = p;
        mg_queue_cap = new_cap;
    }
    mg_queue[mg_queue_len++] = ptr;
}

static void mark_gray_visit_child(int64_t child) {
    if (!kk_is_heap_ptr(child)) return;
    kk_rc_decrement(child);
    if (kk_is_freelist_cell(child)) return;
    if (kk_get_color(child) == KK_COLOR_GRAY) return;
    mg_queue_push(child);
}

static void mark_gray(int64_t root) {
    if (!kk_is_heap_ptr(root)) return;
    if (kk_is_freelist_cell(root)) return;
    if (kk_get_color(root) == KK_COLOR_GRAY) return;
    mg_queue_push(root);
    while (mg_queue_len > 0) {
        int64_t ptr = mg_queue[--mg_queue_len];
        if (kk_is_freelist_cell(ptr)) continue;
        if (kk_get_color(ptr) == KK_COLOR_GRAY) continue;
        kk_set_color(ptr, KK_COLOR_GRAY);
        for_each_child(ptr, mark_gray_visit_child);
    }
}

/* ---- Phase 2: ScanRoots — identify garbage (white) vs live (black) ---- */

/* Each phase uses its own work queue (kept across calls so allocation
 * cost amortizes).  Iterative everywhere — recursion blew the stack
 * on surd-quintic's deep cons-cell chains. */
static int64_t* sb_queue = NULL;
static int64_t  sb_queue_cap = 0;
static int64_t  sb_queue_len = 0;
static void sb_queue_push(int64_t ptr) {
    if (sb_queue_len >= sb_queue_cap) {
        int64_t new_cap = sb_queue_cap < 1024 ? 1024 : sb_queue_cap * 2;
        int64_t* p = (int64_t*)realloc(sb_queue, (size_t)new_cap * sizeof(int64_t));
        if (!p) return;
        sb_queue = p;
        sb_queue_cap = new_cap;
    }
    sb_queue[sb_queue_len++] = ptr;
}

static void scan_black_visit_child(int64_t child) {
    if (!kk_is_heap_ptr(child)) return;
    kk_rc_increment(child);
    if (kk_is_freelist_cell(child)) return;
    if (kk_get_color(child) == KK_COLOR_BLACK) return;
    sb_queue_push(child);
}

static void scan_black(int64_t root) {
    if (!kk_is_heap_ptr(root)) return;
    if (kk_is_freelist_cell(root)) return;
    sb_queue_push(root);
    while (sb_queue_len > 0) {
        int64_t ptr = sb_queue[--sb_queue_len];
        if (kk_is_freelist_cell(ptr)) continue;
        if (kk_get_color(ptr) == KK_COLOR_BLACK) continue;
        kk_set_color(ptr, KK_COLOR_BLACK);
        for_each_child(ptr, scan_black_visit_child);
    }
}

/* scan walks gray-rooted cells iteratively.  Uses scan_visit_child
 * to enqueue children for further scan-processing.  When a cell has
 * rc>0 (external refs), scan_black is invoked — it uses its own
 * sb_queue so scan's queue isn't disturbed. */
static int64_t* sc_queue = NULL;
static int64_t  sc_queue_cap = 0;
static int64_t  sc_queue_len = 0;
static void sc_queue_push(int64_t ptr) {
    if (sc_queue_len >= sc_queue_cap) {
        int64_t new_cap = sc_queue_cap < 1024 ? 1024 : sc_queue_cap * 2;
        int64_t* p = (int64_t*)realloc(sc_queue, (size_t)new_cap * sizeof(int64_t));
        if (!p) return;
        sc_queue = p;
        sc_queue_cap = new_cap;
    }
    sc_queue[sc_queue_len++] = ptr;
}

static void scan_visit_child(int64_t child) {
    if (!kk_is_heap_ptr(child)) return;
    if (kk_is_freelist_cell(child)) return;
    if (kk_get_color(child) != KK_COLOR_GRAY) return;
    sc_queue_push(child);
}

static void scan(int64_t root) {
    if (!kk_is_heap_ptr(root)) return;
    if (kk_is_freelist_cell(root)) return;
    if (kk_get_color(root) != KK_COLOR_GRAY) return;
    sc_queue_push(root);
    while (sc_queue_len > 0) {
        int64_t ptr = sc_queue[--sc_queue_len];
        if (kk_is_freelist_cell(ptr)) continue;
        if (kk_get_color(ptr) != KK_COLOR_GRAY) continue;
        if (kk_get_rc(ptr) > 0) {
            scan_black(ptr);  /* uses sb_queue, not sc_queue */
        } else {
            kk_set_color(ptr, KK_COLOR_WHITE);
            for_each_child(ptr, scan_visit_child);
        }
    }
}

/* ---- Phase 3: CollectRoots — free white objects ---- */

/* Iterative collect_white.  Two-phase: mark all reachable WHITE
 * cells BLACK (collecting their addresses), then free them.  The
 * mark-black-on-visit trick prevents double-processing under
 * iteration the same way it does under recursion. */
static int64_t* cw_queue = NULL;
static int64_t  cw_queue_cap = 0;
static int64_t  cw_queue_len = 0;
static int64_t* cw_freed = NULL;
static int64_t  cw_freed_cap = 0;
static int64_t  cw_freed_len = 0;
static void cw_queue_push(int64_t ptr) {
    if (cw_queue_len >= cw_queue_cap) {
        int64_t new_cap = cw_queue_cap < 1024 ? 1024 : cw_queue_cap * 2;
        int64_t* p = (int64_t*)realloc(cw_queue, (size_t)new_cap * sizeof(int64_t));
        if (!p) return;
        cw_queue = p;
        cw_queue_cap = new_cap;
    }
    cw_queue[cw_queue_len++] = ptr;
}
static void cw_freed_push(int64_t ptr) {
    if (cw_freed_len >= cw_freed_cap) {
        int64_t new_cap = cw_freed_cap < 1024 ? 1024 : cw_freed_cap * 2;
        int64_t* p = (int64_t*)realloc(cw_freed, (size_t)new_cap * sizeof(int64_t));
        if (!p) return;
        cw_freed = p;
        cw_freed_cap = new_cap;
    }
    cw_freed[cw_freed_len++] = ptr;
}

static void collect_white(int64_t root) {
    if (!kk_is_heap_ptr(root)) return;
    if (kk_is_freelist_cell(root)) return;
    if (kk_get_color(root) != KK_COLOR_WHITE) return;

    cw_queue_push(root);
    /* Phase A: walk every reachable WHITE cell, mark BLACK, collect ptr */
    while (cw_queue_len > 0) {
        int64_t ptr = cw_queue[--cw_queue_len];
        if (kk_is_freelist_cell(ptr)) continue;
        if (kk_get_color(ptr) != KK_COLOR_WHITE) continue;
        kk_set_color(ptr, KK_COLOR_BLACK);
        cw_freed_push(ptr);

        int64_t tag_w = *(int64_t*)ptr;
        if (tag_w == KK_CLOSBOR_TAG) continue;  /* doesn't own captures */
        int64_t nf = kk_nfields(ptr);
        int64_t* fields = (int64_t*)(ptr + 8);
        int64_t start = (tag_w == KK_CLOSURE_TAG) ? 1 : 0;
        for (int64_t i = start; i < nf; i++) {
            int64_t child = fields[i];
            if (kk_is_heap_ptr(child)) cw_queue_push(child);
        }
    }
    /* Phase B: free each */
    while (cw_freed_len > 0) {
        int64_t ptr = cw_freed[--cw_freed_len];
        kk_unregister_nfields(ptr);
        total_collected++;
        kk_arena_free((void*)(ptr - 8));
    }
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

    /* Phase 1: MarkRoots — trial-delete internal references.
     * kk_is_heap_ptr only checks alignment + the >0x10000 floor; it
     * doesn't verify the memory is mapped.  At higher collection
     * frequencies, the roots_buf accumulates stale pointers to cells
     * that have been freed and possibly unmapped/recycled to a
     * different size.  Guard reads of *ptr behind kk_arena_maybe_owns
     * which range-checks against the arena slab list. */
    for (int64_t i = 0; i < roots_len; i++) {
        int64_t ptr = roots_buf[i];
        if (kk_is_heap_ptr(ptr)
            && kk_arena_maybe_owns((const void*)(uintptr_t)ptr)
            && !kk_is_freelist_cell(ptr)
            && kk_get_color(ptr) == KK_COLOR_PURPLE) {
            mark_gray(ptr);
        } else {
            roots_buf[i] = 0;
        }
    }

    /* Phase 2: ScanRoots — identify live (black) vs garbage (white) */
    for (int64_t i = 0; i < roots_len; i++) {
        int64_t ptr = roots_buf[i];
        if (ptr != 0 && kk_arena_maybe_owns((const void*)(uintptr_t)ptr)
                     && !kk_is_freelist_cell(ptr)) {
            scan(ptr);
        }
    }

    /* Phase 3: CollectRoots — free white objects */
    for (int64_t i = 0; i < roots_len; i++) {
        int64_t ptr = roots_buf[i];
        if (ptr != 0 && kk_arena_maybe_owns((const void*)(uintptr_t)ptr)
                     && !kk_is_freelist_cell(ptr)) {
            collect_white(ptr);
        }
    }

    roots_len = 0;
    return total_collected - freed_before;
}

int64_t kk_cycle_roots_count(void) {
    return roots_len;
}

int64_t kk_cycle_collected_count(void) {
    return total_collected;
}
