/* Frankenstein bump-arena allocator — implementation.
 *
 * Slab layout: a malloc'd block of KK_ARENA_SLAB_SIZE bytes, used as
 *
 *     [ kk_slab header | bump payload .................... ]
 *
 * The bump pointer (`used`) records how many payload bytes are taken;
 * each kk_arena_alloc bumps it. When a request doesn't fit in the
 * current slab we allocate a new one and link it onto the head of the
 * slab list. Requests larger than the bump area get a dedicated
 * "oversized" slab sized exactly for the request — they still go on
 * the same list, so kk_arena_owns and kk_arena_reset see them.
 *
 * The slab list is singly-linked (newest first). kk_arena_owns walks
 * it: O(slabs) per call, but slabs are large (default 1 MiB) so the
 * list stays short — even a 100 MiB working set is only ~100 nodes.
 */

#include "kk_arena.h"

#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define KK_ARENA_SLAB_SIZE  ((size_t)(1u << 20))   /* 1 MiB */
#define KK_ARENA_ALIGN      ((size_t)8)

typedef struct kk_slab {
    struct kk_slab* next;   /* linked-list of all live slabs */
    size_t          cap;    /* total payload bytes in this slab */
    size_t          used;   /* bytes already handed out */
    uint8_t         data[]; /* flexible payload */
} kk_slab;

static kk_slab* g_head = NULL;          /* newest slab (allocation cursor) */
static int      g_disabled = -1;        /* lazily resolved from env */
static int64_t  g_total_alloc_bytes = 0;
static int64_t  g_total_slab_bytes  = 0;
static uintptr_t g_arena_lo = (uintptr_t)-1;  /* lowest arena byte */
static uintptr_t g_arena_hi = 0;              /* one past highest arena byte */

/* Per-size freelists for recycling dropped cells live further down,
 * in the "Free-list recycling" section.  kk_arena_alloc pulls from
 * them via kk_arena_recycle(); kk_drop pushes via kk_arena_recycle_put
 * when a refcount hits zero on an arena-owned cell.  Surd-quintic's
 * degree-7 root finder allocates ~2.4M cells/sec; without recycling
 * the arena grew to 51 GiB before OOM, with it RSS stays bounded. */

static int arena_enabled(void) {
    if (g_disabled == -1) {
        const char* v = getenv("KK_NO_ARENA");
        g_disabled = (v != NULL && v[0] != '\0' && v[0] != '0') ? 1 : 0;
    }
    return !g_disabled;
}

static size_t round_up(size_t n, size_t a) {
    return (n + (a - 1)) & ~(a - 1);
}

static kk_slab* new_slab(size_t payload_cap) {
    size_t total = sizeof(kk_slab) + payload_cap;
    kk_slab* s = (kk_slab*)malloc(total);
    if (!s) return NULL;
    s->next = g_head;
    s->cap  = payload_cap;
    s->used = 0;
    g_head  = s;
    g_total_slab_bytes += (int64_t)payload_cap;
    /* Update global address range for fast kk_arena_owns */
    uintptr_t lo = (uintptr_t)s->data;
    uintptr_t hi = lo + payload_cap;
    if (lo < g_arena_lo) g_arena_lo = lo;
    if (hi > g_arena_hi) g_arena_hi = hi;
    return s;
}

void* kk_arena_alloc(size_t size) {
    if (!arena_enabled()) return NULL;
    if (size == 0) size = KK_ARENA_ALIGN;
    size = round_up(size, KK_ARENA_ALIGN);

    /* Try the freelist first (see kk_arena_recycle below) — pops a
     * previously-dropped cell of the same size, keeping the pointer
     * inside [g_arena_lo, g_arena_hi). */
    void* recycled = kk_arena_recycle(size);
    if (recycled) return recycled;

    /* Oversized: dedicated slab sized exactly for the request. */
    if (size > KK_ARENA_SLAB_SIZE / 2) {
        kk_slab* s = new_slab(size);
        if (!s) return NULL;
        s->used = size;
        g_total_alloc_bytes += (int64_t)size;
        return (void*)s->data;
    }

    /* Common case: bump in the current slab, allocate a new one if full. */
    if (g_head == NULL || g_head->used + size > g_head->cap) {
        if (new_slab(KK_ARENA_SLAB_SIZE) == NULL) return NULL;
    }
    void* p = (void*)(g_head->data + g_head->used);
    g_head->used += size;
    g_total_alloc_bytes += (int64_t)size;
    return p;
}

int kk_arena_owns(const void* ptr) {
    if (ptr == NULL) return 0;
    uintptr_t u = (uintptr_t)ptr;
    /* Fast path: if ptr is outside the global [lo, hi) range, it can't
     * be arena-owned. This avoids walking the slab list for the vast
     * majority of non-arena pointers (code ptrs, string ptrs, etc.). */
    if (u < g_arena_lo || u >= g_arena_hi) return 0;
    for (kk_slab* s = g_head; s != NULL; s = s->next) {
        uintptr_t lo = (uintptr_t)s->data;
        uintptr_t hi = lo + s->cap;
        if (u >= lo && u < hi) return 1;
    }
    return 0;
}

int kk_arena_maybe_owns(const void* ptr) {
    if (ptr == NULL) return 0;
    uintptr_t u = (uintptr_t)ptr;
    return u >= g_arena_lo && u < g_arena_hi;
}

void kk_arena_free(void* ptr) {
    if (ptr == NULL) return;
    if (kk_arena_maybe_owns(ptr)) return;  /* arena-owned: kk_drop calls
                                            * kk_arena_recycle_put instead;
                                            * see runtime/kk_runtime.c. */
    free(ptr);
}

void kk_arena_reset(void) {
    kk_slab* s = g_head;
    while (s != NULL) {
        kk_slab* next = s->next;
        free(s);
        s = next;
    }
    g_head = NULL;
    g_total_alloc_bytes = 0;
    g_total_slab_bytes  = 0;
}

int64_t kk_arena_bytes_allocated(void) { return g_total_alloc_bytes; }
int64_t kk_arena_bytes_reserved(void)  { return g_total_slab_bytes; }

/* ---- Checkpoint / rollback ---- */

kk_arena_checkpoint_t kk_arena_checkpoint(void) {
    kk_arena_checkpoint_t cp;
    cp.slab = (void*)g_head;
    cp.used = g_head ? g_head->used : 0;
    return cp;
}

void kk_arena_rollback(kk_arena_checkpoint_t cp) {
    kk_slab* target = (kk_slab*)cp.slab;
    /* Free all slabs newer than the checkpoint slab. */
    while (g_head != NULL && g_head != target) {
        kk_slab* next = g_head->next;
        g_total_slab_bytes -= (int64_t)g_head->cap;
        g_total_alloc_bytes -= (int64_t)g_head->used;
        free(g_head);
        g_head = next;
    }
    /* Reset the checkpoint slab's bump pointer. */
    if (g_head != NULL && g_head == target) {
        g_total_alloc_bytes -= (int64_t)(g_head->used - cp.used);
        g_head->used = cp.used;
    }
}

int kk_arena_in_rollback_region(const void* ptr, kk_arena_checkpoint_t cp) {
    if (ptr == NULL) return 0;
    uintptr_t u = (uintptr_t)ptr;
    kk_slab* target = (kk_slab*)cp.slab;
    /* Check slabs newer than checkpoint (all of these will be freed). */
    for (kk_slab* s = g_head; s != NULL && s != target; s = s->next) {
        uintptr_t lo = (uintptr_t)s->data;
        uintptr_t hi = lo + s->used;
        if (u >= lo && u < hi) return 1;
    }
    /* Check the checkpoint slab itself: bytes after cp.used are in the region. */
    if (target != NULL) {
        uintptr_t lo = (uintptr_t)target->data + cp.used;
        uintptr_t hi = (uintptr_t)target->data + target->used;
        if (u >= lo && u < hi) return 1;
    }
    return 0;
}

/* ---- Free-list recycling ---- */

/* Size classes: index = total_bytes / 8.  A cons cell with N fields
 * occupies (2+N)*8 bytes (rc + tag + fields), so class = 2+N.
 * Classes 0..KK_RECYCLE_CLASSES-1 each hold a singly-linked list of
 * dead blocks.  The first 8 bytes of a dead block store the next ptr. */
#define KK_RECYCLE_CLASSES 128  /* up to 126 fields */

static void* g_free_lists[KK_RECYCLE_CLASSES] = {0};

/* Freelist layout: blocks chain through their *second* 8-byte slot
 * (the "tag" position of the dead cell).  The first slot (rc word) is
 * kept as 0 — matches what kk_drop has already written before calling
 * recycle_put.  That way, a stale drop on a recycled cell sees rc=0
 * and early-returns via the "already freed or corrupt" guard instead
 * of treating the next pointer as a refcount and walking children. */
void* kk_arena_recycle(size_t size) {
    size_t cls = size / 8;
    if (cls >= KK_RECYCLE_CLASSES || !g_free_lists[cls]) return NULL;
    void* block = g_free_lists[cls];
    g_free_lists[cls] = ((void**)block)[1];
    return block;
}

void kk_arena_recycle_put(void* block, size_t size) {
    size_t cls = size / 8;
    if (cls >= KK_RECYCLE_CLASSES) return;  /* too big, let it leak */
    ((int64_t*)block)[0] = 0;                /* rc word: dead sentinel */
    ((void**)block)[1]   = g_free_lists[cls];/* tag slot: next ptr */
    g_free_lists[cls] = block;
}
