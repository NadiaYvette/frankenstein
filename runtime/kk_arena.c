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
    return s;
}

void* kk_arena_alloc(size_t size) {
    if (!arena_enabled()) return NULL;
    if (size == 0) size = KK_ARENA_ALIGN;
    size = round_up(size, KK_ARENA_ALIGN);

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
    for (kk_slab* s = g_head; s != NULL; s = s->next) {
        uintptr_t lo = (uintptr_t)s->data;
        uintptr_t hi = lo + s->cap;
        if (u >= lo && u < hi) return 1;
    }
    return 0;
}

void kk_arena_free(void* ptr) {
    if (ptr == NULL) return;
    if (kk_arena_owns(ptr)) return;   /* arena cells outlive individual frees */
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
