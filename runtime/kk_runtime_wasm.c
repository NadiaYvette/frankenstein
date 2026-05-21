/* Frankenstein Wasm Runtime — freestanding Perceus RC for WebAssembly
 *
 * Differences from native kk_runtime.c:
 *   - Bump allocator using Wasm linear memory (no malloc/free)
 *   - No cycle collector (acyclic programs only)
 *   - No stdio (result returned via exported function)
 *   - Values are i64, pointers are i32 (wasm32)
 *   - Pointer values stored as i64 with upper 32 bits zero
 *
 * Boxed value layout (all fields stored as i64, but addressed via i32 ptrs):
 *   [refcount:i64] [tag:i64] [field0:i64] [field1:i64] ...
 *                   ^--- the "pointer" (as i64) points here
 */

#include <stdint.h>
#include <stddef.h>

/* On wasm32, pointers are 32-bit but our values are i64.
 * The casts between int64_t and pointers are intentional. */
#pragma clang diagnostic ignored "-Wint-to-pointer-cast"
#pragma clang diagnostic ignored "-Wpointer-to-int-cast"

/* Helper: convert i64 value to native pointer */
#define PTR(x)  ((void*)(intptr_t)(x))
#define IPTR(x) ((int64_t*)(intptr_t)(x))

/* ---- Bump allocator ---- */

/* Simple bump allocator over a static buffer.
 * Wasm linear memory starts at 1 page (64KB) and wasm-ld can grow it.
 * For now, 1MB static heap is enough for all our benchmarks. */
#define KK_WASM_HEAP_SIZE (1024 * 1024)
static unsigned char kk_heap[KK_WASM_HEAP_SIZE] __attribute__((aligned(8)));
static size_t kk_heap_offset = 0;

static void* wasm_alloc(int64_t size) {
    /* Align to 8 bytes */
    kk_heap_offset = (kk_heap_offset + 7) & ~(size_t)7;
    if (kk_heap_offset + (size_t)size > KK_WASM_HEAP_SIZE) {
        return (void*)0;  /* OOM */
    }
    void* result = &kk_heap[kk_heap_offset];
    kk_heap_offset += (size_t)size;
    return result;
}

/* ---- Heap pointer check ---- */

static inline int kk_is_heap_ptr(int64_t ptr) {
    return ptr != 0 && (ptr & 7) == 0 && ptr > 4096;
}

/* ---- Refcount (at ptr - 8) ---- */

static inline int64_t* kk_rc_ptr(int64_t ptr) {
    return IPTR(ptr - 8);
}

/* ---- Public API (matches native runtime signatures) ---- */

void kk_retain(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* rc = kk_rc_ptr(ptr);
    *rc = *rc + 1;
}

void kk_drop(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* rc = kk_rc_ptr(ptr);
    if (*rc <= 1) {
        /* Refcount zero — recursively drop children.
         * We need nfields but have no side-table in wasm.
         * For now, just leak — correct for acyclic non-freeing demos.
         * TODO: embed nfields in the boxed layout. */
        *rc = 0;
    } else {
        *rc = *rc - 1;
    }
}

void kk_release(int64_t ptr) {
    kk_drop(ptr);
}

int64_t kk_reuse(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* rc = kk_rc_ptr(ptr);
    if (*rc == 1) return ptr;  /* sole owner */
    kk_drop(ptr);
    return 0;
}

int64_t kk_tag(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    return *IPTR(ptr);
}

int64_t kk_field(int64_t ptr, int64_t idx) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* fields = IPTR(ptr + 8);
    return fields[idx];
}

int64_t kk_alloc_con(int64_t tag, int64_t nfields) {
    int64_t total = (2 + nfields) * 8;  /* rc + tag + fields */
    int64_t* block = (int64_t*)wasm_alloc(total);
    if (!block) return 0;
    block[0] = 1;       /* refcount = 1 */
    block[1] = tag;      /* tag */
    for (int64_t i = 0; i < nfields; i++) {
        block[2 + i] = 0;
    }
    return (int64_t)(intptr_t)&block[1];  /* pointer to tag slot */
}

void kk_set_field(int64_t ptr, int64_t idx, int64_t value) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* fields = IPTR(ptr + 8);
    fields[idx] = value;
}

/* ---- Evidence vector (effects) ---- */

int64_t kk_evv_create(int64_t nops) {
    return kk_alloc_con(0x45565630, nops);
}

void kk_evv_set(int64_t evv, int64_t idx, int64_t handler_fn) {
    kk_set_field(evv, idx, handler_fn);
}

int64_t kk_evv_get(int64_t evv, int64_t idx) {
    return kk_field(evv, idx);
}

int64_t kk_unhandled_effect(void) {
    /* Can't fprintf in freestanding wasm — just return error sentinel */
    return -1;
}

/* ---- Mercury choice ---- */

static int64_t mercury_choice_decisions[64];
static int64_t mercury_choice_pos = 0;

int64_t mercury_choose(void) {
    return mercury_choice_decisions[mercury_choice_pos++];
}

int64_t mercury_collect_choices(int64_t fn_ptr) {
    typedef int64_t (*body_fn_t)(void);
    body_fn_t body = (body_fn_t)(intptr_t)fn_ptr;

    /* Discover max depth */
    for (int i = 0; i < 64; i++) mercury_choice_decisions[i] = 0;
    mercury_choice_pos = 0;
    body();
    int64_t max_depth = mercury_choice_pos;
    if (max_depth == 0) {
        mercury_choice_pos = 0;
        return body();
    }

    int64_t total_paths = 1LL << max_depth;
    int64_t sum = 0;
    for (int64_t path = 0; path < total_paths; path++) {
        for (int64_t i = 0; i < max_depth; i++) {
            mercury_choice_decisions[i] = (path >> i) & 1;
        }
        mercury_choice_pos = 0;
        int64_t result = body();
        int64_t depth = mercury_choice_pos;
        int64_t used_mask = (1LL << depth) - 1;
        if ((path & used_mask) == path) {
            sum += result;
        }
    }
    return sum;
}

/* ---- Thunk support ---- */

#define KK_THUNK_TAG 0x4C415A59

int64_t kk_thunk_create(int64_t closure_ptr) {
    int64_t thunk = kk_alloc_con(KK_THUNK_TAG, 2);
    if (thunk == 0) return 0;
    kk_set_field(thunk, 0, 0);
    kk_set_field(thunk, 1, closure_ptr);
    return thunk;
}

int64_t kk_thunk_force(int64_t thunk) {
    if (!kk_is_heap_ptr(thunk)) return thunk;
    int64_t tag = kk_tag(thunk);
    if (tag != KK_THUNK_TAG) return thunk;
    int64_t evaluated = kk_field(thunk, 0);
    if (evaluated) return kk_field(thunk, 1);
    int64_t closure = kk_field(thunk, 1);
    int64_t fn_ptr = kk_field(closure, 0);
    typedef int64_t (*thunk_fn_t)(int64_t);
    int64_t result = ((thunk_fn_t)(intptr_t)fn_ptr)(closure);
    kk_set_field(thunk, 0, 1);
    kk_set_field(thunk, 1, result);
    return result;
}
