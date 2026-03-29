/* Frankenstein minimal runtime — Perceus refcounting + boxed values
 *
 * Boxed value layout (all fields int64_t):
 *   [refcount] [tag] [field0] [field1] ...
 *              ^--- the "pointer" points here (to the tag)
 *
 * The refcount lives at (ptr - 8). Fields start at (ptr + 8).
 * A null/zero pointer means "unboxed integer" and is not dereferenced.
 */

#include <stdlib.h>
#include <stdint.h>

/* Raw allocation: size in bytes */
void* kk_alloc(int64_t size) {
    return malloc((size_t)size);
}

void kk_free(void* ptr) {
    free(ptr);
}

/* Check if a value is a heap pointer (vs an unboxed integer).
 * Heap pointers from kk_alloc_con are 8-byte aligned, so the low 3 bits
 * are zero and the value is above a reasonable threshold.
 * Small integers and other non-pointer values are skipped. */
static inline int kk_is_heap_ptr(int64_t ptr) {
    /* Must be non-zero, 8-byte aligned, and in a plausible heap range.
     * Values below 4096 are almost certainly not valid heap pointers. */
    return ptr != 0 && (ptr & 7) == 0 && ptr > 4096;
}

/* Refcount helpers — pointer to refcount is at (ptr - 8) */
static inline int64_t* kk_rc_ptr(int64_t ptr) {
    return (int64_t*)(ptr - 8);
}

void kk_retain(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    (*kk_rc_ptr(ptr))++;
}

void kk_drop(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* rc = kk_rc_ptr(ptr);
    if (--(*rc) <= 0) {
        /* Free the whole block. The malloc'd pointer is at (ptr - 8). */
        free((void*)(ptr - 8));
    }
}

void kk_release(int64_t ptr) {
    kk_drop(ptr);
}

int64_t kk_reuse(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* rc = kk_rc_ptr(ptr);
    if (*rc == 1) {
        /* Sole owner — reuse the allocation */
        return ptr;
    }
    /* Shared — can't reuse, caller must allocate fresh */
    kk_drop(ptr);
    return 0;
}

/* Read the tag from a boxed value */
int64_t kk_tag(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    return *(int64_t*)ptr;
}

/* Read field[idx] from a boxed value (fields start after the tag) */
int64_t kk_field(int64_t ptr, int64_t idx) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* fields = (int64_t*)(ptr + 8);
    return fields[idx];
}

/* Allocate a constructor: tag + nfields payload slots.
 * Returns pointer to the tag (not the refcount).
 * Layout: [rc=1] [tag] [f0] [f1] ... */
int64_t kk_alloc_con(int64_t tag, int64_t nfields) {
    int64_t total = (2 + nfields) * 8;  /* rc + tag + fields */
    int64_t* block = (int64_t*)malloc((size_t)total);
    if (!block) return 0;
    block[0] = 1;          /* refcount = 1 */
    block[1] = tag;         /* tag */
    /* Zero-init fields */
    for (int64_t i = 0; i < nfields; i++) {
        block[2 + i] = 0;
    }
    /* Return pointer to the tag slot */
    return (int64_t)&block[1];
}

/* Write field[idx] of a boxed value */
void kk_set_field(int64_t ptr, int64_t idx, int64_t value) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* fields = (int64_t*)(ptr + 8);
    fields[idx] = value;
}

/* Evidence vector operations for algebraic effects
 *
 * An evidence vector holds function pointers (as i64) to handler
 * operations for a particular effect.  It is allocated as a regular
 * boxed constructor with tag 0xEVV0.
 */

#include <stdio.h>

#define KK_EVV_TAG 0x45565630  /* "EVV0" */

/* Create an evidence vector with nops operation slots */
int64_t kk_evv_create(int64_t nops) {
    return kk_alloc_con(KK_EVV_TAG, nops);
}

/* Set operation at index idx in the evidence vector */
void kk_evv_set(int64_t evv, int64_t idx, int64_t handler_fn) {
    kk_set_field(evv, idx, handler_fn);
}

/* Get the operation function pointer at index idx */
int64_t kk_evv_get(int64_t evv, int64_t idx) {
    return kk_field(evv, idx);
}

/* Default handler for unhandled effect operations -- abort with message */
int64_t kk_unhandled_effect(void) {
    fprintf(stderr, "frankenstein: unhandled effect operation\n");
    exit(1);
    return 0;
}

/* Thunk support for lazy evaluation (Haskell bridge)
 *
 * Thunk layout (using kk_alloc_con with tag=0xLAZY):
 *   [refcount] [tag=0x4C415A59] [evaluated_flag] [value_or_fn_ptr]
 *
 * evaluated_flag: 0 = unevaluated (value_or_fn_ptr is fn_ptr)
 *                 1 = evaluated   (value_or_fn_ptr is cached result)
 */

#define KK_THUNK_TAG 0x4C415A59  /* "LAZY" */

/* Create a thunk wrapping a zero-arg function pointer */
int64_t kk_thunk_create(int64_t fn_ptr) {
    int64_t thunk = kk_alloc_con(KK_THUNK_TAG, 2);
    if (thunk == 0) return 0;
    kk_set_field(thunk, 0, 0);        /* evaluated_flag = 0 */
    kk_set_field(thunk, 1, fn_ptr);   /* the function pointer */
    return thunk;
}

/* Force a thunk: if unevaluated, call the function and cache the result */
int64_t kk_thunk_force(int64_t thunk) {
    if (!kk_is_heap_ptr(thunk)) return thunk;  /* not a thunk, return as-is */
    int64_t tag = kk_tag(thunk);
    if (tag != KK_THUNK_TAG) return thunk;     /* not a thunk, return as-is */
    int64_t evaluated = kk_field(thunk, 0);
    if (evaluated) {
        return kk_field(thunk, 1);             /* already forced */
    }
    /* Call the zero-arg function */
    int64_t fn_ptr = kk_field(thunk, 1);
    typedef int64_t (*thunk_fn_t)(void);
    int64_t result = ((thunk_fn_t)fn_ptr)();
    /* Cache the result */
    kk_set_field(thunk, 0, 1);                /* mark as evaluated */
    kk_set_field(thunk, 1, result);           /* store result */
    return result;
}
