/*
 * organ_rt.h — Ingolstadt / Frankenstein Zero-GHC-RTS C11 Runtime
 *
 * Supports:
 *   - Profile 0 (#[freestanding_linear]): compile with -DORGAN_FREESTANDING
 *     (zero malloc, zero free, zero RC headers, in-place struct mutation).
 *   - Profile 1 (#[thread_local_perceus]): non-atomic Perceus RC (is_mt elided),
 *     in-place reuse via organ_is_unique(), and ICFP'25 first-order lazy constructors.
 */
#ifndef ORGAN_RT_H
#define ORGAN_RT_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifndef ORGAN_FREESTANDING
#include <stdlib.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    ORGAN_KIND_CTOR      = 0, /* Strict algebraic data constructor */
    ORGAN_KIND_CLOSURE   = 1, /* Partial application / heap closure */
    ORGAN_KIND_THUNK     = 2, /* Unevaluated first-order lazy constructor */
    ORGAN_KIND_BLACKHOLE = 3, /* Cycle detection during thunk forcing */
    ORGAN_KIND_EVALUATED = 4  /* Memoized indirection holding forced value */
} organ_obj_kind_t;

typedef struct organ_obj organ_obj_t;

/* Per-type field drop callback invoked when rc reaches 0. */
typedef void (*organ_drop_fields_fn)(organ_obj_t *obj);

/* First-order lazy thunk evaluator: computes value from captured env slots. */
typedef int64_t (*organ_thunk_eval_fn)(organ_obj_t *thunk_obj);

/* Compact 32-byte Perceus heap header (Profile 1: non-atomic thread-local RC). */
struct organ_obj {
    uint32_t rc;
    uint16_t kind;
    uint16_t arity;
    uint64_t content_tag; /* Deterministic 64-bit FNV-1a ContentTag from .organ-iface */
    organ_drop_fields_fn drop_fields;
    int64_t fields[];     /* Flexible array: unboxed scalars or tagged organ_obj_t* */
};

/* First-order lazy constructor object sharing the organ_obj_t prefix layout. */
typedef struct {
    uint32_t rc;
    uint16_t kind;
    uint16_t env_arity;
    uint64_t content_tag;
    organ_drop_fields_fn drop_fields;
    organ_thunk_eval_fn eval_fn;
    int64_t cached_val;
    int64_t env[];
} organ_thunk_t;

static inline bool organ_is_unique(const organ_obj_t *obj) {
    return (obj != NULL) && (obj->rc == 1u);
}

static inline organ_obj_t *organ_dup(organ_obj_t *obj) {
    if (obj != NULL) {
        obj->rc += 1u;
    }
    return obj;
}

#ifndef ORGAN_FREESTANDING

static inline organ_obj_t *organ_alloc_ctor(uint64_t content_tag,
                                            uint16_t arity,
                                            organ_drop_fields_fn drop_fn) {
    size_t bytes = sizeof(organ_obj_t) + ((size_t)arity * sizeof(int64_t));
    organ_obj_t *obj = (organ_obj_t *)malloc(bytes);
    if (obj != NULL) {
        obj->rc = 1u;
        obj->kind = (uint16_t)ORGAN_KIND_CTOR;
        obj->arity = arity;
        obj->content_tag = content_tag;
        obj->drop_fields = drop_fn;
    }
    return obj;
}

/* Perceus reuse primitive: reuses `reuse_tok` in place if unique & matching arity. */
static inline organ_obj_t *organ_reuse_or_alloc(organ_obj_t *reuse_tok,
                                                uint64_t content_tag,
                                                uint16_t arity,
                                                organ_drop_fields_fn drop_fn) {
    if (reuse_tok != NULL && reuse_tok->rc == 1u && reuse_tok->arity == arity) {
        reuse_tok->kind = (uint16_t)ORGAN_KIND_CTOR;
        reuse_tok->content_tag = content_tag;
        reuse_tok->drop_fields = drop_fn;
        return reuse_tok;
    }
    return organ_alloc_ctor(content_tag, arity, drop_fn);
}

static inline void organ_drop(organ_obj_t *obj) {
    if (obj == NULL) {
        return;
    }
    if (obj->rc > 1u) {
        obj->rc -= 1u;
        return;
    }
    if (obj->drop_fields != NULL) {
        obj->drop_fields(obj);
    }
    free(obj);
}

static inline organ_thunk_t *organ_alloc_thunk(uint64_t content_tag,
                                               uint16_t env_arity,
                                               organ_thunk_eval_fn eval_fn,
                                               organ_drop_fields_fn drop_fn) {
    size_t bytes = sizeof(organ_thunk_t) + ((size_t)env_arity * sizeof(int64_t));
    organ_thunk_t *th = (organ_thunk_t *)malloc(bytes);
    if (th != NULL) {
        th->rc = 1u;
        th->kind = (uint16_t)ORGAN_KIND_THUNK;
        th->env_arity = env_arity;
        th->content_tag = content_tag;
        th->drop_fields = drop_fn;
        th->eval_fn = eval_fn;
        th->cached_val = 0;
    }
    return th;
}

/* Forces a first-order lazy constructor once, memoizing its value in place. */
static inline int64_t organ_force(organ_thunk_t *th) {
    if (th->kind == (uint16_t)ORGAN_KIND_EVALUATED) {
        return th->cached_val;
    }
    if (th->kind == (uint16_t)ORGAN_KIND_BLACKHOLE) {
        abort(); /* Re-entrant cyclic thunk blackhole */
    }
    th->kind = (uint16_t)ORGAN_KIND_BLACKHOLE;
    int64_t val = th->eval_fn((organ_obj_t *)th);
    /* Release captured environment slots now that evaluation finished. */
    if (th->drop_fields != NULL) {
        th->drop_fields((organ_obj_t *)th);
        th->drop_fields = NULL;
    }
    th->cached_val = val;
    th->kind = (uint16_t)ORGAN_KIND_EVALUATED;
    return val;
}

#endif /* !ORGAN_FREESTANDING */

#ifdef __cplusplus
}
#endif

#endif /* ORGAN_RT_H */
