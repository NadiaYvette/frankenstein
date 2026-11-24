/*
 * organ_uring_rts.h — Freestanding C11 Per-Core io_uring Flight Table & Perceus Ownership ABI
 *
 * Shared contract between Frankenstein's zero-GHC-RTS runtime (liborgan_rt),
 * OrganIR's 3-Profile Memory Model, and Penelope.Uring.Plan.
 *
 * Cancellation-Safety Invariant:
 *   Once an SQE is bound to a flight slot, the reactor retains ownership of
 *   `buf_ptr` until the kernel delivers the matching CQE (completion or -ECANCELED).
 *   Caller cancellation NEVER frees or reuses `buf_ptr` early.
 */
#ifndef ORGAN_URING_RTS_H
#define ORGAN_URING_RTS_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* OrganIR 3-Profile Memory Model (mirrors Gargamelle.CodeGen.OrganIR). */
typedef enum {
    ORGAN_PROFILE_FREESTANDING_LINEAR = 0, /* Multiplicity = 1; 0 RC header, linear move */
    ORGAN_PROFILE_THREAD_LOCAL_PERCEUS = 1, /* Non-atomic Perceus RC (is_mt elided) */
    ORGAN_PROFILE_CONCURRENT_SHARED = 2     /* Atomic Release/Acquire RC (gpfsl Arc) */
} organ_mem_profile_t;

/* Lifecycle of a single in-flight SQE slot. */
typedef enum {
    ORGAN_FLIGHT_FREE = 0,
    ORGAN_FLIGHT_ACTIVE = 1,
    ORGAN_FLIGHT_CANCELING = 2
} organ_flight_state_t;

/* Callback invoked strictly upon CQE arrival to drop or recycle DMA buffer ownership. */
typedef void (*organ_buf_release_fn)(void *buf_ptr,
                                     organ_mem_profile_t profile,
                                     int32_t cqe_res,
                                     void *user_ctx);

/* Stackless continuation resumption entrypoint. */
typedef void (*organ_cont_resume_fn)(void *cont_env,
                                     void *buf_ptr,
                                     int32_t cqe_res,
                                     uint32_t cqe_flags);

typedef struct {
    uint32_t generation;
    organ_flight_state_t state;
    organ_mem_profile_t profile;
    void *buf_ptr;
    void *cont_env;
    organ_cont_resume_fn resume_fn;
    organ_buf_release_fn release_fn;
    void *release_ctx;
} organ_flight_slot_t;

typedef struct {
    uint32_t core_id;
    uint32_t capacity;
    uint32_t active_count;
    uint32_t free_head;
    uint32_t *next_free;        /* Intrusive O(1) free-list indices (UINT32_MAX = nil) */
    organ_flight_slot_t *slots; /* Caller-provided static or arena-backed array */
} organ_core_reactor_t;

#define ORGAN_SLOT_NIL UINT32_MAX
#define ORGAN_ERR_FULL (-11)   /* -EAGAIN */
#define ORGAN_ERR_STALE (-116) /* -ESTALE */

static inline uint64_t organ_pack_user_data(uint32_t slot, uint32_t gen) {
    return (((uint64_t)gen) << 32) | (uint64_t)slot;
}

static inline uint32_t organ_unpack_slot(uint64_t user_data) {
    return (uint32_t)(user_data & 0xFFFFFFFFULL);
}

static inline uint32_t organ_unpack_gen(uint64_t user_data) {
    return (uint32_t)(user_data >> 32);
}

static inline void organ_reactor_init(organ_core_reactor_t *r,
                                      uint32_t core_id,
                                      uint32_t capacity,
                                      organ_flight_slot_t *slots,
                                      uint32_t *next_free) {
    r->core_id = core_id;
    r->capacity = capacity;
    r->active_count = 0;
    r->slots = slots;
    r->next_free = next_free;
    r->free_head = (capacity > 0) ? 0u : ORGAN_SLOT_NIL;
    for (uint32_t i = 0; i < capacity; ++i) {
        slots[i].generation = 1u;
        slots[i].state = ORGAN_FLIGHT_FREE;
        slots[i].profile = ORGAN_PROFILE_FREESTANDING_LINEAR;
        slots[i].buf_ptr = NULL;
        slots[i].cont_env = NULL;
        slots[i].resume_fn = NULL;
        slots[i].release_fn = NULL;
        slots[i].release_ctx = NULL;
        next_free[i] = (i + 1u < capacity) ? (i + 1u) : ORGAN_SLOT_NIL;
    }
}

/* Acquires a flight slot in O(1), binding buffer ownership to the returned user_data token. */
static inline int organ_flight_acquire(organ_core_reactor_t *r,
                                       organ_mem_profile_t profile,
                                       void *buf_ptr,
                                       void *cont_env,
                                       organ_cont_resume_fn resume_fn,
                                       organ_buf_release_fn release_fn,
                                       void *release_ctx,
                                       uint64_t *out_user_data) {
    if (r->free_head == ORGAN_SLOT_NIL) {
        return ORGAN_ERR_FULL;
    }
    uint32_t idx = r->free_head;
    r->free_head = r->next_free[idx];
    r->next_free[idx] = ORGAN_SLOT_NIL;

    organ_flight_slot_t *s = &r->slots[idx];
    s->state = ORGAN_FLIGHT_ACTIVE;
    s->profile = profile;
    s->buf_ptr = buf_ptr;
    s->cont_env = cont_env;
    s->resume_fn = resume_fn;
    s->release_fn = release_fn;
    s->release_ctx = release_ctx;
    r->active_count++;

    *out_user_data = organ_pack_user_data(idx, s->generation);
    return 0;
}

/* Marks an in-flight SQE as canceling WITHOUT releasing buf_ptr before CQE arrival. */
static inline int organ_flight_cancel(organ_core_reactor_t *r, uint64_t user_data) {
    uint32_t idx = organ_unpack_slot(user_data);
    uint32_t gen = organ_unpack_gen(user_data);
    if (idx >= r->capacity) {
        return ORGAN_ERR_STALE;
    }
    organ_flight_slot_t *s = &r->slots[idx];
    if (s->state != ORGAN_FLIGHT_ACTIVE || s->generation != gen) {
        return ORGAN_ERR_STALE;
    }
    s->state = ORGAN_FLIGHT_CANCELING;
    return 0;
}

/* Reaps a CQE: validates ABA generation, resumes continuation if active, releases buffer, recycles slot. */
static inline int organ_flight_complete(organ_core_reactor_t *r,
                                        uint64_t user_data,
                                        int32_t cqe_res,
                                        uint32_t cqe_flags) {
    uint32_t idx = organ_unpack_slot(user_data);
    uint32_t gen = organ_unpack_gen(user_data);
    if (idx >= r->capacity) {
        return ORGAN_ERR_STALE;
    }
    organ_flight_slot_t *s = &r->slots[idx];
    if (s->state == ORGAN_FLIGHT_FREE || s->generation != gen) {
        return ORGAN_ERR_STALE;
    }

    bool was_canceling = (s->state == ORGAN_FLIGHT_CANCELING);
    void *buf = s->buf_ptr;
    void *env = s->cont_env;
    organ_mem_profile_t prof = s->profile;
    organ_cont_resume_fn resume = s->resume_fn;
    organ_buf_release_fn release = s->release_fn;
    void *rctx = s->release_ctx;

    /* Bump generation (skip 0) and recycle slot BEFORE invoking callbacks. */
    s->generation = (s->generation == UINT32_MAX) ? 1u : (s->generation + 1u);
    s->state = ORGAN_FLIGHT_FREE;
    s->buf_ptr = NULL;
    s->cont_env = NULL;
    s->resume_fn = NULL;
    s->release_fn = NULL;
    s->release_ctx = NULL;
    r->next_free[idx] = r->free_head;
    r->free_head = idx;
    r->active_count--;

    if (!was_canceling && resume != NULL) {
        resume(env, buf, cqe_res, cqe_flags);
    }
    if (release != NULL) {
        release(buf, prof, cqe_res, rctx);
    }
    return 0;
}

#ifdef __cplusplus
}
#endif

#endif /* ORGAN_URING_RTS_H */
