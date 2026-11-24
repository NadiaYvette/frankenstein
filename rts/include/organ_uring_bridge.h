/*
 * organ_uring_bridge.h — Perceus <-> Per-Core io_uring Ownership Bridge
 *
 * Couples organ_rt.h (Perceus 3-profile heap objects) with organ_uring_rts.h
 * (per-core ABA-tagged flight table) so caller cancellation cannot UAF DMA buffers.
 */
#ifndef ORGAN_URING_BRIDGE_H
#define ORGAN_URING_BRIDGE_H

#include "organ_rt.h"
#include "organ_uring_rts.h"

#if defined(__linux__) && defined(__NR_io_uring_setup)
#include <unistd.h>
#include <sys/syscall.h>
#include <errno.h>
#include <string.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Runtime kernel io_uring availability probe.
 * Returns:
 *   1 if io_uring is natively supported by the kernel and permitted by seccomp
 *   0 if unsupported, blocked by sandbox seccomp policy, or running in simulator mode
 */
static inline int organ_uring_probe_kernel(void) {
#if defined(__linux__) && defined(__NR_io_uring_setup)
    char params[256];
    memset(params, 0, sizeof(params));
    long res = syscall(__NR_io_uring_setup, 0, params);
    if (res < 0) {
        if (errno == EINVAL || errno == EFAULT) {
            return 1;
        }
        return 0;
    }
    close((int)res);
    return 1;
#else
    return 0;
#endif
}

/* Reactor CQE release callback: drops Perceus refcount for Profile 1/2 strictly post-CQE. */
static inline void organ_uring_perceus_release(void *buf_ptr,
                                               organ_mem_profile_t profile,
                                               int32_t cqe_res,
                                               void *user_ctx) {
    (void)cqe_res;
    (void)user_ctx;
#ifndef ORGAN_FREESTANDING
    if (buf_ptr != NULL && profile != ORGAN_PROFILE_FREESTANDING_LINEAR) {
        organ_drop((organ_obj_t *)buf_ptr);
    }
#else
    (void)buf_ptr;
    (void)profile;
#endif
}

/* Binds a Perceus object to an io_uring flight slot with profile-appropriate ownership. */
static inline int organ_uring_submit_obj(organ_core_reactor_t *r,
                                         organ_mem_profile_t profile,
                                         organ_obj_t *buf_obj,
                                         void *cont_env,
                                         organ_cont_resume_fn resume_fn,
                                         uint64_t *out_user_data) {
    if (buf_obj != NULL && profile != ORGAN_PROFILE_FREESTANDING_LINEAR) {
        organ_dup(buf_obj);
    }
    int rc = organ_flight_acquire(r,
                                  profile,
                                  (void *)buf_obj,
                                  cont_env,
                                  resume_fn,
                                  organ_uring_perceus_release,
                                  NULL,
                                  out_user_data);
    if (rc != 0 && buf_obj != NULL && profile != ORGAN_PROFILE_FREESTANDING_LINEAR) {
#ifndef ORGAN_FREESTANDING
        organ_drop(buf_obj);
#endif
    }
    return rc;
}

#ifdef __cplusplus
}
#endif

#endif /* ORGAN_URING_BRIDGE_H */
