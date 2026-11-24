/*
 * test_organ_uring_bridge.c — ASan/UBSan Verification for io_uring Perceus Bridge
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <errno.h>

#include "organ_rt.h"
#include "organ_uring_rts.h"
#include "organ_uring_bridge.h"

#define TEST_CAPACITY 16

static int g_resumed_count = 0;
static int32_t g_last_cqe_res = 0;

static void sample_continuation(void *env, void *buf, int32_t cqe_res, uint32_t flags) {
    (void)env;
    (void)buf;
    (void)flags;
    g_resumed_count++;
    g_last_cqe_res = cqe_res;
}

static void test_profile1_cancel(void) {
    organ_flight_slot_t slots[TEST_CAPACITY];
    uint32_t next_free[TEST_CAPACITY];
    organ_core_reactor_t reactor;
    organ_reactor_init(&reactor, 0, TEST_CAPACITY, slots, next_free);

    /* Allocate Profile 1 object: initial rc = 1 */
    organ_obj_t *obj = organ_alloc_ctor(0x12345678ULL, 2, NULL);
    assert(obj != NULL);
    assert(obj->rc == 1);

    uint64_t user_data = 0;
    int rc = organ_uring_submit_obj(&reactor,
                                    ORGAN_PROFILE_THREAD_LOCAL_PERCEUS,
                                    obj,
                                    NULL,
                                    sample_continuation,
                                    &user_data);
    assert(rc == 0);
    /* In flight: reactor retains ref, so rc is now 2 */
    assert(obj->rc == 2);
    assert(reactor.active_count == 1);

    /* Caller cancels the flight slot */
    rc = organ_flight_cancel(&reactor, user_data);
    assert(rc == 0);

    /* Caller drops its own reference. rc drops to 1, object is NOT freed (no UAF!) */
    organ_drop(obj);

    /* Kernel delivers CQE with -ECANCELED (-125) */
    g_resumed_count = 0;
    rc = organ_flight_complete(&reactor, user_data, -ECANCELED, 0);
    assert(rc == 0);
    /* Because it was canceling, continuation was NOT resumed */
    assert(g_resumed_count == 0);
    /* Slot is recycled */
    assert(reactor.active_count == 0);
    /* Object's remaining ref was dropped by organ_uring_perceus_release and freed.
     * ASan / LSan will verify 0 leaks and 0 heap-use-after-free. */
    printf("  [PASS] Profile 1 cancellation test: 0 UAF, clean release\n");
}

static void test_profile1_normal_completion(void) {
    organ_flight_slot_t slots[TEST_CAPACITY];
    uint32_t next_free[TEST_CAPACITY];
    organ_core_reactor_t reactor;
    organ_reactor_init(&reactor, 0, TEST_CAPACITY, slots, next_free);

    organ_obj_t *obj = organ_alloc_ctor(0x87654321ULL, 1, NULL);
    assert(obj != NULL);
    assert(obj->rc == 1);

    uint64_t user_data = 0;
    int rc = organ_uring_submit_obj(&reactor,
                                    ORGAN_PROFILE_THREAD_LOCAL_PERCEUS,
                                    obj,
                                    NULL,
                                    sample_continuation,
                                    &user_data);
    assert(rc == 0);
    assert(obj->rc == 2);

    /* Normal completion CQE delivers 4096 bytes read */
    g_resumed_count = 0;
    rc = organ_flight_complete(&reactor, user_data, 4096, 0);
    assert(rc == 0);
    assert(g_resumed_count == 1);
    assert(g_last_cqe_res == 4096);

    /* After CQE, reactor dropped its ref; caller still holds 1 ref */
    assert(obj->rc == 1);
    organ_drop(obj);
    assert(reactor.active_count == 0);
    printf("  [PASS] Profile 1 normal completion: rc decremented to 1 post-CQE, caller drops\n");
}

static void test_profile0_freestanding_linear(void) {
    organ_flight_slot_t slots[TEST_CAPACITY];
    uint32_t next_free[TEST_CAPACITY];
    organ_core_reactor_t reactor;
    organ_reactor_init(&reactor, 0, TEST_CAPACITY, slots, next_free);

    /* Profile 0: linear buffer (dummy buffer) */
    char linear_buf[64];
    uint64_t user_data = 0;

    int rc = organ_flight_acquire(&reactor,
                                  ORGAN_PROFILE_FREESTANDING_LINEAR,
                                  linear_buf,
                                  NULL,
                                  sample_continuation,
                                  organ_uring_perceus_release,
                                  NULL,
                                  &user_data);
    assert(rc == 0);
    assert(reactor.active_count == 1);

    g_resumed_count = 0;
    rc = organ_flight_complete(&reactor, user_data, 64, 0);
    assert(rc == 0);
    assert(g_resumed_count == 1);
    assert(reactor.active_count == 0);
    printf("  [PASS] Profile 0 linear move: 0 RC operations, linear return on CQE\n");
}

static void test_probe_kernel(void) {
    int probe = organ_uring_probe_kernel();
    assert(probe == 0 || probe == 1);
    printf("  [PASS] Kernel io_uring probe: returned %d (%s)\n",
           probe, probe == 1 ? "NativeSupported" : "SandboxBlockedFallback");
}

int main(void) {
    printf("=== Running test_organ_uring_bridge ===\n");
    test_profile1_cancel();
    test_profile1_normal_completion();
    test_profile0_freestanding_linear();
    test_probe_kernel();
    printf("=== All test_organ_uring_bridge tests passed ===\n");
    return 0;
}
