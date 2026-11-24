/*
 * test_organ_uring_rts.c — Unit tests for organ_uring_rts.h
 *
 * Verifies:
 *   1. organ_pack_user_data / organ_unpack_slot / organ_unpack_gen correctness.
 *   2. Acquisition and generation tagging.
 *   3. Cancellation safety: cancel does NOT free buffer; CQE arrival releases buffer and bumps generation.
 *   4. Replay attack rejection: old user_data token returns -ESTALE.
 *   5. Ring slot recycling in O(1).
 */
#include "organ_uring_rts.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TEST_CAPACITY 8

typedef struct {
    int release_calls;
    int resume_calls;
    void *last_released_buf;
    organ_mem_profile_t last_profile;
    int32_t last_cqe_res;
    uint32_t last_cqe_flags;
} test_state_t;

static void mock_release(void *buf_ptr, organ_mem_profile_t profile, int32_t cqe_res, void *user_ctx) {
    test_state_t *st = (test_state_t *)user_ctx;
    st->release_calls++;
    st->last_released_buf = buf_ptr;
    st->last_profile = profile;
    st->last_cqe_res = cqe_res;
}

static void mock_resume(void *cont_env, void *buf_ptr, int32_t cqe_res, uint32_t cqe_flags) {
    test_state_t *st = (test_state_t *)cont_env;
    (void)buf_ptr;
    st->resume_calls++;
    st->last_cqe_res = cqe_res;
    st->last_cqe_flags = cqe_flags;
}

static void test_pack_unpack(void) {
    uint32_t slot = 42;
    uint32_t gen = 1337;
    uint64_t token = organ_pack_user_data(slot, gen);
    assert(organ_unpack_slot(token) == slot);
    assert(organ_unpack_gen(token) == gen);
    printf("  [PASS] test_pack_unpack\n");
}

static void test_acquire_and_complete(void) {
    organ_flight_slot_t slots[TEST_CAPACITY];
    uint32_t next_free[TEST_CAPACITY];
    organ_core_reactor_t reactor;
    organ_reactor_init(&reactor, 0, TEST_CAPACITY, slots, next_free);

    test_state_t st;
    memset(&st, 0, sizeof(st));

    int dummy_buf = 0xbeef;
    uint64_t token = 0;
    int rc = organ_flight_acquire(&reactor,
                                  ORGAN_PROFILE_THREAD_LOCAL_PERCEUS,
                                  &dummy_buf,
                                  &st,
                                  mock_resume,
                                  mock_release,
                                  &st,
                                  &token);
    assert(rc == 0);
    assert(reactor.active_count == 1);
    assert(organ_unpack_slot(token) == 0);
    assert(organ_unpack_gen(token) == 1);

    /* Complete with CQE */
    rc = organ_flight_complete(&reactor, token, 128, 0);
    assert(rc == 0);
    assert(reactor.active_count == 0);
    assert(st.resume_calls == 1);
    assert(st.release_calls == 1);
    assert(st.last_released_buf == &dummy_buf);
    assert(st.last_profile == ORGAN_PROFILE_THREAD_LOCAL_PERCEUS);
    assert(st.last_cqe_res == 128);

    /* Slot should be free and generation bumped to 2 */
    assert(slots[0].state == ORGAN_FLIGHT_FREE);
    assert(slots[0].generation == 2);

    /* Stale token replay must fail with -ESTALE */
    rc = organ_flight_complete(&reactor, token, 128, 0);
    assert(rc == ORGAN_ERR_STALE);

    printf("  [PASS] test_acquire_and_complete\n");
}

static void test_cancellation_safety(void) {
    organ_flight_slot_t slots[TEST_CAPACITY];
    uint32_t next_free[TEST_CAPACITY];
    organ_core_reactor_t reactor;
    organ_reactor_init(&reactor, 1, TEST_CAPACITY, slots, next_free);

    test_state_t st;
    memset(&st, 0, sizeof(st));

    int dummy_buf = 0xcafe;
    uint64_t token = 0;
    int rc = organ_flight_acquire(&reactor,
                                  ORGAN_PROFILE_FREESTANDING_LINEAR,
                                  &dummy_buf,
                                  &st,
                                  mock_resume,
                                  mock_release,
                                  &st,
                                  &token);
    assert(rc == 0);
    assert(slots[0].state == ORGAN_FLIGHT_ACTIVE);

    /* Cancel flight: must transition to CANCELING without freeing buffer */
    rc = organ_flight_cancel(&reactor, token);
    assert(rc == 0);
    assert(slots[0].state == ORGAN_FLIGHT_CANCELING);
    assert(st.release_calls == 0); /* CRITICAL INVARIANT: NEVER premature free! */
    assert(st.resume_calls == 0);

    /* Cancel again should fail because not ACTIVE */
    rc = organ_flight_cancel(&reactor, token);
    assert(rc == ORGAN_ERR_STALE);

    /* Kernel returns CQE with -ECANCELED (-125) */
    rc = organ_flight_complete(&reactor, token, -125, 0);
    assert(rc == 0);
    assert(st.resume_calls == 0);  /* Resume was skipped because it was canceled */
    assert(st.release_calls == 1); /* Buffer ownership released now! */
    assert(st.last_released_buf == &dummy_buf);
    assert(st.last_profile == ORGAN_PROFILE_FREESTANDING_LINEAR);
    assert(st.last_cqe_res == -125);

    printf("  [PASS] test_cancellation_safety\n");
}

static void test_capacity_and_recycling(void) {
    organ_flight_slot_t slots[TEST_CAPACITY];
    uint32_t next_free[TEST_CAPACITY];
    organ_core_reactor_t reactor;
    organ_reactor_init(&reactor, 0, TEST_CAPACITY, slots, next_free);

    test_state_t st;
    memset(&st, 0, sizeof(st));

    uint64_t tokens[TEST_CAPACITY];
    for (uint32_t i = 0; i < TEST_CAPACITY; ++i) {
        int rc = organ_flight_acquire(&reactor,
                                      ORGAN_PROFILE_CONCURRENT_SHARED,
                                      NULL,
                                      &st,
                                      NULL,
                                      NULL,
                                      NULL,
                                      &tokens[i]);
        assert(rc == 0);
    }
    assert(reactor.active_count == TEST_CAPACITY);

    /* Attempting to acquire when full must return ORGAN_ERR_FULL (-EAGAIN) */
    uint64_t extra = 0;
    int rc = organ_flight_acquire(&reactor,
                                  ORGAN_PROFILE_CONCURRENT_SHARED,
                                  NULL,
                                  &st,
                                  NULL,
                                  NULL,
                                  NULL,
                                  &extra);
    assert(rc == ORGAN_ERR_FULL);

    /* Release half, re-acquire, verify LIFO/FIFO slot recycling */
    rc = organ_flight_complete(&reactor, tokens[2], 0, 0);
    assert(rc == 0);
    assert(reactor.active_count == TEST_CAPACITY - 1);

    uint64_t new_token = 0;
    rc = organ_flight_acquire(&reactor,
                              ORGAN_PROFILE_CONCURRENT_SHARED,
                              NULL,
                              &st,
                              NULL,
                              NULL,
                              NULL,
                              &new_token);
    assert(rc == 0);
    assert(organ_unpack_slot(new_token) == organ_unpack_slot(tokens[2]));
    assert(organ_unpack_gen(new_token) == organ_unpack_gen(tokens[2]) + 1);

    printf("  [PASS] test_capacity_and_recycling\n");
}

int main(void) {
    printf("=== Running test_organ_uring_rts ===\n");
    test_pack_unpack();
    test_acquire_and_complete();
    test_cancellation_safety();
    test_capacity_and_recycling();
    printf("=== All tests passed cleanly! ===\n");
    return 0;
}
