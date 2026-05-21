/* kk_runtime_profile.c — Instrumented version of kk_runtime.c
 * Counts retain/drop/alloc/reuse calls for profiling.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

/* Counters */
static int64_t kk_retain_count = 0;
static int64_t kk_retain_heap_count = 0;
static int64_t kk_drop_count = 0;
static int64_t kk_drop_heap_count = 0;
static int64_t kk_drop_free_count = 0;
static int64_t kk_alloc_count = 0;
static int64_t kk_reuse_count = 0;
static int64_t kk_reuse_hit_count = 0;

/* Print stats at exit */
__attribute__((destructor))
static void kk_print_profile(void) {
    fprintf(stderr, "\n=== Perceus RC Profile ===\n");
    fprintf(stderr, "kk_retain:     %ld total, %ld on heap objects\n", kk_retain_count, kk_retain_heap_count);
    fprintf(stderr, "kk_drop:       %ld total, %ld on heap objects, %ld freed\n", kk_drop_count, kk_drop_heap_count, kk_drop_free_count);
    fprintf(stderr, "kk_alloc_con:  %ld\n", kk_alloc_count);
    fprintf(stderr, "kk_reuse:      %ld total, %ld reused\n", kk_reuse_count, kk_reuse_hit_count);
    fprintf(stderr, "RC overhead:   %ld ops (%ld retain + %ld drop)\n",
            kk_retain_count + kk_drop_count, kk_retain_count, kk_drop_count);
    fprintf(stderr, "==========================\n");
}

/* --- Copy of kk_runtime.c with instrumentation --- */

static inline int kk_is_heap_ptr(int64_t ptr) {
    return ptr != 0 && (ptr & 7) == 0 && ptr > 4096;
}

static inline int64_t* kk_rc_ptr(int64_t ptr) {
    return (int64_t*)(ptr - 8);
}

void kk_retain(int64_t ptr) {
    kk_retain_count++;
    if (!kk_is_heap_ptr(ptr)) return;
    kk_retain_heap_count++;
    (*kk_rc_ptr(ptr))++;
}

void kk_drop(int64_t ptr) {
    kk_drop_count++;
    if (!kk_is_heap_ptr(ptr)) return;
    kk_drop_heap_count++;
    int64_t* rc = kk_rc_ptr(ptr);
    if (--(*rc) <= 0) {
        kk_drop_free_count++;
        free((void*)(ptr - 8));
    }
}

void kk_release(int64_t ptr) {
    kk_drop(ptr);
}

int64_t kk_reuse(int64_t ptr) {
    kk_reuse_count++;
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* rc = kk_rc_ptr(ptr);
    if (*rc == 1) {
        kk_reuse_hit_count++;
        return ptr;
    }
    kk_drop(ptr);
    return 0;
}

int64_t kk_tag(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    return *(int64_t*)ptr;
}

int64_t kk_field(int64_t ptr, int64_t idx) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* fields = (int64_t*)(ptr + 8);
    return fields[idx];
}

int64_t kk_alloc_con(int64_t tag, int64_t nfields) {
    kk_alloc_count++;
    int64_t total = (2 + nfields) * 8;
    int64_t* block = (int64_t*)malloc((size_t)total);
    if (!block) return 0;
    block[0] = 1;
    block[1] = tag;
    for (int64_t i = 0; i < nfields; i++) {
        block[2 + i] = 0;
    }
    return (int64_t)&block[1];
}

void kk_set_field(int64_t ptr, int64_t idx, int64_t value) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* fields = (int64_t*)(ptr + 8);
    fields[idx] = value;
}

int64_t kk_evv_create(int64_t nops) { return kk_alloc_con(0x45565630, nops); }
void kk_evv_set(int64_t evv, int64_t idx, int64_t h) { kk_set_field(evv, idx, h); }
int64_t kk_evv_get(int64_t evv, int64_t idx) { return kk_field(evv, idx); }
int64_t kk_unhandled_effect(void) { fprintf(stderr, "unhandled effect\n"); exit(1); return 0; }

int64_t mercury_choose(void) { return 0; }
int64_t mercury_collect_choices(int64_t fn_ptr) { return ((int64_t(*)(void))fn_ptr)(); }

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
    if (kk_tag(thunk) != KK_THUNK_TAG) return thunk;
    int64_t ev = kk_field(thunk, 0);
    if (ev) return kk_field(thunk, 1);
    int64_t closure = kk_field(thunk, 1);
    int64_t fn = kk_field(closure, 0);
    int64_t r = ((int64_t(*)(int64_t))fn)(closure);
    kk_set_field(thunk, 0, 1);
    kk_set_field(thunk, 1, r);
    return r;
}

/* --- Stubs for functions added after the profile runtime was created --- */

static int kk_argc_g = 0;
static char** kk_argv_g = NULL;
void kk_args_init(int argc, char** argv) { kk_argc_g = argc; kk_argv_g = argv; }
int64_t kk_args_count(void) { return kk_argc_g; }
int64_t kk_args_get(int64_t i) { return 0; }
int64_t kk_args_progname(void) { return 0; }
void kk_exit(int64_t code) { exit((int)code); }
int64_t kk_structural_eq(int64_t a, int64_t b) { return a == b ? 1 : 0; }
void kk_println_con(int64_t x) { fprintf(stderr, "<con %ld>\n", x); }
