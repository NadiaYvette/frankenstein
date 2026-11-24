/*
 * test_organ_dso.c — POSIX dlopen/dlsym/dlclose Integration & Safe Unload Test
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <dlfcn.h>

#include "organ_rt.h"
#include "organ_dso.h"

static void *host_alloc(size_t sz) {
    return malloc(sz);
}

static void host_free(void *ptr) {
    free(ptr);
}

int main(int argc, char **argv) {
    const char *so_path = (argc > 1) ? argv[1] : "/tmp/libsample_organ_plugin.so";
    printf("=== Testing organ_dso dynamic loading on: %s ===\n", so_path);

    void *h = dlopen(so_path, RTLD_NOW | RTLD_LOCAL);
    if (!h) {
        fprintf(stderr, "dlopen failed: %s\n", dlerror());
        return 1;
    }

    organ_dso_get_meta_fn get_meta = (organ_dso_get_meta_fn)dlsym(h, "organ_dso_get_meta");
    assert(get_meta != NULL);
    const organ_dso_meta_t *meta = get_meta();
    assert(meta != NULL);
    assert(organ_dso_verify_meta(meta, 0xCAFEF00DULL) == 0);
    printf("  [PASS] organ_dso_get_meta: ABI version %u, interface_hash 0x%lx\n",
           meta->abi_version, (unsigned long)meta->interface_hash);

    organ_dso_handle_t dso_handle;
    organ_dso_handle_init(&dso_handle, h, meta);

    organ_rt_host_table_t host_table = {
        .abi_version = ORGAN_DSO_ABI_VERSION,
        .host_alloc = host_alloc,
        .host_free = host_free,
        .host_uring_reactor = NULL
    };

    organ_dso_init_fn init_fn = (organ_dso_init_fn)dlsym(h, "organ_dso_init");
    assert(init_fn != NULL);
    assert(init_fn(&host_table, &dso_handle) == 0);
    printf("  [PASS] organ_dso_init succeeded with injected host table\n");

    typedef organ_obj_t *(*create_fn_t)(void);
    create_fn_t create_fn = (create_fn_t)dlsym(h, "sample_create_obj");
    assert(create_fn != NULL);

    organ_obj_t *obj = create_fn();
    assert(obj != NULL);
    assert(dso_handle.live_objects == 1);
    assert(organ_dso_can_unload(&dso_handle) == false);
    printf("  [PASS] Object created: live_objects = 1, organ_dso_can_unload = false (dlclose prevented)\n");

    /* Dropping the object triggers plugin_drop_fields, decrementing live_objects to 0 */
    organ_drop(obj);
    assert(dso_handle.live_objects == 0);
    assert(organ_dso_can_unload(&dso_handle) == true);
    printf("  [PASS] Object dropped: live_objects = 0, organ_dso_can_unload = true\n");

    int rc = dlclose(h);
    assert(rc == 0);
    printf("  [PASS] dlclose succeeded cleanly with 0 live references\n");

    printf("=== All test_organ_dso tests passed ===\n");
    return 0;
}
