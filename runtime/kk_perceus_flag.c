#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

/* Host-side env-var dispatch for FRANKENSTEIN_NEW_PERCEUS.
 * Mirrors kk_use_new_perceus() in kk_runtime.c so the host binary
 * can use the same FFI shape as the self-hosted stages. */
int kk_use_new_perceus(void) {
    static int initialized = 0;
    static int value = 0;
    if (!initialized) {
        const char* v = getenv("FRANKENSTEIN_NEW_PERCEUS");
        value = (v && v[0] == '1' && v[1] == '\0') ? 1 : 0;
        initialized = 1;
    }
    return value;
}

/* Host-side stub for kk_debug_dump.  The host has no kk_runtime heap
 * layout, so we just print the raw pointer and label for cross-checking
 * call shapes — the meaningful output is from the self-hosted binary
 * where the cell layout matches. */
int kk_debug_dump(int64_t ptr, int64_t label) {
    fprintf(stderr, "[host debug_dump %ld] ptr=0x%lx\n",
            (long)label, (unsigned long)ptr);
    return 0;
}
