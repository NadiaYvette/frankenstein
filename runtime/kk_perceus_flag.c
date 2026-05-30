#include <stdlib.h>

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
