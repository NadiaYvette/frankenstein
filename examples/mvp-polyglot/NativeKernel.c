#include "organ_rt.h"
#include <stdint.h>
#include <stdio.h>

int64_t NativeKernel_compute(int64_t x) {
    return x * 2 + 1;
}
