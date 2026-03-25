/* Frankenstein minimal runtime — Perceus refcounting + boxed values
 *
 * Boxed value layout:
 *   [refcount: int64_t] [tag: int64_t] [field0: int64_t] [field1: int64_t] ...
 *
 * A "pointer" in the Frankenstein IR is an int64_t pointing to the tag field.
 * The refcount lives at offset -8 from the tag.
 */

#ifndef KK_RUNTIME_H
#define KK_RUNTIME_H

#include <stdint.h>

/* Allocation */
void*   kk_alloc(int64_t size);
void    kk_free(void* ptr);

/* Refcounting */
void    kk_drop(int64_t ptr);
void    kk_retain(int64_t ptr);
void    kk_release(int64_t ptr);
int64_t kk_reuse(int64_t ptr);

/* Boxed value access */
int64_t kk_tag(int64_t ptr);
int64_t kk_field(int64_t ptr, int64_t idx);

/* Boxed value construction */
int64_t kk_alloc_con(int64_t tag, int64_t nfields);
void    kk_set_field(int64_t ptr, int64_t idx, int64_t value);

#endif /* KK_RUNTIME_H */
