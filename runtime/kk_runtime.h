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

/* Print an ADT-valued result as an s-expression, terminated by newline.
 * Heap pointers print as (#tag f0 f1 ...); scalars print as decimal ints. */
void    kk_println_con(int64_t v);

/* Boxed value construction */
int64_t kk_alloc_con(int64_t tag, int64_t nfields);
void    kk_set_field(int64_t ptr, int64_t idx, int64_t value);

/* First-class strings.
 *
 * A Frankenstein string is passed as an int64_t that, when cast to
 * const char*, points at a NUL-terminated UTF-8 byte sequence. String
 * literals are emitted as llvm.mlir.global constants and flow through
 * the pipeline as i64 via ptrtoint; these helpers return freshly
 * malloc'd strings that the caller owns (no refcounting yet — strings
 * are leaked, which is fine for short-lived test programs).
 *
 * kk_str_show_int formats a signed integer as decimal digits.
 */
void    kk_println_str(int64_t s);
int64_t kk_str_concat(int64_t a, int64_t b);
int64_t kk_str_len(int64_t s);
int64_t kk_str_eq(int64_t a, int64_t b);
int64_t kk_str_show_int(int64_t n);

#endif /* KK_RUNTIME_H */
