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

/* First-class strings — rope-based, UTF-8.
 *
 * A Frankenstein string is an int64_t holding the address of a
 * heap-allocated kk_string_t header. Strings are immutable ropes:
 *
 *   LEAF   — points at a UTF-8 byte sequence (owned or borrowed)
 *   CONCAT — internal node holding two child strings; concatenation is O(1)
 *
 * Each header carries a refcount (Perceus-compatible) and a cached
 * total byte length, so kk_str_len is O(1). Character counting walks
 * the rope and respects UTF-8 multi-byte sequences (kk_str_char_len).
 *
 * String literals from .rodata are wrapped via kk_string_from_literal
 * with owns_bytes=0 — the leaf borrows the static bytes and never
 * frees them. Newly produced strings (from concat, show_int, etc.)
 * own their headers and, where applicable, their byte buffers.
 *
 * ByteStrings share the same underlying representation but expose a
 * byte-oriented API (no UTF-8 awareness, raw byte indexing).
 */

/* String construction */
int64_t kk_string_from_literal(int64_t bytes_ptr, int64_t byte_len);
int64_t kk_string_from_cstr(int64_t cstr_ptr);
int64_t kk_string_empty(void);

/* String queries (all O(1) except char_len, which walks the rope) */
int64_t kk_str_len(int64_t s);          /* total UTF-8 byte length */
int64_t kk_str_char_len(int64_t s);     /* Unicode codepoint count */
int64_t kk_str_eq(int64_t a, int64_t b);

/* String operations */
int64_t kk_str_concat(int64_t a, int64_t b);   /* O(1) — builds CONCAT node */
int64_t kk_str_flatten(int64_t s);             /* force into a single LEAF */
int64_t kk_str_show_int(int64_t n);

/* I/O */
void    kk_println_str(int64_t s);
void    kk_print_str(int64_t s);

/* Refcounting (Perceus-compatible) */
void    kk_str_retain(int64_t s);
void    kk_str_drop(int64_t s);

/* ByteString — same representation, byte-oriented API */
int64_t kk_bytes_from_literal(int64_t bytes_ptr, int64_t byte_len);
int64_t kk_bytes_len(int64_t b);
int64_t kk_bytes_concat(int64_t a, int64_t b);
int64_t kk_bytes_index(int64_t b, int64_t i);
int64_t kk_bytes_eq(int64_t a, int64_t b);

#endif /* KK_RUNTIME_H */
