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

/* Walk a Haskell [Char] cons-list and print each char.  Uses the
 * hash-based tags stableConTag "[]" = 31636 and stableConTag ":" =
 * 46589.  Codepoints > 127 are written verbatim (no UTF-8 re-encoding).
 * println_* appends a newline; print_* writes the bytes verbatim. */
void    kk_print_haskell_chars(int64_t list);
void    kk_println_haskell_chars(int64_t list);

/* Build a [Char] cons-list for decimal-formatted n, prepending onto
 * tail.  Used by the GHC bridge to implement Show Int. */
int64_t kk_int_to_haskell_chars(int64_t n, int64_t tail);

/* Format a [Int] cons-list as "[n1,n2,n3]" prepended onto tail.  Used
 * by the GHC bridge to implement Show [Int] (the specialised
 * $fShowInt_$cshowList method). */
int64_t kk_int_list_to_haskell_chars(int64_t list, int64_t tail);

/* Append two Haskell [Char] cons-lists (a ++ b).  Used by the GHC
 * bridge to translate GHC.Internal.Base.(++) calls in derived Show. */
int64_t kk_haskell_chars_concat(int64_t a, int64_t b);

/* Rust println! formatted-output support.
 *
 * The Rust bridge represents `Arguments::new(template, args)` as a
 * packed 2-field cell tagged KK_RUST_FMT_TAG.  `std::io::_print` then
 * dispatches through kk_rust_print_dispatch which routes either to
 * kk_print_str (for from_str's plain-string Arguments) or to
 * kk_rust_print_args (for new's packed cell). */
int64_t kk_rust_args_pack(int64_t template_str, int64_t args_struct);
int64_t kk_rust_print_args(int64_t template_str, int64_t args_struct);
int64_t kk_rust_print_dispatch(int64_t v);

/* Safe field access: kk_field(base, idx) if base is a heap pointer,
 * else returns base verbatim.  Used by the Rust bridge to handle
 * both genuine tuple field projections and WithOverflow-flattened
 * value reads through the same intrinsic. */
int64_t kk_rust_field_safe(int64_t base, int64_t idx);

/* Wrap an arg for Rust's `{:?}` Debug format.  The runtime
 * dispatcher in kk_rust_print_one_arg unwraps these and applies the
 * Debug formatter (quotes around strings, escape control chars).
 * For non-string types Debug == Display.  Used by the Rust bridge
 * as the rewrite target for Argument::<'_>::new_debug::<T>. */
int64_t kk_rust_arg_debug(int64_t v);

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
int64_t kk_str_slice(int64_t parent, int64_t byte_offset, int64_t byte_len); /* O(1) substring view */
int64_t kk_str_show_int(int64_t n);

/* I/O */
void    kk_println_str(int64_t s);
void    kk_print_str(int64_t s);

/* Refcounting (Perceus-compatible) */
void    kk_str_retain(int64_t s);
void    kk_str_drop(int64_t s);

/* Low-level string construction (used by Data.Text shims) */
int64_t kk_str_alloc_leaf_owned(const char* bytes, int64_t byte_len);
char*   kk_str_dup_cstr(int64_t s);  /* flatten rope to malloc'd NUL-terminated C string; caller frees */
int64_t kk_str_byte_len(int64_t s);  /* alias for kk_str_len (total bytes) */

/* ByteString — same representation, byte-oriented API */
int64_t kk_bytes_from_literal(int64_t bytes_ptr, int64_t byte_len);
int64_t kk_bytes_len(int64_t b);
int64_t kk_bytes_concat(int64_t a, int64_t b);
int64_t kk_bytes_index(int64_t b, int64_t i);
int64_t kk_bytes_eq(int64_t a, int64_t b);

/* File I/O — paths and contents are first-class strings.
 *
 * read_file slurps the whole file into a freshly-allocated owned LEAF.
 * write_file flattens the contents into a contiguous buffer and writes
 * it atomically with a single fwrite. Both return 0 on success, -1 on
 * failure (read_file returns 0 as a "string" only if you check first
 * via file_exists; on error it returns the empty string).
 */
int64_t kk_read_file(int64_t path_str);     /* string -> string */
int64_t kk_write_file(int64_t path_str, int64_t content_str);  /* -> 0/-1 */
int64_t kk_file_exists(int64_t path_str);   /* -> 0/1 */
int64_t kk_read_line(void);                 /* stdin -> string (no newline) */

/* Process / environment */
int64_t kk_system(int64_t cmd_str);         /* -> exit code */
int64_t kk_getenv(int64_t name_str);        /* -> string ("" if unset) */

/* IORef — single-field mutable cell, allocated as a kk_alloc_con box.
 * The cell holds an int64_t which may be a scalar or another boxed pointer.
 * Refcounting is the caller's responsibility (Perceus inserts retain/drop
 * around the cell itself; the contents are treated opaquely).
 */
int64_t kk_ref_new(int64_t initial);
int64_t kk_ref_get(int64_t ref);
int64_t kk_ref_set(int64_t ref, int64_t value);  /* returns 0 */

/* Lazy thunks — single-shot, evaluate-on-force.
 * kk_thunk_create wraps a 0-arg function pointer in a thunk object.
 * kk_thunk_force evaluates the thunk (once) and returns the result. */
int64_t kk_thunk_create(int64_t fn_ptr);
int64_t kk_thunk_force(int64_t thunk);

/* Comparison — structural ordering for Map/Set shims.
 * kk_str_compare: lexicographic byte comparison on rope strings.
 *   Returns <0, 0, >0.
 * kk_compare: generic structural comparison.
 *   Small ints compare numerically; strings lexicographically;
 *   heap objects by (tag, fields...) lexicographically.
 * kk_is_heap_ptr: returns 1 if the value looks like a heap pointer. */
int64_t kk_str_compare(int64_t a, int64_t b);
int64_t kk_compare(int64_t a, int64_t b);
int64_t kk_structural_eq(int64_t a, int64_t b);
int64_t kk_is_heap_ptr(int64_t v);
int64_t kk_is_string(int64_t v);
int64_t kk_nfields(int64_t ptr);

/* List helpers — used by Map/Set/Text shims.
 * Tags must match Frankenstein's assignProgramTags output for the compiled
 * Emitter.o code which pattern-matches on list constructors directly.
 * Hash-based stable tags: stableConTag ":" = 46589, stableConTag "[]" = 31636.
 * These match the compiled Haskell code which uses the same hash function. */
#define KK_NIL_TAG  31636
#define KK_CONS_TAG 46589
int64_t kk_nil(void);
int64_t kk_cons(int64_t head, int64_t tail);
int64_t kk_list_head(int64_t list);
int64_t kk_list_tail(int64_t list);
int64_t kk_is_nil(int64_t list);

/* Tuple helpers — stableConTag "(,)" = 4058, 2 fields. */
#define KK_PAIR_TAG 4058
int64_t kk_pair(int64_t a, int64_t b);
int64_t kk_fst(int64_t pair);
int64_t kk_snd(int64_t pair);

/* Maybe helpers — stableConTag "Nothing" = 53440, stableConTag "Just" = 61886. */
#define KK_NOTHING_TAG 53440
#define KK_JUST_TAG    61886
int64_t kk_nothing(void);
int64_t kk_just(int64_t x);

/* Abort effect support (setjmp/longjmp) */
int64_t kk_handler_exec(int64_t tag, int64_t body_closure);
int64_t kk_handler_abort(int64_t tag, int64_t value);

/* Plotkin evidence vector dispatch */
int64_t kk_evv_extend(int64_t parent, int64_t eff_id, int64_t op_table);
int64_t kk_evv_lookup(int64_t evv, int64_t eff_id);
int64_t kk_optab_create(int64_t nops);
int64_t kk_optab_set(int64_t tab, int64_t op_idx, int64_t closure);
int64_t kk_optab_get(int64_t tab, int64_t op_idx);

/* PAP (partially-applied function) — wraps a plotkin'd fn so it can
 * be dispatched as a value through the existing closure ABI. */
int64_t kk_pap_call_1(int64_t self, int64_t a);
int64_t kk_pap_call_2(int64_t self, int64_t a, int64_t b);
int64_t kk_pap_call_3(int64_t self, int64_t a, int64_t b, int64_t c);
int64_t kk_pap_alloc(int64_t trampoline, int64_t fn_ptr, int64_t evv);

#endif /* KK_RUNTIME_H */
