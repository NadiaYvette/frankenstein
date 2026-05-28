#define _GNU_SOURCE  /* for asprintf */
/* Frankenstein minimal runtime — Perceus refcounting + boxed values
 *
 * Boxed value layout (all fields int64_t):
 *   [refcount] [tag] [field0] [field1] ...
 *              ^--- the "pointer" points here (to the tag)
 *
 * The refcount lives at (ptr - 8). Fields start at (ptr + 8).
 * A null/zero pointer means "unboxed integer" and is not dereferenced.
 *
 * The high byte of the refcount encodes the cycle collector color
 * (see kk_cycle.h). Normal RC operations use the low 56 bits.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <execinfo.h>
#define _GNU_SOURCE
#include <dlfcn.h>
#include "kk_runtime.h"
#include "kk_cycle.h"
#include "kk_arena.h"

/* String magic marker — used by kk_is_string for O(1) identification.
 * Every kk_string_t has this as its first field. */
#define KK_STRING_MAGIC ((int64_t)0x4B4B535452494E47LL) /* "KKSTRING" */

/* Forward declarations for string tracking (defined later) */
static void kk_register_string(int64_t ptr);
static void kk_unregister_string(int64_t ptr);
int64_t kk_string_checkpoint(void);
void kk_string_rollback(int64_t checkpoint, int64_t rescue_ptr);
/* Forward declarations for string refcounting (different layout than cons) */
void kk_str_retain(int64_t s_i);
void kk_str_drop(int64_t s_i);

/* Raw allocation: size in bytes */
void* kk_alloc(int64_t size) {
    return malloc((size_t)size);
}

void kk_free(void* ptr) {
    free(ptr);
}

/* Check if a value is a heap pointer (vs an unboxed integer).
 * Heap pointers from kk_alloc_con are 8-byte aligned, so the low 3 bits
 * are zero and the value is above a reasonable threshold.
 * Small integers and other non-pointer values are skipped. */
/* Exported version (declared in kk_runtime.h) */
int64_t kk_is_heap_ptr(int64_t ptr) {
    /* Must be non-zero, 8-byte aligned, and above a reasonable threshold. */
    return ptr != 0 && (ptr & 7) == 0 && (uint64_t)ptr > 0x10000;
}

/* Refcount helpers — pointer to refcount is at (ptr - 8) */
static inline int64_t* kk_rc_ptr(int64_t ptr) {
    return (int64_t*)(ptr - 8);
}

void kk_retain(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    if (kk_is_string(ptr)) { kk_str_retain(ptr); return; }
    if (!kk_arena_maybe_owns((const void*)(intptr_t)ptr)) return;
    int64_t* rc = kk_rc_ptr(ptr);
    int64_t cur = *rc & KK_RC_MASK;
    /* Refuse to revive a cell whose refcount has already reached zero.
     * Under KK_RECYCLE=1 such a cell is on the arena freelist with its
     * tag-slot reused as a next-pointer; incrementing the rc would let
     * a subsequent kk_drop decrement it back to zero, recurse into
     * the no-longer-valid "children", and re-push the block onto the
     * freelist — corrupting the chain (the classic double-recycle).
     * The same guard is harmless when recycling is off: rc=0 cells
     * never get retained on a correctly-counted code path. */
    if (cur == 0) return;
    int64_t count = cur + 1;
    *rc = (*rc & (KK_COLOR_MASK | KK_NFIELDS_MASK)) | count;
    *rc = (*rc & (KK_RC_MASK | KK_NFIELDS_MASK)) | KK_COLOR_BLACK;
}

int64_t kk_tag(int64_t ptr);  /* forward decl */

/* Special tags — must be defined before kk_drop which dispatches on them. */
#define KK_CLOSURE_TAG 0x434C4F53  /* "CLOS" — field 0 is raw fn ptr */
#define KK_THUNK_TAG   0x4C415A59  /* "LAZY" — field 0: eval flag, field 1: fn/result */
#define KK_EVV_TAG     0x45565630  /* "EVV0" — all fields are handler fn ptrs */
#define KK_EVV2_TAG    0x45565632  /* "EVV2" — Plotkin evidence stack: (eff_id, op_table) pairs */
#define KK_OPTAB_TAG   0x4F505442  /* "OPTB" — operation table for one effect */
#define KK_PAP_TAG     0x50415030  /* "PAP0" — partially applied fn with pre-supplied args
                                    *
                                    * Layout: [tag, trampoline, wrapped_fn, supplied_0, supplied_1, ...]
                                    * Field 0: kk_pap_call_N (a trampoline raw fn ptr)
                                    * Field 1: wrapped_fn (raw .text fn ptr to the actual fn)
                                    * Field 2..K: pre-supplied args (e.g. evv)
                                    *
                                    * Invoked by the standard closure dispatcher
                                    * (kk_field(c, 0) + llvm.call(c, args...)): the
                                    * trampoline at field 0 receives (self=PAP, args...)
                                    * and forwards to wrapped_fn(supplied..., args...).
                                    *
                                    * Used by the plotkin emitter: when an EVar fn
                                    * appears as a value (not as the head of an EApp),
                                    * the emitter wraps it in a PAP with the current
                                    * evv_p pre-supplied. HOFs (map, fromList, etc.)
                                    * then dispatch through the PAP exactly the same
                                    * way they dispatch any other closure — they don't
                                    * need to know the function was plotkin-transformed.
                                    */

void kk_drop(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return;
    if (kk_is_string(ptr)) { kk_str_drop(ptr); return; }
    if (!kk_arena_maybe_owns((const void*)(intptr_t)ptr)) return;
    int64_t* rc = kk_rc_ptr(ptr);
    int64_t count = *rc & KK_RC_MASK;
    if (count == 0) return;  /* already freed or corrupt — don't double-free */
    if (count > 1) {
        /* Shared — just decrement (preserve color and nfields) */
        *rc = (*rc & (KK_COLOR_MASK | KK_NFIELDS_MASK)) | (count - 1);
        return;
    }
    /* Sole owner (rc == 1) — drop children, then free.
     *
     * kk_thunk_force retains cached results on every read, so lazy
     * selector thunk sharing is properly refcounted.  It is now safe
     * to recursively drop children and free the object. */
    *rc = 0;  /* mark dead first — prevents re-entrant double-free */

    int64_t tag = kk_tag(ptr);
    int64_t nf = kk_nfields(ptr);
    int64_t* fields = (int64_t*)(ptr + 8);

    if (tag == KK_CLOSURE_TAG) {
        /* Closures: field 0 is a raw function pointer — skip it.
         * Fields 1..nfields-1 are captured values — drop them. */
        for (int64_t i = 1; i < nf; i++)
            kk_drop(fields[i]);
    } else if (tag == KK_THUNK_TAG) {
        /* Thunks: field 0 = evaluated flag (integer, skip).
         * Field 1 = fn_ptr if unevaluated, cached result if evaluated.
         * Only drop field 1 when evaluated (cached result is a heap value). */
        if (fields[0] != 0)  /* evaluated */
            kk_drop(fields[1]);
    } else if (tag == KK_EVV_TAG) {
        /* Evidence vectors (legacy): all fields are handler fn pointers — skip */
    } else if (tag == KK_EVV2_TAG) {
        /* Plotkin evv: pairs of (eff_id, op_table). Even slots are integers
         * (effect ids), odd slots are heap op_tables — drop only the odd. */
        for (int64_t i = 1; i < nf; i += 2)
            kk_drop(fields[i]);
    } else if (tag == KK_OPTAB_TAG) {
        /* Op tables: each field is a closure heap pointer — drop each. */
        for (int64_t i = 0; i < nf; i++)
            kk_drop(fields[i]);
    } else if (tag == KK_PAP_TAG) {
        /* PAP: fields 0 (trampoline) and 1 (wrapped fn) are raw .text
         * fn pointers — kk_drop on a non-heap value is a no-op, so we
         * could just drop everything; for clarity, skip those two and
         * drop fields 2..nfields-1 (the supplied heap values such as
         * the captured evv). */
        for (int64_t i = 2; i < nf; i++)
            kk_drop(fields[i]);
    } else {
        /* Regular constructors: drop all fields */
        for (int64_t i = 0; i < nf; i++)
            kk_drop(fields[i]);
    }

    kk_unregister_nfields(ptr);
    /* Reclaim the cell.  For arena-owned blocks, push onto the
     * size-bucketed freelist so the next same-size allocation can
     * reuse the bytes (keeps RSS bounded on long-running workloads
     * like surd-quintic's degree-7 root finder).  For malloc'd
     * blocks (oversized / KK_NO_ARENA), kk_arena_free still calls
     * free().
     *
     * The freelist is opt-in via KK_RECYCLE=1 because surd-quintic
     * (and likely other Idris2-shim workloads) has at least one
     * use-after-drop path that the leak-everything arena masks;
     * enabling recycle naïvely segfaults within seconds. */
    void* block = (void*)(intptr_t)(ptr - 8);
    if (kk_arena_maybe_owns(block)) {
        static int recycle_enabled = -1;
        if (recycle_enabled == -1) {
            const char* v = getenv("KK_RECYCLE");
            recycle_enabled = (v && v[0] && v[0] != '0') ? 1 : 0;
        }
        if (recycle_enabled) {
            kk_arena_recycle_put(block, (size_t)((2 + nf) * 8));
        }
    } else {
        kk_arena_free(block);
    }
}

void kk_release(int64_t ptr) {
    kk_drop(ptr);
}

int64_t kk_reuse(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    int64_t* rc = kk_rc_ptr(ptr);
    if ((*rc & KK_RC_MASK) == 1) {
        /* Sole owner — reuse the allocation */
        return ptr;
    }
    /* Shared — can't reuse, caller must allocate fresh */
    kk_drop(ptr);
    return 0;
}

/* Read the tag from a boxed value */
/* Cached env-var trace flags — `getenv()` is O(envp_count) and was
 * dominating CPU when called per kk_tag/kk_field (60%+ of time on
 * large compiles).  Initialised once on first call, then a single
 * branch on a global. */
static int kk_tag_trace = -1;     /* -1 = uninit, 0 = off, lim>0 = on */
static int kk_field_trace = -1;
static void kk_init_trace_flags(void) {
    if (kk_tag_trace < 0) {
        const char* e = getenv("KK_TAG_TRACE");
        if (!e) kk_tag_trace = 0;
        else {
            const char* lm = getenv("KK_TAG_TRACE_MAX");
            kk_tag_trace = lm ? atoi(lm) : 100;
            if (kk_tag_trace <= 0) kk_tag_trace = 100;
        }
    }
    if (kk_field_trace < 0) {
        const char* e = getenv("KK_FIELD_TRACE");
        if (!e) kk_field_trace = 0;
        else {
            const char* lm = getenv("KK_FIELD_TRACE_MAX");
            kk_field_trace = lm ? atoi(lm) : 200;
            if (kk_field_trace <= 0) kk_field_trace = 200;
        }
    }
}

int64_t kk_tag(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) {
        if (kk_tag_trace > 0) {
            static int n0 = 0;
            if (n0 < kk_tag_trace) fprintf(stderr, "[kk_tag %d] non-heap ptr=%ld → 0\n", n0++, ptr);
        } else if (kk_tag_trace < 0) {
            kk_init_trace_flags();
            return kk_tag(ptr);  /* retry once with initialised flag */
        }
        return 0;
    }
    int64_t t = *(int64_t*)ptr;
    /* Detect use-after-drop: a recycled cell has its tag slot overwritten
     * with (KK_RECYCLE_FLAG | next-ptr) where bit 63 is set.  No valid
     * tag or user-space pointer has bit 63 set, so this is unambiguous.
     * Under KK_RECYCLE_AUDIT=1 we abort so gdb shows the calling fn. */
    if ((t & ((int64_t)1 << 63)) && kk_recycle_audit_enabled()) {
        fprintf(stderr, "[kk_tag] USE-AFTER-DROP: ptr=%p tag=0x%lx\n", (void*)ptr, t);
        abort();
    }
    if (kk_tag_trace > 0) {
        static int n1 = 0;
        if (n1 < kk_tag_trace) fprintf(stderr, "[kk_tag %d] heap ptr=%p tag=%ld (0x%lx)\n", n1++, (void*)ptr, t, t);
    }
    return t;
}

/* Read field[idx] from a boxed value (fields start after the tag) */
int64_t kk_field(int64_t ptr, int64_t idx) {
    if (!kk_is_heap_ptr(ptr)) {
        if (kk_field_trace > 0) {
            static int nf0 = 0;
            if (nf0 < kk_field_trace) fprintf(stderr, "[kk_field %d] non-heap base=%ld idx=%ld → 0\n", nf0++, ptr, idx);
        } else if (kk_field_trace < 0) {
            kk_init_trace_flags();
            return kk_field(ptr, idx);
        }
        return 0;
    }
    /* Detect use-after-drop via the recycled-cell sentinel.  block[0]=0
     * (rc=0) is what kk_drop already wrote; if idx=-1 reads it as the
     * tag slot, the high bit (KK_RECYCLE_FLAG) marks it as a freelist
     * next-pointer.  In practice idx>=0, but the parent tag word's
     * high bit is the canonical signal: see kk_tag.  Here we only
     * surface the case where someone reads field[idx] of a parent
     * whose tag slot is the sentinel — bit 63 of the tag check. */
    if (kk_recycle_audit_enabled()) {
        int64_t parent_tag = *(int64_t*)ptr;
        if (parent_tag & ((int64_t)1 << 63)) {
            int64_t* fields = (int64_t*)(ptr + 8);
            fprintf(stderr,
                "[kk_field] USE-AFTER-DROP: ptr=%p tag=0x%lx idx=%ld field[0]=0x%lx field[1]=0x%lx\n",
                (void*)ptr, parent_tag, idx, fields[0], fields[1]);
            /* If field[0] is in the text segment, it was a closure — show
             * the function name with addr2line for easier root-cause. */
            Dl_info info;
            if (dladdr((void*)(uintptr_t)fields[0], &info) && info.dli_sname) {
                fprintf(stderr, "  field[0] resolves to: %s\n", info.dli_sname);
            }
            abort();
        }
    }
    int64_t* fields = (int64_t*)(ptr + 8);
    int64_t v = fields[idx];
    if (kk_field_trace > 0) {
        static int nf1 = 0;
        if (nf1 < kk_field_trace) fprintf(stderr, "[kk_field %d] base=%p tag=0x%lx idx=%ld → 0x%lx\n", nf1++, (void*)ptr, *(int64_t*)ptr, idx, v);
    }
    return v;
}

/* Structural equality for boxed values.
 * Used by compiled == operator instead of pointer comparison (cmpi eq),
 * because separately allocated boxes (e.g. Char = C# codepoint) with
 * the same contents have different addresses.
 *
 * Returns 1 if equal, 0 if not. */
int64_t kk_structural_eq(int64_t a, int64_t b) {
    /* Fast path: same value (covers unboxed ints, same pointer) */
    if (a == b) return 1;
    /* kk_is_heap_ptr is a syntactic check only (alignment + threshold);
     * an Int value like 65544 (0x10008) passes it.  Require BOTH values
     * to actually be in our arena before dereferencing — otherwise we'd
     * read random memory as a tag.  Values that don't pass this stricter
     * check are unboxed primitives; since the fast-path `a == b` above
     * already rejected equal unboxed values, they must differ here. */
    if (!kk_is_heap_ptr(a) || !kk_is_heap_ptr(b)) return 0;
    int a_owned = kk_arena_maybe_owns((const void*)(intptr_t)a);
    int b_owned = kk_arena_maybe_owns((const void*)(intptr_t)b);
    if (!a_owned || !b_owned) {
        /* String literals live outside the arena; allow that case. */
        if (a_owned != b_owned) return 0;
        if (kk_is_string(a) && kk_is_string(b))
            return kk_str_compare(a, b) == 0 ? 1 : 0;
        return 0;
    }
    /* String comparison: use kk_str_compare (content-based) */
    if (kk_is_string(a) && kk_is_string(b))
        return kk_str_compare(a, b) == 0 ? 1 : 0;
    /* Both are heap pointers in the arena: compare tags */
    int64_t tag_a = kk_tag(a);
    int64_t tag_b = kk_tag(b);
    if (tag_a != tag_b) return 0;
    /* Compare fields */
    int64_t nf_a = kk_nfields(a);
    int64_t nf_b = kk_nfields(b);
    if (nf_a != nf_b) return 0;
    for (int64_t i = 0; i < nf_a; i++) {
        if (!kk_structural_eq(kk_field(a, i), kk_field(b, i))) return 0;
    }
    return 1;
}

/* Allocate a constructor: tag + nfields payload slots.
 * Returns pointer to the tag (not the refcount).
 * Layout: [rc=1] [tag] [f0] [f1] ... */
int64_t kk_alloc_con(int64_t tag, int64_t nfields) {
    int64_t total = (2 + nfields) * 8;  /* rc + tag + fields */
    /* Constructor cells go into the bump arena by default. The arena
     * returns NULL when KK_NO_ARENA is set, in which case we fall back
     * to libc malloc and rely on the matching kk_arena_free in kk_drop
     * to call free() (since kk_arena_owns will report not-owned). */
    int64_t* block = (int64_t*)kk_arena_alloc((size_t)total);
    if (!block) block = (int64_t*)malloc((size_t)total);
    if (!block) return 0;
    /* Pack nfields (clamped to 255) into the RC word alongside color and refcount */
    int64_t nf_bits = (nfields > 255 ? 255 : nfields) << KK_NFIELDS_SHIFT;
    block[0] = KK_COLOR_BLACK | nf_bits | 1;  /* color=black, nfields, refcount=1 */
    block[1] = tag;                             /* tag */
    /* Zero-init fields */
    for (int64_t i = 0; i < nfields; i++) {
        block[2 + i] = 0;
    }
    /* Return pointer to the tag slot */
    int64_t ptr = (int64_t)&block[1];
    static const char* trace = NULL;
    if (trace == NULL) { trace = getenv("KK_ALLOC_TRACE"); if (!trace) trace = ""; }
    if (trace[0]) {
        /* Optional filter on tag (KK_ALLOC_TRACE=CLOS shows only CLOS cells) */
        const char* tag_match = NULL;
        if (tag == 0x434C4F53 && (trace[0] == 'C' || trace[0] == '1')) tag_match = "CLOS";
        else if (tag == 0x4C415A59 && (trace[0] == 'L' || trace[0] == '1')) tag_match = "LAZY";
        else if (trace[0] == '1') tag_match = "OTHER";
        if (tag_match) {
            fprintf(stderr, "[alloc] tag=%s nf=%ld → %p caller=%p\n",
                    tag_match, (long)nfields, (void*)(uintptr_t)ptr,
                    __builtin_return_address(0));
        }
    }
    return ptr;
}

/* Print an ADT-valued result as an s-expression.
 *
 * Output form:
 *   - heap pointer with N fields → "(#TAG f0 f1 ... fN-1)" (fields recursively)
 *   - heap pointer with 0 fields → "(#TAG)"
 *   - non-pointer scalar         → decimal integer
 *
 * The top-level call appends a newline; recursive calls do not. We use a
 * helper to keep the public API a single one-shot printer.
 */
static void kk_print_con_inner(int64_t v) {
    /* kk_is_heap_ptr is a syntactic check (alignment + threshold).  Many
     * unboxed Int values (e.g. 0x10008 = 65544) pass that check yet are
     * not valid heap addresses.  Require arena ownership before
     * dereferencing — string literals are allowed via kk_is_string. */
    if (!kk_is_heap_ptr(v)
        || (!kk_arena_maybe_owns((const void*)(intptr_t)v) && !kk_is_string(v))) {
        printf("%lld", (long long)v);
        return;
    }
    if (kk_is_string(v)) {
        printf("\"");
        kk_print_str(v);
        printf("\"");
        return;
    }
    int64_t tag = kk_tag(v);
    int64_t nf  = kk_nfields(v);
    printf("(#%lld", (long long)tag);
    int64_t* fields = (int64_t*)(v + 8);
    for (int64_t i = 0; i < nf; i++) {
        printf(" ");
        kk_print_con_inner(fields[i]);
    }
    printf(")");
}

void kk_println_con(int64_t v) {
    kk_print_con_inner(v);
    printf("\n");
}

/* Haskell-String printer: walks a [Char] cons-list and prints chars.
 *
 * The GHC bridge encodes Haskell's String (= [Char]) as a cons-list
 * with hash-based tags: stableConTag "[]" = 31636 (Nil), stableConTag
 * ":" = 46589 (Cons).  Field 0 of a Cons holds the Char codepoint
 * (i64), field 1 holds the tail.
 *
 * Each char is a Unicode codepoint (Int in the IR) — emitted via
 * kk_putchar_utf8 as the appropriate UTF-8 byte sequence so that
 * non-ASCII codepoints round-trip cleanly (`café` → 0x63 0x61 0x66
 * 0xC3 0xA9).  Trailing newline appended once at end of println.
 */
#define KK_HASKELL_NIL_TAG  31636
#define KK_HASKELL_CONS_TAG 46589

/* UTF-8-encode a Unicode codepoint to stdout.  Codepoints below
 * 0x80 emit one byte; below 0x800 two bytes; below 0x10000 three;
 * up to 0x10FFFF four.  Invalid codepoints (surrogates / > 0x10FFFF)
 * are written as the Unicode replacement character U+FFFD. */
static void kk_putchar_utf8(int32_t cp) {
    if (cp < 0)                                    cp = 0xFFFD;
    if (cp >= 0xD800 && cp <= 0xDFFF)              cp = 0xFFFD;
    if (cp > 0x10FFFF)                             cp = 0xFFFD;
    if (cp < 0x80) {
        putchar((int)cp);
    } else if (cp < 0x800) {
        putchar((int)(0xC0 | (cp >> 6)));
        putchar((int)(0x80 | (cp & 0x3F)));
    } else if (cp < 0x10000) {
        putchar((int)(0xE0 | (cp >> 12)));
        putchar((int)(0x80 | ((cp >> 6) & 0x3F)));
        putchar((int)(0x80 | (cp & 0x3F)));
    } else {
        putchar((int)(0xF0 | (cp >> 18)));
        putchar((int)(0x80 | ((cp >> 12) & 0x3F)));
        putchar((int)(0x80 | ((cp >> 6) & 0x3F)));
        putchar((int)(0x80 | (cp & 0x3F)));
    }
}

void kk_print_haskell_chars(int64_t list) {
    while (1) {
        if (kk_tag(list) == KK_HASKELL_NIL_TAG) break;
        int64_t ch = kk_field(list, 0);
        kk_putchar_utf8((int32_t)ch);
        list = kk_field(list, 1);
    }
}

void kk_println_haskell_chars(int64_t list) {
    kk_print_haskell_chars(list);
    putchar('\n');
}

/* Build a Haskell [Char] cons-list for the decimal representation of n.
 * `tail` is the list to prepend digits onto (showsPrec-style).
 *
 * Used by the GHC bridge to intercept `show :: Int -> String` and
 * the lower-level `showsPrec :: Int -> Int -> ShowS` calls.  We
 * ignore the precedence argument (callers typically pass 0 or 11);
 * the only observable difference is parenthesisation of negatives in
 * application contexts, which we model by always parenthesising
 * negative numbers when precedence >= 6 — see kk_int_show_at_prec.
 */
int64_t kk_int_to_haskell_chars(int64_t n, int64_t tail) {
    /* Build a cons-cell for a single char. */
    #define CONS_CELL(c, t) ({                                  \
        int64_t _cell = kk_alloc_con(KK_HASKELL_CONS_TAG, 2);  \
        kk_set_field(_cell, 0, (c));                            \
        kk_set_field(_cell, 1, (t));                            \
        _cell;                                                   \
    })
    int64_t result = tail;
    int neg = (n < 0);
    int64_t v = neg ? -n : n;
    /* Build digit chars in reverse (least significant first), then
     * prepend onto result so the high-significance digit ends up at
     * the head of the list. */
    char buf[24];  /* 2^63 ≈ 19 digits + sign + slack */
    int len = 0;
    if (v == 0) {
        buf[len++] = '0';
    } else {
        while (v > 0) {
            buf[len++] = (char)('0' + (v % 10));
            v /= 10;
        }
    }
    if (neg) {
        buf[len++] = '-';
    }
    /* Now prepend each byte in iteration order (which walks from
     * least-significant up through sign), since we want the head of
     * the list to be the most significant char.  We build from the
     * end so each cons points at the previously-built tail. */
    for (int i = 0; i < len; i++) {
        result = CONS_CELL((int64_t)(unsigned char)buf[i], result);
    }
    return result;
    #undef CONS_CELL
}

/* Helper: prepend a single char onto a [Char] cons-list. */
static int64_t kk_cons_char_cell(int64_t ch, int64_t tail) {
    int64_t cell = kk_alloc_con(KK_HASKELL_CONS_TAG, 2);
    kk_set_field(cell, 0, ch);
    kk_set_field(cell, 1, tail);
    return cell;
}

/* Dummy 0-arg CAF used by the GHC bridge to stub out unforced
 * references to GHC.Internal.Show showList methods (`$cshowList`).
 * The bridge points specialised dictionary slots at this so they
 * link cleanly; the dict is never traversed at runtime because user
 * code only invokes showsPrec/show, not showList. */
int64_t dummy_show_caf(void) { return 0; }
int64_t kk_dummy_show_caf(int64_t) __attribute__((alias("dummy_show_caf")));

/* Rust println! format support.
 *
 * Template byte encoding (rustc 2024+):
 *   <len-byte> <literal-bytes...>   first piece
 *   (0xc0 <len-byte> <literal-bytes...>)*   placeholder + next piece
 *   0x00   terminator
 *
 * Frankenstein represents `Arguments::new(template, args)` as a
 * packed 2-field cell tagged KK_RUST_FMT_TAG with fields
 *   field 0 → template kk_string
 *   field 1 → args struct (kk_alloc_con of N i64s, one per arg)
 *
 * std::io::_print dispatches through kk_rust_print_dispatch which
 * checks the value: a kk_string goes through kk_println_str (the
 * from_str path, no formatting); a packed cell is walked by
 * kk_rust_print_args. */

#define KK_RUST_FMT_TAG     0xC0FF1E  /* packed (template, args) cell tag */
#define KK_RUST_DEBUG_TAG   0xDEB07A  /* per-arg Debug-formatter wrapper */
#define KK_RUST_HEX_LO_TAG  0x4845570  /* `{:x}` lower-hex Argument tag */
#define KK_RUST_HEX_HI_TAG  0x4845577  /* `{:X}` upper-hex Argument tag */
#define KK_RUST_OCT_TAG     0x40C7A7   /* `{:o}` octal Argument tag */
#define KK_RUST_BIN_TAG     0x4B17A7   /* `{:b}` binary Argument tag */
#define KK_RUST_U32_TAG     0x42100032 /* per-type numeric arg tags */
#define KK_RUST_I32_TAG     0x42100132
#define KK_RUST_U64_TAG     0x42100064
#define KK_RUST_U16_TAG     0x42100016
#define KK_RUST_I16_TAG     0x42100116
#define KK_RUST_U8_TAG      0x42100008
#define KK_RUST_I8_TAG      0x42100108
#define KK_RUST_STRUCT_TAG  0x575C7C70  /* derive(Debug) struct cell */
#define KK_RUST_F64_TAG     0x42100F64  /* per-type float arg tags;     */
#define KK_RUST_F32_TAG     0x42100F32  /* inner i64 holds the IEEE bits */

int64_t kk_rust_args_pack(int64_t template_str, int64_t args_struct) {
    int64_t cell = kk_alloc_con(KK_RUST_FMT_TAG, 2);
    kk_set_field(cell, 0, template_str);
    kk_set_field(cell, 1, args_struct);
    return cell;
}

static int64_t kk_args_extract(int64_t args_struct, int idx) {
    /* args_struct is a heap-allocated tuple/array.  Field idx holds
     * the i64 value (already unboxed by the bridge's
     * Argument::new_display elision). */
    return kk_field(args_struct, idx);
}

/* Decode the bridge's `__RBYTES:HHHH…` marker form to raw bytes.
 * Returns malloc'd bytes + length; caller frees.  If the input
 * doesn't carry the marker, falls back to returning the original
 * UTF-8 bytes (the from_str case where format-template parsing isn't
 * needed). */
static unsigned char* kk_rust_decode_template(int64_t template_str,
                                              size_t* out_len)
{
    char* raw = kk_str_dup_cstr(template_str);
    if (!raw) { *out_len = 0; return NULL; }
    size_t raw_len = (size_t)kk_str_len(template_str);
    const char marker[] = "__RBYTES:";
    const size_t mlen = sizeof(marker) - 1;
    if (raw_len < mlen || memcmp(raw, marker, mlen) != 0) {
        *out_len = raw_len;
        return (unsigned char*)raw;  /* caller frees */
    }
    /* Decode the hex pairs after the marker. */
    size_t hex_len = raw_len - mlen;
    size_t n_bytes = hex_len / 2;
    unsigned char* out = (unsigned char*)malloc(n_bytes + 1);
    if (!out) { free(raw); *out_len = 0; return NULL; }
    const char* hex = raw + mlen;
    for (size_t i = 0; i < n_bytes; i++) {
        int hi = hex[i*2], lo = hex[i*2 + 1];
        int hv = (hi >= '0' && hi <= '9') ? hi - '0'
               : (hi >= 'A' && hi <= 'F') ? hi - 'A' + 10
               : (hi >= 'a' && hi <= 'f') ? hi - 'a' + 10 : 0;
        int lv = (lo >= '0' && lo <= '9') ? lo - '0'
               : (lo >= 'A' && lo <= 'F') ? lo - 'A' + 10
               : (lo >= 'a' && lo <= 'f') ? lo - 'a' + 10 : 0;
        out[i] = (unsigned char)((hv << 4) | lv);
    }
    out[n_bytes] = 0;
    free(raw);
    *out_len = n_bytes;
    return out;
}

/* Print a kk_string with Rust's Debug-style quoting: wrap in double
 * quotes and escape \n / \t / \r / \" / \\ inside.  Other control
 * characters fall through to printf %02x — close enough to Rust's
 * Debug output for hello-world purposes. */
static void kk_rust_print_str_debug(int64_t s) {
    putchar('"');
    char* buf = kk_str_dup_cstr(s);
    if (buf) {
        int64_t n = kk_str_len(s);
        for (int64_t i = 0; i < n; i++) {
            unsigned char c = (unsigned char)buf[i];
            switch (c) {
                case '\n': fputs("\\n", stdout); break;
                case '\t': fputs("\\t", stdout); break;
                case '\r': fputs("\\r", stdout); break;
                case '\"': fputs("\\\"", stdout); break;
                case '\\': fputs("\\\\", stdout); break;
                default:
                    if (c >= 0x20 && c < 0x7f) {
                        putchar(c);
                    } else {
                        printf("\\x%02x", c);
                    }
                    break;
            }
        }
        free(buf);
    }
    putchar('"');
}

/* Print a single println!-arg.  Dispatches on its runtime shape:
 *   - Debug-tagged cell    → unwrap and use Debug formatter
 *   - hex/oct/bin-tagged   → printf with appropriate format string
 *   - kk_string            → Display: print bytes verbatim
 *   - otherwise            → Display: printf("%ld") (assume i64)
 *
 * The Display dispatch via kk_is_string is best-effort; in practice
 * println! args are small ints or kk_strings, so the heuristic is
 * accurate. */
static void kk_rust_print_one_arg(int64_t v) {
    if (kk_is_heap_ptr(v) && kk_nfields(v) == 1) {
        int64_t tag = kk_tag(v);
        int64_t inner = kk_field(v, 0);
        if (tag == KK_RUST_DEBUG_TAG) {
            if (kk_is_string(inner)) {
                kk_rust_print_str_debug(inner);
            } else if (kk_is_heap_ptr(inner)
                       && kk_tag(inner) == KK_RUST_STRUCT_TAG
                       && kk_nfields(inner) >= 2) {
                /* Tagged-struct Debug.  Shape variants:
                 *   - field_names empty → unit variant (no body):
                 *       `Origin`
                 *   - field_names empty BUT >0 values →
                 *     positional tuple variant: `Circle(10)`
                 *   - field_names non-empty → named-field struct/variant:
                 *       `Point { x: 7, y: 13 }` or `Rect { w: 7, h: 13 }`
                 */
                int64_t name_s  = kk_field(inner, 0);
                int64_t names_s = kk_field(inner, 1);
                int64_t nvals   = kk_nfields(inner) - 2;
                if (kk_is_string(name_s)) kk_print_str(name_s);
                if (nvals == 0) {
                    /* unit variant: just the name */
                } else {
                    /* Determine whether field names are present.  An
                     * empty kk_string (or string consisting only of
                     * commas) signals positional fields. */
                    size_t nlen = kk_is_string(names_s) ? (size_t)kk_str_len(names_s) : 0;
                    char* fnbuf = nlen > 0 ? kk_str_dup_cstr(names_s) : NULL;
                    int has_field_names = 0;
                    if (fnbuf) {
                        for (size_t k = 0; k < nlen; k++) {
                            if (fnbuf[k] != ',' && fnbuf[k] != ' ') {
                                has_field_names = 1;
                                break;
                            }
                        }
                    }
                    if (has_field_names) {
                        fputs(" { ", stdout);
                        const char* p = fnbuf;
                        for (int64_t i = 0; i < nvals; i++) {
                            if (i > 0) fputs(", ", stdout);
                            if (p) {
                                const char* comma = strchr(p, ',');
                                size_t flen = comma ? (size_t)(comma - p) : strlen(p);
                                fwrite(p, 1, flen, stdout);
                                fputs(": ", stdout);
                                p = comma ? comma + 1 : p + flen;
                            }
                            kk_rust_print_one_arg(kk_field(inner, 2 + i));
                        }
                        fputs(" }", stdout);
                    } else {
                        /* positional tuple variant — `Name(v0, v1, …)` */
                        putchar('(');
                        for (int64_t i = 0; i < nvals; i++) {
                            if (i > 0) fputs(", ", stdout);
                            kk_rust_print_one_arg(kk_field(inner, 2 + i));
                        }
                        putchar(')');
                    }
                    if (fnbuf) free(fnbuf);
                }
            } else if (kk_is_heap_ptr(inner)) {
                /* Unknown heap cell: positional fallback. */
                int64_t n = kk_nfields(inner);
                putchar('(');
                for (int64_t i = 0; i < n; i++) {
                    if (i > 0) { putchar(','); putchar(' '); }
                    int64_t fv = kk_field(inner, i);
                    kk_rust_print_one_arg(fv);
                }
                putchar(')');
            } else {
                printf("%ld", (long)inner);
            }
            return;
        }
        if (tag == KK_RUST_U32_TAG) { printf("%u",   (uint32_t)(inner & 0xFFFFFFFF)); return; }
        if (tag == KK_RUST_I32_TAG) { printf("%d",   (int32_t)(inner & 0xFFFFFFFF)); return; }
        if (tag == KK_RUST_U64_TAG) { printf("%llu", (unsigned long long)(uint64_t)inner); return; }
        if (tag == KK_RUST_U16_TAG) { printf("%u",   (unsigned)(inner & 0xFFFF)); return; }
        if (tag == KK_RUST_I16_TAG) { printf("%d",   (int)(int16_t)(inner & 0xFFFF)); return; }
        if (tag == KK_RUST_U8_TAG)  { printf("%u",   (unsigned)(inner & 0xFF)); return; }
        if (tag == KK_RUST_I8_TAG)  { printf("%d",   (int)(int8_t)(inner & 0xFF)); return; }
        if (tag == KK_RUST_F64_TAG) {
            double d;
            uint64_t bits = (uint64_t)inner;
            memcpy(&d, &bits, sizeof(d));
            printf("%g", d);
            return;
        }
        if (tag == KK_RUST_F32_TAG) {
            float f;
            uint32_t bits = (uint32_t)(inner & 0xFFFFFFFF);
            memcpy(&f, &bits, sizeof(f));
            printf("%g", (double)f);
            return;
        }
        if (tag == KK_RUST_HEX_LO_TAG) { printf("%lx", (long)inner); return; }
        if (tag == KK_RUST_HEX_HI_TAG) { printf("%lX", (long)inner); return; }
        if (tag == KK_RUST_OCT_TAG)    { printf("%lo", (long)inner); return; }
        if (tag == KK_RUST_BIN_TAG) {
            /* %b isn't portable; format manually as a binary i64. */
            uint64_t u = (uint64_t)inner;
            if (u == 0) { putchar('0'); return; }
            char buf[65];
            int len = 0;
            while (u && len < 64) { buf[len++] = '0' + (int)(u & 1); u >>= 1; }
            for (int i = len - 1; i >= 0; i--) putchar(buf[i]);
            return;
        }
    }
    if (kk_is_string(v)) {
        kk_print_str(v);
        return;
    }
    printf("%ld", (long)v);
}

/* Wrap an arg for Debug formatting.  Used by the Rust bridge as the
 * rewrite target for core::fmt::rt::Argument::<'_>::new_debug::<T>. */
int64_t kk_rust_arg_debug(int64_t v) {
    int64_t cell = kk_alloc_con(KK_RUST_DEBUG_TAG, 1);
    kk_set_field(cell, 0, v);
    return cell;
}

/* Per-radix wrappers for `{:x}` / `{:X}` / `{:o}` / `{:b}` formats. */
static int64_t kk_rust_arg_radix(int64_t tag, int64_t v) {
    int64_t cell = kk_alloc_con(tag, 1);
    kk_set_field(cell, 0, v);
    return cell;
}
int64_t kk_rust_arg_lower_hex(int64_t v) { return kk_rust_arg_radix(KK_RUST_HEX_LO_TAG, v); }
int64_t kk_rust_arg_upper_hex(int64_t v) { return kk_rust_arg_radix(KK_RUST_HEX_HI_TAG, v); }
int64_t kk_rust_arg_octal(int64_t v)     { return kk_rust_arg_radix(KK_RUST_OCT_TAG, v); }
int64_t kk_rust_arg_binary(int64_t v)    { return kk_rust_arg_radix(KK_RUST_BIN_TAG, v); }

/* Per-type integer wrappers.  The bridge wraps `new_display::<T>` for
 * each non-i64 numeric T so the runtime renderer can mask /
 * sign-extend correctly when printing. */
int64_t kk_rust_arg_u32(int64_t v) { return kk_rust_arg_radix(KK_RUST_U32_TAG, v); }
int64_t kk_rust_arg_i32(int64_t v) { return kk_rust_arg_radix(KK_RUST_I32_TAG, v); }
int64_t kk_rust_arg_u64(int64_t v) { return kk_rust_arg_radix(KK_RUST_U64_TAG, v); }
int64_t kk_rust_arg_u16(int64_t v) { return kk_rust_arg_radix(KK_RUST_U16_TAG, v); }
int64_t kk_rust_arg_i16(int64_t v) { return kk_rust_arg_radix(KK_RUST_I16_TAG, v); }
int64_t kk_rust_arg_u8(int64_t v)  { return kk_rust_arg_radix(KK_RUST_U8_TAG, v); }
int64_t kk_rust_arg_i8(int64_t v)  { return kk_rust_arg_radix(KK_RUST_I8_TAG, v); }

/* Float wrappers.  The bridge bit-casts the f64/f32 literal value to
 * i64 in CoreTranslate.parseConstLit, then wraps with these.  The
 * cell carries the IEEE bit pattern; the printer reinterprets via
 * memcpy when rendering.  f32 bits live in the low 32 bits of the
 * inner i64; f64 bits fill the whole word. */
int64_t kk_rust_arg_f64(int64_t v) { return kk_rust_arg_radix(KK_RUST_F64_TAG, v); }
int64_t kk_rust_arg_f32(int64_t v) { return kk_rust_arg_radix(KK_RUST_F32_TAG, v); }

/* Named-struct builders.  Cell layout:
 *   tag        = KK_RUST_STRUCT_TAG
 *   field 0    = kk_string holding the type name (e.g. "Point")
 *   field 1    = kk_string holding comma-separated field names
 *                (e.g. "x,y") — empty string for tuple-style ctors
 *   field 2..N = value of each named field, in source order
 *
 * The Debug printer looks at fields 0/1 to format
 *   Point { x: 7, y: 13 }
 * and skips the metadata when iterating field values. */
static int64_t kk_rust_struct_alloc(int64_t nvals, int64_t name, int64_t names) {
    int64_t cell = kk_alloc_con(KK_RUST_STRUCT_TAG, 2 + nvals);
    kk_set_field(cell, 0, name);
    kk_set_field(cell, 1, names);
    return cell;
}
int64_t kk_rust_struct_0(int64_t name, int64_t names) {
    return kk_rust_struct_alloc(0, name, names);
}
int64_t kk_rust_struct_1(int64_t name, int64_t names, int64_t a) {
    int64_t c = kk_rust_struct_alloc(1, name, names);
    kk_set_field(c, 2, a); return c;
}
int64_t kk_rust_struct_2(int64_t name, int64_t names, int64_t a, int64_t b) {
    int64_t c = kk_rust_struct_alloc(2, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); return c;
}
int64_t kk_rust_struct_3(int64_t name, int64_t names, int64_t a, int64_t b, int64_t cc) {
    int64_t c = kk_rust_struct_alloc(3, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); kk_set_field(c, 4, cc); return c;
}
int64_t kk_rust_struct_4(int64_t name, int64_t names, int64_t a, int64_t b, int64_t cc, int64_t d) {
    int64_t c = kk_rust_struct_alloc(4, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); kk_set_field(c, 4, cc); kk_set_field(c, 5, d); return c;
}
int64_t kk_rust_struct_5(int64_t name, int64_t names,
                          int64_t a, int64_t b, int64_t cc, int64_t d, int64_t e) {
    int64_t c = kk_rust_struct_alloc(5, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); kk_set_field(c, 4, cc);
    kk_set_field(c, 5, d); kk_set_field(c, 6, e); return c;
}
int64_t kk_rust_struct_6(int64_t name, int64_t names,
                          int64_t a, int64_t b, int64_t cc, int64_t d, int64_t e, int64_t f) {
    int64_t c = kk_rust_struct_alloc(6, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); kk_set_field(c, 4, cc);
    kk_set_field(c, 5, d); kk_set_field(c, 6, e); kk_set_field(c, 7, f); return c;
}
int64_t kk_rust_struct_7(int64_t name, int64_t names,
                          int64_t a, int64_t b, int64_t cc, int64_t d, int64_t e, int64_t f, int64_t g) {
    int64_t c = kk_rust_struct_alloc(7, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); kk_set_field(c, 4, cc);
    kk_set_field(c, 5, d); kk_set_field(c, 6, e); kk_set_field(c, 7, f);
    kk_set_field(c, 8, g); return c;
}
int64_t kk_rust_struct_8(int64_t name, int64_t names,
                          int64_t a, int64_t b, int64_t cc, int64_t d, int64_t e, int64_t f, int64_t g, int64_t h) {
    int64_t c = kk_rust_struct_alloc(8, name, names);
    kk_set_field(c, 2, a); kk_set_field(c, 3, b); kk_set_field(c, 4, cc);
    kk_set_field(c, 5, d); kk_set_field(c, 6, e); kk_set_field(c, 7, f);
    kk_set_field(c, 8, g); kk_set_field(c, 9, h); return c;
}

/* Render an arg to a freshly-malloc'd C string.  Caller frees.
 * Mirrors kk_rust_print_one_arg but goes to a buffer instead of stdout. */
static char* kk_rust_render_one_arg(int64_t v, size_t* out_len) {
    char* result = NULL;
    size_t len = 0;
    if (kk_is_heap_ptr(v) && kk_nfields(v) == 1) {
        int64_t tag = kk_tag(v);
        int64_t inner = kk_field(v, 0);
        const char* fmt = NULL;
        /* Per-type integer wrappers (Display formatter, type-aware). */
        if (tag == KK_RUST_U32_TAG) {
            int wrote = asprintf(&result, "%u", (uint32_t)(inner & 0xFFFFFFFF));
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_I32_TAG) {
            int wrote = asprintf(&result, "%d", (int32_t)(inner & 0xFFFFFFFF));
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_U64_TAG) {
            int wrote = asprintf(&result, "%llu", (unsigned long long)(uint64_t)inner);
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_U16_TAG) {
            int wrote = asprintf(&result, "%u", (unsigned)(inner & 0xFFFF));
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_I16_TAG) {
            int wrote = asprintf(&result, "%d", (int)(int16_t)(inner & 0xFFFF));
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_U8_TAG) {
            int wrote = asprintf(&result, "%u", (unsigned)(inner & 0xFF));
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_I8_TAG) {
            int wrote = asprintf(&result, "%d", (int)(int8_t)(inner & 0xFF));
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        /* Floats: reinterpret bit pattern and render the way Rust's
         * Display does — `%g`-equivalent that elides the fraction for
         * whole-valued floats (3.0 → "3") and keeps significant digits
         * for fractions (3.14 → "3.14").  Precision-aware printing is
         * handled by the spec path; here we render the bare default. */
        if (tag == KK_RUST_F64_TAG) {
            double d;
            uint64_t bits = (uint64_t)inner;
            memcpy(&d, &bits, sizeof(d));
            int wrote = asprintf(&result, "%g", d);
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_F32_TAG) {
            float f;
            uint32_t bits = (uint32_t)(inner & 0xFFFFFFFF);
            memcpy(&f, &bits, sizeof(f));
            int wrote = asprintf(&result, "%g", (double)f);
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote; return result;
        }
        if (tag == KK_RUST_HEX_LO_TAG)      fmt = "%lx";
        else if (tag == KK_RUST_HEX_HI_TAG) fmt = "%lX";
        else if (tag == KK_RUST_OCT_TAG)    fmt = "%lo";
        else if (tag == KK_RUST_BIN_TAG) {
            uint64_t u = (uint64_t)inner;
            char buf[65];
            int blen = 0;
            if (u == 0) { buf[blen++] = '0'; }
            else { while (u && blen < 64) { buf[blen++] = '0' + (int)(u & 1); u >>= 1; } }
            result = (char*)malloc(blen + 1);
            for (int i = 0; i < blen; i++) result[i] = buf[blen - 1 - i];
            result[blen] = 0;
            *out_len = blen;
            return result;
        }
        else if (tag == KK_RUST_DEBUG_TAG) {
            if (kk_is_string(inner)) {
                /* Render debug-quoted string to buffer. */
                size_t n = (size_t)kk_str_len(inner);
                char* bytes = kk_str_dup_cstr(inner);
                /* Worst-case: every byte becomes \xHH (4 bytes), plus 2 quotes + NUL. */
                size_t cap = n * 4 + 3;
                result = (char*)malloc(cap);
                size_t pos = 0;
                result[pos++] = '"';
                if (bytes) {
                    for (size_t i = 0; i < n; i++) {
                        unsigned char c = (unsigned char)bytes[i];
                        switch (c) {
                            case '\n': result[pos++] = '\\'; result[pos++] = 'n'; break;
                            case '\t': result[pos++] = '\\'; result[pos++] = 't'; break;
                            case '\r': result[pos++] = '\\'; result[pos++] = 'r'; break;
                            case '\"': result[pos++] = '\\'; result[pos++] = '"'; break;
                            case '\\': result[pos++] = '\\'; result[pos++] = '\\'; break;
                            default:
                                if (c >= 0x20 && c < 0x7f) result[pos++] = c;
                                else pos += snprintf(result + pos, 5, "\\x%02x", c);
                                break;
                        }
                    }
                    free(bytes);
                }
                result[pos++] = '"';
                result[pos] = 0;
                *out_len = pos;
                return result;
            }
            /* Debug == Display for non-string */
            fmt = "%ld";
            v = inner;
        }
        if (fmt) {
            /* asprintf isn't fully portable but our toolchain has it. */
            int wrote = asprintf(&result, fmt, (long)inner);
            if (wrote < 0) { *out_len = 0; return NULL; }
            *out_len = (size_t)wrote;
            return result;
        }
    }
    if (kk_is_string(v)) {
        size_t n = (size_t)kk_str_len(v);
        char* bytes = kk_str_dup_cstr(v);
        if (!bytes) { *out_len = 0; return NULL; }
        *out_len = n;
        return bytes;
    }
    int wrote = asprintf(&result, "%ld", (long)v);
    if (wrote < 0) { *out_len = 0; return NULL; }
    *out_len = (size_t)wrote;
    return result;
    (void)len;
}

/* Field-spec decoded structure. */
typedef struct {
    char    fill;
    int     align;    /* 0=left, 1=right, 2=center, 3=default */
    int     zero_pad;
    int     plus_sign;
    int     alt_form;
    int     has_width;
    int     has_precision;
    uint16_t width;
    uint16_t precision;
} kk_rust_spec_t;

/* Is this value a "numeric" type per Rust's default-alignment rule? */
static int kk_rust_arg_is_numeric(int64_t v) {
    if (kk_is_string(v)) return 0;
    if (kk_is_heap_ptr(v) && kk_nfields(v) == 1) {
        int64_t tag = kk_tag(v);
        if (tag == KK_RUST_HEX_LO_TAG || tag == KK_RUST_HEX_HI_TAG
            || tag == KK_RUST_OCT_TAG || tag == KK_RUST_BIN_TAG
            || tag == KK_RUST_U32_TAG || tag == KK_RUST_I32_TAG
            || tag == KK_RUST_U64_TAG || tag == KK_RUST_U16_TAG
            || tag == KK_RUST_I16_TAG || tag == KK_RUST_U8_TAG
            || tag == KK_RUST_I8_TAG
            || tag == KK_RUST_F64_TAG || tag == KK_RUST_F32_TAG)
            return 1;
        if (tag == KK_RUST_DEBUG_TAG) {
            int64_t inner = kk_field(v, 0);
            return !kk_is_string(inner);
        }
        return 1;  /* assume number for unknown 1-field cells */
    }
    return 1;  /* raw i64 = number */
}

/* Return the alternate-form prefix string for a radix-tagged arg
 * (or NULL if no prefix applies).  Used by kk_rust_print_arg_with_spec
 * to prepend `0x` / `0o` / `0b` when `#` flag is set. */
static const char* kk_rust_alt_prefix(int64_t v) {
    if (!kk_is_heap_ptr(v) || kk_nfields(v) != 1) return NULL;
    int64_t tag = kk_tag(v);
    if (tag == KK_RUST_HEX_LO_TAG) return "0x";
    if (tag == KK_RUST_HEX_HI_TAG) return "0x";  /* lowercase prefix per Rust convention */
    if (tag == KK_RUST_OCT_TAG)    return "0o";
    if (tag == KK_RUST_BIN_TAG)    return "0b";
    return NULL;
}

/* Print one arg with the given spec applied: render to buffer, apply
 * the `#` alt-form prefix (radix tags only), the `+` sign flag (if
 * numeric and non-negative), precision (truncate for strings;
 * min-digits for ints), then pad to `width` using `fill` chars on
 * the chosen alignment side. */
/* For float-tagged args, precision means decimal places: re-render
 * with `%.Nf` and update *len.  Returns 1 if handled, 0 if v is not
 * a float (caller should keep its existing rendering). */
static int kk_rust_render_float_precision(int64_t v, uint16_t precision,
                                          char** rendered, size_t* len)
{
    if (!kk_is_heap_ptr(v) || kk_nfields(v) != 1) return 0;
    int64_t tag = kk_tag(v);
    int64_t inner = kk_field(v, 0);
    if (tag == KK_RUST_F64_TAG) {
        double d;
        uint64_t bits = (uint64_t)inner;
        memcpy(&d, &bits, sizeof(d));
        char* fresh = NULL;
        int wrote = asprintf(&fresh, "%.*f", (int)precision, d);
        if (wrote < 0) return 0;
        free(*rendered);
        *rendered = fresh;
        *len = (size_t)wrote;
        return 1;
    }
    if (tag == KK_RUST_F32_TAG) {
        float f;
        uint32_t b32 = (uint32_t)(inner & 0xFFFFFFFF);
        memcpy(&f, &b32, sizeof(f));
        char* fresh = NULL;
        int wrote = asprintf(&fresh, "%.*f", (int)precision, (double)f);
        if (wrote < 0) return 0;
        free(*rendered);
        *rendered = fresh;
        *len = (size_t)wrote;
        return 1;
    }
    return 0;
}

static void kk_rust_print_arg_with_spec(int64_t v, const kk_rust_spec_t* spec) {
    size_t len = 0;
    char* rendered = kk_rust_render_one_arg(v, &len);
    if (!rendered) return;
    int is_num = kk_rust_arg_is_numeric(v);
    /* For floats with a precision spec, re-render up front with `%.Nf`
     * so the rest of the pipeline (sign flag, width pad) sees the
     * fully-formatted decimal string.  This must happen before the
     * sign-flag prepend below, otherwise the re-render would discard
     * a manually-prepended '+'. */
    int float_precision_applied = 0;
    if (spec->has_precision
        && kk_rust_render_float_precision(v, spec->precision, &rendered, &len))
    {
        float_precision_applied = 1;
    }
    /* Apply `#` alt-form: prepend radix prefix to the rendered
     * buffer before any other transformations.  The prefix length is
     * tracked separately so the zero-pad path can peel it off and
     * insert zeros between prefix and digits (matching Rust's
     * `{:#010x}` → "0x000000ff" placement). */
    size_t prefix_len = 0;
    if (spec->alt_form) {
        const char* pfx = kk_rust_alt_prefix(v);
        if (pfx) {
            size_t plen = strlen(pfx);
            char* withpfx = (char*)malloc(len + plen + 1);
            if (withpfx) {
                memcpy(withpfx, pfx, plen);
                memcpy(withpfx + plen, rendered, len);
                withpfx[len + plen] = 0;
                free(rendered);
                rendered = withpfx;
                len = len + plen;
                prefix_len = plen;
            }
        }
    }
    /* Apply `+` sign flag: prepend '+' if numeric and not already
     * signed (i.e. doesn't start with '-' or '+').  Skip the prefix
     * region so the sign sits between prefix and digits.  Done
     * before precision so the sign joins the digit count properly. */
    if (spec->plus_sign && is_num && len > prefix_len
        && rendered[prefix_len] != '-' && rendered[prefix_len] != '+')
    {
        char* signed_buf = (char*)malloc(len + 2);
        if (signed_buf) {
            memcpy(signed_buf, rendered, prefix_len);
            signed_buf[prefix_len] = '+';
            memcpy(signed_buf + prefix_len + 1, rendered + prefix_len, len - prefix_len);
            signed_buf[len + 1] = 0;
            free(rendered);
            rendered = signed_buf;
            len = len + 1;
        }
    }
    /* Apply precision before width.  For strings, precision is the
     * max byte count (truncate).  For floats, precision sets the
     * decimal-place count (`%.Nf`).  For integers, precision sets
     * the minimum number of digits (zero-pad on the left between
     * sign and digits). */
    if (spec->has_precision) {
        if (!is_num) {
            if (spec->precision < len) len = spec->precision;
        } else if (float_precision_applied) {
            /* Float precision already applied at the top; the sign flag
             * may have prepended a '+' that the digit-count path would
             * incorrectly treat as part of an integer.  Skip it. */
        } else {
            /* Find digit start: skip leading radix prefix (`0x`/`0o`/`0b`)
             * and any sign character ('-' / '+'). */
            size_t digit_start = prefix_len;
            if (len > digit_start
                && (rendered[digit_start] == '-' || rendered[digit_start] == '+'))
                digit_start += 1;
            size_t digit_count = len - digit_start;
            if (spec->precision > digit_count) {
                size_t extra = spec->precision - digit_count;
                size_t new_len = len + extra;
                char* padded = (char*)malloc(new_len + 1);
                if (padded) {
                    memcpy(padded, rendered, digit_start);
                    for (size_t i = 0; i < extra; i++) padded[digit_start + i] = '0';
                    memcpy(padded + digit_start + extra, rendered + digit_start, digit_count);
                    padded[new_len] = 0;
                    free(rendered);
                    rendered = padded;
                    len = new_len;
                }
            }
        }
    }
    if (!spec->has_width || spec->width <= len) {
        fwrite(rendered, 1, len, stdout);
        free(rendered);
        return;
    }
    size_t pad = spec->width - len;
    int eff_align = spec->align;
    if (eff_align == 3) {
        /* Default alignment: numeric → right, string → left. */
        eff_align = is_num ? 1 : 0;
    }
    char fill = spec->fill;
    if (spec->zero_pad && is_num) {
        fill = '0';
        eff_align = 1;  /* zero-pad implies right-align */
    }
    /* Sign-/prefix-aware zero-pad: emit any radix prefix and sign
     * first, then pad with '0', then the digits.  The "fixed front"
     * is everything before the actual digits — `0x` + optional sign,
     * or just sign, or just prefix. */
    if (spec->zero_pad && is_num) {
        size_t fixed_front = prefix_len;
        if (len > fixed_front
            && (rendered[fixed_front] == '-' || rendered[fixed_front] == '+'))
        {
            fixed_front += 1;
        }
        if (fixed_front > 0) {
            fwrite(rendered, 1, fixed_front, stdout);
            for (size_t i = 0; i < pad; i++) putchar('0');
            fwrite(rendered + fixed_front, 1, len - fixed_front, stdout);
            free(rendered);
            return;
        }
    }
    switch (eff_align) {
        case 0:  /* left */
            fwrite(rendered, 1, len, stdout);
            for (size_t i = 0; i < pad; i++) putchar(fill);
            break;
        case 1:  /* right */
            for (size_t i = 0; i < pad; i++) putchar(fill);
            fwrite(rendered, 1, len, stdout);
            break;
        case 2: { /* center */
            size_t left_pad = pad / 2;
            size_t right_pad = pad - left_pad;
            for (size_t i = 0; i < left_pad; i++) putchar(fill);
            fwrite(rendered, 1, len, stdout);
            for (size_t i = 0; i < right_pad; i++) putchar(fill);
            break;
        }
    }
    free(rendered);
}

/* Decode 4 spec bytes into a kk_rust_spec_t.  Width / precision come
 * separately (their presence is signalled by the marker byte and
 * the align byte's bits 3 / 4). */
static void kk_rust_decode_spec(const unsigned char* sp, kk_rust_spec_t* out) {
    out->fill      = (char)sp[0];
    /* sp[1] reserved / always 0 */
    out->plus_sign = (sp[2] & 0x20) != 0;
    out->alt_form  = (sp[2] & 0x80) != 0;
    unsigned char a = sp[3];
    /* Alignment is encoded in bits 5–6 of the align byte:
     *   00 = left, 01 = right, 10 = center, 11 = default. */
    int align_bits = (a >> 5) & 0x3;
    switch (align_bits) {
        case 0: out->align = 0; break;
        case 1: out->align = 1; break;
        case 2: out->align = 2; break;
        case 3: out->align = 3; break;
    }
    out->zero_pad      = (a & 0x01) != 0;
    out->has_width     = 0;
    out->has_precision = (a & 0x10) != 0;
    out->width         = 0;
    out->precision     = 0;
}

int64_t kk_rust_print_args(int64_t template_str, int64_t args_struct) {
    /* Template encoding (Rust 2024+):
     *   template := piece* '\x00'
     *   piece    := '\xc0'                            (plain placeholder)
     *            |  '\xc1' <spec>{4}                  (placeholder + spec, no width)
     *            |  '\xc3' <spec>{4} <width>{2}       (placeholder + spec + width)
     *            |  <len> <byte>{len}                  (literal piece)
     *
     * Spec bytes (4):
     *   [0] fill char (e.g. 0x20 = ' ', 0x30 = '0', 0x78 = 'x', …)
     *   [1] reserved/zero
     *   [2] sign/alt-form flags
     *         bit 5 (0x20): '+' sign
     *         bit 7 (0x80): '#' alternate form
     *   [3] align byte
     *         nibble HI (bits 4-7): alignment code
     *           0 = left, 2 = right, 4 = center, 6 = default
     *         bit 3 = has-width (also signalled by c3 vs c1 marker)
     *         bit 0 = zero-pad flag
     *
     * Width bytes (2, little-endian u16): minimum field width. */
    size_t total;
    unsigned char* buf = kk_rust_decode_template(template_str, &total);
    if (!buf) return 0;
    const unsigned char* p = buf;
    const unsigned char* end = p + total;
    int arg_idx = 0;
    while (p < end) {
        unsigned char b = *p++;
        if (b == 0) break;                              /* terminator */
        if (b == 0xc0) {                                /* plain placeholder */
            int64_t v = kk_args_extract(args_struct, arg_idx);
            kk_rust_print_one_arg(v);
            arg_idx++;
        } else if ((b & 0xC0) == 0xC0 && (b & 0x01) != 0) {
            /* Placeholder with spec.  Marker byte 0xc{1,3,5,7}:
             *   bit 0 = has-spec (always 1 here)
             *   bit 1 = has-width (extra 2 bytes after spec)
             *   bit 2 = has-precision-value (extra 2 bytes after width) */
            int has_width = (b & 0x02) != 0;
            int has_prec_value = (b & 0x04) != 0;
            int64_t v = kk_args_extract(args_struct, arg_idx);
            arg_idx++;
            kk_rust_spec_t spec;
            if (p + 4 > end) { p = end; continue; }
            kk_rust_decode_spec(p, &spec);
            p += 4;
            if (has_width && p + 2 <= end) {
                spec.has_width = 1;
                spec.width = (uint16_t)p[0] | ((uint16_t)p[1] << 8);
                p += 2;
            }
            if (has_prec_value && p + 2 <= end) {
                spec.has_precision = 1;
                spec.precision = (uint16_t)p[0] | ((uint16_t)p[1] << 8);
                p += 2;
            }
            /* `{:.0}` is encoded as 0xc1 (no precision-value bit) with
             * the align byte's precision-flag bit set; kk_rust_decode_spec
             * already set has_precision=1 and precision=0 for that case. */
            kk_rust_print_arg_with_spec(v, &spec);
        } else {                                        /* literal piece, len=b */
            size_t len = b;
            size_t n = (p + len > end) ? (size_t)(end - p) : len;
            fwrite(p, 1, n, stdout);
            p += n;
        }
    }
    free(buf);
    return 0;
}

int64_t kk_rust_print_dispatch(int64_t v) {
    /* Dispatch on the value's shape: kk_string goes straight to
     * print_str (the from_str fast path); a packed
     * (template, args) cell tagged KK_RUST_FMT_TAG goes through
     * the formatted walker. */
    if (kk_is_string(v)) {
        kk_print_str(v);
        return 0;
    }
    if (kk_is_heap_ptr(v) && kk_tag(v) == KK_RUST_FMT_TAG && kk_nfields(v) == 2) {
        int64_t template_str = kk_field(v, 0);
        int64_t args_struct = kk_field(v, 1);
        return kk_rust_print_args(template_str, args_struct);
    }
    /* Fallback: print as decimal (matches the previous broken
     * behaviour for unsupported shapes). */
    printf("%ld", (long)v);
    return 0;
}

/* Field access from the Rust bridge.  Returns kk_field(base, idx) if
 * base is a heap pointer; otherwise returns base verbatim.  The
 * latter case matches CheckedAdd/Mul-style WithOverflow tuples that
 * the bridge pre-flattens to plain arithmetic (the "base" is already
 * the result i64, not a tuple cell).  The former handles genuine
 * RvAggregate-constructed tuples.
 *
 * When extracting a heap field, we kk_retain the result before
 * returning so it survives a subsequent drop of `base` — Perceus
 * sometimes inserts the parent drop before the field is consumed
 * (the bridge's `_N = _M.K` pattern produces an alias whose
 * lifetime extends past the parent's last use). */
int64_t kk_rust_field_safe(int64_t base, int64_t idx) {
    if (kk_is_heap_ptr(base)) {
        int64_t v = kk_field(base, idx);
        kk_retain(v);
        return v;
    }
    return base;
}

/* Bare-name aliases so the MLIR emitter's PAP wrapping resolves. */
int64_t rust_args_pack(int64_t, int64_t)
  __attribute__((alias("kk_rust_args_pack")));
int64_t rust_print_dispatch(int64_t)
  __attribute__((alias("kk_rust_print_dispatch")));
int64_t rust_field_safe(int64_t, int64_t)
  __attribute__((alias("kk_rust_field_safe")));
int64_t rust_arg_debug(int64_t)
  __attribute__((alias("kk_rust_arg_debug")));
int64_t rust_arg_lower_hex(int64_t)
  __attribute__((alias("kk_rust_arg_lower_hex")));
int64_t rust_arg_upper_hex(int64_t)
  __attribute__((alias("kk_rust_arg_upper_hex")));
int64_t rust_arg_octal(int64_t)
  __attribute__((alias("kk_rust_arg_octal")));
int64_t rust_arg_binary(int64_t)
  __attribute__((alias("kk_rust_arg_binary")));
int64_t rust_arg_u32(int64_t) __attribute__((alias("kk_rust_arg_u32")));
int64_t rust_arg_i32(int64_t) __attribute__((alias("kk_rust_arg_i32")));
int64_t rust_arg_u64(int64_t) __attribute__((alias("kk_rust_arg_u64")));
int64_t rust_arg_u16(int64_t) __attribute__((alias("kk_rust_arg_u16")));
int64_t rust_arg_i16(int64_t) __attribute__((alias("kk_rust_arg_i16")));
int64_t rust_arg_u8(int64_t)  __attribute__((alias("kk_rust_arg_u8")));
int64_t rust_arg_i8(int64_t)  __attribute__((alias("kk_rust_arg_i8")));
int64_t rust_arg_f64(int64_t) __attribute__((alias("kk_rust_arg_f64")));
int64_t rust_arg_f32(int64_t) __attribute__((alias("kk_rust_arg_f32")));
int64_t rust_struct_0(int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_0")));
int64_t rust_struct_1(int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_1")));
int64_t rust_struct_2(int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_2")));
int64_t rust_struct_3(int64_t, int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_3")));
int64_t rust_struct_4(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_4")));
int64_t rust_struct_5(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_5")));
int64_t rust_struct_6(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_6")));
int64_t rust_struct_7(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_7")));
int64_t rust_struct_8(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t)
  __attribute__((alias("kk_rust_struct_8")));

/* List append for Haskell [Char] cons-lists: a ++ b.
 * Walks a (collecting into a buffer), then prepends each element onto
 * b in reverse to preserve original order.  Recurses if a exceeds the
 * stack buffer (rare).
 *
 * Exposed under both names: kk_haskell_chars_concat for direct calls
 * through the intrinsic dispatcher, and haskell_chars_concat (no kk_
 * prefix) for PAP wrappers that the MLIR emitter constructs when
 * (++) is partially applied (e.g. inside derived $cshowsPrec). */
int64_t haskell_chars_concat(int64_t, int64_t)
  __attribute__((alias("kk_haskell_chars_concat")));
int64_t kk_haskell_chars_concat(int64_t a, int64_t b) {
    enum { CAP = 256 };
    int64_t buf[CAP];
    int n = 0;
    int64_t cur = a;
    while (kk_tag(cur) != KK_HASKELL_NIL_TAG && n < CAP) {
        buf[n++] = kk_field(cur, 0);
        cur = kk_field(cur, 1);
    }
    int64_t result = b;
    /* Tail beyond the buffer: recurse on the remainder. */
    if (kk_tag(cur) != KK_HASKELL_NIL_TAG) {
        result = kk_haskell_chars_concat(cur, result);
    }
    for (int i = n - 1; i >= 0; i--) {
        result = kk_cons_char_cell(buf[i], result);
    }
    return result;
}

/* Show Int's showList: format an Int cons-list as "[n1,n2,n3]" and
 * prepend onto tail.  Used by the GHC bridge to intercept the
 * specialised Show [Int] method ($fShowInt_$cshowList).
 *
 * Each input cell's field 0 is a raw i64 (the bridge strips I#
 * wrappers via the existing trExpr (App I# _) cases on list
 * construction); field 1 is the tail.
 */
int64_t kk_int_list_to_haskell_chars(int64_t list, int64_t tail) {
    /* Collect values into a small stack buffer; if the list overflows
     * the buffer we walk twice (rare for everyday hello-world output).
     * 128 elements is plenty for any reasonable print call. */
    enum { CAP = 128 };
    int64_t vals[CAP];
    int len = 0;
    int64_t cur = list;
    while (kk_tag(cur) != KK_HASKELL_NIL_TAG && len < CAP) {
        vals[len++] = kk_field(cur, 0);
        cur = kk_field(cur, 1);
    }
    /* If we hit CAP, the rest is in cur (still a valid cons-list). */

    /* Build the result tail-first: tail, ']', then for each value in
     * reverse, the int chars (with a leading ',' for all but the
     * first emitted element).  Finally prepend '['. */
    int64_t result = tail;
    /* If cur is still non-nil, recursively format it (without the
     * outer brackets) before the ']'. */
    if (kk_tag(cur) != KK_HASKELL_NIL_TAG) {
        /* Format cur as if it were a separate list, then merge — but
         * we want the comma separator between blocks.  Simplest: take
         * the recursive result of formatting [cur_elems] and splice
         * its inner content (drop the leading '[' and the trailing ']')
         * onto result.  Cleaner approach: emit cur's elements directly
         * here using another stack pass. */
        int64_t vals2[CAP];
        int len2 = 0;
        int64_t cur2 = cur;
        while (kk_tag(cur2) != KK_HASKELL_NIL_TAG && len2 < CAP) {
            vals2[len2++] = kk_field(cur2, 0);
            cur2 = kk_field(cur2, 1);
        }
        /* (Lists of >256 elements are truncated.) */
        result = kk_cons_char_cell((int64_t)']', result);
        for (int i = len2 - 1; i >= 0; i--) {
            result = kk_int_to_haskell_chars(vals2[i], result);
            /* Always a comma before — we know there's at least one in-buffer elem ahead. */
            result = kk_cons_char_cell((int64_t)',', result);
        }
    } else {
        result = kk_cons_char_cell((int64_t)']', result);
    }
    /* Now prepend the in-buffer elements in reverse.  The last
     * element emitted (i = 0) is the head of the source list and
     * should NOT be preceded by a comma. */
    for (int i = len - 1; i >= 0; i--) {
        result = kk_int_to_haskell_chars(vals[i], result);
        if (i > 0) {
            result = kk_cons_char_cell((int64_t)',', result);
        }
    }
    result = kk_cons_char_cell((int64_t)'[', result);
    return result;
}


/* First-class strings — rope-based, UTF-8.
 *
 * A Frankenstein string is a heap-allocated kk_string_t header. The
 * header carries a refcount and either a leaf (a contiguous UTF-8
 * byte run, owned or borrowed) or a concat node (two child strings).
 *
 *   LEAF   { bytes, byte_len, owns_bytes }
 *   CONCAT { left,  right,    cached_len }
 *
 * - kk_str_concat is O(1): it allocates one CONCAT header.
 * - kk_str_len is O(1): the total byte length is cached in the header.
 * - kk_str_char_len is O(n): walks the rope, counts UTF-8 lead bytes.
 * - kk_str_flatten collapses a rope into a single owned LEAF.
 * - kk_println_str fast-paths a leaf via fwrite; ropes are flattened
 *   into a temporary buffer.
 *
 * Refcounting follows the same Perceus discipline as boxed values:
 * each header starts at rc=1; kk_str_retain bumps it; kk_str_drop
 * decrements and, on zero, recursively drops children and frees
 * owned byte buffers. Static literals use owns_bytes=0 so the
 * .rodata buffer is never freed.
 *
 * ByteStrings reuse the same kk_string_t representation but expose
 * a byte-oriented API (random byte indexing, no UTF-8 awareness).
 */

#define KK_STR_LEAF   0
#define KK_STR_CONCAT 1
#define KK_STR_SLICE  2  /* view into a parent string; bytes points into parent's buffer */

typedef struct kk_string_s kk_string_t;
struct kk_string_s {
    int64_t  magic;       /* KK_STRING_MAGIC — enables O(1) kk_is_string */
    int64_t  rc;          /* refcount (Perceus) */
    int64_t  byte_len;    /* total UTF-8 byte length, cached */
    int32_t  kind;        /* KK_STR_LEAF | KK_STR_CONCAT */
    int32_t  owns_bytes;  /* leaf only: 1 → free(bytes) on drop */
    union {
        const char* bytes;                              /* LEAF */
        struct { kk_string_t* l; kk_string_t* r; } cat; /* CONCAT */
    } u;
};

/* Forward declarations (the public header isn't included in this TU). */
int64_t kk_string_empty(void);
void    kk_str_drop(int64_t s_i);
void    kk_set_field(int64_t ptr, int64_t idx, int64_t value);

static kk_string_t* kk_str_alloc_leaf(const char* bytes, int64_t byte_len, int owns) {
    kk_string_t* s = (kk_string_t*)malloc(sizeof(kk_string_t));
    if (!s) return NULL;
    s->magic      = KK_STRING_MAGIC;
    s->rc         = 1;
    s->byte_len   = byte_len;
    s->kind       = KK_STR_LEAF;
    s->owns_bytes = owns;
    s->u.bytes    = bytes;
    return s;
}

static kk_string_t* kk_str_alloc_concat(kk_string_t* l, kk_string_t* r) {
    kk_string_t* s = (kk_string_t*)malloc(sizeof(kk_string_t));
    if (!s) return NULL;
    s->magic      = KK_STRING_MAGIC;
    s->rc         = 1;
    s->byte_len   = (l ? l->byte_len : 0) + (r ? r->byte_len : 0);
    s->kind       = KK_STR_CONCAT;
    s->owns_bytes = 0;
    s->u.cat.l    = l;
    s->u.cat.r    = r;
    /* Concat node owns references to its children.
     * Retain them so they survive independent drops of the originals. */
    if (l) l->rc++;
    if (r) r->rc++;
    return s;
}

/* Allocate a slice: a view into parent's buffer at a given offset.
 * O(1) — no copying. Parent is retained (rc++) so its buffer stays alive.
 * Uses union cat: cat.l = parent, cat.r = (kk_string_t*)bytes_ptr. */
static kk_string_t* kk_str_alloc_slice(kk_string_t* parent, const char* bytes, int64_t byte_len) {
    kk_string_t* s = (kk_string_t*)malloc(sizeof(kk_string_t));
    if (!s) return NULL;
    s->magic      = KK_STRING_MAGIC;
    s->rc         = 1;
    s->byte_len   = byte_len;
    s->kind       = KK_STR_SLICE;
    s->owns_bytes = 0;
    s->u.cat.l    = parent;
    s->u.cat.r    = (kk_string_t*)bytes;  /* reinterpret: bytes pointer stored in cat.r */
    if (parent) parent->rc++;
    return s;
}

/* Get the bytes pointer from a leaf or slice (no flattening needed). */
static const char* kk_str_bytes(kk_string_t* s) {
    if (s->kind == KK_STR_SLICE)
        return (const char*)s->u.cat.r;
    return s->u.bytes;
}

/* Public API: create a slice of an existing kk_string.
 * Flattens the parent first (so we have a contiguous buffer to slice into).
 * offset/length are in bytes. Returns a new kk_string (registered). */
int64_t kk_str_slice(int64_t parent_i, int64_t byte_offset, int64_t byte_len) {
    if (byte_len <= 0) return kk_string_empty();
    /* Flatten parent to ensure contiguous bytes */
    int64_t flat = kk_str_flatten(parent_i);
    kk_string_t* parent = (kk_string_t*)flat;
    if (!parent || byte_offset >= parent->byte_len) return kk_string_empty();
    if (byte_offset + byte_len > parent->byte_len)
        byte_len = parent->byte_len - byte_offset;
    const char* bytes = kk_str_bytes(parent) + byte_offset;
    int64_t r = (int64_t)kk_str_alloc_slice(parent, bytes, byte_len);
    kk_register_string(r);
    return r;
}

int64_t kk_string_from_literal(int64_t bytes_ptr, int64_t byte_len) {
    int64_t r = (int64_t)kk_str_alloc_leaf((const char*)bytes_ptr, byte_len, 0);
    kk_register_string(r);
    return r;
}

int64_t kk_string_from_cstr(int64_t cstr_ptr) {
    const char* p = (const char*)cstr_ptr;
    if (p == NULL) return kk_string_empty();
    int64_t n = 0;
    while (p[n] != '\0') n++;
    int64_t r = (int64_t)kk_str_alloc_leaf(p, n, 0);
    kk_register_string(r);
    return r;
}

int64_t kk_string_empty(void) {
    int64_t r = (int64_t)kk_str_alloc_leaf("", 0, 0);
    kk_register_string(r);
    return r;
}

/* Build a single-character string from a Unicode codepoint.  UTF-8
 * encodes the codepoint into 1-4 bytes (replacement char for invalid
 * inputs).  Used by Koka's `char/string` intrinsic. */
int64_t kk_string_from_char(int64_t codepoint) {
    int32_t cp = (int32_t)codepoint;
    if (cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF))
        cp = 0xFFFD;
    char buf[5];
    int n;
    if (cp < 0x80) {
        buf[0] = (char)cp; n = 1;
    } else if (cp < 0x800) {
        buf[0] = (char)(0xC0 | (cp >> 6));
        buf[1] = (char)(0x80 | (cp & 0x3F));
        n = 2;
    } else if (cp < 0x10000) {
        buf[0] = (char)(0xE0 | (cp >> 12));
        buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[2] = (char)(0x80 | (cp & 0x3F));
        n = 3;
    } else {
        buf[0] = (char)(0xF0 | (cp >> 18));
        buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F));
        buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F));
        buf[3] = (char)(0x80 | (cp & 0x3F));
        n = 4;
    }
    buf[n] = '\0';
    /* Copy into a fresh malloc'd buffer so the rope owns the bytes
     * (kk_str_alloc_leaf with owns=1 takes responsibility for free). */
    char* owned = (char*)malloc((size_t)n + 1);
    if (!owned) return kk_string_empty();
    memcpy(owned, buf, (size_t)n + 1);
    int64_t r = (int64_t)kk_str_alloc_leaf(owned, n, 1);
    kk_register_string(r);
    return r;
}

int64_t kk_str_len(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return 0;
    return s->byte_len;
}

/* Walk a rope leaf-by-leaf, counting UTF-8 codepoints (lead bytes). */
static int64_t kk_str_char_count_rec(kk_string_t* s) {
    if (s == NULL) return 0;
    if (s->kind == KK_STR_LEAF || s->kind == KK_STR_SLICE) {
        const unsigned char* p = (const unsigned char*)kk_str_bytes(s);
        int64_t count = 0;
        for (int64_t i = 0; i < s->byte_len; i++) {
            if ((p[i] & 0xC0) != 0x80) count++;
        }
        return count;
    }
    return kk_str_char_count_rec(s->u.cat.l) + kk_str_char_count_rec(s->u.cat.r);
}

int64_t kk_str_char_len(int64_t s_i) {
    return kk_str_char_count_rec((kk_string_t*)s_i);
}

/* Print symbolic backtrace for the current C stack. Used to localize the
 * call site that fed a non-string value into kk_str_concat. */
static void kk_dump_backtrace(const char* label) {
    void* frames[32];
    int n = backtrace(frames, 32);
    fprintf(stderr, "%s: backtrace (%d frames):\n", label, n);
    char** sym = backtrace_symbols(frames, n);
    if (sym) {
        for (int i = 0; i < n; i++) {
            fprintf(stderr, "  [%2d] %s\n", i, sym[i]);
        }
        free(sym);
    }
    fflush(stderr);
}

int64_t kk_str_concat(int64_t a_i, int64_t b_i) {
    /* Force in case either operand is a thunk: plotkin-mode code can
     * deliver a kk_thunk_create_forced-wrapped string here, and reading
     * `->byte_len` directly off a thunk gets the thunk's field-1
     * (cached result pointer) interpreted as a length — garbage. */
    a_i = kk_thunk_force(a_i);
    b_i = kk_thunk_force(b_i);
    /* Validate magic on both inputs. A non-KKSTRING-magic value here is
     * almost always a closure-as-value leak: somewhere upstream a function
     * value (PAP / CLOS) was passed through `<>` as if it were a Text.
     * Print a backtrace so the offending call site is identifiable. */
    if (a_i != 0) {
        int64_t* hdr = (int64_t*)a_i;
        if (hdr[0] != KK_STRING_MAGIC) {
            fprintf(stderr, "kk_str_concat: non-string input a @%p, magic=%#lx\n",
                    (void*)a_i, (long)hdr[0]);
            /* Closure-like layout: dump fn ptr (field 0 in standard tag,
             * which is hdr[2] in our header layout: tag, rc, field_0…) and
             * resolve it via dladdr so we know WHICH function the closure
             * points to. */
            fprintf(stderr, "  hdr: [tag=%#lx rc=%ld f0=%#lx f1=%#lx f2=%#lx]\n",
                    (long)hdr[0], (long)hdr[1], (long)hdr[2],
                    (long)hdr[3], (long)hdr[4]);
            fprintf(stderr, "  f0 (closure fn ptr) -> resolve with: "
                    "nm <binary> | awk '$1<=%lx && next_addr>%lx' "
                    "(after sort)\n", (long)hdr[2], (long)hdr[2]);
            kk_dump_backtrace("kk_str_concat.a");
            abort();
        }
    }
    if (b_i != 0) {
        int64_t* hdr = (int64_t*)b_i;
        if (hdr[0] != KK_STRING_MAGIC) {
            fprintf(stderr, "kk_str_concat: non-string input b @%p, magic=%#lx\n",
                    (void*)b_i, (long)hdr[0]);
            fprintf(stderr, "  hdr: [tag=%#lx rc=%ld f0=%#lx f1=%#lx f2=%#lx]\n",
                    (long)hdr[0], (long)hdr[1], (long)hdr[2],
                    (long)hdr[3], (long)hdr[4]);
            fprintf(stderr, "  f0 (closure fn ptr) -> resolve with: "
                    "nm <binary> | awk '$1<=%lx && next_addr>%lx' "
                    "(after sort)\n", (long)hdr[2], (long)hdr[2]);
            kk_dump_backtrace("kk_str_concat.b");
            abort();
        }
    }
    kk_string_t* a = (kk_string_t*)a_i;
    kk_string_t* b = (kk_string_t*)b_i;
    if (a == NULL || a->byte_len == 0) {
        if (a != NULL) kk_str_drop(a_i);
        return b_i;
    }
    if (b == NULL || b->byte_len == 0) {
        if (b != NULL) kk_str_drop(b_i);
        return a_i;
    }
    int64_t r = (int64_t)kk_str_alloc_concat(a, b);
    kk_register_string(r);
    return r;
}

/* Copy a rope into a contiguous buffer at *out, advancing *out.
 *
 * Iterative walk to avoid stack overflow on deep left-spines.  The
 * JSON parser's pStrBody builds `acc <> chunk <> "\n" <> ...` with
 * one CONCAT node per escape, so a 100 KB string with many escapes
 * produces a left-spine 100K deep — the recursive version overflowed
 * the 8 MB user stack while compiling MlirEmit/Emitter_part17. */
static void kk_str_copy_into(kk_string_t* s, char** out) {
    /* Manual stack of right-subtrees yet to visit (in order). */
    enum { STK_INIT = 64 };
    kk_string_t** stk = NULL;
    size_t stk_cap = 0, stk_n = 0;
    while (s != NULL) {
        if (s->kind == KK_STR_LEAF || s->kind == KK_STR_SLICE) {
            const char* bytes = kk_str_bytes(s);
            for (int64_t i = 0; i < s->byte_len; i++) (*out)[i] = bytes[i];
            *out += s->byte_len;
            /* Pop next subtree, if any. */
            if (stk_n == 0) break;
            s = stk[--stk_n];
        } else {
            /* CONCAT: push right, descend left. */
            if (stk_n == stk_cap) {
                stk_cap = stk_cap ? stk_cap * 2 : STK_INIT;
                stk = (kk_string_t**)realloc(stk, stk_cap * sizeof(*stk));
            }
            stk[stk_n++] = s->u.cat.r;
            s = s->u.cat.l;
        }
    }
    free(stk);
}

int64_t kk_str_flatten(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return kk_string_empty();
    /* If the cell has an arena tag (CLOS/THUNK/etc.) at offset 0 instead
     * of KK_STRING_MAGIC, this Text reference is stale — most likely an
     * arena cell that was freed and reused (a known Perceus refcount
     * gap; see kk_compare for the analogous defensive workaround).
     * Returning the empty string lets callers (e.g. Data_Text_isSuffixOf)
     * compare lengths and report "no match" rather than aborting.
     * KK_STR_FLATTEN_TRACE=1 logs every occurrence so we can find the
     * root cause. */
    if (s->magic != KK_STRING_MAGIC) {
        if (getenv("KK_STR_FLATTEN_TRACE")) {
            extern int kk_arena_maybe_owns(const void* ptr);
            int in_arena = kk_arena_maybe_owns((const void*)s_i);
            fprintf(stderr,
                "kk_str_flatten: non-string cell at %p magic=%#lx arena=%d slot0=%#lx slot1=%#lx — returning empty\n",
                (void*)s_i, (long)s->magic, in_arena,
                (long)s->rc, (long)s->byte_len);
        }
        return kk_string_empty();
    }
    if (s->kind == KK_STR_LEAF || s->kind == KK_STR_SLICE) return s_i;
    int64_t n = s->byte_len;
    /* Sanity check: a valid kk_string_t can't be larger than the heap.
     * If byte_len looks like a heap/stack address (>= 16 GB), the
     * struct is corrupt — likely a layout misuse (e.g. a different
     * heap object with KK_STRING_MAGIC at offset 0 but different
     * remaining fields). Print a diagnostic with the full header
     * contents and the call site for offline analysis. */
    if (n < 0 || n > (1LL << 34)) {
        fprintf(stderr,
            "kk_str_flatten: corrupt kk_string_t at %p\n"
            "  magic=%#lx rc=%ld byte_len=%ld kind=%d owns=%d\n"
            "  u.bytes=%p (interpreted as cat: l=%p r=%p)\n",
            (void*)s_i, (long)s->magic, (long)s->rc, (long)s->byte_len,
            s->kind, s->owns_bytes,
            (void*)s->u.bytes, s->u.cat.l, s->u.cat.r);
        if (s->kind == KK_STR_CONCAT && s->u.cat.l) {
            kk_string_t* l = s->u.cat.l;
            fprintf(stderr, "  cat.l @%p: magic=%#lx rc=%ld byte_len=%ld kind=%d\n",
                (void*)l, (long)l->magic, (long)l->rc, (long)l->byte_len, l->kind);
        }
        if (s->kind == KK_STR_CONCAT && s->u.cat.r) {
            kk_string_t* r = s->u.cat.r;
            fprintf(stderr, "  cat.r @%p: magic=%#lx rc=%ld byte_len=%ld kind=%d\n",
                (void*)r, (long)r->magic, (long)r->rc, (long)r->byte_len, r->kind);
        }
        fflush(stderr);
        abort();
    }
    char* buf = (char*)malloc((size_t)n + 1);
    if (!buf) return 0;
    char* p = buf;
    kk_str_copy_into(s, &p);
    buf[n] = '\0';
    int64_t r = (int64_t)kk_str_alloc_leaf(buf, n, 1);
    kk_register_string(r);
    return r;
}

void kk_print_str(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return;
    if (s->kind == KK_STR_LEAF || s->kind == KK_STR_SLICE) {
        const char* bytes = kk_str_bytes(s);
        if (s->byte_len > 0) fwrite(bytes, 1, (size_t)s->byte_len, stdout);
        return;
    }
    /* Rope: flatten through a temporary buffer (no header allocation). */
    int64_t n = s->byte_len;
    char* buf = (char*)malloc((size_t)n);
    if (!buf) return;
    char* p = buf;
    kk_str_copy_into(s, &p);
    fwrite(buf, 1, (size_t)n, stdout);
    free(buf);
}

void kk_println_str(int64_t s_i) {
    kk_print_str(s_i);
    putchar('\n');
}

/* ---- Idris2 bridge helpers ---------------------------------------------
 * Foreign functions referenced by Idris2's Prelude after the shim's
 * --cg organir lowering.  Names match the C target extracted from
 * MkNmForeign CCS strings (e.g. "C:idris2_putStr,libidris2_support,
 * idris_support.h").  All take and return i64 for ABI uniformity.
 */

/* Idris2 IO primitive: write a string and return the world token (0). */
int64_t idris2_putStr(int64_t s_i, int64_t world) {
    (void)world;
    kk_print_str(s_i);
    return 0;
}

/* Idris2 StrHead primitive: return first byte of a string as an int.
 * Used by Prelude.Show.firstCharIs to test for a leading minus sign. */
int64_t idris_str_head(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL || s->byte_len == 0) return 0;
    if (s->kind == KK_STR_LEAF || s->kind == KK_STR_SLICE) {
        return (int64_t)(unsigned char)kk_str_bytes(s)[0];
    }
    /* Rope: flatten and read the first byte. */
    int64_t flat_i = kk_str_flatten(s_i);
    kk_string_t* flat = (kk_string_t*)flat_i;
    if (flat == NULL || flat->byte_len == 0) return 0;
    return (int64_t)(unsigned char)kk_str_bytes(flat)[0];
}

/* Idris2 StrCons primitive: prepend a Char (i64 Unicode codepoint) to a
 * String.  Encodes the codepoint as 1..4 UTF-8 bytes, allocates a leaf
 * string for them (heap-owned, freed when the leaf is dropped), then
 * concats with the input (O(1) rope concat). */
int64_t idris_str_cons(int64_t cp, int64_t s_i) {
    char* buf = (char*)malloc(4);
    if (!buf) return s_i;
    int64_t n;
    if (cp < 0x80)        { buf[0] = (char)cp;                                                                                                  n = 1; }
    else if (cp < 0x800)  { buf[0] = (char)(0xC0 | (cp >> 6));   buf[1] = (char)(0x80 | (cp & 0x3F));                                          n = 2; }
    else if (cp < 0x10000){ buf[0] = (char)(0xE0 | (cp >> 12));  buf[1] = (char)(0x80 | ((cp >> 6) & 0x3F)); buf[2] = (char)(0x80 | (cp & 0x3F)); n = 3; }
    else                  { buf[0] = (char)(0xF0 | (cp >> 18));  buf[1] = (char)(0x80 | ((cp >> 12) & 0x3F)); buf[2] = (char)(0x80 | ((cp >> 6) & 0x3F)); buf[3] = (char)(0x80 | (cp & 0x3F)); n = 4; }
    int64_t head = kk_str_alloc_leaf_owned(buf, n);
    return kk_str_concat(head, s_i);
}

/* Idris2 Crash primitive: print the message and abort. */
int64_t idris_crash(int64_t msg_i, int64_t _ignored) {
    (void)_ignored;
    fprintf(stderr, "idris_crash: ");
    kk_string_t* s = (kk_string_t*)msg_i;
    if (s != NULL && s->byte_len > 0) {
        int64_t flat = kk_str_flatten(msg_i);
        kk_string_t* fs = (kk_string_t*)flat;
        fwrite(kk_str_bytes(fs), 1, (size_t)fs->byte_len, stderr);
    }
    fputc('\n', stderr);
    abort();
    return 0;
}

/* OrganIR ERaise lowering (NmCrash): unhandled exception. */
int64_t _raise(int64_t e) {
    (void)e;
    fprintf(stderr, "frankenstein: unhandled raise\n");
    abort();
    return 0;
}

/* Double <-> i64 bit-cast helpers. */
static inline double kk_i64_to_double(int64_t x) {
    double d; memcpy(&d, &x, sizeof d); return d;
}
static inline int64_t kk_double_to_i64(double d) {
    int64_t x; memcpy(&x, &d, sizeof x); return x;
}

#define IDRIS_DBL1(NAME) \
    int64_t idris_double_##NAME(int64_t x) { \
        return kk_double_to_i64(NAME(kk_i64_to_double(x))); \
    }
#define IDRIS_DBL2(NAME) \
    int64_t idris_double_##NAME(int64_t a, int64_t b) { \
        return kk_double_to_i64(NAME(kk_i64_to_double(a), kk_i64_to_double(b))); \
    }

IDRIS_DBL1(sin)
IDRIS_DBL1(cos)
IDRIS_DBL1(tan)
IDRIS_DBL1(asin)
IDRIS_DBL1(acos)
IDRIS_DBL1(atan)
IDRIS_DBL1(sqrt)
IDRIS_DBL1(exp)
IDRIS_DBL1(log)
IDRIS_DBL1(floor)
int64_t idris_double_ceiling(int64_t x) {
    return kk_double_to_i64(ceil(kk_i64_to_double(x)));
}
IDRIS_DBL2(pow)

/* Double arithmetic on the i64 bit-pattern ABI.  Idris2's PrimFn Add/Sub/etc.
 * are typed (Add DoubleType vs Add IntType); the shim emits these names for
 * the Double case so we don't accidentally lower them to arith.addi etc. */
int64_t idris_double_add(int64_t a, int64_t b) {
    return kk_double_to_i64(kk_i64_to_double(a) + kk_i64_to_double(b));
}
int64_t idris_double_sub(int64_t a, int64_t b) {
    return kk_double_to_i64(kk_i64_to_double(a) - kk_i64_to_double(b));
}
int64_t idris_double_mul(int64_t a, int64_t b) {
    return kk_double_to_i64(kk_i64_to_double(a) * kk_i64_to_double(b));
}
int64_t idris_double_div(int64_t a, int64_t b) {
    return kk_double_to_i64(kk_i64_to_double(a) / kk_i64_to_double(b));
}
int64_t idris_double_neg(int64_t a) {
    return kk_double_to_i64(-kk_i64_to_double(a));
}
int64_t idris_double_lt (int64_t a, int64_t b) { return kk_i64_to_double(a) <  kk_i64_to_double(b); }
int64_t idris_double_lte(int64_t a, int64_t b) { return kk_i64_to_double(a) <= kk_i64_to_double(b); }
int64_t idris_double_eq (int64_t a, int64_t b) { return kk_i64_to_double(a) == kk_i64_to_double(b); }
int64_t idris_double_gte(int64_t a, int64_t b) { return kk_i64_to_double(a) >= kk_i64_to_double(b); }
int64_t idris_double_gt (int64_t a, int64_t b) { return kk_i64_to_double(a) >  kk_i64_to_double(b); }

#undef IDRIS_DBL1
#undef IDRIS_DBL2

/* Idris2 numeric casts that touch Double need explicit bit-cast plumbing
 * (Frankenstein passes everything as i64, so Doubles are bit-patterns). */
int64_t cast_Integer_Double(int64_t n) {
    return kk_double_to_i64((double)n);
}
int64_t cast_Double_Int(int64_t d) {
    return (int64_t)kk_i64_to_double(d);
}
/* Format a Double like Haskell/Idris2 `show :: Double -> String`:
 * shortest decimal that round-trips, with a mandatory "." or "e"
 * (so 1.0 prints as "1.0", not "1").  Handles NaN and Infinity. */
int64_t cast_Double_String(int64_t d) {
    double v = kk_i64_to_double(d);
    char tmp[64];
    int n;
    if (isnan(v)) {
        n = snprintf(tmp, sizeof tmp, "NaN");
    } else if (isinf(v)) {
        n = snprintf(tmp, sizeof tmp, v < 0 ? "-Infinity" : "Infinity");
    } else {
        /* Shortest round-trip: try precisions 1..17 (17 = DBL_DECIMAL_DIG). */
        n = 0;
        for (int prec = 1; prec <= 17; prec++) {
            n = snprintf(tmp, sizeof tmp, "%.*g", prec, v);
            if (n < 0) { n = 0; break; }
            double back = 0.0;
            sscanf(tmp, "%lf", &back);
            if (back == v) break;
        }
        /* Haskell-style: ensure a "." or "e" is present (1.0 not 1). */
        int has_dot_or_e = 0;
        for (int i = 0; i < n; i++) {
            if (tmp[i] == '.' || tmp[i] == 'e' || tmp[i] == 'E') {
                has_dot_or_e = 1;
                break;
            }
        }
        if (!has_dot_or_e && n + 2 < (int)sizeof tmp) {
            tmp[n++] = '.';
            tmp[n++] = '0';
            tmp[n]   = '\0';
        }
        /* Chez Scheme appends |<precision> to subnormal (denormal) flonums,
         * where precision is the bit-length of the stored 52-bit mantissa
         * (1..52).  Idris2's native chez backend inherits this, so byte-
         * identical surd output requires matching here. */
        uint64_t bits;
        memcpy(&bits, &v, sizeof bits);
        int exp_bits  = (int)((bits >> 52) & 0x7FF);
        uint64_t mant = bits & (((uint64_t)1 << 52) - 1);
        if (exp_bits == 0 && mant != 0) {
            int prec = 0;
            for (uint64_t m = mant; m != 0; m >>= 1) prec++;
            n += snprintf(tmp + n, sizeof tmp - (size_t)n, "|%d", prec);
        }
    }
    if (n < 0) n = 0;
    /* Copy onto the heap and hand ownership to the string (owns=1)
     * — the stack buffer can't outlive this call. */
    char* heap = (char*)malloc((size_t)n + 1);
    if (!heap) return kk_string_empty();
    memcpy(heap, tmp, (size_t)n);
    heap[n] = '\0';
    int64_t r = (int64_t)kk_str_alloc_leaf(heap, (int64_t)n, 1);
    kk_register_string(r);
    return r;
}

int64_t kk_str_eq(int64_t a_i, int64_t b_i) {
    kk_string_t* a = (kk_string_t*)a_i;
    kk_string_t* b = (kk_string_t*)b_i;
    if (a == b) return 1;
    int64_t la = (a ? a->byte_len : 0);
    int64_t lb = (b ? b->byte_len : 0);
    if (la != lb) return 0;
    if (la == 0) return 1;
    /* Flatten both into temporaries and compare. A smarter implementation
     * would walk both ropes lazily; for now this is correct and simple. */
    char* fa = (char*)malloc((size_t)la);
    char* fb = (char*)malloc((size_t)lb);
    if (!fa || !fb) { free(fa); free(fb); return 0; }
    char* pa = fa; kk_str_copy_into(a, &pa);
    char* pb = fb; kk_str_copy_into(b, &pb);
    int eq = 1;
    for (int64_t i = 0; i < la; i++) {
        if (fa[i] != fb[i]) { eq = 0; break; }
    }
    free(fa);
    free(fb);
    return eq;
}

int64_t kk_str_show_int(int64_t n) {
    /* Format a signed 64-bit integer as decimal digits, no libc deps. */
    int neg = 0;
    uint64_t u;
    if (n < 0) { neg = 1; u = (uint64_t)(-(n + 1)) + 1; }
    else       { u = (uint64_t)n; }
    char tmp[24];
    int len = 0;
    if (u == 0) { tmp[len++] = '0'; }
    else { while (u > 0) { tmp[len++] = (char)('0' + (u % 10)); u /= 10; } }
    int total = len + (neg ? 1 : 0);
    char* buf = (char*)malloc((size_t)total + 1);
    if (!buf) return 0;
    int pos = 0;
    if (neg) buf[pos++] = '-';
    for (int i = len - 1; i >= 0; i--) buf[pos++] = tmp[i];
    buf[total] = '\0';
    int64_t r = (int64_t)kk_str_alloc_leaf(buf, total, 1);
    kk_register_string(r);
    return r;
}

void kk_str_retain(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s != NULL) s->rc++;
}

void kk_str_drop(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return;
    if (s->rc <= 0) return;  /* already dead — don't double-free */
    s->rc--;
    if (s->rc > 0) return;   /* still shared */
    /* Sole owner — free string and its children. */
    kk_unregister_string(s_i);
    if (s->kind == KK_STR_CONCAT) {
        kk_str_drop((int64_t)s->u.cat.l);
        kk_str_drop((int64_t)s->u.cat.r);
    } else if (s->kind == KK_STR_SLICE) {
        /* Drop reference to parent string */
        kk_str_drop((int64_t)s->u.cat.l);
    } else if (s->kind == KK_STR_LEAF && s->owns_bytes) {
        free((void*)s->u.bytes);
    }
    s->magic = 0;  /* prevent dangling pointer from looking like a live string */
    free(s);
}

/* Public wrappers for shim use */
int64_t kk_str_alloc_leaf_owned(const char* bytes, int64_t byte_len) {
    int64_t r = (int64_t)kk_str_alloc_leaf(bytes, byte_len, 1);
    kk_register_string(r);
    return r;
}

int64_t kk_str_byte_len(int64_t s) {
    return kk_str_len(s);
}

/* ByteString — same kk_string_t representation, byte-oriented API. */

int64_t kk_bytes_from_literal(int64_t bytes_ptr, int64_t byte_len) {
    return kk_string_from_literal(bytes_ptr, byte_len);
}

int64_t kk_bytes_len(int64_t b) {
    return kk_str_len(b);
}

int64_t kk_bytes_concat(int64_t a, int64_t b) {
    return kk_str_concat(a, b);
}

int64_t kk_bytes_eq(int64_t a, int64_t b) {
    return kk_str_eq(a, b);
}

/* Random byte access — walks the rope to find the leaf containing index i. */
static int64_t kk_bytes_index_rec(kk_string_t* s, int64_t i) {
    if (s == NULL) return -1;
    if (s->kind == KK_STR_LEAF || s->kind == KK_STR_SLICE) {
        if (i < 0 || i >= s->byte_len) return -1;
        return (int64_t)(unsigned char)kk_str_bytes(s)[i];
    }
    int64_t left_len = (s->u.cat.l ? s->u.cat.l->byte_len : 0);
    if (i < left_len) return kk_bytes_index_rec(s->u.cat.l, i);
    return kk_bytes_index_rec(s->u.cat.r, i - left_len);
}

int64_t kk_bytes_index(int64_t b_i, int64_t i) {
    return kk_bytes_index_rec((kk_string_t*)b_i, i);
}

/* File I/O, process, environment.
 *
 * Frankenstein strings flow through these intrinsics as int64_t pointers
 * to kk_string_t headers. Paths and contents are converted to NUL-
 * terminated C strings via a small helper that flattens the rope into
 * a freshly malloc'd buffer the caller must free. */

char* kk_str_dup_cstr(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    int64_t n = (s ? s->byte_len : 0);
    char* buf = (char*)malloc((size_t)n + 1);
    if (!buf) return NULL;
    char* p = buf;
    if (s) kk_str_copy_into(s, &p);
    buf[n] = '\0';
    return buf;
}

int64_t kk_read_file(int64_t path_str) {
    char* path = kk_str_dup_cstr(path_str);
    if (!path) return kk_string_empty();
    FILE* f = fopen(path, "rb");
    free(path);
    if (!f) return kk_string_empty();
    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); return kk_string_empty(); }
    long size = ftell(f);
    if (size < 0) { fclose(f); return kk_string_empty(); }
    rewind(f);
    char* buf = (char*)malloc((size_t)size + 1);
    if (!buf) { fclose(f); return kk_string_empty(); }
    size_t got = fread(buf, 1, (size_t)size, f);
    fclose(f);
    buf[got] = '\0';
    { int64_t r = (int64_t)kk_str_alloc_leaf(buf, (int64_t)got, 1);
      kk_register_string(r); return r; }
}

int64_t kk_write_file(int64_t path_str, int64_t content_str) {
    char* path = kk_str_dup_cstr(path_str);
    if (!path) return -1;
    FILE* f = fopen(path, "wb");
    free(path);
    if (!f) return -1;
    kk_string_t* s = (kk_string_t*)content_str;
    int64_t n = (s ? s->byte_len : 0);
    if (n > 0) {
        char* buf = (char*)malloc((size_t)n);
        if (!buf) { fclose(f); return -1; }
        char* p = buf;
        kk_str_copy_into(s, &p);
        size_t wrote = fwrite(buf, 1, (size_t)n, f);
        free(buf);
        if (wrote != (size_t)n) { fclose(f); return -1; }
    }
    fclose(f);
    return 0;
}

int64_t kk_file_exists(int64_t path_str) {
    char* path = kk_str_dup_cstr(path_str);
    if (!path) return 0;
    FILE* f = fopen(path, "rb");
    free(path);
    if (!f) return 0;
    fclose(f);
    return 1;
}

int64_t kk_read_line(void) {
    /* Read up to a newline or EOF from stdin. Strip the trailing '\n'. */
    size_t cap = 128, len = 0;
    char* buf = (char*)malloc(cap);
    if (!buf) return kk_string_empty();
    int c;
    while ((c = getchar()) != EOF && c != '\n') {
        if (len + 1 >= cap) {
            cap *= 2;
            char* nb = (char*)realloc(buf, cap);
            if (!nb) { free(buf); return kk_string_empty(); }
            buf = nb;
        }
        buf[len++] = (char)c;
    }
    buf[len] = '\0';
    { int64_t r = (int64_t)kk_str_alloc_leaf(buf, (int64_t)len, 1);
      kk_register_string(r); return r; }
}

int64_t kk_system(int64_t cmd_str) {
    char* cmd = kk_str_dup_cstr(cmd_str);
    if (!cmd) return -1;
    int rc = system(cmd);
    free(cmd);
    return (int64_t)rc;
}

int64_t kk_getenv(int64_t name_str) {
    char* name = kk_str_dup_cstr(name_str);
    if (!name) return kk_string_empty();
    const char* val = getenv(name);
    free(name);
    if (!val) return kk_string_empty();
    /* Borrow the libc-owned string — getenv result is process-lifetime. */
    int64_t n = 0;
    while (val[n] != '\0') n++;
    { int64_t r = (int64_t)kk_str_alloc_leaf(val, n, 0);
      kk_register_string(r); return r; }
}

/* ---- Command-line arguments ----
 *
 * The main() wrapper stashes argc/argv into these globals at program
 * start. Programs read them through the intrinsics args_count / args_get,
 * which excludes argv[0] (the program name) — matching Haskell's
 * System.Environment.getArgs. The program name is available separately
 * through args_progname.
 */
static int   kk_g_argc = 0;
static char** kk_g_argv = NULL;

void kk_args_init(int argc, char** argv) {
    kk_g_argc = argc;
    kk_g_argv = argv;
    /* Match Idris2's chez backend: line-buffered stdout regardless of
     * whether stdout is a tty.  Otherwise putStr output sits in glibc's
     * 4KiB block buffer until program exit, so progressive test-runner
     * output (PASS / FAIL lines) only appears once everything completes
     * — defeating the purpose of incremental reporting. */
    setvbuf(stdout, NULL, _IOLBF, 0);
}

int64_t kk_args_count(void) {
    return (int64_t)(kk_g_argc > 0 ? kk_g_argc - 1 : 0);
}

int64_t kk_args_get(int64_t i) {
    /* Map i in [0..argc-2] to argv[i+1]. Out-of-range returns empty. */
    if (i < 0 || i >= (int64_t)(kk_g_argc - 1)) return kk_string_empty();
    const char* s = kk_g_argv[i + 1];
    int64_t n = 0;
    while (s[n] != '\0') n++;
    /* Borrow argv memory — it's process-lifetime. */
    { int64_t r = (int64_t)kk_str_alloc_leaf(s, n, 0);
      kk_register_string(r); return r; }
}

int64_t kk_args_progname(void) {
    if (kk_g_argc <= 0 || kk_g_argv == NULL) return kk_string_empty();
    const char* s = kk_g_argv[0];
    int64_t n = 0;
    while (s[n] != '\0') n++;
    { int64_t r = (int64_t)kk_str_alloc_leaf(s, n, 0);
      kk_register_string(r); return r; }
}

/* Exit the process with the given status code. Wrapped so callers can
 * use it as an i64 intrinsic from MLIR without pulling in an llvm.func
 * declaration for libc exit(). */
void kk_exit(int64_t code) {
    exit((int)code);
}

/* IORef — single-field mutable cell allocated as a kk_alloc_con box.
 * Tag is "REF0"; payload index 0 holds the current value. */
#define KK_REF_TAG 0x52454630

int64_t kk_ref_new(int64_t initial) {
    int64_t cell = kk_alloc_con(KK_REF_TAG, 1);
    if (cell == 0) return 0;
    kk_set_field(cell, 0, initial);
    return cell;
}

int64_t kk_ref_get(int64_t ref) {
    return kk_field(ref, 0);
}

int64_t kk_ref_set(int64_t ref, int64_t value) {
    kk_set_field(ref, 0, value);
    return 0;
}

/* Idris2 %extern IORef primitives.  Signatures match the NmExtPrim
 * call sites the shim generates: (erased type-arg, value/ref/..., world).
 * The type arg is a stand-in literal we ignore; the world arg is the
 * IO state token, also opaque.  Refcount management mirrors prim__putStr —
 * the caller owns its references, we don't retain/drop anything here. */
int64_t idris2_newIORef(int64_t _ty, int64_t initial, int64_t _world) {
    (void)_ty; (void)_world;
    return kk_ref_new(initial);
}
int64_t idris2_readIORef(int64_t _ty, int64_t ref, int64_t _world) {
    (void)_ty; (void)_world;
    int64_t v = kk_ref_get(ref);
    kk_retain(v);  /* caller will drop this ref */
    return v;
}
int64_t idris2_writeIORef(int64_t _ty, int64_t ref, int64_t value, int64_t _world) {
    (void)_ty; (void)_world;
    /* Drop the previous value so writes don't leak. */
    int64_t old = kk_ref_get(ref);
    kk_drop(old);
    kk_retain(value);  /* the cell now owns a reference */
    kk_set_field(ref, 0, value);
    return 0;
}

/* Write field[idx] of a boxed value */
void kk_set_field(int64_t ptr, int64_t idx, int64_t value) {
    if (!kk_is_heap_ptr(ptr)) return;
    int64_t* fields = (int64_t*)(ptr + 8);
    fields[idx] = value;
}

/* Evidence vector operations for algebraic effects
 *
 * An evidence vector holds function pointers (as i64) to handler
 * operations for a particular effect.  It is allocated as a regular
 * boxed constructor with tag 0xEVV0.
 */

#include <stdio.h>

/* KK_EVV_TAG defined at top of file */

/* Create an evidence vector with nops operation slots */
int64_t kk_evv_create(int64_t nops) {
    return kk_alloc_con(KK_EVV_TAG, nops);
}

/* Set operation at index idx in the evidence vector */
void kk_evv_set(int64_t evv, int64_t idx, int64_t handler_fn) {
    kk_set_field(evv, idx, handler_fn);
}

/* Get the operation function pointer at index idx */
int64_t kk_evv_get(int64_t evv, int64_t idx) {
    return kk_field(evv, idx);
}

/* Default handler for unhandled effect operations -- abort with message */
int64_t kk_unhandled_effect(void) {
    fprintf(stderr, "frankenstein: unhandled effect operation\n");
    exit(1);
    return 0;
}

/* ======================================================================
 * Plotkin-style evidence-vector dispatch (KK_EVV2_TAG)
 *
 * An evv is a stack of (effect_id, op_table) pairs stored in adjacent
 * field slots. Field layout: [eff_id_0, op_table_0, eff_id_1, op_table_1, ...].
 * A NULL (0) parent represents the empty evv.
 *
 * Effect ids are produced by the compiler as deterministic hashes of the
 * effect name (or by a runtime intern table — TBD). The runtime simply
 * compares them numerically.
 *
 * An op_table is an OPTB-tagged kk_alloc_con whose fields are the closures
 * for that effect's operations, ordered by the index assigned in the
 * effect declaration.
 * ====================================================================== */

/* Build a child evv = parent ++ [(eff_id, op_table)] (top-of-stack last). */
int64_t kk_evv_extend(int64_t parent, int64_t eff_id, int64_t op_table) {
    int64_t parent_slots = (parent == 0) ? 0 : (kk_nfields(parent) / 2);
    int64_t child = kk_alloc_con(KK_EVV2_TAG, (parent_slots + 1) * 2);
    for (int64_t i = 0; i < parent_slots; i++) {
        kk_set_field(child, 2*i,   kk_field(parent, 2*i));
        kk_set_field(child, 2*i+1, kk_field(parent, 2*i+1));
    }
    kk_set_field(child, 2*parent_slots,   eff_id);
    kk_set_field(child, 2*parent_slots+1, op_table);
    return child;
}

/* Look up the op_table for the most recently installed handler of eff_id.
 * Returns 0 if no handler is in scope. */
int64_t kk_evv_lookup(int64_t evv, int64_t eff_id) {
    if (evv == 0) return 0;
    int64_t slots = kk_nfields(evv) / 2;
    for (int64_t i = slots - 1; i >= 0; i--) {
        if (kk_field(evv, 2*i) == eff_id) {
            return kk_field(evv, 2*i+1);
        }
    }
    return 0;
}

/* Allocate an op_table with `nops` empty slots. */
int64_t kk_optab_create(int64_t nops) {
    return kk_alloc_con(KK_OPTAB_TAG, nops);
}

/* ======================================================================
 * PAP (partially-applied function) — see KK_PAP_TAG definition for
 * layout. Trampolines per dispatch arity; allocators per original arity.
 *
 * The N in kk_pap_call_N is the number of args the HOF or external
 * caller passes (i.e. the function's *original* arity before plotkin
 * prepended evv). The trampoline forwards (evv, args...) to the
 * wrapped fn pointer, which has signature (evv, args...).
 *
 * Why both trampoline and wrapped_fn are needed: the existing closure
 * dispatcher calls field-0 as `field_0(self, args...)` with `self`
 * being the closure. That signature doesn't match the wrapped fn,
 * which expects `(evv, args...)` without self. The trampoline absorbs
 * the self argument, looks up the actual wrapped_fn and pre-supplied
 * evv, and issues the final call with the right shape.
 * ====================================================================== */

/* Trampoline for an original 1-arg fn (now 2-arg with evv prepended).
 * The closure dispatcher calls us as (self_pap, a). We extract the
 * wrapped fn and evv from self_pap and call wrapped(evv, a). */
int64_t kk_pap_call_1(int64_t self, int64_t a) {
    int64_t fn_word = kk_field(self, 1);
    int64_t evv     = kk_field(self, 2);
    int64_t (*fn)(int64_t, int64_t) =
        (int64_t (*)(int64_t, int64_t))(intptr_t)fn_word;
    return fn(evv, a);
}

/* Trampoline for an original 2-arg fn (now 3-arg with evv prepended). */
int64_t kk_pap_call_2(int64_t self, int64_t a, int64_t b) {
    int64_t fn_word = kk_field(self, 1);
    int64_t evv     = kk_field(self, 2);
    int64_t (*fn)(int64_t, int64_t, int64_t) =
        (int64_t (*)(int64_t, int64_t, int64_t))(intptr_t)fn_word;
    return fn(evv, a, b);
}

/* Trampoline for an original 3-arg fn (now 4-arg with evv prepended). */
int64_t kk_pap_call_3(int64_t self, int64_t a, int64_t b, int64_t c) {
    int64_t fn_word = kk_field(self, 1);
    int64_t evv     = kk_field(self, 2);
    int64_t (*fn)(int64_t, int64_t, int64_t, int64_t) =
        (int64_t (*)(int64_t, int64_t, int64_t, int64_t))(intptr_t)fn_word;
    return fn(evv, a, b, c);
}

/* Allocate a PAP wrapping fn_ptr with evv pre-supplied, dispatchable
 * via the existing closure ABI. The trampoline argument is the address
 * of one of kk_pap_call_N picked by the emitter based on the wrapped
 * fn's original arity. */
int64_t kk_pap_alloc(int64_t trampoline, int64_t fn_ptr, int64_t evv) {
    int64_t pap = kk_alloc_con(KK_PAP_TAG, 3);
    kk_set_field(pap, 0, trampoline);
    kk_set_field(pap, 1, fn_ptr);
    kk_set_field(pap, 2, evv);
    return pap;
}

/* Store a closure at op_idx in the op_table. Returns the table for chaining. */
int64_t kk_optab_set(int64_t tab, int64_t op_idx, int64_t closure) {
    kk_set_field(tab, op_idx, closure);
    return tab;
}

/* Fetch the closure for op_idx from the op_table. */
int64_t kk_optab_get(int64_t tab, int64_t op_idx) {
    return kk_field(tab, op_idx);
}

/* ======================================================================
 * Abort effect support (setjmp/longjmp)
 *
 * For effects where the handler doesn't call resume (abort effects, like
 * exceptions), we use setjmp/longjmp to discard the body's continuation.
 *
 * Usage pattern (generated by the evidence pass):
 *   tag = unique_id
 *   body_closure = lambda() { ...; kk_handler_abort(tag, val); ... }
 *   result = kk_handler_exec(tag, body_closure)
 *
 * kk_handler_exec calls setjmp in its own stack frame (which stays alive
 * while the body runs), then invokes the body closure. If the body calls
 * kk_handler_abort, longjmp returns to kk_handler_exec.
 *
 * Note: longjmp bypasses Perceus drops in the abandoned continuation.
 * Arena checkpoint/rollback reclaims constructor cells in bulk, and
 * string checkpoint/rollback frees malloc'd strings allocated during
 * the body. Remaining leaks (e.g. deeply nested transitive references)
 * are acceptable in bootstrapping mode (kk_drop is already a no-op).
 * ====================================================================== */

#include <setjmp.h>

#define KK_MAX_HANDLER_FRAMES 64

typedef struct {
    jmp_buf env;
    int64_t tag;
    int64_t result;
    kk_arena_checkpoint_t arena_cp;  /* arena state at handler entry */
    int64_t string_cp;               /* string log index at handler entry */
} kk_handler_frame_t;

static kk_handler_frame_t kk_handler_stack[KK_MAX_HANDLER_FRAMES];
static int64_t kk_handler_sp = 0;

/* Deep-rescue a boxed value from the arena rollback region.
 *
 * If `val` is a heap pointer inside the region that kk_arena_rollback(cp)
 * is about to free, memcpy the entire cell (refcount + tag + fields) into
 * a fresh malloc allocation and return the new pointer-to-tag.  Then
 * recursively rescue any fields that also point into the rollback region.
 *
 * Strings (malloc'd, not arena-allocated) are never in the rollback region
 * and are skipped.  Arena-allocated cells can't form cycles (fields are set
 * once, always pointing at older allocations), so recursion terminates.
 *
 * depth_limit prevents runaway recursion in pathological cases. */
#define KK_RESCUE_DEPTH_LIMIT 256

static int64_t kk_rescue_from_arena_depth(int64_t val, kk_arena_checkpoint_t cp, int depth);

static int64_t kk_rescue_from_arena(int64_t val, kk_arena_checkpoint_t cp) {
    return kk_rescue_from_arena_depth(val, cp, 0);
}

static int64_t kk_rescue_from_arena_depth(int64_t val, kk_arena_checkpoint_t cp, int depth) {
    if (!kk_is_heap_ptr(val)) return val;
    if (kk_is_string(val)) return val;  /* strings are malloc'd, not arena */
    if (depth >= KK_RESCUE_DEPTH_LIMIT) return val;  /* safety limit */
    /* The Frankenstein pointer points at the tag; refcount is at (ptr - 8). */
    void* block = (void*)(val - 8);
    if (!kk_arena_in_rollback_region(block, cp)) return val;
    /* Compute cell size: 2 header words + nfields payload words. */
    int64_t nf = kk_nfields(val);
    size_t cell_bytes = (size_t)(2 + nf) * 8;
    int64_t* copy = (int64_t*)malloc(cell_bytes);
    if (!copy) return val;  /* OOM fallback: leak rather than crash */
    memcpy(copy, block, cell_bytes);
    int64_t new_ptr = (int64_t)&copy[1];  /* pointer to tag */
    kk_register_nfields(new_ptr, nf);
    /* Recursively rescue fields that point into the rollback region. */
    int64_t* fields = (int64_t*)(new_ptr + 8);
    for (int64_t i = 0; i < nf; i++) {
        fields[i] = kk_rescue_from_arena_depth(fields[i], cp, depth + 1);
    }
    return new_ptr;
}

/* Execute a body under a handler frame (callback-based setjmp pattern).
 *
 * Critical: setjmp MUST be called in a function that stays on the call
 * stack while the body runs. kk_handler_exec provides this: it calls
 * setjmp, then the body closure. If the body (or anything it calls)
 * invokes kk_handler_abort with a matching tag, longjmp returns here
 * and we return the abort value instead.
 *
 * Arena integration: on entry, an arena checkpoint is saved. On abort,
 * the arena is rolled back to the checkpoint, freeing all constructor
 * cells allocated during the body. If the abort result is itself a heap
 * value in the rollback region, it is deep-rescued (recursively copied
 * to malloc, including transitive field references) before rollback.
 *
 * body_closure: a Frankenstein closure (boxed heap value).
 *   field 0 = function pointer (int64_t (*)(int64_t closure))
 *   fields 1..n = captured variables (if any)
 * The function is invoked as fptr(closure) following the standard
 * Frankenstein closure ABI.
 *
 * Returns: body result (normal) or abort value (if body performed abort)
 */
int64_t kk_handler_exec(int64_t tag, int64_t body_closure) {
    typedef int64_t (*body_fn_t)(int64_t);
    if (kk_handler_sp >= KK_MAX_HANDLER_FRAMES) {
        fprintf(stderr, "frankenstein: handler stack overflow\n");
        exit(1);
    }
    kk_handler_frame_t *frame = &kk_handler_stack[kk_handler_sp++];
    frame->tag = tag;
    frame->result = 0;
    frame->arena_cp = kk_arena_checkpoint();
    frame->string_cp = kk_string_checkpoint();
    /* Extract function pointer from closure field 0 */
    int64_t fptr = kk_field(body_closure, 0);
    if (setjmp(frame->env) == 0) {
        /* Normal path: invoke body closure */
        int64_t result = ((body_fn_t)fptr)(body_closure);
        kk_handler_sp--;  /* pop handler frame */
        return result;
    } else {
        /* Abort path: longjmp landed here.
         * Rescue the result value from the arena before rollback. */
        int64_t result = kk_rescue_from_arena(frame->result, frame->arena_cp);
        /* Roll back arena (constructor cells) and strings (malloc'd). */
        kk_string_rollback(frame->string_cp, result);
        kk_arena_rollback(frame->arena_cp);
        return result;
    }
}

/* Abort: find the matching handler frame and longjmp to it.
 * Declared as returning i64 for MLIR compatibility (never actually returns). */
int64_t kk_handler_abort(int64_t tag, int64_t value) {
    int64_t i = kk_handler_sp;
    while (i > 0) {
        i--;
        if (kk_handler_stack[i].tag == tag) {
            kk_handler_stack[i].result = value;
            kk_handler_sp = i;  /* pop all frames above */
            longjmp(kk_handler_stack[i].env, 1);
        }
    }
    fprintf(stderr, "frankenstein: no handler for abort effect (tag=%ld)\n", tag);
    exit(1);
    return 0;  /* unreachable */
}

/* Mercury choice effect support — multi-shot effect via iterative path enumeration
 *
 * mercury_choose() returns 0 or 1 based on a global path variable.
 * mercury_collect_choices(fn_ptr) runs the body for all binary choice paths,
 * summing the results.  Each call to mercury_choose() reads one bit from
 * the path, so N choices yield up to 2^N paths.
 *
 * Short paths (where the body makes fewer choices than the max depth)
 * are deduplicated: a result is only counted when the unused high bits
 * of the path are all zero.
 */

#include <string.h>

static int64_t mercury_choice_decisions[64];
static int64_t mercury_choice_pos = 0;

int64_t mercury_choose(void) {
    return mercury_choice_decisions[mercury_choice_pos++];
}

/* Mercury failure helpers — referenced by the bridge when semidet
 * predicates fail or exn handlers default.  The synthetic def the
 * bridge emits is module-qualified (e.g. demo_X_mercury_fail), but
 * unqualified call sites can land on this runtime stub instead. */
int64_t mercury_fail(void) { return 0; }
int64_t mercury_exn_fail(void) { return 0; }

/* Mercury stdlib stubs for the surd-mercury integer module.  Surd
 * vendors Mercury's stdlib 'integer.m' (arbitrary precision), but
 * Frankenstein substitutes a plain-i64 model — sufficient for the
 * smoke tests, lossy for large values.  Names match what the bridge
 * emits after sanitization of 'integer.<op>'. */
int64_t integer_zero(void)               { return 0; }
int64_t integer_one(void)                { return 1; }
int64_t integer_is_zero(int64_t x)       { return x == 0 ? 1 : 0; }
int64_t integer_zl(int64_t a, int64_t b) { return a <  b ? 1 : 0; } /* < */
int64_t integer_zg(int64_t a, int64_t b) { return a >  b ? 1 : 0; } /* > */
int64_t integer_zezl(int64_t a, int64_t b){return a <= b ? 1 : 0; } /* =< */
int64_t integer_zgze(int64_t a, int64_t b){return a >= b ? 1 : 0; } /* >= */
int64_t integer_zp(int64_t a, int64_t b) { return a + b; }          /* + */
int64_t integer_zm(int64_t a, int64_t b) { return a - b; }          /* - */
int64_t integer_zt(int64_t a, int64_t b) { return a * b; }          /* * */
int64_t integer_zs(int64_t a, int64_t b) { return b == 0 ? 0 : a / b; } /* / */
int64_t integer_abs(int64_t x)           { return x < 0 ? -x : x; }
int64_t integer_signum(int64_t x)        { return x < 0 ? -1 : (x > 0 ? 1 : 0); }
int64_t integer_float(int64_t x)         { double d = (double)x;
                                            int64_t b; memcpy(&b, &d, 8); return b; }

/* integer.to_string: format an int as decimal in an owned heap string. */
int64_t integer_to_string(int64_t x) {
    char tmp[32];
    int n = snprintf(tmp, sizeof tmp, "%lld", (long long)x);
    if (n < 0) n = 0;
    char* heap = (char*)malloc((size_t)n + 1);
    if (!heap) return kk_string_empty();
    memcpy(heap, tmp, (size_t)n);
    heap[n] = '\0';
    int64_t r = (int64_t)kk_str_alloc_leaf(heap, (int64_t)n, 1);
    kk_register_string(r);
    return r;
}

/* Mercury builtin.unify/2 — structural equality.  Forwards to
 * kk_structural_eq which already handles the i64/string/arena cases. */
int64_t unify(int64_t a, int64_t b) { return kk_structural_eq(a, b); }

/* Mercury integer.// — sanitised name "integer___" (one underscore per
 * each of '.', '/', '/' in the source name).  Plain integer division;
 * returns 0 on zero divisor rather than trapping. */
int64_t integer___(int64_t a, int64_t b) { return b == 0 ? 0 : a / b; }

/* Mercury integer.rem — integer remainder. */
int64_t integer_rem(int64_t a, int64_t b) { return b == 0 ? 0 : a % b; }

/* Mercury integer.mod — for our i64 model identical to rem. */
int64_t integer_mod(int64_t a, int64_t b) { return b == 0 ? 0 : a % b; }

/* Mercury unary integer negation.  The bridge emits the source form
 * `integer.(- X)` as a 1-arg call to "integer.-", but the runtime's
 * `integer_zm` is binary subtraction.  Route to a dedicated unary stub
 * (named `integer_neg`) so the call is saturated. */
int64_t integer_neg(int64_t a) { return -a; }

/* Mercury `int.<op>` aliases — identical to `integer.<op>` in the i64
 * model.  Mercury HLDS sometimes prints `int.(X - Y)` etc. for plain
 * int arithmetic, distinct from the `integer.*` arbitrary-precision
 * paths the bridge already routes via the `integer_*` stubs above. */
int64_t int_zp(int64_t a, int64_t b) { return a + b; }
int64_t int_zm(int64_t a, int64_t b) { return a - b; }
int64_t int_zt(int64_t a, int64_t b) { return a * b; }
int64_t int_zs(int64_t a, int64_t b) { return b == 0 ? 0 : a / b; }
/* Mercury int.// (integer division, sanitises to "int___"). */
int64_t int___(int64_t a, int64_t b) { return b == 0 ? 0 : a / b; }
/* Mercury int./ (single-slash division, sanitises to "int__"). */
int64_t int__(int64_t a, int64_t b) { return b == 0 ? 0 : a / b; }
/* Mercury int.div: integer division rounded TOWARDS NEGATIVE INFINITY (floor).
 * Differs from C's @/@ for mixed-sign operands: C trunc-divides toward zero,
 * Mercury floor-divides.  e.g. int.div(-1, 2) = -1 (Mercury floor)
 * vs C @-1 / 2 = 0@ (C trunc).
 *
 * Why the runtime distinguishes: surd's @reduce_atom@ does
 * @Full = int.div(E, N), Rem = int.mod(E, N)@ on signed E (notably E=-1
 * during @invert_monomial@'s exponent-negation pass).  Trunc-division
 * yielded @Full=0, Rem=-1@ → @C@ accumulator unchanged → @reduce_monomial@'s
 * result became @1 · (√R)^(-1)@ instead of @(1/R) · √R@, leaving a
 * negative-exponent atom that downstream @from_norm_expr@ rendered as
 * @re_pow(re_root(2, re_lit(R)), -1)@ — which simplify_via_canonical
 * subsequently collapsed to zero, producing surd-elliptic's "0 · F(...)"
 * leading-coefficient bug. */
int64_t int_div(int64_t a, int64_t b) {
    if (b == 0) return 0;
    int64_t q = a / b;
    int64_t r = a % b;
    /* Correction for floor when signs differ and there's a non-zero remainder */
    if ((r != 0) && ((r < 0) != (b < 0))) q -= 1;
    return q;
}
/* int.pow(Base, Exp) — non-negative integer exponentiation. */
int64_t int_pow(int64_t base, int64_t exp) {
    int64_t r = 1;
    while (exp > 0) {
        if (exp & 1) r *= base;
        base *= base;
        exp >>= 1;
    }
    return r;
}
int64_t int_zl(int64_t a, int64_t b) { return a <  b ? 1 : 0; }
int64_t int_zg(int64_t a, int64_t b) { return a >  b ? 1 : 0; }
int64_t int_zezl(int64_t a, int64_t b){ return a <= b ? 1 : 0; }
int64_t int_zgze(int64_t a, int64_t b){ return a >= b ? 1 : 0; }
int64_t int_zezeze(int64_t a, int64_t b){ return a == b ? 1 : 0; }
int64_t int_rem(int64_t a, int64_t b) { return b == 0 ? 0 : a % b; }
/* Mercury int.mod: remainder matching floor-division (@int.div@), so the
 * sign of the result follows @b@ (non-negative when @b > 0@).  Distinct
 * from @int.rem@ which follows C's @%@ (sign follows @a@).
 *
 * Why: pairs with @int_div@'s floor semantics so the identity
 * @a = int.div(a,b) * b + int.mod(a,b)@ holds with @0 <= int.mod(a,b) < b@
 * for @b > 0@.  See @int_div@'s comment for the surd-elliptic bug. */
int64_t int_mod(int64_t a, int64_t b) {
    if (b == 0) return 0;
    int64_t r = a % b;
    if ((r != 0) && ((r < 0) != (b < 0))) r += b;
    return r;
}
int64_t int_max(int64_t a, int64_t b) { return a > b ? a : b; }
int64_t int_min(int64_t a, int64_t b) { return a < b ? a : b; }
int64_t int_abs(int64_t x) { return x < 0 ? -x : x; }
int64_t int_neg(int64_t x) { return -x; }

/* Mercury `float.<op>` stubs.  Floats are stored as the raw IEEE-754
 * bit pattern in an i64 (same convention as integer_float above).
 * The bridge emits these names; without runtime entries they show up
 * as link-time unresolved symbols in float-heavy Mercury programs
 * (trig_table, elliptic_integral). */
static inline double kkf_unbox(int64_t b) { double d; memcpy(&d, &b, 8); return d; }
static inline int64_t kkf_box(double d) { int64_t b; memcpy(&b, &d, 8); return b; }

int64_t float_zp(int64_t a, int64_t b) { return kkf_box(kkf_unbox(a) + kkf_unbox(b)); }
int64_t float_zm(int64_t a, int64_t b) { return kkf_box(kkf_unbox(a) - kkf_unbox(b)); }
int64_t float_zt(int64_t a, int64_t b) { return kkf_box(kkf_unbox(a) * kkf_unbox(b)); }
int64_t float_zs(int64_t a, int64_t b) {
    double y = kkf_unbox(b);
    return kkf_box(y == 0.0 ? 0.0 : kkf_unbox(a) / y);
}
/* Mercury float./ (single-slash division, sanitises to "float__"). */
int64_t float__(int64_t a, int64_t b) {
    double y = kkf_unbox(b);
    return kkf_box(y == 0.0 ? 0.0 : kkf_unbox(a) / y);
}
int64_t float_zl(int64_t a, int64_t b)   { return kkf_unbox(a) <  kkf_unbox(b) ? 1 : 0; }
int64_t float_zg(int64_t a, int64_t b)   { return kkf_unbox(a) >  kkf_unbox(b) ? 1 : 0; }
int64_t float_zezl(int64_t a, int64_t b) { return kkf_unbox(a) <= kkf_unbox(b) ? 1 : 0; }
int64_t float_zgze(int64_t a, int64_t b) { return kkf_unbox(a) >= kkf_unbox(b) ? 1 : 0; }
int64_t float_zezeze(int64_t a, int64_t b){ return kkf_unbox(a) == kkf_unbox(b) ? 1 : 0; }
int64_t float_abs(int64_t a) {
    double d = kkf_unbox(a);
    return kkf_box(d < 0 ? -d : d);
}
int64_t float_neg(int64_t a) { return kkf_box(-kkf_unbox(a)); }
int64_t float_max(int64_t a, int64_t b) {
    double x = kkf_unbox(a), y = kkf_unbox(b);
    return kkf_box(x > y ? x : y);
}
int64_t float_min(int64_t a, int64_t b) {
    double x = kkf_unbox(a), y = kkf_unbox(b);
    return kkf_box(x < y ? x : y);
}
/* `float.float(I)` converts an integer to a float (same as integer_float). */
int64_t float_float(int64_t x) {
    double d = (double)x;
    return kkf_box(d);
}
int64_t float_round(int64_t a) {
    double d = kkf_unbox(a);
    /* Mercury's float.round/1 returns an int; round half-away-from-zero. */
    return (int64_t)(d < 0 ? d - 0.5 : d + 0.5);
}
int64_t float_truncate(int64_t a) {
    double d = kkf_unbox(a);
    return (int64_t)d;
}
int64_t float_floor(int64_t a) {
    double d = kkf_unbox(a);
    int64_t r = (int64_t)d;
    return (d < 0 && d != (double)r) ? r - 1 : r;
}
int64_t float_ceiling(int64_t a) {
    double d = kkf_unbox(a);
    int64_t r = (int64_t)d;
    return (d > 0 && d != (double)r) ? r + 1 : r;
}
int64_t float_is_nan(int64_t a) {
    double d = kkf_unbox(a);
    return (d != d) ? 1 : 0;
}
int64_t float_is_inf(int64_t a) {
    double d = kkf_unbox(a);
    return (d != 0.0 && d * 2.0 == d) ? 1 : 0;
}

/* Mercury `math.<op>` stubs.  Floats use the same bit-pattern-in-i64
 * encoding as the float_* operators above. */
int64_t math_pi(void)             { return kkf_box(3.14159265358979323846); }
int64_t math_e(void)              { return kkf_box(2.71828182845904523536); }
int64_t math_sqrt(int64_t a)      { return kkf_box(sqrt(kkf_unbox(a))); }
int64_t math_sin(int64_t a)       { return kkf_box(sin(kkf_unbox(a))); }
int64_t math_cos(int64_t a)       { return kkf_box(cos(kkf_unbox(a))); }
int64_t math_tan(int64_t a)       { return kkf_box(tan(kkf_unbox(a))); }
int64_t math_asin(int64_t a)      { return kkf_box(asin(kkf_unbox(a))); }
int64_t math_acos(int64_t a)      { return kkf_box(acos(kkf_unbox(a))); }
int64_t math_atan(int64_t a)      { return kkf_box(atan(kkf_unbox(a))); }
int64_t math_exp(int64_t a)       { return kkf_box(exp(kkf_unbox(a))); }
int64_t math_ln(int64_t a)        { return kkf_box(log(kkf_unbox(a))); }
int64_t math_log10(int64_t a)     { return kkf_box(log10(kkf_unbox(a))); }
int64_t math_log2(int64_t a)      { return kkf_box(log2(kkf_unbox(a))); }
int64_t math_pow(int64_t a, int64_t b) { return kkf_box(pow(kkf_unbox(a), kkf_unbox(b))); }
int64_t math_atan2(int64_t a, int64_t b) { return kkf_box(atan2(kkf_unbox(a), kkf_unbox(b))); }
int64_t math_sinh(int64_t a)      { return kkf_box(sinh(kkf_unbox(a))); }
int64_t math_cosh(int64_t a)      { return kkf_box(cosh(kkf_unbox(a))); }
int64_t math_tanh(int64_t a)      { return kkf_box(tanh(kkf_unbox(a))); }

/* require.unexpected/3: Mercury aborts on unexpected with a formatted
 * message.  We fprintf to stderr and exit; the args are heap-allocated
 * Mercury strings (boxed pointers to char* via kk_string_from_literal).
 * The string layout puts a char* at field 1 — but for stubs, we just
 * trap and exit(1) for now. */
int64_t require_unexpected(int64_t mod, int64_t pred, int64_t msg) {
    (void)mod; (void)pred; (void)msg;
    fprintf(stderr, "*** Mercury: unexpected condition (require.unexpected)\n");
    exit(1);
    return 0;
}

/* private_builtin.* type-info helpers: in our erased-type model these
 * are identity-ish — the bridge passes type-info-shaped values but
 * doesn't use their contents.  Return the input so downstream code
 * that just threads the value keeps working. */
int64_t private_builtin_type_info_from_typeclass_info(int64_t tci, int64_t idx) {
    (void)idx; return tci;
}
int64_t private_builtin_superclass_from_typeclass_info(int64_t tci, int64_t idx) {
    (void)idx; return tci;
}
int64_t private_builtin_instance_constructor_from_typeclass_info(int64_t tci, int64_t idx) {
    (void)idx; return tci;
}

/* Mercury `string.<op>` stubs.  These are aliases for the existing
 * kk_str_* runtime helpers, exposed under the names the bridge emits
 * (string_<op>) so unresolved-symbol failures go away. */
int64_t string_length(int64_t s)                  { return kk_str_char_len(s); }
int64_t string_append(int64_t a, int64_t b)       { return kk_str_concat(a, b); }
int64_t string_from_char(int64_t c)               { return kk_string_from_char(c); }
int64_t string_int_to_string(int64_t i)           { return kk_str_show_int(i); }
/* Mercury @string.from_int(I) = string@ — same semantics as
 * @string.int_to_string@.  surd's @euler_integrate.int_sqrt@ uses this
 * to bridge an integer through @math.sqrt@ via the string round-trip;
 * without an actual implementation the bridge's link-time leak stub
 * returned 0, downstream @integer.det_from_string(0)@ also returned 0,
 * and @sqrt(4)@ silently became 0 in rat_sqrt — triggering a
 * divide-by-zero in @inv_hyp_arg@ during inverse-trig detection. */
int64_t string_from_int(int64_t i)                { return kk_str_show_int(i); }

/* string.append_list(Xs) — concatenate a list of strings. */
int64_t string_append_list(int64_t xs) {
    int64_t acc = kk_string_empty();
    while (!kk_is_nil(xs)) {
        int64_t h = kk_field(xs, 0);
        int64_t t = kk_field(xs, 1);
        acc = kk_str_concat(acc, h);
        xs = t;
    }
    return acc;
}

/* string.join_list(Sep, Xs) — join with separator. */
int64_t string_join_list(int64_t sep, int64_t xs) {
    if (kk_is_nil(xs)) return kk_string_empty();
    int64_t acc = kk_field(xs, 0);
    int64_t rest = kk_field(xs, 1);
    while (!kk_is_nil(rest)) {
        acc = kk_str_concat(acc, sep);
        acc = kk_str_concat(acc, kk_field(rest, 0));
        rest = kk_field(rest, 1);
    }
    return acc;
}

/* Mercury heap-string layout: tag KK_STRING_TAG, field 0 = char*,
 * field 1 = byte_len (i64).  Reuses the kk_field convention. */

/* string.index(S, I) — return the codepoint at byte index I, or 0
 * if out of range.  Trivial char-array indexing (ASCII fast path). */
int64_t string_index(int64_t s, int64_t i) {
    int64_t ptr_i = kk_field(s, 0);
    int64_t len = kk_field(s, 1);
    if (i < 0 || i >= len) return 0;
    const char* p = (const char*)(uintptr_t)ptr_i;
    return (int64_t)(unsigned char)p[i];
}

/* string.contains_char(S, C) — semidet: returns 1 if C appears in S. */
int64_t string_contains_char(int64_t s, int64_t c) {
    int64_t ptr_i = kk_field(s, 0);
    int64_t len = kk_field(s, 1);
    const char* p = (const char*)(uintptr_t)ptr_i;
    for (int64_t i = 0; i < len; i++) {
        if ((int64_t)(unsigned char)p[i] == c) return 1;
    }
    return 0;
}

/* string.duplicate_char(C, N) — string of N copies of char C. */
int64_t string_duplicate_char(int64_t c, int64_t n) {
    if (n <= 0) return kk_string_empty();
    char* buf = (char*)malloc((size_t)n + 1);
    if (!buf) return kk_string_empty();
    memset(buf, (int)c, (size_t)n);
    buf[n] = 0;
    int64_t r = kk_str_alloc_leaf_owned(buf, n);
    free(buf);
    return r;
}

/* string.to_int(S) — semidet: returns the parsed int, or 0 on failure.
 * In Mercury, this returns Maybe(int); the bridge's semidet handling
 * boxes the success value or returns a sentinel. */
int64_t string_to_int(int64_t s) {
    int64_t ptr_i = kk_field(s, 0);
    int64_t len = kk_field(s, 1);
    const char* p = (const char*)(uintptr_t)ptr_i;
    char tmp[64];
    int64_t n = len < 63 ? len : 63;
    memcpy(tmp, p, (size_t)n);
    tmp[n] = 0;
    return (int64_t)strtoll(tmp, NULL, 10);
}

/* string.to_float(S) — semidet: same shape as to_int. */
int64_t string_to_float(int64_t s) {
    /* @s@ is a @kk_string_t*@, NOT a ctor cell.  The previous version
     * indexed it as if @kk_field(s, 0)@ would yield the byte pointer,
     * but on a real string struct that offset is @rc@ (refcount).
     * memcpy from a refcount-as-pointer crashes.  Use the kk_string_t
     * accessors: @byte_len@ and @kk_str_bytes@ via flatten so any
     * concat/slice rope is materialised contiguously. */
    if (!s) return kkf_box(0.0);
    int64_t flat = kk_str_flatten(s);
    kk_string_t* str = (kk_string_t*)flat;
    if (!str) return kkf_box(0.0);
    int64_t len = str->byte_len;
    const char* p = kk_str_bytes(str);
    char tmp[64];
    int64_t n = len < 63 ? len : 63;
    if (n > 0 && p) memcpy(tmp, p, (size_t)n);
    tmp[n] = 0;
    return kkf_box(strtod(tmp, NULL));
}

/* string.sub_string_search(Whole, Sub) → ByteIndex (semidet).
 * Use memmem-style search; return -1 if not found (bridge will treat
 * the semidet correctly via the failure path). */
int64_t string_sub_string_search(int64_t whole, int64_t sub) {
    int64_t wp_i = kk_field(whole, 0), wl = kk_field(whole, 1);
    int64_t sp_i = kk_field(sub, 0),   sl = kk_field(sub, 1);
    const char* wp = (const char*)(uintptr_t)wp_i;
    const char* sp = (const char*)(uintptr_t)sp_i;
    if (sl == 0) return 0;
    if (sl > wl) return -1;
    for (int64_t i = 0; i <= wl - sl; i++) {
        if (memcmp(wp + i, sp, (size_t)sl) == 0) return i;
    }
    return -1;
}

/* Forward declarations for the static helpers defined further below. */
static int64_t kk_call_closure_1(int64_t closure, int64_t a);
static int64_t kk_call_closure_2(int64_t closure, int64_t a, int64_t b);
static int64_t kk_call_closure_3(int64_t closure, int64_t a, int64_t b, int64_t c);
static int64_t kk_call_closure_4(int64_t closure, int64_t a, int64_t b, int64_t c, int64_t d);

/* list.all_true(P, Xs) — semidet: P holds for every X in Xs.
 * HLDS prepends one type_info arg (for the element type), so the
 * runtime signature is (TI, P, Xs).  The previous 2-arg signature
 * misaligned the stack: P pulled in the TI's value and Xs pulled in
 * the closure, so kk_call_closure_1 dereferenced a non-closure cell
 * as a function pointer and wild-jumped (observed in surd-elliptic's
 * norm_poly_is_zero → list.all_true(norm_is_zero, _) call site). */
int64_t list_all_true(int64_t tinfo, int64_t p, int64_t xs) {
    (void)tinfo;
    while (!kk_is_nil(xs)) {
        int64_t h = kk_field(xs, 0);
        int64_t r = kk_call_closure_1(p, h);
        if (r == 0) return 0;
        xs = kk_field(xs, 1);
    }
    return 1;
}

/* list.drop(N, Xs, Suffix) — det: bind Suffix to Xs with first N elems
 * removed.  In our model returns the list directly. */
int64_t list_drop(int64_t n, int64_t xs) {
    while (n > 0 && !kk_is_nil(xs)) {
        xs = kk_field(xs, 1);
        n--;
    }
    return xs;
}

/* list.delete_all(Xs, X, Ys) — remove all occurrences of X from Xs. */
int64_t list_delete_all(int64_t tinfo, int64_t xs, int64_t x) {
    (void)tinfo;
    /* Build result by walking, skipping any element equal to x. */
    int64_t* slots = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(xs)) {
        int64_t h = kk_field(xs, 0);
        if (!kk_structural_eq(h, x)) {
            if (n + 1 > cap) {
                cap = cap == 0 ? 8 : cap * 2;
                slots = (int64_t*)realloc(slots, (size_t)cap * sizeof(int64_t));
            }
            slots[n++] = h;
        }
        xs = kk_field(xs, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(slots[i], acc);
    free(slots);
    return acc;
}

/* list.det_tail(Xs) — return the tail; abort if Xs is nil. */
int64_t list_det_tail(int64_t tinfo, int64_t xs) {
    (void)tinfo;
    if (kk_is_nil(xs)) {
        fprintf(stderr, "*** list.det_tail: empty list\n");
        exit(1);
    }
    return kk_field(xs, 1);
}

/* list.det_split_last(Xs, AllButLast, Last) — split off the last element.
 * In our flattened return we'd return a tuple {AllButLast, Last}; for
 * the bridge's i64 model, return the AllButLast list (Last is dropped
 * since the bridge's call-site only binds the first output). */
int64_t list_det_split_last(int64_t tinfo, int64_t xs) {
    (void)tinfo;
    if (kk_is_nil(xs)) {
        fprintf(stderr, "*** list.det_split_last: empty list\n");
        exit(1);
    }
    /* Walk and collect everything except the last element. */
    int64_t* slots = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(xs)) {
        int64_t h = kk_field(xs, 0);
        int64_t t = kk_field(xs, 1);
        if (kk_is_nil(t)) break;  /* h is the second-to-last; t is the [last]. */
        if (n + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            slots = (int64_t*)realloc(slots, (size_t)cap * sizeof(int64_t));
        }
        slots[n++] = h;
        xs = t;
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(slots[i], acc);
    free(slots);
    return acc;
}

/* list.take_upto(N, Xs, Ys) — take at most N elements from Xs. */
int64_t list_take_upto(int64_t tinfo, int64_t n, int64_t xs) {
    (void)tinfo;
    int64_t* slots = NULL;
    int64_t cap = 0, cnt = 0;
    while (cnt < n && !kk_is_nil(xs)) {
        int64_t h = kk_field(xs, 0);
        if (cnt + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            slots = (int64_t*)realloc(slots, (size_t)cap * sizeof(int64_t));
        }
        slots[cnt++] = h;
        xs = kk_field(xs, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = cnt - 1; i >= 0; i--) acc = kk_cons(slots[i], acc);
    free(slots);
    return acc;
}

/* list.sort_and_remove_dups(Xs, Ys) — sort (typeinfo-dispatched compare)
 * and drop consecutive duplicates.  Use kk_compare for ordering. */
int64_t list_sort_and_remove_dups(int64_t tinfo, int64_t xs) {
    (void)tinfo;
    /* Materialize into an array. */
    int64_t* arr = NULL;
    int64_t cap = 0, n = 0;
    {
        int64_t cur = xs;
        while (!kk_is_nil(cur)) {
            if (n + 1 > cap) {
                cap = cap == 0 ? 16 : cap * 2;
                arr = (int64_t*)realloc(arr, (size_t)cap * sizeof(int64_t));
            }
            arr[n++] = kk_field(cur, 0);
            cur = kk_field(cur, 1);
        }
    }
    /* Simple insertion sort using kk_compare. */
    for (int64_t i = 1; i < n; i++) {
        int64_t v = arr[i];
        int64_t j = i - 1;
        while (j >= 0 && kk_compare(arr[j], v) > 0) {
            arr[j + 1] = arr[j];
            j--;
        }
        arr[j + 1] = v;
    }
    /* Build cons list, skipping consecutive duplicates. */
    int64_t acc = kk_nil();
    int64_t prev = 0;
    int havePrev = 0;
    for (int64_t i = n - 1; i >= 0; i--) {
        if (!havePrev || !kk_structural_eq(arr[i], prev)) {
            acc = kk_cons(arr[i], acc);
            prev = arr[i];
            havePrev = 1;
        }
    }
    free(arr);
    return acc;
}

/* list.map_corresponding(F, Xs, Ys, Zs) — parallel map; both lists same
 * length.  Typeinfo dispatch prepends 3 type args for input/input/output. */
int64_t list_map_corresponding(int64_t tinfo_a, int64_t tinfo_b,
                               int64_t tinfo_c, int64_t f,
                               int64_t xs, int64_t ys) {
    (void)tinfo_a; (void)tinfo_b; (void)tinfo_c;
    int64_t* slots = NULL;
    int64_t cap = 0, cnt = 0;
    while (!kk_is_nil(xs) && !kk_is_nil(ys)) {
        int64_t h1 = kk_field(xs, 0);
        int64_t h2 = kk_field(ys, 0);
        int64_t z = kk_call_closure_2(f, h1, h2);
        if (cnt + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            slots = (int64_t*)realloc(slots, (size_t)cap * sizeof(int64_t));
        }
        slots[cnt++] = z;
        xs = kk_field(xs, 1);
        ys = kk_field(ys, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = cnt - 1; i >= 0; i--) acc = kk_cons(slots[i], acc);
    free(slots);
    return acc;
}

/* int.even / int.odd — semidet parity tests. */
int64_t int_even(int64_t n) { return (n % 2 == 0) ? 1 : 0; }
int64_t int_odd(int64_t n)  { return (n % 2 != 0) ? 1 : 0; }

/* integer.pow(Base, Exp, Result) — non-negative integer exponentiation. */
int64_t integer_pow(int64_t base, int64_t exp) {
    int64_t r = 1;
    while (exp > 0) {
        if (exp & 1) r *= base;
        base *= base;
        exp >>= 1;
    }
    return r;
}
int64_t integer_det_to_int(int64_t x) { return x; }

/* float.round_to_int / float.truncate_to_int — aliases for the
 * already-existing float_round / float_truncate stubs. */
int64_t float_round_to_int(int64_t a)    { return float_round(a); }
int64_t float_truncate_to_int(int64_t a) { return float_truncate(a); }

/* mercury_not — logical negation: returns 1 - x for x in {0, 1}. */
int64_t mercury_not(int64_t closure) {
    /* The argument is a closure (a goal-as-value).  Call it; invert. */
    int64_t r = kk_call_closure_1(closure, 0);
    return r == 0 ? 1 : 0;
}

/* builtin.compare(Result, A, B) — set Result to ordering of A vs B.
 * In Mercury this is a 3-arg pred with the result as output; the
 * bridge sees it as 4 args (including a typeclass-info arg).  We
 * accept four i64 args and return the comparison result. */
int64_t builtin_compare(int64_t tinfo, int64_t a, int64_t b, int64_t cmp_out) {
    (void)tinfo; (void)cmp_out;
    int64_t c = kk_compare(a, b);
    /* Mercury comparison_result: (<)=0, (=)=1, (>)=2 — but our enum
     * uses different tags.  We return the kk_compare value; callers
     * that pattern-match on builtin.(=)/(<)/(>) tags will misalign
     * but for stub purposes this is the best we can do without
     * threading ctor tags from the bridge. */
    return c;
}
int64_t builtin_ordering(int64_t tinfo, int64_t a, int64_t b) {
    (void)tinfo;
    return kk_compare(a, b);
}

/* list_range(Lo, Hi) — Mercury list range syntax @list.(Lo .. Hi)@. */
extern int64_t kk_range_list(int64_t lo, int64_t hi);
int64_t list_range(int64_t lo, int64_t hi) { return kk_range_list(lo, hi); }

/* Mercury private-builtin type-info helpers.  The compiler synthesises
 * @type_info_const@ / @type_info_cell_constructor@ calls during type
 * specialisation to carry around runtime type-info values; our
 * erased-type model treats them as opaque i64 sentinels.  The arity
 * suffix matches the bridge's call-site naming convention
 * (@__N@ where N is the call arity post-output-drop). */
int64_t type_info_const__1(int64_t n) { return n; }
int64_t type_info_cell_constructor__2(int64_t a, int64_t b) {
    (void)a; (void)b; return 0;
}
int64_t type_info_cell_constructor__4(int64_t a, int64_t b, int64_t c, int64_t d) {
    (void)a; (void)b; (void)c; (void)d; return 0;
}
int64_t type_info_cell_constructor__3(int64_t a, int64_t b, int64_t c) {
    (void)a; (void)b; (void)c; return 0;
}
int64_t type_info_cell_constructor__5(int64_t a, int64_t b, int64_t c, int64_t d, int64_t e) {
    (void)a; (void)b; (void)c; (void)d; (void)e; return 0;
}
int64_t typeclass_info_const__1(int64_t n) { return n; }

/* Mercury io.command_line_arguments(Args, !IO) — returns the argv list
 * (excluding argv[0]).  Bridge calls with 2 args after dropping
 * STATE_VARIABLE_IO output: (Args, IO).  Args is the bridge's
 * synthetic output-binding slot (the bridge writes to it via the
 * let-binding), so we just return kk_nil().  IO threads through. */
int64_t io_command_line_arguments(int64_t args, int64_t io) {
    (void)args; (void)io;
    return kk_nil();
}

/* Mercury pair "minus" — surd-mercury uses @K - V@ as a pair literal
 * (the @pair@ module declares @-@ as the pair constructor).  Returns
 * a 2-field heap cell with fields {K, V}.  The 3-arg variant emerges
 * when the pair is the OUTPUT of a det predicate; the bridge passes
 * the would-be output last, so we honour the (K, V, _Out) signature
 * by ignoring the third arg. */
int64_t pair_zm(int64_t k, int64_t v) {
    int64_t p = kk_alloc_con(0, 2);
    kk_set_field(p, 0, k);
    kk_set_field(p, 1, v);
    return p;
}

/* Mercury `map` module — implemented here as a cons-of-pairs.  Mercury's
 * real implementation is a 2-3 tree, but for surd-mercury demos a flat
 * list suffices.  Each entry is a 2-field pair cell {Key, Value}; the
 * map itself is a cons-list of these pairs.  Empty map = nil. */
/* Mercury's stdlib @pair(A, B)@ is defined as @type pair(A, B) ---> A - B@ —
 * the @-@ infix is the pair constructor.  The bridge's ECon path for
 * source-level @pair.(A - B)@ patterns allocates cells with tag
 * @stableConTag("-") = 46576@; user-side deconstructs like
 * @T = (Mono - C)@ in @norm_inv@ and @norm_coeff@ check that tag.
 *
 * The earlier tag-0 sig made every runtime-built pair invisible to
 * those source patterns — @norm_inv@'s single-mono fast path skipped,
 * @norm_coeff@ couldn't extract a pure-rational coefficient, and
 * surd's @rationalize_inv@ tripped its @require.error("cannot invert")@
 * fallback in the elliptic-integral reduction.  Aligning the tag means
 * runtime-allocated pairs deconstruct cleanly via the source-level
 * idiom — the same convention the bridge already uses. */
#define KK_PAIR_DASH_TAG 46576
static int64_t kk_pair_new(int64_t k, int64_t v) {
    int64_t p = kk_alloc_con(KK_PAIR_DASH_TAG, 2);
    kk_set_field(p, 0, k);
    kk_set_field(p, 1, v);
    return p;
}
static int64_t kk_pair_fst(int64_t p) { return kk_field(p, 0); }
static int64_t kk_pair_snd(int64_t p) { return kk_field(p, 1); }

/* map.init(M) — bind M to the empty map. */
/* map.init — Mercury passes 2 TCIs (for K and V types).  Both ignored. */
int64_t map_init(int64_t tinfo_k, int64_t tinfo_v) {
    (void)tinfo_k; (void)tinfo_v;
    return kk_nil();
}

/* map.search(M, K, V) — semidet: V := value associated with K in M;
 * fails (returns 0) if missing.  Mercury threads two type-infos
 * (one per type parameter of map(K, V)), so the runtime signature
 * is (TI_K, TI_V, M, K) → V.  The returned value is RETAINED so the
 * caller owns it independently of M (which is typically dropped
 * after this returns). */
int64_t map_search(int64_t tinfo_k, int64_t tinfo_v, int64_t m, int64_t k) {
    (void)tinfo_k; (void)tinfo_v;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        if (kk_structural_eq(kk_pair_fst(p), k)) {
            int64_t v = kk_pair_snd(p);
            kk_retain(v);
            return v;
        }
        m = kk_field(m, 1);
    }
    return 0;
}

/* map.lookup(M, K) = V — det.  Mercury passes 2 TCIs (K, V types). */
int64_t map_lookup(int64_t tinfo_k, int64_t tinfo_v, int64_t m, int64_t k) {
    (void)tinfo_v;
    return map_search(tinfo_k, tinfo_v, m, k);
}

/* map.contains(M, K) — semidet.  Takes 2 TCIs (K, V) like map.search. */
int64_t map_contains(int64_t tinfo_k, int64_t tinfo_v, int64_t m, int64_t k) {
    (void)tinfo_k; (void)tinfo_v;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        if (kk_structural_eq(kk_pair_fst(p), k)) return 1;
        m = kk_field(m, 1);
    }
    return 0;
}

/* map.count(M) = N — det.  Mercury threads 2 TCIs (K, V). */
int64_t map_count(int64_t tinfo_k, int64_t tinfo_v, int64_t m) {
    (void)tinfo_k; (void)tinfo_v;
    int64_t n = 0;
    while (!kk_is_nil(m)) { n++; m = kk_field(m, 1); }
    return n;
}

/* map.set(M, K, V) = M' — det.  Mercury passes 2 TCIs (K, V types).
 * Insert (or update if K already present) and return the new map.  Walks
 * once; if K is found we replace its pair, otherwise prepend a fresh
 * pair at the head. */
int64_t map_set(int64_t tinfo_k, int64_t tinfo_v, int64_t m, int64_t k, int64_t v) {
    (void)tinfo_k; (void)tinfo_v;
    /* Build a new map by walking M, replacing the matching pair if any.
     * Retain each surviving key/value before putting it in the new map:
     * the input map M is dropped by the caller after this returns, and
     * its drop cascades through cons → pair → (key, value), so without
     * retain the new map's references would be freed underneath. */
    int64_t* keys = NULL;
    int64_t* vals = NULL;
    int64_t cap = 0, n = 0;
    int replaced = 0;
    int64_t cur = m;
    while (!kk_is_nil(cur)) {
        int64_t p = kk_field(cur, 0);
        int64_t pk = kk_pair_fst(p);
        int64_t pv = kk_pair_snd(p);
        if (n + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            keys = (int64_t*)realloc(keys, (size_t)cap * sizeof(int64_t));
            vals = (int64_t*)realloc(vals, (size_t)cap * sizeof(int64_t));
        }
        if (!replaced && kk_structural_eq(pk, k)) {
            kk_retain(pk);
            keys[n] = pk; vals[n] = v;
            replaced = 1;
        } else {
            kk_retain(pk);
            kk_retain(pv);
            keys[n] = pk; vals[n] = pv;
        }
        n++;
        cur = kk_field(cur, 1);
    }
    int64_t acc = kk_nil();
    if (!replaced) acc = kk_cons(kk_pair_new(k, v), acc);
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(kk_pair_new(keys[i], vals[i]), acc);
    free(keys); free(vals);
    return acc;
}

/* map.det_insert(M, K, V) and map.det_update(M, K, V) both alias to set
 * in our stub.  Real Mercury aborts on key-already-present /
 * key-not-found, but stub behaviour is to silently insert/update.
 * Mercury passes 2 TCIs (K, V types). */
/* Mercury's @map.det_insert(K, V, !M)@ and @map.det_update(K, V, !M)@
 * expand at HLDS level to @det_insert(TCI_K, TCI_V, K, V, M_in, M_out)@,
 * with K and V BEFORE the input map.  Mercury's @map.set@ uses the
 * @(M0, K, V) = M1@ convention with M FIRST.  The bridge passes args
 * in HLDS order, so the runtime stubs for det_insert/det_update must
 * accept @(tinfo_k, tinfo_v, k, v, m)@ — not @(tinfo_k, tinfo_v, m, k, v)@
 * as previously coded (the original signature was a guess that
 * silently swallowed surd's surd-elliptic insertions: walking the atom
 * cell as if it were a cons list produced bogus 3-entry atom maps
 * which propagated as "no radical" through @from_norm_expr@ and
 * rendered as "0 · F(...)"). */
int64_t map_det_insert(int64_t tinfo_k, int64_t tinfo_v, int64_t k, int64_t v, int64_t m) {
    return map_set(tinfo_k, tinfo_v, m, k, v);
}
int64_t map_det_update(int64_t tinfo_k, int64_t tinfo_v, int64_t k, int64_t v, int64_t m) {
    return map_set(tinfo_k, tinfo_v, m, k, v);
}

/* Mercury @map.delete(M, K) = M'@ — polymorphic over both K and V, so
 * HLDS prepends TWO type_info args (TI_K, TI_V).  Same arity-mismatch
 * pattern as @map_keys@: a one-TI sig drops the second TI off the
 * stack alignment and shifts the actual map pointer one slot, leaving
 * the runtime traversing random memory. */
int64_t map_delete(int64_t tinfo_k, int64_t tinfo_v, int64_t m, int64_t k) {
    (void)tinfo_k; (void)tinfo_v;
    int64_t* keys = NULL;
    int64_t* vals = NULL;
    int64_t cap = 0, n = 0;
    int64_t cur = m;
    while (!kk_is_nil(cur)) {
        int64_t p = kk_field(cur, 0);
        int64_t pk = kk_pair_fst(p);
        int64_t pv = kk_pair_snd(p);
        if (!kk_structural_eq(pk, k)) {
            if (n + 1 > cap) {
                cap = cap == 0 ? 8 : cap * 2;
                keys = (int64_t*)realloc(keys, (size_t)cap * sizeof(int64_t));
                vals = (int64_t*)realloc(vals, (size_t)cap * sizeof(int64_t));
            }
            keys[n] = pk; vals[n] = pv;
            n++;
        }
        cur = kk_field(cur, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(kk_pair_new(keys[i], vals[i]), acc);
    free(keys); free(vals);
    return acc;
}

/* map.from_assoc_list(Pairs, M) — det: build a map from a list of
 * key-value pairs.  We accept the list directly since our map is
 * already a list of pairs. */
/* Mercury's HLDS adds two type-info arguments for map's K and V type
 * params (see normal_form.canonicalize_nested_root's call site).  The
 * stub ignores both; in our model the map IS the assoc list.  Taking
 * the third arg here matches the HLDS call shape so the bridge
 * doesn't see the call as oversaturated and synthesize a bogus
 * closure-indirect call on the returned list. */
int64_t map_from_assoc_list(int64_t tinfo_k, int64_t tinfo_v, int64_t pairs) {
    (void)tinfo_k;
    (void)tinfo_v;
    return pairs;
}

/* map.values(M, Vs) — det: list of values in some order. */
int64_t map_values(int64_t tinfo, int64_t m) {
    (void)tinfo;
    int64_t* vs = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        if (n + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            vs = (int64_t*)realloc(vs, (size_t)cap * sizeof(int64_t));
        }
        vs[n++] = kk_pair_snd(p);
        m = kk_field(m, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(vs[i], acc);
    free(vs);
    return acc;
}

/* map.keys(M, Ks) — det: list of keys in some order. */
/* Mercury @map.keys/1@ is polymorphic over both K and V — the HLDS
 * prepends TWO type_info args (TI_K, TI_V), not one.  Earlier signature
 * (tinfo, m) caused the bridge's call to emit the 3-arg version against
 * a 2-arg runtime decl; the LLVM lowering then treated the actual map
 * pointer as the result and proceeded with random memory as the keys
 * list, leading to segfaults in @find_radical_atom@'s @map.keys(As)@.
 * Accept both TCIs and ignore them. */
int64_t map_keys(int64_t tinfo_k, int64_t tinfo_v, int64_t m) {
    (void)tinfo_k; (void)tinfo_v;
    int64_t* ks = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        if (n + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            ks = (int64_t*)realloc(ks, (size_t)cap * sizeof(int64_t));
        }
        ks[n++] = kk_pair_fst(p);
        m = kk_field(m, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(ks[i], acc);
    free(ks);
    return acc;
}

/* map.foldl(F, M, A, A') — det: fold F over (K, V) pairs threading
 * the accumulator.  F is a 3-arg closure (K, V, A) -> A'.
 * Mercury's HLDS prepends typeclass-info args; we accept and ignore. */
int64_t map_foldl(int64_t tinfo_k, int64_t tinfo_v, int64_t tinfo_a,
                  int64_t f, int64_t m, int64_t acc0) {
    (void)tinfo_k; (void)tinfo_v; (void)tinfo_a;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        int64_t k = kk_pair_fst(p);
        int64_t v = kk_pair_snd(p);
        /* Retain k, v, and f: closure consumes them via Perceus drops.
         * Without retains, M's pair still references k/v while the
         * closure simultaneously drops them, taking refcount to 0 and
         * wiping the values out of subsequent iterations' lookups
         * (observed in surd's dag_fold_constants foldl over Nodes). */
        kk_retain(k);
        kk_retain(v);
        kk_retain(f);
        acc0 = kk_call_closure_3(f, k, v, acc0);
        m = kk_field(m, 1);
    }
    return acc0;
}

/* Mercury `set` module — flat-list implementation (kk_cons / kk_nil),
 * with kk_structural_eq for membership.  Like map.* this is a stub
 * sufficient for surd-mercury demos rather than a real balanced tree. */
int64_t set_init(int64_t tinfo)            { (void)tinfo; return kk_nil(); }

int64_t set_member(int64_t tinfo, int64_t x, int64_t s) {
    (void)tinfo;
    while (!kk_is_nil(s)) {
        int64_t h = kk_field(s, 0);
        if (kk_structural_eq(h, x)) return 1;
        s = kk_field(s, 1);
    }
    return 0;
}

int64_t set_insert(int64_t tinfo, int64_t s, int64_t x) {
    if (set_member(tinfo, x, s)) return s;
    return kk_cons(x, s);
}

int64_t set_delete(int64_t tinfo, int64_t s, int64_t x) {
    return list_delete_all(tinfo, s, x);
}

int64_t set_contains(int64_t tinfo, int64_t s, int64_t x) {
    return set_member(tinfo, x, s);
}

int64_t set_make_singleton_set(int64_t tinfo, int64_t x) {
    (void)tinfo;
    return kk_cons(x, kk_nil());
}

int64_t set_count(int64_t tinfo, int64_t s) {
    (void)tinfo;
    int64_t n = 0;
    while (!kk_is_nil(s)) { n++; s = kk_field(s, 1); }
    return n;
}

int64_t set_to_sorted_list(int64_t tinfo, int64_t s) {
    return list_sort_and_remove_dups(tinfo, s);
}

int64_t set_from_list(int64_t tinfo, int64_t xs) {
    return list_sort_and_remove_dups(tinfo, xs);
}

int64_t set_is_empty(int64_t tinfo, int64_t s) {
    (void)tinfo;
    return kk_is_nil(s);
}

int64_t set_union(int64_t tinfo, int64_t a, int64_t b) {
    while (!kk_is_nil(a)) {
        int64_t h = kk_field(a, 0);
        b = set_insert(tinfo, b, h);
        a = kk_field(a, 1);
    }
    return b;
}

int64_t set_intersect(int64_t tinfo, int64_t a, int64_t b) {
    int64_t* slots = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(a)) {
        int64_t h = kk_field(a, 0);
        if (set_member(tinfo, h, b)) {
            if (n + 1 > cap) {
                cap = cap == 0 ? 8 : cap * 2;
                slots = (int64_t*)realloc(slots, (size_t)cap * sizeof(int64_t));
            }
            slots[n++] = h;
        }
        a = kk_field(a, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(slots[i], acc);
    free(slots);
    return acc;
}

int64_t set_difference(int64_t tinfo, int64_t a, int64_t b) {
    int64_t* slots = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(a)) {
        int64_t h = kk_field(a, 0);
        if (!set_member(tinfo, h, b)) {
            if (n + 1 > cap) {
                cap = cap == 0 ? 8 : cap * 2;
                slots = (int64_t*)realloc(slots, (size_t)cap * sizeof(int64_t));
            }
            slots[n++] = h;
        }
        a = kk_field(a, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(slots[i], acc);
    free(slots);
    return acc;
}

/* Mercury integer.div / integer.det_from_string / etc. */
int64_t integer_div(int64_t a, int64_t b) { return b == 0 ? 0 : a / b; }
int64_t integer_det_from_string(int64_t s) {
    /* @s@ is a @kk_string_t*@, not a ctor cell.  The earlier
     * @kk_field(s, 0)@ shape was reading the string's @rc@ slot as a
     * byte pointer — memcpy then dereferenced a refcount-as-address and
     * crashed in __memmove_avx_unaligned_erms.  Use kk_str_flatten +
     * kk_str_bytes + byte_len. */
    if (!s) return 0;
    int64_t flat = kk_str_flatten(s);
    kk_string_t* str = (kk_string_t*)flat;
    if (!str) return 0;
    int64_t len = str->byte_len;
    const char* p = kk_str_bytes(str);
    char tmp[64];
    int64_t n = len < 63 ? len : 63;
    if (n > 0 && p) memcpy(tmp, p, (size_t)n);
    tmp[n] = 0;
    return (int64_t)strtoll(tmp, NULL, 10);
}

/* Mercury string.float_to_string(F, S) — det: stringify a float. */
int64_t string_float_to_string(int64_t f) {
    double d;
    memcpy(&d, &f, 8);
    char tmp[64];
    int n = snprintf(tmp, sizeof tmp, "%g", d);
    if (n < 0) n = 0;
    return kk_str_alloc_leaf_owned(tmp, n);
}

/* Mercury string.prefix(S, P) — semidet: returns 1 if S starts with P. */
int64_t string_prefix(int64_t s, int64_t p) {
    int64_t sp_i = kk_field(s, 0), sl = kk_field(s, 1);
    int64_t pp_i = kk_field(p, 0), pl = kk_field(p, 1);
    const char* sp = (const char*)(uintptr_t)sp_i;
    const char* pp = (const char*)(uintptr_t)pp_i;
    if (pl > sl) return 0;
    return memcmp(sp, pp, (size_t)pl) == 0 ? 1 : 0;
}

/* Mercury map.is_empty(M) — semidet. */
int64_t map_is_empty(int64_t tinfo_k, int64_t tinfo_v, int64_t m) {
    (void)tinfo_k; (void)tinfo_v;
    return kk_is_nil(m);
}

/* Mercury map.singleton(K, V) = M — det: build a one-entry map. */
int64_t map_singleton(int64_t tinfo_k, int64_t tinfo_v, int64_t k, int64_t v) {
    (void)tinfo_k; (void)tinfo_v;
    return kk_cons(kk_pair_new(k, v), kk_nil());
}

/* Mercury map.to_assoc_list(M, AL) — det: return the list of pairs. */
int64_t map_to_assoc_list(int64_t tinfo_k, int64_t tinfo_v, int64_t m) {
    (void)tinfo_k; (void)tinfo_v;
    return m;
}

/* Mercury map.overlay(M1, M2, M3) — det: M2 overlays M1 (M2's keys win). */
int64_t map_overlay(int64_t tinfo_k, int64_t tinfo_v, int64_t m1, int64_t m2) {
    int64_t m = m1;
    int64_t cur = m2;
    while (!kk_is_nil(cur)) {
        int64_t p = kk_field(cur, 0);
        m = map_set(tinfo_k, tinfo_v, m, kk_pair_fst(p), kk_pair_snd(p));
        cur = kk_field(cur, 1);
    }
    return m;
}

/* Mercury map.map_values(F, M, M') — apply F to every value.
 * F is a Mercury @func(K, V) = V'@ — TAKES TWO ARGS (key + value).
 * Earlier impl passed only the value via @kk_call_closure_1@, leaving
 * the closure's second slot uninitialised; the lambda's V slot then
 * received the closure pointer itself (or a garbage i64), which
 * downstream @rational.'*'@ saw as a malformed rational and (via the
 * @safe_rational_mul@ shim) substituted with @rational(0,1)@.
 * normal_form's @norm_scale@'s value-mapping then collapsed every
 * non-zero entry to zero, @map_filter_nonzero@ stripped them all out,
 * and @norm_inv@ tripped its "division by zero" guard on the now-empty
 * map.  Pass BOTH k and v so the lambda's V binding matches the actual
 * map value. */
int64_t map_map_values(int64_t tinfo_k, int64_t tinfo_v1, int64_t tinfo_v2,
                       int64_t f, int64_t m) {
    (void)tinfo_k; (void)tinfo_v1; (void)tinfo_v2;
    int64_t* keys = NULL;
    int64_t* vals = NULL;
    int64_t cap = 0, n = 0;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        int64_t k = kk_pair_fst(p);
        int64_t v = kk_pair_snd(p);
        if (n + 1 > cap) {
            cap = cap == 0 ? 8 : cap * 2;
            keys = (int64_t*)realloc(keys, (size_t)cap * sizeof(int64_t));
            vals = (int64_t*)realloc(vals, (size_t)cap * sizeof(int64_t));
        }
        keys[n] = k;
        kk_retain(k);
        kk_retain(v);
        kk_retain(f);
        vals[n] = kk_call_closure_2(f, k, v);
        n++;
        m = kk_field(m, 1);
    }
    int64_t acc = kk_nil();
    for (int64_t i = n - 1; i >= 0; i--) acc = kk_cons(kk_pair_new(keys[i], vals[i]), acc);
    free(keys); free(vals);
    return acc;
}

/* Mercury map.foldl2(F, M, A0, A, B0, B) — fold over (K, V) pairs
 * threading TWO accumulators.  F is the closure @pred(K, V, A, A', B, B')
 * is det@; the bridge's GoalLambda translator emits the closure body
 * with a @tuple(A', B')@ terminator (multi-output convention), so each
 * call returns a 2-field tuple cell — field 0 is A', field 1 is B'.
 *
 * Returns a @tuple(A, B)@ cell so the call site can recover BOTH final
 * accumulators.  The bridge's @useTupleConvention@ path
 * (extended for @map.foldl2@) deconstructs this tuple at the caller.
 * Without that, the second accumulator (atoms map in surd's
 * @reduce_monomial@) defaulted to literal 0 → @reduce_nested_roots@
 * received an empty map → norm_expr collapsed to zero → surd-elliptic's
 * @simplify_rad@ rendered every leading-coefficient term as "0 · F(...)". */
int64_t map_foldl2(int64_t tinfo_k, int64_t tinfo_v, int64_t tinfo_a, int64_t tinfo_b,
                   int64_t f, int64_t m, int64_t a, int64_t b) {
    (void)tinfo_k; (void)tinfo_v; (void)tinfo_a; (void)tinfo_b;
    while (!kk_is_nil(m)) {
        int64_t p = kk_field(m, 0);
        int64_t k = kk_pair_fst(p);
        int64_t v = kk_pair_snd(p);
        kk_retain(k);
        kk_retain(v);
        kk_retain(f);
        int64_t result = kk_call_closure_4(f, k, v, a, b);
        if (kk_is_heap_ptr(result) && kk_nfields(result) >= 2) {
            a = kk_field(result, 0);
            b = kk_field(result, 1);
            kk_retain(a);
            kk_retain(b);
            kk_drop(result);
        } else {
            a = result;
        }
        m = kk_field(m, 1);
    }
    /* Return tuple(A, B) using the standard "tuple" ctor tag (20379 from
     * stableConTag("tuple") — matches the bridge's GoalLambda multi-output
     * terminator and useTupleConvention's PatCon "tuple" deconstruction). */
    int64_t tup = kk_alloc_con(20379, 2);
    kk_set_field(tup, 0, a);
    kk_set_field(tup, 1, b);
    return tup;
}

/* Mercury list.foldl2(P, L, A, A', B, B') — list fold threading TWO accs.
 * Same convention as map.foldl2 — drop the second acc. */
int64_t list_foldl2_impl(int64_t f, int64_t xs, int64_t a, int64_t b) {
    while (!kk_is_nil(xs)) {
        int64_t h = kk_field(xs, 0);
        /* See kk_list_foldl for the retain rationale: the closure
         * receives h as owned and may drop it, but the cons cell still
         * references h via field 0.  Retain f too for the closure's
         * self-drop. */
        kk_retain(h);
        kk_retain(f);
        a = kk_call_closure_2(f, h, a);
        xs = kk_field(xs, 1);
    }
    (void)b;
    return a;
}

/* integer.divide_with_rem(A, B, Q, R) — Mercury pred binding Q and R
 * (quotient and remainder).  Originally returned a 2-tuple {Q, R}, but
 * the bridge's call-site convention binds the first trailing-output to
 * the returned value and default-binds subsequent outputs to 0.  Without
 * any bridge-side tuple deconstruct, surd's @prime_factors.count_factor@
 * received the tuple POINTER as Q (treating it as an integer), recursed
 * with the pointer value as N, and looped indefinitely.
 *
 * Return Q directly (an int).  The bridge's @wrapSecondaries@ pass
 * (see @CoreTranslate.hs@) supplies R via a separate @integer_rem@
 * call, matching the natural mathematical relation. */
int64_t integer_divide_with_rem(int64_t a, int64_t b) {
    return b == 0 ? 0 : a / b;
}

/* ---- Mercury HLDS higher-order shims -------------------------------------
 * The bridge sees Mercury `apply(F, X)`, `list.foldl(P, L, A)` etc. as
 * direct calls; after arity-tagging and stdlib-prefix filtering, the
 * call-site link symbols are `apply__2`, `list_foldl`, etc.  These
 * shims forward to the runtime's existing closure/list machinery
 * (kk_call_closure_N, kk_list_foldl, kk_list_map, kk_list_filter).
 *
 * Closure ABI (kk_runtime.h):  closure[0] = fptr i64; fptr is invoked
 * as fptr(closure, args...).  All values flow as i64.
 */
/* Forward declarations of the existing helpers (defined later in this TU). */
static int64_t kk_call_closure_1(int64_t closure, int64_t a);
static int64_t kk_call_closure_2(int64_t closure, int64_t a, int64_t b);
static int64_t kk_call_closure_3(int64_t closure, int64_t a, int64_t b, int64_t c);
static int64_t kk_call_closure_4(int64_t closure, int64_t a, int64_t b, int64_t c, int64_t d);
int64_t kk_list_foldl(int64_t xs, int64_t z, int64_t f);
int64_t kk_list_map(int64_t xs, int64_t f);
int64_t kk_list_filter(int64_t xs, int64_t p);
int64_t kk_list_length(int64_t xs);

/* Generic HO dispatch (apply / call / class_method_call). */
int64_t apply__2(int64_t f, int64_t a)        { return kk_call_closure_1(f, a); }
int64_t apply__3(int64_t f, int64_t a, int64_t b) { return kk_call_closure_2(f, a, b); }
int64_t call__2 (int64_t f, int64_t a)        { return kk_call_closure_1(f, a); }
int64_t call__3 (int64_t f, int64_t a, int64_t b) { return kk_call_closure_2(f, a, b); }
/* Mercury class_method_call/N — typeclass dispatch.
 *
 * Signature: @class_method_call(TypeClassInfo, MethodIndex, args...) = Result@
 *
 * The first arg is a typeclass-info dictionary (a heap cell with
 * method function pointers in its fields).  The second arg is the
 * method index.  The rest are the call arguments.
 *
 * In a real implementation we'd extract @TCI[MethodIndex]@ as a
 * closure and call it with the args.  The bridge's
 * @typeclass_info_const(0)@ stub returns an integer (not a real
 * dict), so the closure-extract would crash.  Return @0@ as a
 * sentinel result instead — keeps the binary running long enough
 * to exercise more of the program.  Programs that actually depend
 * on typeclass dispatch (numeric tower preds, comparison etc.) will
 * compute wrong answers, but at least won't segfault. */
int64_t class_method_call__2(int64_t f, int64_t a) {
    /* class_method_call(TCI, MethodIdx) — nullary method.  Without
     * the real dict we can't know whether the method is @zero@, @one@,
     * @nan@ etc.  Return 1 — a non-zero sentinel that won't trigger
     * downstream "divide by zero" if used as a denominator (the most
     * common nullary methods are constants like @ring.one@ that
     * downstream arithmetic expects to be non-zero). */
    (void)f; (void)a;
    return 1;
}
int64_t class_method_call__3(int64_t f, int64_t a, int64_t b) {
    /* class_method_call(TCI, MethodIdx, x) — unary method.  Return
     * the last arg (x) — treats @ring.negate@, @ring.abs@, etc. as
     * identity.  Wrong but non-zero, avoiding divide-by-zero cascades. */
    (void)f; (void)a;
    return b;
}

/* Mercury list.foldl(F, L, A0) = A — typeclass dispatch prepends two
 * TypeInfo args, so the bridge sees 5 inputs (TI1, TI2, F, L, A0). */
int64_t list_foldl(int64_t ti1, int64_t ti2,
                   int64_t f, int64_t list, int64_t acc) {
    (void)ti1; (void)ti2;
    return kk_list_foldl(list, acc, f);
}

/* list.map(F, L) — 4 inputs after the two TypeInfo args. */
int64_t list_map(int64_t ti1, int64_t ti2, int64_t f, int64_t list) {
    (void)ti1; (void)ti2;
    return kk_list_map(list, f);
}

/* list.filter(P, L) — 3 inputs (one TypeInfo, the pred, the list). */
int64_t list_filter(int64_t ti, int64_t p, int64_t list) {
    (void)ti;
    return kk_list_filter(list, p);
}

/* list.length(L) — 2 inputs (TypeInfo + the list). */
int64_t list_length(int64_t ti, int64_t list) {
    (void)ti;
    return kk_list_length(list);
}

/* list.append(L1, L2) = L3 — concatenate two lists. */
int64_t list_append(int64_t ti, int64_t l1, int64_t l2) {
    (void)ti;
    /* Build reversed l1 then prepend onto l2. */
    int64_t rev = kk_nil();
    while (kk_is_heap_ptr(l1) && kk_tag(l1) == KK_CONS_TAG) {
        rev = kk_cons(kk_field(l1, 0), rev);
        l1 = kk_field(l1, 1);
    }
    int64_t r = l2;
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        r = kk_cons(kk_field(rev, 0), r);
        rev = kk_field(rev, 1);
    }
    return r;
}

/* list.reverse(L) = L' — already provided as kk_list_reverse. */
int64_t list_reverse(int64_t ti, int64_t list) {
    (void)ti;
    /* Iterative reverse via cons. */
    int64_t r = kk_nil();
    while (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        r = kk_cons(kk_field(list, 0), r);
        list = kk_field(list, 1);
    }
    return r;
}

/* list.condense(LL) = L — flatten a list of lists.  Walks the outer
 * list and concatenates each element list via list.append. */
int64_t list_condense(int64_t ti, int64_t lol) {
    (void)ti;
    int64_t r = kk_nil();
    /* Walk outer list, collecting cells in reverse. */
    int64_t rev = kk_nil();
    while (kk_is_heap_ptr(lol) && kk_tag(lol) == KK_CONS_TAG) {
        int64_t inner = kk_field(lol, 0);
        while (kk_is_heap_ptr(inner) && kk_tag(inner) == KK_CONS_TAG) {
            rev = kk_cons(kk_field(inner, 0), rev);
            inner = kk_field(inner, 1);
        }
        lol = kk_field(lol, 1);
    }
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        r = kk_cons(kk_field(rev, 0), r);
        rev = kk_field(rev, 1);
    }
    return r;
}

/* list.duplicate(N, X) = L — list with N copies of X. */
int64_t list_duplicate(int64_t ti, int64_t n, int64_t x) {
    (void)ti;
    int64_t r = kk_nil();
    while (n > 0) { r = kk_cons(x, r); n--; }
    return r;
}

/* list.last(L) = X — return the last element (or 0 if empty).
 *
 * Retain the returned element: it lives inside the list cell, sharing the
 * list's reference.  Without an explicit retain, the caller's drop of the
 * returned value also frees the element from inside the list — observed
 * in surd's @lead_coeff(poly([1/1]), LC) :- Cs = [_|_], list.last(Cs, LC)@
 * called from a SEMIDET-WITH-OUTPUT pred whose translation discards the
 * test phase's bound output via @kk_drop@.  The drop hit the only
 * remaining ref of the shared rational, freeing it; the body phase then
 * returned the freed pointer to its caller, which @safe_rational_div@
 * saw as a malformed rational and substituted with @0/1@ — yielding
 * surd-elliptic's "0 · F(...)" leading-coefficient bug. */
int64_t list_last(int64_t ti, int64_t list) {
    (void)ti;
    int64_t last = 0;
    while (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        last = kk_field(list, 0);
        list = kk_field(list, 1);
    }
    if (last != 0) kk_retain(last);
    return last;
}

/* list.det_last(L) = X — same as list.last but det (aborts on empty);
 * since the bridge doesn't expose aborting, just return 0 on empty.
 * Same shared-element retain as list_last. */
int64_t list_det_last(int64_t ti, int64_t list) {
    return list_last(ti, list);
}


/* list.member(X, L) — semidet, returns 1 if X appears in L.  Caller
 * provides typeinfo + element + list (3 args). */
int64_t list_member(int64_t ti, int64_t x, int64_t list) {
    (void)ti;
    while (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        if (kk_structural_eq(kk_field(list, 0), x)) return 1;
        list = kk_field(list, 1);
    }
    return 0;
}

/* list.index0(L, N) = X — return Nth element (0-indexed). */
int64_t list_det_index0(int64_t ti, int64_t list, int64_t n) {
    (void)ti;
    while (n > 0 && kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        list = kk_field(list, 1);
        n--;
    }
    if (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        /* Retain: see list_last for the rationale — the element is shared
         * with the list cell, and the caller's drop of the returned value
         * must not free memory the list still references. */
        int64_t elem = kk_field(list, 0);
        if (elem != 0) kk_retain(elem);
        return elem;
    }
    return 0;
}

/* list.det_replace_nth(L, N, X) = L' — replace Nth element with X.
 * Mercury convention: N is 1-INDEXED (the Nth element is counted from 1),
 * NOT 0-indexed.  Callers like poly.add_at_index compute @N = Idx + 1@
 * specifically because they treat det_replace_nth as 1-based; if we
 * treat it as 0-based, the replacement falls one slot to the right and
 * usually walks off the end of short accumulator lists — the product
 * never updates and mul_accumulate yields the original zero-seeded
 * accumulator, collapsing every poly.mul result to poly([]). */
int64_t list_det_replace_nth(int64_t ti, int64_t list, int64_t n, int64_t x) {
    (void)ti;
    int64_t rev = kk_nil();
    int64_t i = 1;
    while (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        int64_t elem = (i == n) ? x : kk_field(list, 0);
        rev = kk_cons(elem, rev);
        list = kk_field(list, 1);
        i++;
    }
    int64_t r = kk_nil();
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        r = kk_cons(kk_field(rev, 0), r);
        rev = kk_field(rev, 1);
    }
    return r;
}

/* list.filter_map(F, L) = L' — Mercury's filter_map for a semidet
 * func/pred returns the OUTPUT VALUE R directly (NOT @yes(R)@); on
 * failure the bridge's closure returns 0 / a non-heap sentinel.
 * Include the result directly when it's a heap pointer; skip on 0 /
 * unboxed sentinel.  The previous version extracted
 * @kk_field(result, 0)@ on the assumption the closure wrapped its
 * output in @yes(R)@, which cons'd the numerator of a 2-field
 * rational instead of the rational itself — breaking
 * @make_candidates@ downstream (the integer numerator was later
 * deconstructed as @r(N, D)@, reading garbage D, hitting
 * @rational_norm@'s div-by-zero crash in @poly.div_mod@). */
int64_t list_filter_map(int64_t ti1, int64_t ti2, int64_t closure, int64_t list) {
    (void)ti1; (void)ti2;
    int64_t rev = kk_nil();
    while (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        int64_t result = kk_call_closure_1(closure, kk_field(list, 0));
        if (kk_is_heap_ptr(result)) {
            rev = kk_cons(result, rev);
        }
        list = kk_field(list, 1);
    }
    int64_t r = kk_nil();
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        r = kk_cons(kk_field(rev, 0), r);
        rev = kk_field(rev, 1);
    }
    return r;
}

/* list.sort(L) = L' — simple insertion sort (uses kk_structural_eq
 * for ordering, which compares heap pointers/values lexicographically
 * for non-string heap cells — good enough for many surd usages but
 * NOT a real semantic sort). */
/* `list.sort/3 (TypeInfo, Cmp, List)` — sort a list using the
 * user-supplied comparator closure.  The Mercury bridge dispatches
 * the explicit-comparator form here (vs. list_sort/2 which uses
 * the typeinfo's default compare).  The comparator returns a
 * Mercury comparison_result cell; by the bridge's lookupConTag
 * convention, ctors are tagged in declaration order, so the
 * "less than" tag is strictly smaller than the "greater than"
 * tag.  We discover the lt-tag lazily on the first non-equal
 * comparison.  Retains cmp and elements per call because
 * Perceus-translated closure bodies consume their args. */
static int64_t kk_cmp_lt_tag_cached = -1;
int64_t list_sort__3(int64_t ti, int64_t cmp, int64_t list) {
    (void)ti;
    int64_t n = 0; int64_t tmp = list;
    while (kk_is_heap_ptr(tmp) && kk_tag(tmp) == KK_CONS_TAG) {
        n++; tmp = kk_field(tmp, 1);
    }
    if (n <= 1) return list;
    int64_t* arr = (int64_t*)malloc(sizeof(int64_t) * (size_t)n);
    if (!arr) return list;
    int64_t i = 0; tmp = list;
    while (kk_is_heap_ptr(tmp) && kk_tag(tmp) == KK_CONS_TAG) {
        arr[i++] = kk_field(tmp, 0); tmp = kk_field(tmp, 1);
    }
    int64_t lt_tag = kk_cmp_lt_tag_cached;
    for (i = 0; i < n - 1; i++) {
        for (int64_t j = 0; j < n - 1 - i; j++) {
            kk_retain(cmp);
            kk_retain(arr[j]);
            kk_retain(arr[j+1]);
            int64_t r1 = kk_call_closure_2(cmp, arr[j], arr[j+1]);
            int64_t t1 = kk_is_heap_ptr(r1) ? kk_tag(r1) : r1;
            int swap = 0;
            if (lt_tag >= 0) {
                if (t1 != lt_tag) {
                    /* Could still be Eq; check reverse. */
                    kk_retain(cmp);
                    kk_retain(arr[j]);
                    kk_retain(arr[j+1]);
                    int64_t r2 = kk_call_closure_2(cmp, arr[j+1], arr[j]);
                    int64_t t2 = kk_is_heap_ptr(r2) ? kk_tag(r2) : r2;
                    if (t2 == lt_tag) swap = 1;
                }
            } else {
                /* Discover lt-tag from a reverse call. */
                kk_retain(cmp);
                kk_retain(arr[j]);
                kk_retain(arr[j+1]);
                int64_t r2 = kk_call_closure_2(cmp, arr[j+1], arr[j]);
                int64_t t2 = kk_is_heap_ptr(r2) ? kk_tag(r2) : r2;
                if (t1 != t2) {
                    lt_tag = t1 < t2 ? t1 : t2;
                    kk_cmp_lt_tag_cached = lt_tag;
                    if (t2 == lt_tag) swap = 1;
                }
            }
            if (swap) {
                int64_t t = arr[j]; arr[j] = arr[j+1]; arr[j+1] = t;
            }
        }
    }
    int64_t r = kk_nil();
    for (i = n - 1; i >= 0; i--) {
        kk_retain(arr[i]);
        r = kk_cons(arr[i], r);
    }
    free(arr);
    return r;
}

int64_t list_sort(int64_t ti, int64_t list) {
    (void)ti;
    /* Default comparison: bubble sort by raw i64 value (proxy
     * ordering).  Use for the typeinfo-driven Mercury `list.sort/2`
     * form; the explicit-comparator form `list.sort/3` is handled
     * by list_sort__3 above. */
    int64_t n = 0; int64_t tmp = list;
    while (kk_is_heap_ptr(tmp) && kk_tag(tmp) == KK_CONS_TAG) {
        n++; tmp = kk_field(tmp, 1);
    }
    if (n <= 1) return list;
    int64_t* arr = (int64_t*)malloc(sizeof(int64_t) * (size_t)n);
    if (!arr) return list;
    int64_t i = 0; tmp = list;
    while (kk_is_heap_ptr(tmp) && kk_tag(tmp) == KK_CONS_TAG) {
        arr[i++] = kk_field(tmp, 0); tmp = kk_field(tmp, 1);
    }
    /* Bubble sort by raw i64 value. */
    for (i = 0; i < n - 1; i++) {
        for (int64_t j = 0; j < n - 1 - i; j++) {
            if (arr[j] > arr[j+1]) {
                int64_t t = arr[j]; arr[j] = arr[j+1]; arr[j+1] = t;
            }
        }
    }
    int64_t r = kk_nil();
    for (i = n - 1; i >= 0; i--) {
        kk_retain(arr[i]);
        r = kk_cons(arr[i], r);
    }
    free(arr);
    return r;
}

/* list.foldl2(F, L, A0, B0) = (A, B) — fold with TWO accumulators.
 * F : (elem, A, B) -> (A', B').  Returns a 2-tuple of finals.
 * 8 args after typeclass dispatch: (ti1, ti2, ti3, ti4, F, L, A0, B0). */
int64_t list_foldl2(int64_t ti1, int64_t ti2, int64_t ti3, int64_t ti4,
                    int64_t closure, int64_t list,
                    int64_t a, int64_t b) {
    (void)ti1; (void)ti2; (void)ti3; (void)ti4;
    /* Bridge currently can't represent a 2-tuple-output closure cleanly;
     * approximate by treating F as `(elem, A) -> A` (collapsing B). */
    while (kk_is_heap_ptr(list) && kk_tag(list) == KK_CONS_TAG) {
        a = kk_call_closure_2(closure, kk_field(list, 0), a);
        list = kk_field(list, 1);
    }
    (void)b;
    int64_t pair = kk_alloc_con(0, 2);
    kk_set_field(pair, 0, a);
    kk_set_field(pair, 1, b);
    return pair;
}

/* Generic 3-arg HO dispatch. */
int64_t class_method_call__4(int64_t f, int64_t a, int64_t b, int64_t c) {
    /* class_method_call(TCI, MethodIdx, x, y) — binary method.
     * Return the FIRST non-TCI arg (x) as a non-zero sentinel that
     * lets the program advance without divide-by-zero.  Wrong for
     * ring_mul / ring_add but at least progresses. */
    (void)f; (void)a; (void)c;
    return b;
}
int64_t apply__4(int64_t f, int64_t a, int64_t b, int64_t c) {
    return kk_call_closure_3(f, a, b, c);
}
int64_t call__4(int64_t f, int64_t a, int64_t b, int64_t c) {
    return kk_call_closure_3(f, a, b, c);
}

/* `list.(L1 ++ L2)` — sanitises to `list_zpzp` (`+` → `zp`, twice).
 * Same semantics as `list.append`; route through list_append. */
int64_t list_zpzp(int64_t ti, int64_t l1, int64_t l2) {
    return list_append(ti, l1, l2);
}

/* `string.(S1 ++ S2)` — string concat.  Build a CONCAT rope. */
extern int64_t kk_str_concat(int64_t a, int64_t b);
int64_t string_zpzp(int64_t s1, int64_t s2) {
    return kk_str_concat(s1, s2);
}

/* `char.det_from_int(N)` / `char.to_int(C)` — in the i64 model, both
 * collapse to identity (chars are stored as their codepoint i64). */
int64_t char_det_from_int(int64_t n) { return n; }
int64_t char_to_int(int64_t c)        { return c; }

/* Helper: returns 1 if msg_i's bytes contain "division by zero". */
static int kk_msg_is_div_by_zero(int64_t msg_i) {
    if (msg_i == 0) return 0;
    int64_t flat = kk_str_flatten(msg_i);
    const char *bytes = kk_str_bytes((kk_string_t*)flat);
    if (bytes == NULL) return 0;
    return strstr(bytes, "division by zero") != NULL ? 1 : 0;
}

/* Mercury require.error/1 — fatal error with a message.  Prints the
 * message and exits with status 1.  Used by surd's rational_norm
 * when the denominator is zero. */
int64_t require_error(int64_t msg_i) {
    kk_print_str(msg_i);
    fputc('\n', stdout);
    fflush(stdout);
    exit(1);
    return 0;  /* unreachable */
}

/* Mercury require.func_error/3 — fatal error with type info,
 * function-name string, and message.  Surd's rational_norm /
 * rational.reciprocal call this via the 3-arg form
 * `require.func_error(TypeCtorInfo, FnName, Msg)`.  Normally exits
 * with status 1.  For "division by zero" messages, silently return
 * 0 (the bridge's semidet-failure sentinel / "no value") so e.g.
 * monic_poly's reciprocal-of-zero path can survive and produce a
 * sensible 0 result instead of crashing the whole demo when the
 * input polynomial has a malformed (NULL) leading coefficient from
 * upstream multi-output pred handling. */
int64_t require_func_error(int64_t tci, int64_t fn_i, int64_t msg_i) {
    (void)tci;
    if (kk_msg_is_div_by_zero(msg_i)) return 0;
    fputs("error: ", stdout);
    if (fn_i != 0) kk_print_str(fn_i);
    fputs(": ", stdout);
    if (msg_i != 0) kk_print_str(msg_i);
    fputc('\n', stdout);
    fflush(stdout);
    exit(1);
    return 0;  /* unreachable */
}

/* Mercury integer.integer/1 — wraps an int as an integer.  In the
 * i64-based model the wrap is identity. */
int64_t integer_integer(int64_t x) { return x; }

/* Atom "rational" — appears as a 0-arg arg to type_ctor_info inside
 * rational_norm's division-by-zero error path that the smoke test
 * never reaches.  Stub returns 0 so the link resolves. */
int64_t rational(void) { return 0; }

/* safe_from_integers(N, D) — replacement for surd's
 * @rational.from_integers/2@ that returns the 0 sentinel when D=0
 * instead of crashing in @rational_norm@'s div-by-zero @error@ path.
 * Surd's @factoring.make_candidates@ has a @not is_zero(D)@ guard
 * before this call in its inner semidet closure, but the bridge
 * currently compiles the guard as a no-op (see GoalNot CPS terminator),
 * so the body runs for D=0 too.  Routing @from_integers@ here via
 * @rewriteTypeclassMethod@ defuses that path without disturbing
 * downstream poly arithmetic (the same workaround applied at
 * @rational_norm@ globally caused elliptic regressions because
 * legitimate norm callers also saw the 0 sentinel and looped).
 *
 * The rational ctor "r" has stable tag @stableConTag("r") = 46645@
 * (djb2 hash of "r" mod 65521); the bridge's Core.ConTags pass
 * computes the same value for every compilation. */
#define KK_RATIONAL_R_TAG 46645

static int64_t kk_i64_abs(int64_t x) { return x < 0 ? -x : x; }
static int64_t kk_i64_gcd(int64_t a, int64_t b) {
    a = kk_i64_abs(a); b = kk_i64_abs(b);
    while (b != 0) { int64_t t = a % b; a = b; b = t; }
    return a == 0 ? 1 : a;
}

/* __int128 helpers — used by the rational arithmetic shims below to keep
 * intermediate products safe past the i64 budget that Mercury's
 * arbitrary-precision @integer@ takes for granted.  Surd's elliptic-integral
 * reduction runs poly.bisect_root for 50 halvings; each iteration roughly
 * doubles the rational denominators, so by ~30 iterations the
 * @aD * bD@ product in @rational.+@ / @rational.*@ exceeds i64 and the
 * result silently becomes 0, tripping @rational.rational_norm@'s Den=0
 * guard.  Computing in __int128 and rescaling lossy-by-2 once the reduced
 * result no longer fits i64 preserves the bisection's sign-of-eval
 * discipline (the only thing approx_roots needs) at the cost of bounded
 * precision loss in deep iterations. */
typedef __int128 i128_t;
static i128_t kk_i128_abs(i128_t x) { return x < 0 ? -x : x; }
static i128_t kk_i128_gcd(i128_t a, i128_t b) {
    a = kk_i128_abs(a); b = kk_i128_abs(b);
    while (b != 0) { i128_t t = a % b; a = b; b = t; }
    return a == 0 ? 1 : a;
}

/* Normalize an i128 num/den fraction: make den positive, reduce by gcd,
 * and rescale (lossily) by powers of 2 until both num and den fit i64.
 * Sets *out_num and *out_den to the resulting i64 values. */
static void kk_i128_normalize_to_i64(i128_t num, i128_t den,
                                      int64_t *out_num, int64_t *out_den) {
    if (den == 0) { *out_num = 0; *out_den = 1; return; }
    if (den < 0) { num = -num; den = -den; }
    i128_t g = kk_i128_gcd(num, den);
    num /= g;
    den /= g;
    /* Lossy rescale to fit i64 (sign-preserving).  Keeps the magnitude
     * approximately correct; deep bisection iterations lose precision
     * but sign-of-eval — the bisection's discriminator — stays valid. */
    while (num > (i128_t)INT64_MAX || num < -(i128_t)INT64_MAX
        || den > (i128_t)INT64_MAX) {
        num >>= 1;
        den >>= 1;
        if (den == 0) { den = 1; break; }
    }
    *out_num = (int64_t)num;
    *out_den = (int64_t)den;
}

static int64_t kk_alloc_rational_from_i128(i128_t num, i128_t den) {
    int64_t n64, d64;
    kk_i128_normalize_to_i64(num, den, &n64, &d64);
    int64_t cell = kk_alloc_con(KK_RATIONAL_R_TAG, 2);
    kk_set_field(cell, 0, n64);
    kk_set_field(cell, 1, d64);
    return cell;
}

int64_t safe_from_integers__2(int64_t n, int64_t d) {
    if (d == 0) return 0;  /* sentinel — caller (list.filter_map) skips */
    int64_t cell = kk_alloc_con(KK_RATIONAL_R_TAG, 2);
    if (n == 0) {
        kk_set_field(cell, 0, 0);
        kk_set_field(cell, 1, 1);
        return cell;
    }
    int64_t g = kk_i64_gcd(n, d);
    int64_t sign_d = d < 0 ? -1 : 1;
    int64_t num = (n / g) * sign_d;
    int64_t den = kk_i64_abs(d) / g;
    kk_set_field(cell, 0, num);
    kk_set_field(cell, 1, den);
    return cell;
}

/* Defensive shim for rational.* (multiplication).  If either operand
 * has denominator 0 (a malformed rational that shouldn't exist but
 * does appear in surd's factoring path on euler example 6 — an
 * r(0, 0) cell with the right tag but field 1 = 0 flows into
 * @poly.div_mod@'s @ring_mul@ inner loop), substitute @r(0, 1)@ and
 * keep going.  Sound rationals get the normal r(An,Ad)*r(Bn,Bd)
 * computation via @rational_norm@ logic (replicated in C). */
static int kk_rational_is_malformed(int64_t r) {
    if (!kk_is_heap_ptr(r)) return 1;
    if (kk_tag(r) != KK_RATIONAL_R_TAG) return 1;
    if (kk_nfields(r) < 2) return 1;
    if (kk_field(r, 1) == 0) return 1;
    return 0;
}

int64_t safe_rational_mul__2(int64_t a, int64_t b) {
    if (kk_rational_is_malformed(a) || kk_rational_is_malformed(b)) {
        return safe_from_integers__2(0, 1);
    }
    int64_t aN = kk_field(a, 0), aD = kk_field(a, 1);
    int64_t bN = kk_field(b, 0), bD = kk_field(b, 1);
    /* Compute in i128 then normalize+rescale to i64.  The old per-factor
     * gcd pre-reduction (aN/g1)*(bN/g2) survives most surd arithmetic but
     * still overflows on the bisect_root path's deep iterations. */
    i128_t num = (i128_t)aN * (i128_t)bN;
    i128_t den = (i128_t)aD * (i128_t)bD;
    int64_t n64, d64;
    kk_i128_normalize_to_i64(num, den, &n64, &d64);
    return safe_from_integers__2(n64, d64);
}

/* Mercury @rational.+/2@ via i128 intermediates.  Reduce in i128 and
 * route the i64-resized result through safe_from_integers__2 for
 * consistency with mul (which retains the same allocation discipline). */
int64_t safe_rational_add__2(int64_t a, int64_t b) {
    if (kk_rational_is_malformed(a) || kk_rational_is_malformed(b)) {
        return safe_from_integers__2(0, 1);
    }
    int64_t aN = kk_field(a, 0), aD = kk_field(a, 1);
    int64_t bN = kk_field(b, 0), bD = kk_field(b, 1);
    i128_t num = (i128_t)aN * (i128_t)bD + (i128_t)bN * (i128_t)aD;
    i128_t den = (i128_t)aD * (i128_t)bD;
    int64_t n64, d64;
    kk_i128_normalize_to_i64(num, den, &n64, &d64);
    return safe_from_integers__2(n64, d64);
}

/* Mercury @rational.-/2@ (binary subtract).  Some surd sites also call
 * @rational.-/1@ (unary negate) — that path stays on the user-defined
 * Mercury impl since negating r(An, Ad) only flips the sign of An (no
 * arithmetic, no overflow). */
int64_t safe_rational_sub__2(int64_t a, int64_t b) {
    if (kk_rational_is_malformed(a) || kk_rational_is_malformed(b)) {
        return safe_from_integers__2(0, 1);
    }
    int64_t aN = kk_field(a, 0), aD = kk_field(a, 1);
    int64_t bN = kk_field(b, 0), bD = kk_field(b, 1);
    i128_t num = (i128_t)aN * (i128_t)bD - (i128_t)bN * (i128_t)aD;
    i128_t den = (i128_t)aD * (i128_t)bD;
    return kk_alloc_rational_from_i128(num, den);
}

/* Mercury @rational./2@.  Substitutes the rational-zero sentinel when
 * dividing by zero (matches existing surd safe-shim convention). */
int64_t safe_rational_div__2(int64_t a, int64_t b) {
    if (kk_rational_is_malformed(a) || kk_rational_is_malformed(b)) {
        return safe_from_integers__2(0, 1);
    }
    int64_t aN = kk_field(a, 0), aD = kk_field(a, 1);
    int64_t bN = kk_field(b, 0), bD = kk_field(b, 1);
    if (bN == 0) return safe_from_integers__2(0, 1);
    i128_t num = (i128_t)aN * (i128_t)bD;
    i128_t den = (i128_t)aD * (i128_t)bN;
    return kk_alloc_rational_from_i128(num, den);
}

/* surd's @rad_normalize.extract_nth_power(N, M, Extracted, Remainder)@
 * decomposes a positive integer M into Extracted^N * Remainder where
 * Extracted is the largest Nth power dividing M.  Surd's body uses
 * @list.foldl2@ to thread two accumulators through the prime
 * factorization, but the bridge's HO closure model can't bind both
 * outputs cleanly — only one accumulator survives, the other surfaces
 * as a default 0, type-confusion cascades into @rational_norm@ deep
 * in extract_root_lit.  Implement the decomposition directly in C
 * (trial-division factorization, accumulate P^(E//N) into Extracted
 * and P^(E%N) into Remainder).  Returns the @Extracted@ rational;
 * the bridge supplies @Remainder@ via a separate
 * @extract_nth_power_remainder@ call (wrapSecondaries).
 *
 * Inputs are i64 (the bridge's integer model); outputs are
 * heap-allocated rationals with the standard "r" tag. */
static int64_t kk_i64_pow(int64_t base, int64_t exp) {
    int64_t r = 1;
    while (exp-- > 0) r *= base;
    return r;
}

static void kk_extract_nth_factor(int64_t n, int64_t m,
                                   int64_t *ext, int64_t *rem) {
    /* M = product over primes p of p^E.
     * Extracted *= p^(E/N), Remainder *= p^(E%N). */
    int64_t extracted = 1, remainder = 1;
    if (m <= 1) { *ext = 1; *rem = 1; return; }
    int64_t p = 2;
    while (p * p <= m) {
        int64_t e = 0;
        while (m % p == 0) { m /= p; e++; }
        if (e > 0) {
            extracted *= kk_i64_pow(p, e / n);
            remainder *= kk_i64_pow(p, e % n);
        }
        p = (p == 2) ? 3 : p + 2;
    }
    if (m > 1) {
        /* m itself is a prime factor with exponent 1. */
        remainder *= m;  /* e=1, e/N=0, e%N=1 (assuming N>=1) */
    }
    *ext = extracted;
    *rem = remainder;
}

int64_t extract_nth_power_extracted__2(int64_t n, int64_t m) {
    int64_t ext, rem;
    kk_extract_nth_factor(n, kk_i64_abs(m), &ext, &rem);
    return safe_from_integers__2(ext, 1);
}

int64_t extract_nth_power_remainder__2(int64_t n, int64_t m) {
    int64_t ext, rem;
    kk_extract_nth_factor(n, kk_i64_abs(m), &ext, &rem);
    return safe_from_integers__2(rem, 1);
}

/* surd's @rad_normalize.partition_lits(L, Lits, Rest)@ splits a list
 * of rad_exprs into two lists: rational literals extracted from
 * @re_lit(R)@ / @re_inv(re_lit(R))@ cells (collected as a list of
 * rationals), and the remaining non-literal rad_exprs.  It's a det
 * pred with TWO output positions; the bridge's call-site
 * trailing-output-drop heuristic only binds the FIRST output to the
 * call result, default-binding subsequent outputs to 0.  The Rest
 * list surfaced as NULL, then @build_mul(NULL)@ returned NULL,
 * @apply_coeff(C, NULL)@ produced @re_mul(re_lit(C), NULL)@, and
 * @pretty.pretty_prec@ rendered the NULL field as "?" — visible
 * in surd-euler example 6's @1/?·ln|…2·?…|@ output where the `?`
 * should be @√2@.
 *
 * Provide both outputs via two runtime stubs: the primary call
 * binds Lits; the bridge's @wrapSecondaries@ supplies Rest via the
 * @_rest@ stub.  Implementation iterates the input list once and
 * partitions; both stubs share the same logic but only return the
 * relevant half.  Refcount-naive: the input list is read twice
 * (once per stub).  Acceptable for surd's usage. */
#define KK_RE_LIT_TAG 31636   /* djb2("re_lit") mod 65521 — runtime
                                  needs to know this to deconstruct */
#define KK_RE_INV_TAG 28701   /* djb2("re_inv") */

static int kk_compute_tag(const char* name) {
    int64_t acc = 5381;
    while (*name) acc = acc * 33 + (unsigned char)*name++;
    if (acc < 0) acc = -acc;
    return (int)(acc % 65521);
}

static int kk_re_lit_tag(void) {
    static int tag = -1;
    if (tag < 0) tag = kk_compute_tag("re_lit");
    return tag;
}

static int kk_re_inv_tag(void) {
    static int tag = -1;
    if (tag < 0) tag = kk_compute_tag("re_inv");
    return tag;
}

/* Returns 1 if E is re_lit(R) and writes R to *out_r.
 * Returns 2 if E is re_inv(re_lit(R)) and writes R to *out_r (caller
 *   should compute 1/R if needed; we set out_r to the inner R only).
 * Returns 0 otherwise. */
static int kk_classify_rad_lit(int64_t e, int64_t *out_r) {
    if (!kk_is_heap_ptr(e)) return 0;
    int64_t t = kk_tag(e);
    if (t == kk_re_lit_tag()) {
        *out_r = kk_field(e, 0);
        return 1;
    }
    if (t == kk_re_inv_tag()) {
        int64_t inner = kk_field(e, 0);
        if (kk_is_heap_ptr(inner) && kk_tag(inner) == kk_re_lit_tag()) {
            *out_r = kk_field(inner, 0);
            return 2;
        }
    }
    return 0;
}

/* Build the Lits list (list of rationals) from L. */
int64_t partition_lits_lits__1(int64_t list) {
    /* Reverse-accumulate into rev, then reverse to preserve order. */
    int64_t rev = kk_nil();
    int64_t cur = list;
    while (kk_is_heap_ptr(cur) && kk_tag(cur) == KK_CONS_TAG) {
        int64_t h = kk_field(cur, 0);
        int64_t r;
        int klass = kk_classify_rad_lit(h, &r);
        if (klass == 1) {
            rev = kk_cons(r, rev);
        } else if (klass == 2) {
            /* re_inv(re_lit(R)) — Lits adds 1/R.  Need rational division;
             * use safe_from_integers__2 to construct 1/R = r(D, N) for
             * R = r(N, D) (assuming R is non-zero; surd's partition_lits
             * guards with `not R = rational.zero`, but for D=0 in the
             * stored rational we just skip to avoid div-by-zero). */
            int64_t rN = kk_field(r, 0);
            int64_t rD = kk_field(r, 1);
            if (rN != 0) {
                int64_t inv = safe_from_integers__2(rD, rN);
                rev = kk_cons(inv, rev);
            }
        }
        cur = kk_field(cur, 1);
    }
    /* Reverse rev to preserve original order. */
    int64_t out = kk_nil();
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        out = kk_cons(kk_field(rev, 0), out);
        rev = kk_field(rev, 1);
    }
    return out;
}

/* Build the Rest list (list of non-literal rad_exprs) from L. */
int64_t partition_lits_rest__1(int64_t list) {
    int64_t rev = kk_nil();
    int64_t cur = list;
    while (kk_is_heap_ptr(cur) && kk_tag(cur) == KK_CONS_TAG) {
        int64_t h = kk_field(cur, 0);
        int64_t r;
        int klass = kk_classify_rad_lit(h, &r);
        if (klass == 0) {
            rev = kk_cons(h, rev);
        }
        /* For re_inv(re_lit(zero)) we'd want to keep it in Rest too;
         * approximate by always treating klass != 0 as "literal" and
         * dropping.  Surd guards inv(re_lit(0)) so it doesn't appear. */
        cur = kk_field(cur, 1);
    }
    int64_t out = kk_nil();
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        out = kk_cons(kk_field(rev, 0), out);
        rev = kk_field(rev, 1);
    }
    return out;
}

/* Mercury io.format/4 — after the bridge's output-arg-drop, the call
 * shape is io_format(Fmt, Args, IO).  Fmt is a kk_string_t pointer.
 * Args is the head of a list built from list_cons cells with
 * poly_type-wrapped values (each element is a 1-field ctor for
 * s/i/f/c wrapping its value).  Walks Fmt char by char, substituting
 * each %X spec with the next list element.  Other chars print
 * verbatim — Mercury's HLDS already decoded escape sequences at
 * parse time. */
int64_t io_format(int64_t fmt_i, int64_t args_list, int64_t io_state) {
    if (fmt_i == 0) return io_state;
    kk_string_t* s = (kk_string_t*)fmt_i;
    int64_t n = s->byte_len;
    if (n <= 0) return io_state;
    char* buf = (char*)malloc((size_t)n);
    if (!buf) return io_state;
    char* p = buf;
    /* Flatten rope into the buffer; leaves already give direct bytes. */
    {
        kk_string_t* st[64];
        int top = 0;
        st[top++] = s;
        while (top > 0) {
            kk_string_t* cur = st[--top];
            if (cur->kind == KK_STR_LEAF) {
                const char* bytes = cur->u.bytes;
                for (int64_t i = 0; i < cur->byte_len; i++) *p++ = bytes[i];
            } else if (cur->kind == KK_STR_SLICE) {
                /* For SLICE, u.cat.r is reused to hold the bytes pointer. */
                const char* bytes = (const char*)cur->u.cat.r;
                for (int64_t i = 0; i < cur->byte_len; i++) *p++ = bytes[i];
            } else {
                /* CONCAT — push right first so left is processed first. */
                if (top + 2 > 64) break;
                st[top++] = cur->u.cat.r;
                st[top++] = cur->u.cat.l;
            }
        }
    }

    int64_t cur_args = args_list;
    for (int64_t i = 0; i < n; i++) {
        char c = buf[i];
        if (c == '%' && i + 1 < n) {
            /* Parse extended spec: %[flags][width][.precision]<conv>.
             * Handles Mercury's @io.format("%.10f", ...)@ style. */
            int64_t j = i + 1;
            while (j < n && (buf[j] == '-' || buf[j] == '+' || buf[j] == ' '
                          || buf[j] == '#' || buf[j] == '0')) j++;
            while (j < n && buf[j] >= '0' && buf[j] <= '9') j++;
            if (j < n && buf[j] == '.') {
                j++;
                while (j < n && buf[j] >= '0' && buf[j] <= '9') j++;
            }
            if (j >= n) { putchar(c); continue; }
            char spec = buf[j];
            if (spec == '%') {
                putchar('%');
                i = j;
                continue;
            }
            /* Pop next element from the list (cons cell, 2 fields). */
            if (cur_args != 0 && kk_is_heap_ptr(cur_args)
                && kk_nfields(cur_args) == 2) {
                int64_t head = kk_field(cur_args, 0);
                int64_t tail = kk_field(cur_args, 1);
                /* Poly_type wrapper — 1 field holding the inner value. */
                int64_t val =
                    (kk_is_heap_ptr(head) && kk_nfields(head) >= 1)
                    ? kk_field(head, 0) : head;
                /* Build the substitution format string [i..j] inclusive,
                 * then dispatch via printf for proper width/precision. */
                int64_t spec_len = j - i + 1;
                char fmt_buf[32];
                if (spec_len < (int64_t)sizeof(fmt_buf)) {
                    memcpy(fmt_buf, buf + i, (size_t)spec_len);
                    fmt_buf[spec_len] = '\0';
                    switch (spec) {
                      case 's':
                        /* %s with width/precision: kk_print_str doesn't
                         * support them; print raw if val nonzero. */
                        if (val != 0) kk_print_str(val);
                        break;
                      case 'd':
                      case 'i': {
                        /* Replace %d/%i with %lld for int64_t. */
                        char alt[34];
                        memcpy(alt, fmt_buf, (size_t)spec_len - 1);
                        alt[spec_len - 1] = 'l';
                        alt[spec_len]     = 'l';
                        alt[spec_len + 1] = spec;
                        alt[spec_len + 2] = '\0';
                        printf(alt, (long long)val);
                        break;
                      }
                      case 'f':
                      case 'e':
                      case 'g':
                      case 'E':
                      case 'G': {
                        double d;
                        memcpy(&d, &val, 8);
                        printf(fmt_buf, d);
                        break;
                      }
                      case 'c':
                        printf(fmt_buf, (int)val);
                        break;
                      default:
                        fwrite(fmt_buf, 1, (size_t)spec_len, stdout);
                        break;
                    }
                } else {
                    /* Fallback: write the raw spec text. */
                    fwrite(buf + i, 1, (size_t)spec_len, stdout);
                }
                cur_args = tail;
                i = j;
            } else {
                putchar(c);
            }
        } else {
            putchar(c);
        }
    }
    fflush(stdout);
    free(buf);
    return io_state;
}

int64_t mercury_collect_choices(int64_t fn_ptr) {
    typedef int64_t (*body_fn_t)(void);
    body_fn_t body = (body_fn_t)fn_ptr;

    /* Phase 1: discover max depth by running with all-0 decisions */
    memset(mercury_choice_decisions, 0, sizeof(mercury_choice_decisions));
    mercury_choice_pos = 0;
    body();
    int64_t max_depth = mercury_choice_pos;
    if (max_depth == 0) {
        /* No choices made — single deterministic result */
        mercury_choice_pos = 0;
        return body();
    }

    /* Phase 2: enumerate all 2^max_depth paths */
    int64_t total_paths = 1LL << max_depth;
    int64_t sum = 0;
    for (int64_t path = 0; path < total_paths; path++) {
        /* Set decisions from bits of path */
        for (int64_t i = 0; i < max_depth; i++) {
            mercury_choice_decisions[i] = (path >> i) & 1;
        }
        mercury_choice_pos = 0;
        int64_t result = body();
        int64_t depth = mercury_choice_pos;

        /* Only count if unused bits (beyond depth) are all zero.
         * This avoids double-counting short paths that are reached
         * by multiple path prefixes. */
        int64_t used_mask = (1LL << depth) - 1;
        if ((path & used_mask) == path) {
            sum += result;
        }
    }
    return sum;
}

/* Thunk support for lazy evaluation (Haskell/Idris bridges)
 *
 * Thunk layout (using kk_alloc_con with tag=0xLAZY):
 *   [refcount] [tag=0x4C415A59] [evaluated_flag] [closure_or_result]
 *
 * evaluated_flag: 0 = unevaluated (field 1 holds a closure cell —
 *                                  field 0 of the closure is the fn_ptr,
 *                                  remaining fields are captured values)
 *                 1 = evaluated   (field 1 holds the cached result)
 *
 * The closure-based encoding lets EDelay bodies capture enclosing-scope
 * variables.  An earlier zero-arg encoding forced eager evaluation, which
 * diverged on recursive lazy definitions (e.g. Idris2's `countFrom`).
 */

/* KK_THUNK_TAG defined at top of file */

/* Create a thunk wrapping a closure cell.  The closure must have a
 * function pointer at field 0; remaining fields are captured variables.
 * When forced, kk_thunk_force invokes `fn(closure)` to compute the
 * value, then caches it in field 1 and drops the closure. */
int64_t kk_thunk_create(int64_t closure_ptr) {
    int64_t thunk = kk_alloc_con(KK_THUNK_TAG, 2);
    if (thunk == 0) return 0;
    kk_set_field(thunk, 0, 0);            /* evaluated_flag = 0 */
    kk_set_field(thunk, 1, closure_ptr);  /* closure (owns its captures) */
    return thunk;
}

/* Create a pre-forced thunk wrapping an already-computed result.
 * Used when eager evaluation was necessary (e.g., the thunk body
 * captured variables from an enclosing scope).  kk_thunk_force
 * will see evaluated_flag=1 and return the cached result directly. */
int64_t kk_thunk_create_forced(int64_t result) {
    int64_t thunk = kk_alloc_con(KK_THUNK_TAG, 2);
    if (thunk == 0) return result;     /* fallback: return raw value */
    kk_retain(result);                 /* thunk cache holds a reference */
    kk_set_field(thunk, 0, 1);        /* evaluated_flag = 1 */
    kk_set_field(thunk, 1, result);   /* cached result */
    return thunk;
}

/* Force a thunk: if unevaluated, call the function and cache the result.
 *
 * Retain semantics: every call to kk_thunk_force returns a reference that
 * the caller "owns" (and will eventually drop via Perceus).  On the first
 * evaluation we retain the result because the thunk's cache now holds a
 * second reference.  On cache hits we retain again so the caller gets its
 * own reference.  When the thunk itself is freed (rc→0), kk_drop drops
 * the cached result, releasing the thunk's reference.  This correctly
 * handles GHC's selector thunks, where multiple selectors share one
 * cached pair and each drops it independently. */
int64_t kk_thunk_force(int64_t thunk) {
    if (!kk_is_heap_ptr(thunk)) return thunk;  /* not a thunk, return as-is */
    if (!kk_arena_maybe_owns((const void*)(intptr_t)thunk)) return thunk; /* not our heap */
    int64_t tag = kk_tag(thunk);
    if (tag != KK_THUNK_TAG) return thunk;     /* not a thunk, return as-is */
    int64_t evaluated = kk_field(thunk, 0);
    if (evaluated) {
        int64_t result = kk_field(thunk, 1);
        kk_retain(result);                     /* caller will drop this ref */
        if (getenv("KK_FORCE_TRACE")) {
            fprintf(stderr, "[force CACHED] thunk=%p → %p (tag=0x%lx)\n",
                    (void*)(uintptr_t)thunk, (void*)(uintptr_t)result,
                    kk_is_heap_ptr(result) ? *(int64_t*)result : 0L);
            fflush(stderr);
        }
        return result;
    }
    /* Unevaluated: field 1 holds a closure cell.  Invoke fn(closure). */
    int64_t closure = kk_field(thunk, 1);
    int64_t fn_ptr = kk_field(closure, 0);
    /* DIAGNOSTIC: dump closure fields before evaluation if KK_FORCE_TRACE is set
     * and fn is the suspicious lambda_p16_5152 (compile-time addr will vary). */
    if (getenv("KK_FORCE_TRACE")) {
        int64_t nf = kk_nfields(closure);
        fprintf(stderr, "[force NEW pre] thunk=%p closure=%p fn=%p nfields=%ld\n",
                (void*)(uintptr_t)thunk, (void*)(uintptr_t)closure,
                (void*)(uintptr_t)fn_ptr, (long)nf);
        for (int64_t i = 1; i < nf && i < 5; i++) {
            int64_t fi = kk_field(closure, i);
            int64_t fi_tag = kk_is_heap_ptr(fi) ? *(int64_t*)fi : 0;
            fprintf(stderr, "  closure.field[%ld] = %p (tag=0x%lx)",
                    (long)i, (void*)(uintptr_t)fi, fi_tag);
            /* If field is a CLOS, also dump its field[0] (fn ptr) and field[1] */
            if (fi_tag == 0x434C4F53 && kk_is_heap_ptr(fi)) {
                int64_t fi_nf = kk_nfields(fi);
                int64_t fi_f0 = kk_field(fi, 0);
                fprintf(stderr, " nf=%ld f0=%p", (long)fi_nf, (void*)(uintptr_t)fi_f0);
                if (fi_nf > 1) {
                    int64_t fi_f1 = kk_field(fi, 1);
                    int64_t fi_f1_tag = kk_is_heap_ptr(fi_f1) ? *(int64_t*)fi_f1 : 0;
                    fprintf(stderr, " f1=%p(tag=0x%lx)", (void*)(uintptr_t)fi_f1, fi_f1_tag);
                }
            }
            fprintf(stderr, "\n");
        }
        fflush(stderr);
    }
    typedef int64_t (*thunk_fn_t)(int64_t);
    int64_t result = ((thunk_fn_t)fn_ptr)(closure);
    if (getenv("KK_FORCE_TRACE")) {
        fprintf(stderr, "[force NEW post] thunk=%p closure=%p fn=%p → %p (tag=0x%lx)\n",
                (void*)(uintptr_t)thunk, (void*)(uintptr_t)closure,
                (void*)(uintptr_t)fn_ptr, (void*)(uintptr_t)result,
                kk_is_heap_ptr(result) ? *(int64_t*)result : 0L);
        fflush(stderr);
    }
    /* Closure has done its job (its captures were extracted/retained by
     * the body's prologue).  Drop it so its captures release one ref. */
    kk_drop(closure);
    /* Cache the result — retain because the thunk now owns a reference */
    kk_retain(result);
    kk_set_field(thunk, 0, 1);                /* mark as evaluated */
    kk_set_field(thunk, 1, result);           /* store result */
    return result;
}

/* ================================================================== */
/*  String tracking — distinguish kk_string_t from kk_alloc_con       */
/* ================================================================== */

/* String table must be large enough for ALL strings during program lifetime.
 * The MLIR emitter generates many small strings (SSA names, keywords, etc.)
 * and can easily exceed 65K strings for programs with 20+ definitions.
 * 1M entries × 8 bytes = 8 MB — acceptable for bootstrapping.
 *
 * JSON parsing of a 3 MB OrganIR file allocates ~3M small Texts (rope
 * nodes from acc<>chunk concatenation in pStrBody).  With a 4M-entry
 * table that's 75% load → linear-probing collisions dominate insert.
 * Bumped to 16M (128 MB).  Memory is fine for bootstrap. */
#define KK_STRING_TABLE_SIZE (1 << 24)  /* 16,777,216 */
#define KK_STRING_TOMBSTONE ((int64_t)1) /* deleted entry sentinel (odd → never a valid ptr) */
static int64_t string_table[KK_STRING_TABLE_SIZE];
/* Global string address range — O(1) rejection for non-string pointers. */
static uintptr_t g_string_lo = (uintptr_t)-1;
static uintptr_t g_string_hi = 0;

/* String registration log — records every registered string pointer in
 * allocation order. Used by kk_string_rollback to identify which strings
 * were allocated inside a handler body and should be freed on abort.
 * The hash table (string_table) provides O(1) lookup; the log provides
 * O(1) checkpoint/rollback. */
#define KK_STRING_LOG_SIZE (1 << 24)
static int64_t string_log[KK_STRING_LOG_SIZE];
static int64_t string_log_len = 0;

static void kk_register_string(int64_t ptr) {
    /* Update global string address range for O(1) rejection */
    uintptr_t u = (uintptr_t)ptr;
    if (u < g_string_lo) g_string_lo = u;
    if (u + sizeof(void*) > g_string_hi) g_string_hi = u + sizeof(void*);
    int64_t idx = (ptr >> 3) & (KK_STRING_TABLE_SIZE - 1);
    int64_t first_tombstone = -1;
    for (int64_t i = 0; i < KK_STRING_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_STRING_TABLE_SIZE - 1);
        if (string_table[probe] == ptr) return;  /* already registered */
        if (string_table[probe] == KK_STRING_TOMBSTONE && first_tombstone < 0)
            first_tombstone = probe;
        if (string_table[probe] == 0) {
            int64_t slot = first_tombstone >= 0 ? first_tombstone : probe;
            string_table[slot] = ptr;
            if (string_log_len < KK_STRING_LOG_SIZE) {
                string_log[string_log_len++] = ptr;
            }
            return;
        }
    }
}

static void kk_unregister_string(int64_t ptr) {
    int64_t idx = (ptr >> 3) & (KK_STRING_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_STRING_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_STRING_TABLE_SIZE - 1);
        if (string_table[probe] == ptr) {
            string_table[probe] = KK_STRING_TOMBSTONE;  /* preserve probe chain */
            return;
        }
        if (string_table[probe] == 0) return;
    }
}

int64_t kk_is_string(int64_t ptr) {
    if (!kk_is_heap_ptr(ptr)) return 0;
    uintptr_t u = (uintptr_t)ptr;
    /* Fast rejection: if ptr is outside the string address range,
     * it can only be a string if it's in the arena range (rare case:
     * some string pointers overlap the arena address space). */
    int in_string_range = (u >= g_string_lo && u < g_string_hi);
    if (!in_string_range) {
        /* Not in string range. Check arena range — if yes, use magic marker. */
        if (kk_arena_maybe_owns((const void*)(intptr_t)ptr))
            return *(int64_t*)(intptr_t)ptr == KK_STRING_MAGIC;
        return 0;
    }
    /* In string range. Arena-range pointers can be dereferenced for magic check. */
    if (kk_arena_maybe_owns((const void*)(intptr_t)ptr))
        return *(int64_t*)(intptr_t)ptr == KK_STRING_MAGIC;
    /* String-range, non-arena: use hash table (with tombstones). */
    int64_t idx = (ptr >> 3) & (KK_STRING_TABLE_SIZE - 1);
    for (int64_t i = 0; i < KK_STRING_TABLE_SIZE; i++) {
        int64_t probe = (idx + i) & (KK_STRING_TABLE_SIZE - 1);
        if (string_table[probe] == ptr) return 1;
        if (string_table[probe] == 0)  return 0;
        /* skip tombstones */
    }
    return 0;
}

/* String checkpoint/rollback — parallel to arena checkpoint/rollback.
 *
 * kk_string_checkpoint() returns the current string log length.
 * kk_string_rollback() walks all log entries from the checkpoint forward,
 * freeing each string (unless it is the rescued abort result), then
 * truncates the log back to the checkpoint.
 *
 * We must not call kk_str_drop for these strings because their refcounts
 * may be inconsistent (Perceus drops were skipped by longjmp). Instead
 * we forcibly free the kk_string_t and its owned bytes, and unregister
 * from the hash table. */
int64_t kk_string_checkpoint(void) {
    return string_log_len;
}

static void kk_str_force_free(int64_t s_i);

void kk_string_rollback(int64_t checkpoint, int64_t rescue_ptr) {
    for (int64_t i = checkpoint; i < string_log_len; i++) {
        int64_t ptr = string_log[i];
        if (ptr == 0) continue;          /* already freed normally */
        if (ptr == rescue_ptr) continue;  /* rescue: abort result string */
        /* Check still registered (may have been dropped normally before abort). */
        if (!kk_is_string(ptr)) continue;
        kk_unregister_string(ptr);
        kk_str_force_free(ptr);
    }
    string_log_len = checkpoint;
}

/* Forcibly free a kk_string_t and its owned byte buffer, without touching
 * refcounts or recursing into children (children may also be in the rollback
 * region and will be freed by their own log entries). */
static void kk_str_force_free(int64_t s_i) {
    kk_string_t* s = (kk_string_t*)s_i;
    if (s == NULL) return;
    if (s->kind == KK_STR_LEAF && s->owns_bytes) {
        free((void*)s->u.bytes);
    }
    /* Clear magic so freed memory isn't mistaken for a live string */
    s->magic = 0;
    /* For CONCAT nodes, children are freed by their own log entries. */
    free(s);
}

/* ================================================================== */
/*  String comparison                                                  */
/* ================================================================== */

/* Flatten a kk_string to a C string for comparison.
 * Returns a malloc'd buffer that the caller must free. */
static const char* kk_str_flatten_cmp(int64_t s, int64_t *out_len) {
    kk_string_t* str = (kk_string_t*)s;
    if (!str) { *out_len = 0; return ""; }
    /* Flatten the rope so we get contiguous bytes */
    int64_t flat = kk_str_flatten(s);
    kk_string_t* f = (kk_string_t*)flat;
    *out_len = f->byte_len;
    return kk_str_bytes(f);
}

int64_t kk_str_compare(int64_t a, int64_t b) {
    if (a == b) return 0;
    int64_t len_a, len_b;
    const char* sa = kk_str_flatten_cmp(a, &len_a);
    const char* sb = kk_str_flatten_cmp(b, &len_b);
    int64_t min_len = len_a < len_b ? len_a : len_b;
    int cmp = memcmp(sa, sb, (size_t)min_len);
    if (cmp != 0) return cmp;
    return (len_a > len_b) - (len_a < len_b);
}

/* ================================================================== */
/*  Generic structural comparison                                      */
/* ================================================================== */

/* No-op for now — use kk_is_heap_ptr + kk_is_string checks directly */

int64_t kk_compare(int64_t a, int64_t b) {
    if (a == b) return 0;
    /* Both strings? */
    if (kk_is_string(a) && kk_is_string(b))
        return kk_str_compare(a, b);
    /* Both heap objects (con)? Compare by tag, then fields. */
    if (kk_is_heap_ptr(a) && kk_is_heap_ptr(b) &&
        !kk_is_string(a) && !kk_is_string(b)) {
        /* Validate both pointers are arena-owned before dereferencing.
         * Set operations on Names can encounter freed fields if Perceus
         * dropped a Name that's still referenced in the Set.  Fall back
         * to address comparison for non-arena pointers. */
        if (!kk_arena_maybe_owns((const void*)(intptr_t)a) ||
            !kk_arena_maybe_owns((const void*)(intptr_t)b)) {
            return (a > b) - (a < b);
        }
        int64_t ta = kk_tag(a), tb = kk_tag(b);
        if (ta != tb) return (ta > tb) - (ta < tb);
        int64_t nfa = kk_nfields(a), nfb = kk_nfields(b);
        int64_t n = nfa < nfb ? nfa : nfb;
        for (int64_t i = 0; i < n; i++) {
            int64_t c = kk_compare(kk_field(a, i), kk_field(b, i));
            if (c != 0) return c;
        }
        return (nfa > nfb) - (nfa < nfb);
    }
    /* Fallback: integer comparison */
    return (a > b) - (a < b);
}

/* ================================================================== */
/*  List / Tuple / Maybe helpers                                       */
/* ================================================================== */

int64_t kk_nil(void) {
    return kk_alloc_con(KK_NIL_TAG, 0);
}

int64_t kk_cons(int64_t head, int64_t tail) {
    int64_t c = kk_alloc_con(KK_CONS_TAG, 2);
    kk_set_field(c, 0, head);
    kk_set_field(c, 1, tail);
    return c;
}

int64_t kk_list_head(int64_t list) { return kk_field(list, 0); }
int64_t kk_list_tail(int64_t list) { return kk_field(list, 1); }

int64_t kk_is_nil(int64_t list) {
    if (!kk_is_heap_ptr(list)) return 1;
    return kk_tag(list) == KK_NIL_TAG && kk_nfields(list) == 0;
}

/* Closure-call helpers.  A Koka closure is a heap cell tagged
 * KK_CLOSURE_TAG ('CLOS') whose field 0 holds the function pointer
 * (as i64) and fields 1..N hold captured values.  The wrapper at
 * the function pointer takes the closure as its first argument
 * followed by the remaining arguments; the PAP wrapper extracts
 * captures from the closure on entry.  These helpers are used by
 * the list HOF shims below to invoke the user-provided callback.
 */
static int64_t kk_call_closure_1(int64_t closure, int64_t a) {
    int64_t fptr_i64 = kk_field(closure, 0);
    typedef int64_t (*fn_t)(int64_t, int64_t);
    fn_t fn = (fn_t)(uintptr_t)fptr_i64;
    return fn(closure, a);
}

static int64_t kk_call_closure_2(int64_t closure, int64_t a, int64_t b) {
    int64_t fptr_i64 = kk_field(closure, 0);
    typedef int64_t (*fn_t)(int64_t, int64_t, int64_t);
    fn_t fn = (fn_t)(uintptr_t)fptr_i64;
    return fn(closure, a, b);
}

static int64_t kk_call_closure_3(int64_t closure,
                                 int64_t a, int64_t b, int64_t c) {
    int64_t fptr_i64 = kk_field(closure, 0);
    typedef int64_t (*fn_t)(int64_t, int64_t, int64_t, int64_t);
    fn_t fn = (fn_t)(uintptr_t)fptr_i64;
    return fn(closure, a, b, c);
}

static int64_t kk_call_closure_4(int64_t closure,
                                 int64_t a, int64_t b, int64_t c, int64_t d) {
    int64_t fptr_i64 = kk_field(closure, 0);
    typedef int64_t (*fn_t)(int64_t, int64_t, int64_t, int64_t, int64_t);
    fn_t fn = (fn_t)(uintptr_t)fptr_i64;
    return fn(closure, a, b, c, d);
}

/* Reverse a list in place by walking head-to-tail and rebuilding
 * with a fresh nil seed.  Used by kk_list_map / kk_list_filter to
 * restore source order after their accumulator-tail builds. */
static int64_t kk_list_reverse(int64_t xs) {
    int64_t r = kk_nil();
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        /* The new cons cell stores h, and the input list's cons cell
         * still references h via field 0.  When the input list is
         * later dropped, h is dropped too; without retain the
         * returned reversed list would alias a freed cell. */
        kk_retain(h);
        r = kk_cons(h, r);
        xs = kk_field(xs, 1);
    }
    return r;
}

/* `list.map(f)` — apply f to each element, preserving order. */
int64_t kk_list_map(int64_t xs, int64_t f) {
    int64_t acc = kk_nil();
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        /* Retain h and f: closure consumes both (h as arg, f as self
         * via Perceus drop in the closure body).  See kk_list_foldl. */
        kk_retain(h);
        kk_retain(f);
        int64_t mapped = kk_call_closure_1(f, h);
        acc = kk_cons(mapped, acc);
        xs = kk_field(xs, 1);
    }
    return kk_list_reverse(acc);
}

/* `list.filter(p)` — keep elements where p returns nonzero. */
int64_t kk_list_filter(int64_t xs, int64_t p) {
    int64_t acc = kk_nil();
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        /* Retain h and p for the predicate call. */
        kk_retain(h);
        kk_retain(p);
        if (kk_call_closure_1(p, h)) {
            /* And once more on h for storing into the output cons. */
            kk_retain(h);
            acc = kk_cons(h, acc);
        }
        xs = kk_field(xs, 1);
    }
    return kk_list_reverse(acc);
}

/* `list.foldl(z, f)` — left fold; f takes (acc, x) -> new-acc. */
int64_t kk_list_foldl(int64_t xs, int64_t z, int64_t f) {
    int64_t acc = z;
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        /* Retain h and f: the closure receives h as an owned arg and
         * its Perceus-inserted drops will consume the refcount.  The
         * cons cell still references h via field 0, so without this
         * retain the closure's drop takes h's refcount to 0 — wiping
         * its nfields metadata while the rest of the program still
         * accesses the cell (observed for rational coefficients in
         * surd's lcm_of_denoms → list.foldl with a lambda body that
         * destructures C into denom(C)).  Retain f too because the
         * closure body's Perceus also drops its self arg. */
        kk_retain(h);
        kk_retain(f);
        acc = kk_call_closure_2(f, h, acc);
        xs = kk_field(xs, 1);
    }
    return acc;
}

/* `list.all(p)` — short-circuit conjunction of p applied to each element.
 * Returns 1 if every element satisfies p (or list is empty), 0 otherwise. */
int64_t kk_list_all(int64_t xs, int64_t p) {
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h); kk_retain(p);  /* See kk_list_foldl. */
        if (!kk_call_closure_1(p, h)) return 0;
        xs = kk_field(xs, 1);
    }
    return 1;
}

/* `list.any(p)` — short-circuit disjunction. */
int64_t kk_list_any(int64_t xs, int64_t p) {
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h); kk_retain(p);  /* See kk_list_foldl. */
        if (kk_call_closure_1(p, h)) return 1;
        xs = kk_field(xs, 1);
    }
    return 0;
}

/* `list.drop(n)` — return list with the first n elements removed.
 * If n exceeds the list length, returns nil. */
int64_t kk_list_drop(int64_t xs, int64_t n) {
    while (n > 0 && kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        xs = kk_field(xs, 1);
        n--;
    }
    return xs;
}

/* `list.take(n)` — return the first n elements (or fewer if shorter). */
int64_t kk_list_take(int64_t xs, int64_t n) {
    int64_t acc = kk_nil();
    while (n > 0 && kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h);  /* h is stored in the new cons cell; the input
                        * list still references it. */
        acc = kk_cons(h, acc);
        xs = kk_field(xs, 1);
        n--;
    }
    return kk_list_reverse(acc);
}

/* List length: count cons cells (O(n)). */
int64_t kk_list_length(int64_t xs) {
    int64_t n = 0;
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        n++;
        xs = kk_field(xs, 1);
    }
    return n;
}

/* List concatenation: append ys to the end of xs. */
int64_t kk_list_concat(int64_t xs, int64_t ys) {
    int64_t acc = ys;
    int64_t rev = kk_list_reverse(xs);
    while (kk_is_heap_ptr(rev) && kk_tag(rev) == KK_CONS_TAG) {
        int64_t h = kk_field(rev, 0);
        kk_retain(h);  /* h is stored in the new cons; rev's cons cell
                        * still references it. */
        acc = kk_cons(h, acc);
        rev = kk_field(rev, 1);
    }
    return acc;
}

/* `list.flatmap(f)` — map then concat: f maps each x to a list and
 * we concatenate the result lists in order. */
int64_t kk_list_flatmap(int64_t xs, int64_t f) {
    int64_t acc = kk_nil();
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h); kk_retain(f);  /* See kk_list_foldl. */
        int64_t sub = kk_call_closure_1(f, h);
        /* Append `sub` to acc. */
        while (kk_is_heap_ptr(sub) && kk_tag(sub) == KK_CONS_TAG) {
            int64_t sh = kk_field(sub, 0);
            kk_retain(sh);  /* sh is stored in acc; sub's cons cell
                             * still references it. */
            acc = kk_cons(sh, acc);
            sub = kk_field(sub, 1);
        }
        xs = kk_field(xs, 1);
    }
    return kk_list_reverse(acc);
}

/* `list.filter-map(f)` — f maps each element to a maybe<b>; keep
 * the Just values.  Koka stdlib: `pub fun filter-map(xs, f) ...`. */
int64_t kk_list_filter_map(int64_t xs, int64_t f) {
    int64_t acc = kk_nil();
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h); kk_retain(f);  /* See kk_list_foldl. */
        int64_t r = kk_call_closure_1(f, h);
        /* Just(v) cells carry KK_JUST_TAG with the payload at field 0;
         * Nothing cells carry KK_NOTHING_TAG and no payload.  Both tags
         * match the djb2-hash assignment Frankenstein puts on the
         * generated `Just`/`Nothing` constructors. */
        if (kk_is_heap_ptr(r) && kk_tag(r) == KK_JUST_TAG) {
            int64_t inner = kk_field(r, 0);
            kk_retain(inner);  /* inner is stored in acc; r's Just cell
                                * still references it. */
            acc = kk_cons(inner, acc);
        }
        xs = kk_field(xs, 1);
    }
    return kk_list_reverse(acc);
}

/* `range/list(lo, hi)` — build the list [lo, lo+1, ..., hi].
 * Returns nil if lo > hi.  Used by Koka's `list(lo, hi)` /
 * `range/list` stdlib intrinsic. */
int64_t kk_range_list(int64_t lo, int64_t hi) {
    int64_t r = kk_nil();
    for (int64_t i = hi; i >= lo; i--) {
        r = kk_cons(i, r);
    }
    return r;
}

/* `unjust(m)` — extract the value from a Just; on Nothing, return 0
 * (we don't have proper exceptions here; the surd code expects to
 * only call this on Just values). */
int64_t kk_unjust(int64_t maybe_v) {
    if (kk_is_heap_ptr(maybe_v) && kk_nfields(maybe_v) >= 1) {
        int64_t v = kk_field(maybe_v, 0);
        kk_retain(v);  /* v escapes as the return value; the Just
                        * cell still references it. */
        return v;
    }
    return 0;
}

/* `maybe/head(xs)` — head of a list as a Maybe.  Returns Nothing
 * (encoded as nil) for empty lists, or Just(head) wrapped in a Cons-
 * shaped cell (compatible with our maybe encoding). */
int64_t kk_maybe_head(int64_t xs) {
    if (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        /* Just(head) — allocate a 1-field cell holding the head.
         * The tag we use here matches Cons since our maybe
         * encoding piggy-backs on Cons / Nil. */
        int64_t h = kk_field(xs, 0);
        kk_retain(h);  /* h is stored in the returned cell AND
                        * remains referenced by xs's cons cell. */
        return kk_cons(h, kk_nil());
    }
    /* Nothing → nil */
    return kk_nil();
}

/* `list.zip(ys)` — pair corresponding elements, stopping at the
 * shorter list's end.  Returns a list of Tuple2 cells. */
int64_t kk_list_zip(int64_t xs, int64_t ys) {
    int64_t acc = kk_nil();
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG
        && kk_is_heap_ptr(ys) && kk_tag(ys) == KK_CONS_TAG) {
        int64_t x = kk_field(xs, 0);
        int64_t y = kk_field(ys, 0);
        /* Retain x, y: each is stored in the new pair AND remains
         * referenced by its source cons cell. */
        kk_retain(x);
        kk_retain(y);
        /* Build Tuple2(x, y) using KK_CONS_TAG (Koka represents
         * Tuple2 as a 2-field cell; tag value doesn't matter for
         * field access). */
        int64_t pair = kk_alloc_con(KK_CONS_TAG, 2);
        kk_set_field(pair, 0, x);
        kk_set_field(pair, 1, y);
        acc = kk_cons(pair, acc);
        xs = kk_field(xs, 1);
        ys = kk_field(ys, 1);
    }
    return kk_list_reverse(acc);
}

/* `list.map-indexed(f)` — like map, but f takes (index, element). */
int64_t kk_list_map_indexed(int64_t xs, int64_t f) {
    int64_t acc = kk_nil();
    int64_t i = 0;
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h); kk_retain(f);  /* See kk_list_foldl. */
        int64_t mapped = kk_call_closure_2(f, i, h);
        acc = kk_cons(mapped, acc);
        xs = kk_field(xs, 1);
        i++;
    }
    return kk_list_reverse(acc);
}

/* `joinsep/join(xs, sep)` — join a list of strings with sep between
 * adjacent elements.  Each cons cell of xs holds a kk_string head. */
int64_t kk_joinsep_join(int64_t xs, int64_t sep) {
    if (!(kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG)) {
        kk_str_drop(sep);
        return kk_string_empty();
    }
    int64_t acc = kk_field(xs, 0);
    xs = kk_field(xs, 1);
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        /* Each kk_str_concat consumes a refcount of `sep`; retain so the
         * caller's single ownership survives every loop iteration.  Without
         * this, an empty `sep` is freed by kk_str_concat's b->byte_len==0
         * branch on iteration 1 and subsequent iterations read garbage. */
        kk_str_retain(sep);
        acc = kk_str_concat(acc, sep);
        acc = kk_str_concat(acc, h);
        xs = kk_field(xs, 1);
    }
    kk_str_drop(sep);
    return acc;
}

/* `throw(msg)` — generic exception.  We don't have proper effect
 * handlers wired for exn yet; just print the message to stderr
 * and exit. */
int64_t kk_throw(int64_t msg) {
    char* buf = kk_str_dup_cstr(msg);
    fprintf(stderr, "throw: %s\n", buf ? buf : "(unknown)");
    if (buf) free(buf);
    exit(1);
    return 0;
}

/* `list.foreach(f)` — apply f to each element for its side effect;
 * return unit (0). */
int64_t kk_list_foreach(int64_t xs, int64_t f) {
    while (kk_is_heap_ptr(xs) && kk_tag(xs) == KK_CONS_TAG) {
        int64_t h = kk_field(xs, 0);
        kk_retain(h); kk_retain(f);  /* See kk_list_foldl. */
        (void)kk_call_closure_1(f, h);
        xs = kk_field(xs, 1);
    }
    return 0;
}

int64_t kk_pair(int64_t a, int64_t b) {
    int64_t p = kk_alloc_con(KK_PAIR_TAG, 2);
    kk_set_field(p, 0, a);
    kk_set_field(p, 1, b);
    return p;
}

int64_t kk_fst(int64_t pair) { return kk_field(pair, 0); }
int64_t kk_snd(int64_t pair) { return kk_field(pair, 1); }

int64_t kk_nothing(void) { return kk_alloc_con(KK_NOTHING_TAG, 0); }
int64_t kk_just(int64_t x) {
    int64_t j = kk_alloc_con(KK_JUST_TAG, 1);
    kk_set_field(j, 0, x);
    return j;
}
