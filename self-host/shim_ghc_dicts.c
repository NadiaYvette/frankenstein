/* GHC.Internal typeclass dictionary shims for the self-hosted binary.
 *
 * These were previously NULL-returning stubs created by the linker's
 * --unresolved-symbols=ignore-in-object-files.  Most are typeclass
 * dictionaries that downstream code dereferences as closure values
 * (accessing field 0 = NULL crashes).
 *
 * Top stub callers (Target B from BOOTSTRAP_RESTORATION.md):
 *   841  GHC_Internal_Data_Either_zdfMonadEither$0
 *   539  GHC_Internal_Show_zdfShowList$0
 *   226  GHC_Internal_Data_Either_zdfApplicativeEither$0
 *   127  GHC_Internal_Stack_Types_emptyCallStack$0
 *    84  GHC_Internal_Prim_dataToTagSmallzh$0
 *    77  GHC_Internal_Base_pure$0 (already shimmed in shim_ghc_prim.c)
 *    61  GHC_Internal_Data_Tuple_fst$0
 *    56  GHC_Internal_Show_zdfShowMaybe$0
 *    49  GHC_Internal_Data_Either_zdfFunctorEither$0
 *    42  GHC_Internal_Classes_not$0
 *    39  GHC_Internal_Unicode_isDigit$0
 *    30  GHC_Internal_Data_Tuple_snd$0
 */

#include <stdint.h>
#include "../runtime/kk_runtime.h"

#define KK_CLOSURE_TAG 0x434C4F53  /* 'CLOS' */
#define KK_EITHER_MONAD_MARKER 0xEE17E8LL  /* see shim_ghc_prim.c */

/* Make a 0-capture closure (just an fptr in field 0). */
static int64_t make_closure0(void* fptr) {
    int64_t cell = kk_alloc_con(KK_CLOSURE_TAG, 1);
    kk_set_field(cell, 0, (int64_t)(intptr_t)fptr);
    return cell;
}

/* ------------------------------------------------------------------ */
/*  Either typeclass dictionaries — all return the marker that         */
/*  shim_ghc_prim.c's bind_3 / etc. dispatch on.                       */
/* ------------------------------------------------------------------ */

int64_t ghc_either_monad_0(void) __asm__("GHC_Internal_Data_Either_zdfMonadEither$0");
int64_t ghc_either_monad_0(void) { return KK_EITHER_MONAD_MARKER; }

int64_t ghc_either_applicative_0(void) __asm__("GHC_Internal_Data_Either_zdfApplicativeEither$0");
int64_t ghc_either_applicative_0(void) { return KK_EITHER_MONAD_MARKER; }

int64_t ghc_either_functor_0(void) __asm__("GHC_Internal_Data_Either_zdfFunctorEither$0");
int64_t ghc_either_functor_0(void) { return KK_EITHER_MONAD_MARKER; }

/* ------------------------------------------------------------------ */
/*  Show dictionaries — return a closure that emits a placeholder.     */
/*  GHC's auto-derived Show for lists/Maybe is complex; the real fix   */
/*  is bridge-side substitution, but a non-NULL value at least stops   */
/*  the crash.                                                          */
/* ------------------------------------------------------------------ */

static int64_t show_placeholder_runner(int64_t clos, int64_t x) {
    (void)clos; (void)x;
    /* Returns an empty Haskell-string ([Char] cons list = nil). */
    return kk_nil();
}

int64_t ghc_show_list_0(void) __asm__("GHC_Internal_Show_zdfShowList$0");
int64_t ghc_show_list_0(void) {
    /* The dict's "show" field would be at index N; downstream Frankenstein
     * code expects a callable.  Return a closure that emits "" for now. */
    return make_closure0((void*)&show_placeholder_runner);
}

int64_t ghc_show_maybe_0(void) __asm__("GHC_Internal_Show_zdfShowMaybe$0");
int64_t ghc_show_maybe_0(void) {
    return make_closure0((void*)&show_placeholder_runner);
}

int64_t ghc_show_tuple2_0(void) __asm__("GHC_Internal_Show_zdfShowTuple2$0");
int64_t ghc_show_tuple2_0(void) {
    return make_closure0((void*)&show_placeholder_runner);
}

/* ------------------------------------------------------------------ */
/*  emptyCallStack — GHC's HasCallStack constraint value.              */
/* ------------------------------------------------------------------ */

int64_t ghc_empty_callstack_0(void) __asm__("GHC_Internal_Stack_Types_emptyCallStack$0");
int64_t ghc_empty_callstack_0(void) {
    /* CallStack is a list of (String, SrcLoc) pairs; empty = nil. */
    return kk_nil();
}

/* ------------------------------------------------------------------ */
/*  Tuple projections.                                                  */
/* ------------------------------------------------------------------ */

static int64_t fst_runner(int64_t clos, int64_t p) {
    (void)clos;
    return kk_fst(p);
}
int64_t ghc_fst_0(void) __asm__("GHC_Internal_Data_Tuple_fst$0");
int64_t ghc_fst_0(void) { return make_closure0((void*)&fst_runner); }

static int64_t snd_runner(int64_t clos, int64_t p) {
    (void)clos;
    return kk_snd(p);
}
int64_t ghc_snd_0(void) __asm__("GHC_Internal_Data_Tuple_snd$0");
int64_t ghc_snd_0(void) { return make_closure0((void*)&snd_runner); }

/* ------------------------------------------------------------------ */
/*  Boolean not.                                                        */
/* ------------------------------------------------------------------ */

#define KK_TRUE_TAG  24914
#define KK_FALSE_TAG 44872

static int64_t not_runner(int64_t clos, int64_t b) {
    (void)clos;
    /* Match shim_ghc_prim.c's tobool semantics: heap-tagged or unboxed. */
    int truthy;
    if (b == 0) truthy = 0;
    else if (b == 1) truthy = 1;
    else if (kk_is_heap_ptr(b)) truthy = (kk_tag(b) == KK_TRUE_TAG);
    else truthy = (b != 0);
    return truthy ? 0 : 1;
}
int64_t ghc_classes_not_0(void) __asm__("GHC_Internal_Classes_not$0");
int64_t ghc_classes_not_0(void) { return make_closure0((void*)&not_runner); }

/* ------------------------------------------------------------------ */
/*  Prim: dataToTagSmall# — extracts constructor tag.                  */
/* ------------------------------------------------------------------ */

static int64_t data_to_tag_runner(int64_t clos, int64_t x) {
    (void)clos;
    if (kk_is_heap_ptr(x)) return kk_tag(x);
    return x;  /* unboxed (e.g., Int#) — return as-is */
}
int64_t ghc_data_to_tag_small_0(void) __asm__("GHC_Internal_Prim_dataToTagSmallzh$0");
int64_t ghc_data_to_tag_small_0(void) { return make_closure0((void*)&data_to_tag_runner); }

/* ------------------------------------------------------------------ */
/*  Unicode predicates — minimal ASCII coverage.                       */
/* ------------------------------------------------------------------ */

static int64_t is_digit_runner(int64_t clos, int64_t c) {
    (void)clos;
    return (c >= '0' && c <= '9') ? 1 : 0;
}
int64_t ghc_unicode_is_digit_0(void) __asm__("GHC_Internal_Unicode_isDigit$0");
int64_t ghc_unicode_is_digit_0(void) { return make_closure0((void*)&is_digit_runner); }

static int64_t is_space_runner(int64_t clos, int64_t c) {
    (void)clos;
    return (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' || c == '\f') ? 1 : 0;
}
int64_t ghc_unicode_is_space_0(void) __asm__("GHC_Internal_Unicode_isSpace$0");
int64_t ghc_unicode_is_space_0(void) { return make_closure0((void*)&is_space_runner); }

/* ------------------------------------------------------------------ */
/*  Data.Text.dropWhile — partial application returns Text→Text.       */
/*  Implementation walks bytes (ASCII-only).  For UTF-8 the closure    */
/*  caller is the JSON whitespace predicate (isSpace), so ASCII is     */
/*  enough.                                                             */
/* ------------------------------------------------------------------ */

#include <stdlib.h>

/* Forward decls for call/retain helpers — defined in shim_ghc_prim.c
 * as static, so duplicate the minimum here. */
typedef int64_t (*fn1_t)(int64_t, int64_t);

static int64_t shim_call1(int64_t clos, int64_t a) {
    kk_retain(a);
    /* Retain closure — lifted lambdas drop their closure-arg (commit 871afa7).
     * Phase 12c step 8. */
    kk_retain(clos);
    clos = kk_thunk_force(clos);
    if (!kk_is_heap_ptr(clos)) {
        typedef int64_t (*raw1_t)(int64_t);
        return ((raw1_t)(intptr_t)clos)(a);
    }
    int64_t fp = kk_field(clos, 0);
    if (fp == 0) return 0;  /* fail-soft on broken closures */
    return ((fn1_t)(intptr_t)fp)(clos, a);
}

static int64_t dropWhile_runner(int64_t clos, int64_t text) {
    int64_t pred = kk_field(clos, 1);
    int64_t total = kk_str_len(text);
    char* cstr = kk_str_dup_cstr(text);
    int64_t offset = 0;
    while (offset < total) {
        int64_t cp = (int64_t)(unsigned char)cstr[offset];
        kk_retain(pred);
        int64_t r = shim_call1(pred, cp);
        if (!r) break;
        offset++;
    }
    free(cstr);
    if (offset == 0) return text;  /* nothing dropped */
    return kk_str_slice(text, offset, total - offset);
}

int64_t data_text_dropWhile_1(int64_t pred) __asm__("Data_Text_dropWhile$1");
int64_t data_text_dropWhile_1(int64_t pred) {
    int64_t cell = kk_alloc_con(KK_CLOSURE_TAG, 2);
    kk_set_field(cell, 0, (int64_t)(intptr_t)&dropWhile_runner);
    kk_set_field(cell, 1, pred);
    return cell;
}

/* ------------------------------------------------------------------ */
/*  GHC.Internal.Data.Maybe.maybeToList: Maybe a -> [a]                */
/*  Nothing  -> []                                                      */
/*  Just x   -> [x]                                                     */
/*                                                                      */
/*  Discriminates by field count, since the tag of `Just` depends on    */
/*  stableConTag's hash (unstable to read off without recomputing).     */
/*  Nothing is a 0-field constructor; Just is 1-field.                  */
/* ------------------------------------------------------------------ */
int64_t ghc_maybeToList_1(int64_t m) __asm__("GHC_Internal_Data_Maybe_maybeToList$1");
int64_t ghc_maybeToList_1(int64_t m) {
    if (!kk_is_heap_ptr(m) || kk_nfields(m) == 0) return kk_nil();
    return kk_cons(kk_field(m, 0), kk_nil());
}
