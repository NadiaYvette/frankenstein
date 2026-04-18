/* Cross-module shims for Frankenstein self-hosted binary.
 *
 * Two categories:
 *
 * 1. $0 function references — the emitter generates foo$0() when a function
 *    is used as a value (passed to map, foldr, etc.) rather than called.
 *    These return a 1-field closure wrapping the function pointer.
 *
 * 2. False externals — local where-bound helpers (go, pat_, nub, etc.) that
 *    GHC floated to top-level.  nameModule_maybe returns Just for them, so
 *    the emitter module-qualifies the reference, but the definition gets a
 *    different name (lambda123).  Stub them as abort() until the emitter
 *    is fixed to recognize let-bound names.
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../runtime/kk_runtime.h"

/* ------------------------------------------------------------------ */
/*  $0 function references: return a closure wrapping the function    */
/* ------------------------------------------------------------------ */

/* Helper: wrap a 1-arg function pointer in a closure.
 * Closure convention: call1(clos, arg) calls clos->field[0](clos, arg).
 * So field 0 must be a trampoline that extracts the real fn from field 1
 * and calls fn(arg), discarding the closure argument.
 * Uses CLOS tag so kk_drop skips field 0 (code pointer). */
#define CLOS_TAG_CM 0x434C4F53
static int64_t trampoline_1arg(int64_t clos, int64_t arg) {
    int64_t fn_ptr = kk_field(clos, 1);
    typedef int64_t (*raw1_t)(int64_t);
    return ((raw1_t)(intptr_t)fn_ptr)(arg);
}
static int64_t mk_closure1(int64_t (*fn)(int64_t)) {
    int64_t c = kk_alloc_con(CLOS_TAG_CM, 2);
    kk_set_field(c, 0, (int64_t)(intptr_t)trampoline_1arg);
    kk_set_field(c, 1, (int64_t)(intptr_t)fn);
    return c;
}

/* Helper: wrap a 2-arg function pointer in a closure.
 * Same trampoline pattern: field[0] = trampoline, field[1] = real fn.
 * The trampoline is called as trampoline(clos, arg1, arg2) and
 * forwards to fn(arg1, arg2). */
static int64_t trampoline_2arg(int64_t clos, int64_t a, int64_t b) {
    int64_t fn_ptr = kk_field(clos, 1);
    typedef int64_t (*raw2_t)(int64_t, int64_t);
    return ((raw2_t)(intptr_t)fn_ptr)(a, b);
}
static int64_t mk_closure2(int64_t (*fn)(int64_t, int64_t)) {
    int64_t c = kk_alloc_con(CLOS_TAG_CM, 2);
    kk_set_field(c, 0, (int64_t)(intptr_t)trampoline_2arg);
    kk_set_field(c, 1, (int64_t)(intptr_t)fn);
    return c;
}

/* --- Core/Types selectors used as values --- */

extern int64_t Frankenstein_Core_Types_bindExpr(int64_t);
extern int64_t Frankenstein_Core_Types_branchBody(int64_t);
extern int64_t Frankenstein_Core_Types_branchPattern(int64_t);
extern int64_t Frankenstein_Core_Types_defExpr(int64_t);
extern int64_t Frankenstein_Core_Types_progData(int64_t);
extern int64_t Frankenstein_Core_Types_progEffects(int64_t);
extern int64_t Frankenstein_Core_Types_progName(int64_t);

/* Use __asm__ to define the exact linker symbol with $ in the name */

int64_t frkn_bindExpr_0(void) __asm__("Frankenstein_Core_Types_bindExpr$0");
int64_t frkn_bindExpr_0(void) { return mk_closure1(Frankenstein_Core_Types_bindExpr); }

int64_t frkn_branchBody_0(void) __asm__("Frankenstein_Core_Types_branchBody$0");
int64_t frkn_branchBody_0(void) { return mk_closure1(Frankenstein_Core_Types_branchBody); }

int64_t frkn_branchPattern_0(void) __asm__("Frankenstein_Core_Types_branchPattern$0");
int64_t frkn_branchPattern_0(void) { return mk_closure1(Frankenstein_Core_Types_branchPattern); }

int64_t frkn_defExpr_0(void) __asm__("Frankenstein_Core_Types_defExpr$0");
int64_t frkn_defExpr_0(void) { return mk_closure1(Frankenstein_Core_Types_defExpr); }

int64_t frkn_progData_0(void) __asm__("Frankenstein_Core_Types_progData$0");
int64_t frkn_progData_0(void) { return mk_closure1(Frankenstein_Core_Types_progData); }

int64_t frkn_progEffects_0(void) __asm__("Frankenstein_Core_Types_progEffects$0");
int64_t frkn_progEffects_0(void) { return mk_closure1(Frankenstein_Core_Types_progEffects); }

int64_t frkn_progName_0(void) __asm__("Frankenstein_Core_Types_progName$0");
int64_t frkn_progName_0(void) { return mk_closure1(Frankenstein_Core_Types_progName); }

/* --- KokaBridge/CoreTranslate translateProgram used as value --- */
extern int64_t Frankenstein_KokaBridge_CoreTranslate_translateProgram(int64_t);

int64_t frkn_koka_translateProgram_0(void) __asm__("Frankenstein_KokaBridge_CoreTranslate_translateProgram$0");
int64_t frkn_koka_translateProgram_0(void) {
    return mk_closure1(Frankenstein_KokaBridge_CoreTranslate_translateProgram);
}

/* ------------------------------------------------------------------ */
/*  zeze$0 (==) and zp$0 (+) as closures.                            */
/*  GHC's typeclass dictionary stripping leaves these as module-local */
/*  references.  For unboxed ints/chars, == is just integer compare.  */
/* ------------------------------------------------------------------ */

/* == : compare two values (works for ints, chars, and boxed values by
 * comparing field 0 when both are heap-allocated cons with same tag) */
static int64_t prim_eq(int64_t a, int64_t b) {
    if (a == b) return 1;
    /* If both are heap-allocated constructors, compare field 0 (unbox) */
    if (kk_is_heap_ptr(a) && !kk_is_string(a) &&
        kk_is_heap_ptr(b) && !kk_is_string(b)) {
        if (kk_tag(a) == kk_tag(b) && kk_nfields(a) == 1 && kk_nfields(b) == 1)
            return kk_field(a, 0) == kk_field(b, 0) ? 1 : 0;
    }
    /* String equality */
    if (kk_is_string(a) && kk_is_string(b)) {
        char* sa = kk_str_dup_cstr(a);
        char* sb = kk_str_dup_cstr(b);
        int eq = sa && sb && strcmp(sa, sb) == 0;
        free(sa); free(sb);
        return eq ? 1 : 0;
    }
    return 0;
}
/* + : integer addition */
static int64_t prim_plus(int64_t a, int64_t b) { return a + b; }

/* Each module that uses (==) as a value gets its own zeze$0 */
#define ZEZE_SHIM(mod) \
    int64_t mod##_zeze_0(void) __asm__(#mod "_zeze$0"); \
    int64_t mod##_zeze_0(void) { return mk_closure2(prim_eq); }

ZEZE_SHIM(Frankenstein_MlirEmit_Emitter)
ZEZE_SHIM(Frankenstein_Core_FlattenPatterns)
ZEZE_SHIM(Frankenstein_MercuryBridge_HldsParse)
ZEZE_SHIM(Frankenstein_RustBridge_MirParse)

/* _ze$0: bare == (no module qualifier — used by some GHC-generated code) */
int64_t bare_ze_0(void) __asm__("_ze$0");
int64_t bare_ze_0(void) { return mk_closure2(prim_eq); }

/* zp$0: + as a value in Perceus */
int64_t perceus_zp_0(void) __asm__("Frankenstein_Core_Perceus_zp$0");
int64_t perceus_zp_0(void) { return mk_closure2(prim_plus); }

/* ------------------------------------------------------------------ */
/*  Additional $0 closures not already in shim_ghc_prim/list/etc.     */
/*  Many $0 symbols are already defined in shim_ghc_prim.o,           */
/*  shim_ghc_list.o, shim_data_*.o, shim_system.o. Only add the      */
/*  ones that are genuinely missing here.                              */
/* ------------------------------------------------------------------ */

/* Data_Map_Internal_zdfEqMap$0 (distinct from Data_Map_Internal__fEqMap$0) */
int64_t data_map_zdfEqMap_0(void) __asm__("Data_Map_Internal_zdfEqMap$0");
int64_t data_map_zdfEqMap_0(void) { return mk_closure2(prim_eq); }

/* ------------------------------------------------------------------ */
/*  Koka donor library constants as closures ($0)                     */
/* ------------------------------------------------------------------ */

/* These are all 0-arg constants that return empty/null/zero values */

int64_t common_nameNil_0(void) __asm__("Common_Name_nameNil$0");
int64_t common_nameNil_0(void) { return kk_string_empty(); }
int64_t common_rangeNull_0(void) __asm__("Common_Range_rangeNull$0");
int64_t common_rangeNull_0(void) { return 0; }
int64_t common_noFip_0(void) __asm__("Common_Syntax_noFip$0");
int64_t common_noFip_0(void) { return 0; }
int64_t common_valueReprZero_0(void) __asm__("Common_Syntax_valueReprZero$0");
int64_t common_valueReprZero_0(void) { return 0; }
int64_t compile_flagsNull_0(void) __asm__("Compile_Options_flagsNull$0");
int64_t compile_flagsNull_0(void) { return 0; }
int64_t core_exprTrue_0(void) __asm__("Core_Core_exprTrue$0");
int64_t core_exprTrue_0(void) { return 1; }
int64_t kind_kindEffect_0(void) __asm__("Kind_Kind_kindEffect$0");
int64_t kind_kindEffect_0(void) { return 0; }
int64_t kind_kindHeap_0(void) __asm__("Kind_Kind_kindHeap$0");
int64_t kind_kindHeap_0(void) { return 0; }
int64_t kind_kindLabel_0(void) __asm__("Kind_Kind_kindLabel$0");
int64_t kind_kindLabel_0(void) { return 0; }
int64_t kind_kindStar_0(void) __asm__("Kind_Kind_kindStar$0");
int64_t kind_kindStar_0(void) { return 0; }
int64_t type_effectEmpty_0(void) __asm__("Type_Type_effectEmpty$0");
int64_t type_effectEmpty_0(void) { return 0; }
int64_t type_typeTotal_0(void) __asm__("Type_Type_typeTotal$0");
int64_t type_typeTotal_0(void) { return 0; }
int64_t type_typeUnit_0(void) __asm__("Type_Type_typeUnit$0");
int64_t type_typeUnit_0(void) { return 0; }

/* GHC bridge internals — stubs (only used in GhcBridge, not self-hosting) */
static int64_t stub_id(int64_t x) { return x; }
int64_t ghc_isAlgTyCon_0(void) __asm__("GHC_Core_TyCon_isAlgTyCon$0");
int64_t ghc_isAlgTyCon_0(void) { return mk_closure1(stub_id); }
int64_t ghc_xopt_set_0(void) __asm__("GHC_Driver_DynFlags_xopt_set$0");
int64_t ghc_xopt_set_0(void) { return mk_closure2(prim_eq); }
int64_t ghc_getSessionDynFlags_0(void) __asm__("GHC_Driver_Monad_getSessionDynFlags$0");
int64_t ghc_getSessionDynFlags_0(void) { return 0; }
int64_t ghc_getModuleGraph_0(void) __asm__("GHC_Driver_Session_Inspect_getModuleGraph$0");
int64_t ghc_getModuleGraph_0(void) { return 0; }
int64_t ghc_isTyVar_0(void) __asm__("GHC_Types_Var_isTyVar$0");
int64_t ghc_isTyVar_0(void) { return mk_closure1(stub_id); }

/* System.Directory — already in shim_system.o */

/* --- ConTags.o: abs and mod inlined from GHC typeclasses --- */

/* abs$0: returns a closure wrapping abs :: Int -> Int */
static int64_t contags_abs_impl(int64_t x) {
    return x < 0 ? -x : x;
}
int64_t contags_abs_0(void) __asm__("Frankenstein_Core_ConTags_abs$0");
int64_t contags_abs_0(void) { return mk_closure1(contags_abs_impl); }

/* mod$0: returns a closure wrapping mod :: Int -> Int -> Int
 * Haskell mod satisfies: result has same sign as divisor */
static int64_t contags_mod_impl(int64_t x, int64_t y) {
    if (y == 0) return 0;
    int64_t r = x % y;
    if ((r > 0 && y < 0) || (r < 0 && y > 0)) r += y;
    return r;
}
int64_t contags_mod_0(void) __asm__("Frankenstein_Core_ConTags_mod$0");
int64_t contags_mod_0(void) { return mk_closure2(contags_mod_impl); }

/* --- error$0: error as a value (closure wrapping error$1) --- */
extern int64_t GHC_Internal_Err_error_1(int64_t) __asm__("GHC_Internal_Err_error$1");
int64_t ghc_err_error_0(void) __asm__("GHC_Internal_Err_error$0");
int64_t ghc_err_error_0(void) { return mk_closure1(GHC_Internal_Err_error_1); }

/* --- Koka donor library stubs (only used by KokaBridge, not exercised) --- */

int64_t common_newName_1(int64_t s) __asm__("Common_Name_newName$1");
int64_t common_newName_1(int64_t s) { return s; /* name = string identity */ }

int64_t common_newQualified_2(int64_t m, int64_t n) __asm__("Common_Name_newQualified$2");
int64_t common_newQualified_2(int64_t m, int64_t n) { (void)m; return n; }

int64_t kind_kindFun_2(int64_t a, int64_t b) __asm__("Kind_Kind_kindFun$2");
int64_t kind_kindFun_2(int64_t a, int64_t b) { (void)a; return b; }

int64_t type_effectExtend_2(int64_t a, int64_t b) __asm__("Type_Type_effectExtend$2");
int64_t type_effectExtend_2(int64_t a, int64_t b) { (void)a; return b; }

/* False externals removed — the emitter's promoted-fn mechanism
   resolves all 27 where-bound helpers (go, pat_, nub, etc.) to
   their lambda-lifted names within each .o file. */
