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
#include "../runtime/kk_runtime.h"

/* ------------------------------------------------------------------ */
/*  $0 function references: return a closure wrapping the function    */
/* ------------------------------------------------------------------ */

/* Helper: wrap a 1-arg function pointer in a kk_con closure.
 * Field 0 = function pointer (cast to i64).
 * The caller (map, foldr, etc.) will extract and call it.
 * Tag 0 = standard closure tag.  */
static int64_t mk_closure1(int64_t (*fn)(int64_t)) {
    int64_t c = kk_alloc_con(0, 1);
    kk_set_field(c, 0, (int64_t)fn);
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
/*  False externals: local helpers that the emitter externalizes.     */
/*  These are where-bound functions (go, pat_, nub, etc.) that GHC   */
/*  gave module info to after floating.  The actual code lives under  */
/*  lambda* names in the same .o file.  For now: abort with a        */
/*  diagnostic message so we know which one fires.                    */
/* ------------------------------------------------------------------ */

#define FALSE_EXTERN(sym, arity, ...) \
    int64_t sym##_stub(__VA_ARGS__) __asm__(#sym); \
    int64_t sym##_stub(__VA_ARGS__) { \
        fprintf(stderr, "FATAL: false external called: %s\n", #sym); \
        abort(); \
    }

/* The macros need actual parameter names for the varargs */
#define A0
#define A1 int64_t a
#define A2 int64_t a, int64_t b
#define A3 int64_t a, int64_t b, int64_t c
#define A4 int64_t a, int64_t b, int64_t c, int64_t d

/* Core/ConTags */
FALSE_EXTERN(Frankenstein_Core_ConTags_go$0,    0, A0)
FALSE_EXTERN(Frankenstein_Core_ConTags_go$1,    1, A1)
FALSE_EXTERN(Frankenstein_Core_ConTags_pat_$0,  0, A0)

/* Core/CycleAnalysis */
FALSE_EXTERN(Frankenstein_Core_CycleAnalysis_go$0, 0, A0)
FALSE_EXTERN(Frankenstein_Core_CycleAnalysis_go$1, 1, A1)

/* Core/Evidence */
FALSE_EXTERN(Frankenstein_Core_Evidence_go$2, 2, A2)

/* Core/FlattenPatterns */
FALSE_EXTERN(Frankenstein_Core_FlattenPatterns_go$2,        2, A2)
FALSE_EXTERN(Frankenstein_Core_FlattenPatterns_patNested$0, 0, A0)

/* Core/Linker */
FALSE_EXTERN(Frankenstein_Core_Linker_go$0,             0, A0)
FALSE_EXTERN(Frankenstein_Core_Linker_go$1,             1, A1)
FALSE_EXTERN(Frankenstein_Core_Linker_go$2,             2, A2)
FALSE_EXTERN(Frankenstein_Core_Linker_nub$1,            1, A1)
FALSE_EXTERN(Frankenstein_Core_Linker_patternBinds$0,   0, A0)
FALSE_EXTERN(Frankenstein_Core_Linker_rewritePattern$0, 0, A0)

/* Core/Perceus */
FALSE_EXTERN(Frankenstein_Core_Perceus_go$0, 0, A0)
FALSE_EXTERN(Frankenstein_Core_Perceus_go$1, 1, A1)

/* GhcBridge/CoreTranslate */
FALSE_EXTERN(Frankenstein_GhcBridge_CoreTranslate_go$2, 2, A2)

/* KokaBridge/CoreTranslate */
FALSE_EXTERN(Frankenstein_KokaBridge_CoreTranslate_go$0, 0, A0)
FALSE_EXTERN(Frankenstein_KokaBridge_CoreTranslate_go$1, 1, A1)

/* MercuryBridge/HldsParse */
FALSE_EXTERN(Frankenstein_MercuryBridge_HldsParse_go$3,    3, A3)
FALSE_EXTERN(Frankenstein_MercuryBridge_HldsParse_tryOp$1, 1, A1)

/* MlirEmit/Emitter */
FALSE_EXTERN(Frankenstein_MlirEmit_Emitter_exprCallsPrint$1, 1, A1)
FALSE_EXTERN(Frankenstein_MlirEmit_Emitter_stripTypeLam$1,   1, A1)
FALSE_EXTERN(Frankenstein_MlirEmit_Emitter_tconName$1,       1, A1)
FALSE_EXTERN(Frankenstein_MlirEmit_Emitter_topLamArity$1,    1, A1)

/* RustBridge/MirParse */
FALSE_EXTERN(Frankenstein_RustBridge_MirParse_go$2, 2, A2)
FALSE_EXTERN(Frankenstein_RustBridge_MirParse_go$4, 4, A4)
