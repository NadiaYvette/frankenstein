/* Cross-module $N → base aliases — generation 2.
 * Companion to cross_module_aliases.c that handles symbols not yet
 * covered there.  Separate compilation unit so ALIAS0/ALIAS1
 * declarations of the same base do not conflict across files.
 */

#include <stdint.h>

extern int64_t kk_alloc_con(int64_t tag, int64_t nfields);
extern void    kk_set_field(int64_t ptr, int64_t idx, int64_t val);
extern int64_t kk_field(int64_t ptr, int64_t idx);
#define CLOS_TAG_AN 0x434C4F53

/* Trampoline for 1-arg closures: extracts the real fn from field 1
 * and calls fn(arg), discarding the closure pointer.  Matches the
 * convention in cross_module_shims.c. */
static int64_t trampoline_1arg_an(int64_t clos, int64_t arg) {
    int64_t fn = kk_field(clos, 1);
    typedef int64_t (*raw1_t)(int64_t);
    return ((raw1_t)(intptr_t)fn)(arg);
}

/* ALIAS0: function used as a value reference (e.g. `map f xs`).
 * Return a 2-field CLOS — field 0 = trampoline, field 1 = real fn ptr.
 * When the caller later does call1(clos, arg), it dispatches via the
 * trampoline.  This is the SAME pattern as cross_module_shims.c's
 * frkn_*_0 functions.
 *
 * Previously this macro did `return sym()` — wrong: it called sym
 * with no args, passing uninitialized rdi as if it were the
 * function's 1-arg parameter.  For 1-arg sym this corrupted the
 * downstream computation (e.g. etaExpandBuiltinAlias\$0 in stage 2
 * received a bind-closure as its def parameter via uninitialized rdi).
 */
#define ALIAS0(sym) \
    extern int64_t sym(int64_t); \
    int64_t sym##__a0(void) __asm__(#sym "$0"); \
    int64_t sym##__a0(void) { \
        int64_t c = kk_alloc_con(CLOS_TAG_AN, 2); \
        kk_set_field(c, 0, (int64_t)(intptr_t)&trampoline_1arg_an); \
        kk_set_field(c, 1, (int64_t)(intptr_t)&sym); \
        return c; \
    }
#define ALIAS1(sym) extern int64_t sym(int64_t); int64_t sym##__a1(int64_t a) __asm__(#sym "$1"); int64_t sym##__a1(int64_t a) { return sym(a); }
#define ALIAS2(sym) extern int64_t sym(int64_t, int64_t); int64_t sym##__a2(int64_t a, int64_t b) __asm__(#sym "$2"); int64_t sym##__a2(int64_t a, int64_t b) { return sym(a, b); }
#define ALIAS3(sym) extern int64_t sym(int64_t, int64_t, int64_t); int64_t sym##__a3(int64_t a, int64_t b, int64_t c) __asm__(#sym "$3"); int64_t sym##__a3(int64_t a, int64_t b, int64_t c) { return sym(a, b, c); }

ALIAS1(Frankenstein_GhcBridge_CoreTranslate_classifyBind)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_collectStaticShowList)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_collectValueLams)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_ghcIoOutputRuntime)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_isDictArg)
ALIAS2(Frankenstein_GhcBridge_CoreTranslate_isInfixOfStr)
ALIAS2(Frankenstein_GhcBridge_CoreTranslate_isPrefixOf)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_isRealWorldArg)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_isShowIntListMethod)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_isShowTupleSgo)
ALIAS2(Frankenstein_GhcBridge_CoreTranslate_pickShowArgs)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_qualifyName)
ALIAS2(Frankenstein_GhcBridge_CoreTranslate_translateAltCon)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_translateLit)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_translateName)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_translateTyCons)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_translateTyVar)
ALIAS1(Frankenstein_GhcBridge_CoreTranslate_translateType)
ALIAS2(Frankenstein_GhcBridge_CoreTranslate_unpackLitStringToCons)
ALIAS2(Frankenstein_GhcBridge_CoreTranslate_wrapLazyUses)
ALIAS0(Frankenstein_KokaBridge_CoreTranslate_translateProgram)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_extractTypeDecls)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_parseCtorApp)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_parseMercuryBuiltin)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_parseQualifiedOp)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_parseQuotedOpCall)
ALIAS2(Frankenstein_MercuryBridge_HldsParse_parseTupleLiteral)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_splitCtorArgs)
ALIAS2(Frankenstein_MlirEmit_Emitter_buildTopFnArity)
ALIAS1(Frankenstein_MlirEmit_Emitter_builtinWrapperSpec)
ALIAS1(Frankenstein_MlirEmit_Emitter_effectRowNameEmit)
ALIAS1(Frankenstein_MlirEmit_Emitter_emitCycleCandidate)
ALIAS1(Frankenstein_MlirEmit_Emitter_emitKokaBuiltins)
ALIAS0(Frankenstein_MlirEmit_Emitter_etaExpandBuiltinAlias)
ALIAS1(Frankenstein_MlirEmit_Emitter_floatVariant)
ALIAS1(Frankenstein_MlirEmit_Emitter_isBoolConCase)
ALIAS1(Frankenstein_MlirEmit_Emitter_isF64Bits)
/* skipped ALIAS0(Frankenstein_MlirEmit_Emitter_lookupType) — conflicts with ALIAS1 in this file */
ALIAS1(Frankenstein_MlirEmit_Emitter_lookupType)
/* skipped ALIAS0(Frankenstein_MlirEmit_Emitter_nameToSsa) — conflicts with ALIAS1 in this file */
ALIAS1(Frankenstein_MlirEmit_Emitter_nameToSsa)
ALIAS3(Frankenstein_MlirEmit_Emitter_precomputeCapturesWith)
ALIAS2(Frankenstein_MlirEmit_Emitter_qualifyBindName)
ALIAS1(Frankenstein_MlirEmit_Emitter_recordF64Bits)
ALIAS1(Frankenstein_MlirEmit_Emitter_sanitizeName)
ALIAS1(Frankenstein_RustBridge_MirParse_extractBodies)
ALIAS1(Frankenstein_RustBridge_MirParse_isCallTerm)
ALIAS2(Frankenstein_RustBridge_MirParse_jLookup)
ALIAS1(Frankenstein_RustBridge_MirParse_looksLikeTypeName)
ALIAS1(Frankenstein_RustBridge_MirParse_parseCallTerm)
ALIAS1(Frankenstein_RustBridge_MirParse_parseFieldAccess)
ALIAS0(Frankenstein_RustBridge_MirParse_parseOperand)
ALIAS1(Frankenstein_RustBridge_MirParse_parsePlace)
ALIAS1(Frankenstein_RustBridge_MirParse_parseStructFields)
ALIAS1(Frankenstein_RustBridge_MirParse_parseSwitchTargets)
ALIAS1(Frankenstein_RustBridge_MirParse_splitOperands)
