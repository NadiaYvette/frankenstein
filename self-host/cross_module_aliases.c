/* Cross-module arity aliases for Frankenstein self-hosted binary.
 *
 * The emitter appends $N (arity) to unresolved external calls, e.g.
 * nameText$1 for a 1-arg call to nameText.  The definitions in the
 * self-hosted .o files don't carry the suffix.  These thin C wrappers
 * bridge the gap using __asm__ labels for the $ symbol names.
 *
 * Generated from: nm -u on the self-host .o files
 */

#include <stdint.h>

/* Macro for 1-arg aliases (most common: record selectors) */
#define ALIAS1(sym) \
    extern int64_t sym(int64_t); \
    int64_t sym##__a1(int64_t a) __asm__(#sym "$1"); \
    int64_t sym##__a1(int64_t a) { return sym(a); }

/* Macro for N-arg aliases */
#define ALIAS2(sym) \
    extern int64_t sym(int64_t, int64_t); \
    int64_t sym##__a2(int64_t a, int64_t b) __asm__(#sym "$2"); \
    int64_t sym##__a2(int64_t a, int64_t b) { return sym(a, b); }

#define ALIAS3(sym) \
    extern int64_t sym(int64_t, int64_t, int64_t); \
    int64_t sym##__a3(int64_t a, int64_t b, int64_t c) __asm__(#sym "$3"); \
    int64_t sym##__a3(int64_t a, int64_t b, int64_t c) { return sym(a, b, c); }

/* Frankenstein_-prefixed aliases for stage2 MLIR.
 * Stage2 generates @frankenstein_Sym$N but the base aliases are Sym$N.
 * These route frankenstein_Sym$N → Sym$N (the unprefixed version). */
#define FALIAS0(sym) \
    extern int64_t sym##_dollar0(void) __asm__(#sym "$0"); \
    int64_t f_##sym##_fa0(void) __asm__("frankenstein_" #sym "$0"); \
    int64_t f_##sym##_fa0(void) { return sym##_dollar0(); }

#define FALIAS1(sym) \
    extern int64_t sym##_dollar1(int64_t) __asm__(#sym "$1"); \
    int64_t f_##sym##_fa1(int64_t a) __asm__("frankenstein_" #sym "$1"); \
    int64_t f_##sym##_fa1(int64_t a) { return sym##_dollar1(a); }

#define FALIAS3(sym) \
    extern int64_t sym##_dollar3(int64_t, int64_t, int64_t) __asm__(#sym "$3"); \
    int64_t f_##sym##_fa3(int64_t a, int64_t b, int64_t c) __asm__("frankenstein_" #sym "$3"); \
    int64_t f_##sym##_fa3(int64_t a, int64_t b, int64_t c) { return sym##_dollar3(a, b, c); }

/* Suffix-strip aliases for stage2: sym$N → sym (strip the arity suffix).
 * Use function pointers to avoid conflicting extern declarations when
 * both $0 and $1 are aliased for the same symbol. */
typedef int64_t (*fn0_t)(void);
typedef int64_t (*fn1_t)(int64_t);

#define SFALIAS0(sym) \
    int64_t s_##sym##_sa0(void) __asm__(#sym "$0"); \
    int64_t s_##sym##_sa0(void) { \
        extern char sym; /* just get the address */ \
        return ((fn0_t)(void*)&sym)(); \
    }

#define SFALIAS1(sym) \
    int64_t s_##sym##_sa1(int64_t a) __asm__(#sym "$1"); \
    int64_t s_##sym##_sa1(int64_t a) { \
        extern char sym; \
        return ((fn1_t)(void*)&sym)(a); \
    }

/* =================================================================== */
/*  Core/Types.hs selectors — all 1-arg                                */
/* =================================================================== */

ALIAS1(Frankenstein_Core_Types_bindExpr)
ALIAS1(Frankenstein_Core_Types_bindName)
ALIAS1(Frankenstein_Core_Types_bindSort)
ALIAS1(Frankenstein_Core_Types_bindType)
ALIAS1(Frankenstein_Core_Types_branchBody)
ALIAS1(Frankenstein_Core_Types_branchGuard)
ALIAS1(Frankenstein_Core_Types_branchPattern)
ALIAS1(Frankenstein_Core_Types_conFields)
ALIAS1(Frankenstein_Core_Types_conName)
ALIAS1(Frankenstein_Core_Types_dataCons)
ALIAS1(Frankenstein_Core_Types_dataName)
ALIAS1(Frankenstein_Core_Types_dataParams)
ALIAS1(Frankenstein_Core_Types_defExpr)
ALIAS1(Frankenstein_Core_Types_defName)
ALIAS1(Frankenstein_Core_Types_defSort)
ALIAS1(Frankenstein_Core_Types_defType)
ALIAS1(Frankenstein_Core_Types_defVisibility)
ALIAS1(Frankenstein_Core_Types_effectName)
ALIAS1(Frankenstein_Core_Types_effectOps)
ALIAS1(Frankenstein_Core_Types_nameText)
ALIAS1(Frankenstein_Core_Types_nameUnique)
ALIAS1(Frankenstein_Core_Types_opName)
ALIAS1(Frankenstein_Core_Types_progData)
ALIAS1(Frankenstein_Core_Types_progDefs)
ALIAS1(Frankenstein_Core_Types_progEffects)
ALIAS1(Frankenstein_Core_Types_progName)
ALIAS1(Frankenstein_Core_Types_qnameModule)
ALIAS1(Frankenstein_Core_Types_qnameName)
ALIAS1(Frankenstein_Core_Types_tcKind)
ALIAS1(Frankenstein_Core_Types_tcName)
ALIAS1(Frankenstein_Core_Types_tvKind)
ALIAS1(Frankenstein_Core_Types_tvName)

/* =================================================================== */
/*  Core/EffectOpt.hs                                                  */
/* =================================================================== */

ALIAS1(Frankenstein_Core_EffectOpt_isAbortHandler)

/* =================================================================== */
/*  Core/ConTags.hs                                                    */
/* =================================================================== */

ALIAS1(Frankenstein_Core_ConTags_assignProgramTags)
ALIAS1(Frankenstein_Core_ConTags_conKey)

/* =================================================================== */
/*  GhcBridge/CoreTranslate.hs                                        */
/* =================================================================== */

ALIAS3(Frankenstein_GhcBridge_CoreTranslate_translateProgram)

/* =================================================================== */
/*  KokaBridge/CoreTranslate.hs                                       */
/* =================================================================== */

ALIAS1(Frankenstein_KokaBridge_CoreTranslate_translateProgram)

/* =================================================================== */
/*  MercuryBridge/HldsParse.hs selectors                              */
/* =================================================================== */

ALIAS1(Frankenstein_MercuryBridge_HldsParse_hldsModule)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_hldsPreds)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_hldsTypes)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_predArgNames)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_predDet)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_predGoal)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_predModes)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_predName)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_typeDeclCtors)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_typeDeclName)
ALIAS1(Frankenstein_MercuryBridge_HldsParse_typeDeclParams)

/* =================================================================== */
/*  MlirEmit/Dialects.hs                                              */
/* =================================================================== */

ALIAS1(Frankenstein_MlirEmit_Dialects_renderOp)

/* =================================================================== */
/*  RustBridge/MirParse.hs selectors                                  */
/* =================================================================== */

ALIAS1(Frankenstein_RustBridge_MirParse_bbIndex)
ALIAS1(Frankenstein_RustBridge_MirParse_bbStatements)
ALIAS1(Frankenstein_RustBridge_MirParse_bbTerminator)
ALIAS1(Frankenstein_RustBridge_MirParse_localIndex)
ALIAS1(Frankenstein_RustBridge_MirParse_localType)
ALIAS1(Frankenstein_RustBridge_MirParse_mirArgCount)
ALIAS1(Frankenstein_RustBridge_MirParse_mirBlocks)
ALIAS1(Frankenstein_RustBridge_MirParse_mirBodies)
ALIAS1(Frankenstein_RustBridge_MirParse_mirLocals)
ALIAS1(Frankenstein_RustBridge_MirParse_mirName)
ALIAS1(Frankenstein_RustBridge_MirParse_parseOperand)
ALIAS1(Frankenstein_RustBridge_MirParse_parseStmt)
ALIAS1(Frankenstein_RustBridge_MirParse_parseTerminator)
ALIAS1(Frankenstein_RustBridge_MirParse_switchBlock)
ALIAS1(Frankenstein_RustBridge_MirParse_switchVal)

/* =================================================================== */
/*  OrganIR/Types.hs selectors (cross-module: Consumer → Types)        */
/* =================================================================== */

ALIAS1(OrganIR_Types_conFields)
ALIAS1(OrganIR_Types_conName)
ALIAS1(OrganIR_Types_defExpr)
ALIAS1(OrganIR_Types_defName)
ALIAS1(OrganIR_Types_defSort)
ALIAS1(OrganIR_Types_defType)
ALIAS1(OrganIR_Types_defVisibility)
ALIAS1(OrganIR_Types_dtConstructors)
ALIAS1(OrganIR_Types_dtName)
ALIAS1(OrganIR_Types_dtTypeParams)
ALIAS1(OrganIR_Types_edName)
ALIAS1(OrganIR_Types_edOperations)
ALIAS1(OrganIR_Types_edTypeParams)
ALIAS1(OrganIR_Types_irModule)
ALIAS1(OrganIR_Types_modDataTypes)
ALIAS1(OrganIR_Types_modDefs)
ALIAS1(OrganIR_Types_modEffectDecls)
ALIAS1(OrganIR_Types_modName)
ALIAS1(OrganIR_Types_opName)
ALIAS1(OrganIR_Types_opType)
ALIAS1(OrganIR_Types_qnModule)

/* =================================================================== */
/*  OrganIR/Parse.hs (cross-module: Consumer → Parse)                  */
/* =================================================================== */

ALIAS1(OrganIR_Parse_parseOrganIR)

/* =================================================================== */
/*  Stage 2: frankenstein_-prefixed arity aliases                       */
/*  Stage2 MLIR generates @frankenstein_Sym$N but base aliases are     */
/*  Sym$N (without prefix). Route frankenstein_Sym$N → Sym(args).      */
/* =================================================================== */

/* Core/Types selectors */
FALIAS0(Frankenstein_Core_Types_bindExpr)
FALIAS1(Frankenstein_Core_Types_bindExpr)
FALIAS1(Frankenstein_Core_Types_bindName)
FALIAS1(Frankenstein_Core_Types_bindSort)
FALIAS1(Frankenstein_Core_Types_bindType)
FALIAS1(Frankenstein_Core_Types_branchBody)
FALIAS1(Frankenstein_Core_Types_branchPattern)
FALIAS1(Frankenstein_Core_Types_conName)
FALIAS1(Frankenstein_Core_Types_dataCons)
FALIAS0(Frankenstein_Core_Types_defExpr)
FALIAS1(Frankenstein_Core_Types_defExpr)
FALIAS1(Frankenstein_Core_Types_defName)
FALIAS1(Frankenstein_Core_Types_defSort)
FALIAS1(Frankenstein_Core_Types_defType)
FALIAS1(Frankenstein_Core_Types_nameText)
FALIAS1(Frankenstein_Core_Types_progData)
FALIAS1(Frankenstein_Core_Types_progDefs)
FALIAS1(Frankenstein_Core_Types_qnameName)

/* GhcBridge/CoreTranslate */
FALIAS3(Frankenstein_GhcBridge_CoreTranslate_translateProgram)

/* =================================================================== */
/*  Stage 2: suffix-strip aliases                                       */
/*  Stage2 generates @frankenstein_sym$N but the function exists as     */
/*  @frankenstein_sym (no suffix). Route sym$N → sym(args).            */
/* =================================================================== */

/* GhcBridge/CoreTranslate */
SFALIAS0(frankenstein_Frankenstein_GhcBridge_CoreTranslate_isStateVar)
SFALIAS1(frankenstein_Frankenstein_GhcBridge_CoreTranslate_collectArgs)
SFALIAS1(frankenstein_Frankenstein_GhcBridge_CoreTranslate_collectForAlls)

/* KokaBridge/CoreTranslate */
SFALIAS1(frankenstein_Frankenstein_KokaBridge_CoreTranslate_extractCName)

/* MlirEmit/Dialects selectors */
SFALIAS0(frankenstein_Frankenstein_MlirEmit_Dialects_valName)
SFALIAS1(frankenstein_Frankenstein_MlirEmit_Dialects_valName)
SFALIAS0(frankenstein_Frankenstein_MlirEmit_Dialects_valType)
SFALIAS1(frankenstein_Frankenstein_MlirEmit_Dialects_valType)
SFALIAS1(frankenstein_Frankenstein_MlirEmit_Dialects_blockArgs)
SFALIAS1(frankenstein_Frankenstein_MlirEmit_Dialects_blockLabel)
SFALIAS1(frankenstein_Frankenstein_MlirEmit_Dialects_blockOps)
SFALIAS1(frankenstein_Frankenstein_MlirEmit_Dialects_funcBlocks)
