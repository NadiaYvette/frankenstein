/* Cross-module arity aliases for Frankenstein self-hosted binary.
 *
 * The emitter appends $N (arity) to unresolved external calls, e.g.
 * nameText$1 for a 1-arg call to nameText.  The definitions in the
 * self-hosted .o files don't carry the suffix.  These zero-overhead
 * assembly aliases bridge the gap.
 *
 * Generated from: nm -u on the self-host .o files
 */

.text

/* === Core/Types.hs selectors === */
.globl Frankenstein_Core_Types_bindExpr$1
.set   Frankenstein_Core_Types_bindExpr$1, Frankenstein_Core_Types_bindExpr

.globl Frankenstein_Core_Types_bindName$1
.set   Frankenstein_Core_Types_bindName$1, Frankenstein_Core_Types_bindName

.globl Frankenstein_Core_Types_bindSort$1
.set   Frankenstein_Core_Types_bindSort$1, Frankenstein_Core_Types_bindSort

.globl Frankenstein_Core_Types_bindType$1
.set   Frankenstein_Core_Types_bindType$1, Frankenstein_Core_Types_bindType

.globl Frankenstein_Core_Types_branchBody$1
.set   Frankenstein_Core_Types_branchBody$1, Frankenstein_Core_Types_branchBody

.globl Frankenstein_Core_Types_branchGuard$1
.set   Frankenstein_Core_Types_branchGuard$1, Frankenstein_Core_Types_branchGuard

.globl Frankenstein_Core_Types_branchPattern$1
.set   Frankenstein_Core_Types_branchPattern$1, Frankenstein_Core_Types_branchPattern

.globl Frankenstein_Core_Types_conFields$1
.set   Frankenstein_Core_Types_conFields$1, Frankenstein_Core_Types_conFields

.globl Frankenstein_Core_Types_conName$1
.set   Frankenstein_Core_Types_conName$1, Frankenstein_Core_Types_conName

.globl Frankenstein_Core_Types_dataCons$1
.set   Frankenstein_Core_Types_dataCons$1, Frankenstein_Core_Types_dataCons

.globl Frankenstein_Core_Types_dataName$1
.set   Frankenstein_Core_Types_dataName$1, Frankenstein_Core_Types_dataName

.globl Frankenstein_Core_Types_dataParams$1
.set   Frankenstein_Core_Types_dataParams$1, Frankenstein_Core_Types_dataParams

.globl Frankenstein_Core_Types_defExpr$1
.set   Frankenstein_Core_Types_defExpr$1, Frankenstein_Core_Types_defExpr

.globl Frankenstein_Core_Types_defName$1
.set   Frankenstein_Core_Types_defName$1, Frankenstein_Core_Types_defName

.globl Frankenstein_Core_Types_defSort$1
.set   Frankenstein_Core_Types_defSort$1, Frankenstein_Core_Types_defSort

.globl Frankenstein_Core_Types_defType$1
.set   Frankenstein_Core_Types_defType$1, Frankenstein_Core_Types_defType

.globl Frankenstein_Core_Types_defVisibility$1
.set   Frankenstein_Core_Types_defVisibility$1, Frankenstein_Core_Types_defVisibility

.globl Frankenstein_Core_Types_effectName$1
.set   Frankenstein_Core_Types_effectName$1, Frankenstein_Core_Types_effectName

.globl Frankenstein_Core_Types_effectOps$1
.set   Frankenstein_Core_Types_effectOps$1, Frankenstein_Core_Types_effectOps

.globl Frankenstein_Core_Types_nameText$1
.set   Frankenstein_Core_Types_nameText$1, Frankenstein_Core_Types_nameText

.globl Frankenstein_Core_Types_nameUnique$1
.set   Frankenstein_Core_Types_nameUnique$1, Frankenstein_Core_Types_nameUnique

.globl Frankenstein_Core_Types_opName$1
.set   Frankenstein_Core_Types_opName$1, Frankenstein_Core_Types_opName

.globl Frankenstein_Core_Types_progData$1
.set   Frankenstein_Core_Types_progData$1, Frankenstein_Core_Types_progData

.globl Frankenstein_Core_Types_progDefs$1
.set   Frankenstein_Core_Types_progDefs$1, Frankenstein_Core_Types_progDefs

.globl Frankenstein_Core_Types_progEffects$1
.set   Frankenstein_Core_Types_progEffects$1, Frankenstein_Core_Types_progEffects

.globl Frankenstein_Core_Types_progName$1
.set   Frankenstein_Core_Types_progName$1, Frankenstein_Core_Types_progName

.globl Frankenstein_Core_Types_qnameModule$1
.set   Frankenstein_Core_Types_qnameModule$1, Frankenstein_Core_Types_qnameModule

.globl Frankenstein_Core_Types_qnameName$1
.set   Frankenstein_Core_Types_qnameName$1, Frankenstein_Core_Types_qnameName

.globl Frankenstein_Core_Types_tcKind$1
.set   Frankenstein_Core_Types_tcKind$1, Frankenstein_Core_Types_tcKind

.globl Frankenstein_Core_Types_tcName$1
.set   Frankenstein_Core_Types_tcName$1, Frankenstein_Core_Types_tcName

.globl Frankenstein_Core_Types_tvKind$1
.set   Frankenstein_Core_Types_tvKind$1, Frankenstein_Core_Types_tvKind

.globl Frankenstein_Core_Types_tvName$1
.set   Frankenstein_Core_Types_tvName$1, Frankenstein_Core_Types_tvName

/* === Core/ConTags.hs === */
.globl Frankenstein_Core_ConTags_assignProgramTags$1
.set   Frankenstein_Core_ConTags_assignProgramTags$1, Frankenstein_Core_ConTags_assignProgramTags

.globl Frankenstein_Core_ConTags_conKey$1
.set   Frankenstein_Core_ConTags_conKey$1, Frankenstein_Core_ConTags_conKey

/* === GhcBridge/CoreTranslate.hs === */
.globl Frankenstein_GhcBridge_CoreTranslate_translateProgram$3
.set   Frankenstein_GhcBridge_CoreTranslate_translateProgram$3, Frankenstein_GhcBridge_CoreTranslate_translateProgram

/* === KokaBridge/CoreTranslate.hs === */
.globl Frankenstein_KokaBridge_CoreTranslate_translateProgram$1
.set   Frankenstein_KokaBridge_CoreTranslate_translateProgram$1, Frankenstein_KokaBridge_CoreTranslate_translateProgram

/* === MercuryBridge/HldsParse.hs selectors === */
.globl Frankenstein_MercuryBridge_HldsParse_hldsModule$1
.set   Frankenstein_MercuryBridge_HldsParse_hldsModule$1, Frankenstein_MercuryBridge_HldsParse_hldsModule

.globl Frankenstein_MercuryBridge_HldsParse_hldsPreds$1
.set   Frankenstein_MercuryBridge_HldsParse_hldsPreds$1, Frankenstein_MercuryBridge_HldsParse_hldsPreds

.globl Frankenstein_MercuryBridge_HldsParse_hldsTypes$1
.set   Frankenstein_MercuryBridge_HldsParse_hldsTypes$1, Frankenstein_MercuryBridge_HldsParse_hldsTypes

.globl Frankenstein_MercuryBridge_HldsParse_predArgNames$1
.set   Frankenstein_MercuryBridge_HldsParse_predArgNames$1, Frankenstein_MercuryBridge_HldsParse_predArgNames

.globl Frankenstein_MercuryBridge_HldsParse_predDet$1
.set   Frankenstein_MercuryBridge_HldsParse_predDet$1, Frankenstein_MercuryBridge_HldsParse_predDet

.globl Frankenstein_MercuryBridge_HldsParse_predGoal$1
.set   Frankenstein_MercuryBridge_HldsParse_predGoal$1, Frankenstein_MercuryBridge_HldsParse_predGoal

.globl Frankenstein_MercuryBridge_HldsParse_predModes$1
.set   Frankenstein_MercuryBridge_HldsParse_predModes$1, Frankenstein_MercuryBridge_HldsParse_predModes

.globl Frankenstein_MercuryBridge_HldsParse_predName$1
.set   Frankenstein_MercuryBridge_HldsParse_predName$1, Frankenstein_MercuryBridge_HldsParse_predName

.globl Frankenstein_MercuryBridge_HldsParse_typeDeclCtors$1
.set   Frankenstein_MercuryBridge_HldsParse_typeDeclCtors$1, Frankenstein_MercuryBridge_HldsParse_typeDeclCtors

.globl Frankenstein_MercuryBridge_HldsParse_typeDeclName$1
.set   Frankenstein_MercuryBridge_HldsParse_typeDeclName$1, Frankenstein_MercuryBridge_HldsParse_typeDeclName

.globl Frankenstein_MercuryBridge_HldsParse_typeDeclParams$1
.set   Frankenstein_MercuryBridge_HldsParse_typeDeclParams$1, Frankenstein_MercuryBridge_HldsParse_typeDeclParams

/* === MlirEmit/Dialects.hs === */
.globl Frankenstein_MlirEmit_Dialects_renderOp$1
.set   Frankenstein_MlirEmit_Dialects_renderOp$1, Frankenstein_MlirEmit_Dialects_renderOp

/* === RustBridge/MirParse.hs selectors === */
.globl Frankenstein_RustBridge_MirParse_bbIndex$1
.set   Frankenstein_RustBridge_MirParse_bbIndex$1, Frankenstein_RustBridge_MirParse_bbIndex

.globl Frankenstein_RustBridge_MirParse_bbStatements$1
.set   Frankenstein_RustBridge_MirParse_bbStatements$1, Frankenstein_RustBridge_MirParse_bbStatements

.globl Frankenstein_RustBridge_MirParse_bbTerminator$1
.set   Frankenstein_RustBridge_MirParse_bbTerminator$1, Frankenstein_RustBridge_MirParse_bbTerminator

.globl Frankenstein_RustBridge_MirParse_localIndex$1
.set   Frankenstein_RustBridge_MirParse_localIndex$1, Frankenstein_RustBridge_MirParse_localIndex

.globl Frankenstein_RustBridge_MirParse_localType$1
.set   Frankenstein_RustBridge_MirParse_localType$1, Frankenstein_RustBridge_MirParse_localType

.globl Frankenstein_RustBridge_MirParse_mirArgCount$1
.set   Frankenstein_RustBridge_MirParse_mirArgCount$1, Frankenstein_RustBridge_MirParse_mirArgCount

.globl Frankenstein_RustBridge_MirParse_mirBlocks$1
.set   Frankenstein_RustBridge_MirParse_mirBlocks$1, Frankenstein_RustBridge_MirParse_mirBlocks

.globl Frankenstein_RustBridge_MirParse_mirBodies$1
.set   Frankenstein_RustBridge_MirParse_mirBodies$1, Frankenstein_RustBridge_MirParse_mirBodies

.globl Frankenstein_RustBridge_MirParse_mirLocals$1
.set   Frankenstein_RustBridge_MirParse_mirLocals$1, Frankenstein_RustBridge_MirParse_mirLocals

.globl Frankenstein_RustBridge_MirParse_mirName$1
.set   Frankenstein_RustBridge_MirParse_mirName$1, Frankenstein_RustBridge_MirParse_mirName

.globl Frankenstein_RustBridge_MirParse_parseOperand$1
.set   Frankenstein_RustBridge_MirParse_parseOperand$1, Frankenstein_RustBridge_MirParse_parseOperand

.globl Frankenstein_RustBridge_MirParse_parseStmt$1
.set   Frankenstein_RustBridge_MirParse_parseStmt$1, Frankenstein_RustBridge_MirParse_parseStmt

.globl Frankenstein_RustBridge_MirParse_parseTerminator$1
.set   Frankenstein_RustBridge_MirParse_parseTerminator$1, Frankenstein_RustBridge_MirParse_parseTerminator

.globl Frankenstein_RustBridge_MirParse_switchBlock$1
.set   Frankenstein_RustBridge_MirParse_switchBlock$1, Frankenstein_RustBridge_MirParse_switchBlock

.globl Frankenstein_RustBridge_MirParse_switchVal$1
.set   Frankenstein_RustBridge_MirParse_switchVal$1, Frankenstein_RustBridge_MirParse_switchVal
