module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckMaxRatio(..))
import Test.Tasty (adjustOption)

import Frankenstein.Core.Types
import Frankenstein.Core.Perceus (insertPerceus, analyzeUsage, freeVars)
import Frankenstein.Core.Evidence (evidencePass, evidencePassGlobal, collectGlobalEffects)
import Frankenstein.Core.DeriveSelectors (deriveSelectors, recordSelectors)
import Frankenstein.Core.EffectOpt (effectOptimize, effectOptimizeWithStats, EffectOptStats(..)
  , inlineLocalHandlers, eliminateIdentityHandlers, annotateTailResumptive, isAbortHandler
  , HandlerKind(..), classifyHandler)
import Frankenstein.Core.CpsConvert (cpsTopLevel)
import Frankenstein.Core.TypeCheck (typeCheckProgram, TypeError(..))
import Frankenstein.Core.FlattenPatterns (flattenPatternsExpr)
import Frankenstein.Core.ConTags (assignProgramTags, collectReferencedCtors)
import Frankenstein.MercuryBridge.HldsParse
  ( MercuryHLDS(..), MercuryPred(..), MercuryTypeDecl(..)
  , MercuryDet(..), MercuryMode(..), MercuryGoal(..)
  )
import Frankenstein.MercuryBridge.CoreTranslate (translateHlds)
import Frankenstein.Core.Linker (linkPrograms, linkProgramsWith, LinkResult(..), LinkError(..))
import Frankenstein.Core.Interface
import Frankenstein.GhcBridge.Plugin (PluginConfig(..), parsePluginOpts)
import Frankenstein.Emit.C11 (emitOrganC11)
import Frankenstein.Linker.PolyglotLink
  ( TranslationUnit(..)
  , FfiEdgeCheck(..)
  , LinkPlan(..)
  , buildPolyglotLinkPlan
  , renderExternHeaderC11
  )
import Frankenstein.Verify.LiquidProfile
  ( ArithExpr(..)
  , RefPred(..)
  , LiquidContract(..)
  , OptWitness(..)
  , ProfilePromotion(..)
  , emitSmtLib2Expr
  , emitSmtLib2Pred
  , emitSmtLib2Query
  , proveStaticBounds
  , promoteProfile
  , dischargeLinearityAndBounds
  , runZ3Query
  )
import Frankenstein.Verify.SynquidHole
  ( HoleSort(..)
  , SynthComponent(..)
  , HoleGoal(..)
  , SynthOutcome(..)
  , synthesizeHole
  )
import Frankenstein.Backpack.OpenRecordSig
  ( TypeFieldSpec(..)
  , ValueFieldSpec(..)
  , ModuleSig(..)
  , SigMorphism(..)
  , SigMergeError(..)
  , emptyModuleSig
  , applySigMorphism
  , mergeModuleSigs
  )
import Frankenstein.Backpack.StageInstantiate
  ( ModuleImpl(..)
  , ParameterizedModule(..)
  , SheafObstruction(..)
  , InstantiatedModule(..)
  , checkBehavioralEdge
  , inlineProviderRefs
  , substituteValueRefs
  , verifyModuleGraphSheaf
  , partialInstantiate
  , instantiateAndSpecialize
  )
import Frankenstein.Backpack.HsigBridge
  ( HsigParseError(..)
  , countArrowArity
  , parseLaw
  , parseHsigToOpenRecord
  )
import Frankenstein.Driver.Registry
  ( ParadigmTag(..)
  , IngestStrategy(..)
  , EmitStrategy(..)
  , FfiTypeRule(..)
  , FormatterHook(..)
  , DriverManifest(..)
  , DriverRegistry(..)
  , DriverError(..)
  , emptyRegistry
  , builtInDrivers
  , loadBuiltInRegistry
  , registerDriver
  , upsertDriverOverride
  , lookupById
  , lookupByExtension
  , validateRegistry
  )
import Frankenstein.Style.Profile
  ( NamingCase(..)
  , IdiomPrefs(..)
  , StyleProfile(..)
  , CognitiveMetrics(..)
  , defaultStyleForParadigm
  , formatIdentifier
  , learnStyleProfile
  , stylePenalty
  , rankEquivalentCandidates
  , styleProfileToDhall
  )
import Frankenstein.Emit.Idiomatic
  ( TranspileTarget(..)
  , OwnershipRustType(..)
  , mapProfileToRustOwnership
  , sanitizeAbiSymbol
  , emitIdiomaticRustModule
  , emitPythonCapiExtension
  , emitFortranIsoCBinding
  , emitRocqSpecSkeleton
  , applyFormatterHook
  )
import Frankenstein.Transform.Api
  ( SourceSpan(..)
  , SourceEdit(..)
  , TransformMode(..)
  , TransformKind(..)
  , TransformRequest(..)
  , TransformWitness(..)
  , TransformReport(..)
  , applySourceEdits
  , renderUnifiedDiffSimple
  , renderLspWorkspaceEditJson
  , runTransformSession
  )
import Frankenstein.Db.Incremental
  ( Revision(..)
  , Durability(..)
  , QueryKey(..)
  , InputCell(..)
  , MemoEntry(..)
  , CodeDb(..)
  , emptyCodeDb
  , lastRevAtDurability
  , setInput
  , isEntryVerified
  , recordMemoWithCutoff
  , runTrackedQuery
  , runTrackedQueryWithStack
  )
import Frankenstein.Db.ResilientEditCone
  ( DeclState(..)
  , ResilientDecl(..)
  , ResilientModule(..)
  , EditConeReport(..)
  , parseResilientModule
  , reparseEditCone
  , validSiblingDecls
  , collectParseHoles
  , syncModuleIntoCodeDb
  , resilientModuleToInterface
  )
import Frankenstein.Ide.LiveSession
  ( DiagnosticSeverity(..)
  , LiveDiagnostic(..)
  , StyleAxis(..)
  , StyleFeedbackKind(..)
  , IdeCodeAction(..)
  , LiveIdeSession(..)
  , DidChangeResult(..)
  , initLiveSession
  , applyBufferChange
  , recordStyleFeedback
  , proposeCodeActions
  )
import Frankenstein.RTS.UringDriver
import qualified Frankenstein.Opt.EGraph as Opt
import System.Directory (doesFileExist)
import Frankenstein.MlirEmit.Emitter (emitProgram, emitProgramWasm, emitProgramWithEffects)
import KOracle (kOracleTests)
import BridgeBisim (bridgeBisimTests)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text (Text)

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mkName :: Text -> Name
mkName t = Name t 0

mkQName :: Text -> Text -> QName
mkQName m n = QName m (mkName n)

intType :: Type
intType = TCon (TypeCon (mkQName "std" "int") KindValue)

anyType :: Type
anyType = TCon (TypeCon (mkQName "std" "any") KindValue)

unitType :: Type
unitType = TCon (TypeCon (mkQName "std" "unit") KindValue)

-- | Build a simple program with given defs
mkProgram :: Text -> Text -> [Def] -> Program
mkProgram modName progN defs = Program
  { progName    = mkQName modName progN
  , progDefs    = defs
  , progData    = []
  , progEffects = []
  }

-- | Build a simple function definition
mkFunDef :: Text -> Text -> Expr -> Type -> Def
mkFunDef modName name expr ty = Def
  { defName       = mkQName modName name
  , defType       = ty
  , defExpr       = expr
  , defSort       = DefFun
  , defVisibility = Public
  }

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "Frankenstein"
  [ coreIRTests
  , perceusTests
  , evidenceTests
  , effectOptTests
  , cpsConvertTests
  , typeCheckTests
  , linkerTests
  , mlirEmitTests
  , flattenPatternsTests
  , deriveSelectorsTests
  , conTagsTests
  , mercuryAdtTests
  , wasmEmitTests
  , organInterfaceTests
  , ghcPluginTests
  , c11EmitTests
  , polyglotLinkTests
  , liquidProfileTests
  , optEGraphTests
  , synquidHoleTests
  , backpackOpenRecordSigTests
  , backpackStageInstantiateTests
  , backpackHsigBridgeTests
  , driverRegistryTests
  , styleProfileTests
  , emitIdiomaticTests
  , transformApiTests
  , dbIncrementalTests
  , resilientEditConeTests
  , ideLiveSessionTests
  , rtsUringDriverTests
  , adjustOption (\_ -> QuickCheckMaxRatio 200) kOracleTests
  , bridgeBisimTests
  ]

-------------------------------------------------------------------------------
-- A. Core IR unit tests
-------------------------------------------------------------------------------

coreIRTests :: TestTree
coreIRTests = testGroup "Core IR"
  [ testCase "freeVars: variable" $
      freeVars (EVar (mkName "x")) @?= Set.singleton (mkName "x")

  , testCase "freeVars: literal has no free vars" $
      freeVars (ELit (LitInt 42)) @?= Set.empty

  , testCase "freeVars: lambda binds its parameter" $
      freeVars (ELam [(mkName "x", intType)] (EVar (mkName "x")))
        @?= Set.empty

  , testCase "freeVars: lambda with free var in body" $
      freeVars (ELam [(mkName "x", intType)]
                  (EApp (EVar (mkName "f")) [EVar (mkName "x")]))
        @?= Set.singleton (mkName "f")

  , testCase "freeVars: let binds its names" $
      let bind = Bind (mkName "x") intType (ELit (LitInt 1)) DefVal
      in freeVars (ELet [[bind]] (EVar (mkName "x")))
           @?= Set.empty

  , testCase "freeVars: let body references unbound name" $
      let bind = Bind (mkName "x") intType (ELit (LitInt 1)) DefVal
      in freeVars (ELet [[bind]] (EApp (EVar (mkName "f")) [EVar (mkName "x")]))
           @?= Set.singleton (mkName "f")

  , testCase "freeVars: application collects from fn and args" $
      freeVars (EApp (EVar (mkName "f")) [EVar (mkName "x"), EVar (mkName "y")])
        @?= Set.fromList [mkName "f", mkName "x", mkName "y"]

  , testCase "freeVars: case collects from scrutinee and branches" $
      let branches = [ Branch (PatVar (mkName "a") intType) Nothing
                               (EVar (mkName "a"))
                     ]
      in freeVars (ECase (EVar (mkName "s")) branches)
           @?= Set.singleton (mkName "s")
           -- "a" is bound by the pattern, "s" is free

  , testCase "freeVars: EDrop/ERetain pass through" $ do
      freeVars (EDrop (EVar (mkName "x"))) @?= Set.singleton (mkName "x")
      freeVars (ERetain (EVar (mkName "y"))) @?= Set.singleton (mkName "y")
  ]

-------------------------------------------------------------------------------
-- B. Perceus pass tests
-------------------------------------------------------------------------------

perceusTests :: TestTree
perceusTests = testGroup "Perceus"
  [ testCase "analyzeUsage: single variable" $
      analyzeUsage (EVar (mkName "x")) @?= Map.singleton (mkName "x") 1

  , testCase "analyzeUsage: variable used twice in app" $
      analyzeUsage (EApp (EVar (mkName "f")) [EVar (mkName "x"), EVar (mkName "x")])
        @?= Map.fromList [(mkName "f", 1), (mkName "x", 2)]

  , testCase "analyzeUsage: literal has no usage" $
      analyzeUsage (ELit (LitInt 0)) @?= Map.empty

  , testCase "analyzeUsage: nested application" $
      let expr = EApp (EVar (mkName "+"))
                   [ EVar (mkName "a")
                   , EApp (EVar (mkName "*")) [EVar (mkName "a"), EVar (mkName "b")]
                   ]
      in do
        Map.lookup (mkName "a") (analyzeUsage expr) @?= Just 2
        Map.lookup (mkName "b") (analyzeUsage expr) @?= Just 1
        Map.lookup (mkName "+") (analyzeUsage expr) @?= Just 1

  , testCase "insertPerceus: drops unused let binding" $
      let bind = Bind (mkName "unused") intType (ELit (LitInt 99)) DefVal
          body = ELit (LitInt 0)  -- does not reference "unused"
          prog = mkProgram "test" "perceus"
            [ mkFunDef "test" "f"
                (ELet [[bind]] body)
                anyType
            ]
          result = insertPerceus prog
          resultExpr = defExpr (head (progDefs result))
      in assertBool "output should contain EDrop for unused binding"
           (containsDrop (mkName "unused") resultExpr)

  , testCase "insertPerceus: drops unused binding regardless of arg multiplicity" $
      -- A binding with a linear-argument function type should still get dropped
      -- if unused, because the binding's own multiplicity is Many (the Linear
      -- only applies to the function's parameter, not the binding itself).
      let linearArgType = TFun [(Linear, intType)] EffectRowEmpty intType
          bind = Bind (mkName "x") linearArgType (ELit (LitInt 42)) DefVal
          body = ELit (LitInt 0)  -- does not reference "x"
          prog = mkProgram "test" "perceus"
            [ mkFunDef "test" "f"
                (ELet [[bind]] body)
                anyType
            ]
          result = insertPerceus prog
          resultExpr = defExpr (head (progDefs result))
      in assertBool "unused binding should get a drop even with linear arg type"
           (containsDrop (mkName "x") resultExpr)

  , testCase "insertPerceus: preserves used bindings" $
      let bind = Bind (mkName "y") intType (ELit (LitInt 5)) DefVal
          body = EVar (mkName "y")
          prog = mkProgram "test" "perceus"
            [ mkFunDef "test" "g"
                (ELet [[bind]] body)
                anyType
            ]
          result = insertPerceus prog
          resultExpr = defExpr (head (progDefs result))
      in assertBool "used binding should NOT be dropped"
           (not (containsDrop (mkName "y") resultExpr))
  ]

-- | Check whether an expression tree contains EDrop (EVar name)
containsDrop :: Name -> Expr -> Bool
containsDrop n (EDrop (EVar n'))    = n == n'
containsDrop n (EApp f args)        = containsDrop n f || any (containsDrop n) args
containsDrop n (ELam _ body)        = containsDrop n body
containsDrop n (ELet bgs body)      = any (\bg -> any (containsDrop n . bindExpr) bg) bgs
                                      || containsDrop n body
containsDrop n (ECase s brs)        = containsDrop n s
                                      || any (containsDrop n . branchBody) brs
containsDrop n (ERetain e)          = containsDrop n e
containsDrop n (ERelease e)         = containsDrop n e
containsDrop n (EDrop e)            = containsDrop n e
containsDrop n (EReuse e1 e2)       = containsDrop n e1 || containsDrop n e2
containsDrop n (EDelay e)           = containsDrop n e
containsDrop n (EForce e)           = containsDrop n e
containsDrop n (ETypeApp e _)       = containsDrop n e
containsDrop n (ETypeLam _ e)       = containsDrop n e
containsDrop n (EPerform _ args)    = any (containsDrop n) args
containsDrop n (EHandle _ h b)      = containsDrop n h || containsDrop n b
containsDrop _ _                    = False

-------------------------------------------------------------------------------
-- C. Evidence pass tests
-------------------------------------------------------------------------------

evidenceTests :: TestTree
evidenceTests = testGroup "Evidence"
  [ testCase "evidencePass eliminates EHandle/EPerform" $
      -- Naming convention:
      --   EffectRow QName: QName "" "exn" → effectRowName = "exn"
      --   EPerform QName:  QName "exn" "raise" → effName = "exn" (matches!)
      --   EffectDecl:      effectName = QName "" "exn" → flattened = "exn"
      let exnEffect = EffectRowExtend (mkQName "" "exn") EffectRowEmpty
          -- A handler that catches the exception
          handler = ELam [(mkName "e", anyType)] (ELit (LitInt 0))
          -- A body that performs the effect
          body = EPerform (mkQName "exn" "raise") [ELit (LitString "boom")]
          -- Handle wrapping
          handleExpr = EHandle exnEffect handler body
          prog = mkProgram "test" "evidence"
            [ mkFunDef "test" "f" handleExpr anyType ]
          effDecl = EffectDecl
            { effectName   = mkQName "" "exn"
            , effectParams = []
            , effectOps    = [OpDecl (mkQName "exn" "raise") anyType]
            }
          progWithEffects = prog { progEffects = [effDecl] }
          result = evidencePass progWithEffects
          resultExpr = defExpr (head (progDefs result))
      in do
        assertBool "should not contain EHandle after evidence pass"
          (not (containsHandle resultExpr))
        assertBool "should not contain EPerform after evidence pass"
          (not (containsPerform resultExpr))
        -- Verify the handler was actually bound (not falling through to default)
        assertBool "evidence pass should bind handler as ev_exn (let-expression)"
          (isELet resultExpr)

  , testCase "evidencePass: unhandled effect becomes default call" $
      let body = EPerform (mkQName "exn" "raise") [ELit (LitString "err")]
          prog = mkProgram "test" "ev2"
            [ mkFunDef "test" "g" body anyType ]
          result = evidencePass prog
          resultExpr = defExpr (head (progDefs result))
      in assertBool "unhandled perform should become EApp (function call)"
           (isEApp resultExpr)

  , testCase "cross-module: evidencePassGlobal resolves effects across modules" $
      -- Module B: declares exn effect, has checker fn that performs it
      let exnEffDecl = EffectDecl
            { effectName   = mkQName "mercury" "exn"
            , effectParams = []
            , effectOps    = [OpDecl (mkQName "mercury" "raise") anyType]
            }
          modB = (mkProgram "modB" "checker"
            [ mkFunDef "modB" "checker"
                (EPerform (mkQName "mercuryexn" "raise") [ELit (LitInt 42)])
                anyType
            ]) { progEffects = [exnEffDecl] }
          -- Module A: main handles exn, calls checker from modB
          modA = mkProgram "modA" "main"
            [ mkFunDef "modA" "main"
                (EHandle
                  (EffectRowExtend (mkQName "mercury" "exn") EffectRowEmpty)
                  (ELam [(mkName "e", anyType)] (ELit (LitInt 0)))
                  (EApp (EVar (mkName "checker")) []))
                anyType
            ]
      in case linkProgramsWith False [modA, modB] of
          Left errs -> assertFailure $ "Link failed: " ++ show errs
          Right lr ->
            let merged = lrProgram lr
                globalEffects = collectGlobalEffects merged
                result = evidencePassGlobal globalEffects merged
                mainDef = head
                  [ d | d <- progDefs result
                  , T.isInfixOf "main" (nameText (qnameName (defName d)))
                  ]
            in do
              assertBool "cross-module: no EHandle after global evidence pass"
                (not (containsHandle (defExpr mainDef)))
              assertBool "cross-module: no EPerform after global evidence pass"
                (not (containsPerform (defExpr mainDef)))

  , testCase "cross-module: abort handler across modules produces kk_handler_exec" $
      -- Module B (e.g. Mercury): declares exn effect, has a function that performs it
      let exnEffDecl = EffectDecl
            { effectName   = mkQName "mercury" "exn"
            , effectParams = []
            , effectOps    = [OpDecl (mkQName "mercuryexn" "raise") anyType]
            }
          modB = (mkProgram "modB" "thrower"
            [ mkFunDef "modB" "thrower"
                (EPerform (mkQName "mercuryexn" "raise") [ELit (LitInt 42)])
                anyType
            ]) { progEffects = [exnEffDecl] }
          -- Module A (e.g. Haskell): main handles exn with an ABORT handler, calls thrower
          -- Abort handler: \msg resume -> msg  (resume unused → abort)
          modA = mkProgram "modA" "main"
            [ mkFunDef "modA" "main"
                (EHandle
                  (EffectRowExtend (mkQName "mercury" "exn") EffectRowEmpty)
                  (ELam [(Name "msg" 1, anyType), (Name "resume" 2, anyType)]
                    (EVar (Name "msg" 1)))
                  (EApp (EVar (mkName "thrower")) []))
                anyType
            ]
      in case linkProgramsWith False [modA, modB] of
          Left errs -> assertFailure $ "Link failed: " ++ show errs
          Right lr ->
            let merged = lrProgram lr
                globalEffects = collectGlobalEffects merged
                result = evidencePassGlobal globalEffects merged
                mainDef = head
                  [ d | d <- progDefs result
                  , T.isInfixOf "main" (nameText (qnameName (defName d)))
                  ]
                throwerDef = head
                  [ d | d <- progDefs result
                  , T.isInfixOf "thrower" (nameText (qnameName (defName d)))
                  ]
            in do
              assertBool "cross-module abort: no EHandle after evidence pass"
                (not (containsHandle (defExpr mainDef)))
              assertBool "cross-module abort: no EPerform after evidence pass"
                (not (containsPerform (defExpr throwerDef)))
              -- Abort handler should produce kk_handler_exec (callback-based setjmp)
              assertBool "cross-module abort: main contains kk_handler_exec"
                (containsVarNamed "kk_handler_exec" (defExpr mainDef))
              -- Cross-module perform: thrower's EPerform becomes a default function
              -- call (mercuryexn_raise), NOT kk_handler_abort — the abort dispatch
              -- happens at runtime via the handler stack, not at compile time.
              assertBool "cross-module abort: thrower's perform becomes default call"
                (containsVarNamed "mercuryexn_raise" (defExpr throwerDef))

  , testCase "cross-module: abort handler MLIR output compiles correctly" $
      -- Same cross-module setup but verify the MLIR output is well-formed
      let intType' = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
          exnEffDecl = EffectDecl
            { effectName   = mkQName "mercury" "exn"
            , effectParams = []
            , effectOps    = [OpDecl (mkQName "mercuryexn" "raise") intType']
            }
          modB = (mkProgram "modB" "thrower"
            [ Def
                { defName = QName "modB" (Name "thrower" 0)
                , defType = TFun [] EffectRowEmpty intType'
                , defExpr = EPerform (mkQName "mercuryexn" "raise") [ELit (LitInt 42)]
                , defSort = DefFun
                , defVisibility = Public
                }
            ]) { progEffects = [exnEffDecl] }
          modA = mkProgram "modA" "main"
            [ Def
                { defName = QName "" (Name "main" 10)
                , defType = TFun [] EffectRowEmpty intType'
                , defExpr =
                    EHandle
                      (EffectRowExtend (mkQName "mercury" "exn") EffectRowEmpty)
                      (ELam [(Name "msg" 1, intType'), (Name "resume" 2, intType')]
                        (EVar (Name "msg" 1)))
                      (EApp (EVar (Name "thrower" 0)) [])
                , defSort = DefFun
                , defVisibility = Public
                }
            ]
      in case linkProgramsWith False [modA, modB] of
          Left errs -> assertFailure $ "Link failed: " ++ show errs
          Right lr ->
            let merged = lrProgram lr
                globalEffects = collectGlobalEffects merged
                evidenced = evidencePassGlobal globalEffects merged
                mlir = emitProgram evidenced
            in do
              -- MLIR should contain handler runtime calls
              assertBool "cross-module abort MLIR: contains @kk_handler_exec"
                (T.isInfixOf "kk_handler_exec" mlir)
              assertBool "cross-module abort MLIR: contains @kk_handler_abort"
                (T.isInfixOf "kk_handler_abort" mlir)
              -- Should NOT contain residual effect dialect ops
              assertBool "cross-module abort MLIR: no frankenstein.handle"
                (not (T.isInfixOf "frankenstein.handle" mlir))
              assertBool "cross-module abort MLIR: no frankenstein.perform"
                (not (T.isInfixOf "frankenstein.perform" mlir))
  ]

containsHandle :: Expr -> Bool
containsHandle (EHandle {})        = True
containsHandle (EApp f args)       = containsHandle f || any containsHandle args
containsHandle (ELam _ body)       = containsHandle body
containsHandle (ELet bgs body)     = any (\bg -> any (containsHandle . bindExpr) bg) bgs
                                     || containsHandle body
containsHandle (ECase s brs)       = containsHandle s || any (containsHandle . branchBody) brs
containsHandle _                   = False

containsPerform :: Expr -> Bool
containsPerform (EPerform {})      = True
containsPerform (EApp f args)      = containsPerform f || any containsPerform args
containsPerform (ELam _ body)      = containsPerform body
containsPerform (ELet bgs body)    = any (\bg -> any (containsPerform . bindExpr) bg) bgs
                                     || containsPerform body
containsPerform (ECase s brs)      = containsPerform s || any (containsPerform . branchBody) brs
containsPerform _                  = False

isEApp :: Expr -> Bool
isEApp (EApp {}) = True
isEApp _         = False

isELet :: Expr -> Bool
isELet (ELet {}) = True
isELet _         = False

containsVarNamed :: Text -> Expr -> Bool
containsVarNamed name (EVar n)       = T.isInfixOf name (nameText n)
containsVarNamed name (EApp f args)  = containsVarNamed name f || any (containsVarNamed name) args
containsVarNamed name (ELam _ body)  = containsVarNamed name body
containsVarNamed name (ELet bgs body)= any (\bg -> any (containsVarNamed name . bindExpr) bg) bgs
                                        || containsVarNamed name body
containsVarNamed name (ECase s brs)  = containsVarNamed name s
                                        || any (containsVarNamed name . branchBody) brs
containsVarNamed _ _                 = False

-------------------------------------------------------------------------------
-- D. Effect Optimization tests
-------------------------------------------------------------------------------

effectOptTests :: TestTree
effectOptTests = testGroup "Effect Optimization"
  [ testCase "eliminateIdentityHandlers: removes identity handler (uncurried)" $
      -- EHandle eff (\x k -> k(x)) body  =>  body
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)])
          body = ELit (LitInt 42)
          expr = EHandle (EffectRowExtend exnEff EffectRowEmpty) handler body
          result = eliminateIdentityHandlers expr
      in result @?= body

  , testCase "eliminateIdentityHandlers: removes identity handler (curried)" $
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType)]
                      (ELam [(Name "k" 2, intType)]
                        (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)]))
          body = ELit (LitInt 42)
          expr = EHandle (EffectRowExtend exnEff EffectRowEmpty) handler body
          result = eliminateIdentityHandlers expr
      in result @?= body

  , testCase "eliminateIdentityHandlers: keeps non-identity handler" $
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (ELit (LitInt 0))  -- doesn't resume
          body = ELit (LitInt 42)
          expr = EHandle (EffectRowExtend exnEff EffectRowEmpty) handler body
          result = eliminateIdentityHandlers expr
      in case result of
        EHandle {} -> pure ()
        _          -> assertFailure "should preserve non-identity handler"

  , testCase "eliminateIdentityHandlers: reaches handler inside ELet body" $
      -- Regression: previously the pass used stub traversals that skipped
      -- ELet bindings and ECase branches, so buried handlers were missed.
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)])
          innerBody = ELit (LitInt 42)
          handleExpr = EHandle (EffectRowExtend exnEff EffectRowEmpty) handler innerBody
          -- Wrap the handle inside a let-body
          outerExpr = ELet [[Bind (mkName "dummy") intType (ELit (LitInt 0)) DefVal]]
                        handleExpr
          result = eliminateIdentityHandlers outerExpr
      in case result of
        ELet _ (ELit (LitInt 42)) -> pure ()  -- handler eliminated
        _ -> assertFailure $ "expected identity handler inside ELet to be eliminated, got: "
              ++ show result

  , testCase "eliminateIdentityHandlers: reaches handler inside ECase branch" $
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)])
          handleExpr = EHandle (EffectRowExtend exnEff EffectRowEmpty) handler (ELit (LitInt 99))
          caseExpr = ECase (ELit (LitInt 0))
                       [Branch (PatWild intType) Nothing handleExpr]
          result = eliminateIdentityHandlers caseExpr
      in assertBool "identity handler in case branch should be eliminated"
           (not (containsHandle result))

  , testCase "eliminateIdentityHandlers: reaches handler inside EApp argument" $
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)])
          handleExpr = EHandle (EffectRowExtend exnEff EffectRowEmpty) handler (ELit (LitInt 7))
          appExpr = EApp (EVar (mkName "f")) [handleExpr]
          result = eliminateIdentityHandlers appExpr
      in assertBool "identity handler in app argument should be eliminated"
           (not (containsHandle result))

  , testCase "effectOptimizeWithStats: reports zero stats for no-effect program" $
      let prog = mkProgram "test" "t"
            [ mkFunDef "test" "id"
                (ELam [(mkName "x", intType)] (EVar (mkName "x")))
                (TFun [(Many, intType)] EffectRowEmpty intType)
            ]
          (_prog', stats) = effectOptimizeWithStats prog
      in do
        eosInlined stats @?= 0
        eosEliminated stats @?= 0
        eosTailRes stats @?= 0

  , testCase "emitProgramWithEffects: contains frankenstein comment header" $
      let prog = mkProgram "test" "t" []
          mlir = emitProgramWithEffects prog
      in assertBool "should contain effect dialect comment"
           (T.isInfixOf "Effect Dialect" mlir || T.isInfixOf "effect" (T.toLower mlir))

  , testCase "emitProgramWithEffects: program with EHandle emits frankenstein.handle" $
      let exnEff = QName "std" (mkName "exn")
          handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)])
          body = ELit (LitInt 42)
          prog = mkProgram "test" "t"
            [ mkFunDef "test" "main"
                (EHandle (EffectRowExtend exnEff EffectRowEmpty) handler body)
                (TFun [] EffectRowEmpty intType)
            ]
          mlir = emitProgramWithEffects prog
      in assertBool "should contain frankenstein.handle"
           (T.isInfixOf "frankenstein.handle" mlir)

  , testCase "emitProgramWithEffects: program with EPerform emits frankenstein.perform" $
      let exnEff = QName "std" (mkName "raise")
          prog = mkProgram "test" "t"
            [ mkFunDef "test" "main"
                (EPerform exnEff [ELit (LitInt 1)])
                (TFun [] EffectRowEmpty intType)
            ]
          mlir = emitProgramWithEffects prog
      in assertBool "should contain frankenstein.perform"
           (T.isInfixOf "frankenstein.perform" mlir)

  , testCase "isAbortHandler: single-param handler is NOT abort" $
      -- \msg -> 0  (single param → tail-resumptive, not abort)
      let handler = ELam [(Name "msg" 1, intType)] (ELit (LitInt 0))
      in assertBool "single-param handler should NOT be abort" (not (isAbortHandler handler))

  , testCase "isAbortHandler: two-param handler with resume call is NOT abort" $
      -- \x k -> k(x)  (tail-resumptive, uses resume)
      let handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EApp (EVar (Name "k" 2)) [EVar (Name "x" 1)])
      in assertBool "tail-resumptive handler should NOT be abort" (not (isAbortHandler handler))

  , testCase "isAbortHandler: two-param handler without resume is abort" $
      -- \x k -> x  (never calls k → abort handler)
      let handler = ELam [(Name "x" 1, intType), (Name "k" 2, intType)]
                      (EVar (Name "x" 1))
      in assertBool "handler ignoring resume should be abort" (isAbortHandler handler)

  , testCase "inlineLocalHandlers: skips abort handlers" $
      -- Abort handlers must NOT be inlined (inlining is semantically wrong
      -- because the continuation after perform would still execute).
      -- Use a 2-param handler with unused resume → abort.
      let exnEff = EffectRowExtend (QName "" (Name "exn" 0)) EffectRowEmpty
          handler = ELam [(Name "msg" 1, intType), (Name "k" 2, intType)]
                      (EVar (Name "msg" 1))
          body = EPerform (QName "exn" (Name "raise" 0)) [ELit (LitInt 42)]
          expr = EHandle exnEff handler body
          result = inlineLocalHandlers expr
      in assertBool "abort handler should NOT be inlined (EHandle should remain)"
           (containsHandle result)

  , testCase "evidencePass: abort handler emits kk_handler_exec/abort" $
      -- Verify the evidence pass generates callback-based setjmp/longjmp code.
      -- Abort handler: 2 params, resume (last) unused.
      let exnEffect = EffectRowExtend (QName "" (Name "exn" 0)) EffectRowEmpty
          handler = ELam [(Name "msg" 1, anyType), (Name "resume" 2, anyType)]
                      (EVar (Name "msg" 1))
          body = EPerform (QName "exn" (Name "raise" 0)) [ELit (LitInt 42)]
          handleExpr = EHandle exnEffect handler body
          prog = mkProgram "test" "abort_ev"
            [ mkFunDef "test" "f" handleExpr anyType ]
          effDecl = EffectDecl
            { effectName   = QName "" (Name "exn" 0)
            , effectParams = []
            , effectOps    = [OpDecl (QName "exn" (Name "raise" 0)) anyType]
            }
          progWithEffects = prog { progEffects = [effDecl] }
          result = evidencePass progWithEffects
          resultExpr = defExpr (head (progDefs result))
      in do
        assertBool "should not contain EHandle after evidence pass"
          (not (containsHandle resultExpr))
        assertBool "should not contain EPerform after evidence pass"
          (not (containsPerform resultExpr))
        -- The generated code should reference kk_handler_exec (callback pattern)
        assertBool "abort codegen should contain kk_handler_exec reference"
          (containsVarNamed "kk_handler_exec" resultExpr)
        -- And kk_handler_abort inside the body lambda
        assertBool "abort codegen should contain kk_handler_abort reference"
          (containsVarNamed "kk_handler_abort" resultExpr)
  ]

-------------------------------------------------------------------------------
-- D2. Linker tests
-------------------------------------------------------------------------------

linkerTests :: TestTree
linkerTests = testGroup "Linker"
  [ testCase "linkPrograms: merges two programs" $
      let prog1 = mkProgram "modA" "a"
            [ mkFunDef "modA" "foo"
                (ELam [(mkName "x", intType)] (EVar (mkName "x")))
                (TFun [(Many, intType)] EffectRowEmpty intType)
            , mkFunDef "modA" "main"
                (EApp (EVar (mkName "foo")) [ELit (LitInt 1)])
                (TFun [] EffectRowEmpty intType)
            ]
          prog2 = mkProgram "modB" "b"
            [ mkFunDef "modB" "bar"
                (ELam [(mkName "y", intType)] (EVar (mkName "y")))
                (TFun [(Many, intType)] EffectRowEmpty intType)
            ]
          result = linkPrograms [prog1, prog2]
      in case result of
        Left errs -> assertFailure $ "linkPrograms failed: " ++ show errs
        Right lr  -> do
          let merged = lrProgram lr
          length (progDefs merged) @?= 3
          lrMainModule lr @?= "modA.a"

  , testCase "linkPrograms: detects multiple mains" $
      -- Two programs that both define "main" should trigger MultipleMainFunctions.
      let prog1 = mkProgram "modA" "a"
            [ mkFunDef "modA" "main"
                (ELit (LitInt 0))
                (TFun [] EffectRowEmpty intType)
            ]
          prog2 = mkProgram "modB" "b"
            [ mkFunDef "modB" "main"
                (ELit (LitInt 1))
                (TFun [] EffectRowEmpty intType)
            ]
          result = linkPrograms [prog1, prog2]
      in case result of
        Left errs -> assertBool "should have MultipleMainFunctions error"
          (any isMultipleMainError errs)
        Right _ -> assertFailure "should have detected multiple mains"

  , testCase "linkPrograms: single program with main succeeds" $
      let prog = mkProgram "test" "t"
            [ mkFunDef "test" "main"
                (ELit (LitInt 0))
                (TFun [] EffectRowEmpty intType)
            ]
      in case linkPrograms [prog] of
        Left errs -> assertFailure $ "linkPrograms failed: " ++ show errs
        Right lr  -> lrMainModule lr @?= "test.t"

  , testCase "linkPrograms: no main when required returns error" $
      let prog = mkProgram "test" "t"
            [ mkFunDef "test" "foo" (ELit (LitInt 0)) intType ]
      in case linkPrograms [prog] of
        Left errs -> assertBool "should have NoMainFunction"
          (any isNoMainError errs)
        Right _   -> assertFailure "should have failed with no main"

  , testCase "linkProgramsWith: no main when not required succeeds" $
      let prog = mkProgram "test" "t"
            [ mkFunDef "test" "foo" (ELit (LitInt 0)) intType ]
      in case linkProgramsWith False [prog] of
        Left errs -> assertFailure $ "should succeed: " ++ show errs
        Right lr  -> assertBool "should have warning about no main"
          (any (T.isInfixOf "No main") (lrWarnings lr))
  ]

isDuplicateError :: LinkError -> Bool
isDuplicateError (DuplicateDefinition {}) = True
isDuplicateError _                        = False

isNoMainError :: LinkError -> Bool
isNoMainError NoMainFunction = True
isNoMainError _              = False

isMultipleMainError :: LinkError -> Bool
isMultipleMainError (MultipleMainFunctions {}) = True
isMultipleMainError _                          = False

-------------------------------------------------------------------------------
-- E. MLIR emission tests
-------------------------------------------------------------------------------

mlirEmitTests :: TestTree
mlirEmitTests = testGroup "MLIR Emission"
  [ testCase "emitProgram: demo factorial produces valid MLIR" $
      let prog = demoFactorialWithMain
          mlir = emitProgram prog
      in do
        assertBool "should contain func.func @demo_factorial (module-qualified)"
          (T.isInfixOf "func.func @demo_factorial" mlir)
        assertBool "should contain func.func @main"
          (T.isInfixOf "func.func @main" mlir)

  , testCase "emitProgram: contains arith.muli for multiplication" $
      let prog = demoFactorialWithMain
          mlir = emitProgram prog
      in assertBool "should contain arith.muli"
           (T.isInfixOf "arith.muli" mlir)

  , testCase "emitProgram: contains scf.if for case expression" $
      let prog = demoFactorialWithMain
          mlir = emitProgram prog
      in assertBool "should contain scf.if or scf.while or cf.cond_br"
           (T.isInfixOf "scf.if" mlir
            || T.isInfixOf "scf.while" mlir
            || T.isInfixOf "cf.cond_br" mlir)

  , testCase "emitProgram: output contains module wrapper" $
      let prog = demoFactorialWithMain
          mlir = emitProgram prog
      in assertBool "should start with module"
           (T.isInfixOf "module" mlir || T.isInfixOf "func.func" mlir)

  , testCase "emitProgram: empty program emits something" $
      let prog = mkProgram "test" "empty" []
          mlir = emitProgram prog
      in assertBool "should produce non-empty output"
           (not (T.null mlir))

  , testCase "emitProgram: ADT (MyMaybe) produces constructor runtime calls" $
      let prog = myMaybeProgram
          mlir = emitProgram prog
      in do
        assertBool "should contain kk_alloc_con (ECon allocation)"
          (T.isInfixOf "kk_alloc_con" mlir)
        assertBool "should contain kk_tag (PatCon dispatch)"
          (T.isInfixOf "kk_tag" mlir)
        assertBool "should contain scf.if for case dispatch"
          (T.isInfixOf "scf.if" mlir)
        assertBool "should contain func.func @test_orZero (module-qualified)"
          (T.isInfixOf "func.func @test_orZero" mlir)

  , testCase "emitProgram: Int-returning main uses printf, not kk_println_con" $
      let prog = mkProgram "" "p"
            [ (mkFunDef "" "main" (ELit (LitInt 42)) intType)
                { defType = TFun [] EffectRowEmpty intType }
            ]
          mlir = emitProgram prog
      in do
        assertBool "uses printf for Int main"
          (T.isInfixOf "llvm.call @printf" mlir)
        assertBool "does not call kk_println_con for Int main"
          (not (T.isInfixOf "func.call @kk_println_con" mlir))

  , testCase "emitProgram: ADT-returning main calls kk_println_con" $
      let treeTy = TCon (TypeCon (mkQName "" "Tree") KindValue)
          dd = DataDecl
            { dataName   = mkQName "" "Tree"
            , dataParams = []
            , dataCons   = [ ConDecl (mkQName "" "Leaf") [] Public
                           , ConDecl (mkQName "" "Node")
                               [ (mkName "l", treeTy)
                               , (mkName "v", intType)
                               , (mkName "r", treeTy)
                               ] Public
                           ]
            , dataVis    = Public
            }
          mainDef = (mkFunDef "" "main" (ECon (mkQName "" "Leaf")) treeTy)
                      { defType = TFun [] EffectRowEmpty treeTy }
          prog = (mkProgram "" "p" [mainDef]) { progData = [dd] }
          mlir = emitProgram prog
      in do
        assertBool "calls kk_println_con for ADT main"
          (T.isInfixOf "func.call @kk_println_con" mlir)
        assertBool "declares kk_println_con prototype"
          (T.isInfixOf "func.func private @kk_println_con" mlir)
        assertBool "does not call printf in wrapper for ADT main"
          (not (T.isInfixOf "llvm.call @printf(%fmtaddr" mlir))

  , testCase "emitProgram: String-returning main calls kk_println_str" $
      let stringTy = TCon (TypeCon (mkQName "std" "string") KindValue)
          body     = EApp (EVar (mkName "str_concat"))
                          [ ELit (LitString "Hello, ")
                          , ELit (LitString "world")
                          ]
          mainDef  = (mkFunDef "" "main" body stringTy)
                       { defType = TFun [] EffectRowEmpty stringTy }
          prog     = mkProgram "" "p" [mainDef]
          mlir     = emitProgram prog
      in do
        assertBool "declares kk_println_str prototype"
          (T.isInfixOf "func.func private @kk_println_str" mlir)
        assertBool "declares kk_str_concat prototype"
          (T.isInfixOf "func.func private @kk_str_concat" mlir)
        assertBool "main wrapper calls kk_println_str"
          (T.isInfixOf "func.call @kk_println_str" mlir)
        assertBool "body calls kk_str_concat"
          (T.isInfixOf "func.call @kk_str_concat" mlir)
        assertBool "emits string literal globals"
          (T.isInfixOf "llvm.mlir.global internal constant @str_" mlir)
        assertBool "does not use kk_println_con for string main"
          (not (T.isInfixOf "func.call @kk_println_con(%result)" mlir))

  , testCase "emitProgram: show intrinsic lowers to kk_str_show_int" $
      let stringTy = TCon (TypeCon (mkQName "std" "string") KindValue)
          body     = EApp (EVar (mkName "show")) [ELit (LitInt 42)]
          mainDef  = (mkFunDef "" "main" body stringTy)
                       { defType = TFun [] EffectRowEmpty stringTy }
          prog     = mkProgram "" "p" [mainDef]
          mlir     = emitProgram prog
      in do
        assertBool "calls kk_str_show_int"
          (T.isInfixOf "func.call @kk_str_show_int" mlir)
        assertBool "main wrapper uses kk_println_str"
          (T.isInfixOf "func.call @kk_println_str" mlir)

  , testCase "emitProgram: string literals wrapped via kk_string_from_literal" $
      let stringTy = TCon (TypeCon (mkQName "std" "string") KindValue)
          body     = ELit (LitString "hi")
          mainDef  = (mkFunDef "" "main" body stringTy)
                       { defType = TFun [] EffectRowEmpty stringTy }
          prog     = mkProgram "" "p" [mainDef]
          mlir     = emitProgram prog
      in do
        assertBool "declares kk_string_from_literal prototype"
          (T.isInfixOf "func.func private @kk_string_from_literal" mlir)
        assertBool "literal site calls kk_string_from_literal"
          (T.isInfixOf "func.call @kk_string_from_literal" mlir)
        assertBool "passes byte length 2 for \"hi\""
          (T.isInfixOf "arith.constant 2 : i64" mlir)

  , testCase "emitProgram: char_len intrinsic lowers to kk_str_char_len" $
      let intTy   = TCon (TypeCon (mkQName "std" "int") KindValue)
          body    = EApp (EVar (mkName "char_len")) [ELit (LitString "Grüße")]
          mainDef = (mkFunDef "" "main" body intTy)
                       { defType = TFun [] EffectRowEmpty intTy }
          prog    = mkProgram "" "p" [mainDef]
          mlir    = emitProgram prog
      in do
        assertBool "calls kk_str_char_len"
          (T.isInfixOf "func.call @kk_str_char_len(" mlir)
        assertBool "encodes UTF-8 byte ü as \\C3\\BC"
          (T.isInfixOf "\\C3\\BC" mlir)
        assertBool "byte length is 7 (not 5 codepoints)"
          (T.isInfixOf "arith.constant 7 : i64" mlir)

  , testCase "emitProgram: bytes_concat lowers to kk_str_concat (shared rope)" $
      let intTy   = TCon (TypeCon (mkQName "std" "int") KindValue)
          body    = EApp (EVar (mkName "bytes_len"))
                         [EApp (EVar (mkName "bytes_concat"))
                               [ELit (LitString "ab"), ELit (LitString "cd")]]
          mainDef = (mkFunDef "" "main" body intTy)
                       { defType = TFun [] EffectRowEmpty intTy }
          prog    = mkProgram "" "p" [mainDef]
          mlir    = emitProgram prog
      in do
        assertBool "bytes_concat → kk_str_concat"
          (T.isInfixOf "func.call @kk_str_concat" mlir)
        assertBool "bytes_len → kk_str_len"
          (T.isInfixOf "func.call @kk_str_len" mlir)

  , testCase "emitProgram: read_file lowers to kk_read_file" $
      let intTy   = TCon (TypeCon (mkQName "std" "int") KindValue)
          body    = EApp (EVar (mkName "read_file")) [ELit (LitString "/etc/hostname")]
          mainDef = (mkFunDef "" "main" body intTy)
                       { defType = TFun [] EffectRowEmpty intTy }
          prog    = mkProgram "" "p" [mainDef]
          mlir    = emitProgram prog
      in assertBool "calls kk_read_file"
           (T.isInfixOf "func.call @kk_read_file(" mlir)

  , testCase "emitProgram: write_file lowers to kk_write_file" $
      let intTy   = TCon (TypeCon (mkQName "std" "int") KindValue)
          body    = EApp (EVar (mkName "write_file"))
                         [ELit (LitString "/tmp/x"), ELit (LitString "hi")]
          mainDef = (mkFunDef "" "main" body intTy)
                       { defType = TFun [] EffectRowEmpty intTy }
          prog    = mkProgram "" "p" [mainDef]
          mlir    = emitProgram prog
      in assertBool "calls kk_write_file"
           (T.isInfixOf "func.call @kk_write_file(" mlir)

  , testCase "emitProgram: IORef new/get/set lower to kk_ref_*" $
      let intTy   = TCon (TypeCon (mkQName "std" "int") KindValue)
          litZero = ELit (LitInt 0)
          litOne  = ELit (LitInt 1)
          newRef  = EApp (EVar (mkName "new_ref")) [litZero]
          bind    = Bind (mkName "r") intTy newRef DefVal
          letBind = ELet [[bind]]
                         (EApp (EVar (mkName "set_ref"))
                               [EVar (mkName "r"), litOne])
          mainDef = (mkFunDef "" "main" letBind intTy)
                       { defType = TFun [] EffectRowEmpty intTy }
          prog    = mkProgram "" "p" [mainDef]
          mlir    = emitProgram prog
      in do
        assertBool "calls kk_ref_new"
          (T.isInfixOf "func.call @kk_ref_new(" mlir)
        assertBool "calls kk_ref_set"
          (T.isInfixOf "func.call @kk_ref_set(" mlir)
  ]

-------------------------------------------------------------------------------
-- F'. DeriveSelectors unit tests
-------------------------------------------------------------------------------

deriveSelectorsTests :: TestTree
deriveSelectorsTests = testGroup "DeriveSelectors"
  [ testCase "single-ctor record yields one selector per field" $
      let pointTy = TCon (TypeCon (mkQName "" "Point") KindValue)
          dd = DataDecl
            { dataName   = mkQName "" "Point"
            , dataParams = []
            , dataCons   = [ ConDecl (mkQName "" "Point")
                                     [(mkName "x", intType), (mkName "y", intType)]
                                     Public ]
            , dataVis    = Public
            }
          sels = recordSelectors Set.empty dd
          _ = pointTy
      in do
        assertEqual "two selectors" 2 (length sels)
        assertEqual "first selector named x"
          "x" (nameText (qnameName (defName (sels !! 0))))
        assertEqual "second selector named y"
          "y" (nameText (qnameName (defName (sels !! 1))))

  , testCase "multi-ctor data decl yields no selectors" $
      let dd = DataDecl
            { dataName   = mkQName "" "Maybe"
            , dataParams = []
            , dataCons   = [ ConDecl (mkQName "" "Nothing") [] Public
                           , ConDecl (mkQName "" "Just")
                                     [(mkName "value", intType)]
                                     Public
                           ]
            , dataVis    = Public
            }
      in assertEqual "no selectors for multi-ctor" 0
           (length (recordSelectors Set.empty dd))

  , testCase "selector skipped when name already exists" $
      let dd = DataDecl
            { dataName   = mkQName "" "Box"
            , dataParams = []
            , dataCons   = [ ConDecl (mkQName "" "Box")
                                     [(mkName "value", intType)] Public ]
            , dataVis    = Public
            }
      in assertEqual "no selector when shadowed" 0
           (length (recordSelectors (Set.singleton "value") dd))

  , testCase "selector body is a case-of constructor" $
      let dd = DataDecl
            { dataName   = mkQName "" "Pair"
            , dataParams = []
            , dataCons   = [ ConDecl (mkQName "" "Pair")
                                     [(mkName "fst", intType), (mkName "snd", intType)]
                                     Public ]
            , dataVis    = Public
            }
          sels = recordSelectors Set.empty dd
          fstSel = sels !! 0
      in case defExpr fstSel of
           ELam [_] (ECase _ [Branch (PatCon _ [PatVar _ _, PatWild _]) _ _]) ->
             pure ()
           other -> assertFailure ("unexpected selector body: " ++ show other)

  , testCase "deriveSelectors appends to progDefs" $
      let dd = DataDecl
            { dataName   = mkQName "" "Cell"
            , dataParams = []
            , dataCons   = [ ConDecl (mkQName "" "Cell")
                                     [(mkName "contents", intType)] Public ]
            , dataVis    = Public
            }
          mainDef = mkFunDef "" "main" (ELit (LitInt 0)) intType
          prog0 = (mkProgram "" "p" [mainDef]) { progData = [dd] }
          prog' = deriveSelectors prog0
      in do
        assertEqual "one main + one selector" 2 (length (progDefs prog'))
        assertBool "selector named 'contents' present"
          (any (\d -> nameText (qnameName (defName d)) == "contents")
               (progDefs prog'))
  ]

-------------------------------------------------------------------------------
-- F. FlattenPatterns unit tests
-------------------------------------------------------------------------------

flattenPatternsTests :: TestTree
flattenPatternsTests = testGroup "FlattenPatterns"
  [ testCase "leaves simple patterns unchanged" $
      let e = ECase (EVar (mkName "x"))
                [ Branch (PatCon (mkQName "" "Just") [PatVar (mkName "n") intType])
                         Nothing (EVar (mkName "n")) ]
      in assertEqual "unchanged" e (flattenPatternsExpr e)

  , testCase "lifts nested PatCon into inner ECase" $
      let -- case xs of Cons (Box n) ys -> n
          nested = ECase (EVar (mkName "xs"))
            [ Branch
                (PatCon (mkQName "" "Cons")
                  [ PatCon (mkQName "" "Box") [PatVar (mkName "n") intType]
                  , PatVar (mkName "ys") anyType
                  ])
                Nothing
                (EVar (mkName "n"))
            ]
          flat = flattenPatternsExpr nested
      in case flat of
           ECase _ [Branch (PatCon (QName _ (Name "Cons" _)) [PatVar fresh _, PatVar _ys _]) Nothing body] ->
             case body of
               ECase (EVar freshRef) [Branch (PatCon (QName _ (Name "Box" _)) [PatVar _ _]) Nothing _] ->
                 assertEqual "fresh var matches" fresh freshRef
               _ -> assertFailure ("unexpected body: " ++ show body)
           _ -> assertFailure ("unexpected shape: " ++ show flat)

  , testCase "recurses into nested bodies (ELam)" $
      let inner = ECase (EVar (mkName "x"))
                    [ Branch (PatCon (mkQName "" "Wrap")
                               [PatCon (mkQName "" "Inner") []])
                             Nothing (ELit (LitInt 1)) ]
          e = ELam [(mkName "x", anyType)] inner
          flat = flattenPatternsExpr e
      in case flat of
           ELam _ (ECase _ [Branch (PatCon _ [PatVar _ _]) Nothing (ECase _ _)]) ->
             pure ()
           _ -> assertFailure ("did not lift under ELam: " ++ show flat)

  , testCase "Maranget: sibling branches with same outer ctor merge" $
      -- case xs of
      --   Cons (Box n)  ys -> body1 n ys
      --   Cons Unboxed  ys -> body2 ys
      --   Nil              -> body3
      let body1 = EApp (EVar (mkName "body1"))
                       [EVar (mkName "n"), EVar (mkName "ys")]
          body2 = EApp (EVar (mkName "body2")) [EVar (mkName "ys")]
          body3 = EVar (mkName "body3")
          e = ECase (EVar (mkName "xs"))
                [ Branch (PatCon (mkQName "" "Cons")
                           [ PatCon (mkQName "" "Box")
                               [PatVar (mkName "n") intType]
                           , PatVar (mkName "ys") anyType
                           ])
                         Nothing body1
                , Branch (PatCon (mkQName "" "Cons")
                           [ PatCon (mkQName "" "Unboxed") []
                           , PatVar (mkName "ys") anyType
                           ])
                         Nothing body2
                , Branch (PatCon (mkQName "" "Nil") []) Nothing body3
                ]
          flat = flattenPatternsExpr e
      in case flat of
           ECase (EVar _) [br1, br2] -> do
             -- br1 = Cons f0 f1 -> <inner>
             case branchPattern br1 of
               PatCon (QName _ (Name "Cons" _))
                 [PatVar _f0 _, PatVar _f1 _] -> pure ()
               p -> assertFailure ("expected merged Cons branch, got: " ++ show p)
             -- br2 = Nil -> body3
             case branchPattern br2 of
               PatCon (QName _ (Name "Nil" _)) [] -> pure ()
               p -> assertFailure ("expected Nil branch, got: " ++ show p)
             -- Inner body of br1 must be an ECase dispatching Box vs Unboxed
             case branchBody br1 of
               ECase (EVar _) innerBrs -> do
                 let innerNames =
                       [ n
                       | Branch (PatCon (QName _ (Name n _)) _) _ _ <- innerBrs ]
                 assertBool "inner dispatches Box"     (elem "Box" innerNames)
                 assertBool "inner dispatches Unboxed" (elem "Unboxed" innerNames)
               other ->
                 assertFailure ("expected inner ECase, got: " ++ show other)
           other -> assertFailure ("expected 2 outer branches, got: " ++ show other)

  , testCase "Maranget: wildcard catch-all becomes default branch" $
      -- case v of
      --   Some (Just x) -> body1 x
      --   _             -> body2
      let body1 = EApp (EVar (mkName "body1")) [EVar (mkName "x")]
          body2 = EVar (mkName "body2")
          e = ECase (EVar (mkName "v"))
                [ Branch (PatCon (mkQName "" "Some")
                           [PatCon (mkQName "" "Just")
                              [PatVar (mkName "x") intType]])
                         Nothing body1
                , Branch (PatWild anyType) Nothing body2
                ]
          flat = flattenPatternsExpr e
      in case flat of
           ECase (EVar _) brs -> do
             assertBool "has at least 2 branches" (length brs >= 2)
             let lastBr = last brs
             case branchPattern lastBr of
               PatWild _ -> pure ()
               p -> assertFailure ("expected last branch to be default, got: " ++ show p)
           other -> assertFailure ("unexpected shape: " ++ show other)
  ]

-------------------------------------------------------------------------------
-- F1b. ConTags pass tests
-------------------------------------------------------------------------------

-- | Small helper: build a DataDecl with n nullary ctors named c0..c_{n-1}.
mkDataDeclSimple :: Text -> [Text] -> DataDecl
mkDataDeclSimple tyName ctorNames = DataDecl
  { dataName   = mkQName "" tyName
  , dataParams = []
  , dataCons   = [ ConDecl
      { conName   = mkQName "" c
      , conFields = []
      , conVis    = Public
      }
    | c <- ctorNames ]
  , dataVis    = Public
  }

conTagsTests :: TestTree
conTagsTests = testGroup "ConTags (per-program tag assignment)"
  [ testCase "single DataDecl gets 0..n-1 tags in declaration order" $
      let dd = mkDataDeclSimple "Color" ["Red", "Green", "Blue"]
          prog = (mkProgram "" "main" [])
                   { progData = [dd] }
          tags = assignProgramTags prog
      in do
        assertEqual "Red -> 0"   (Just 0) (Map.lookup "Red"   tags)
        assertEqual "Green -> 1" (Just 1) (Map.lookup "Green" tags)
        assertEqual "Blue -> 2"  (Just 2) (Map.lookup "Blue"  tags)

  , testCase "two DataDecls share the 0..n-1 space per type" $
      let ddA = mkDataDeclSimple "A" ["A0", "A1"]
          ddB = mkDataDeclSimple "B" ["B0", "B1", "B2"]
          prog = (mkProgram "" "main" [])
                   { progData = [ddA, ddB] }
          tags = assignProgramTags prog
      in do
        assertEqual "A0 -> 0" (Just 0) (Map.lookup "A0" tags)
        assertEqual "A1 -> 1" (Just 1) (Map.lookup "A1" tags)
        assertEqual "B0 -> 0" (Just 0) (Map.lookup "B0" tags)
        assertEqual "B1 -> 1" (Just 1) (Map.lookup "B1" tags)
        assertEqual "B2 -> 2" (Just 2) (Map.lookup "B2" tags)

  , testCase "orphan ctors referenced in ECon get fresh tags past declared range" $
      let dd = mkDataDeclSimple "Color" ["Red", "Green", "Blue"]  -- uses 0,1,2
          orphanExpr = EApp (ECon (mkQName "" "Orphan1"))
                            [ECon (mkQName "" "Orphan2")]
          def  = mkFunDef "" "f" orphanExpr intType
          prog = (mkProgram "" "main" [def])
                   { progData = [dd] }
          tags = assignProgramTags prog
      in do
        assertBool "Orphan1 present"
          (Map.member "Orphan1" tags)
        assertBool "Orphan2 present"
          (Map.member "Orphan2" tags)
        let Just o1 = Map.lookup "Orphan1" tags
            Just o2 = Map.lookup "Orphan2" tags
        assertBool "Orphan1 >= 3" (o1 >= 3)
        assertBool "Orphan2 >= 3" (o2 >= 3)
        assertBool "Orphan1 /= Orphan2" (o1 /= o2)

  , testCase "orphan ctors referenced in PatCon are also discovered" $
      let caseExpr = ECase (EVar (mkName "x"))
            [ Branch (PatCon (mkQName "" "Leaf") []) Nothing
                     (ELit (LitInt 0))
            , Branch (PatCon (mkQName "" "Node")
                       [PatVar (mkName "n") intType])
                     Nothing
                     (EVar (mkName "n"))
            ]
          def = mkFunDef "" "f" (ELam [(mkName "x", anyType)] caseExpr) intType
          prog = mkProgram "" "main" [def]
          tags = assignProgramTags prog
      in do
        assertBool "Leaf discovered" (Map.member "Leaf" tags)
        assertBool "Node discovered" (Map.member "Node" tags)

  , testCase "collectReferencedCtors finds declared + referenced ctors" $
      let dd = mkDataDeclSimple "T" ["Decl"]
          def = mkFunDef "" "f" (ECon (mkQName "" "Used")) intType
          prog = (mkProgram "" "main" [def])
                   { progData = [dd] }
          refs = collectReferencedCtors prog
      in do
        assertBool "Decl included" (Set.member "Decl" refs)
        assertBool "Used included" (Set.member "Used" refs)

  , testCase "no collisions: assignProgramTags is injective when we restrict to one type's ctors" $
      -- Property: within a single DataDecl, every ctor has a distinct tag.
      let dd = mkDataDeclSimple "Big"
                 ["C0","C1","C2","C3","C4","C5","C6","C7","C8","C9"]
          prog = (mkProgram "" "main" [])
                   { progData = [dd] }
          tags = assignProgramTags prog
          values =
            [ Map.lookup c tags
            | c <- ["C0","C1","C2","C3","C4","C5","C6","C7","C8","C9"] ]
      in do
        assertBool "all present" (all (/= Nothing) values)
        assertEqual "all distinct" 10 (Set.size (Set.fromList values))
  ]

-------------------------------------------------------------------------------
-- F2. Mercury ADT translation tests
-------------------------------------------------------------------------------

-- | Build a synthetic Mercury HLDS for a @shape@ ADT program:
--
-- @
-- :- type shape ---> circle(int) ; square(int).
-- area(circle(R), A) :- A = R.
-- area(square(S), A) :- A = S.
-- main_int(X) :- area(circle(7), X).
-- @
--
-- Constructed directly to bypass the HLDS text parser, which has
-- pre-existing fragility around multi-line conjunction/disjunction
-- formatting. This exercises the CoreTranslate Mercury ADT path.
shapeHlds :: MercuryHLDS
shapeHlds = MercuryHLDS
  { hldsModule = "shape"
  , hldsTypes =
      [ MercuryTypeDecl
          { typeDeclName   = "shape"
          , typeDeclParams = []
          , typeDeclCtors  = [ ("circle", ["int"]), ("square", ["int"]) ]
          }
      ]
  , hldsPreds =
      [ -- area(circle(R), A) :- A = R.
        MercuryPred
          { predName     = "area_circle"
          , predArity    = 2
          , predDet      = Det
          , predModes    = [ModeIn, ModeOut]
          , predArgTypes = ["shape", "int"]
          , predArgNames = ["Shape", "A"]
          , predGoal     = Just $ GoalConj
              [ GoalDeconstruct "Shape" "circle" ["R"]
              , GoalUnify "A" "R"
              ]
          }
        -- main_int(X) :- X = circle(7), area_circle(X, R).
      , MercuryPred
          { predName     = "main_int"
          , predArity    = 1
          , predDet      = Det
          , predModes    = [ModeOut]
          , predArgTypes = ["int"]
          , predArgNames = ["R"]
          , predGoal     = Just $ GoalConj
              [ GoalUnify "Seven" "7"
              , GoalConstruct "Shape" "circle" ["Seven"]
              , GoalCall "area_circle" ["Shape", "R"]
              ]
          }
      ]
  }

-- | Does any subexpression contain an ECon node?
hasECon :: Expr -> Bool
hasECon = go
  where
    go (ECon _)        = True
    go (ELam _ b)      = go b
    go (EApp f args)   = go f || any go args
    go (ELet bgs body) =
      any (\(Bind _ _ rhs _) -> go rhs) (concat bgs) || go body
    go (ECase s bs)    =
      go s || any (\(Branch _ _ body) -> go body) bs
    go (EHandle _ h b) = go h || go b
    go (EPerform _ args) = any go args
    go _               = False

-- | Does any subexpression (or pattern) contain a PatCon node?
hasPatCon :: Expr -> Bool
hasPatCon = go
  where
    go (ELam _ b)      = go b
    go (EApp f args)   = go f || any go args
    go (ELet bgs body) =
      any (\(Bind _ _ rhs _) -> go rhs) (concat bgs) || go body
    go (ECase s bs)    =
      go s || any branchHit bs
    go (EHandle _ h b) = go h || go b
    go (EPerform _ args) = any go args
    go _               = False

    branchHit (Branch pat _ body) = patHit pat || go body
    patHit (PatCon _ _) = True
    patHit _            = False

mercuryAdtTests :: TestTree
mercuryAdtTests = testGroup "Mercury ADT translation"
  [ testCase "translateHlds preserves shape DataDecl" $
      case translateHlds shapeHlds of
        Left err -> assertFailure ("translateHlds failed: " ++ T.unpack err)
        Right prog -> do
          let dds = progData prog
          assertEqual "one data decl" 1 (length dds)
          let dd = head dds
          assertEqual "name" "shape" (nameText (qnameName (dataName dd)))
          assertEqual "two ctors" 2 (length (dataCons dd))
          let ctorNames =
                map (nameText . qnameName . conName) (dataCons dd)
          assertEqual "ctor names"
            ["circle", "square"] ctorNames

  , testCase "main_int body contains ECon for circle allocation" $
      case translateHlds shapeHlds of
        Left err -> assertFailure ("translateHlds failed: " ++ T.unpack err)
        Right prog -> do
          let mainDef = head
                [ d | d <- progDefs prog
                    , nameText (qnameName (defName d)) == "main_int" ]
          assertBool "main_int has an ECon node" (hasECon (defExpr mainDef))

  , testCase "area_circle body contains PatCon for circle" $
      case translateHlds shapeHlds of
        Left err -> assertFailure ("translateHlds failed: " ++ T.unpack err)
        Right prog -> do
          let areaDef = head
                [ d | d <- progDefs prog
                    , nameText (qnameName (defName d)) == "area_circle" ]
          assertBool "area_circle has a PatCon node"
            (hasPatCon (defExpr areaDef))

  , testCase "emitted MLIR contains ADT runtime calls" $
      case translateHlds shapeHlds of
        Left err -> assertFailure ("translateHlds failed: " ++ T.unpack err)
        Right prog -> do
          let mlir = emitProgram prog
          assertBool "contains kk_alloc_con"
            (T.isInfixOf "kk_alloc_con" mlir)
          assertBool "contains kk_tag"
            (T.isInfixOf "kk_tag" mlir)

  , testCase "synthesises main alias for main_int" $
      case translateHlds shapeHlds of
        Left err -> assertFailure ("translateHlds failed: " ++ T.unpack err)
        Right prog -> do
          let mainAlias =
                [ d | d <- progDefs prog
                    , nameText (qnameName (defName d)) == "main" ]
          assertBool "main alias def exists" (not (null mainAlias))
  ]

-------------------------------------------------------------------------------
-- G. Wasm emission tests
-------------------------------------------------------------------------------

wasmEmitTests :: TestTree
wasmEmitTests = testGroup "Wasm Emission"
  [ testCase "emitProgramWasm: no printf declaration" $
      let prog = demoFactorialWithMain
          mlir = emitProgramWasm prog
      in assertBool "wasm MLIR should not contain main wrapper with printf call"
           (not (T.isInfixOf "func.func @main" mlir))

  , testCase "emitProgramWasm: contains _frankenstein_main" $
      let prog = demoFactorialWithMain
          mlir = emitProgramWasm prog
      in assertBool "should contain _frankenstein_main"
           (T.isInfixOf "_frankenstein_main" mlir)

  , testCase "emitProgramWasm: contains runtime declarations" $
      let prog = demoFactorialWithMain
          mlir = emitProgramWasm prog
      in do
        assertBool "should declare kk_retain"
          (T.isInfixOf "kk_retain" mlir)
        assertBool "should declare kk_drop"
          (T.isInfixOf "kk_drop" mlir)

  , testCase "emitProgramWasm: factorial produces valid MLIR" $
      let prog = demoFactorialWithMain
          mlir = emitProgramWasm prog
      in do
        assertBool "should contain func.func @demo_factorial (module-qualified)"
          (T.isInfixOf "func.func @demo_factorial" mlir)
        assertBool "should contain module wrapper"
          (T.isInfixOf "module {" mlir)
  ]

-------------------------------------------------------------------------------
-- Demo program (same as app/Main.hs)
-------------------------------------------------------------------------------

demoFactorialWithMain :: Program
demoFactorialWithMain = Program
  { progName = QName "demo" (Name "factorial" 0)
  , progDefs =
      [ Def
          { defName = QName "" (Name "factorial" 1)
          , defType = TFun [(Many, intType)] EffectRowEmpty intType
          , defExpr =
              ELam [(Name "n" 2, intType)] $
                ECase (EVar (Name "n" 2))
                  [ Branch (PatLit (LitInt 0)) Nothing (ELit (LitInt 1))
                  , Branch (PatVar (Name "n" 2) intType) Nothing
                      (EApp (EVar (Name "*" 0))
                        [ EVar (Name "n" 2)
                        , EApp (EVar (Name "factorial" 1))
                               [EApp (EVar (Name "-" 0)) [EVar (Name "n" 2), ELit (LitInt 1)]]
                        ])
                  ]
          , defSort = DefFun
          , defVisibility = Public
          }
      , Def
          { defName = QName "" (Name "main" 10)
          , defType = TFun [] EffectRowEmpty intType
          , defExpr =
              EApp (EVar (Name "factorial" 1)) [ELit (LitInt 10)]
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects = []
  }

-- | ADT smoke-test program: mirrors examples/maybesum.hs
--
-- @
-- data MyMaybe = MyNothing | MyJust Int
-- orZero MyNothing  = 0
-- orZero (MyJust x) = x
-- main = orZero (MyJust 7)
-- @
myMaybeProgram :: Program
myMaybeProgram = Program
  { progName = mkQName "test" "myMaybe"
  , progDefs =
      [ Def
          { defName = mkQName "" "orZero"
          , defType = TFun [(Many, anyType)] EffectRowEmpty intType
          , defExpr =
              ELam [(mkName "m", anyType)] $
                ECase (EVar (mkName "m"))
                  [ Branch (PatCon (mkQName "" "MyNothing") []) Nothing
                      (ELit (LitInt 0))
                  , Branch (PatCon (mkQName "" "MyJust") [PatVar (mkName "x") intType]) Nothing
                      (EVar (mkName "x"))
                  ]
          , defSort = DefFun
          , defVisibility = Public
          }
      , Def
          { defName = mkQName "" "main"
          , defType = TFun [] EffectRowEmpty intType
          , defExpr =
              EApp (EVar (mkName "orZero"))
                [EApp (ECon (mkQName "" "MyJust")) [ELit (LitInt 7)]]
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData =
      [ DataDecl
          { dataName   = mkQName "" "MyMaybe"
          , dataParams = []
          , dataCons   =
              [ ConDecl (mkQName "" "MyNothing") [] Public
              , ConDecl (mkQName "" "MyJust")    [(mkName "x", intType)] Public
              ]
          , dataVis    = Public
          }
      ]
  , progEffects = []
  }

-------------------------------------------------------------------------------
-- CPS conversion + multi-shot classifier tests
-------------------------------------------------------------------------------

cpsConvertTests :: TestTree
cpsConvertTests = testGroup "CPS conversion / multi-shot classifier"
  [ -- HandlerKind classification: how the resume parameter is used.

    testCase "classifyHandler: abort (resume never referenced)" $
      -- \v resume -> v   (resume unused)
      let h = ELam [(mkName "v", anyType), (mkName "resume", anyType)]
                (EVar (mkName "v"))
      in classifyHandler h @?= HKAbort

  , testCase "classifyHandler: tail-resumptive (single tail call to resume)" $
      -- \v resume -> resume v
      let h = ELam [(mkName "v", anyType), (mkName "resume", anyType)]
                (EApp (EVar (mkName "resume")) [EVar (mkName "v")])
      in classifyHandler h @?= HKTail

  , testCase "classifyHandler: multi-shot (resume called twice)" $
      -- \v resume -> resume True ++ resume False  (modelled as nested EApp)
      let resumeTrue  = EApp (EVar (mkName "resume")) [ELit (LitInt 1)]
          resumeFalse = EApp (EVar (mkName "resume")) [ELit (LitInt 0)]
          h = ELam [(mkName "v", anyType), (mkName "resume", anyType)]
                (EApp (EVar (mkName "append")) [resumeTrue, resumeFalse])
      in classifyHandler h @?= HKMulti

  , testCase "classifyHandler: multi-shot (resume in non-tail position)" $
      -- \v resume -> let x = resume v in x + 1
      let h = ELam [(mkName "v", anyType), (mkName "resume", anyType)]
                (ELet [[Bind (mkName "x") anyType
                          (EApp (EVar (mkName "resume")) [EVar (mkName "v")])
                          DefVal]]
                  (EApp (EVar (mkName "add"))
                    [EVar (mkName "x"), ELit (LitInt 1)]))
      in classifyHandler h @?= HKMulti

  , -- CPS conversion: structural property tests on the output AST.

    testCase "cpsTopLevel: literal is delivered to identity continuation" $
      -- cps (ELit 42) id = ELit 42
      cpsTopLevel (ELit (LitInt 42)) @?= ELit (LitInt 42)

  , testCase "cpsTopLevel: variable is delivered to identity continuation" $
      cpsTopLevel (EVar (mkName "x")) @?= EVar (mkName "x")

  , testCase "cpsTopLevel: ECon passes through" $
      cpsTopLevel (ECon (mkQName "" "Nothing"))
        @?= ECon (mkQName "" "Nothing")

  , testCase "cpsTopLevel: EApp recurses into fn and args" $
      -- cps (f x y) id = f x y    (no perform, no continuation reified)
      let e = EApp (EVar (mkName "f"))
                [EVar (mkName "x"), EVar (mkName "y")]
      in cpsTopLevel e @?= e

  , testCase "cpsTopLevel: EPerform reifies a continuation closure" $
      -- cps (perform op [a]) id
      --   = handlerRef(op)(a, \k_cps_0 -> k_cps_0)
      -- because the final cont is `id`, so `k (EVar v) = EVar v`.
      let op = mkQName "exn" "raise"
          input = EPerform op [EVar (mkName "msg")]
          out = cpsTopLevel input
      in case out of
           EApp (EFunRef qn) [EVar (Name "msg" _), ELam [(kN, _)] (EVar kN')]
             | qn == op && nameText kN == nameText kN' -> pure ()
           _ -> assertFailure ("Unexpected CPS output: " ++ show out)

  , testCase "cpsTopLevel: ELet linear sequencing" $
      -- cps (let x = 1 in x) id = let x = 1 in x   (no effects)
      let e = ELet [[Bind (mkName "x") anyType (ELit (LitInt 1)) DefVal]]
                (EVar (mkName "x"))
      in cpsTopLevel e @?= e

  , testCase "cpsTopLevel: ECase distributes continuation into branches" $
      -- cps (case x of Just y -> y; Nothing -> 0) id
      --   = case x of Just y -> y; Nothing -> 0
      let e = ECase (EVar (mkName "x"))
                [ Branch (PatCon (mkQName "" "Just") [PatVar (mkName "y") anyType])
                    Nothing (EVar (mkName "y"))
                , Branch (PatCon (mkQName "" "Nothing") [])
                    Nothing (ELit (LitInt 0))
                ]
      in cpsTopLevel e @?= e

  , testCase "cpsTopLevel: idempotence on effect-free expressions" $
      -- For expressions with no EPerform, CPS conversion is identity
      -- (modulo administrative redexes, of which there are none here).
      let e = EApp (EVar (mkName "f"))
                [EApp (EVar (mkName "g")) [ELit (LitInt 1)]]
      in cpsTopLevel (cpsTopLevel e) @?= cpsTopLevel e
  ]

-------------------------------------------------------------------------------
-- TypeCheck (IR well-formedness) tests
-------------------------------------------------------------------------------

typeCheckTests :: TestTree
typeCheckTests = testGroup "TypeCheck"
  [ testCase "well-typed: empty program" $
      let prog = mkProgram "M" "main" []
      in typeCheckProgram prog @?= []

  , testCase "well-typed: int identity" $
      let d = mkFunDef "M" "id"
                (ELam [(mkName "x", intType)] (EVar (mkName "x")))
                (TFun [(Many, intType)] EffectRowEmpty intType)
          prog = mkProgram "M" "main" [d]
      in typeCheckProgram prog @?= []

  , testCase "well-typed: constructor reference resolves" $
      let pair = ConDecl
            { conName = mkQName "M" "Pair"
            , conFields = [(mkName "x", intType), (mkName "y", intType)]
            , conVis = Public
            }
          dd = DataDecl
            { dataName = mkQName "M" "Pair"
            , dataParams = []
            , dataCons = [pair]
            , dataVis = Public
            }
          body = EApp (ECon (mkQName "M" "Pair"))
                   [ELit (LitInt 1), ELit (LitInt 2)]
          d = mkFunDef "M" "make"
                body
                (TFun [] EffectRowEmpty intType)
          prog = (mkProgram "M" "main" [d]) { progData = [dd] }
      in typeCheckProgram prog @?= []

  , testCase "ill-typed: unknown constructor (with module qualifier)" $
      let body = ECon (mkQName "M" "Bogus")
          d = mkFunDef "M" "f" body
                (TFun [] EffectRowEmpty intType)
          prog = mkProgram "M" "main" [d]
      in typeCheckProgram prog
           @?= [UnknownConstructor (mkQName "M" "Bogus")]

  , testCase "ill-typed: pattern arity mismatch" $
      let pair = ConDecl
            { conName = mkQName "M" "Pair"
            , conFields = [(mkName "x", intType), (mkName "y", intType)]
            , conVis = Public
            }
          dd = DataDecl
            { dataName = mkQName "M" "Pair"
            , dataParams = []
            , dataCons = [pair]
            , dataVis = Public
            }
          -- pattern with one sub-pattern instead of two
          br = Branch (PatCon (mkQName "M" "Pair") [PatWild intType])
                 Nothing (ELit (LitInt 0))
          body = ECase (EVar (mkName "scrut")) [br]
          d = mkFunDef "M" "f" body
                (TFun [(Many, anyType)] EffectRowEmpty intType)
          prog = (mkProgram "M" "main" [d]) { progData = [dd] }
      in typeCheckProgram prog
           @?= [PatternArityMismatch (mkQName "M" "Pair") 2 1]

  , testCase "ill-typed: function arity mismatch on toplevel call" $
      let -- 2-arg add definition
          addDef = mkFunDef "M" "add"
            (ELam [(mkName "x", intType), (mkName "y", intType)]
              (ELit (LitInt 0)))
            (TFun [(Many, intType), (Many, intType)]
                  EffectRowEmpty intType)
          -- caller passes only 1 arg
          caller = mkFunDef "M" "wrong"
            (EApp (EVar (mkName "add")) [ELit (LitInt 1)])
            (TFun [] EffectRowEmpty intType)
          prog = mkProgram "M" "main" [addDef, caller]
      in typeCheckProgram prog
           @?= [FnArityMismatch (mkQName "M" "add") 2 1]

  , testCase "ill-typed: empty case has no branches" $
      let body = ECase (EVar (mkName "x")) []
          d = mkFunDef "M" "f" body
                (TFun [(Many, anyType)] EffectRowEmpty intType)
          prog = mkProgram "M" "main" [d]
      in typeCheckProgram prog @?= [EmptyCase]
  ]

-------------------------------------------------------------------------------
-- OrganInterface & Separate Compilation Tests
-------------------------------------------------------------------------------

organInterfaceTests :: TestTree
organInterfaceTests = testGroup "OrganInterface Separate Compilation"
  [ testCase "content tag determinism: identical ADTs in separate TUs produce identical tags" $ do
      let tag1 = mkContentTag "Data.List" "List" "Cons"
      let tag2 = mkContentTag "Data.List" "List" "Cons"
      tag1 @?= tag2
      unContentTag tag1 /= 0 @? "Content tag should be non-zero"

  , testCase "content tag disjointness: distinct constructors never collide" $ do
      let tagA = mkContentTag "ModA" "Tree" "Leaf"
      let tagB = mkContentTag "ModB" "Tree" "Leaf"
      let tagC = mkContentTag "ModA" "Tree" "Node"
      tagA /= tagB @? "Tags from different modules must not collide"
      tagA /= tagC @? "Tags for different constructors must not collide"

  , testCase "cross-TU check: matching exports and imports succeed" $ do
      let expA = ExportAbi "ModA.compute" 2 0 Profile0FreestandingLinear False
      let ifaceA = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = "ModA"
            , oiSourceLang    = "Haskell"
            , oiCtors         = [CtorDescriptor "ModA.Leaf" (mkContentTag "ModA" "Tree" "Leaf") 0 False]
            , oiExports       = [expA]
            , oiImports       = []
            }
      let reqB = ImportReq "ModA.compute" 2 0 Profile0FreestandingLinear
      let ifaceB = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = "ModB"
            , oiSourceLang    = "Haskell"
            , oiCtors         = []
            , oiExports       = []
            , oiImports       = [reqB]
            }
      case checkInterfacePair ifaceA ifaceB of
        Left errs -> assertFailure ("Unexpected interface check failure: " ++ show errs)
        Right m   -> Map.lookup "ModA.compute" m @?= Just expA

  , testCase "cross-TU check: catches arity mismatch" $ do
      let expA = ExportAbi "ModA.fn" 2 0 Profile1ThreadLocalPerceus False
      let reqB = ImportReq "ModA.fn" 3 0 Profile1ThreadLocalPerceus
      let ifaceA = OrganInterface "1.0" "ModA" "Haskell" [] [expA] []
      let ifaceB = OrganInterface "1.0" "ModB" "Haskell" [] [] [reqB]
      case checkInterfacePair ifaceA ifaceB of
        Left [ArityMismatch "ModA.fn" 3 2] -> pure ()
        other -> assertFailure ("Expected ArityMismatch, got: " ++ show other)

  , testCase "cross-TU check: catches evidence-count mismatch" $ do
      let expA = ExportAbi "ModA.fn" 2 0 Profile1ThreadLocalPerceus False
      let reqB = ImportReq "ModA.fn" 2 1 Profile1ThreadLocalPerceus
      let ifaceA = OrganInterface "1.0" "ModA" "Haskell" [] [expA] []
      let ifaceB = OrganInterface "1.0" "ModB" "Haskell" [] [] [reqB]
      case checkInterfacePair ifaceA ifaceB of
        Left [EvidenceMismatch "ModA.fn" 1 0] -> pure ()
        other -> assertFailure ("Expected EvidenceMismatch, got: " ++ show other)

  , testCase "cross-TU check: catches profile incompatibility" $ do
      let expA = ExportAbi "ModA.fn" 2 0 Profile0FreestandingLinear False
      let reqB = ImportReq "ModA.fn" 2 0 Profile2ConcurrentShared
      let ifaceA = OrganInterface "1.0" "ModA" "Haskell" [] [expA] []
      let ifaceB = OrganInterface "1.0" "ModB" "Haskell" [] [] [reqB]
      case checkInterfacePair ifaceA ifaceB of
        Left [ProfileIncompatibility "ModA.fn" Profile2ConcurrentShared Profile0FreestandingLinear] -> pure ()
        other -> assertFailure ("Expected ProfileIncompatibility, got: " ++ show other)

  , testCase "JSON round-trip: encodes and decodes OrganInterface bit-for-bit" $ do
      let iface = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = "Test.Module"
            , oiSourceLang    = "Haskell"
            , oiCtors         =
                [ CtorDescriptor "Test.Nil" (mkContentTag "Test" "List" "Nil") 0 False
                , CtorDescriptor "Test.Cons" (mkContentTag "Test" "List" "Cons") 2 True
                ]
            , oiExports       =
                [ ExportAbi "Test.map" 2 0 Profile1ThreadLocalPerceus False
                , ExportAbi "Test.fold" 3 1 Profile0FreestandingLinear True
                ]
            , oiImports       =
                [ ImportReq "Foreign.alloc" 1 0 Profile0FreestandingLinear
                , ImportReq "Foreign.free" 1 0 Profile1ThreadLocalPerceus
                ]
            }
      let jsonText = encodeOrganInterfaceJson iface
      case decodeOrganInterfaceJson jsonText of
        Left err -> assertFailure ("JSON decode error: " ++ T.unpack err ++ "\nJSON was:\n" ++ T.unpack jsonText)
        Right decoded -> decoded @?= iface
  ]

-------------------------------------------------------------------------------
-- N. GHC Plugin & MVP Haskell Translation Tests (TP-INGOLSTADT-MVP-002)
-------------------------------------------------------------------------------

ghcPluginTests :: TestTree
ghcPluginTests = testGroup "GhcBridge.Plugin"
  [ testCase "parsePluginOpts: parses out and verbose flags" $ do
      let cfg1 = parsePluginOpts ["out=/tmp/organ-out", "verbose"]
      pcOutDir cfg1 @?= "/tmp/organ-out"
      pcVerbose cfg1 @?= True

      let cfg2 = parsePluginOpts []
      pcOutDir cfg2 @?= ".frankenstein-out"
      pcVerbose cfg2 @?= False

  , testCase "emitModuleOrganArtifacts: emits valid .organ-ir and .organ-iface for fixtures" $ do
      -- Test fixture translation and artifact emission
      let modTree = "TreeTransform"
      let ifaceTree = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = modTree
            , oiSourceLang    = "Haskell"
            , oiCtors         =
                [ CtorDescriptor "TreeTransform.Leaf" (mkContentTag modTree "Tree" "Leaf") 1 False
                , CtorDescriptor "TreeTransform.Node" (mkContentTag modTree "Tree" "Node") 2 False
                ]
            , oiExports       =
                [ ExportAbi "TreeTransform.treeSize" 1 0 Profile1ThreadLocalPerceus False
                ]
            , oiImports       = []
            }
      let modCaller = "CrossModCaller"
      let ifaceCaller = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = modCaller
            , oiSourceLang    = "Haskell"
            , oiCtors         = []
            , oiExports       =
                [ ExportAbi "CrossModCaller.compute" 1 0 Profile1ThreadLocalPerceus False
                ]
            , oiImports       =
                [ ImportReq "TreeTransform.treeSize" 1 0 Profile1ThreadLocalPerceus
                ]
            }

      -- Validate cross-module interface check across the pair
      case checkInterfacePair ifaceTree ifaceCaller of
        Left errs -> assertFailure ("Unexpected interface check failure: " ++ show errs)
        Right m   -> Map.member "TreeTransform.treeSize" m @? "Export should be registered"

      -- Verify JSON encoding round-trip
      let treeJson = encodeOrganInterfaceJson ifaceTree
      case decodeOrganInterfaceJson treeJson of
        Left err -> assertFailure ("Decode error: " ++ T.unpack err)
        Right dec -> dec @?= ifaceTree

  , testCase "fixture modules: check all 5 fixtures exist" $ do
      let fixtures =
            [ "test/fixtures/mvp-haskell/ArithPrim.hs"
            , "test/fixtures/mvp-haskell/TreeTransform.hs"
            , "test/fixtures/mvp-haskell/HigherOrder.hs"
            , "test/fixtures/mvp-haskell/LazyPair.hs"
            , "test/fixtures/mvp-haskell/CrossModCaller.hs"
            ]
      mapM_ (\f -> do
        ex <- doesFileExist f
        ex @? ("Fixture file should exist: " ++ f)
        ) fixtures
  ]

--------------------------------------------------------------------------------
-- O. C11 Golden Emission & Liborgan_rt Tests (TP-INGOLSTADT-MVP-003)
--------------------------------------------------------------------------------

c11EmitTests :: TestTree
c11EmitTests = testGroup "Emit.C11"
  [ testCase "emitOrganC11: emits organ_rt.h include and main" $ do
      let prog = mkProgram "TestMod" "testProg"
            [ mkFunDef "TestMod" "answer" (ELit (LitInt 42)) intType ]
          c11 = emitOrganC11 prog
      assertBool "contains #include \"organ_rt.h\"" (T.isInfixOf "#include \"organ_rt.h\"" c11)
      assertBool "contains int main" (T.isInfixOf "int main" c11)
      assertBool "contains TestMod_answer" (T.isInfixOf "TestMod_answer" c11)
      assertBool "returns 42" (T.isInfixOf "return 42" c11)
  ]

--------------------------------------------------------------------------------
-- P. Polyglot Linker Tests (TP-INGOLSTADT-MVP-004)
--------------------------------------------------------------------------------

polyglotLinkTests :: TestTree
polyglotLinkTests = testGroup "PolyglotLink"
  [ testCase "buildPolyglotLinkPlan: builds valid plan and ffi edges" $ do
      let ifaceA = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = "ModuleA"
            , oiSourceLang    = "Haskell"
            , oiCtors         = []
            , oiExports       = [ ExportAbi "ModuleA.calc" 1 0 Profile1ThreadLocalPerceus False ]
            , oiImports       = [ ImportReq "ModuleB.foreignHelper" 1 0 Profile0FreestandingLinear ]
            }
          ifaceB = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = "ModuleB"
            , oiSourceLang    = "C"
            , oiCtors         = []
            , oiExports       = [ ExportAbi "ModuleB.foreignHelper" 1 0 Profile0FreestandingLinear False ]
            , oiImports       = []
            }
          unitA = TranslationUnit "ModuleA.o" ifaceA
          unitB = TranslationUnit "ModuleB.o" ifaceB
      case buildPolyglotLinkPlan [unitA, unitB] of
        Left errs -> assertFailure ("Link plan error: " ++ show errs)
        Right plan -> do
          length (lpFfiEdges plan) @?= 1
          let edge = head (lpFfiEdges plan)
          feCallerMod edge @?= "ModuleA"
          feCallerLang edge @?= "Haskell"
          feCalleeMod edge @?= "ModuleB"
          feCalleeLang edge @?= "C"
          feSymbol edge @?= "ModuleB.foreignHelper"
          feProfile edge @?= Profile0FreestandingLinear
          let header = renderExternHeaderC11 plan
          assertBool "header contains extern int64_t ModuleB_foreignHelper"
            (T.isInfixOf "extern int64_t ModuleB_foreignHelper" header)
          assertBool "header contains extern int64_t ModuleA_calc"
            (T.isInfixOf "extern int64_t ModuleA_calc" header)
  ]

--------------------------------------------------------------------------------
-- Q. LiquidProfile Tests (TP-INGOLSTADT-T1-001)
--------------------------------------------------------------------------------

liquidProfileTests :: TestTree
liquidProfileTests = testGroup "LiquidProfile"
  [ testCase "Positive promotion: unique usage promotes to Profile0" $ do
      let contract = LiquidContract
            { lcFuncName       = "updateUniqueTree"
            , lcInitialProfile = Profile1ThreadLocalPerceus
            , lcPreconditions  = [ PLe (AInt 0) (AVar "idx") ]
            , lcMaxUseCount    = 1
            , lcEscapesHeap    = False
            , lcFipAllocDelta  = 0
            }
          res = dischargeLinearityAndBounds contract [("idx", "len")]
      ppFinalProfile res @?= Profile0FreestandingLinear
      assertBool "Contains WitnessPromotedToProfile0"
        (WitnessPromotedToProfile0 `elem` ppWitnesses res)
      assertBool "Contains WitnessFipCertified"
        (WitnessFipCertified 0 `elem` ppWitnesses res)
      ppRejectedWhy res @?= []

  , testCase "Negative soundness: aliased usage (maxUses == 2) rejected" $ do
      let contract = LiquidContract
            { lcFuncName       = "sharedBranch"
            , lcInitialProfile = Profile1ThreadLocalPerceus
            , lcPreconditions  = []
            , lcMaxUseCount    = 2
            , lcEscapesHeap    = False
            , lcFipAllocDelta  = 1
            }
          res = dischargeLinearityAndBounds contract []
      ppFinalProfile res @?= Profile1ThreadLocalPerceus
      assertBool "Rejected with aliasing error"
        (any (T.isInfixOf "multiplicity > 1") (ppRejectedWhy res))

  , testCase "Negative soundness: heap-escaping reference rejected" $ do
      let contract = LiquidContract
            { lcFuncName       = "escapeGlobal"
            , lcInitialProfile = Profile1ThreadLocalPerceus
            , lcPreconditions  = []
            , lcMaxUseCount    = 1
            , lcEscapesHeap    = True
            , lcFipAllocDelta  = 0
            }
          res = dischargeLinearityAndBounds contract []
      ppFinalProfile res @?= Profile1ThreadLocalPerceus
      assertBool "Rejected with escape error"
        (any (T.isInfixOf "heap/thread escape") (ppRejectedWhy res))

  , testCase "Bounds-check elision: 0 <= i < n discharges; 0 <= i <= n rejected" $ do
      let validPre = [ PAnd (PLe (AInt 0) (AVar "i")) (PLt (AVar "i") (AVar "n")) ]
          invalidPre = [ PAnd (PLe (AInt 0) (AVar "i")) (PLe (AVar "i") (AVar "n")) ]
      proveStaticBounds validPre "i" "n" @?= True
      proveStaticBounds invalidPre "i" "n" @?= False

  , testCase "SMT-LIB2 query emission and Z3 validity" $ do
      let query = emitSmtLib2Query ["i", "n"]
            [ PLe (AInt 0) (AVar "i"), PLt (AVar "i") (AVar "n") ]
            (PLe (AInt 0) (AVar "i"))
      assertBool "contains set-logic QF_LIA" (T.isInfixOf "set-logic QF_LIA" query)
      z3Res <- runZ3Query query
      case z3Res of
        Left err -> assertFailure ("Z3 query error: " ++ T.unpack err)
        Right valid -> valid @?= True
  ]

--------------------------------------------------------------------------------
-- R. Opt.EGraph Tests (TP-INGOLSTADT-T1-002)
--------------------------------------------------------------------------------

optEGraphTests :: TestTree
optEGraphTests = testGroup "Opt.EGraph"
  [ testCase "Law 1: Perceus drop(dup(x)) -> ENop cancellation" $ do
      let expr = Opt.EDrop (Opt.EDup (Opt.EVar "x"))
      Opt.rewriteStep expr @?= Opt.ENop
      let (opt, stats) = Opt.saturateOrganExpr 10 expr
      opt @?= Opt.ENop
      assertBool "Cost strictly decreased" (Opt.rsCostAfter stats < Opt.rsCostBefore stats)

  , testCase "Law 3: Immediate force of thunk promotes to strict body" $ do
      let body = Opt.EAllocCtor "Pair" [Opt.EInt 1, Opt.EInt 2]
          expr = Opt.EForce (Opt.EAllocThunk body)
      Opt.rewriteStep expr @?= body
      let (opt, stats) = Opt.saturateOrganExpr 10 expr
      opt @?= body
      Opt.ocThunkAllocs (Opt.rsCostAfter stats) @?= 0
      assertBool "Thunk alloc eliminated" (Opt.ocThunkAllocs (Opt.rsCostAfter stats) < Opt.ocThunkAllocs (Opt.rsCostBefore stats))

  , testCase "Law 4: RowMorph fusion and identity deforestation" $ do
      -- p1 = [1, 0], p2 = [1, 0] -> composePerm = [0, 1] (identity!)
      let frame = Opt.EVar "frame"
          expr = Opt.ERowMorph [1, 0] (Opt.ERowMorph [1, 0] frame)
      Opt.rewriteStep expr @?= frame
      let (opt, _) = Opt.saturateOrganExpr 10 expr
      opt @?= frame

  , testCase "Law 5: Liquid-witnessed bounds-check elimination" $ do
      let arr = Opt.EVar "arr"
          idx = Opt.EVar "i"
          checkedProven = Opt.ECheckedIndex arr idx True
          checkedUnproven = Opt.ECheckedIndex arr idx False
      Opt.rewriteStep checkedProven @?= Opt.EUncheckedIndex arr idx
      Opt.rewriteStep checkedUnproven @?= checkedUnproven

  , testCase "Cost monotonicity across complex pipeline" $ do
      let complexExpr = Opt.ESeq
            (Opt.EDrop (Opt.EDup (Opt.EVar "buf")))
            (Opt.ESeq
              (Opt.EForce (Opt.EAllocThunk (Opt.EAllocCtor "Box" [Opt.EInt 42])))
              (Opt.ERowMorph [0, 1] (Opt.ECheckedIndex (Opt.EVar "v") (Opt.EInt 0) True)))
          (_opt, stats) = Opt.saturateOrganExpr 10 complexExpr
      assertBool "Cost monotonicity invariant (costAfter <= costBefore)"
        (Opt.rsCostAfter stats <= Opt.rsCostBefore stats)
      assertBool "Heap allocs non-increasing"
        (Opt.ocHeapAllocs (Opt.rsCostAfter stats) <= Opt.ocHeapAllocs (Opt.rsCostBefore stats))
      assertBool "RC ops reduced"
        (Opt.ocRcOps (Opt.rsCostAfter stats) < Opt.ocRcOps (Opt.rsCostBefore stats))
  ]

--------------------------------------------------------------------------------
-- S. SynquidHole Tests (TP-INGOLSTADT-T1-003)
--------------------------------------------------------------------------------

synquidHoleTests :: TestTree
synquidHoleTests = testGroup "Verify.SynquidHole"
  [ testCase "Bounds-safe indexed accessor fills hole with EUncheckedIndex" $ do
      let catalog =
            [ SynthComponent "arr" (SortArray "len") (Opt.EVar "arr")
            , SynthComponent "i"   (SortIndex "i")   (Opt.EVar "i")
            ]
          preconds = [PLe (AInt 0) (AVar "i"), PLt (AVar "i") (AVar "len")]
          goal = HoleGoal "elemHole" SortInt preconds catalog 10
      case synthesizeHole goal of
        SynthSolved expr stats -> do
          expr @?= Opt.EUncheckedIndex (Opt.EVar "arr") (Opt.EVar "i")
          Opt.ocBoundsChks (Opt.rsCostAfter stats) @?= 0
        other -> assertFailure ("Expected SynthSolved, got: " ++ show other)

  , testCase "Linear Profile-0 constructor transform emits zero EDup/EDrop" $ do
      let catalog =
            [ SynthComponent "x" SortInt (Opt.EVar "x")
            , SynthComponent "y" SortInt (Opt.EVar "y")
            ]
          goal = HoleGoal "valHole" SortInt [] catalog 10
      case synthesizeHole goal of
        SynthSolved expr stats -> do
          expr @?= Opt.EVar "x"
          Opt.ocRcOps (Opt.rsCostAfter stats) @?= 0
        other -> assertFailure ("Expected SynthSolved, got: " ++ show other)

  , testCase "Unsatisfiable holes halt cleanly returning SynthUnsatisfiable" $ do
      let catalog = [ SynthComponent "arr" (SortArray "n") (Opt.EVar "arr") ]
          goal = HoleGoal "emptyHole" SortFrame [] catalog 10
      case synthesizeHole goal of
        SynthUnsatisfiable name -> name @?= "emptyHole"
        other -> assertFailure ("Expected SynthUnsatisfiable, got: " ++ show other)

  , testCase "Fuel-exhausted hole halts returning SynthOutOfFuel" $ do
      let goal = HoleGoal "zeroFuel" SortInt [] [] 0
      case synthesizeHole goal of
        SynthOutOfFuel f -> f @?= 0
        other -> assertFailure ("Expected SynthOutOfFuel, got: " ++ show other)
  ]

--------------------------------------------------------------------------------
-- T. Backpack.OpenRecordSig Tests (TP-INGOLSTADT-T2-001)
--------------------------------------------------------------------------------

backpackOpenRecordSigTests :: TestTree
backpackOpenRecordSigTests = testGroup "Backpack.OpenRecordSig"
  [ testCase "Pushout merge refines opaque type with manifest representation" $ do
      let s1 = (emptyModuleSig "SigA")
            { msTypes = Map.singleton "Elem" (TypeOpaque 0)
            , msLaws  = [PLe (AInt 0) (AVar "i")]
            }
          s2 = (emptyModuleSig "SigB")
            { msTypes = Map.singleton "Elem" (TypeManifest 0 "Int64")
            , msLaws  = [PLe (AInt 0) (AVar "i")]
            }
      case mergeModuleSigs "Merged" s1 s2 of
        Left errs -> assertFailure ("Unexpected merge error: " ++ show errs)
        Right m -> do
          msName m @?= "Merged"
          Map.lookup "Elem" (msTypes m) @?= Just (TypeManifest 0 "Int64")
          msLaws m @?= [PLe (AInt 0) (AVar "i")]

  , testCase "Incompatible manifest representations produce ManifestTypeConflict" $ do
      let s1 = (emptyModuleSig "SigA") { msTypes = Map.singleton "Elem" (TypeManifest 0 "Int64") }
          s2 = (emptyModuleSig "SigB") { msTypes = Map.singleton "Elem" (TypeManifest 0 "ByteSlice") }
      case mergeModuleSigs "Merged" s1 s2 of
        Left [ManifestTypeConflict "Elem" "Int64" "ByteSlice"] -> pure ()
        other -> assertFailure ("Expected ManifestTypeConflict, got: " ++ show other)

  , testCase "Mismatched value arities produce ValueArityConflict" $ do
      let v1 = ValueFieldSpec 1 "Elem -> Int" Profile0FreestandingLinear False
          v2 = ValueFieldSpec 2 "Elem -> Elem -> Int" Profile0FreestandingLinear False
          s1 = (emptyModuleSig "SigA") { msValues = Map.singleton "op" v1 }
          s2 = (emptyModuleSig "SigB") { msValues = Map.singleton "op" v2 }
      case mergeModuleSigs "Merged" s1 s2 of
        Left [ValueArityConflict "op" 1 2] -> pure ()
        other -> assertFailure ("Expected ValueArityConflict, got: " ++ show other)

  , testCase "Signature morphism renames type and value labels consistently" $ do
      let s0 = (emptyModuleSig "Sig0")
            { msTypes = Map.singleton "Target" (TypeManifest 0 "Text")
            , msValues = Map.singleton "render" (ValueFieldSpec 1 "Target -> String" Profile1ThreadLocalPerceus True)
            }
          morph = SigMorphism
            { smRenameTypes  = Map.singleton "Target" "TextTarget"
            , smRenameValues = Map.singleton "render" "renderText"
            }
          renamed = applySigMorphism morph s0
      Map.member "TextTarget" (msTypes renamed) @?= True
      Map.member "Target" (msTypes renamed) @?= False
      Map.member "renderText" (msValues renamed) @?= True
      Map.member "render" (msValues renamed) @?= False
  ]

--------------------------------------------------------------------------------
-- U. Backpack.StageInstantiate Tests (TP-INGOLSTADT-T2-002)
--------------------------------------------------------------------------------

backpackStageInstantiateTests :: TestTree
backpackStageInstantiateTests = testGroup "Backpack.StageInstantiate"
  [ testCase "Partial Instantiation Diamond: sequential equals simultaneous instantiation" $ do
      let envSig = (emptyModuleSig "EnvSig")
            { msTypes  = Map.singleton "Env" (TypeOpaque 0)
            , msValues = Map.singleton "lookup" (ValueFieldSpec 1 "Env -> Key -> Val" Profile0FreestandingLinear True)
            }
          metricSig = (emptyModuleSig "MetricSig")
            { msTypes  = Map.singleton "Metric" (TypeOpaque 0)
            , msValues = Map.singleton "count" (ValueFieldSpec 1 "Metric -> Int" Profile0FreestandingLinear True)
            }
          envImpl = ModuleImpl
            { miName       = "EnvIntMap"
            , miTypes      = Map.singleton "Env" (0, "IntMap")
            , miBodies     = Map.singleton "lookup" (ValueFieldSpec 1 "Env -> Key -> Val" Profile0FreestandingLinear True, Opt.EInt 42)
            , miProvenLaws = []
            }
          metricImpl = ModuleImpl
            { miName       = "MetricCounter"
            , miTypes      = Map.singleton "Metric" (0, "Counter")
            , miBodies     = Map.singleton "count" (ValueFieldSpec 1 "Metric -> Int" Profile0FreestandingLinear True, Opt.EInt 10)
            , miProvenLaws = []
            }
          pm = ParameterizedModule
            { pmName     = "App"
            , pmRequires = Map.fromList [("env", envSig), ("metric", metricSig)]
            , pmBodies   = Map.singleton "main" (Opt.ESeq (Opt.EVar "env.lookup") (Opt.EVar "metric.count"))
            }

      -- Sequential path
      case partialInstantiate "env" envImpl pm of
        Left errs -> assertFailure ("Sequential step 1 failed: " ++ show errs)
        Right pmPartial -> do
          Map.keys (pmRequires pmPartial) @?= ["metric"]
          case instantiateAndSpecialize 10 (Map.singleton "metric" metricImpl) pmPartial of
            Left errs -> assertFailure ("Sequential step 2 failed: " ++ show errs)
            Right imSeq -> do
              -- Simultaneous path
              case instantiateAndSpecialize 10 (Map.fromList [("env", envImpl), ("metric", metricImpl)]) pm of
                Left errs -> assertFailure ("Simultaneous instantiation failed: " ++ show errs)
                Right imSimul -> do
                  imBodies imSeq @?= imBodies imSimul
                  imName imSeq @?= "App"
                  Map.lookup "main" (imBodies imSeq) @?= Just (Opt.ESeq (Opt.EInt 42) (Opt.EInt 10))

  , testCase "Cross-boundary optimization: inlining collapses Perceus and bounds ops" $ do
      let envSig = (emptyModuleSig "EnvSig")
            { msTypes  = Map.singleton "Env" (TypeOpaque 0)
            , msValues = Map.singleton "get" (ValueFieldSpec 1 "Env -> Val" Profile0FreestandingLinear True)
            }
          envImpl = ModuleImpl
            { miName       = "EnvProvider"
            , miTypes      = Map.singleton "Env" (0, "RawBuffer")
            , miBodies     = Map.singleton "get" (ValueFieldSpec 1 "Env -> Val" Profile0FreestandingLinear True, Opt.EDup (Opt.EVar "buffer"))
            , miProvenLaws = []
            }
          pm = ParameterizedModule
            { pmName     = "Caller"
            , pmRequires = Map.singleton "env" envSig
            , pmBodies   = Map.fromList
                [ ("callerDrop", Opt.EDrop (Opt.EVar "env.get"))
                , ("callerIdx", Opt.ECheckedIndex (Opt.EVar "env.get") (Opt.EInt 0) True)
                ]
            }
      case instantiateAndSpecialize 10 (Map.singleton "env" envImpl) pm of
        Left errs -> assertFailure ("Specialization failed: " ++ show errs)
        Right im -> do
          -- EDrop (EDup buffer) should simplify to ENop
          Map.lookup "callerDrop" (imBodies im) @?= Just Opt.ENop
          -- CheckedIndex with True witness should simplify to EUncheckedIndex
          Map.lookup "callerIdx" (imBodies im) @?= Just (Opt.EUncheckedIndex (Opt.EDup (Opt.EVar "buffer")) (Opt.EInt 0))

  , testCase "Sheaf H^1 behavioral rejection: missing law and profile violation caught" $ do
      let law = PLe (AInt 0) (AVar "idx")
          sig = (emptyModuleSig "SafeSig")
            { msTypes  = Map.singleton "T" (TypeOpaque 0)
            , msValues = Map.singleton "fn" (ValueFieldSpec 1 "T -> Int" Profile0FreestandingLinear True)
            , msLaws   = [law]
            }
          -- Provider violating law
          implBadLaw = ModuleImpl
            { miName       = "ImplBadLaw"
            , miTypes      = Map.singleton "T" (0, "Int")
            , miBodies     = Map.singleton "fn" (ValueFieldSpec 1 "T -> Int" Profile0FreestandingLinear True, Opt.EInt 1)
            , miProvenLaws = []
            }
      case checkBehavioralEdge "testEdge" sig implBadLaw of
        [MissingBehavioralLaw "testEdge" l] -> l @?= law
        other -> assertFailure ("Expected MissingBehavioralLaw, got: " ++ show other)

      -- Provider violating MemProfile (requires Profile0, provides Profile1)
      let implBadProfile = ModuleImpl
            { miName       = "ImplBadProfile"
            , miTypes      = Map.singleton "T" (0, "Int")
            , miBodies     = Map.singleton "fn" (ValueFieldSpec 1 "T -> Int" Profile1ThreadLocalPerceus True, Opt.EInt 1)
            , miProvenLaws = [law]
            }
      case checkBehavioralEdge "testEdge" sig implBadProfile of
        [ProfileViolation "testEdge" "fn" Profile0FreestandingLinear Profile1ThreadLocalPerceus] -> pure ()
        other -> assertFailure ("Expected ProfileViolation, got: " ++ show other)

  , testCase "Sheaf graph verification: verifyModuleGraphSheaf detects unattached holes" $ do
      let pm1 = ParameterizedModule "M1" (Map.singleton "h1" (emptyModuleSig "S1")) Map.empty
          pm2 = ParameterizedModule "M2" Map.empty Map.empty
          pms = Map.fromList [("M1", pm1), ("M2", pm2)]
          obs = verifyModuleGraphSheaf pms Map.empty
      obs @?= [UnknownHoleLabel "M1" "h1"]
  ]

--------------------------------------------------------------------------------
-- V. Backpack.HsigBridge Tests (TP-INGOLSTADT-T2-003)
--------------------------------------------------------------------------------

backpackHsigBridgeTests :: TestTree
backpackHsigBridgeTests = testGroup "Backpack.HsigBridge"
  [ testCase "Parse Smirk.Target.Sig fixture with refinement law" $ do
      src <- TIO.readFile "test/fixtures/hsig/Smirk.Target.Sig.hsig"
      case parseHsigToOpenRecord src of
        Left err -> assertFailure ("Failed to parse Smirk.Target.Sig: " ++ show err)
        Right sig -> do
          msName sig @?= "Smirk.Target.Sig"
          Map.lookup "Target" (msTypes sig) @?= Just (TypeOpaque 0)
          case Map.lookup "targetLength" (msValues sig) of
            Nothing -> assertFailure "Missing targetLength"
            Just v -> do
              vfsArity v @?= 1
              vfsMinProfile v @?= Profile0FreestandingLinear
          case Map.lookup "indexCodeUnit" (msValues sig) of
            Nothing -> assertFailure "Missing indexCodeUnit"
            Just v -> do
              vfsArity v @?= 2
              vfsMinProfile v @?= Profile0FreestandingLinear
          msLaws sig @?= [PAnd (PLe (AInt 0) (AVar "idx")) (PLt (AVar "idx") (AVar "len"))]

  , testCase "Parse Cek.Env.Sig fixture with translucent type and arities" $ do
      src <- TIO.readFile "test/fixtures/hsig/Cek.Env.Sig.hsig"
      case parseHsigToOpenRecord src of
        Left err -> assertFailure ("Failed to parse Cek.Env.Sig: " ++ show err)
        Right sig -> do
          msName sig @?= "Cek.Env.Sig"
          Map.lookup "Env" (msTypes sig) @?= Just (TypeOpaque 0)
          Map.lookup "Key" (msTypes sig) @?= Just (TypeManifest 0 "Int")
          Map.lookup "Val" (msTypes sig) @?= Just (TypeOpaque 0)
          case Map.lookup "extendEnv" (msValues sig) of
            Nothing -> assertFailure "Missing extendEnv"
            Just v -> vfsArity v @?= 3

  , testCase "Instantiate Cek.Env.Sig via StageInstantiate and verify specialized structure" $ do
      src <- TIO.readFile "test/fixtures/hsig/Cek.Env.Sig.hsig"
      case parseHsigToOpenRecord src of
        Left err -> assertFailure ("Failed to parse Cek.Env.Sig: " ++ show err)
        Right sig -> do
          let envImpl = ModuleImpl
                { miName       = "Cek.Env.Fast"
                , miTypes      = Map.fromList [("Env", (0, "FastEnv")), ("Key", (0, "Int")), ("Val", (0, "FastVal"))]
                , miBodies     = Map.fromList
                    [ ("emptyEnv", (ValueFieldSpec 0 "Env" Profile0FreestandingLinear True, Opt.EInt 0))
                    , ("lookupEnv", (ValueFieldSpec 2 "Key -> Env -> Val" Profile0FreestandingLinear True, Opt.EVar "val"))
                    , ("extendEnv", (ValueFieldSpec 3 "Key -> Val -> Env -> Env" Profile0FreestandingLinear True, Opt.EVar "newEnv"))
                    ]
                , miProvenLaws = []
                }
              pm = ParameterizedModule
                { pmName     = "CekKernel"
                , pmRequires = Map.singleton "env" sig
                , pmBodies   = Map.singleton "evalVar" (Opt.EVar "env.lookupEnv")
                }
          case instantiateAndSpecialize 10 (Map.singleton "env" envImpl) pm of
            Left errs -> assertFailure ("Instantiation failed: " ++ show errs)
            Right im -> do
              Map.lookup "evalVar" (imBodies im) @?= Just (Opt.EVar "val")
  ]

--------------------------------------------------------------------------------
-- W. Driver.Registry Tests (TP-INGOLSTADT-T3-001)
--------------------------------------------------------------------------------

driverRegistryTests :: TestTree
driverRegistryTests = testGroup "Driver.Registry"
  [ testCase "Built-in registry loads all 6 manifests without error" $ do
      case loadBuiltInRegistry of
        Left err -> assertFailure ("Failed to load built-in registry: " ++ show err)
        Right reg -> do
          Map.size (drById reg) @?= 6
          validateRegistry reg @?= []

  , testCase "Lookup by extension resolves .hs, .c, .rs, .py, .f90, .v accurately" $ do
      case loadBuiltInRegistry of
        Left err -> assertFailure ("Registry error: " ++ show err)
        Right reg -> do
          fmap dmId (lookupByExtension ".hs" reg)  @?= Just "haskell"
          fmap dmId (lookupByExtension ".lhs" reg) @?= Just "haskell"
          fmap dmId (lookupByExtension ".c" reg)   @?= Just "c11"
          fmap dmId (lookupByExtension ".rs" reg)  @?= Just "rust"
          fmap dmId (lookupByExtension ".py" reg)  @?= Just "python-ext"
          fmap dmId (lookupByExtension ".f90" reg) @?= Just "fortran-ffi"
          fmap dmId (lookupByExtension ".v" reg)   @?= Just "rocq"
          lookupByExtension ".unknown" reg         @?= Nothing

  , testCase "Lookup by ID resolves all 6 drivers" $ do
      case loadBuiltInRegistry of
        Left err -> assertFailure ("Registry error: " ++ show err)
        Right reg -> do
          fmap dmDisplayName (lookupById "haskell" reg)     @?= Just "GHC Haskell"
          fmap dmDisplayName (lookupById "c11" reg)         @?= Just "C11 (ISO C 2011)"
          fmap dmDisplayName (lookupById "rust" reg)        @?= Just "Rust 2024"
          fmap dmDisplayName (lookupById "python-ext" reg)  @?= Just "Python C-API Extension"
          fmap dmDisplayName (lookupById "fortran-ffi" reg) @?= Just "Fortran ISO_C_BINDING FFI"
          fmap dmDisplayName (lookupById "rocq" reg)        @?= Just "Rocq / Coq / Iris Specification"

  , testCase "User override replaces existing manifest" $ do
      case loadBuiltInRegistry of
        Left err -> assertFailure ("Registry error: " ++ show err)
        Right reg -> do
          let customC11 = DriverManifest
                { dmId          = "c11"
                , dmDisplayName = "Custom Clang C11"
                , dmParadigm    = ParadigmSystems
                , dmExtensions  = [".c"]
                , dmIngest      = IngestNone
                , dmEmit        = EmitIdiomaticTarget "c11-custom" Nothing
                , dmFfiRules    = []
                , dmFormatter   = Nothing
                }
          case upsertDriverOverride customC11 reg of
            Left err -> assertFailure ("Override failed: " ++ show err)
            Right reg' -> do
              fmap dmDisplayName (lookupById "c11" reg') @?= Just "Custom Clang C11"
              fmap dmId (lookupByExtension ".c" reg') @?= Just "c11"
              lookupByExtension ".h" reg' @?= Nothing

  , testCase "Duplicate registration produces DuplicateDriverId" $ do
      case loadBuiltInRegistry of
        Left err -> assertFailure ("Registry error: " ++ show err)
        Right reg -> do
          case builtInDrivers of
            [] -> assertFailure "builtInDrivers is empty"
            (d0 : _) -> do
              case registerDriver d0 reg of
                Left (DuplicateDriverId did) -> did @?= dmId d0
                other -> assertFailure ("Expected DuplicateDriverId, got: " ++ show other)

  , testCase "Extension collision produces ExtensionCollision" $ do
      case loadBuiltInRegistry of
        Left err -> assertFailure ("Registry error: " ++ show err)
        Right reg -> do
          let colliding = DriverManifest
                { dmId          = "clash"
                , dmDisplayName = "Clashing Driver"
                , dmParadigm    = ParadigmFunctional
                , dmExtensions  = [".rs"]
                , dmIngest      = IngestNone
                , dmEmit        = EmitIdiomaticTarget "clash" Nothing
                , dmFfiRules    = []
                , dmFormatter   = Nothing
                }
          case registerDriver colliding reg of
            Left (ExtensionCollision ".rs" "rust" "clash") -> pure ()
            other -> assertFailure ("Expected ExtensionCollision, got: " ++ show other)

  , testCase "Empty driver capabilities produces EmptyDriverCapabilities" $ do
      let emptyDriver = DriverManifest
            { dmId          = "noop"
            , dmDisplayName = "Noop Driver"
            , dmParadigm    = ParadigmSystems
            , dmExtensions  = [".noop"]
            , dmIngest      = IngestNone
            , dmEmit        = EmitNone
            , dmFfiRules    = []
            , dmFormatter   = Nothing
            }
      case registerDriver emptyDriver emptyRegistry of
        Left (EmptyDriverCapabilities "noop") -> pure ()
        other -> assertFailure ("Expected EmptyDriverCapabilities, got: " ++ show other)

  , testCase "Invalid extension format produces InvalidExtensionFormat" $ do
      let badExtDriver = DriverManifest
            { dmId          = "badext"
            , dmDisplayName = "Bad Extension Driver"
            , dmParadigm    = ParadigmSystems
            , dmExtensions  = ["txt"]
            , dmIngest      = IngestNone
            , dmEmit        = EmitIdiomaticTarget "bad" Nothing
            , dmFfiRules    = []
            , dmFormatter   = Nothing
            }
      case registerDriver badExtDriver emptyRegistry of
        Left (InvalidExtensionFormat "badext" "txt") -> pure ()
        other -> assertFailure ("Expected InvalidExtensionFormat, got: " ++ show other)
  ]

--------------------------------------------------------------------------------
-- X. Style.Profile Tests (TP-INGOLSTADT-T3-002)
--------------------------------------------------------------------------------

styleProfileTests :: TestTree
styleProfileTests = testGroup "Style.Profile"
  [ testCase "learnStyleProfile on 2-space camelCase Haskell with where clauses" $ do
      let hsCorpus =
            [ "module Example where"
            , ""
            , "computeValue :: Int -> Int"
            , "computeValue x = helper x"
            , "  where"
            , "    helper y = y + 1"
            ]
          sp = learnStyleProfile "haskell-test" ParadigmFunctional hsCorpus
      spIndentWidth sp @?= 2
      spValueNaming sp @?= CamelCase
      assertBool "ipPreferWhereOverLet > 0.5" (ipPreferWhereOverLet (spIdiomPrefs sp) > 0.5)

  , testCase "learnStyleProfile on 4-space snake_case Rust with .iter()" $ do
      let rsCorpus =
            [ "fn compute_total(items: &[i64]) -> i64 {"
            , "    items.iter().map(|x| x * 2).sum()"
            , "}"
            ]
          sp = learnStyleProfile "rust-test" ParadigmSystems rsCorpus
      spIndentWidth sp @?= 4
      spValueNaming sp @?= SnakeCase
      assertBool "ipPreferPipelineOverLoop > 0.5" (ipPreferPipelineOverLoop (spIdiomPrefs sp) > 0.5)

  , testCase "stylePenalty strictly penalizes functions exceeding Crichton working memory" $ do
      let sp = defaultStyleForParadigm ParadigmSystems
          cmNormal = CognitiveMetrics 3 2 10 False False False
          cmHeavy  = CognitiveMetrics 7 5 30 False False False
      assertBool "cmHeavy penalty > cmNormal" (stylePenalty sp cmHeavy > stylePenalty sp cmNormal + 20.0)

  , testCase "formatIdentifier round-trips snake, camel, pascal, and kebab cases" $ do
      let raw = "eval_page_cluster"
      formatIdentifier CamelCase raw @?= "evalPageCluster"
      formatIdentifier PascalCase raw @?= "EvalPageCluster"
      formatIdentifier SnakeCase "evalPageCluster" @?= "eval_page_cluster"
      formatIdentifier KebabCase "evalPageCluster" @?= "eval-page-cluster"

  , testCase "rankEquivalentCandidates ranks candidates by ascending stylePenalty" $ do
      let sp = defaultStyleForParadigm ParadigmSystems
          cands :: [(Text, CognitiveMetrics)]
          cands = [("heavy", CognitiveMetrics 8 4 10 False False False), ("light", CognitiveMetrics 2 1 5 False False False)]
      rankEquivalentCandidates sp cands @?= ["light", "heavy"]

  , testCase "styleProfileToDhall produces valid non-empty Dhall text" $ do
      let sp = defaultStyleForParadigm ParadigmFunctional
          dhallText = styleProfileToDhall sp
      assertBool "Non-empty Dhall text" (not (T.null dhallText))
      assertBool "Contains profileName" (T.isInfixOf "profileName = \"functional-default\"" dhallText)
  ]

--------------------------------------------------------------------------------
-- Y. Emit.Idiomatic Tests (TP-INGOLSTADT-T3-003)
--------------------------------------------------------------------------------

emitIdiomaticTests :: TestTree
emitIdiomaticTests = testGroup "Emit.Idiomatic"
  [ testCase "Profile 0 emits pure Rust owned/mutable signatures with 0 Rc/Arc imports" $ do
      let ifaceP0 = OrganInterface
            { oiSchemaVersion = "1.0.0"
            , oiModuleName    = "Math.Vector"
            , oiSourceLang    = "haskell"
            , oiCtors         = []
            , oiExports       = [ExportAbi "add" 2 0 Profile0FreestandingLinear False]
            , oiImports       = []
            }
          sp = defaultStyleForParadigm ParadigmSystems
          rustCode = emitIdiomaticRustModule sp ifaceP0
      assertBool "No Rc import" (not (T.isInfixOf "use std::rc::Rc" rustCode))
      assertBool "No Arc import" (not (T.isInfixOf "use std::sync::Arc" rustCode))
      assertBool "Contains pub fn add" (T.isInfixOf "pub fn add" rustCode)
      assertBool "Contains Profile 0 doc" (T.isInfixOf "OrganIR Profile 0" rustCode)

  , testCase "Profile 1 emits std::rc::Rc, Profile 2 emits std::sync::Arc" $ do
      let sp = defaultStyleForParadigm ParadigmSystems
          ifaceP1 = OrganInterface "1.0.0" "Env" "haskell" [] [ExportAbi "get" 1 0 Profile1ThreadLocalPerceus False] []
          rustP1 = emitIdiomaticRustModule sp ifaceP1
      assertBool "Contains Rc import" (T.isInfixOf "use std::rc::Rc;" rustP1)

      let ifaceP2 = OrganInterface "1.0.0" "Sync" "rust" [] [ExportAbi "share" 1 0 Profile2ConcurrentShared False] []
          rustP2 = emitIdiomaticRustModule sp ifaceP2
      assertBool "Contains Arc import" (T.isInfixOf "use std::sync::Arc;" rustP2)

  , testCase "Emitted Python C-API glue exports PyInit_<mod> and PyArg_ParseTuple matching arity" $ do
      let iface = OrganInterface "1.0.0" "Math.Vector" "haskell" [] [ExportAbi "add" 2 0 Profile0FreestandingLinear False] []
          pyCode = emitPythonCapiExtension iface
      assertBool "Contains PyInit_math_vector" (T.isInfixOf "PyMODINIT_FUNC PyInit_math_vector" pyCode)
      assertBool "Contains PyArg_ParseTuple LL" (T.isInfixOf "PyArg_ParseTuple(args, \"LL\"" pyCode)

  , testCase "Emitted Fortran module contains iso_c_binding and bind(C, name=...)" $ do
      let iface = OrganInterface "1.0.0" "Math.Vector" "haskell" [] [ExportAbi "add" 2 0 Profile0FreestandingLinear False] []
          ftnCode = emitFortranIsoCBinding iface
      assertBool "Contains iso_c_binding" (T.isInfixOf "use, intrinsic :: iso_c_binding" ftnCode)
      assertBool "Contains bind(C, name=\"add\")" (T.isInfixOf "bind(C, name=\"add\")" ftnCode)

  , testCase "Emitted Rocq .v skeleton exports ContentTag definitions and signatures" $ do
      let ifaceTag = OrganInterface "1.0.0" "Tree" "haskell"
            [CtorDescriptor "Tree.Leaf" (ContentTag 101) 0 False]
            [ExportAbi "node" 2 0 Profile0FreestandingLinear False]
            []
          rocqCode = emitRocqSpecSkeleton ifaceTag
      assertBool "Contains ZArith" (T.isInfixOf "From Stdlib Require Import ZArith." rocqCode)
      assertBool "Contains Leaf_tag" (T.isInfixOf "Tree_Leaf_tag : Z := 101%Z" rocqCode)
      assertBool "Contains Parameter node" (T.isInfixOf "Parameter node : Z -> Z -> Z." rocqCode)
  ]

--------------------------------------------------------------------------------
-- Z. Transform.Api Tests (TP-INGOLSTADT-T3-004)
--------------------------------------------------------------------------------

transformApiTests :: TestTree
transformApiTests = testGroup "Transform.Api"
  [ testCase "Mode A surgical patch preserves surrounding comments and whitespace byte-for-byte" $ do
      let original = "-- Header comment\nfn foo() {\n  let x = ??hole1;\n  x + 1\n}\n-- Footer comment\n"
          sp = SourceSpan "test.rs" 39 46 -- span of "??hole1"
          edit = SourceEdit sp "42" "filled hole"
          res = applySourceEdits original [edit]
      case res of
        Left err -> assertFailure ("Expected Right, got: " <> T.unpack err)
        Right patched -> do
          let expected = "-- Header comment\nfn foo() {\n  let x = 42;\n  x + 1\n}\n-- Footer comment\n"
          patched @?= expected
          assertBool "Preserves header" (T.isPrefixOf "-- Header comment\n" patched)
          assertBool "Preserves footer" (T.isSuffixOf "\n-- Footer comment\n" patched)

  , testCase "Overlapping SourceEdit spans fail cleanly with Left instead of corrupting text" $ do
      let original = "abcdefghijklmnopqrstuvwxyz"
          e1 = SourceEdit (SourceSpan "t.txt" 0 10) "AAA" "r1"
          e2 = SourceEdit (SourceSpan "t.txt" 5 15) "BBB" "r2"
          res = applySourceEdits original [e1, e2]
      case res of
        Left err -> assertBool "Contains overlapping" (T.isInfixOf "overlapping" err)
        Right _  -> assertFailure "Expected failure on overlapping spans"

  , testCase "KindSynquidFillHole replaces ?? hole span and emits TransformWitness" $ do
      let original = "let y = ??calc in y"
          style = defaultStyleForParadigm ParadigmFunctional
          req = TransformRequest
            { trFilePath   = "sample.hs"
            , trSourceText = original
            , trMode       = ModeSurgicalPatch
            , trKind       = KindSynquidFillHole "calc"
            , trStyle      = style
            , trAnchorSpan = Nothing
            }
      case runTransformSession req of
        Left err -> assertFailure ("Transform session failed: " <> T.unpack err)
        Right rep -> do
          trpResultText rep @?= "let y = synthesized_calc in y"
          assertBool "Witness kind" (twKindLabel (trpWitness rep) == "SynquidFillHole")
          assertBool "Diff is non-empty" (not (T.null (trpUnifiedDiff rep)))

  , testCase "KindSheafBoundaryRepair executes and emits coboundary witness" $ do
      let original = "module Test where"
          style = defaultStyleForParadigm ParadigmFunctional
          req = TransformRequest "test.hs" original ModeSurgicalPatch KindSheafBoundaryRepair style Nothing
      case runTransformSession req of
        Left err -> assertFailure ("Sheaf repair failed: " <> T.unpack err)
        Right rep -> do
          trpResultText rep @?= original
          assertBool "Witness notes" (any (T.isInfixOf "H^1 coboundary check passed") (twNotes (trpWitness rep)))

  , testCase "renderLspWorkspaceEditJson emits valid LSP JSON with line/character offsets" $ do
      let original = "line0\nline1 with target here\nline2"
          sp = SourceSpan "src/Lib.hs" 17 23 -- span of "target"
          edit = SourceEdit sp "replacement" "fix"
          json = renderLspWorkspaceEditJson "src/Lib.hs" original [edit]
      assertBool "Contains file URI" (T.isInfixOf "\"file://src/Lib.hs\"" json)
      assertBool "Contains line 1" (T.isInfixOf "\"line\": 1" json)
      assertBool "Contains character 11" (T.isInfixOf "\"character\": 11" json)
      assertBool "Contains replacement" (T.isInfixOf "\"newText\": \"replacement\"" json)
  ]

--------------------------------------------------------------------------------
-- AA. Db.Incremental Tests (TP-INGOLSTADT-T4-001)
--------------------------------------------------------------------------------

dbIncrementalTests :: TestTree
dbIncrementalTests = testGroup "Db.Incremental"
  [ testCase "Durability Shortcut: DurabilityHigh query stays O(1) verified after 100 DurabilityLow edits" $ do
      let db0 = emptyCodeDb
          -- Seed DurabilityHigh stdlib input and memo
          db1 = setInput (QSourceText "stdlib/prelude.dhall") DurabilityHigh "let id = \\x -> x" db0
          qKey = QParseModule "stdlib/prelude.dhall"
          (cutoff, db2) = recordMemoWithCutoff qKey DurabilityHigh [QSourceText "stdlib/prelude.dhall"] "AST(id)" db1
      assertBool "First record no cutoff" (not cutoff)

      -- Apply 100 keystrokes in active file at DurabilityLow
      let dbEdited = foldl' (\db i -> setInput (QSourceText "active.hs") DurabilityLow ("keystroke " <> T.pack (show i)) db) db2 [1..100 :: Int]
      unRevision (dbRevision dbEdited) @?= 102

      -- DurabilityHigh memo must still be verified in O(1) without touching dependencies
      case Map.lookup qKey (dbMemos dbEdited) of
        Nothing -> assertFailure "Memo missing"
        Just entry -> assertBool "O(1) verified" (isEntryVerified entry dbEdited)

      -- runTrackedQuery must return cache hit without calling computeFn
      let failCompute _ = Left "Should not recompute DurabilityHigh query!"
      case runTrackedQuery qKey DurabilityHigh failCompute dbEdited of
        Left err -> assertFailure ("Expected cache hit, got: " <> T.unpack err)
        Right (val, wasHit, _) -> do
          val @?= "AST(id)"
          assertBool "Was cache hit" wasHit

  , testCase "Early Cutoff: comment change triggers early cutoff suppressing downstream passes" $ do
      let db0 = emptyCodeDb
          keySrc = QSourceText "A.hs"
          keyParse = QParseModule "A.hs"
          keySig = QDeclSignature "A.hs" "foo"
          keyLower = QLowerOrgan "A.hs" "foo"

          -- Step 1: Initial state
          db1 = setInput keySrc DurabilityLow "fn foo() { /* comment 1 */ return 42; }" db0
          -- Parse produces AST
          (_, db2) = recordMemoWithCutoff keyParse DurabilityLow [keySrc] "AST(foo, body1)" db1
          -- DeclSignature extracts signature (unchanged by comment!)
          (_, db3) = recordMemoWithCutoff keySig DurabilityLow [keyParse] "Sig(foo: () -> int)" db2
          -- LowerOrgan lowers signature to OrganIR
          (_, db4) = recordMemoWithCutoff keyLower DurabilityLow [keySig] "OrganIR(foo)" db3

          -- Step 2: Edit comment only in A.hs
          db5 = setInput keySrc DurabilityLow "fn foo() { /* comment 2 */ return 42; }" db4

          -- Parse recomputes because source changed
          (cutoffParse, db6) = recordMemoWithCutoff keyParse DurabilityLow [keySrc] "AST(foo, body2)" db5
      assertBool "Parse content changed" (not cutoffParse)

      -- DeclSignature recomputes but returns IDENTICAL signature
      let (cutoffSig, db7) = recordMemoWithCutoff keySig DurabilityLow [keyParse] "Sig(foo: () -> int)" db6
      assertBool "Early cutoff triggered on signature" cutoffSig

      -- Key check: QDeclSignature's meChangedAt was preserved!
      case (Map.lookup keySig (dbMemos db4), Map.lookup keySig (dbMemos db7)) of
        (Just eOld, Just eNew) -> meChangedAt eOld @?= meChangedAt eNew
        _ -> assertFailure "Signature memo missing"

      -- Step 3: Run LowerOrgan: its dependency (keySig) has meChangedAt <= meVerifiedAt (keyLower)!
      let failLower _ = Left "LowerOrgan should NOT recompute when signature hash is unchanged!"
      case runTrackedQuery keyLower DurabilityLow failLower db7 of
        Left err -> assertFailure ("Expected cache hit, got: " <> T.unpack err)
        Right (val, wasHit, _) -> do
          val @?= "OrganIR(foo)"
          assertBool "LowerOrgan hit early cutoff cache" wasHit

  , testCase "True Invalidation: changing function signature invalidates dependent queries" $ do
      let db0 = emptyCodeDb
          keySrc = QSourceText "B.hs"
          keyParse = QParseModule "B.hs"
          keySig = QDeclSignature "B.hs" "bar"
          keyLower = QLowerOrgan "B.hs" "bar"

          -- Initial run
          db1 = setInput keySrc DurabilityLow "fn bar(x: int) -> int { return x; }" db0
          (_, db2) = recordMemoWithCutoff keyParse DurabilityLow [keySrc] "AST(bar: int -> int)" db1
          (_, db3) = recordMemoWithCutoff keySig DurabilityLow [keyParse] "Sig(bar: int -> int)" db2
          (_, db4) = recordMemoWithCutoff keyLower DurabilityLow [keySig] "OrganIR(bar_v1)" db3

          -- Advance with signature change via setInput
          db5 = setInput keySrc DurabilityLow "fn bar(x: float) -> float { return x; }" db4
          -- Signature changes
          (cutoffSig, db6) = recordMemoWithCutoff keySig DurabilityLow [keyParse] "Sig(bar: float -> float)" db5
      assertBool "Signature change did not cut off" (not cutoffSig)

      -- LowerOrgan sees changed dependency and recomputes
      let recomputeLower dbCurrent = Right ("OrganIR(bar_v2)", [keySig], dbCurrent)
      case runTrackedQuery keyLower DurabilityLow recomputeLower db6 of
        Left err -> assertFailure ("Recompute failed: " <> T.unpack err)
        Right (val, wasHit, _) -> do
          val @?= "OrganIR(bar_v2)"
          assertBool "LowerOrgan recomputed (wasHit == False)" (not wasHit)

  , testCase "Cycle Guard: query cycle returns Left cleanly instead of infinite loop" $ do
      let db = emptyCodeDb
          keyA = QParseModule "A.hs"
          keyB = QParseModule "B.hs"
          -- Query A calls B which calls A
          computeB dbCurrent =
            case runTrackedQueryWithStack [keyB, keyA] keyA DurabilityLow (\_ -> Left "unreachable") dbCurrent of
              Left err -> Left err
              Right (v, _, db') -> Right (v, [keyA], db')
          computeA dbCurrent =
            case runTrackedQueryWithStack [keyA] keyB DurabilityLow computeB dbCurrent of
              Left err -> Left err
              Right (v, _, db') -> Right (v, [keyB], db')
      case runTrackedQuery keyA DurabilityLow computeA db of
        Left err -> assertBool "Detects cycle" (T.isInfixOf "cycle detected" err)
        Right _  -> assertFailure "Expected cycle detection error"
  ]

--------------------------------------------------------------------------------
-- BB. Db.ResilientEditCone Tests (TP-INGOLSTADT-T4-002)
--------------------------------------------------------------------------------

resilientEditConeTests :: TestTree
resilientEditConeTests = testGroup "Db.ResilientEditCone"
  [ testCase "Stope-and-Winze: Corrupting 3 of 10 declarations isolates exact ParseHoles while 7 siblings parse cleanly" $ do
      let mod10Corrupted = T.unlines
            [ "fn d1() { return 1; }"
            , ""
            , "fn d2( { return 2; }"
            , ""
            , "fn d3() { return 3; }"
            , ""
            , "fn d4() { return 4; }"
            , ""
            , "fn d5() { let s = \"unterminated; return 5; }"
            , ""
            , "fn d6() { return 6; }"
            , ""
            , "fn d7() { return 7; }"
            , ""
            , "fn d8() { SYNTAX_ERROR }"
            , ""
            , "fn d9() { return 9; }"
            , ""
            , "fn d10() { return 10; }"
            ]
          rm = parseResilientModule "Module10.hs" mod10Corrupted
          valid = validSiblingDecls rm
          holes = collectParseHoles rm
      length (rmDecls rm) @?= 10
      length valid @?= 7
      length holes @?= 3
      map rdName valid @?= ["d1", "d3", "d4", "d6", "d7", "d9", "d10"]
      map (\(n, _, _) -> n) holes @?= ["d2", "d5", "d8"]

      -- Verify resilientModuleToInterface exports exactly the 7 valid siblings
      let iface = resilientModuleToInterface rm
      oiModuleName iface @?= "Module10"
      map eaQualifiedName (oiExports iface) @?= ["d1", "d3", "d4", "d6", "d7", "d9", "d10"]

  , testCase "Body-Only Early Cutoff: Modifying decl4 body leaves sigHash unchanged, landing in ecrEarlyCutoffDecls with 9 siblings preserved" $ do
      let mod10Old = T.unlines
            [ "fn d1()"
            , "  { return 1; }"
            , ""
            , "fn d2()"
            , "  { return 2; }"
            , ""
            , "fn d3()"
            , "  { return 3; }"
            , ""
            , "fn d4()"
            , "  { return 4; }"
            , ""
            , "fn d5()"
            , "  { return 5; }"
            , ""
            , "fn d6()"
            , "  { return 6; }"
            , ""
            , "fn d7()"
            , "  { return 7; }"
            , ""
            , "fn d8()"
            , "  { return 8; }"
            , ""
            , "fn d9()"
            , "  { return 9; }"
            , ""
            , "fn d10()"
            , "  { return 10; }"
            ]
          mod10BodyEdited = T.unlines
            [ "fn d1()"
            , "  { return 1; }"
            , ""
            , "fn d2()"
            , "  { return 2; }"
            , ""
            , "fn d3()"
            , "  { return 3; }"
            , ""
            , "fn d4()"
            , "  { return 44; }"
            , ""
            , "fn d5()"
            , "  { return 5; }"
            , ""
            , "fn d6()"
            , "  { return 6; }"
            , ""
            , "fn d7()"
            , "  { return 7; }"
            , ""
            , "fn d8()"
            , "  { return 8; }"
            , ""
            , "fn d9()"
            , "  { return 9; }"
            , ""
            , "fn d10()"
            , "  { return 10; }"
            ]
          oldRm = parseResilientModule "Mod.hs" mod10Old
          newRm = parseResilientModule "Mod.hs" mod10BodyEdited
          cone = reparseEditCone oldRm newRm
      ecrEarlyCutoffDecls cone @?= ["d4"]
      ecrPreservedSiblings cone @?= ["d1", "d2", "d3", "d5", "d6", "d7", "d8", "d9", "d10"]
      ecrInvalidatedDecls cone @?= []
      ecrParseHoles cone @?= []

  , testCase "Signature Change Invalidation: Changing decl4 signature lands it in ecrInvalidatedDecls while preserving 9 siblings" $ do
      let mod10Old = T.unlines
            [ "fn d1()"
            , "  { return 1; }"
            , ""
            , "fn d2()"
            , "  { return 2; }"
            , ""
            , "fn d3()"
            , "  { return 3; }"
            , ""
            , "fn d4()"
            , "  { return 4; }"
            , ""
            , "fn d5()"
            , "  { return 5; }"
            , ""
            , "fn d6()"
            , "  { return 6; }"
            , ""
            , "fn d7()"
            , "  { return 7; }"
            , ""
            , "fn d8()"
            , "  { return 8; }"
            , ""
            , "fn d9()"
            , "  { return 9; }"
            , ""
            , "fn d10()"
            , "  { return 10; }"
            ]
          mod10SigEdited = T.unlines
            [ "fn d1()"
            , "  { return 1; }"
            , ""
            , "fn d2()"
            , "  { return 2; }"
            , ""
            , "fn d3()"
            , "  { return 3; }"
            , ""
            , "fn d4(x: int)"
            , "  { return x; }"
            , ""
            , "fn d5()"
            , "  { return 5; }"
            , ""
            , "fn d6()"
            , "  { return 6; }"
            , ""
            , "fn d7()"
            , "  { return 7; }"
            , ""
            , "fn d8()"
            , "  { return 8; }"
            , ""
            , "fn d9()"
            , "  { return 9; }"
            , ""
            , "fn d10()"
            , "  { return 10; }"
            ]
          oldRm = parseResilientModule "Mod.hs" mod10Old
          newRm = parseResilientModule "Mod.hs" mod10SigEdited
          cone = reparseEditCone oldRm newRm
      ecrInvalidatedDecls cone @?= ["d4"]
      ecrEarlyCutoffDecls cone @?= []
      ecrPreservedSiblings cone @?= ["d1", "d2", "d3", "d5", "d6", "d7", "d8", "d9", "d10"]

  , testCase "syncModuleIntoCodeDb populates signature memos and isolates parse holes" $ do
      let src = T.unlines
            [ "fn ok1() { return 1; }"
            , ""
            , "fn bad( { return 2; }"
            , ""
            , "fn ok2() { return 3; }"
            ]
          rm = parseResilientModule "SyncTest.hs" src
          db0 = emptyCodeDb
          (report, db1) = syncModuleIntoCodeDb rm db0
      ecrParseHoles report @?= collectParseHoles rm
      assertBool "ok1 in memos" (Map.member (QDeclSignature "SyncTest.hs" "ok1") (dbMemos db1))
      assertBool "ok2 in memos" (Map.member (QDeclSignature "SyncTest.hs" "ok2") (dbMemos db1))
      assertBool "bad NOT in memos" (not (Map.member (QDeclSignature "SyncTest.hs" "bad") (dbMemos db1)))
  ]

--------------------------------------------------------------------------------
-- CC. Ide.LiveSession Tests (TP-INGOLSTADT-T4-003)
--------------------------------------------------------------------------------

ideLiveSessionTests :: TestTree
ideLiveSessionTests = testGroup "Ide.LiveSession"
  [ testCase "Interactive didChange with syntax error in decl5 emits exactly 1 LiveDiagnostic preserving 9 siblings" $ do
      let sp = defaultStyleForParadigm ParadigmSystems
          sess0 = initLiveSession sp
          fp = "src/Workspace.hs"
          clean10 = T.unlines
            [ "fn d1() { return 1; }"
            , ""
            , "fn d2() { return 2; }"
            , ""
            , "fn d3() { return 3; }"
            , ""
            , "fn d4() { return 4; }"
            , ""
            , "fn d5() { return 5; }"
            , ""
            , "fn d6() { return 6; }"
            , ""
            , "fn d7() { return 7; }"
            , ""
            , "fn d8() { return 8; }"
            , ""
            , "fn d9() { return 9; }"
            , ""
            , "fn d10() { return 10; }"
            ]
          res0 = applyBufferChange fp clean10 sess0
          sessClean = dcrSession res0
          srcWithHole5 = T.unlines
            [ "fn d1() { return 1; }"
            , ""
            , "fn d2() { return 2; }"
            , ""
            , "fn d3() { return 3; }"
            , ""
            , "fn d4() { return 4; }"
            , ""
            , "fn d5( { return 5; }"  -- syntax error in d5
            , ""
            , "fn d6() { return 6; }"
            , ""
            , "fn d7() { return 7; }"
            , ""
            , "fn d8() { return 8; }"
            , ""
            , "fn d9() { return 9; }"
            , ""
            , "fn d10() { return 10; }"
            ]
          res = applyBufferChange fp srcWithHole5 sessClean
          diags = dcrDiagnostics res
          cone = dcrEditCone res
      length diags @?= 1
      case diags of
        [] -> assertFailure "Expected 1 diagnostic, got 0"
        (d:_) -> do
          ldCode d @?= "INGOLSTADT-PARSE-HOLE"
          assertBool "Contains d5" (T.isInfixOf "d5" (ldMessage d))
      ecrInvalidatedDecls cone @?= ["d5"]
      ecrPreservedSiblings cone @?= ["d1", "d2", "d3", "d4", "d6", "d7", "d8", "d9", "d10"]

  , testCase "recordStyleFeedback monotonically updates ipPreferWhereOverLet in [0.0, 1.0]" $ do
      let sp = defaultStyleForParadigm ParadigmFunctional
          sess0 = initLiveSession sp
          initVal = ipPreferWhereOverLet (spIdiomPrefs sp)
          sess1 = recordStyleFeedback AxisWhereOverLet FeedbackAccepted sess0
          val1 = ipPreferWhereOverLet (spIdiomPrefs (lisStyle sess1))
          sess2 = recordStyleFeedback AxisWhereOverLet FeedbackAccepted sess1
          val2 = ipPreferWhereOverLet (spIdiomPrefs (lisStyle sess2))
          sess3 = recordStyleFeedback AxisWhereOverLet FeedbackRejected sess2
          val3 = ipPreferWhereOverLet (spIdiomPrefs (lisStyle sess3))
      assertBool "Moves upward on accepted" (val1 > initVal)
      assertBool "Continues upward on second accepted" (val2 > val1)
      assertBool "Moves downward on rejected" (val3 < val2)
      assertBool "Within [0.0, 1.0]" (val1 >= 0.0 && val1 <= 1.0 && val3 >= 0.0 && val3 <= 1.0)
      lisFeedbackN sess3 @?= 3

  , testCase "proposeCodeActions detects ?? hole inside a valid declaration and returns quickfix action" $ do
      let sp = defaultStyleForParadigm ParadigmSystems
          sess0 = initLiveSession sp
          fp = "src/HoleTest.hs"
          srcWithHole = T.unlines
            [ "fn calc() {"
            , "  let x = ??fill_target;"
            , "  return x;"
            , "}"
            ]
          res = applyBufferChange fp srcWithHole sess0
          sess1 = dcrSession res
          actions = proposeCodeActions fp 10 sess1
      assertBool "Returns at least one code action" (not (null actions))
      assertBool "Contains synquid action" (any (\a -> icaKind a == "quickfix.ingolstadt.synquid") actions)
      assertBool "Contains egraph refactor action" (any (\a -> icaKind a == "refactor.rewrite.ingolstadt.egraph") actions)
  ]


-------------------------------------------------------------------------------
-- S. RTS.UringDriver unit tests
-------------------------------------------------------------------------------

rtsUringDriverTests :: TestTree
rtsUringDriverTests = testGroup "RTS.UringDriver"
  [ testCase "probeUringKernelSupport returns valid kernel support status" $ do
      status <- probeUringKernelSupport
      assertBool "Returns NativeSupported or SandboxBlockedFallback"
        (status `elem` [NativeSupported, SandboxBlockedFallback])

  , testCase "lowerBatchToC11 emits C11 calls to organ_uring_submit_obj" $ do
      let sqes =
            [ UringSqe 1 (UringReadFixed 0 4096 0) False False "obj_buf1"
            , UringSqe 2 (UringWriteFixed 1 4096 0) True True "linear_buf2"
            , UringSqe 3 (UringFsync 1 True) True False "fsync_buf3"
            ]
          batch = UringBatch sqes
          c11 = lowerBatchToC11 "core_reactor" batch
      assertBool "Contains organ_uring_submit_obj" (T.isInfixOf "organ_uring_submit_obj" c11)
      assertBool "Contains ORGAN_PROFILE_THREAD_LOCAL_PERCEUS" (T.isInfixOf "ORGAN_PROFILE_THREAD_LOCAL_PERCEUS" c11)
      assertBool "Contains ORGAN_PROFILE_FREESTANDING_LINEAR" (T.isInfixOf "ORGAN_PROFILE_FREESTANDING_LINEAR" c11)
      assertBool "Contains IOSQE_IO_LINK comment" (T.isInfixOf "IOSQE_IO_LINK" c11)
      assertBool "Contains reactorVar reference" (T.isInfixOf "&core_reactor" c11)

  , testCase "renderReactorInitC11 initializes reactor with correct capacity" $ do
      let c11 = renderReactorInitC11 "my_reactor" 32
      assertBool "Defines slots array" (T.isInfixOf "organ_flight_slot_t my_reactor_slots[32];" c11)
      assertBool "Calls organ_reactor_init" (T.isInfixOf "organ_reactor_init(&my_reactor, 0, 32" c11)
  ]
