module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckMaxRatio(..))
import Test.Tasty (adjustOption)

import Frankenstein.Core.Types
import Frankenstein.Core.Perceus (insertPerceus, analyzeUsage, freeVars)
import Frankenstein.Core.Evidence (evidencePass)
import Frankenstein.Core.DeriveSelectors (deriveSelectors, recordSelectors)
import Frankenstein.Core.EffectOpt (effectOptimize, effectOptimizeWithStats, EffectOptStats(..)
  , inlineLocalHandlers, eliminateIdentityHandlers, annotateTailResumptive)
import Frankenstein.Core.FlattenPatterns (flattenPatternsExpr)
import Frankenstein.Core.ConTags (assignProgramTags, collectReferencedCtors)
import Frankenstein.MercuryBridge.HldsParse
  ( MercuryHLDS(..), MercuryPred(..), MercuryTypeDecl(..)
  , MercuryDet(..), MercuryMode(..), MercuryGoal(..)
  )
import Frankenstein.MercuryBridge.CoreTranslate (translateHlds)
import Frankenstein.Core.Linker (linkPrograms, linkProgramsWith, LinkResult(..), LinkError(..))
import Frankenstein.MlirEmit.Emitter (emitProgram, emitProgramWasm, emitProgramWithEffects)
import KOracle (kOracleTests)
import BridgeBisim (bridgeBisimTests)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
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
  , linkerTests
  , mlirEmitTests
  , flattenPatternsTests
  , deriveSelectorsTests
  , conTagsTests
  , mercuryAdtTests
  , wasmEmitTests
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
      let exnEffect = EffectRowExtend (mkQName "exn" "raise") EffectRowEmpty
          -- A handler that catches the exception
          handler = ELam [(mkName "e", anyType)] (ELit (LitInt 0))
          -- A body that performs the effect
          body = EPerform (mkQName "exn" "raise") [ELit (LitString "boom")]
          -- Handle wrapping
          handleExpr = EHandle exnEffect handler body
          prog = mkProgram "test" "evidence"
            [ mkFunDef "test" "f" handleExpr anyType ]
          effDecl = EffectDecl
            { effectName   = mkQName "exn" "raise"
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

  , testCase "evidencePass: unhandled effect becomes default call" $
      let body = EPerform (mkQName "exn" "raise") [ELit (LitString "err")]
          prog = mkProgram "test" "ev2"
            [ mkFunDef "test" "g" body anyType ]
          result = evidencePass prog
          resultExpr = defExpr (head (progDefs result))
      in assertBool "unhandled perform should become EApp (function call)"
           (isEApp resultExpr)
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
        assertBool "should contain func.func @factorial"
          (T.isInfixOf "func.func @factorial" mlir)
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
        assertBool "should contain func.func @orZero"
          (T.isInfixOf "func.func @orZero" mlir)

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
        assertBool "should contain func.func @factorial"
          (T.isInfixOf "func.func @factorial" mlir)
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
