module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Frankenstein.Core.Types
import Frankenstein.Core.Perceus (insertPerceus, analyzeUsage, freeVars)
import Frankenstein.Core.Evidence (evidencePass)
import Frankenstein.Core.Linker (linkPrograms, linkProgramsWith, LinkResult(..), LinkError(..))
import Frankenstein.MlirEmit.Emitter (emitProgram)

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
  , linkerTests
  , mlirEmitTests
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

  , testCase "insertPerceus: does not drop linear let-binding" $
      -- Linear bindings (from TFun type) should NOT get drops even if unused
      -- because linear values must be used exactly once (a type error, not a drop).
      let linearType = TFun [(Linear, intType)] EffectRowEmpty intType
          bind = Bind (mkName "x") linearType (ELit (LitInt 42)) DefVal
          body = ELit (LitInt 0)  -- does not reference "x"
          prog = mkProgram "test" "perceus"
            [ mkFunDef "test" "f"
                (ELet [[bind]] body)
                anyType
            ]
          result = insertPerceus prog
          resultExpr = defExpr (head (progDefs result))
      in assertBool "linear let-binding should NOT get a drop"
           (not (containsDrop (mkName "x") resultExpr))

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
-- D. Linker tests
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
