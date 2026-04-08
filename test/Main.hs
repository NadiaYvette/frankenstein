module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck (QuickCheckMaxRatio(..))
import Test.Tasty (adjustOption)

import Frankenstein.Core.Types
import Frankenstein.Core.Perceus (insertPerceus, analyzeUsage, freeVars)
import Frankenstein.Core.Evidence (evidencePass)
import Frankenstein.Core.EffectOpt (effectOptimize, effectOptimizeWithStats, EffectOptStats(..)
  , inlineLocalHandlers, eliminateIdentityHandlers, annotateTailResumptive)
import Frankenstein.Core.FlattenPatterns (flattenPatternsExpr)
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
