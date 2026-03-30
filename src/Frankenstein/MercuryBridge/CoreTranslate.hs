-- | Mercury HLDS -> Frankenstein Core Translation
--
-- Translates Mercury's post-mode-check HLDS into Frankenstein Core.
-- Key mappings:
--
-- Determinism -> Effects:
--   det      -> pure (no effect)
--   semidet  -> exn effect (can fail)
--   multi    -> choice effect (multiple solutions)
--   nondet   -> exn + choice effects
--
-- Modes -> Argument positions:
--   in       -> normal function argument
--   out      -> part of return value (tupled if multiple outs)
--   di       -> linear argument (consumed, Multiplicity = Linear)
--   uo       -> linear return (unique output)
--
-- Goals -> Expressions:
--   conjunction  -> let-sequencing
--   disjunction  -> choice effect (perform choose)
--   unification  -> pattern match or constructor application
--   call         -> function application
--   if-then-else -> case expression
--   switch       -> case expression
--   construct    -> constructor application
--   deconstruct  -> pattern match

module Frankenstein.MercuryBridge.CoreTranslate
  ( translateHlds
  , translatePred
  ) where

import Frankenstein.Core.Types
import Frankenstein.MercuryBridge.HldsParse

import Data.Text (Text)
import qualified Data.Text as T

-- | Translate a full Mercury HLDS module to Frankenstein Core
translateHlds :: MercuryHLDS -> Either Text Program
translateHlds hlds = do
  defs <- mapM translatePred (hldsPreds hlds)
  let dataDecls = map (translateMercuryTypeDecl (hldsModule hlds)) (hldsTypes hlds)
      -- Default handler for exn.fail: returns 0
      -- Named "mercury_fail" with empty module to match the evidence pass's
      -- unhandled effect naming convention (effName <> "_" <> opN).
      failHandler = Def
        { defName = QName "" (Name "mercury_fail" 0)
        , defType = TFun [] EffectRowEmpty intT
        , defExpr = ELit (LitInt 0)
        , defSort = DefFun
        , defVisibility = Public
        }
      intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
  Right $ Program
    { progName = QName (hldsModule hlds) (Name "main" 0)
    , progDefs = failHandler : defs
    , progData = dataDecls
    , progEffects = mercuryEffects
    }

-- | Built-in effect declarations for Mercury's determinism system
mercuryEffects :: [EffectDecl]
mercuryEffects =
  [ EffectDecl
      { effectName = QName "mercury" (Name "exn" 0)
      , effectParams = []
      , effectOps = [OpDecl (QName "mercury" (Name "fail" 0))
                            (TFun [] EffectRowEmpty (TCon (TypeCon (QName "std" (Name "void" 0)) KindValue)))]
      }
  , EffectDecl
      { effectName = QName "mercury" (Name "choice" 0)
      , effectParams = []
      , effectOps = [OpDecl (QName "mercury" (Name "choose" 0))
                            (TFun [] EffectRowEmpty (TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)))]
      }
  ]

-- | Translate a Mercury HLDS type declaration to a Frankenstein DataDecl.
translateMercuryTypeDecl :: Text -> MercuryTypeDecl -> DataDecl
translateMercuryTypeDecl modName td = DataDecl
  { dataName   = QName modName (Name (typeDeclName td) 0)
  , dataParams = [ TypeVar (Name p 0) KindStar Many | p <- typeDeclParams td ]
  , dataCons   = [ ConDecl
      { conName   = QName modName (Name ctorName 0)
      , conFields = [ (Name ("field_" <> T.pack (show i)) 0,
                       TCon (TypeCon (QName "std" (Name fieldTy 0)) KindValue))
                     | (i, fieldTy) <- zip [(0 :: Int)..] fieldTys
                     ]
      , conVis    = Public
      }
    | (ctorName, fieldTys) <- typeDeclCtors td
    ]
  , dataVis    = Public
  }

-- | Translate a single Mercury predicate to a Frankenstein definition
translatePred :: MercuryPred -> Either Text Def
translatePred pred' = do
  let name = QName "mercury" (Name (predName pred') 0)
      -- Separate input and output modes
      indexedModes = zip [0..] (predModes pred')
      inputModes  = [i | (i, m) <- indexedModes, m == ModeIn || m == ModeDi]
      _outputModes = [i | (i, m) <- indexedModes, m == ModeOut || m == ModeUo]

      -- Build argument types with multiplicity
      argTypes = [(modeToMult m, TCon intType)
                 | m <- predModes pred', m == ModeIn || m == ModeDi]

      -- Build effect row from determinism
      effRow = detToEffectRow (predDet pred')

      -- Return type
      retType = TCon intType

      funType = TFun argTypes effRow retType

      -- Build parameter list from argument names
      inputArgNames = [predArgNames pred' !! i | i <- inputModes, i < length (predArgNames pred')]
      params = [(Name argN 0, TCon intType)
               | argN <- inputArgNames]

      -- Translate the goal body
      rawGoalBody = case predGoal pred' of
        Just goal -> translateGoal goal
        Nothing   -> ELit (LitString "no body")

      -- For semidet predicates: wrap in "if test then 1 else perform exn.fail"
      goalBody = case predDet pred' of
        Semidet ->
          ECase rawGoalBody
            [ Branch (PatLit (LitInt 1)) Nothing (ELit (LitInt 1))
            , Branch (PatWild boolType) Nothing
                (EPerform (QName "mercury" (Name "fail" 0)) [])
            ]
        _ -> rawGoalBody
      boolType = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)

      body = if null params then goalBody
             else ELam params goalBody

  Right $ Def
    { defName = name
    , defType = funType
    , defExpr = body
    , defSort = DefFun
    , defVisibility = Public
    }
  where
    intType = TypeCon (QName "std" (Name "int" 0)) KindValue

-- | Convert Mercury mode to multiplicity
modeToMult :: MercuryMode -> Multiplicity
modeToMult ModeIn     = Many
modeToMult ModeOut    = Many
modeToMult ModeDi     = Linear   -- destructive input: must be consumed
modeToMult ModeUo     = Linear   -- unique output: exclusively owned
modeToMult ModeUnused = Many

-- | Convert Mercury determinism to an effect row
detToEffectRow :: MercuryDet -> EffectRow
detToEffectRow Det       = EffectRowEmpty  -- pure
detToEffectRow Semidet   = EffectRowExtend (QName "mercury" (Name "exn" 0)) EffectRowEmpty
detToEffectRow Multi     = EffectRowExtend (QName "mercury" (Name "choice" 0)) EffectRowEmpty
detToEffectRow Nondet    = EffectRowExtend (QName "mercury" (Name "exn" 0))
                             (EffectRowExtend (QName "mercury" (Name "choice" 0)) EffectRowEmpty)
detToEffectRow Failure   = EffectRowExtend (QName "mercury" (Name "exn" 0)) EffectRowEmpty
detToEffectRow Erroneous = EffectRowExtend (QName "mercury" (Name "exn" 0)) EffectRowEmpty
detToEffectRow CCMulti   = EffectRowExtend (QName "mercury" (Name "choice" 0)) EffectRowEmpty
detToEffectRow CCNondet  = EffectRowExtend (QName "mercury" (Name "exn" 0))
                             (EffectRowExtend (QName "mercury" (Name "choice" 0)) EffectRowEmpty)

-- | Translate a Mercury goal to a Frankenstein expression
translateGoal :: MercuryGoal -> Expr
translateGoal (GoalUnify x y) =
  -- Unification becomes equality check or assignment depending on mode
  -- For now: just emit as a call to unify
  EApp (EVar (Name "unify" 0)) [EVar (Name x 0), EVar (Name y 0)]

translateGoal (GoalCall predName' args)
  -- Mercury builtin comparison: "int.>" etc. → comparison that returns bool
  -- For semidet, the caller wraps this so failure performs exn.fail
  | Just op <- stripIntOp predName'
  , [lhs, rhs] <- args
  = EApp (EVar (Name op 0)) [EVar (Name lhs 0), EVar (Name rhs 0)]
  -- Mercury builtin arithmetic: "int.+" etc. → arithmetic
  | otherwise
  = EApp (EVar (Name predName' 0)) (map (\a -> EVar (Name a 0)) args)
  where
    stripIntOp n = case T.stripPrefix "int." n of
      Just op -> Just op
      Nothing -> Nothing

translateGoal (GoalConj goals) =
  -- Conjunction: sequence goals with let-bindings
  foldr (\g rest -> ELet [[Bind (Name "_" 0) unitType (translateGoal g) DefVal]] rest)
        (ELit (LitInt 0))  -- unit value
        goals
  where unitType = TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue)

translateGoal (GoalDisj goals) =
  -- Disjunction: perform choice effect between alternatives
  case goals of
    []     -> EPerform (QName "mercury" (Name "fail" 0)) []
    [g]    -> translateGoal g
    (g:gs) -> ECase (EPerform (QName "mercury" (Name "choose" 0)) [])
                [ Branch (PatLit (LitInt 1)) Nothing (translateGoal g)
                , Branch (PatWild boolType) Nothing (translateGoal (GoalDisj gs))
                ]
  where boolType = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)

translateGoal (GoalNot goal) =
  -- not(Goal) = try Goal, if succeeds then fail, if fails then succeed
  -- Implemented via effect handling
  EApp (EVar (Name "mercury_not" 0)) [ELam [] (translateGoal goal)]

translateGoal (GoalIfThenElse cond then' else') =
  ECase (translateGoal cond)
    [ Branch (PatLit (LitInt 1)) Nothing (translateGoal then')
    , Branch (PatWild boolType) Nothing (translateGoal else')
    ]
  where boolType = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)

translateGoal (GoalSwitch var cases) =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name tag 0)) []) Nothing (translateGoal body)
    | (tag, body) <- cases
    ]

translateGoal (GoalConstruct var ctor args) =
  ELet [[Bind (Name var 0)
              (TCon (TypeCon (QName "std" (Name "any" 0)) KindValue))
              (EApp (ECon (QName "" (Name ctor 0))) (map (\a -> EVar (Name a 0)) args))
              DefVal]]
       (ELit (LitInt 0))

translateGoal (GoalDeconstruct var ctor args) =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name ctor 0)) (map (\a -> PatVar (Name a 0) anyType) args))
             Nothing
             (ELit (LitInt 0))
    ]
  where anyType = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)

translateGoal (GoalForeign body) =
  EApp (EVar (Name "foreign" 0)) [ELit (LitString body)]

translateGoal (GoalUnparsed text) =
  EApp (EVar (Name "unparsed_goal" 0)) [ELit (LitString text)]
