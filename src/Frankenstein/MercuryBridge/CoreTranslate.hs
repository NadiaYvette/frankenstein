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
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Read (readMaybe)

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
      -- Generate _all wrappers for multi predicates
      multiWrappers = concatMap (makeMultiWrapper (hldsModule hlds)) (hldsPreds hlds)
      -- Synthesise a Frankenstein @main@ if the source declared a
      -- @:- pred main_int(int::out) is det.@ convention. This lets
      -- Mercury programs without the standard @io@-threaded main serve
      -- as native entry points that return an Int result.
      mainAliases =
        [ Def
            { defName = QName "" (Name "main" 0)
            , defType = TFun [] EffectRowEmpty intT
            , defExpr = EApp (EVar (Name "main_int" 0)) []
            , defSort = DefFun
            , defVisibility = Public
            }
        | any (\p -> predName p == "main_int" && predDet p == Det)
              (hldsPreds hlds)
        ]
  Right $ Program
    { progName = QName (hldsModule hlds) (Name "main" 0)
    , progDefs = failHandler : defs ++ multiWrappers ++ mainAliases
    , progData = dataDecls
    , progEffects = mercuryEffects
    }

-- | Generate _all wrapper for multi predicates.
-- For a multi predicate @pick@, generates @pick_all@ which calls
-- @mercury_collect_choices@ with a function pointer to @pick@.
-- This enumerates all choice-effect solutions and returns their sum.
makeMultiWrapper :: Text -> MercuryPred -> [Def]
makeMultiWrapper _modName pred'
  | predDet pred' `elem` [Multi, Nondet, CCMulti, CCNondet] =
      let rawName = predName pred'
          -- The raw predicate's mangled name after evidence pass
          mangledName = "mercury_" <> rawName
          wrapperName = rawName <> "_all"
          intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
      in [ Def
             { defName = QName "" (Name wrapperName 0)
             , defType = TFun [] EffectRowEmpty intT
             , defExpr = EApp (EVar (Name "mercury_collect_choices" 0))
                              [EFunRef (QName "mercury" (Name rawName 0))]
             , defSort = DefFun
             , defVisibility = Public
             }
         ]
  | otherwise = []

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

      -- The output variable is the terminator of the CPS-translated body.
      -- For det predicates with at least one out mode, the body evaluates
      -- to the value bound to that output variable. Pure predicates with
      -- no outputs (rare) fall back to 0.
      outputModes = [i | (i, m) <- indexedModes, m == ModeOut || m == ModeUo]
      outputName  = case [predArgNames pred' !! i
                         | i <- outputModes, i < length (predArgNames pred')] of
                      (n:_) -> Just n
                      []    -> Nothing
      terminator  = case outputName of
        Just n  -> EVar (Name n 0)
        Nothing -> ELit (LitInt 0)
      -- Initial binding environment: input parameters are bound on entry.
      -- Output variables are NOT in the initial env — they get bound as
      -- the goal executes, which is what lets construct goals recognise
      -- "LHS is fresh" → ECon allocation.
      initialEnv  = Set.fromList (map (\n -> n) inputArgNames)

      -- Translate the goal body via CPS so variables flow correctly
      -- between successive conjuncts.
      rawGoalBody = case predGoal pred' of
        Just goal -> translateGoalK initialEnv goal terminator
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

-- | Translate a Mercury goal to a Frankenstein expression (legacy
-- zero-knowledge entry point). Prefer 'translateGoalK' which threads
-- a binding environment and continuation so that variables flow
-- correctly across conjuncts.
translateGoal :: MercuryGoal -> Expr
translateGoal g = translateGoalK Set.empty g (ELit (LitInt 0))

-- | CPS-style goal translation.
--
-- @translateGoalK env g k@ translates @g@ with knowledge of the variables
-- bound on entry (@env@) and a continuation expression @k@ that represents
-- the "rest of the computation" after @g@ succeeds. Bindings introduced by
-- @g@ (construct, deconstruct, unify-with-var, switch arm) scope over @k@.
translateGoalK :: Set Text -> MercuryGoal -> Expr -> Expr

-- Unification. Four cases, depending on which side is a literal and which
-- side is a fresh variable that needs to be bound.
translateGoalK env (GoalUnify x y) k =
  let lhsLit = readMaybe (T.unpack x) :: Maybe Integer
      rhsLit = readMaybe (T.unpack y) :: Maybe Integer
      bindLhs = not (Set.member x env)
      bindRhs = not (Set.member y env)
  in case (lhsLit, rhsLit) of
       -- Both literals: no binding; just continue.
       (Just _, Just _) -> k
       -- X = <literal>: bind X to the literal if not yet bound.
       (_, Just n) | bindLhs ->
         ELet [[Bind (Name x 0) intTy (ELit (LitInt n)) DefVal]] k
       -- <literal> = Y: bind Y to the literal if not yet bound.
       (Just n, _) | bindRhs ->
         ELet [[Bind (Name y 0) intTy (ELit (LitInt n)) DefVal]] k
       -- X = Y, one side bound: bind the other as an alias.
       (Nothing, Nothing) | bindLhs && not bindRhs ->
         ELet [[Bind (Name x 0) intTy (EVar (Name y 0)) DefVal]] k
       (Nothing, Nothing) | bindRhs && not bindLhs ->
         ELet [[Bind (Name y 0) intTy (EVar (Name x 0)) DefVal]] k
       -- Fallback: emit a stub unify call.
       _ ->
         ELet [[Bind (Name "_" 0) intTy
                  (EApp (EVar (Name "unify" 0))
                        [EVar (Name x 0), EVar (Name y 0)])
                  DefVal]] k

translateGoalK _env (GoalCall predName' args) k =
  let callExpr
        | Just op <- stripIntOp predName'
        , [lhs, rhs] <- args =
            EApp (EVar (Name op 0)) [EVar (Name lhs 0), EVar (Name rhs 0)]
        | otherwise =
            EApp (EVar (Name predName' 0))
                 (map (\a -> EVar (Name a 0)) args)
      stripIntOp n = T.stripPrefix "int." n
      -- If the call has a plausible output argument (last arg), bind it.
      -- This is a heuristic: Mercury HLDS dumps list output vars in the
      -- argument list, and for det predicates the last position is
      -- typically the returned value. Callers that use the result in a
      -- later goal will already have the variable name bound via this.
  in case args of
       [] -> ELet [[Bind (Name "_" 0) intTy callExpr DefVal]] k
       _  -> let outName = last args
             in if Set.member outName _env
                then ELet [[Bind (Name "_" 0) intTy callExpr DefVal]] k
                else ELet [[Bind (Name outName 0) intTy callExpr DefVal]] k

translateGoalK env (GoalConj goals) k =
  -- foldr: first goal wraps the rest (left-to-right execution order).
  let go (g, envNow) acc = translateGoalK envNow g acc
      envsFor = scanl extendBindingsFor env goals
      pairs = zip goals envsFor
  in foldr go k pairs

translateGoalK env (GoalDisj goals) k = case goals of
  []     -> EPerform (QName "mercury" (Name "fail" 0)) []
  [g]    -> translateGoalK env g k
  (g:gs) -> ECase (EPerform (QName "mercury" (Name "choose" 0)) [])
              [ Branch (PatLit (LitInt 1)) Nothing (translateGoalK env g k)
              , Branch (PatWild boolTy)    Nothing
                       (translateGoalK env (GoalDisj gs) k)
              ]

translateGoalK env (GoalNot goal) k =
  -- Unchanged semantics: wrap negation as a call to a runtime helper.
  ELet [[Bind (Name "_" 0) intTy
           (EApp (EVar (Name "mercury_not" 0))
                 [ELam [] (translateGoalK env goal (ELit (LitInt 0)))])
           DefVal]]
       k

translateGoalK env (GoalIfThenElse cond then' else') k =
  ECase (translateGoalK env cond (ELit (LitInt 1)))
    [ Branch (PatLit (LitInt 1)) Nothing (translateGoalK env then' k)
    , Branch (PatWild boolTy)    Nothing (translateGoalK env else' k)
    ]

translateGoalK env (GoalSwitch var cases) k =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name tag 0)) []) Nothing
             (translateGoalK env body k)
    | (tag, body) <- cases
    ]

-- GoalConstruct: "LHS = ctor(args)". If LHS is already bound we treat it
-- as a deconstruct (pattern match); otherwise it's a construct (allocate).
translateGoalK env (GoalConstruct var ctor args) k
  | Set.member var env =
      -- Deconstruct: match scrutinee against the ctor, bind fresh arg vars.
      ECase (EVar (Name var 0))
        [ Branch (PatCon (QName "" (Name ctor 0))
                   [PatVar (Name a 0) anyTy | a <- args])
                 Nothing k
        ]
  | otherwise =
      -- Construct: allocate the ctor and let-bind LHS.
      ELet [[Bind (Name var 0) anyTy
               (EApp (ECon (QName "" (Name ctor 0)))
                     [EVar (Name a 0) | a <- args])
               DefVal]] k

translateGoalK _env (GoalDeconstruct var ctor args) k =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name ctor 0))
               [PatVar (Name a 0) anyTy | a <- args])
             Nothing k
    ]

translateGoalK _env (GoalForeign body) k =
  ELet [[Bind (Name "_" 0) intTy
           (EApp (EVar (Name "foreign" 0)) [ELit (LitString body)])
           DefVal]] k

translateGoalK _env (GoalUnparsed text) k =
  ELet [[Bind (Name "_" 0) intTy
           (EApp (EVar (Name "unparsed_goal" 0)) [ELit (LitString text)])
           DefVal]] k

-- | Compute the set of variables bound by a goal, for updating the
-- environment as we walk a conjunction left-to-right.
extendBindingsFor :: Set Text -> MercuryGoal -> Set Text
extendBindingsFor env g = case g of
  GoalUnify x y ->
    let lhsLit = isJust (readMaybe (T.unpack x) :: Maybe Integer)
        rhsLit = isJust (readMaybe (T.unpack y) :: Maybe Integer)
    in (if not lhsLit then Set.insert x env else env)
       `Set.union`
       (if not rhsLit then Set.insert y env else env)
  GoalConstruct var _ args ->
    -- Whether it's construct or deconstruct, both directions bind something.
    Set.insert var (foldr Set.insert env args)
  GoalDeconstruct var _ args ->
    Set.insert var (foldr Set.insert env args)
  GoalSwitch var _ -> Set.insert var env
  GoalCall _ args  -> foldr Set.insert env args
  GoalConj gs      -> foldl extendBindingsFor env gs
  _                -> env
  where
    isJust (Just _) = True
    isJust Nothing  = False

-- | Common type shortcuts used by the translator.
intTy :: Type
intTy = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

boolTy :: Type
boolTy = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)

anyTy :: Type
anyTy = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)
