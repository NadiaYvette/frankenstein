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
  , translateMultiHlds
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
  let moduleCtors =
        [ (hldsModule hlds <> "." <> cname, cname)
        | t <- hldsTypes hlds, (cname, _) <- typeDeclCtors t ]
      userCtorNames = Set.fromList ([q | (q, _) <- moduleCtors]
                                 ++ [b | (_, b) <- moduleCtors])
      knownCtors = Set.union userCtorNames stdlibCtorNames
  defs <- mapM (translatePred knownCtors (hldsModule hlds)) (hldsPreds hlds)
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
      mainIntAliases =
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
      -- Synthesise a no-arg @main@ that calls Mercury's
      -- @main(io::di, io::uo)@ with a dummy 0 IO state and discards the
      -- result.  Mercury's io stdlib calls (e.g. io.write_string) are
      -- routed to the Frankenstein runtime printers, which are
      -- side-effectful regardless of the state-thread argument.
      mainIoAliases =
        [ Def
            { defName = QName "" (Name "main" 0)
            , defType = TFun [] EffectRowEmpty intT
            , defExpr = EApp (EVar (Name "main_io_impl" 0)) [ELit (LitInt 0)]
            , defSort = DefFun
            , defVisibility = Public
            }
        | null mainIntAliases  -- prefer the int form if both somehow declared
        , any (\p -> predName p == "main"
                     && predDet p == Det
                     && length (predModes p) == 2
                     && all (\m -> m == ModeDi || m == ModeUo) (predModes p))
              (hldsPreds hlds)
        ]
      mainAliases = mainIntAliases ++ mainIoAliases
  Right $ Program
    { progName = QName (hldsModule hlds) (Name "main" 0)
    , progDefs = failHandler : defs ++ multiWrappers ++ mainAliases
    , progData = dataDecls
    , progEffects = mercuryEffects
    }

-- | Translate a list of HLDS modules into a single 'Program'.  The first
-- element of the list is treated as the entry module (its name becomes
-- 'progName'; its 'main'/'main_int' triggers the synthesised alias).
-- All other modules contribute defs + data decls but no main aliases.
--
-- The single-module 'mercury_fail' fallback and 'mercury_effects' row are
-- generated once for the merged program.  Pred bodies retain their original
-- 'predName' (already qualified upstream where necessary), so a call to
-- 'rational.numer' in the entry module resolves against the 'rational'
-- module's def by lookup, not by re-mangling.
translateMultiHlds :: [MercuryHLDS] -> Either Text Program
translateMultiHlds [] = Left "translateMultiHlds: empty module list"
translateMultiHlds (entry : rest) = do
  -- Build a global ctor-name set from every module's data decls plus
  -- ctor names that appear in deconstruct contexts in pred bodies.
  -- The determinism-stage HLDS dump often omits `:- type` declarations,
  -- so the body scan is essential: a ctor used in `Var = mod.ctor(args)`
  -- with Var an input arg is unambiguously a real data constructor.
  -- Includes both bare ('r') and qualified ('rational.r') forms so
  -- parseCtorApp's preserved qualifier matches.
  let moduleCtors h =
        [ (hldsModule h <> "." <> cname, cname)
        | t <- hldsTypes h, (cname, _) <- typeDeclCtors t ]
      userCtorPairs = moduleCtors entry ++ concatMap moduleCtors rest
      userCtorNames = Set.fromList ([q | (q, _) <- userCtorPairs]
                                 ++ [b | (_, b) <- userCtorPairs])
      bodyCtorPairs = concatMap collectCtorsFromPred (hldsPreds entry)
                   ++ concatMap (\h -> concatMap collectCtorsFromPred (hldsPreds h)) rest
      bodyCtorNames = Set.fromList ([q | (q, _) <- bodyCtorPairs]
                                 ++ [b | (_, b) <- bodyCtorPairs])
      knownCtors = Set.unions [userCtorNames, bodyCtorNames, stdlibCtorNames]
  -- Translate every module's predicates and data decls.
  entryDefs <- mapM (translatePred knownCtors (hldsModule entry)) (hldsPreds entry)
  restDefsLists <- mapM (\h -> mapM (translatePred knownCtors (hldsModule h)) (hldsPreds h)) rest
  let entryData = map (translateMercuryTypeDecl (hldsModule entry)) (hldsTypes entry)
      restData  = concatMap (\h -> map (translateMercuryTypeDecl (hldsModule h)) (hldsTypes h)) rest
      restDefs  = concat restDefsLists
      -- One fallback handler for the merged program.
      failHandler = Def
        { defName = QName "" (Name "mercury_fail" 0)
        , defType = TFun [] EffectRowEmpty intT
        , defExpr = ELit (LitInt 0)
        , defSort = DefFun
        , defVisibility = Public
        }
      intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
      -- Multi-predicate wrappers per source module (qualified appropriately).
      entryMulti = concatMap (makeMultiWrapper (hldsModule entry)) (hldsPreds entry)
      restMulti  = concatMap (\h -> concatMap (makeMultiWrapper (hldsModule h))
                                              (hldsPreds h)) rest
      -- main aliases only synthesised from the entry module.
      mainIntAliases =
        [ Def
            { defName = QName "" (Name "main" 0)
            , defType = TFun [] EffectRowEmpty intT
            , defExpr = EApp (EVar (Name "main_int" 0)) []
            , defSort = DefFun
            , defVisibility = Public
            }
        | any (\p -> predName p == "main_int" && predDet p == Det)
              (hldsPreds entry)
        ]
      mainIoAliases =
        [ Def
            { defName = QName "" (Name "main" 0)
            , defType = TFun [] EffectRowEmpty intT
            , defExpr = EApp (EVar (Name "main_io_impl" 0)) [ELit (LitInt 0)]
            , defSort = DefFun
            , defVisibility = Public
            }
        | null mainIntAliases
        , any (\p -> predName p == "main"
                     && predDet p == Det
                     && length (predModes p) == 2
                     && all (\m -> m == ModeDi || m == ModeUo) (predModes p))
              (hldsPreds entry)
        ]
      mainAliases = mainIntAliases ++ mainIoAliases
  Right $ Program
    { progName    = QName (hldsModule entry) (Name "main" 0)
    , progDefs    = failHandler : entryDefs ++ restDefs ++ entryMulti ++ restMulti ++ mainAliases
    , progData    = entryData ++ restData
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
                     | (i, fieldTy) <- zip [(0 :: Int)..length fieldTys - 1] fieldTys
                     ]
      , conVis    = Public
      }
    | (ctorName, fieldTys) <- typeDeclCtors td
    ]
  , dataVis    = Public
  }

-- | Translate a single Mercury predicate to a Frankenstein definition
translatePred :: Set Text -> Text -> MercuryPred -> Either Text Def
translatePred knownCtors srcModule pred' = do
  let -- The user's @main(io::di, io::uo)@ predicate is renamed to
      -- @mercury_main_io@ so the synthesised no-arg @main@ alias can
      -- delegate to it without a name collision.
      -- Number of input arguments (mode in/di) — the arity the function
      -- presents at the call site after output-arg-drop.  Used to
      -- disambiguate overloads like @rational.+/1@ (unary) vs
      -- @rational.+/2@ (binary) which would otherwise collapse to the
      -- same MLIR symbol.  Sanitisation preserves underscores so the
      -- @__<n>@ suffix survives through to the linker.
      inputArity = length [m | m <- predModes pred'
                             , m == ModeIn || m == ModeDi]
      effectiveName
        | predName pred' == "main"
        , length (predModes pred') == 2
        , all (\m -> m == ModeDi || m == ModeUo) (predModes pred')
        = "main_io_impl"
        | otherwise = predName pred' <> "__" <> T.pack (show inputArity)
      -- Use the actual source module name rather than a synthetic
      -- "mercury" tag.  The linker resolves cross-module calls like
      -- `rational.cmp` against the bare name `cmp__N` in the symbol
      -- table, preferring the candidate whose home module matches
      -- the dot-split prefix — that only works if defs carry their
      -- real source module.
      name = QName srcModule (Name effectiveName 0)
      -- Separate input and output modes
      pmodes = predModes pred'
      indexedModes = zip [0..length pmodes - 1] pmodes
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
      -- between successive conjuncts.  Wrap with a default binding of
      -- the output var so error-path branches (e.g. require.error in
      -- one ITE arm of rational_norm) still satisfy the terminator's
      -- EVar reference instead of leaking a free-name to the link
      -- stage.  Working branches that actually bind the output via
      -- ELet shadow the default by lexical scope.
      bodyExpr goal = case outputName of
        Just n  -> ELet [[Bind (Name n 0) anyTy (ELit (LitInt 0)) DefVal]]
                         (translateGoalK knownCtors initialEnv goal terminator)
        Nothing -> translateGoalK knownCtors initialEnv goal terminator
      rawGoalBody = case predGoal pred' of
        Just goal -> bodyExpr goal
        Nothing   -> ELit (LitString "no body")

      -- For semidet predicates: wrap in "if test then 1 else perform exn.fail"
      goalBody = case predDet pred' of
        Semidet ->
          -- For semidet with no output var, the CPS terminator is 0 and
          -- the test result gets discarded. Use translateGoalAsTest which
          -- yields the test result directly as the scrutinee.
          let testExpr = case (outputName, predGoal pred') of
                (Nothing, Just goal) -> translateGoalAsTest knownCtors initialEnv goal
                _                    -> rawGoalBody
          in ECase testExpr
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

-- | Translate a Mercury goal as a test expression that yields the
-- boolean result directly (1 = success, 0 = failure). Used for semidet
-- predicates with no output variable, where the goal IS the test.
-- For conjunctions, evaluates each goal and returns the last result.
-- For a single comparison, returns the comparison result.
translateGoalAsTest :: Set Text -> Set Text -> MercuryGoal -> Expr
translateGoalAsTest knownCtors env (GoalCall predName' args)
  | Just op <- T.stripPrefix "int." predName'
  , [lhs, rhs] <- args =
      EApp (EVar (Name op 0)) [argExpr lhs, argExpr rhs]
  | otherwise =
      let isStdlibPrefixed n = any (`T.isPrefixOf` n)
            ["io.", "int.", "integer.", "string.", "list.", "char."
            , "bool.", "require.", "exception.", "math.", "float."
            , "builtin.", "private_builtin."]
          taggedName
            | isStdlibPrefixed predName' = predName'
            | otherwise = predName' <> "__" <> T.pack (show (length args))
      in EApp (EVar (Name taggedName 0))
           (map argExpr args)
translateGoalAsTest knownCtors env (GoalConj goals) = case goals of
  []  -> ELit (LitInt 1)
  [g] -> translateGoalAsTest knownCtors env g
  _   -> -- For multi-goal conjunctions, bind intermediate goals and
         -- return the last. Use CPS for all but the last goal.
         let initGoals = init goals
             lastGoal  = last goals
             envsFor   = scanl extendBindingsFor env goals
             initPairs = zip initGoals envsFor
             lastEnv   = envsFor !! (length goals - 1)
             innerExpr = translateGoalAsTest knownCtors lastEnv lastGoal
         in foldr (\(g, e) acc -> translateGoalK knownCtors e g acc) innerExpr initPairs
translateGoalAsTest knownCtors env (GoalIfThenElse cond then' else') =
  ECase (translateGoalAsTest knownCtors env cond)
    [ Branch (PatLit (LitInt 1)) Nothing (translateGoalAsTest knownCtors env then')
    , Branch (PatWild boolTy)    Nothing (translateGoalAsTest knownCtors env else')
    ]
  where boolTy = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)
translateGoalAsTest knownCtors env goal =
  -- Fallback: use CPS with the result as terminator — this handles
  -- unification and other goal types correctly.
  translateGoalK knownCtors env goal (ELit (LitInt 1))

-- | Translate a Mercury goal to a Frankenstein expression (legacy
-- zero-knowledge entry point). Prefer 'translateGoalK' which threads
-- a binding environment and continuation so that variables flow
-- correctly across conjuncts.
translateGoal :: MercuryGoal -> Expr
translateGoal g = translateGoalK Set.empty Set.empty g (ELit (LitInt 0))

-- | CPS-style goal translation.
--
-- @translateGoalK env g k@ translates @g@ with knowledge of the variables
-- bound on entry (@env@) and a continuation expression @k@ that represents
-- the "rest of the computation" after @g@ succeeds. Bindings introduced by
-- @g@ (construct, deconstruct, unify-with-var, switch arm) scope over @k@.
translateGoalK :: Set Text -> Set Text -> MercuryGoal -> Expr -> Expr

-- Unification. Several cases, depending on which side is a literal and
-- which side is a fresh variable that needs to be bound.
translateGoalK _kctors env (GoalUnify x y) k =
  let lhsLit = readMaybe (T.unpack x) :: Maybe Integer
      rhsLit = readMaybe (T.unpack y) :: Maybe Integer
      lhsStr = parseMercuryStringLit x
      rhsStr = parseMercuryStringLit y
      bindLhs = not (Set.member x env)
      bindRhs = not (Set.member y env)
  in case (lhsLit, rhsLit, lhsStr, rhsStr) of
       -- Both Int literals: no binding; just continue.
       (Just _, Just _, _, _) -> k
       -- X = <int literal>: bind X to the literal if not yet bound.
       (_, Just n, _, _) | bindLhs ->
         ELet [[Bind (Name x 0) intTy (ELit (LitInt n)) DefVal]] k
       -- <int literal> = Y: bind Y to the literal if not yet bound.
       (Just n, _, _, _) | bindRhs ->
         ELet [[Bind (Name y 0) intTy (ELit (LitInt n)) DefVal]] k
       -- X = "string literal": bind X to the LitString.
       (_, _, _, Just s) | bindLhs ->
         ELet [[Bind (Name x 0) stringTy (ELit (LitString s)) DefVal]] k
       -- "string literal" = Y: bind Y to the LitString.
       (_, _, Just s, _) | bindRhs ->
         ELet [[Bind (Name y 0) stringTy (ELit (LitString s)) DefVal]] k
       -- X = Y, one side bound: bind the other as an alias.
       (Nothing, Nothing, Nothing, Nothing) | bindLhs && not bindRhs ->
         ELet [[Bind (Name x 0) intTy (EVar (Name y 0)) DefVal]] k
       (Nothing, Nothing, Nothing, Nothing) | bindRhs && not bindLhs ->
         ELet [[Bind (Name y 0) intTy (EVar (Name x 0)) DefVal]] k
       -- Fallback: emit a stub unify call.  Use argExpr so literal
       -- ints / chars / strings don't leak as bogus free EVar refs
       -- (e.g. `0` sanitised to `_0$0`).
       _ ->
         ELet [[Bind (Name "_" 0) intTy
                  (EApp (EVar (Name "unify" 0))
                        [argExpr x, argExpr y])
                  DefVal]] k

translateGoalK _kctors _env (GoalCall predName' args) k =
  -- Identify the output variable using a "last unbound arg" heuristic.
  -- Mercury HLDS lists every argument of a predicate at the call site,
  -- inputs and outputs alike.  For det predicates, the output is bound
  -- on return — so the unbound last arg names the receive slot, not an
  -- input value.  Passing it to the callee as if it were an input
  -- produces a free reference that the emitter resolves to a top-level
  -- 0-arg call (`@STATE_VARIABLE_IO_8$0()`), surfacing later as an
  -- unresolved symbol at link time.
  let env = _env
      (callInputs, outputBinding) = case args of
        [] -> ([], Nothing)
        _  -> let outName = last args
              in if Set.member outName env
                 then (args, Nothing)
                 else (init args, Just outName)
      -- Same arity-suffix convention as translatePred uses for def
      -- names: append "__<n>" where n is the number of inputs the
      -- callee receives.  Lets the linker disambiguate overloads
      -- like @rational.+/1@ vs @rational.+/2@.  Skip for known
      -- stdlib-prefixed callees whose targets are runtime stubs with
      -- fixed names (no overload disambiguation needed there).
      isStdlibPrefixed n = any (`T.isPrefixOf` n)
        ["io.", "int.", "integer.", "string.", "list.", "char."
        , "bool.", "require.", "exception.", "math.", "float."
        , "builtin.", "private_builtin."]
      taggedName
        | isStdlibPrefixed predName' = predName'
        | otherwise = predName' <> "__" <> T.pack (show (length callInputs))
      callExpr
        | Just op <- stripIntOp predName'
        , [lhs, rhs] <- args =
            EApp (EVar (Name op 0)) [argExpr lhs, argExpr rhs]
        -- Mercury io stdlib calls: route through the Frankenstein
        -- runtime's string printer.  The trailing two args are the
        -- io::di/uo state variables — discarded since the runtime is
        -- effectful but does not thread an IO state token.
        | Just rtName <- ioCallRuntimeName predName' args =
            case args of
              (s:_) -> EApp (EVar (Name rtName 0)) [argExpr s]
              []    -> EApp (EVar (Name rtName 0)) []
        -- Unary integer negation: the bridge's parseMercuryBuiltin
        -- emits `integer.(- X)` as `integer.-` with 1 arg.  The
        -- runtime's @integer_zm@ is binary subtraction; route to a
        -- dedicated unary stub instead so the call is saturated.
        | predName' == "integer.-", [a] <- callInputs =
            EApp (EVar (Name "integer_neg" 0)) [argExpr a]
        -- Unary integer plus: identity in the i64 model.
        | predName' == "integer.+", [a] <- callInputs =
            argExpr a
        | otherwise =
            EApp (EVar (Name taggedName 0))
                 (map argExpr callInputs)
      stripIntOp n = T.stripPrefix "int." n
      -- Mercury io.* predicates that have a direct runtime equivalent.
      -- Returns the Frankenstein runtime name when the call shape matches.
      ioCallRuntimeName n as = case (n, length as) of
        ("io.write_string", 3) -> Just "print_str"
        ("io.print",        3) -> Just "print_str"
        ("io.write",        3) -> Just "print_str"
        ("io.write_line",   3) -> Just "println_str"
        ("io.print_line",   3) -> Just "println_str"
        ("io.nl",           2) -> Just "putStrLn"  -- prints just newline (need empty string + putStrLn)
        _                      -> Nothing
  in case outputBinding of
       Nothing      -> ELet [[Bind (Name "_" 0) intTy callExpr DefVal]] k
       Just outName -> ELet [[Bind (Name outName 0) intTy callExpr DefVal]] k

translateGoalK kctors env (GoalConj goals) k =
  -- foldr: first goal wraps the rest (left-to-right execution order).
  let go (g, envNow) acc = translateGoalK kctors envNow g acc
      envsFor = scanl extendBindingsFor env goals
      pairs = zip goals envsFor
  in foldr go k pairs

translateGoalK kctors env (GoalDisj goals) k = case goals of
  []     -> EPerform (QName "mercury" (Name "fail" 0)) []
  [g]    -> translateGoalK kctors env g k
  (g:gs) -> ECase (EPerform (QName "mercury" (Name "choose" 0)) [])
              [ Branch (PatLit (LitInt 1)) Nothing (translateGoalK kctors env g k)
              , Branch (PatWild boolTy)    Nothing
                       (translateGoalK kctors env (GoalDisj gs) k)
              ]

translateGoalK kctors env (GoalNot goal) k =
  -- Unchanged semantics: wrap negation as a call to a runtime helper.
  ELet [[Bind (Name "_" 0) intTy
           (EApp (EVar (Name "mercury_not" 0))
                 [ELam [] (translateGoalK kctors env goal (ELit (LitInt 0)))])
           DefVal]]
       k

translateGoalK kctors env (GoalIfThenElse cond then' else') k =
  -- The cond must yield the boolean test result as the scrutinee.
  -- translateGoalK threads the terminator (the value to leave in the
  -- continuation), which would discard the actual test result and
  -- always feed `1` back, hard-coding the then-branch.  Use the
  -- test-form translator instead.
  ECase (translateGoalAsTest kctors env cond)
    [ Branch (PatLit (LitInt 1)) Nothing (translateGoalK kctors env then' k)
    , Branch (PatWild boolTy)    Nothing (translateGoalK kctors env else' k)
    ]

translateGoalK kctors env (GoalSwitch var cases) k =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name tag 0)) []) Nothing
             (translateGoalK kctors env body k)
    | (tag, body) <- cases
    ]

-- GoalConstruct: "LHS = ctor(args)".
-- - If LHS is already bound: deconstruct (pattern match against the ctor).
--   The ctor name may be module-qualified ("rational.r") from parseCtorApp;
--   strip the prefix for the pattern's QName.
-- - If LHS is fresh AND ctor is in knownCtors: construct (allocate).
-- - If LHS is fresh AND ctor is NOT in knownCtors: it's actually a user
--   function or stdlib runtime stub disguised as a ctor by parseCtorApp.
--   Route to GoalCall semantics so the linker can find the def.
translateGoalK kctors env (GoalConstruct var ctor args) k
  | Set.member var env =
      -- Deconstruct: match scrutinee against the ctor.  Use the bare
      -- ctor name (post-strip) so the pattern matches the data-decl's
      -- ECon, which is also emitted bare.
      let bareCtor = case T.breakOnEnd "." ctor of
            ("", n) -> n
            (_,  n) -> n
      in ECase (EVar (Name var 0))
        [ Branch (PatCon (QName "" (Name bareCtor 0))
                   [PatVar (Name a 0) anyTy | a <- args])
                 Nothing k
        ]
  | Set.member ctor kctors || Set.member bareCtor kctors =
      -- Real ctor allocation.  ECon name is bare (the data-decl form).
      ELet [[Bind (Name var 0) anyTy
               (EApp (ECon (QName "" (Name bareCtor 0)))
                     (map argExpr args))
               DefVal]] k
  | otherwise =
      -- Not a ctor — must be a function call disguised by parseCtorApp.
      -- Re-emit through the GoalCall path with the LHS appended as the
      -- output arg.
      translateGoalK kctors env (GoalCall ctor (args ++ [var])) k
  where
    bareCtor = case T.breakOnEnd "." ctor of
      ("", n) -> n
      (_,  n) -> n

-- GoalLambda: bind LHS to a closure value.  The body translates as if
-- it were a det/semidet pred's body with the input params already in
-- env and (for the func form) the output var receiving the body's
-- last-bound value.  Returns the LHS bound to an ELam — the emitter's
-- lambda-lift pass already heap-allocates this as a closure with the
-- captured outer-scope variables in fields 1..n.
translateGoalK kctors env (GoalLambda lhs params mOut body) k =
  let -- Inputs are bound on entry to the lambda body.
      bodyInitialEnv = Set.union env (Set.fromList params)
      bodyTerminator = case mOut of
        Just o  -> EVar (Name o 0)
        Nothing -> ELit (LitInt 1)
      -- For func-form lambdas, wrap the body in an outer ELet
      -- defaulting the output to 0 so error-path branches still
      -- satisfy the terminator's reference (same trick as
      -- translatePred uses for whole-pred bodies).
      bodyExpr = case mOut of
        Just o  ->
          ELet [[Bind (Name o 0) anyTy (ELit (LitInt 0)) DefVal]]
               (translateGoalK kctors bodyInitialEnv body bodyTerminator)
        Nothing ->
          translateGoalK kctors bodyInitialEnv body bodyTerminator
      lamParams = [(Name p 0, anyTy) | p <- params]
      lamExpr   = ELam lamParams bodyExpr
  in ELet [[Bind (Name lhs 0) anyTy lamExpr DefVal]] k

translateGoalK _kctors _env (GoalDeconstruct var ctor args) k =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name ctor 0))
               [PatVar (Name a 0) anyTy | a <- args])
             Nothing k
    ]

translateGoalK _kctors _env (GoalForeign body) k =
  ELet [[Bind (Name "_" 0) intTy
           (EApp (EVar (Name "foreign" 0)) [ELit (LitString body)])
           DefVal]] k

translateGoalK _kctors _env (GoalUnparsed text) k =
  ELet [[Bind (Name "_" 0) intTy
           (EApp (EVar (Name "unparsed_goal" 0)) [ELit (LitString text)])
           DefVal]] k

-- | Compute the set of variables bound by a goal, for updating the
-- environment as we walk a conjunction left-to-right.
extendBindingsFor :: Set Text -> MercuryGoal -> Set Text
extendBindingsFor env g = case g of
  GoalUnify x y ->
    let lhsLit = isJust (readMaybe (T.unpack x) :: Maybe Integer)
                   || isJust (parseMercuryStringLit x)
        rhsLit = isJust (readMaybe (T.unpack y) :: Maybe Integer)
                   || isJust (parseMercuryStringLit y)
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
  GoalLambda var _ _ _ -> Set.insert var env
  _                -> env
  where
    isJust (Just _) = True
    isJust Nothing  = False

-- | Find ctor names used in deconstruct contexts within a pred's
-- body.  A deconstruct is a GoalConstruct where the LHS variable is
-- an input arg of the pred (already bound on entry).  Such usages
-- can only be data constructors (not function calls).  Returns
-- (qualified, bare) pairs so the matcher catches either spelling.
collectCtorsFromPred :: MercuryPred -> [(Text, Text)]
collectCtorsFromPred pred' =
  let -- Input args are everything bound on entry: clause-head names
      -- whose mode is in or di.
      indexed   = zip [0::Int ..] (predModes pred')
      inputIxs  = [i | (i, m) <- indexed, m == ModeIn || m == ModeDi]
      args      = predArgNames pred'
      inputArgs = Set.fromList [args !! i | i <- inputIxs, i < length args]
      go env g = case g of
        GoalConstruct v ctor _
          | Set.member v env -> [splitCtor ctor]
          | otherwise        -> []
        GoalConj gs ->
          let envs = scanl extendBindingsFor env gs
          in concat (zipWith go envs gs)
        GoalDisj gs            -> concatMap (go env) gs
        GoalIfThenElse c t e   -> go env c ++ go env t ++ go env e
        GoalNot g'             -> go env g'
        GoalSwitch _ cases     -> concatMap (\(_, b) -> go env b) cases
        GoalLambda _ params _ b ->
          go (Set.union env (Set.fromList params)) b
        _                      -> []
      splitCtor c = case T.breakOnEnd "." c of
        ("", n) -> (c, n)         -- bare ctor (no module)
        (_,  n) -> (c, n)         -- qualified — keep full + bare name
  in case predGoal pred' of
       Just goal -> go inputArgs goal
       Nothing   -> []

-- | Mercury stdlib ctors the bridge must treat as real data
-- constructors rather than function calls, even though parseCtorApp
-- can't tell from the syntax alone.  Includes both bare and
-- module-qualified forms so the disambiguation in
-- @translateGoalK GoalConstruct@ catches either spelling.
stdlibCtorNames :: Set Text
stdlibCtorNames = Set.fromList
  [ "s", "i", "f", "c"
  , "string.s", "string.i", "string.f", "string.c"
  , "type_ctor_info", "private_builtin.type_info"
  , "list_Nil", "list_Cons"            -- emitted by parseListLiteral
                                       -- (capitalised so the bridge hits
                                       -- kkConsTag/kkNilTag fast path)
  , "list.[]"
  , "tuple"                            -- emitted by parseTupleLiteral
                                       -- for Mercury's @{A, B, C}@ form
  -- builtin comparison_result tags (parseQualifiedOp emits these)
  , "builtin.=", "builtin.<", "builtin.>"
  , "=", "<", ">"
  -- Bridge-synthesised placeholder for unimplemented higher-order
  -- features (lambdas, HO calls).  Treated as a 0-arg ctor so the
  -- LHS gets a let-binding instead of fusing into an unify-stub
  -- meganame.
  , "lambda_placeholder"
  -- Common Mercury stdlib 0-arg atoms (enumeration ctors).
  , "bool.yes", "bool.no", "yes", "no"
  ]

-- | Lift a Mercury HLDS atom into the right Core expression: int
-- literal → ELit (LitInt); double-quoted string → ELit (LitString);
-- single-quoted char → ELit (LitInt codepoint); anything else falls
-- back to an EVar reference (which resolves against the binding env
-- or escapes as a free name downstream).
argExpr :: Text -> Expr
argExpr a =
  case readMaybe (T.unpack a) :: Maybe Integer of
    Just n  -> ELit (LitInt n)
    Nothing -> case parseMercuryStringLit a of
      Just s  -> ELit (LitString s)
      Nothing -> case parseMercuryCharLit a of
        Just cp -> ELit (LitInt cp)
        Nothing -> EVar (Name a 0)

-- | Recognise Mercury's @'c'@ char literal form (also accepts the
-- parenthesised @('c')@ form HLDS prints) and return the codepoint
-- as an Integer.  Used by 'argExpr' so a char arg like @'a'@
-- doesn't leak as a free EVar reference (sanitised to @_a__$0@ at
-- link time).
parseMercuryCharLit :: Text -> Maybe Integer
parseMercuryCharLit raw =
  let t = let s = T.strip raw
          in case T.uncons s of
               Just ('(', inner) -> case T.unsnoc inner of
                 Just (i, ')') -> T.strip i
                 _ -> s
               _ -> s
  in case T.uncons t of
    Just ('\'', rest) -> case T.unsnoc rest of
      Just (inner, '\'') -> case T.uncons inner of
        Just (c, rest')
          | T.null rest' -> Just (toInteger (fromEnum c))
        _ | T.length inner == 2
          , Just ('\\', after) <- T.uncons inner
          , Just (esc, _) <- T.uncons after
          -> case esc of
               'n' -> Just 10
               't' -> Just 9
               'r' -> Just 13
               '\\' -> Just 92
               '\'' -> Just 39
               _   -> Just (toInteger (fromEnum esc))
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

-- | Common type shortcuts used by the translator.
intTy :: Type
intTy = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

boolTy :: Type
boolTy = TCon (TypeCon (QName "std" (Name "bool" 0)) KindValue)

anyTy :: Type
anyTy = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)

stringTy :: Type
stringTy = TCon (TypeCon (QName "std" (Name "string" 0)) KindValue)

-- | Recognise a Mercury HLDS string literal (surrounded by double
-- quotes).  Returns the unquoted content if so.  Used to translate
-- @V_n = "literal"@ unifications into LitString bindings rather than
-- falling through to the unify-stub fallback.
parseMercuryStringLit :: Text -> Maybe Text
parseMercuryStringLit t = case T.uncons t of
  Just ('"', rest) -> case T.unsnoc rest of
    Just (inside, '"') -> Just (unescape inside)
    _                  -> Nothing
  _ -> Nothing
  where
    -- Mercury string literals use Haskell-style escapes (\n, \t, \", \\).
    -- The HLDS emitter prints them verbatim, so undo the escapes here.
    unescape = T.pack . go . T.unpack
    go []             = []
    go ('\\':'n':xs)  = '\n' : go xs
    go ('\\':'t':xs)  = '\t' : go xs
    go ('\\':'r':xs)  = '\r' : go xs
    go ('\\':'"':xs)  = '"'  : go xs
    go ('\\':'\\':xs) = '\\' : go xs
    go ('\\':'0':xs)  = '\0' : go xs
    go (c:xs)         = c    : go xs
