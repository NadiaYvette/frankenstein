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
import Control.Applicative ((<|>))
import Data.Maybe (isJust)

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
makeMultiWrapper modName pred'
  | predDet pred' `elem` [Multi, Nondet, CCMulti, CCNondet] =
      let rawName    = predName pred'
          -- Match translatePred's effectiveName convention: append
          -- "__<inputArity>" to non-main pred names so the EFunRef
          -- resolves against the actual emitted def symbol.
          inputArity = length [m | m <- predModes pred', m == ModeIn || m == ModeDi]
          effectiveName
            | rawName == "main" = rawName
            | otherwise         = rawName <> "__" <> T.pack (show inputArity)
          wrapperName = rawName <> "_all"
          intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
      in [ Def
             { defName = QName "" (Name wrapperName 0)
             , defType = TFun [] EffectRowEmpty intT
             , defExpr = EApp (EVar (Name "mercury_collect_choices" 0))
                              [EFunRef (QName modName (Name effectiveName 0))]
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

      -- For semidet predicates: behaviour depends on whether the pred has
      -- an output variable.
      --
      --   * No output (pure test, e.g. @is_zero/1@): wrap the test result
      --     as @case test of 1 -> 1 ; _ -> fail@.
      --
      --   * With output (e.g. @lead_coeff(P, LC)@): just return the raw
      --     body — the body's CPS terminator IS the output value.  The
      --     CALLER (translateGoalAsTest GoalCall) is responsible for
      --     treating @output == 0@ as failure and any non-zero value as
      --     success.  Doing the success/fail tag here would either throw
      --     away the output (returning 1) or compare the output to
      --     literal 1 (always false for heap-pointer outputs).
      goalBody = case predDet pred' of
        Semidet ->
          case (outputName, predGoal pred') of
            (Nothing, Just goal) ->
              let testExpr = translateGoalAsTest knownCtors initialEnv goal
              in ECase testExpr
                   [ Branch (PatLit (LitInt 1)) Nothing (ELit (LitInt 1))
                   , Branch (PatWild boolType) Nothing
                       (EPerform (QName "mercury" (Name "fail" 0)) [])
                   ]
            _ -> rawGoalBody
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
    -- Use a Mercury-specific value type (NOT std.int) so the Perceus
    -- pass treats Mercury values as boxed/heap-allocated and inserts
    -- proper retain/drop pairs.  The bridge's pred args are typed
    -- @mercury.value@ — same i64 width at the MLIR level but
    -- triggers refcount management for multi-use args (e.g. an EI
    -- struct used by multiple ei_a/ei_b/ei_c deconstructs).
    intType = TypeCon (QName "mercury" (Name "value" 0)) KindValue

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
  | Just (newName, newArgs) <- rewriteTypeclassMethod predName' args =
      translateGoalAsTest knownCtors env (GoalCall newName newArgs)
translateGoalAsTest knownCtors env (GoalCall predName' args)
  -- If the call has an UNBOUND last arg, it's an output binding (Mercury
  -- HLDS lists every formal parameter, including outputs).  Reuse
  -- translateGoalK's output-binding heuristic so the output gets bound
  -- via `let outName = call(inputs...)` BEFORE returning the test
  -- result; otherwise the output appears as a free-EVar reference in
  -- the cond and leaks as an unresolved extern at link time.  For
  -- semidet predicates in a cond position the call's result IS the
  -- test, so terminate the CPS with the bound name (or @1@ when all
  -- args are already bound).
  | not (null args)
  , let outName = last args
  , not (Set.member outName env)
  , isJustVar outName =
      -- Semidet pred in test position with an unbound output.  The HLDS
      -- caller expects the if-cond to fire when the predicate succeeds
      -- (= output bound to a non-zero / non-failure value).  Previously
      -- the bridge yielded the raw output value as the test result; that
      -- only matched literal 1 inside the surrounding ECase, so any
      -- heap-pointer or non-1 numeric output sent the cond to its else
      -- branch (surd's @det_lead_coeff@ then aborted, or worse, the
      -- caller's then-branch re-ran with a stale/wrong output).  Map any
      -- non-zero output to 1 via an ECase so the success path matches.
      let testExpr = ECase (EVar (Name outName 0))
            [ Branch (PatLit (LitInt 0)) Nothing (ELit (LitInt 0))
            , Branch (PatWild anyTy)     Nothing (ELit (LitInt 1))
            ]
      in translateGoalK knownCtors env (GoalCall predName' args) testExpr
  | otherwise =
      let isStdlibPrefixed n = any (`T.isPrefixOf` n)
            ["io.", "int.", "integer.", "string.", "list.", "char."
            , "bool.", "require.", "exception.", "math.", "float."
            , "builtin.", "private_builtin."
            , "map.", "set.", "maybe.", "pair.", "assoc_list."]
            || n `elem` ["mercury_fail", "mercury_not", "list_range"
                        , "unify", "mercury_choose"]
          taggedName
            | isStdlibPrefixed predName' = predName'
            | otherwise = predName' <> "__" <> T.pack (show (length args))
      in EApp (EVar (Name taggedName 0))
           (map argExpr args)
  where
    -- A "real" variable arg is one that won't be treated as a literal by
    -- argExpr: not a Mercury int/string/char literal, and not a lowercase
    -- atom (which the bridge wraps as a 0-arg ctor allocation).
    isJustVar t =
      case readMaybe (T.unpack t) :: Maybe Integer of
        Just _ -> False
        Nothing -> isNothing (parseMercuryStringLit t)
                   && isNothing (parseMercuryCharLit t)
                   && not (isMercuryAtomLowercase t)
    isMercuryAtomLowercase t = case T.uncons t of
      Just (c, _) | c >= 'a' && c <= 'z' -> True
      _ -> False
    isNothing Nothing = True
    isNothing _       = False
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
-- Deconstruct in test position: a semidet pattern match.  The ECase
-- produced by translateGoalK is exhaustive (single branch), so the
-- body always runs even when the tag doesn't match — the test would
-- then erroneously succeed.  Emit a tag-check explicitly: return 1
-- when the scrutinee matches the ctor (binding the pattern vars in
-- the body), and 0 when it doesn't (fall through to a PatWild arm).
translateGoalAsTest _kctors env (GoalConstruct var ctor args)
  | Set.member var env =
      let bareCtor = case T.breakOnEnd "." ctor of
            ("", n) -> n
            (_,  n) -> n
      in ECase (EVar (Name var 0))
        [ Branch (PatCon (QName "" (Name bareCtor 0))
                   [PatVar (Name a 0) anyTy | a <- args])
                 Nothing (ELit (LitInt 1))
        , Branch (PatWild anyTy) Nothing (ELit (LitInt 0))
        ]
translateGoalAsTest _kctors _env (GoalDeconstruct var ctor args) =
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name ctor 0))
               [PatVar (Name a 0) anyTy | a <- args])
             Nothing (ELit (LitInt 1))
    , Branch (PatWild anyTy) Nothing (ELit (LitInt 0))
    ]
-- Unify in test position: @X = Y@ where BOTH sides are already bound is
-- a semidet structural equality check.  The fallback path below would
-- translate it as @let _ = unify(X, Y) in 1@ — discarding the unify
-- result so every check succeeds.  For surd's @R = rational.zero@ inside
-- @rat_sqrt@, that always-true behaviour skipped the int_sqrt branch and
-- returned @yes(rational.zero)@ for any non-zero R, propagating S=0
-- through @euler1(S)@ into apply_euler and zeroing every integral.
-- Bind both sides as aliases (no-op if already in scope), call unify,
-- and yield its result as the test outcome.
translateGoalAsTest _kctors env (GoalUnify x y)
  -- Both bound, non-literal: structural equality via unify runtime.
  | Set.member x env, Set.member y env
  , isNothing (readMaybeInt x), isNothing (readMaybeInt y)
  , isNothing (parseMercuryStringLit x), isNothing (parseMercuryStringLit y)
  , isNothing (parseMercuryCharLit x), isNothing (parseMercuryCharLit y)
  , isNothing (parseMercuryFloatLit x), isNothing (parseMercuryFloatLit y)
  = EApp (EVar (Name "unify" 0)) [EVar (Name x 0), EVar (Name y 0)]
  -- Bound var = integer literal: emit cmpi-eq directly.  HLDS uses this
  -- shape pervasively to check function results against constants, e.g.
  -- @V_17 = degree(W), V_17 = 0@ inside yun_loop's cond — without an
  -- explicit equality test the fallback @let _ = unify(V, lit) in 1@
  -- always yielded 1 and every such cond fired its then-branch,
  -- collapsing factor's square_free result to [] (no factors added).
  | Set.member x env, Just n <- readMaybeInt y
  = ECase (EVar (Name x 0))
      [ Branch (PatLit (LitInt n)) Nothing (ELit (LitInt 1))
      , Branch (PatWild anyTy)     Nothing (ELit (LitInt 0))
      ]
  | Set.member y env, Just n <- readMaybeInt x
  = ECase (EVar (Name y 0))
      [ Branch (PatLit (LitInt n)) Nothing (ELit (LitInt 1))
      , Branch (PatWild anyTy)     Nothing (ELit (LitInt 0))
      ]
  where
    readMaybeInt :: Text -> Maybe Integer
    readMaybeInt t = readMaybe (T.unpack t)
    isNothing Nothing = True
    isNothing _       = False
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

-- | Typeclass-method-wrapper rewrites: surd's polymorphic code goes
-- through @poly.ring_zero(TCI)@, @poly.ring_add(TCI, A, B)@, etc.,
-- which the Mercury compiler implements as
-- @class_method_call(TCI, MethodIdx, args)@.  We don't have a real
-- typeclass-info dictionary, so the class_method_call stubs return
-- identity sentinels and produce wrong values.  As a monomorphisation
-- hack, rewrite these wrappers at translation time to direct calls
-- on the rational instance (the most common instance in surd
-- demos).  Returns Just rewritten-call on a match, Nothing
-- otherwise.
rewriteTypeclassMethod :: Text -> [Text] -> Maybe (Text, [Text])
rewriteTypeclassMethod predName' args = case (predName', args) of
  -- ring(K) methods: drop the leading TypeClassInfo arg, dispatch
  -- to the rational instance.  The HLDS emits these in two forms:
  --   * function form @V = poly.ring_zero(TCI)@ — bridge appends V as
  --     trailing output arg, so we see @[TCI, V]@ at this point.
  --   * predicate form @poly.ring_add(TCI, A, B, V)@ — already 4 args.
  -- Match both: the bare wrapper-arity pattern AND the +1 output arg.
  ("poly.ring_zero",     [_tci])           -> Just ("rational.zero",  [])
  ("poly.ring_zero",     [_tci, o])        -> Just ("rational.zero",  [o])
  ("poly.ring_one",      [_tci])           -> Just ("rational.one",   [])
  ("poly.ring_one",      [_tci, o])        -> Just ("rational.one",   [o])
  ("poly.ring_add",      [_tci, a, b])     -> Just ("rational.+",     [a, b])
  ("poly.ring_add",      [_tci, a, b, o])  -> Just ("rational.+",     [a, b, o])
  ("poly.ring_sub",      [_tci, a, b])     -> Just ("rational.-",     [a, b])
  ("poly.ring_sub",      [_tci, a, b, o])  -> Just ("rational.-",     [a, b, o])
  ("poly.ring_mul",      [_tci, a, b])     -> Just ("rational.*",     [a, b])
  ("poly.ring_mul",      [_tci, a, b, o])  -> Just ("rational.*",     [a, b, o])
  ("poly.ring_negate",   [_tci, a])        -> Just ("rational.-",     [a])
  ("poly.ring_negate",   [_tci, a, o])     -> Just ("rational.-",     [a, o])
  ("poly.ring_is_zero",  [_tci, a])        -> Just ("rational.is_zero", [a])
  ("poly.ring_from_int", [_tci, a])        -> Just ("rational.rational", [a])
  ("poly.ring_from_int", [_tci, a, o])     -> Just ("rational.rational", [a, o])
  ("poly.ring_equal",    [_tci, a, b])     -> Just ("rational.equal", [a, b])
  -- field(K) methods.
  ("poly.field_div",     [_tci, a, b])     -> Just ("rational./",     [a, b])
  ("poly.field_div",     [_tci, a, b, o])  -> Just ("rational./",     [a, b, o])
  ("poly.field_recip",   [_tci, a])        -> Just ("rational.recip", [a])
  ("poly.field_recip",   [_tci, a, o])     -> Just ("rational.recip", [a, o])
  -- ord(K) methods.
  ("poly.ord_cmp",       [_tci, a, b])     -> Just ("rational.cmp",   [a, b])
  ("poly.ord_cmp",       [_tci, a, b, o])  -> Just ("rational.cmp",   [a, b, o])
  ("poly.ord_lt",        [_tci, a, b])     -> Just ("rational.<",     [a, b])
  ("poly.ord_le",        [_tci, a, b])     -> Just ("rational.=<",    [a, b])
  ("poly.ord_gt",        [_tci, a, b])     -> Just ("rational.>",     [a, b])
  ("poly.ord_ge",        [_tci, a, b])     -> Just ("rational.>=",    [a, b])
  _ -> Nothing

-- | Names that share their bare-form with a known stdlib TYPE but are
-- actually stdlib FUNCTIONS — when parseCtorApp returns one of these
-- the translator must route through GoalCall (function semantics)
-- rather than ECon (allocation).  E.g. @integer.integer/1@ converts
-- an @int@ to an @integer@; the bare name @integer@ matches a type
-- ctor recorded via type_ctor_info, but the call is a function.
isKnownStdlibFunction :: Text -> Bool
isKnownStdlibFunction n = n `elem`
  [ "integer.integer"
  , "integer.zero", "integer.one"
  , "rational.rational", "rational.zero", "rational.one"
  , "float.float"
  , "char.det_from_int", "char.to_int"
  , "string.from_int", "string.int_to_string"
  -- Mercury's typeclass-info construction: HLDS emits
  -- @TCI = typeclass_info_const(N)@ where N is the instance index.
  -- Route to the runtime stub (identity returning N) so the TCI is a
  -- plain int.  Without this guard the bridge sees @typeclass_info_const@
  -- as a known ctor and allocates a 1-field cell with field 0 = N;
  -- downstream typeclass dispatch then tries to call that field as a
  -- function pointer (= N) and crashes at PC=N.
  , "typeclass_info_const"
  , "private_builtin.typeclass_info_const"
  ]

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
      -- Mercury char literals (e.g. @'a'@) carry an int codepoint
      -- through the i64-model; treat them like int literals for
      -- binding-direction inference so X = 'a' binds X to 97
      -- rather than falling to the unify-stub fallback (which
      -- leaves X unbound and surfaces as a free-EVar leak).
      lhsChar = parseMercuryCharLit x
      rhsChar = parseMercuryCharLit y
      lhsFloat = parseMercuryFloatLit x
      rhsFloat = parseMercuryFloatLit y
      bindLhs = not (Set.member x env)
      bindRhs = not (Set.member y env)
  in case (lhsLit <|> lhsChar, rhsLit <|> rhsChar, lhsStr, rhsStr, lhsFloat, rhsFloat) of
       -- Both Int literals: no binding; just continue.
       (Just _, Just _, _, _, _, _) -> k
       -- X = <int literal>: bind X to the literal if not yet bound.
       (_, Just n, _, _, _, _) | bindLhs ->
         ELet [[Bind (Name x 0) intTy (ELit (LitInt n)) DefVal]] k
       -- <int literal> = Y: bind Y to the literal if not yet bound.
       (Just n, _, _, _, _, _) | bindRhs ->
         ELet [[Bind (Name y 0) intTy (ELit (LitInt n)) DefVal]] k
       -- X = <float literal>: bind X to the LitFloat.  Crucial for
       -- Mercury programs that unify a fresh var with a float
       -- constant; without this the var leaks as a free EVar ref.
       (_, _, _, _, _, Just d) | bindLhs ->
         ELet [[Bind (Name x 0) intTy (ELit (LitFloat d)) DefVal]] k
       -- <float literal> = Y: bind Y to the LitFloat.
       (_, _, _, _, Just d, _) | bindRhs ->
         ELet [[Bind (Name y 0) intTy (ELit (LitFloat d)) DefVal]] k
       -- X = "string literal": bind X to the LitString.
       (_, _, _, Just s, _, _) | bindLhs ->
         ELet [[Bind (Name x 0) stringTy (ELit (LitString s)) DefVal]] k
       -- "string literal" = Y: bind Y to the LitString.
       (_, _, Just s, _, _, _) | bindRhs ->
         ELet [[Bind (Name y 0) stringTy (ELit (LitString s)) DefVal]] k
       -- X = Y, one side bound: bind the other as an alias.
       -- Type the alias as @mercury.value@ (boxed) so Perceus inserts
       -- retains based on multi-use count.  Using @intTy@ (std.int) here
       -- would mark the alias as unboxed and Perceus would skip refcounting
       -- — but the underlying value is often a heap-allocated rational /
       -- list / etc.  kk_retain/kk_drop are safe on plain ints too because
       -- they gate on kk_is_heap_ptr.
       (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) | bindLhs && not bindRhs ->
         ELet [[Bind (Name x 0) valueTy (EVar (Name y 0)) DefVal]] k
       (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing) | bindRhs && not bindLhs ->
         ELet [[Bind (Name y 0) valueTy (EVar (Name x 0)) DefVal]] k
       -- Fallback: emit a stub unify call.  Use argExpr so literal
       -- ints / chars / strings don't leak as bogus free EVar refs
       -- (e.g. `0` sanitised to `_0$0`).
       _ ->
         ELet [[Bind (Name "_" 0) intTy
                  (EApp (EVar (Name "unify" 0))
                        [argExpr x, argExpr y])
                  DefVal]] k

translateGoalK _kctors _env (GoalCall predName' args) k
  | Just (newName, newArgs) <- rewriteTypeclassMethod predName' args =
      translateGoalK _kctors _env (GoalCall newName newArgs) k
translateGoalK _kctors _env (GoalCall predName' args) k =
  -- Identify output variables using a "trailing unbound args" heuristic.
  -- Mercury HLDS lists every argument of a predicate at the call site,
  -- inputs and outputs alike.  For det predicates whose mode declaration
  -- is `(in, in, out, out)` (e.g. poly.div_mod / 4), BOTH trailing outputs
  -- are unbound at the call site and must be dropped from the input
  -- arg list — otherwise the call's arity disagrees with translatePred's
  -- definition arity (which only counts ModeIn/ModeDi positions).
  --
  -- The bind side: we treat the FIRST trailing-unbound arg as the
  -- "primary" output (the value the call returns), matching translatePred's
  -- choice of @outputName = head [ args !! i | i <- outputModes ]@.
  -- Additional trailing outputs are dropped without re-binding; this
  -- mirrors the bridge's current behaviour for multi-output preds where
  -- only the first output's value flows through the CPS chain.
  let env = _env
      isLitArg t = isJust (readMaybe (T.unpack t) :: Maybe Integer)
                || isJust (parseMercuryStringLit t)
                || isJust (parseMercuryCharLit t)
                || isJust (parseMercuryFloatLit t)
      isBound t = Set.member t env || isLitArg t
      -- Walk args from the right, dropping a contiguous run of unbound
      -- variable names.  Anything else stays in callInputs.
      (revBoundPrefix, trailingUnbound) =
        let (suffix, prefix) = span (not . isBound) (reverse args)
        in (reverse prefix, reverse suffix)
      -- Mercury's `private_builtin.type_info_from_typeclass_info` and
      -- `private_builtin.superclass_from_typeclass_info` are 3-place
      -- predicates @pred(TCI, Idx, Out)@; our runtime stubs are 2-arg
      -- identity functions.  When @Out@ is already bound (Mercury emits
      -- the same extraction redundantly), keeping it in callInputs makes
      -- the emitter oversaturate the 2-arg stub and dispatch through
      -- field 0 of the TCI as a function pointer — crash.  Drop the
      -- 3rd arg and re-bind it (idempotent: stub returns same value).
      isPrivateBuiltinTciFn n = n `elem`
        [ "private_builtin.type_info_from_typeclass_info"
        , "private_builtin.superclass_from_typeclass_info"
        , "private_builtin.instance_constructor_from_typeclass_info"
        ]
      privBuiltinAllBound =
        isPrivateBuiltinTciFn predName' && null trailingUnbound
                                        && length revBoundPrefix == 3
      callInputs
        | privBuiltinAllBound = take 2 revBoundPrefix
        | otherwise = revBoundPrefix
      outputBinding = case trailingUnbound of
        (n:_) -> Just n
        []
          | privBuiltinAllBound -> Just (last revBoundPrefix)
          | otherwise           -> Nothing
      -- Same arity-suffix convention as translatePred uses for def
      -- names: append "__<n>" where n is the number of inputs the
      -- callee receives.  Lets the linker disambiguate overloads
      -- like @rational.+/1@ vs @rational.+/2@.  Skip for known
      -- stdlib-prefixed callees whose targets are runtime stubs with
      -- fixed names (no overload disambiguation needed there).
      isStdlibPrefixed n = any (`T.isPrefixOf` n)
        ["io.", "int.", "integer.", "string.", "list.", "char."
        , "bool.", "require.", "exception.", "math.", "float."
        , "builtin.", "private_builtin."
        , "map.", "set.", "maybe.", "pair.", "assoc_list."]
        -- The bridge-synthesised intrinsics live without a module
        -- qualifier (they correspond to runtime helpers like
        -- @mercury_fail@, @mercury_not@, @list_range@) — keep them
        -- bare so the call name matches the def name verbatim.
        || n `elem` ["mercury_fail", "mercury_not", "list_range"
                    , "unify", "mercury_choose"]
      taggedName
        | isStdlibPrefixed predName' = predName'
        | otherwise = predName' <> "__" <> T.pack (show (length callInputs))
      callExpr
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
      -- Default-bind every secondary trailing-unbound output to 0.
      -- The call only returns one value (the FIRST output), but multi-
      -- output preds like @poly.div_mod(In1, In2, Q, R)@ have the
      -- caller subsequently reference R as if bound.  Without a default
      -- binding, R surfaces as an unresolved EVar leaks at link time.
      -- @0@ is a placeholder — semantically wrong but link-clean, and
      -- programs that actually depend on the secondary output will
      -- need an explicit multi-output tuple-returning protocol later.
      secondaryOutputs = case trailingUnbound of
        (_:rest) -> rest
        []       -> []
      -- Compute secondary outputs for known multi-output preds.
      -- @poly.div_mod(TCI, F, G, Q, R)@: R = F - Q*G is the natural
      -- mathematical relation, computable from the call's bound Q +
      -- the input F & G.  Without this, surd's @gcd@ recurses with
      -- R = 0 (literal int, defaulted from secondary-output placeholder)
      -- and the next div_mod gets G=NULL → reciprocal(NULL) crash.
      isDivMod = predName' `elem` ["poly.div_mod", "div_mod"]
      divModRemainderExpr fVar gVar qName tciVar =
        EApp (EVar (Name "poly_sub__3" 0))
          [ EVar (Name tciVar 0)
          , EVar (Name fVar 0)
          , EApp (EVar (Name "poly_mul__3" 0))
              [ EVar (Name tciVar 0)
              , EVar (Name qName 0)
              , EVar (Name gVar 0)
              ]
          ]
      wrapSecondaries body = foldr
        (\n acc ->
           let defaultExpr
                 -- div_mod: secondary output is R = F - Q*G.
                 | isDivMod, length args == 5
                 , [tciV, fV, gV] <- take 3 args
                 , Just qV <- outputBinding
                 = divModRemainderExpr fV gV qV tciV
                 | otherwise = ELit (LitInt 0)
           in ELet [[Bind (Name n 0) valueTy defaultExpr DefVal]] acc)
        body
        secondaryOutputs
  in case outputBinding of
       Nothing      -> ELet [[Bind (Name "_" 0) valueTy callExpr DefVal]]
                            (wrapSecondaries k)
       Just outName -> ELet [[Bind (Name outName 0) valueTy callExpr DefVal]]
                            (wrapSecondaries k)

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
  -- Mercury's if-then-else lets the cond's local bindings flow into
  -- the then-branch.  Standard structure: ECase on the cond's
  -- boolean test result.  THEN-side is the cond's CPS chain
  -- wrapping the then-body so cond's pattern variables enter the
  -- then-body's lexical scope.
  let condEnv  = extendBindingsFor env cond
      thenE    = translateGoalK kctors condEnv then' k
      elseE    = translateGoalK kctors env else' k
      -- Re-run cond's CPS chain wrapping thenE; cond's bindings
      -- now reach thenE.  Safe to duplicate now that PAP wrappers
      -- are named per-arity (no cross-arity wrapper-sharing bug).
      thenWithCondBindings = translateGoalK kctors env cond thenE
  in ECase (translateGoalAsTest kctors env cond)
       [ Branch (PatLit (LitInt 1)) Nothing thenWithCondBindings
       , Branch (PatWild boolTy)    Nothing elseE
       ]

translateGoalK kctors env (GoalSwitch var cases) k =
  -- Strip module qualifier and @/arity@ suffix from the HLDS functor name
  -- so the switch arm's tag matches the construction-side ECon tag.
  -- HLDS prints functors as @module.ctor/arity@ (e.g. @euler1/1@);
  -- parseCtorApp on the construction side returns just @euler1@.  Without
  -- matching strips the tag hashes diverge and every switch arm fails.
  ECase (EVar (Name var 0))
    [ Branch (PatCon (QName "" (Name (bareTag tag) 0)) []) Nothing
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
  | Set.member ctor kctors || Set.member bareCtor kctors
  , not (isKnownStdlibFunction ctor) =
      -- Real ctor allocation.  ECon name is bare (the data-decl form).
      -- The @isKnownStdlibFunction@ guard rejects names like
      -- @integer.integer@ that have the same bare name as a type
      -- ctor (@integer@) but are actually FUNCTIONS in the stdlib
      -- (int-to-integer conversion).  Without this guard, the
      -- type_ctor_info scan adds @integer@ to kctors, then the
      -- @rational(N) = rational_norm(integer(N), integer.one)@
      -- chain routes @integer(N)@ through ECon allocation instead
      -- of EApp + integer_integer runtime stub — producing a
      -- 1-field cell where surd expected the raw integer N, and the
      -- whole rational arithmetic chain inherits a non-integer
      -- pointer in the numerator field.
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
  -- For ITE / switch / disjunction, intersect the bindings from every
  -- branch: a variable is bound after the construct only if every
  -- branch binds it.  This propagates AStr-style ITE-internal
  -- bindings to subsequent conjuncts so they don't leak as free
  -- EVar references at the emit stage.
  GoalIfThenElse _ t e ->
    -- Variables bound by an error-only branch (e.g. `require.unexpected`
    -- / `require.error` / `mercury_fail`) never flow out of the ITE —
    -- the program either aborts or backtracks.  Treating such a branch
    -- as "binds everything" lets the OTHER branch's bindings reach
    -- subsequent conjuncts.  Without this, an ITE whose else-arm calls
    -- @require.unexpected@ (the common dead-code marker in Mercury)
    -- intersects to nothing and a variable that the then-arm bound
    -- (like AN in factoring.candidates_from_int_coeffs) appears
    -- unbound at the next conjunct.
    let thenE = extendBindingsFor env t
        elseE = extendBindingsFor env e
        thenAborts = goalAlwaysAborts t
        elseAborts = goalAlwaysAborts e
    in case (thenAborts, elseAborts) of
         (False, True) -> thenE   -- else aborts → then's bindings flow
         (True, False) -> elseE   -- then aborts → else's bindings flow
         _             -> Set.intersection thenE elseE
  GoalDisj gs -> case gs of
    []   -> env
    (g0:rest') ->
      -- Same abort-propagation as for ITE: filter out disj arms that
      -- always abort, then intersect the rest.  An "all arms abort"
      -- disjunction is unreachable; keep env as-is in that case.
      let arms = filter (not . goalAlwaysAborts) (g0 : rest')
      in case arms of
           []       -> env
           (a0:as') ->
             foldl (\acc g -> Set.intersection acc (extendBindingsFor env g))
                   (extendBindingsFor env a0) as'
  _                -> env
  where
    isJust (Just _) = True
    isJust Nothing  = False

-- | Conservative check: does this goal always abort or fail (never
-- produces normal-return bindings)?  Recognises the standard Mercury
-- error idioms surd-mercury uses for dead-code branches.
goalAlwaysAborts :: MercuryGoal -> Bool
goalAlwaysAborts g = case g of
  GoalCall n _
    | n == "require.unexpected"
   || n == "require.error"
   || n == "require.func_error"
   || n == "exception.throw"
   || n == "mercury_fail"
   || n == "throw" -> True
  GoalConj gs   -> any goalAlwaysAborts gs
  GoalDisj gs
    | not (null gs) -> all goalAlwaysAborts gs
  GoalIfThenElse _ t e -> goalAlwaysAborts t && goalAlwaysAborts e
  _ -> False

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
      -- Mercury HLDS at the determinism stage strips the `:- type ...`
      -- declarations, but every type that survives in the IR is
      -- referenced via a @type_ctor_info(module, ctor_name, arity)@
      -- pseudo-call.  Scan the body for these and collect the second
      -- arg as a known ctor name — sufficient to disambiguate
      -- @CTOR(args)@ from a user function call at translation time
      -- even when the ctor only appears in CONSTRUCT contexts.
      goTCI g = case g of
        GoalConstruct _ ctor cargs
          | ctor == "type_ctor_info" || ctor == "private_builtin.type_info_const"
          , (modName : ctorName : _) <- cargs
            -> [(modName <> "." <> ctorName, ctorName)]
          | otherwise -> []
        GoalCall _ _ -> []
        GoalConj gs            -> concatMap goTCI gs
        GoalDisj gs            -> concatMap goTCI gs
        GoalIfThenElse c t e   -> goTCI c ++ goTCI t ++ goTCI e
        GoalNot g'             -> goTCI g'
        GoalSwitch _ cases     -> concatMap (\(_, b) -> goTCI b) cases
        GoalLambda _ _ _ b     -> goTCI b
        _                      -> []
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
       Just goal -> go inputArgs goal ++ goTCI goal
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
-- single-quoted char → ELit (LitInt codepoint); bare lowercase atom
-- → 0-arg ctor allocation (mercury module names and enum tags pass
-- through as ARGS to ctors like `type_ctor_info(rational, rational,
-- 0)` — without this they'd leak as free EVar refs at link time).
-- Anything else falls back to an EVar reference.
argExpr :: Text -> Expr
argExpr a =
  case readMaybe (T.unpack a) :: Maybe Integer of
    Just n  -> ELit (LitInt n)
    Nothing -> case parseMercuryFloatLit a of
      Just d  -> ELit (LitFloat d)
      Nothing -> case parseMercuryStringLit a of
        Just s  -> ELit (LitString s)
        Nothing -> case parseMercuryCharLit a of
          Just cp -> ELit (LitInt cp)
          Nothing
            -- Mercury naming convention: variables start with uppercase
            -- or `_`, atoms start with lowercase.  Treat bare-lowercase
            -- alphanumeric-plus-underscore tokens as 0-arg ctors.
            | isMercuryAtom a -> EApp (ECon (QName "" (Name a 0))) []
            | otherwise -> EVar (Name a 0)
  where
    isMercuryAtom t = case T.uncons t of
      Just (c, rest)
        | c >= 'a' && c <= 'z'
        , T.all (\ch -> ch == '_' || (ch >= 'a' && ch <= 'z')
                                  || (ch >= 'A' && ch <= 'Z')
                                  || (ch >= '0' && ch <= '9')) rest
        -> True
      _ -> False

-- | Recognise a Mercury HLDS float literal.  Mercury prints these in
-- standard decimal form (`0.5`, `1.0`, `-2.3`) or scientific notation
-- (`1e-06`, `1.5e10`, `1e300`).  We require either a decimal point
-- OR an exponent marker to distinguish from plain integers (which
-- argExpr already handles via 'readMaybe \@Integer' BEFORE this).
-- Returns the parsed Double, leaving the bit-pattern conversion to
-- 'emitExpr (ELit (LitFloat _))'.
parseMercuryFloatLit :: Text -> Maybe Double
parseMercuryFloatLit t =
  let s = T.unpack (T.strip t)
  in if any (\c -> c == '.' || c == 'e' || c == 'E') s
        && not (T.any (== ' ') (T.strip t))
     then readMaybe s
     else Nothing

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

-- | Strip both the module qualifier (everything up to and including the
-- last @.@) and the @/arity@ suffix from an HLDS functor name, returning
-- the bare constructor name that hashes to the same tag as the
-- construction-side @ECon@.  For example:
-- @euler_integrate.euler1/1@ → @euler1@, @list_Cons/2@ → @list_Cons@.
bareTag :: Text -> Text
bareTag t =
  let afterDot = case T.breakOnEnd "." t of
        ("", n) -> n
        (_,  n) -> n
      (prefix, slashRest) = T.breakOn "/" afterDot
      arityRest = T.drop 1 slashRest
  in if not (T.null arityRest) && T.all (\c -> c >= '0' && c <= '9') arityRest
       then prefix
       else afterDot

-- | Common type shortcuts used by the translator.
intTy :: Type
intTy = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

-- | Mercury "any boxed value" type used for alias binds where we don't
-- know whether the RHS is a plain int or a heap pointer.  Treated as
-- boxed by Perceus so multi-use vars get refcounted.
valueTy :: Type
valueTy = TCon (TypeCon (QName "mercury" (Name "value" 0)) KindValue)

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
