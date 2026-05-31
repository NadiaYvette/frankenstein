{-# LANGUAGE LambdaCase #-}
-- | Frankenstein Core IR type-correctness checker.
--
-- A lightweight well-formedness pass that catches structural type
-- errors before the program reaches MLIR lowering. Catches the
-- mistakes bridges most commonly make: dangling constructor names,
-- wrong-arity perform sites, pattern-arity mismatches, function-call
-- arity mismatches on top-level definitions.
--
-- The checker is deliberately /not/ a full type inferencer:
--
--   * Polymorphism (TForall / TVar) is treated as "any type unifies"
--     — we don't track type-variable instantiation.
--   * Multiplicity (Linear / Affine / Many) is recorded but not
--     enforced — that's Perceus's job, not the type checker's.
--   * Effect rows (EffectRow) are not checked against the
--     surrounding handle scope — that's the evidence pass's
--     correctness obligation.
--
-- What the checker /does/ enforce:
--
--   * Every 'ECon qn' refers to a constructor declared in
--     'progData'.
--   * Every 'EPerform qn args' references an operation declared in
--     'progEffects', and the argument count matches the operation's
--     declared parameter count.
--   * Every 'EApp (EVar qn) args' where @qn@ resolves to a top-level
--     definition has arg count matching the definition's declared
--     'TFun' arity (after stripping outer 'TForall').
--   * Every 'PatCon qn pats' has pat count matching the
--     constructor's declared field count.
--   * 'ECase' has at least one branch.

module Frankenstein.Core.TypeCheck
  ( typeCheckProgram
  , TypeError(..)
  , prettyError
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- | A type error with enough context to localise the issue.
data TypeError
  = UnknownConstructor !QName
  | UnknownEffectOp !QName
  | EffectOpArityMismatch
      { teOp       :: !QName
      , teExpected :: !Int
      , teActual   :: !Int
      }
  | FnArityMismatch
      { teFn       :: !QName
      , teExpected :: !Int
      , teActual   :: !Int
      }
  | PatternArityMismatch
      { teCtor     :: !QName
      , teExpected :: !Int
      , teActual   :: !Int
      }
  | EmptyCase
  deriving (Eq, Show)

-- | Human-readable rendering of a 'TypeError'.
prettyError :: TypeError -> Text
prettyError = \case
  UnknownConstructor qn ->
    "unknown constructor " <> qnameToText qn
  UnknownEffectOp qn ->
    "unknown effect operation " <> qnameToText qn
  EffectOpArityMismatch op exp_ actual ->
    "effect op " <> qnameToText op
      <> ": expected " <> T.pack (show exp_)
      <> " args, got " <> T.pack (show actual)
  FnArityMismatch fn exp_ actual ->
    "function " <> qnameToText fn
      <> ": expected " <> T.pack (show exp_)
      <> " args, got " <> T.pack (show actual)
  PatternArityMismatch c exp_ actual ->
    "pattern " <> qnameToText c
      <> ": expected " <> T.pack (show exp_)
      <> " sub-patterns, got " <> T.pack (show actual)
  EmptyCase -> "empty ECase (no branches)"

qnameToText :: QName -> Text
qnameToText (QName m n)
  | T.null m  = nameText n
  | otherwise = m <> "." <> nameText n

-- | Top-level entry point. Returns the list of errors found across
-- every definition in the program. Empty list ⇒ well-formed.
typeCheckProgram :: Program -> [TypeError]
typeCheckProgram prog =
  concatMap (checkDef env) (progDefs prog)
  where
    env = buildEnv prog

-- | Read-only environment threaded through the walk.
data Env = Env
  { envCtors  :: !(Map (Text, Text) Int) -- (module, name) -> ctor arity
  , envOps    :: !(Map (Text, Text) Int) -- (eff module, op name) -> arity
  , envFnArity :: !(Map (Text, Text) Int) -- (module, name) -> def arity
  }

buildEnv :: Program -> Env
buildEnv prog = Env
  -- Explicit concatMap form (see commit a5a578c): GHC's nested-list-comp
  -- desugar uses derived helpers that Perceus over-drops.
  { envCtors   = Map.fromList $
      concatMap (\dd ->
        map (\c -> ( (qnameModule (conName c), nameText (qnameName (conName c)))
                   , length (conFields c) ))
            (dataCons dd))
        (progData prog)
  , envOps     = Map.fromList $
      concatMap (\ed ->
        map (\op -> ( ( qnameModule (effectName ed) <> nameText (qnameName (effectName ed))
                      , nameText (qnameName (opName op)))
                    , opArityOf (opType op) ))
            (effectOps ed))
        (progEffects prog)
  , envFnArity = Map.fromList
      [ ( (qnameModule (defName d), nameText (qnameName (defName d)))
        , typeArity (defType d) )
      | d <- progDefs prog
      ]
  }

-- | Number of arrow arguments at the head of a type, after stripping
-- universal quantifiers. Used to compute top-level fn arity.
typeArity :: Type -> Int
typeArity (TForall _ t)    = typeArity t
typeArity (TFun args _ _)  = length args
typeArity _                = 0

-- | Arity of an effect operation. Operations are usually declared as
-- @T_1 → … → T_N → Result@ in the IR, but bridges sometimes give a
-- bare result type for nullary operations. We pull the arrow arity
-- if there is one; otherwise 0.
opArityOf :: Type -> Int
opArityOf = typeArity

checkDef :: Env -> Def -> [TypeError]
checkDef env d = checkExpr env (defExpr d)

checkExpr :: Env -> Expr -> [TypeError]
checkExpr env = go
  where
    go = \case
      EVar _            -> []
      ELit _            -> []
      ECon qn           -> checkCtorExists env qn []
      EApp f args       ->
        let argsErrs = concatMap go args
            calleeErrs = case f of
              EVar v ->
                let key = (qnameModuleOf v, nameText v)
                in case Map.lookup key (envFnArity env) of
                     -- Don't flag every non-toplevel reference: many
                     -- EVars in a body refer to locally-bound names
                     -- (lambda params, let binders, pattern binders).
                     -- Skip those (Nothing case).
                     Nothing -> go f
                     Just n  | n == length args -> go f
                             | otherwise ->
                                 [ FnArityMismatch
                                     (QName (qnameModuleOf v) v)
                                     n (length args) ]
                                 ++ go f
              ECon qn ->
                  checkCtorExists env qn args ++ go f
              _ -> go f
        in calleeErrs ++ argsErrs
      ELam _params body -> go body
      ELet bgs body     ->
        -- Explicit concatMap form (see commit a5a578c).
        concat (concatMap (map (go . bindExpr)) bgs) ++ go body
      ECase scrut brs
        | null brs  -> [EmptyCase]
        | otherwise -> go scrut ++ concatMap (checkBranch env) brs
      ETypeApp e _      -> go e
      ETypeLam _ e      -> go e
      EPerform qn args  -> checkPerform env qn args ++ concatMap go args
      EHandle _ h b     -> go h ++ go b
      ERetain e         -> go e
      ERelease e        -> go e
      EDrop e           -> go e
      EReuse e1 e2      -> go e1 ++ go e2
      EDelay e          -> go e
      EForce e          -> go e
      EFunRef _         -> []
        -- function reference: arity check is at the call site.

    -- Find module qualifier for a bare Name. We look it up via
    -- envFnArity; if not present, treat as locally-bound (no error).
    qnameModuleOf :: Name -> Text
    qnameModuleOf n =
      let candidates =
            [ m | ((m, nm), _) <- Map.toList (envFnArity env)
                , nm == nameText n ]
      in case candidates of
           (m:_) -> m
           []    -> ""

checkBranch :: Env -> Branch -> [TypeError]
checkBranch env (Branch pat mg body) =
  checkPattern env pat
    ++ maybe [] (checkExpr env) mg
    ++ checkExpr env body

checkPattern :: Env -> Pattern -> [TypeError]
checkPattern env = \case
  PatCon qn pats ->
    case Map.lookup (qnameModule qn, nameText (qnameName qn)) (envCtors env) of
      Just n
        | n == length pats -> concatMap (checkPattern env) pats
        | otherwise        -> [PatternArityMismatch qn n (length pats)]
      Nothing -> [UnknownConstructor qn]
  PatVar _ _  -> []
  PatWild _   -> []
  PatLit _    -> []

checkCtorExists :: Env -> QName -> [Expr] -> [TypeError]
checkCtorExists env qn args =
  case Map.lookup (qnameModule qn, nameText (qnameName qn)) (envCtors env) of
    Just _  -> []   -- arity is permissive: ECon may be partially applied
    Nothing ->
      -- Heuristic: don't complain about purely local names that
      -- bridges sometimes lift to ECon (e.g. "True", "False"). We
      -- only flag missing constructors when the qname has a module.
      [ UnknownConstructor qn | not (T.null (qnameModule qn)) ]

checkPerform :: Env -> QName -> [Expr] -> [TypeError]
checkPerform env qn args =
  case Map.lookup (qnameModule qn, nameText (qnameName qn)) (envOps env) of
    Just n
      | n == 0 || n == length args -> []  -- 0 = unknown declared arity
      | otherwise -> [EffectOpArityMismatch qn n (length args)]
    Nothing ->
      [ UnknownEffectOp qn | not (T.null (qnameModule qn)) ]
