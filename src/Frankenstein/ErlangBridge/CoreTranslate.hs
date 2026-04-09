-- | Erlang forms (as produced by @erl_parse:parse_form/1@) →
-- Frankenstein Core.
--
-- Supported subset:
--
--   * @-module(name).@ and @-export([...]).@ attributes (consumed,
--     no semantic effect).
--   * @f(X1, X2, ...) when Guard -> Body; f(X1, X2, ...) -> Body.@
--     with multiple clauses. Guards compile into nested @if@ chains;
--     constructor patterns (tagged tuples + atoms) compile into a real
--     'ECase' over the matching argument.
--   * Expressions: integer literals, variable refs, binary ops
--     (@+ - * / rem == /= < =< > >=@), function application
--     @call f([args])@, local calls @f(args)@, tagged tuples
--     @{cons, h, t}@ as constructor allocation, @case E of P -> B; ...@.
--   * @main()@ (if present) becomes the entry point; its return
--     value is the program's result.
--
-- == ADT story
--
-- Tagged tuples whose first element is an atom are treated as
-- constructor allocations: @{leaf, V}@ becomes @EApp (ECon "leaf") [V]@,
-- and @{node, L, V, R}@ becomes @EApp (ECon "node") [L, V, R]@. The
-- corresponding pattern syntax in clause heads or @case@ branches
-- becomes 'PatCon'. The @ConTags@ pass assigns these orphan ctors
-- deterministic tags after the bridge is done.
--
-- Bare atom literals in expression position remain 'EVar' references
-- (Erlang has no explicit type declarations, so it's not safe to
-- assume every atom is a ctor); to use a nullary ctor, write a unary
-- tuple wrapper or rely on context that injects it as a sub-term of a
-- ctor pattern.
module Frankenstein.ErlangBridge.CoreTranslate
  ( translateErlang
  ) where

import Frankenstein.Core.Types
import Frankenstein.ErlangBridge.AstParse

import Data.Text (Text)
import qualified Data.Text as T

translateErlang :: Text -> ETerm -> Either Text Program
translateErlang modName (ETList forms) = do
  defs <- concat <$> mapM (translateForm modName) forms
  let mainDef = case filter isMain defs of
        (m:_) -> [ Def
                     { defName       = QName modName (Name "main" 0)
                     , defType       = TFun [] EffectRowEmpty intT
                     , defExpr       = ELam [] (EApp (EVar (Name (nmFromDef m) 0)) [])
                     , defSort       = DefFun
                     , defVisibility = Public
                     }
                 ]
        _ -> []
      allDefs = defs ++ mainDef
      -- Synthesize a single 'erlang_adt' DataDecl listing every
      -- constructor used in expressions or patterns. The Erlang
      -- bridge has no declared types, so we infer the ctor inventory
      -- from use sites — this satisfies the linker (which warns on
      -- unknown ctor refs) and gives the ConTags pass deterministic
      -- tag assignment without a separate orphan path.
      ctorInventory = collectErlangCtors allDefs
      adtDecl
        | null ctorInventory = []
        | otherwise =
            [ DataDecl
                { dataName   = QName modName (Name "erlang_adt" 0)
                , dataParams = []
                , dataCons   =
                    [ ConDecl
                        { conName   = QName modName (Name cn 0)
                        , conFields =
                            [ (Name ("f" <> T.pack (show i)) 0, intT)
                            | i <- [0 .. arity - 1]
                            ]
                        , conVis    = Public
                        }
                    | (cn, arity) <- ctorInventory
                    ]
                , dataVis    = Public
                }
            ]
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = allDefs
    , progData    = adtDecl
    , progEffects = []
    }
  where
    isMain d = case qnameName (defName d) of
                 Name "main_fn" _ -> True
                 _                -> False
    nmFromDef d = nameText (qnameName (defName d))
translateErlang _ other = Left ("translateErlang: expected form list, got " <> T.pack (show other))

-- | Translate a single Erlang top-level form.
translateForm :: Text -> ETerm -> Either Text [Def]
translateForm modName form = case form of
  ETTuple [ETAtom "attribute", _, ETAtom _, _] ->
    Right []  -- -module, -export, etc.
  ETTuple [ETAtom "function", _, ETAtom name, _arity, ETList clauses] -> do
    d <- translateFunction modName name clauses
    pure [d]
  ETTuple (ETAtom "eof" : _) -> Right []
  other -> Left ("translateForm: unsupported: " <> T.pack (show other))

translateFunction :: Text -> Text -> [ETerm] -> Either Text Def
translateFunction modName name clauses = do
  (params, body) <- clausesToExpr name clauses
  let lamParams = [(Name p 0, intT) | p <- params]
      argTypes  = [(Many, intT)     | _ <- params]
      fnTy      = TFun argTypes EffectRowEmpty intT
      -- Avoid clashing with the generated wrapper "main" when the
      -- user's function is already called main.
      declName  = if name == "main" then "main_fn" else name
  pure Def
    { defName       = QName modName (Name declName 0)
    , defType       = fnTy
    , defExpr       = ELam lamParams body
    , defSort       = DefFun
    , defVisibility = Public
    }

-- | Combine a list of clauses into a single expression. The
-- parameter names come from the first clause. Subsequent clauses are
-- folded into nested @if@ expressions guarded by the clause guards.
-- Pattern matching on literal arguments (e.g. @fact(0) -> 1;@) is
-- modelled as a guard @arg == 0@.
--
-- Special case: if every clause has exactly one parameter and at
-- least one clause uses a constructor pattern (tagged tuple or atom),
-- the whole function compiles to a single 'ECase' over the parameter
-- with one 'Branch' per clause. This routes Erlang's pattern match
-- through the same 'FlattenPatterns' Maranget compiler the Haskell
-- bridge uses.
clausesToExpr :: Text -> [ETerm] -> Either Text ([Text], Expr)
clausesToExpr fname clauses
  | not (null clauses)
  , Just bodyExpr <- tryCtorDispatch fname clauses
  = bodyExpr
clausesToExpr fname clauses = case clauses of
  [] -> Left ("function " <> fname <> ": no clauses")
  (first:rest) -> do
    (params, firstBody) <- extractParamsAndBody first
    firstCond <- clauseCondition params first
    body <- foldClauses params firstCond firstBody rest
    pure (params, body)
  where
    extractParamsAndBody :: ETerm -> Either Text ([Text], Expr)
    extractParamsAndBody (ETTuple [ETAtom "clause", _, ETList pats, _guards, ETList bodyExprs]) = do
      let params = [ synthParamName i p | (i, p) <- zip [0::Int ..] pats ]
          rename = [ (o, nm) | (p, nm) <- zip pats params, Just o <- [origName p] ]
      body <- translateBody rename bodyExprs
      pure (params, body)
    extractParamsAndBody other = Left ("bad clause shape: " <> T.pack (show other))

    -- If the pattern is a variable, use its name; otherwise synth "arg_N".
    synthParamName _ (ETTuple [ETAtom "var", _, ETAtom nm]) = T.toLower nm
    synthParamName i _ = "arg_" <> T.pack (show i)

    origName :: ETerm -> Maybe Text
    origName (ETTuple [ETAtom "var", _, ETAtom nm]) = Just nm
    origName _ = Nothing

    clauseCondition :: [Text] -> ETerm -> Either Text (Maybe Expr)
    clauseCondition params (ETTuple [ETAtom "clause", _, ETList pats, ETList guards, _]) = do
      -- Combine per-argument literal-pattern checks with user guards.
      patChecks <- sequence
        [ case pat of
            ETTuple [ETAtom "integer", _, ETInt n] ->
              Right (Just (EApp (EVar (Name "==" 0))
                                [EVar (Name pname 0), ELit (LitInt n)]))
            ETTuple [ETAtom "var", _, _] -> Right Nothing
            other -> Left ("clause pattern NYI: " <> T.pack (show other))
        | (pat, pname) <- zip pats params
        ]
      guardE <- case guards of
        [] -> Right Nothing
        (ETList gs : _) -> do
          -- each gs is a sequence of guard tests ANDed together
          es <- mapM (translateExpr []) gs
          pure (combineAnd es)
        _ -> Right Nothing
      let checks = [c | Just c <- patChecks] ++ maybe [] (:[]) guardE
      pure (combineAnd checks)
    clauseCondition _ _ = Right Nothing

    combineAnd :: [Expr] -> Maybe Expr
    combineAnd [] = Nothing
    combineAnd [e] = Just e
    combineAnd (e:es) = Just $
      foldr (\a acc -> EApp (EVar (Name "*" 0)) [a, acc]) e es
      -- Emulate boolean conjunction via multiplication on 0/1.

    foldClauses :: [Text] -> Maybe Expr -> Expr -> [ETerm] -> Either Text Expr
    foldClauses _ _ body [] = Right body
    foldClauses params mcond firstBody (c:cs) = do
      (_, thisBody) <- extractParamsAndBody c
      mthisCond <- clauseCondition params c
      restBody <- foldClauses params mthisCond thisBody cs
      pure $ case mcond of
        Just cond ->
          ECase cond
            [ Branch (PatLit (LitInt 0)) Nothing restBody
            , Branch (PatWild intT)      Nothing firstBody
            ]
        Nothing -> firstBody

-- | If every clause has exactly one parameter and the union of clause
-- patterns includes at least one constructor pattern, compile the
-- function to @\x -> case x of P1 -> B1 ; P2 -> B2 ; ...@. Returns
-- 'Nothing' for clause shapes that don't fit the constructor-dispatch
-- mould (zero-arg, multi-arg, or all-trivial-pattern functions stay on
-- the legacy guard-fold path).
tryCtorDispatch :: Text -> [ETerm] -> Maybe (Either Text ([Text], Expr))
tryCtorDispatch _fname clauses = do
  -- Every clause must have exactly one parameter slot.
  rawPats <- mapM clauseSinglePat clauses
  -- At least one of those patterns must be a constructor pattern.
  if not (any isCtorPat rawPats)
    then Nothing
    else Just $ do
      branches <- mapM clauseToCtorBranch clauses
      let argName = "x"
      pure ([argName], ECase (EVar (Name argName 0)) branches)
  where
    clauseSinglePat (ETTuple [ETAtom "clause", _, ETList [p], _, _]) = Just p
    clauseSinglePat _ = Nothing

    isCtorPat (ETTuple [ETAtom "atom", _, _]) = True
    isCtorPat (ETTuple [ETAtom "tuple", _, ETList kids])
      | (ETTuple [ETAtom "atom", _, _] : _) <- kids = True
    isCtorPat _ = False

    clauseToCtorBranch :: ETerm -> Either Text Branch
    clauseToCtorBranch ct = case ct of
      ETTuple [ETAtom "clause", _, ETList [patT], _guards, ETList bodyTs] -> do
        pat <- translatePattern patT
        body <- translateBody (patVarBindings pat) bodyTs
        pure (Branch pat Nothing body)
      _ -> Left ("tryCtorDispatch: bad clause: " <> T.pack (show ct))

-- | Translate a body: a sequence of expressions returning the last.
translateBody :: [(Text, Text)] -> [ETerm] -> Either Text Expr
translateBody _ [] = Right (ELit (LitInt 0))
translateBody env [e] = translateExpr env e
translateBody env (e:es) = do
  e1 <- translateExpr env e
  e2 <- translateBody env es
  pure (seqExpr e1 e2)

translateExpr :: [(Text, Text)] -> ETerm -> Either Text Expr
translateExpr env t = case t of
  ETTuple [ETAtom "integer", _, ETInt n] ->
    Right (ELit (LitInt n))
  ETTuple [ETAtom "var", _, ETAtom nm] ->
    Right (EVar (Name (lookupName env nm) 0))
  -- Bare atom in expression position. In the Int-only era this was a
  -- variable reference; with ADT support it now becomes a nullary
  -- 'ECon'. The classic Erlang booleans @true@/@false@ are still
  -- mapped to 1/0 so existing arithmetic-style examples keep working.
  ETTuple [ETAtom "atom", _, ETAtom nm]
    | nm == "true"  -> Right (ELit (LitInt 1))
    | nm == "false" -> Right (ELit (LitInt 0))
    | otherwise     -> Right (ECon (mkCtorQName nm))
  ETTuple [ETAtom "op", _, ETAtom op, a, b] -> do
    a' <- translateExpr env a
    b' <- translateExpr env b
    pure (EApp (EVar (Name (mapOp op) 0)) [a', b'])
  ETTuple [ETAtom "op", _, ETAtom op, a] -> do
    a' <- translateExpr env a
    pure (EApp (EVar (Name (mapUnaryOp op) 0)) [a'])
  ETTuple [ETAtom "call", _, callee, ETList args] -> do
    fn <- case callee of
      ETTuple [ETAtom "atom", _, ETAtom nm] ->
        Right (EVar (Name (renameMain nm) 0))
      _ -> translateExpr env callee
    args' <- mapM (translateExpr env) args
    pure (EApp fn args')
  ETTuple [ETAtom "match", _, _pat, rhs] ->
    -- @X = E@ at statement position; we only support the RHS value
    -- (full pattern matching is out of scope).
    translateExpr env rhs
  -- Tagged tuple: {tag_atom, arg1, arg2, ...} -> EApp (ECon tag) [args].
  -- This is the constructor allocation form.
  ETTuple [ETAtom "tuple", _, ETList kids]
    | (ETTuple [ETAtom "atom", _, ETAtom tag] : restKids) <- kids -> do
        args' <- mapM (translateExpr env) restKids
        pure (EApp (ECon (mkCtorQName tag)) args')
  -- case Scrut of P1 -> B1; P2 -> B2; ... end
  ETTuple [ETAtom "case", _, scrutT, ETList clauseTs] -> do
    scrut <- translateExpr env scrutT
    branches <- mapM (translateCaseClause env) clauseTs
    pure (ECase scrut branches)
  other -> Left ("Unsupported Erlang expression: " <> T.pack (show other))
  where
    renameMain "main" = "main_fn"
    renameMain n      = n

-- | Build a constructor QName. The module part is left empty so the
-- 'ConTags' pass keys on the bare name (matches the codegen convention).
mkCtorQName :: Text -> QName
mkCtorQName tag = QName "" (Name tag 0)

-- | Walk all defs and collect every constructor referenced in
-- expressions or patterns, paired with the maximum arity seen at any
-- use site. The result is sorted by ctor name for stable output.
collectErlangCtors :: [Def] -> [(Text, Int)]
collectErlangCtors defs =
  let pairs = concatMap (ctorsInExpr . defExpr) defs
      acc   = foldr (\(n, a) m ->
                       case lookup n m of
                         Just a' -> (n, max a a') : filter ((/= n) . fst) m
                         Nothing -> (n, a) : m)
                    [] pairs
  in sortByName acc
  where
    sortByName = foldr ins []
    ins x [] = [x]
    ins x@(n, _) (y@(m, _) : rest)
      | n <= m    = x : y : rest
      | otherwise = y : ins x rest

ctorsInExpr :: Expr -> [(Text, Int)]
ctorsInExpr = go
  where
    go (ECon qn) = [(nameText (qnameName qn), 0)]
    go (EApp (ECon qn) args) =
      (nameText (qnameName qn), length args) : concatMap go args
    go (EApp f args) = go f ++ concatMap go args
    go (ELam _ b) = go b
    go (ELet bgs body) =
      go body ++ [r | bg <- bgs, b <- bg, r <- go (bindExpr b)]
    go (ECase scrut bs) =
      go scrut ++ concatMap branchCtors bs
    go (ETypeApp e _) = go e
    go (ETypeLam _ e) = go e
    go (EPerform _ args) = concatMap go args
    go (EHandle _ h b) = go h ++ go b
    go (ERetain e) = go e
    go (ERelease e) = go e
    go (EDrop e) = go e
    go (EReuse a b) = go a ++ go b
    go (EDelay e) = go e
    go (EForce e) = go e
    go _ = []

    branchCtors (Branch p mg body) =
      patCtors p ++ maybe [] go mg ++ go body

    patCtors (PatCon qn subs) =
      (nameText (qnameName qn), length subs) : concatMap patCtors subs
    patCtors _ = []

-- | Translate a single @case@ clause into a Frankenstein 'Branch'. The
-- clause carries one pattern (since the scrutinee is a single value)
-- and a body.
translateCaseClause :: [(Text, Text)] -> ETerm -> Either Text Branch
translateCaseClause env clauseT = case clauseT of
  ETTuple [ETAtom "clause", _, ETList [patT], _guards, ETList bodyTs] -> do
    pat <- translatePattern patT
    let envWithPat = env ++ patVarBindings pat
    body <- translateBody envWithPat bodyTs
    pure (Branch pat Nothing body)
  other -> Left ("translateCaseClause: bad clause: " <> T.pack (show other))

-- | Translate an Erlang pattern AST node into a Frankenstein 'Pattern'.
-- Recognized forms:
--
--   * @{var, _, X}@               → 'PatVar'
--   * @{integer, _, N}@           → 'PatLit'
--   * @{atom, _, nm}@             → 'PatCon' nm []   (nullary ctor)
--   * @{tuple, _, [{atom,...}+]}@ → 'PatCon' tag subs (n-ary ctor)
--
-- Variables get a fresh-ish lower-cased name to mirror the rest of
-- the bridge's naming convention.
translatePattern :: ETerm -> Either Text Pattern
translatePattern p = case p of
  ETTuple [ETAtom "var", _, ETAtom nm] ->
    Right (PatVar (Name (T.toLower nm) 0) intT)
  ETTuple [ETAtom "integer", _, ETInt n] ->
    Right (PatLit (LitInt n))
  ETTuple [ETAtom "atom", _, ETAtom nm] ->
    Right (PatCon (mkCtorQName nm) [])
  ETTuple [ETAtom "tuple", _, ETList kids]
    | (ETTuple [ETAtom "atom", _, ETAtom tag] : restKids) <- kids -> do
        subs <- mapM translatePattern restKids
        pure (PatCon (mkCtorQName tag) subs)
  other -> Left ("translatePattern: unsupported: " <> T.pack (show other))

-- | Walk a translated pattern and collect the (orig, translated) name
-- pairs of every 'PatVar' inside it. Pass these into 'translateBody' so
-- the body's variable references resolve to the bound names.
patVarBindings :: Pattern -> [(Text, Text)]
patVarBindings (PatVar n _)   = [(nameText n, nameText n)]
patVarBindings (PatCon _ subs) = concatMap patVarBindings subs
patVarBindings (PatWild _)    = []
patVarBindings (PatLit _)     = []

-- | Erlang identifiers are case-sensitive variables; we normalised
-- them via 'synthParamName' at the clause level. We still look up in
-- case a user-side name slips through.
lookupName :: [(Text, Text)] -> Text -> Text
lookupName env nm = case lookup nm env of
  Just mapped -> mapped
  Nothing     -> T.toLower nm

seqExpr :: Expr -> Expr -> Expr
seqExpr e1 e2 =
  ELet [[Bind (Name "_seq" 0) intT e1 DefVal]] e2

-- | Map Erlang binary operator atoms to Frankenstein primitives.
mapOp :: Text -> Text
mapOp "+"   = "+"
mapOp "-"   = "-"
mapOp "*"   = "*"
mapOp "div" = "/"
mapOp "rem" = "mod"
mapOp "=="  = "=="
mapOp "/="  = "/="
mapOp "=<"  = "<="
mapOp "<"   = "<"
mapOp ">"   = ">"
mapOp ">="  = ">="
mapOp "=:=" = "=="
mapOp "=/=" = "/="
mapOp o     = o

mapUnaryOp :: Text -> Text
mapUnaryOp "-" = "negate"
mapUnaryOp "+" = "id"
mapUnaryOp o   = o

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
