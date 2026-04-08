-- | Erlang forms (as produced by @erl_parse:parse_form/1@) →
-- Frankenstein Core.
--
-- Supported subset:
--
--   * @-module(name).@ and @-export([...]).@ attributes (consumed,
--     no semantic effect).
--   * @f(X1, X2, ...) when Guard -> Body; f(X1, X2, ...) -> Body.@
--     with multiple clauses, all Int-typed. Guards compile into
--     nested @if@ chains.
--   * Expressions: integer literals, variable refs, binary ops
--     (@+ - * / rem == /= < =< > >=@), function application
--     @call f([args])@, local calls @f(args)@.
--   * @main()@ (if present) becomes the entry point; its return
--     value is the program's result.
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
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = defs ++ mainDef
    , progData    = []
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
clausesToExpr :: Text -> [ETerm] -> Either Text ([Text], Expr)
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
  ETTuple [ETAtom "atom", _, ETAtom nm] ->
    Right (EVar (Name nm 0))
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
  other -> Left ("Unsupported Erlang expression: " <> T.pack (show other))
  where
    renameMain "main" = "main_fn"
    renameMain n      = n

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
