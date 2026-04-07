-- | Go AST → Frankenstein Core translation.
--
-- Consumes the 'GoSExpr' produced by 'AstParse' and emits a 'Program'.
-- Currently supports the integer-arithmetic / recursive-function subset:
-- @func@ declarations, @return@, @if@/@else@, integer literals,
-- identifiers, calls, binary ops, unary ops, simple assignment.
--
-- Goroutines, channels, interfaces, methods, structs, and slices are
-- explicitly NOT supported in this initial bridge — see ROADMAP for the
-- planned scheduler/runtime work.
--
-- Operator names produced by @go/token@ (e.g. \"+\", \"<=\") align with
-- the canonical primitives recognised by 'Frankenstein.MlirEmit.Emitter',
-- so no special-casing is needed.

module Frankenstein.GoBridge.CoreTranslate
  ( translateGoAst
  , translateGoFile
  ) where

import Frankenstein.Core.Types
import Frankenstein.GoBridge.AstParse

import Data.Text (Text)
import qualified Data.Text as T

translateGoAst :: GoSExpr -> Either Text Program
translateGoAst sx = case sx of
  SList (SAtom "Module" : SStr modName : SList decls : _) -> do
    defs <- mapM (translateDecl modName) decls
    pure Program
      { progName    = QName modName (Name "main" 0)
      , progDefs    = concat defs
      , progData    = []
      , progEffects = []
      }
  other -> Left $ "Expected (Module name (decls...)), got: " <> T.pack (showHead other)

translateGoFile :: Text -> GoSExpr -> Either Text Program
translateGoFile _ = translateGoAst

translateDecl :: Text -> GoSExpr -> Either Text [Def]
translateDecl _ (SList [SAtom "Skip"]) = pure []  -- imports etc.
translateDecl modName
  (SList [SAtom "FuncDecl", SStr name, SList params, body]) = do
    paramNames <- mapM expectStr params
    let pNames = [(Name p 0, intT) | p <- paramNames]
        argTypes = [(Many, intT) | _ <- paramNames]
        fnTy = TFun argTypes EffectRowEmpty intT
    bodyExpr <- translateBlock body
    let lam = if null pNames then bodyExpr else ELam pNames bodyExpr
    pure [Def
      { defName       = QName modName (Name name 0)
      , defType       = fnTy
      , defExpr       = lam
      , defSort       = DefFun
      , defVisibility = Public
      }]
translateDecl _ other =
  Left $ "Unsupported top-level Go decl: " <> T.pack (showHead other)

-- | A @(Block (stmts...))@ becomes a single Core expression.
translateBlock :: GoSExpr -> Either Text Expr
translateBlock (SList [SAtom "Block", SList stmts]) = translateStmts stmts
translateBlock other =
  Left $ "Expected (Block ...), got: " <> T.pack (showHead other)

-- | Statement-to-expression strategy mirrors the Python bridge: an
-- @if@ followed by more statements becomes a case where the missing
-- branch falls through to the rest. Used for the canonical Go
-- early-return:
--
-- @
--   if n <= 1 { return 1 }
--   return n * factorial(n-1)
-- @
translateStmts :: [GoSExpr] -> Either Text Expr
translateStmts []  = Right (ELit (LitInt 0))
translateStmts [s] = translateStmtTail s
translateStmts (s:ss) = case s of
  SList [SAtom "Return", _] -> translateStmtTail s

  SList [SAtom "Assign", SStr nm, valE] -> do
    val  <- translateExpr valE
    rest <- translateStmts ss
    pure $ ELet [[Bind (Name nm 0) intT val DefVal]] rest

  SList [SAtom "ExprStmt", e] -> do
    eExpr <- translateExpr e
    rest  <- translateStmts ss
    pure $ ELet [[Bind (Name "_" 0) intT eExpr DefVal]] rest

  SList [SAtom "If", testE, thenBlk, elseBlk] -> do
    test  <- translateExpr testE
    rest  <- translateStmts ss
    thenE <- translateBranchOrFallthrough thenBlk rest
    elseE <- translateBranchOrFallthrough elseBlk rest
    pure $ ECase test
      [ Branch (PatLit (LitInt 0)) Nothing elseE
      , Branch (PatWild intT)      Nothing thenE
      ]

  _ -> translateStmts ss

-- | Translate the *final* statement of a block — must produce a value.
translateStmtTail :: GoSExpr -> Either Text Expr
translateStmtTail s = case s of
  SList [SAtom "Return", SList [SAtom "None"]] -> Right (ELit (LitInt 0))
  SList [SAtom "Return", e] -> translateExpr e

  SList [SAtom "If", testE, thenBlk, elseBlk] -> do
    test  <- translateExpr testE
    thenE <- translateBlock thenBlk
    elseE <- translateBlock elseBlk
    pure $ ECase test
      [ Branch (PatLit (LitInt 0)) Nothing elseE
      , Branch (PatWild intT)      Nothing thenE
      ]

  SList [SAtom "Assign", SStr nm, valE] -> do
    val <- translateExpr valE
    pure $ ELet [[Bind (Name nm 0) intT val DefVal]] (EVar (Name nm 0))

  SList [SAtom "ExprStmt", e] -> translateExpr e

  other -> Left $ "Unsupported tail statement: " <> T.pack (showHead other)

-- | Translate a (Block ...) used as an if-arm; if the block is empty
-- (e.g. missing else), fall through to the supplied continuation.
translateBranchOrFallthrough :: GoSExpr -> Expr -> Either Text Expr
translateBranchOrFallthrough (SList [SAtom "Block", SList []]) fallthrough =
  Right fallthrough
translateBranchOrFallthrough blk _ = translateBlock blk

-- | Translate an expression node.
translateExpr :: GoSExpr -> Either Text Expr
translateExpr (SList [SAtom "Constant", SInt n]) = Right (ELit (LitInt n))
translateExpr (SList [SAtom "Name", SStr nm])    = Right (EVar (Name nm 0))

translateExpr (SList [SAtom "BinOp", SStr op, lE, rE]) = do
  l <- translateExpr lE
  r <- translateExpr rE
  fn <- goBinOp op
  pure $ EApp (EVar (Name fn 0)) [l, r]

translateExpr (SList [SAtom "UnaryOp", SStr "-", inner]) = do
  v <- translateExpr inner
  pure $ EApp (EVar (Name "negate" 0)) [v]
translateExpr (SList [SAtom "UnaryOp", SStr op, _]) =
  Left $ "Unsupported Go unary op: " <> op

translateExpr (SList [SAtom "Call", SStr fname, SList args]) = do
  argEs <- mapM translateExpr args
  pure $ EApp (EVar (Name fname 0)) argEs

translateExpr other =
  Left $ "Unsupported Go expression: " <> T.pack (showHead other)

-- ---------------------------------------------------------------------------
-- Operator name mapping. The Go @token@ package's String() representation
-- already matches the canonical primitive names recognised by the MLIR
-- emitter for arithmetic / comparison ops, so the mapping is mostly
-- identity. We list it explicitly so unsupported ops produce a clear error.

goBinOp :: Text -> Either Text Text
goBinOp "+"  = Right "+"
goBinOp "-"  = Right "-"
goBinOp "*"  = Right "*"
goBinOp "/"  = Right "/"
goBinOp "%"  = Right "mod"
goBinOp "==" = Right "=="
goBinOp "!=" = Right "/="
goBinOp "<"  = Right "<"
goBinOp "<=" = Right "<="
goBinOp ">"  = Right ">"
goBinOp ">=" = Right ">="
goBinOp "&"  = Right "andI#"
goBinOp "|"  = Right "orI#"
goBinOp "^"  = Right "xorI#"
goBinOp op   = Left $ "Unsupported Go binary operator: " <> op

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

expectStr :: GoSExpr -> Either Text Text
expectStr (SStr s) = Right s
expectStr other    = Left $ "Expected string, got: " <> T.pack (show other)

showHead :: GoSExpr -> String
showHead (SList (SAtom h : _)) = "(" <> T.unpack h <> " ...)"
showHead (SList _) = "(<list>)"
showHead (SAtom a) = T.unpack a
showHead (SStr s)  = "\"" <> T.unpack s <> "\""
showHead (SInt n)  = show n
