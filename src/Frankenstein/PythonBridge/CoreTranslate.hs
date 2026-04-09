-- | Python AST → Frankenstein Core translation.
--
-- Consumes the 'PySExpr' produced by 'AstParse' and emits a 'Program'.
-- Python is dynamically typed; we type every value as @int@ for the
-- subset we currently support (integer arithmetic, comparisons, ints
-- as truthiness). Booleans use the standard 0/1 convention.
--
-- Effects: Python's @raise@ would map to the @exn@ effect, but the
-- supported subset is exception-free, so all definitions are pure.
-- Multiplicity is 'Many' (Python is GC'd / reference-shared).

module Frankenstein.PythonBridge.CoreTranslate
  ( translatePythonAst
  , translatePyModule
  ) where

import Frankenstein.Core.Types
import Frankenstein.PythonBridge.AstParse

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set

-- | Translate a Python module S-expression into a Frankenstein 'Program'.
--
-- Two-phase: first walk top-level statements collecting 'ClassDef' nodes
-- into 'DataDecl's, then translate 'FunctionDef's with the resulting class
-- table in scope so that @Foo(...)@ calls become 'ECon' applications and
-- @obj.field@ becomes a selector call.
translatePythonAst :: Text -> PySExpr -> Either Text Program
translatePythonAst modName sexpr = do
  (datas, fnStmts) <- splitTopLevels sexpr
  let classNames = Set.fromList [ qnameName (dataName d) | d <- datas
                                                         , let _ = nameText ]
      classNameTexts = Set.map nameText classNames
  defs <- mapM (translateTopLevel modName classNameTexts) fnStmts
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = defs
    , progData    = datas
    , progEffects = []
    }

-- | Walk the @(Module ...)@ form and split top-level statements into
-- ('DataDecl's from ClassDefs) and (FunctionDef nodes still to translate).
splitTopLevels :: PySExpr -> Either Text ([DataDecl], [PySExpr])
splitTopLevels (SList (SAtom "Module" : stmts)) = go stmts [] []
  where
    go []     ds fs = pure (reverse ds, reverse fs)
    go (s:ss) ds fs = case s of
      SList [SAtom "ClassDef", SStr name, SList rawFields] -> do
        fieldNames <- mapM expectStr rawFields
        let con = ConDecl
              { conName   = QName T.empty (Name name 0)
              , conFields = [ (Name f 0, intT) | f <- fieldNames ]
              , conVis    = Public
              }
            dd = DataDecl
              { dataName   = QName T.empty (Name name 0)
              , dataParams = []
              , dataCons   = [con]
              , dataVis    = Public
              }
        go ss (dd : ds) fs
      _ -> go ss ds (s : fs)
splitTopLevels other =
  Left $ "Expected (Module ...), got: " <> T.pack (show other)

-- | Translate a deferred top-level statement; only FunctionDefs are
-- expected here (ClassDefs were lifted out by 'splitTopLevels').
translateTopLevel :: Text -> Set.Set Text -> PySExpr -> Either Text Def
translateTopLevel modName classes (SList [SAtom "FunctionDef", SStr name, SList params, SList body]) = do
  paramNames <- mapM expectStr params
  let pNames = [(Name p 0, intT) | p <- paramNames]
      argTypes = [(Many, intT) | _ <- paramNames]
      fnTy = TFun argTypes EffectRowEmpty intT
  bodyExpr <- translateBlock classes body
  let lam = if null pNames then bodyExpr else ELam pNames bodyExpr
  pure Def
    { defName       = QName modName (Name name 0)
    , defType       = fnTy
    , defExpr       = lam
    , defSort       = DefFun
    , defVisibility = Public
    }
translateTopLevel _ _ other =
  Left $ "Top-level statement must be a FunctionDef, got: "
       <> T.pack (showHead other)

-- | Backwards-compatible wrapper used by the test suite: assumes no
-- classes are present, just translates FunctionDefs.
translatePyModule :: Text -> PySExpr -> Either Text [Def]
translatePyModule modName sexpr = do
  (datas, fnStmts) <- splitTopLevels sexpr
  let classNames = Set.fromList [ nameText (qnameName (dataName d)) | d <- datas ]
  mapM (translateTopLevel modName classNames) fnStmts

-- | Translate a sequence of Python statements to a single Core expression.
--
-- Statement-to-expression strategy:
--
--   * @return e@      — produces the value @e@ for the enclosing block.
--   * @x = e ; rest@  — @let x = e in <rest>@.
--   * @if t: a [else: b] ; rest@  — if both branches always end in a
--     @return@, the @rest@ is ignored. Otherwise, the @if@ becomes a
--     case where the missing branch falls through to @rest@. This handles
--     the canonical Python "early return" pattern:
--
--         if n <= 1:
--             return 1
--         return n * factorial(n - 1)
--
--     becomes @case (n <= 1) of 0 -> n*factorial(n-1) ; _ -> 1@.
--
-- The @rest@ is duplicated into both arms when needed; this is acceptable
-- for small programs and matches what GHC's desugarer does for simple
-- early-return patterns.
translateBlock :: Set.Set Text -> [PySExpr] -> Either Text Expr
translateBlock _       []  = Right (ELit (LitInt 0))
translateBlock classes [s] = translateStmtTail classes s
translateBlock classes (s:ss) = case s of
  SList [SAtom "Return", _] -> translateStmtTail classes s  -- ss is dead code

  SList [SAtom "Assign", SStr nm, valE] -> do
    val  <- translateExpr classes valE
    rest <- translateBlock classes ss
    pure $ ELet [[Bind (Name nm 0) intT val DefVal]] rest

  SList [SAtom "ExprStmt", e] -> do
    eExpr <- translateExpr classes e
    rest  <- translateBlock classes ss
    pure $ ELet [[Bind (Name "_" 0) intT eExpr DefVal]] rest

  SList [SAtom "If", testE, SList thenStmts, SList elseStmts] -> do
    test  <- translateExpr classes testE
    rest  <- translateBlock classes ss
    -- Each branch uses its own statements; an empty branch falls through
    -- to the rest of the enclosing block.
    thenE <- if null thenStmts then pure rest else translateBlock classes thenStmts
    elseE <- if null elseStmts then pure rest else translateBlock classes elseStmts
    -- case test of 0 -> elseE ; _ -> thenE
    pure $ ECase test
      [ Branch (PatLit (LitInt 0)) Nothing elseE
      , Branch (PatWild intT)      Nothing thenE
      ]

  _ -> translateBlock classes ss  -- skip unsupported non-final stmts silently

-- | Translate a statement that is the *last* statement in a block.
translateStmtTail :: Set.Set Text -> PySExpr -> Either Text Expr
translateStmtTail classes s = case s of
  SList [SAtom "Return", SList [SAtom "None"]] ->
    Right (ELit (LitInt 0))
  SList [SAtom "Return", e] -> translateExpr classes e

  SList [SAtom "If", testE, SList thenStmts, SList elseStmts] -> do
    test  <- translateExpr classes testE
    thenE <- translateBlock classes thenStmts
    elseE <- translateBlock classes elseStmts
    pure $ ECase test
      [ Branch (PatLit (LitInt 0)) Nothing elseE
      , Branch (PatWild intT)      Nothing thenE
      ]

  SList [SAtom "Assign", SStr nm, valE] -> do
    val <- translateExpr classes valE
    pure $ ELet [[Bind (Name nm 0) intT val DefVal]] (EVar (Name nm 0))

  SList [SAtom "ExprStmt", e] -> translateExpr classes e

  other -> Left $ "Unsupported tail statement: " <> T.pack (showHead other)

-- | Translate a Python expression node to a Core 'Expr'. The @classes@
-- set carries the names of record classes in scope, so @Foo(...)@ becomes
-- a constructor application instead of a plain function call.
translateExpr :: Set.Set Text -> PySExpr -> Either Text Expr
translateExpr _ (SList [SAtom "Constant", SInt n]) = Right (ELit (LitInt n))
translateExpr _ (SList [SAtom "ConstantBool", SInt n]) = Right (ELit (LitInt n))
translateExpr _ (SList [SAtom "ConstantStr", SStr s]) = Right (ELit (LitString s))
translateExpr _ (SList [SAtom "Name", SStr nm]) = Right (EVar (Name nm 0))

translateExpr classes (SList [SAtom "BinOp", SStr op, lE, rE]) = do
  l <- translateExpr classes lE
  r <- translateExpr classes rE
  binOpFn <- pyBinOp op
  pure $ EApp (EVar (Name binOpFn 0)) [l, r]

translateExpr classes (SList [SAtom "UnaryOp", SStr "USub", inner]) = do
  v <- translateExpr classes inner
  pure $ EApp (EVar (Name "negate" 0)) [v]
translateExpr _ (SList [SAtom "UnaryOp", SStr op, _]) =
  Left $ "Unsupported unary op: " <> op

translateExpr classes (SList [SAtom "Compare", lE, SStr op, rE]) = do
  l <- translateExpr classes lE
  r <- translateExpr classes rE
  cmpFn <- pyCmpOp op
  pure $ EApp (EVar (Name cmpFn 0)) [l, r]

-- Attribute access: obj.field becomes a selector call (the auto-derived
-- selector function is generated by Frankenstein.Core.DeriveSelectors).
translateExpr classes (SList [SAtom "Attr", objE, SStr field]) = do
  obj <- translateExpr classes objE
  pure $ EApp (EVar (Name field 0)) [obj]

translateExpr classes (SList [SAtom "Call", SStr fname, SList args]) = do
  argEs <- mapM (translateExpr classes) args
  if Set.member fname classes
    then pure $ EApp (ECon (QName T.empty (Name fname 0))) argEs
    else pure $ EApp (EVar (Name fname 0)) argEs

translateExpr _ other =
  Left $ "Unsupported expression: " <> T.pack (showHead other)

-- ---------------------------------------------------------------------------
-- Operator name mapping. We translate Python operators to canonical
-- Frankenstein primitive names that the MLIR emitter recognises (the same
-- names used by the GHC-bridge int primops, e.g. "primAddInt").

-- Map Python operator names to the canonical primitive names recognised
-- by the MLIR emitter (see Emitter.hs integer-binop dispatch).
pyBinOp :: Text -> Either Text Text
pyBinOp "Add"      = Right "+"
pyBinOp "Sub"      = Right "-"
pyBinOp "Mult"     = Right "*"
pyBinOp "FloorDiv" = Right "/"
pyBinOp "Mod"      = Right "mod"
pyBinOp op         = Left $ "Unsupported binary operator: " <> op

pyCmpOp :: Text -> Either Text Text
pyCmpOp "Eq"    = Right "=="
pyCmpOp "NotEq" = Right "/="
pyCmpOp "Lt"    = Right "<"
pyCmpOp "LtE"   = Right "<="
pyCmpOp "Gt"    = Right ">"
pyCmpOp "GtE"   = Right ">="
pyCmpOp op      = Left $ "Unsupported comparison operator: " <> op

-- ---------------------------------------------------------------------------
-- Helpers

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

expectStr :: PySExpr -> Either Text Text
expectStr (SStr s) = Right s
expectStr other    = Left $ "Expected string, got: " <> T.pack (show other)

-- | Best-effort short description of a node for error messages.
showHead :: PySExpr -> String
showHead (SList (SAtom h : _)) = "(" <> T.unpack h <> " ...)"
showHead (SList _) = "(<list>)"
showHead (SAtom a) = T.unpack a
showHead (SStr s)  = "\"" <> T.unpack s <> "\""
showHead (SInt n)  = show n
