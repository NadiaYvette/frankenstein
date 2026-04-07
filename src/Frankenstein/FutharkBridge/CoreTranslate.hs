-- | Futhark AST → Frankenstein Core translation.
--
-- Consumes the 'FutModule' produced by 'Frankenstein.FutharkBridge.Parser'
-- and emits a 'Program'. The supported subset is integer arithmetic and
-- recursive functions; arrays / SOACs are explicitly out of scope for the
-- initial bridge (see ROADMAP for the planned linalg/vector dialect work).
--
-- All values are typed as @std.int@ and treated with multiplicity 'Many'.

module Frankenstein.FutharkBridge.CoreTranslate
  ( translateFuthark
  ) where

import Frankenstein.Core.Types
import Frankenstein.FutharkBridge.Parser

import Data.Text (Text)

translateFuthark :: Text -> FutModule -> Either Text Program
translateFuthark modName (FutModule _ defs) = do
  defs' <- mapM (translateDef modName) defs
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = defs'
    , progData    = []
    , progEffects = []
    }

translateDef :: Text -> FutDef -> Either Text Def
translateDef modName (FutDef name params body) = do
  bodyExpr <- translateExpr body
  let pNames   = [(Name p 0, intT) | p <- params]
      argTypes = [(Many, intT)     | _ <- params]
      fnTy     = TFun argTypes EffectRowEmpty intT
      lam      = if null pNames then bodyExpr else ELam pNames bodyExpr
  pure Def
    { defName       = QName modName (Name name 0)
    , defType       = fnTy
    , defExpr       = lam
    , defSort       = DefFun
    , defVisibility = Public
    }

translateExpr :: FutExpr -> Either Text Expr
translateExpr (FELit n)     = Right (ELit (LitInt n))
translateExpr (FEVar nm)    = Right (EVar (Name nm 0))
translateExpr (FENeg e)     = do
  e' <- translateExpr e
  pure $ EApp (EVar (Name "negate" 0)) [e']
translateExpr (FEApp f args) = do
  f'    <- translateExpr f
  args' <- mapM translateExpr args
  pure $ EApp f' args'
translateExpr (FEBin op l r) = do
  l' <- translateExpr l
  r' <- translateExpr r
  fn <- futBinOp op
  pure $ EApp (EVar (Name fn 0)) [l', r']
translateExpr (FEIf c t e) = do
  c' <- translateExpr c
  t' <- translateExpr t
  e' <- translateExpr e
  pure $ ECase c'
    [ Branch (PatLit (LitInt 0)) Nothing e'
    , Branch (PatWild intT)      Nothing t'
    ]
translateExpr (FELet nm rhs body) = do
  rhs'  <- translateExpr rhs
  body' <- translateExpr body
  pure $ ELet [[Bind (Name nm 0) intT rhs' DefVal]] body'

-- | Map Futhark operator tokens to canonical primitive names recognised
-- by 'Frankenstein.MlirEmit.Emitter'.
futBinOp :: Text -> Either Text Text
futBinOp "+"  = Right "+"
futBinOp "-"  = Right "-"
futBinOp "*"  = Right "*"
futBinOp "/"  = Right "/"
futBinOp "%"  = Right "mod"
futBinOp "==" = Right "=="
futBinOp "!=" = Right "/="
futBinOp "<"  = Right "<"
futBinOp "<=" = Right "<="
futBinOp ">"  = Right ">"
futBinOp ">=" = Right ">="
futBinOp "&"  = Right "andI#"
futBinOp "|"  = Right "orI#"
futBinOp "^"  = Right "xorI#"
futBinOp op   = Left $ "Unsupported Futhark binary operator: " <> op

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
