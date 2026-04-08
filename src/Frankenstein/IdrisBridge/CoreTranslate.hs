-- | Idris 2 source → Frankenstein Core.
--
-- Int-subset translator that follows the same shape as the other
-- minimal bridges (OCaml, Swift, Scheme): every top-level declaration
-- becomes a 'Def' of type @(Int, ..., Int) -> Int@, and a distinguished
-- @main@ declaration becomes the entry point.
module Frankenstein.IdrisBridge.CoreTranslate
  ( translateIdris
  ) where

import Frankenstein.Core.Types
import Frankenstein.IdrisBridge.Parse

import Data.Text (Text)

translateIdris :: Text -> [IDecl] -> Either Text Program
translateIdris modName decls = do
  defs <- mapM (translateDecl modName) decls
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = defs
    , progData    = []
    , progEffects = []
    }

translateDecl :: Text -> IDecl -> Either Text Def
translateDecl modName IDecl { idName = nm, idParams = ps, idBody = body } = do
  body' <- translateExpr body
  let lamParams = [(Name p 0, intT) | p <- ps]
      argTypes  = [(Many, intT)     | _ <- ps]
      fnTy      = if null ps
                  then TFun [] EffectRowEmpty intT
                  else TFun argTypes EffectRowEmpty intT
      -- Idris allows zero-arg definitions like @main = expr@, but
      -- the Frankenstein pipeline expects @main@ to be a function.
      -- Wrap zero-arg @main@ in an empty lambda.
      defExpr'  = if null ps && nm /= "main"
                  then body'
                  else ELam lamParams body'
      defSort'  = if null ps && nm /= "main" then DefVal else DefFun
  pure Def
    { defName       = QName modName (Name nm 0)
    , defType       = fnTy
    , defExpr       = defExpr'
    , defSort       = defSort'
    , defVisibility = Public
    }

translateExpr :: IExpr -> Either Text Expr
translateExpr e = case e of
  IInt n  -> Right (ELit (LitInt n))
  IVar nm -> Right (EVar (Name nm 0))
  IApp fn args -> do
    fn'   <- translateExpr fn
    args' <- mapM translateExpr args
    pure (EApp fn' args')
  IBin op a b -> do
    a' <- translateExpr a
    b' <- translateExpr b
    pure (EApp (EVar (Name op 0)) [a', b'])
  IIf c t el -> do
    c'  <- translateExpr c
    t'  <- translateExpr t
    el' <- translateExpr el
    pure $ ECase c'
      [ Branch (PatLit (LitInt 0)) Nothing el'
      , Branch (PatWild intT)      Nothing t'
      ]

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
