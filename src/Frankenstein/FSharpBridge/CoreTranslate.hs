-- | F# s-expression dump → Frankenstein Core.
--
-- Supported subset (mirroring the OCaml/Scheme/Swift bridges):
--
--   * @module Name@ header.
--   * @let [rec] name arg1 arg2 ... = expr@ at the top level, Int-only.
--   * Expressions: integer literals, variable references, operators
--     (@+ - * / = <> < <= > >=@), @if e1 then e2 else e3@, function
--     application, @let name = ... in body@.
--   * A top-level @let main () = expr@ becomes the @main@ function;
--     its body is the program's result.
--
-- The input is a flat s-expression form produced by the F# bridge's
-- @fsdump.fsx@ walker — see 'Frankenstein.FSharpBridge.Driver'.
module Frankenstein.FSharpBridge.CoreTranslate
  ( translateFSharp
  ) where

import Frankenstein.Core.Types
import Frankenstein.SchemeBridge.Reader

import Data.Text (Text)
import qualified Data.Text as T

translateFSharp :: Text -> [SExpr] -> Either Text Program
translateFSharp modName forms = do
  -- The script emits a single @(module NAME decl1 decl2 ...)@ form.
  (_, decls) <- case forms of
    [SList (SSym "module" : SSym nm : ds)] -> Right (nm, ds)
    _ -> Left ("FSharp: expected a single (module ...) form, got: "
              <> T.pack (show forms))
  defs <- mapM (translateDecl modName) decls
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = defs
    , progData    = []
    , progEffects = []
    }

translateDecl :: Text -> SExpr -> Either Text Def
translateDecl modName decl = case decl of
  SList [SSym "let", SSym name, SList params, body] -> do
    let paramNames = [ p | SSym p <- params ]
    body' <- translateExpr body
    let lamParams = [(Name p 0, intT) | p <- paramNames]
        argTypes  = [(Many, intT)     | _ <- paramNames]
        fnTy      = if null paramNames
                    then TFun [] EffectRowEmpty intT
                    else TFun argTypes EffectRowEmpty intT
        defExpr'  = if null paramNames && name /= "main"
                    then body'
                    else ELam lamParams body'
        defSort'  = if null paramNames && name /= "main"
                    then DefVal
                    else DefFun
    pure Def
      { defName       = QName modName (Name name 0)
      , defType       = fnTy
      , defExpr       = defExpr'
      , defSort       = defSort'
      , defVisibility = Public
      }
  other -> Left ("FSharp: unsupported top-level form: " <> T.pack (show other))

translateExpr :: SExpr -> Either Text Expr
translateExpr e = case e of
  SList [SSym "int", SInt n] -> Right (ELit (LitInt n))
  SList [SSym "int", SSym s] ->
    -- In case the reader split a negative literal oddly.
    case reads (T.unpack s) :: [(Integer, String)] of
      [(n, "")] -> Right (ELit (LitInt n))
      _         -> Left ("FSharp: bad int literal: " <> s)
  SList [SSym "unit"] -> Right (ELit (LitInt 0))
  SList [SSym "var", SSym nm] -> Right (EVar (Name (mapOp nm) 0))
  SList [SSym "var", SInt n] -> Right (ELit (LitInt n))  -- impossible but safe
  SList [SSym "app", f, a] -> do
    f' <- translateExpr f
    a' <- translateExpr a
    pure (curryApp f' [a'])
  SList [SSym "if", c, t, el] -> do
    c' <- translateExpr c
    t' <- translateExpr t
    e' <- translateExpr el
    pure $ ECase c'
      [ Branch (PatLit (LitInt 0)) Nothing e'
      , Branch (PatWild intT)      Nothing t'
      ]
  SList [SSym "letin", SList binds, body] -> do
    body' <- translateExpr body
    foldr wrapBind (Right body') binds
    where
      wrapBind :: SExpr -> Either Text Expr -> Either Text Expr
      wrapBind (SList [SSym "let", SSym nm, SList _params, rhs]) acc = do
        rhs' <- translateExpr rhs
        acc' <- acc
        pure (ELet [[Bind (Name nm 0) intT rhs' DefVal]] acc')
      wrapBind other _ =
        Left ("FSharp letin: bad binding " <> T.pack (show other))
  SList [SSym "seq", e1, e2] -> do
    e1' <- translateExpr e1
    e2' <- translateExpr e2
    pure (ELet [[Bind (Name "_seq" 0) intT e1' DefVal]] e2')
  SList (SSym "str" : _) -> Right (ELit (LitInt 0))  -- format strings: ignored
  other -> Left ("FSharp: unsupported expression: " <> T.pack (show other))

-- | @(app (app f a) b)@ collapses into @f(a, b)@ so that curried F#
-- applications become n-ary Frankenstein applications.
curryApp :: Expr -> [Expr] -> Expr
curryApp (EApp inner prev) trail = curryApp inner (prev ++ trail)
curryApp f args                  = EApp f args

-- | Map F# operator notation to Frankenstein primitive names.
mapOp :: Text -> Text
mapOp "="  = "=="
mapOp "<>" = "/="
mapOp o    = o

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
