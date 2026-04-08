-- | OCaml parsetree → Frankenstein Core.
--
-- Supported subset (mirroring the Swift/Scheme bridges):
--
--   * @let [rec] name x y ... = expr@ at the top level, Int-only.
--   * Expressions: integer literals, variable refs, binary ops
--     (@+ - * / = <> < <= > >=@), @if e1 then e2 else e3@,
--     @fun x -> body@, function application.
--   * @let () = print_int (expr)@ at the end of the file becomes the
--     body of @main@.
--
-- Modules, functors, GADTs, records, pattern matching beyond simple
-- variables, and anything involving the OCaml stdlib beyond
-- @print_int@ are out of scope for this first bridge. The point is
-- "an eighth language through the Int pipeline," not an OCaml
-- implementation.
module Frankenstein.OCamlBridge.CoreTranslate
  ( translateOCaml
  ) where

import Frankenstein.Core.Types
import Frankenstein.OCamlBridge.AstParse

import Data.Text (Text)
import qualified Data.Text as T

-- | Translate a parsed OCaml parsetree root into a Program.
translateOCaml :: Text -> OLine -> Either Text Program
translateOCaml modName root = do
  -- The file-level outer brackets become a <list> at the root.
  let topLists = findHeads "<list>" root
      directItems = findHeads "structure_item" root
      items = case topLists of
        (l:_) -> findHeads "structure_item" l
        []    -> directItems
  (defs, mMain) <- foldDefs modName items [] Nothing
  let mainDef = case mMain of
        Nothing -> []
        Just e ->
          [ Def
              { defName       = QName modName (Name "main" 0)
              , defType       = TFun [] EffectRowEmpty intT
              , defExpr       = ELam [] e
              , defSort       = DefFun
              , defVisibility = Public
              }
          ]
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = defs ++ mainDef
    , progData    = []
    , progEffects = []
    }

foldDefs
  :: Text -> [OLine] -> [Def] -> Maybe Expr
  -> Either Text ([Def], Maybe Expr)
foldDefs _ [] accDefs accMain = Right (reverse accDefs, accMain)
foldDefs modName (it:rest) accDefs accMain = do
  r <- translateStructureItem modName it
  case r of
    Left d  -> foldDefs modName rest (d : accDefs) accMain
    Right e ->
      let accMain' = case accMain of
            Nothing   -> Just e
            Just prev -> Just (seqExpr prev e)
      in foldDefs modName rest accDefs accMain'

-- | A structure_item is either a named let (returns Left Def) or a
-- unit-bound effectful @let () = ...@ (returns Right Expr).
translateStructureItem :: Text -> OLine -> Either Text (Either Def Expr)
translateStructureItem modName item = do
  -- structure_item children: [Pstr_value Rec/Nonrec, <list>]
  -- <list> children: [<def>, <def>, ...]
  -- <def> children: [pattern, expression]
  let lists = filter (T.isPrefixOf "<list>" . oHead) (oChildren item)
      defs  = concatMap (filter (T.isPrefixOf "<def>" . oHead) . oChildren) lists
      allKids = case defs of
        (d:_) -> oChildren d
        []    -> []
  pat <- case filter (T.isPrefixOf "pattern" . oHead) allKids of
           (p:_) -> Right p
           []    -> Left "structure_item: no pattern"
  expr <- case filter (T.isPrefixOf "expression" . oHead) allKids of
           (e:_) -> Right e
           []    -> Left "structure_item: no expression"
  patName <- patternName pat
  case patName of
    Nothing -> do
      -- let () = <expr>: treat <expr> as a main-ish statement.
      e <- translateExpr expr
      pure (Right (unwrapPrintInt e))
    Just nm -> do
      (params, body) <- peelParams expr
      body' <- translateExpr body
      let lamParams = [(Name p 0, intT) | p <- params]
          argTypes  = [(Many, intT)     | _ <- params]
          fnTy      = if null params
                      then intT
                      else TFun argTypes EffectRowEmpty intT
          defExpr'  = if null params
                      then body'
                      else ELam lamParams body'
      pure $ Left Def
        { defName       = QName modName (Name nm 0)
        , defType       = fnTy
        , defExpr       = defExpr'
        , defSort       = if null params then DefVal else DefFun
        , defVisibility = Public
        }

-- | Extract the variable name from a pattern line. Returns 'Nothing'
-- for unit pattern @()@.
patternName :: OLine -> Either Text (Maybe Text)
patternName pat = case oChildren pat of
  (c:_) -> case oHead c of
    h | "Ppat_var" `T.isPrefixOf` h ->
        case oString c of
          Just s  -> Right (Just s)
          Nothing -> Left "Ppat_var: missing name"
    h | "Ppat_construct" `T.isPrefixOf` h ->
        case oString c of
          Just "()" -> Right Nothing
          _         -> Left "unsupported Ppat_construct"
    h -> Left $ "unsupported pattern: " <> h
  [] -> Left "empty pattern"

-- | Peel off curried @Pexp_function@ layers, returning parameter
-- names and the final body expression.
--
-- In the ocamlc parsetree dump, an @expression@ whose head is
-- @Pexp_function@ has as children (at the same indent as the
-- @Pexp_function@ marker): the marker itself, the @Pparam_val@ list,
-- an optional type constraint (@None@/@Some@), and @Pfunction_body@
-- with the body expression.
peelParams :: OLine -> Either Text ([Text], OLine)
peelParams = go []
  where
    go acc e =
      let kids = oChildren e
          isFn = any (T.isPrefixOf "Pexp_function" . oHead) kids
      in if isFn
         then do
           -- Params live inside the first <list> sibling of Pexp_function.
           let listKids = case filter (T.isPrefixOf "<list>" . oHead) kids of
                            (l:_) -> oChildren l
                            []    -> []
               params = [ paramVarName pp
                        | pp <- listKids
                        , "Pparam_val" `T.isPrefixOf` oHead pp
                        ]
               mBody = case [ b | bd <- kids
                                , "Pfunction_body" `T.isPrefixOf` oHead bd
                                , b <- take 1 (filter (T.isPrefixOf "expression" . oHead) (oChildren bd))
                                ] of
                         (b:_) -> Just b
                         []    -> Nothing
           case mBody of
             Just b  -> go (acc ++ params) b
             Nothing -> Left "Pexp_function: no body"
         else Right (acc, e)

paramVarName :: OLine -> Text
paramVarName pp =
  let pats = filter (T.isPrefixOf "pattern" . oHead) (oChildren pp)
  in case pats of
       (p:_) -> case oChildren p of
         (v:_) | "Ppat_var" `T.isPrefixOf` oHead v ->
           case oString v of
             Just s  -> s
             Nothing -> "_"
         _ -> "_"
       _ -> "_"

-- | Expression translation. The parent @expression@ node has as its
-- children the constructor marker (e.g. @Pexp_apply@) plus all the
-- constructor's argument nodes as siblings.
translateExpr :: OLine -> Either Text Expr
translateExpr e = case oChildren e of
  [] -> Left $ "expression: no children (" <> oHead e <> ")"
  kids@(c:_)
    | "Pexp_function" `T.isPrefixOf` oHead c -> do
        (params, body) <- peelParams e
        body' <- translateExpr body
        pure (ELam [(Name p 0, intT) | p <- params] body')
    | "Pexp_apply" `T.isPrefixOf` oHead c -> do
        -- After the Pexp_apply marker: first expression child is the
        -- callee, then a <list> contains <arg>/Nolabel/expression
        -- triples at the same indent level. We collect every
        -- expression found under a label marker inside the list.
        let exprs = filter (T.isPrefixOf "expression" . oHead) kids
        fn <- case exprs of
          (f:_) -> translateExpr f
          _     -> Left "Pexp_apply: no callee expression"
        let argListKids = case filter (T.isPrefixOf "<list>" . oHead) kids of
                            (l:_) -> oChildren l
                            []    -> []
            -- Each arg is a Nolabel (or Labelled "...") node whose
            -- children include the expression.
            labels = filter isLabel argListKids
            argExprs = concatMap (filter (T.isPrefixOf "expression" . oHead) . oChildren) labels
        args' <- mapM translateExpr argExprs
        pure (EApp fn args')
    | "Pexp_constant" `T.isPrefixOf` oHead c -> do
        -- `constant` node is a sibling of Pexp_constant at the
        -- expression level; its PConst_int child carries the value.
        let consts = filter (T.isPrefixOf "constant" . oHead) kids
        case consts of
          (k:_) -> case oChildren k of
            (x:_) | "PConst_int" `T.isPrefixOf` oHead x ->
              case oInt x of
                Just n  -> Right (ELit (LitInt n))
                Nothing -> Left "PConst_int: bad value"
            _ -> Left "Pexp_constant: no PConst_int"
          _ -> Left "Pexp_constant: no constant sibling"

    | "Pexp_ifthenelse" `T.isPrefixOf` oHead c -> do
        let exprs = filter (T.isPrefixOf "expression" . oHead) kids
        case exprs of
          (condE : thenE : rest) -> do
            cond' <- translateExpr condE
            then' <- translateExpr thenE
            else' <- case rest of
              (eE:_) -> translateExpr eE
              [] ->
                case filter (T.isPrefixOf "Some" . oHead) kids of
                  (s:_) -> case filter (T.isPrefixOf "expression" . oHead) (oChildren s) of
                    (x:_) -> translateExpr x
                    _     -> Right (ELit (LitInt 0))
                  _ -> Right (ELit (LitInt 0))
            pure $ ECase cond'
              [ Branch (PatLit (LitInt 0)) Nothing else'
              , Branch (PatWild intT)      Nothing then'
              ]
          _ -> Left "Pexp_ifthenelse: missing branches"
    | otherwise -> translateExprHead c

translateExprHead :: OLine -> Either Text Expr
translateExprHead c
  | "Pexp_constant" `T.isPrefixOf` h = do
      let cs = filter (T.isPrefixOf "constant" . oHead) (oChildren c)
      case cs of
        (k:_) -> case oChildren k of
          (x:_) | "PConst_int" `T.isPrefixOf` oHead x ->
            case oInt x of
              Just n  -> Right (ELit (LitInt n))
              Nothing -> Left "PConst_int: bad value"
          _ -> Left "constant: no PConst_int"
        _ -> Left "Pexp_constant: no constant child"

  | "Pexp_ident" `T.isPrefixOf` h = case oString c of
      Just nm -> Right (EVar (Name (mapOp nm) 0))
      Nothing -> Left "Pexp_ident: missing name"

  | otherwise = Left $ "Unsupported OCaml expression head: " <> h
  where
    h = oHead c

-- | Is this line a label marker (Nolabel/Labelled/Optional)?
isLabel :: OLine -> Bool
isLabel l = let h = oHead l
            in  "Nolabel" `T.isPrefixOf` h
             || "Labelled" `T.isPrefixOf` h
             || "Optional" `T.isPrefixOf` h

-- | Desugar @print_int expr@ to just @expr@ so the generated @main@
-- returns the printed value, matching other bridges' conventions.
unwrapPrintInt :: Expr -> Expr
unwrapPrintInt (EApp (EVar (Name "print_int" _)) (e:_)) = e
unwrapPrintInt (EApp (EVar (Name "print_endline" _)) (e:_)) = e
unwrapPrintInt e = e

seqExpr :: Expr -> Expr -> Expr
seqExpr e1 e2 =
  ELet [[Bind (Name "_seq" 0) intT e1 DefVal]] e2

-- | Map OCaml operators to Frankenstein's canonical primitive names.
mapOp :: Text -> Text
mapOp "="  = "=="
mapOp "<>" = "/="
mapOp "mod" = "mod"
mapOp o    = o  -- +, -, *, /, <, <=, >, >= all match directly

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
