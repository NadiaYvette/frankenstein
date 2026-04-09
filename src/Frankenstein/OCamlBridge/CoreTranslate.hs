-- | OCaml parsetree → Frankenstein Core.
--
-- Supported subset (mirroring the Swift/Scheme bridges):
--
--   * @let [rec] name x y ... = expr@ at the top level.
--   * Expressions: integer literals, variable refs, binary ops
--     (@+ - * / = <> < <= > >=@), @if e1 then e2 else e3@,
--     @fun x -> body@, function application.
--   * @let () = print_int (expr)@ at the end of the file becomes the
--     body of @main@.
--   * @type t = A | B of int * t * t | ...@ algebraic data type
--     declarations: each variant becomes a 'ConDecl' in a fresh
--     'DataDecl' added to 'progData'. The variant arity is the length
--     of the @Ptype_variant@'s child list.
--   * @C@ / @C (e1, e2, ...)@ constructor expressions translate to
--     'ECon' / @EApp (ECon ...) [args]@.
--   * @match e with | C1 -> b1 | C2 (x, y) -> b2 | _ -> ...@
--     translates to 'ECase' with 'PatCon' branches; nested patterns
--     are flattened later by 'FlattenPatterns'.
--
-- Modules, functors, GADTs, records, and anything involving the OCaml
-- stdlib beyond @print_int@ remain out of scope.
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
  (datas, defs, mMain) <- foldDefs modName items [] [] Nothing
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
    , progData    = datas
    , progEffects = []
    }

foldDefs
  :: Text -> [OLine] -> [DataDecl] -> [Def] -> Maybe Expr
  -> Either Text ([DataDecl], [Def], Maybe Expr)
foldDefs _ [] accData accDefs accMain =
  Right (reverse accData, reverse accDefs, accMain)
foldDefs modName (it:rest) accData accDefs accMain = do
  r <- translateStructureItem modName it
  case r of
    SData ds -> foldDefs modName rest (reverse ds ++ accData) accDefs accMain
    SDef d   -> foldDefs modName rest accData (d : accDefs) accMain
    SMain e  ->
      let accMain' = case accMain of
            Nothing   -> Just e
            Just prev -> Just (seqExpr prev e)
      in foldDefs modName rest accData accDefs accMain'

data StructResult
  = SData [DataDecl]
  | SDef Def
  | SMain Expr

-- | A structure_item is one of: a @type@ declaration (returns SData),
-- a named let (returns SDef), or a unit-bound effectful @let () = ...@
-- (returns SMain).
translateStructureItem :: Text -> OLine -> Either Text StructResult
translateStructureItem modName item
  | any (T.isPrefixOf "Pstr_type" . oHead) (oChildren item) = do
      ds <- translateTypeDecls modName item
      pure (SData ds)
  | otherwise = do
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
      pure (SMain (unwrapPrintInt e))
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
      pure $ SDef Def
        { defName       = QName modName (Name nm 0)
        , defType       = fnTy
        , defExpr       = defExpr'
        , defSort       = if null params then DefVal else DefFun
        , defVisibility = Public
        }

-- | Walk a structure_item containing a Pstr_type and produce one
-- DataDecl per @type_declaration@ entry. We deliberately ignore
-- ptype_params (no type-variable parameters), ptype_manifest, and
-- private/public.
translateTypeDecls :: Text -> OLine -> Either Text [DataDecl]
translateTypeDecls modName item =
  let lists = filter (T.isPrefixOf "<list>" . oHead) (oChildren item)
      tyDecls = concatMap (filter (T.isPrefixOf "type_declaration" . oHead)
                          . oChildren) lists
  in mapM (translateTypeDecl modName) tyDecls

translateTypeDecl :: Text -> OLine -> Either Text DataDecl
translateTypeDecl modName td = do
  tyName <- case oString td of
    Just s  -> Right s
    Nothing -> Left "type_declaration: missing name"
  -- Find a Ptype_variant somewhere in the subtree, then its <list>
  -- of variant entries.
  let variants = case findPtypeVariant td of
        Just v  -> case filter (T.isPrefixOf "<list>" . oHead) (oChildren v) of
          (l:_) -> oChildren l
          []    -> []
        Nothing -> []
  cons <- mapM (translateVariant modName) variants
  pure DataDecl
    { dataName   = QName modName (Name tyName 0)
    , dataParams = []
    , dataCons   = cons
    , dataVis    = Public
    }

-- | The Ptype_variant marker is nested under @ptype_kind =@; walk the
-- subtree until we find it.
findPtypeVariant :: OLine -> Maybe OLine
findPtypeVariant l =
  case filter (T.isPrefixOf "Ptype_variant" . oHead) (oChildren l) of
    (v:_) -> Just v
    []    -> firstJust findPtypeVariant (oChildren l)
  where
    firstJust _ [] = Nothing
    firstJust f (x:xs) = case f x of
      Just r  -> Just r
      Nothing -> firstJust f xs

-- | A variant entry is a location-only line whose children are:
-- @"Name"@, optionally a @<list>@ of core_type fields (or a literal
-- @[]@ for nullary), and a @None@/@Some@ marker for GADT-style result.
translateVariant :: Text -> OLine -> Either Text ConDecl
translateVariant modName v = do
  ctorName <- case [ s | c <- oChildren v
                       , Just s <- [oString c]
                       , "\"" `T.isPrefixOf` oHead c ] of
    (n:_) -> Right n
    []    -> Left "variant: missing constructor name"
  -- Field core_types live inside the first <list> child (if any).
  let fieldTypes = case filter (T.isPrefixOf "<list>" . oHead) (oChildren v) of
        (l:_) -> filter (T.isPrefixOf "core_type" . oHead) (oChildren l)
        []    -> []
  pure ConDecl
    { conName   = QName modName (Name ctorName 0)
    , conFields = [ (Name ("f" <> T.pack (show i)) 0, intT)
                  | (i, _) <- zip [0 :: Int ..] fieldTypes ]
    , conVis    = Public
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

    | "Pexp_construct" `T.isPrefixOf` oHead c -> do
        -- Children of the expression: Pexp_construct "Name", then
        -- None or Some <expression>. Some's expression may itself
        -- be a Pexp_tuple holding multiple args.
        nm <- case oString c of
          Just s  -> Right s
          Nothing -> Left "Pexp_construct: missing name"
        if nm == "()"
          then Right (ELit (LitInt 0))
          else do
            let con = ECon (QName "" (Name nm 0))
            -- Look for a Some carrying argument expressions.
            argExprs <- case filter (T.isPrefixOf "Some" . oHead) kids of
              (s:_) -> case filter (T.isPrefixOf "expression" . oHead) (oChildren s) of
                (argE:_) ->
                  case oChildren argE of
                    (h:_) | "Pexp_tuple" `T.isPrefixOf` oHead h -> do
                      let listKids = case filter (T.isPrefixOf "<list>" . oHead) (oChildren argE) of
                                       (l:_) -> filter (T.isPrefixOf "expression" . oHead) (oChildren l)
                                       []    -> []
                      mapM translateExpr listKids
                    _ -> do
                      a <- translateExpr argE
                      pure [a]
                [] -> Right []
              [] -> Right []
            if null argExprs
              then Right con
              else Right (EApp con argExprs)

    | "Pexp_match" `T.isPrefixOf` oHead c -> do
        -- After Pexp_match marker: an expression (scrutinee) followed
        -- by a <list> of <case> entries. Each <case> has a pattern
        -- and an expression child.
        let exprs = filter (T.isPrefixOf "expression" . oHead) kids
        scrut <- case exprs of
          (s:_) -> translateExpr s
          _     -> Left "Pexp_match: no scrutinee"
        let caseListKids = case filter (T.isPrefixOf "<list>" . oHead) kids of
              (l:_) -> filter (T.isPrefixOf "<case>" . oHead) (oChildren l)
              []    -> []
        branches <- mapM translateCase caseListKids
        pure (ECase scrut branches)

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

-- | Translate a single @<case>@ entry to a 'Branch'.
translateCase :: OLine -> Either Text Branch
translateCase cs = do
  pat <- case filter (T.isPrefixOf "pattern" . oHead) (oChildren cs) of
    (p:_) -> Right p
    []    -> Left "<case>: no pattern"
  expr <- case filter (T.isPrefixOf "expression" . oHead) (oChildren cs) of
    (e:_) -> Right e
    []    -> Left "<case>: no expression"
  pat' <- translatePattern pat
  body <- translateExpr expr
  pure (Branch pat' Nothing body)

-- | Translate a @pattern@ node to a Core 'Pattern'.
translatePattern :: OLine -> Either Text Pattern
translatePattern p = case oChildren p of
  [] -> Right (PatWild intT)
  (c:_)
    | "Ppat_any" `T.isPrefixOf` oHead c -> Right (PatWild intT)
    | "Ppat_var" `T.isPrefixOf` oHead c ->
        case oString c of
          Just s  -> Right (PatVar (Name s 0) intT)
          Nothing -> Right (PatWild intT)
    | "Ppat_constant" `T.isPrefixOf` oHead c -> do
        let consts = filter (T.isPrefixOf "constant" . oHead) (oChildren p)
        case consts of
          (k:_) -> case oChildren k of
            (x:_) | "PConst_int" `T.isPrefixOf` oHead x ->
              case oInt x of
                Just n  -> Right (PatLit (LitInt n))
                Nothing -> Right (PatWild intT)
            _ -> Right (PatWild intT)
          _ -> Right (PatWild intT)
    | "Ppat_construct" `T.isPrefixOf` oHead c -> do
        nm <- case oString c of
          Just s  -> Right s
          Nothing -> Left "Ppat_construct: missing name"
        if nm == "()"
          then Right (PatLit (LitInt 0))
          else do
            -- Check for a Some carrying argument patterns.
            argPats <- case filter (T.isPrefixOf "Some" . oHead) (oChildren p) of
              (s:_) -> case filter (T.isPrefixOf "pattern" . oHead) (oChildren s) of
                (argP:_) ->
                  case oChildren argP of
                    (h:_) | "Ppat_tuple" `T.isPrefixOf` oHead h -> do
                      let listKids = case filter (T.isPrefixOf "<list>" . oHead) (oChildren argP) of
                            (l:_) -> filter (T.isPrefixOf "pattern" . oHead) (oChildren l)
                            []    -> []
                      mapM translatePattern listKids
                    _ -> do
                      a <- translatePattern argP
                      pure [a]
                [] -> Right []
              [] -> Right []
            Right (PatCon (QName "" (Name nm 0)) argPats)
    | otherwise -> Right (PatWild intT)

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
