-- | Swift (via @swiftc -dump-ast@) → Frankenstein Core.
--
-- Supported subset (enough for the Perceus cross-check):
--
--   * Top-level @func name(_ x: Int, ...) -> Int@ with an @Int@-only
--     body: integer literals, parameter references, recursive calls,
--     binary @+ - * / % == != < <= > >=@, and @if cond { return A }
--     return B@.
--   * Top-level @let x = expr@ bindings (emitted as zero-arg defs).
--   * A single top-level @print(expr)@ at the bottom of the file,
--     which becomes @main@.
--
-- Classes, closures, structs, and generics are explicitly out of scope
-- for the first bridge — the point is to get an Int-typed program
-- through the same Perceus pipeline as the other frontends so its RC
-- decisions can be compared against @swiftc -O -emit-sil@.
module Frankenstein.SwiftBridge.CoreTranslate
  ( translateSwift
  ) where

import Frankenstein.Core.Types
import Frankenstein.SwiftBridge.AstParse

import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- Entry

translateSwift :: Text -> Node -> Either Text Program
translateSwift modName root = do
  let topKids = nodeChildren root
  (defs, mainExpr) <- translateTop modName topKids
  let mainDef = case mainExpr of
        Nothing -> []
        Just e  ->
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

-- | Walk top-level children, extracting function decls, global lets,
-- and the final print call (which becomes the body of @main@).
translateTop
  :: Text
  -> [Node]
  -> Either Text ([Def], Maybe Expr)
translateTop modName = go [] Nothing
  where
    go accDefs accMain [] = Right (reverse accDefs, accMain)
    go accDefs accMain (n:rest) = case nodeTag n of
      "func_decl" -> do
        d <- translateFuncDecl modName n
        go (d : accDefs) accMain rest
      "top_level_code_decl" -> do
        -- Either a pattern_binding_decl (global let) or a bare call
        -- (the print(...) line). Peek the brace_stmt's single child.
        let inner = concatMap nodeChildren (findChildren "brace_stmt" n)
        case inner of
          [pbd] | nodeTag pbd == "pattern_binding_decl" -> do
            d <- translateGlobalLet modName pbd
            go (d : accDefs) accMain rest
          _ -> do
            -- Treat the whole brace_stmt as an expression sequence.
            e <- translateBrace n
            -- Stitch sequential top-level statements into a single
            -- expression by ELet-chaining (each result discarded).
            let accMain' = case accMain of
                  Nothing   -> Just e
                  Just prev -> Just (seqExpr prev e)
            go accDefs accMain' rest
      "var_decl" -> go accDefs accMain rest  -- storage-only echo, ignore
      _ -> go accDefs accMain rest           -- ignore noise

-- ---------------------------------------------------------------------------
-- Function declarations

translateFuncDecl :: Text -> Node -> Either Text Def
translateFuncDecl modName n = do
  name <- case nodeStrings n of
    (s:_) -> Right (cleanFuncName s)
    _     -> Left "func_decl: missing name string"
  let params = case findChild "parameter_list" n of
        Just pl -> [ paramName p | p <- findChildren "parameter" pl ]
        Nothing -> []
  body <- case findChild "brace_stmt" n of
    Just b  -> translateBrace b
    Nothing -> Left $ "func_decl " <> name <> ": missing brace_stmt"
  let lamParams = [(Name p 0, intT) | p <- params]
      argTypes  = [(Many, intT)     | _ <- params]
      fnTy      = TFun argTypes EffectRowEmpty intT
  pure Def
    { defName       = QName modName (Name name 0)
    , defType       = fnTy
    , defExpr       = ELam lamParams body
    , defSort       = DefFun
    , defVisibility = Public
    }
  where
    paramName p = case nodeStrings p of
      (s:_) -> s
      _     -> "_"

-- | Strip the Swift argument-label suffix @(_:)@ so the name matches
-- the identifier used elsewhere in the file.
cleanFuncName :: Text -> Text
cleanFuncName s = case T.breakOn "(" s of
  (n, _) -> n

-- ---------------------------------------------------------------------------
-- Global let

translateGlobalLet :: Text -> Node -> Either Text Def
translateGlobalLet modName pbd = do
  let entry = case findChild "pattern_entry" pbd of
        Just e  -> e
        Nothing -> pbd   -- some versions inline the entry
  name <- case findChild "pattern_named" entry of
    Just p -> case nodeStrings p of
      (s:_) -> Right s
      _     -> Left "pattern_named: no name"
    Nothing -> Left "global let: no pattern_named"
  initExpr <- case concatMap nodeChildren (findChildren "pattern_entry" pbd) of
    -- pattern_entry's children are declref/call/etc. AFTER pattern_named
    kids -> case filter (\k -> nodeTag k /= "pattern_named") kids of
      (e:_) -> translateExpr e
      []    -> Left $ "global let " <> name <> ": no initializer"
  pure Def
    { defName       = QName modName (Name name 0)
    , defType       = intT
    , defExpr       = initExpr
    , defSort       = DefVal
    , defVisibility = Public
    }

-- ---------------------------------------------------------------------------
-- Statements: brace_stmt is a block whose "value" is either a
-- return_stmt's expression or (for a top-level trailing print call)
-- a call_expr.

translateBrace :: Node -> Either Text Expr
translateBrace b = do
  let kids = nodeChildren b
  stmtsToExpr kids

-- | Translate a sequence of statements/expressions into a single Expr.
-- Early returns inside @if@ become @ECase@ branches.
stmtsToExpr :: [Node] -> Either Text Expr
stmtsToExpr []     = Right (ELit (LitInt 0))
stmtsToExpr [s]    = translateStmt s
stmtsToExpr (s:ss) = case nodeTag s of
  "if_stmt" -> do
    -- Swift early-return pattern: (if cond { return A }) (return B)
    -- becomes ECase cond [0 -> B, _ -> A].
    cond <- translateIfCond s
    thenE <- case findChildren "brace_stmt" s of
      (tb:_) -> translateBrace tb
      _      -> Left "if_stmt: missing then-branch"
    elseE <- stmtsToExpr ss
    pure $ ECase cond
      [ Branch (PatLit (LitInt 0)) Nothing elseE
      , Branch (PatWild intT)      Nothing thenE
      ]
  _ -> do
    e1 <- translateStmt s
    e2 <- stmtsToExpr ss
    pure (seqExpr e1 e2)

seqExpr :: Expr -> Expr -> Expr
seqExpr e1 e2 =
  ELet [[Bind (Name "_seq" 0) intT e1 DefVal]] e2

translateStmt :: Node -> Either Text Expr
translateStmt n = case nodeTag n of
  "return_stmt" -> case nodeChildren n of
    (e:_) -> translateExpr e
    _     -> Right (ELit (LitInt 0))
  "brace_stmt" -> translateBrace n
  "if_stmt" -> do
    cond <- translateIfCond n
    thenE <- case findChildren "brace_stmt" n of
      (tb:_) -> translateBrace tb
      _      -> Left "if_stmt: missing then-branch"
    pure $ ECase cond
      [ Branch (PatLit (LitInt 0)) Nothing (ELit (LitInt 0))
      , Branch (PatWild intT)      Nothing thenE
      ]
  _ -> translateExpr n

translateIfCond :: Node -> Either Text Expr
translateIfCond n = case findChild "binary_expr" n of
  Just be -> translateExpr be
  Nothing ->
    -- Older swiftc dumps wrap conditions differently; recurse.
    case concatMap nodeChildren (nodeChildren n) of
      (c:_) -> translateExpr c
      _     -> Left "if_stmt: unable to locate condition"

-- ---------------------------------------------------------------------------
-- Expressions

translateExpr :: Node -> Either Text Expr
translateExpr n = case nodeTag n of
  "integer_literal_expr" -> case nodeAttr "value" n of
    Just v -> case reads (T.unpack v) :: [(Integer, String)] of
      [(i, "")] -> Right (ELit (LitInt i))
      _         -> Left $ "integer_literal_expr: bad value " <> v
    Nothing -> Left "integer_literal_expr: missing value attr"

  "declref_expr" -> case nodeAttr "decl" n of
    Just d  -> Right (EVar (Name (lastSegment d) 0))
    Nothing -> Left "declref_expr: missing decl"

  "binary_expr" -> translateBinary n

  "call_expr" -> translateCall n

  "paren_expr" -> case nodeChildren n of
    (c:_) -> translateExpr c
    _     -> Left "paren_expr: empty"

  "prefix_unary_expr" -> case nodeChildren n of
    (opNode : argNode : _) -> do
      let opName = case nodeAttr "decl" opNode of
            Just d  -> lastSegment d
            Nothing -> "negate"
      a <- translateExpr argNode
      pure (EApp (EVar (Name opName 0)) [a])
    _ -> Left "prefix_unary_expr: malformed"

  "dot_syntax_call_expr" -> case nodeChildren n of
    -- First kid is the method ref, rest are the receiver's args
    (_ : argList : _) -> case findChild "argument" argList of
      Just a -> case nodeChildren a of
        (e:_) -> translateExpr e
        _     -> Left "dot_syntax_call_expr: empty argument"
      Nothing -> Left "dot_syntax_call_expr: no argument"
    _ -> Left "dot_syntax_call_expr: missing kids"

  -- Wrapping nodes: skip non-expression siblings (builtin_conformance
  -- etc.) and translate the first real child.
  "load_expr"                -> unwrap n
  "erasure_expr"             -> unwrap n
  "function_conversion_expr" -> unwrap n
  "inject_into_optional"     -> unwrap n
  "vararg_expansion_expr"    -> unwrap n
  "array_expr"               -> unwrap n

  other -> Left $ "Unsupported Swift expr: " <> other

-- | Skip metadata nodes (builtin_conformance, type_expr, etc.) and
-- translate the first child that actually looks like an expression.
unwrap :: Node -> Either Text Expr
unwrap n = case filter isExprNode (nodeChildren n) of
  (c:_) -> translateExpr c
  _     -> Left $ "unwrap: no expression child in " <> nodeTag n

isExprNode :: Node -> Bool
isExprNode n = case nodeTag n of
  "builtin_conformance" -> False
  "type_expr"           -> False
  _                     -> True

translateBinary :: Node -> Either Text Expr
translateBinary n = do
  -- binary_expr shape:
  --   (binary_expr
  --     (dot_syntax_call_expr   ...  (declref_expr decl="... extension.OP"))
  --     (argument_list (argument EXPR1) (argument EXPR2)))
  opName <- case findChild "dot_syntax_call_expr" n of
    Just ds -> case findChild "declref_expr" ds of
      Just dr -> case nodeAttr "decl" dr of
        Just d  -> Right (lastSegment d)
        Nothing -> Left "binary_expr: declref without decl"
      Nothing -> Left "binary_expr: no declref in dot_syntax_call_expr"
    Nothing -> Left "binary_expr: no operator dot_syntax_call_expr"
  let primName = mapOp opName
  argList <- case findChild "argument_list" n of
    Just al -> Right al
    Nothing -> Left "binary_expr: missing argument_list"
  args <- mapM argExpr (findChildren "argument" argList)
  case args of
    [a, b] -> Right (EApp (EVar (Name primName 0)) [a, b])
    _      -> Left "binary_expr: expected two arguments"

translateCall :: Node -> Either Text Expr
translateCall n = do
  -- call_expr: first child is callee (declref or nested call),
  -- subsequent argument_list carries the arguments.
  fn <- case nodeChildren n of
    (c:_) -> case nodeTag c of
      "declref_expr"         -> translateExpr c
      "dot_syntax_call_expr" -> Left "dot_syntax_call_expr as callee NYI"
      _                      -> translateExpr c
    _ -> Left "call_expr: no callee"
  argList <- case findChild "argument_list" n of
    Just al -> Right al
    Nothing -> Left "call_expr: missing argument_list"
  -- Skip arguments with a named label (Swift's print(_:separator:terminator:)
  -- passes default values for separator and terminator that we don't model).
  let positional = filter (\a -> nodeAttr "label" a == Nothing)
                          (findChildren "argument" argList)
  args <- mapM argExpr positional
  -- Special-case print(x): desugar to the expression x itself so
  -- the generated main returns the printed value (matching other
  -- bridges' behaviour).
  case fn of
    EVar (Name nm _)
      | "print" `T.isPrefixOf` nm, (e:_) <- args -> Right e
    _ -> Right (EApp fn args)

argExpr :: Node -> Either Text Expr
argExpr a = case nodeChildren a of
  (e:_) -> translateExpr e
  _     -> Left "argument: empty"

-- | Map a Swift operator identifier like @Swift.(file).Int extension.*@
-- or the final-segment form @*@ into the canonical primitive names the
-- MLIR emitter recognises.
mapOp :: Text -> Text
mapOp "*"  = "*"
mapOp "+"  = "+"
mapOp "-"  = "-"
mapOp "/"  = "/"
mapOp "%"  = "mod"
mapOp "==" = "=="
mapOp "!=" = "/="
mapOp "<"  = "<"
mapOp "<=" = "<="
mapOp ">"  = ">"
mapOp ">=" = ">="
mapOp other = other  -- pass through; error out later if unknown

-- | Given a Swift-qualified identifier like
-- @"simple.(file).sq@/tmp/.../simple.swift:1:6"@, strip module and
-- location decoration to recover @"sq"@. The last dot-separated
-- segment before any trailing @\@path:L:C@ is the bare identifier.
lastSegment :: Text -> Text
lastSegment t =
  let noLoc = T.takeWhile (/= '@') t
  in  case reverse (T.splitOn "." noLoc) of
        (seg:_) -> case T.breakOn " extension." noLoc of
          (_, rest) | not (T.null rest) ->
            T.drop (T.length " extension.") rest
          _ -> seg
        [] -> t

-- ---------------------------------------------------------------------------

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
