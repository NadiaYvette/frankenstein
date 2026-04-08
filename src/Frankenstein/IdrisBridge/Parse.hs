-- | Minimal source-level parser for a tiny Int subset of Idris 2.
--
-- Idris 2 has no public parsetree dumper on the command line, and
-- shelling out to a real elaborator would drag in a lot of machinery
-- for what is meant to be a small \"eighth-language\" style bridge.
-- We follow the same pattern as the Scheme reader: hand-rolled,
-- zero-dependency, scoped to exactly what our two demo programs use.
--
-- Supported:
--
--   * @module Name@ header (ignored).
--   * @name : Type@ type signatures (ignored — we always assume Int).
--   * @name arg1 arg2 ... = expr@ single-line definitions.
--   * Expressions: integer literals, variable references, function
--     application (juxtaposition), parenthesised sub-expressions,
--     binary operators (@+ - * \/ \< \<= \> \>= == \/=@), and
--     @if e1 then e2 else e3@.
--
-- Anything else raises a parse error. For the full Idris 2 syntax,
-- layout, records, interfaces, totality annotations, implicits,
-- universes, linear types, etc. — use the real compiler.
module Frankenstein.IdrisBridge.Parse
  ( IExpr(..)
  , IDecl(..)
  , parseIdris
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- | Idris expression tree. Deliberately smaller than 'SExpr' so the
-- translator in 'CoreTranslate' can pattern-match directly.
data IExpr
  = IInt   !Integer
  | IVar   !Text
  | IApp   !IExpr ![IExpr]
  | IBin   !Text !IExpr !IExpr   -- ^ already mapped to Frankenstein op names
  | IIf    !IExpr !IExpr !IExpr
  deriving (Show, Eq)

-- | Top-level declaration. Type signatures are dropped before we get
-- here, so a 'IDecl' is always a definition.
data IDecl = IDecl
  { idName   :: !Text
  , idParams :: ![Text]
  , idBody   :: !IExpr
  } deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Top-level entry point

parseIdris :: Text -> Either Text [IDecl]
parseIdris src =
  let rawLines = T.lines src
      clean    = map stripComment rawLines
      joined   = joinDefLines clean
      logical  = filter (not . T.null . T.strip) joined
      bodies   = [ l | l <- logical
                     , not ("module " `T.isPrefixOf` T.stripStart l)
                     , not (isTypeSig l) ]
  in mapM parseDecl bodies

-- | Idris 2 line comments start with @--@ and run to end of line.
stripComment :: Text -> Text
stripComment t = case T.breakOn "--" t of
  (pre, _) -> pre

-- | A type signature line has the shape @name : <rest>@ with the
-- colon appearing before any @=@.
isTypeSig :: Text -> Bool
isTypeSig line =
  let l = T.strip line
      (beforeEq, _) = T.breakOn "=" l
      -- In Idris 2, @==@ would trip this up, but @==@ can't appear at
      -- top-level in a type signature — only inside an expression
      -- after the @=@. So finding any @=@ on the line means this is a
      -- definition, not a sig.
  in case T.breakOn ":" beforeEq of
       (_, rest) | not (T.null rest) -> True
       _                             -> False

-- | Merge continuation-indented lines into the line that starts them.
-- Top-level declarations in Idris begin at column 0; an indented line
-- is part of the preceding declaration.
joinDefLines :: [Text] -> [Text]
joinDefLines = go
  where
    go []     = []
    go (l:ls) =
      let (cont, rest) = span isIndented ls
          merged       = T.intercalate " " (l : map T.strip cont)
      in merged : go rest
    isIndented t =
      not (T.null (T.strip t))
      && isSpace (T.head t)

-- ---------------------------------------------------------------------------
-- Declaration parsing

parseDecl :: Text -> Either Text IDecl
parseDecl line = do
  let (lhs, rhs) = T.breakOn "=" line
  rhsBody <- case T.uncons rhs of
    Just ('=', r) -> Right (T.strip r)
    _             -> Left ("Idris: missing '=' in declaration: " <> line)
  let ws = T.words (T.strip lhs)
  case ws of
    []       -> Left ("Idris: empty declaration: " <> line)
    (nm:ps)  -> do
      body <- parseExprFull rhsBody
      pure IDecl { idName = nm, idParams = ps, idBody = body }

-- ---------------------------------------------------------------------------
-- Expression parser
--
-- Recursive-descent with a tiny operator-precedence table. The input
-- has already been stripped of comments and layout, so we just
-- tokenise on the fly from a 'Text' buffer.

data Tok
  = TInt !Integer
  | TIdent !Text
  | TOp !Text
  | TLPar
  | TRPar
  | TKw !Text   -- if, then, else
  deriving (Show, Eq)

tokenize :: Text -> Either Text [Tok]
tokenize = go . T.strip
  where
    go t | T.null t = Right []
    go t =
      let t' = T.dropWhile isSpace t
      in if T.null t' then Right [] else
        case T.uncons t' of
          Just ('(', r) -> (TLPar :) <$> go r
          Just (')', r) -> (TRPar :) <$> go r
          Just (c, _)
            | isDigit c -> do
                let (ds, r) = T.span isDigit t'
                case reads (T.unpack ds) :: [(Integer, String)] of
                  [(n, "")] -> (TInt n :) <$> go r
                  _         -> Left ("bad integer: " <> ds)
            | isAlpha c || c == '_' -> do
                let (nm, r) = T.span identChar t'
                    tok = case nm of
                      "if"   -> TKw "if"
                      "then" -> TKw "then"
                      "else" -> TKw "else"
                      _      -> TIdent nm
                (tok :) <$> go r
            | isOpChar c -> do
                let (op, r) = T.span isOpChar t'
                (TOp op :) <$> go r
          _ -> Left ("unexpected char in expression: " <> T.take 10 t')
    identChar c = isAlphaNum c || c == '_' || c == '\''
    isOpChar c = c `elem` ("+-*/<>=/" :: String)

parseExprFull :: Text -> Either Text IExpr
parseExprFull src = do
  toks <- tokenize src
  (e, rest) <- pExpr toks
  case rest of
    [] -> Right e
    _  -> Left ("trailing tokens after expression: " <> T.pack (show rest))

-- | @pExpr@ — top of the precedence chain. Includes 'if'.
pExpr :: [Tok] -> Either Text (IExpr, [Tok])
pExpr (TKw "if" : rest) = do
  (c, r1) <- pExpr rest
  case r1 of
    (TKw "then" : r2) -> do
      (t, r3) <- pExpr r2
      case r3 of
        (TKw "else" : r4) -> do
          (e, r5) <- pExpr r4
          pure (IIf c t e, r5)
        _ -> Left "if: missing 'else'"
    _ -> Left "if: missing 'then'"
pExpr toks = pCmp toks

-- | Comparison operators — @<= \< \> \>= == \/=@. Non-associative in
-- Idris, but we parse them left-associatively for simplicity.
pCmp :: [Tok] -> Either Text (IExpr, [Tok])
pCmp ts = do
  (l, r) <- pAdd ts
  goCmp l r
  where
    goCmp l (TOp op : rs) | isCmp op = do
      (r', rs') <- pAdd rs
      goCmp (IBin (mapCmp op) l r') rs'
    goCmp l rs = Right (l, rs)
    isCmp op = op `elem` ["<", "<=", ">", ">=", "==", "/="]

mapCmp :: Text -> Text
mapCmp o = o

pAdd :: [Tok] -> Either Text (IExpr, [Tok])
pAdd ts = do
  (l, r) <- pMul ts
  goAdd l r
  where
    goAdd l (TOp op : rs)
      | op == "+" || op == "-" = do
          (r', rs') <- pMul rs
          goAdd (IBin op l r') rs'
    goAdd l rs = Right (l, rs)

pMul :: [Tok] -> Either Text (IExpr, [Tok])
pMul ts = do
  (l, r) <- pApp ts
  goMul l r
  where
    goMul l (TOp op : rs)
      | op == "*" || op == "/" = do
          (r', rs') <- pApp rs
          goMul (IBin op l r') rs'
    goMul l rs = Right (l, rs)

-- | Function application by juxtaposition. Collects a head atom and
-- as many following atoms as there are, left-associatively.
pApp :: [Tok] -> Either Text (IExpr, [Tok])
pApp ts = do
  (h, r) <- pAtom ts
  goApp h [] r
  where
    goApp h acc rs = case peekAtom rs of
      Just _ -> do
        (a, rs') <- pAtom rs
        goApp h (acc ++ [a]) rs'
      Nothing ->
        if null acc
          then Right (h, rs)
          else Right (IApp h acc, rs)

    peekAtom (TInt _ : _)   = Just ()
    peekAtom (TIdent _ : _) = Just ()
    peekAtom (TLPar : _)    = Just ()
    peekAtom _              = Nothing

pAtom :: [Tok] -> Either Text (IExpr, [Tok])
pAtom (TInt n : rs)   = Right (IInt n, rs)
pAtom (TIdent nm : rs) = Right (IVar nm, rs)
pAtom (TLPar : rs)    = do
  (e, rs') <- pExpr rs
  case rs' of
    (TRPar : rs'') -> Right (e, rs'')
    _              -> Left "missing ')'"
-- A bare unary minus applied to a literal.
pAtom (TOp "-" : TInt n : rs) = Right (IInt (negate n), rs)
pAtom ts = Left ("expected atom, got: " <> T.pack (show (take 3 ts)))
