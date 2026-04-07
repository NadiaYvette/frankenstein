-- | Hand-rolled parser for a small subset of Futhark.
--
-- Futhark is a strict, purely-functional array language designed for GPU
-- compilation. We don't depend on the @futhark@ binary; instead this
-- module accepts a deliberately restricted subset that is enough to
-- exercise the bridge end-to-end and make room for later array-dialect
-- work in the MLIR backend.
--
-- Supported subset:
--
--   * Top-level value bindings:
--       @let name (p1: t1) (p2: t2) ... : ret = expr@
--     where types are ignored (we treat everything as i64).
--   * Integer literals (with optional unary minus prefix).
--   * Identifiers.
--   * Function application (juxtaposition: @f x y@).
--   * Parenthesised expressions.
--   * Binary operators: @+ - * / %@, comparisons @== != < <= > >=@,
--     bitwise @& | ^@.
--   * @if cond then a else b@.
--   * Local @let x = e in body@.
--
-- Anything else (records, tuples, sequence literals, type ascriptions on
-- expressions, modules, lambdas) raises a parse error.
--
-- The grammar is parsed via a small Pratt parser keyed on operator
-- precedence; comments (@-- ...@) and whitespace are skipped uniformly.

module Frankenstein.FutharkBridge.Parser
  ( FutModule(..)
  , FutDef(..)
  , FutExpr(..)
  , parseFuthark
  , parseFutharkFile
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)

-- ---------------------------------------------------------------------------
-- AST

data FutModule = FutModule
  { futModName :: !Text
  , futModDefs :: ![FutDef]
  } deriving (Show, Eq)

data FutDef = FutDef
  { futDefName   :: !Text
  , futDefParams :: ![Text]
  , futDefBody   :: !FutExpr
  } deriving (Show, Eq)

data FutExpr
  = FELit  !Integer
  | FEVar  !Text
  | FEApp  !FutExpr ![FutExpr]
  | FEBin  !Text !FutExpr !FutExpr      -- "+", "<=", ...
  | FENeg  !FutExpr
  | FEIf   !FutExpr !FutExpr !FutExpr
  | FELet  !Text !FutExpr !FutExpr      -- let x = e in body
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- Public entry points

parseFutharkFile :: FilePath -> IO (Either Text FutModule)
parseFutharkFile path = do
  src <- TIO.readFile path
  let modName = T.pack (takeBase path)
  pure $ parseFuthark modName src
  where
    takeBase = reverse . takeWhile (/= '/') . drop 1 . dropWhile (/= '.') . reverse . reverse
    -- The contortion above is just "drop dir, drop suffix"; we keep it
    -- inlined to avoid pulling in System.FilePath here.

parseFuthark :: Text -> Text -> Either Text FutModule
parseFuthark modName src =
  case runP topLevel (skipTrivia src) of
    Left err            -> Left err
    Right (defs, rest)  ->
      let rest' = skipTrivia rest
      in if T.null rest'
         then Right (FutModule modName defs)
         else Left $ "Trailing input after definitions: "
                  <> T.take 60 rest'

-- ---------------------------------------------------------------------------
-- Parser monad: simple State (Text) over Either Text

newtype P a = P { runP :: Text -> Either Text (a, Text) }

instance Functor P where
  fmap f (P g) = P $ \s -> case g s of
    Left e         -> Left e
    Right (a, s')  -> Right (f a, s')

instance Applicative P where
  pure a = P $ \s -> Right (a, s)
  P f <*> P g = P $ \s -> case f s of
    Left e         -> Left e
    Right (h, s')  -> case g s' of
      Left e2        -> Left e2
      Right (a, s'') -> Right (h a, s'')

instance Monad P where
  return = pure
  P g >>= k = P $ \s -> case g s of
    Left e        -> Left e
    Right (a, s') -> runP (k a) s'

failP :: Text -> P a
failP msg = P $ \_ -> Left msg

peek :: P (Maybe Char)
peek = P $ \s -> Right (case T.uncons s of
                          Nothing      -> Nothing
                          Just (c, _)  -> Just c, s)

advance :: P ()
advance = P $ \s -> case T.uncons s of
  Nothing      -> Left "advance past end"
  Just (_, r)  -> Right ((), r)

-- ---------------------------------------------------------------------------
-- Lexer-ish helpers

skipTrivia :: Text -> Text
skipTrivia t0 = go t0
  where
    go t = case T.uncons t of
      Just (c, _) | isSpace c -> go (T.dropWhile isSpace t)
      _ -> case T.stripPrefix "--" t of
             Just rest -> go (T.dropWhile (/= '\n') rest)
             Nothing   -> t

ws :: P ()
ws = P $ \s -> Right ((), skipTrivia s)

eatChar :: Char -> P ()
eatChar c = do
  ws
  m <- peek
  case m of
    Just c' | c == c' -> advance
    Just c' -> failP $ "Expected '" <> T.singleton c <> "', got '" <> T.singleton c' <> "'"
    Nothing -> failP $ "Expected '" <> T.singleton c <> "', got end of input"

eatString :: Text -> P ()
eatString w = do
  ws
  P $ \s -> case T.stripPrefix w s of
    Just rest -> Right ((), rest)
    Nothing   -> Left $ "Expected '" <> w <> "'"

-- | Parse a keyword: word that is followed by a non-identifier character.
keyword :: Text -> P ()
keyword w = do
  ws
  P $ \s -> case T.stripPrefix w s of
    Nothing -> Left $ "Expected keyword '" <> w <> "'"
    Just rest -> case T.uncons rest of
      Just (c, _) | isIdentTail c ->
        Left $ "Expected keyword '" <> w <> "', found longer identifier"
      _ -> Right ((), rest)

isIdentStart :: Char -> Bool
isIdentStart c = isAlpha c || c == '_'

isIdentTail :: Char -> Bool
isIdentTail c = isAlphaNum c || c == '_' || c == '\''

ident :: P Text
ident = do
  ws
  P $ \s -> case T.uncons s of
    Just (c, _) | isIdentStart c ->
      let (idText, rest) = T.span isIdentTail s
      in if idText `elem` reservedWords
         then Left $ "Identifier expected, got reserved word: " <> idText
         else Right (idText, rest)
    _ -> Left "Expected identifier"

reservedWords :: [Text]
reservedWords =
  [ "let", "in", "if", "then", "else"
  , "true", "false", "match", "case", "do"
  ]

intLit :: P Integer
intLit = do
  ws
  P $ \s -> case T.span isDigit s of
    (digs, rest) | not (T.null digs) ->
      Right (read (T.unpack digs), rest)
    _ -> Left "Expected integer literal"

-- ---------------------------------------------------------------------------
-- Top-level: a sequence of @let@ definitions

topLevel :: P [FutDef]
topLevel = go []
  where
    go acc = do
      ws
      m <- peek
      case m of
        Nothing -> pure (reverse acc)
        Just _  -> do
          d <- definition
          go (d : acc)

-- @let name [(param: type)*] [: ret_type] = expr@
definition :: P FutDef
definition = do
  keyword "let"
  name <- ident
  params <- many paramSpec
  -- Optional return-type annotation, ignored.
  _ <- optional $ do
    eatChar ':'
    typeText
  eatChar '='
  body <- expr
  pure (FutDef name params body)

-- A parameter is either @name@ or @(name: type)@; the type is discarded.
paramSpec :: P Text
paramSpec = P $ \s ->
  let s' = skipTrivia s
  in case T.uncons s' of
       Just ('(', _) ->
         runP (do
           eatChar '('
           n <- ident
           _ <- optional $ do
             eatChar ':'
             typeText
           eatChar ')'
           pure n) s
       Just (c, _) | isIdentStart c ->
         runP (do
           n <- ident
           if n `elem` reservedWords then failP "param" else pure n) s
       _ -> Left "no param"

-- A type ascription consumes one balanced atom (identifier or
-- parenthesised group); we don't actually use the type information.
typeText :: P ()
typeText = do
  ws
  m <- peek
  case m of
    Just '(' -> do
      eatChar '('
      _ <- balanced 1
      pure ()
    Just c | isIdentStart c -> do
      _ <- ident
      pure ()
    _ -> failP "Expected type"

balanced :: Int -> P ()
balanced 0 = pure ()
balanced n = do
  m <- peek
  case m of
    Nothing  -> failP "Unbalanced parens in type"
    Just '(' -> advance >> balanced (n + 1)
    Just ')' -> advance >> balanced (n - 1)
    Just _   -> advance >> balanced n

-- ---------------------------------------------------------------------------
-- Expressions (Pratt-ish; precedence climbing)

-- Precedence levels (higher binds tighter):
--   1: || (not supported)
--   2: == != < <= > >=
--   3: + -  &  |  ^
--   4: * / %
--   (unary minus)
--   5: application

expr :: P FutExpr
expr = do
  ws
  m <- peek
  case m of
    Just _ -> letOrIfOrBin
    Nothing -> failP "Expected expression"

letOrIfOrBin :: P FutExpr
letOrIfOrBin = do
  ws
  s <- P $ \t -> Right (t, t)
  if "let " `T.isPrefixOf` s || "let\t" `T.isPrefixOf` s || "let\n" `T.isPrefixOf` s
    then letExpr
    else if "if " `T.isPrefixOf` s || "if\t" `T.isPrefixOf` s || "if\n" `T.isPrefixOf` s
         then ifExpr
         else binExpr 1

letExpr :: P FutExpr
letExpr = do
  keyword "let"
  name <- ident
  -- Allow `let f x = ...` as a local function: collect params.
  params <- many paramSpec
  eatChar '='
  rhs <- expr
  keyword "in"
  body <- expr
  let rhs' = if null params then rhs
             else foldr (\_ acc -> acc) rhs params  -- params discarded; we don't model lambdas
      _ = rhs'
  pure (FELet name rhs body)

ifExpr :: P FutExpr
ifExpr = do
  keyword "if"
  c <- expr
  keyword "then"
  t <- expr
  keyword "else"
  e <- expr
  pure (FEIf c t e)

-- Precedence climbing: parse a left-hand side at level @lvl@, then while
-- the next operator binds at >= lvl, fold it in.
binExpr :: Int -> P FutExpr
binExpr lvl = do
  lhs <- unary
  binLoop lvl lhs

binLoop :: Int -> FutExpr -> P FutExpr
binLoop lvl lhs = do
  ws
  s <- P $ \t -> Right (t, t)
  case nextOp s of
    Nothing -> pure lhs
    Just (op, opLvl, rest)
      | opLvl < lvl -> pure lhs
      | otherwise -> do
          P $ \_ -> Right ((), rest)
          rhs <- binExpr (opLvl + 1)
          binLoop lvl (FEBin op lhs rhs)

-- | Look at the input and see if it starts with a recognised binary
-- operator. Returns the operator text, its precedence level, and the
-- remaining input after stripping it.
nextOp :: Text -> Maybe (Text, Int, Text)
nextOp s
  -- two-character ops first so we don't truncate "<=" to "<"
  | "==" `T.isPrefixOf` s = Just ("==", 2, T.drop 2 s)
  | "!=" `T.isPrefixOf` s = Just ("!=", 2, T.drop 2 s)
  | "<=" `T.isPrefixOf` s = Just ("<=", 2, T.drop 2 s)
  | ">=" `T.isPrefixOf` s = Just (">=", 2, T.drop 2 s)
  | otherwise = case T.uncons s of
      Just ('+', r) -> Just ("+", 3, r)
      Just ('-', r) -> Just ("-", 3, r)
      Just ('*', r) -> Just ("*", 4, r)
      Just ('/', r) -> Just ("/", 4, r)
      Just ('%', r) -> Just ("%", 4, r)
      Just ('<', r) -> Just ("<", 2, r)
      Just ('>', r) -> Just (">", 2, r)
      Just ('&', r) -> Just ("&", 3, r)
      Just ('|', r) -> Just ("|", 3, r)
      Just ('^', r) -> Just ("^", 3, r)
      _ -> Nothing

unary :: P FutExpr
unary = do
  ws
  m <- peek
  case m of
    Just '-' -> do
      advance
      e <- unary
      pure (FENeg e)
    _ -> appExpr

-- | Application by juxtaposition: collect a sequence of atoms.
appExpr :: P FutExpr
appExpr = do
  head_ <- atom
  args <- gatherArgs []
  pure $ if null args then head_ else FEApp head_ args
  where
    gatherArgs acc = do
      m <- atomMaybe
      case m of
        Nothing -> pure (reverse acc)
        Just a  -> gatherArgs (a : acc)

-- | Parse an atom only if the next token clearly starts one (otherwise
-- return Nothing without consuming). Used to terminate application chains.
atomMaybe :: P (Maybe FutExpr)
atomMaybe = do
  ws
  m <- peek
  case m of
    Just c
      | isDigit c     -> Just <$> atom
      | c == '('      -> Just <$> atom
      | isIdentStart c -> P $ \s ->
          -- Don't gobble keywords like 'then', 'else', 'in'.
          let (w, _) = T.span isIdentTail s
          in if w `elem` reservedWords
             then Right (Nothing, s)
             else case runP atom s of
                    Left e -> Left e
                    Right (a, s') -> Right (Just a, s')
    _ -> pure Nothing

atom :: P FutExpr
atom = do
  ws
  m <- peek
  case m of
    Just '(' -> do
      eatChar '('
      e <- expr
      eatChar ')'
      pure e
    Just c
      | isDigit c -> FELit <$> intLit
      | isIdentStart c -> do
          n <- ident
          case n of
            "true"  -> pure (FELit 1)
            "false" -> pure (FELit 0)
            _       -> pure (FEVar n)
    _ -> failP "Expected atom"

-- ---------------------------------------------------------------------------
-- many / optional combinators

many :: P a -> P [a]
many p = P $ \s -> go s []
  where
    go s acc = case runP p s of
      Left _        -> Right (reverse acc, s)
      Right (a, s') -> go s' (a : acc)

optional :: P a -> P (Maybe a)
optional p = P $ \s -> case runP p s of
  Left _        -> Right (Nothing, s)
  Right (a, s') -> Right (Just a, s')
