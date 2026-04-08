-- | Parser for @swiftc -dump-ast@ output.
--
-- The output is a LISP-ish S-expression tree with three kinds of
-- "content" inside each node:
--
--   * Nested @(child ...)@ nodes
--   * Double-quoted strings, which may be positional names or
--     attribute values (with an immediately-preceding @key=@ atom)
--   * Bracketed range markers @[/path/file.swift:L:C - line:L:C]@ —
--     we strip these entirely because they only carry source
--     locations we don't need
--   * Plain atoms like @access=internal@ or @nothrow@ or
--     @interface_type="(Int) -> Int"@ where the @=@ glues a key
--     to its quoted value
--
-- This module produces a simple 'Node' tree. Semantic interpretation
-- lives in 'Frankenstein.SwiftBridge.CoreTranslate'.
module Frankenstein.SwiftBridge.AstParse
  ( Node(..)
  , parseSwiftAst
  , nodeTag
  , nodeAttr
  , nodeStrings
  , nodeChildren
  , findChildren
  , findChild
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isSpace)

-- | A node in the parsed AST tree.
data Node = Node
  { nTag      :: !Text                -- ^ Head of the list: @func_decl@, @binary_expr@, ...
  , nAttrs    :: ![(Text, Text)]      -- ^ @key=value@ or @key="value"@ pairs
  , nStrs     :: ![Text]              -- ^ Positional (non-key) quoted strings
  , nKids     :: ![Node]              -- ^ Nested child nodes
  } deriving (Show)

nodeTag :: Node -> Text
nodeTag = nTag

nodeAttr :: Text -> Node -> Maybe Text
nodeAttr k n = lookup k (nAttrs n)

nodeStrings :: Node -> [Text]
nodeStrings = nStrs

nodeChildren :: Node -> [Node]
nodeChildren = nKids

findChildren :: Text -> Node -> [Node]
findChildren tag n = filter ((== tag) . nTag) (nKids n)

findChild :: Text -> Node -> Maybe Node
findChild tag n = case findChildren tag n of
  (x:_) -> Just x
  []    -> Nothing

--------------------------------------------------------------------------------
-- Tokenizer
--------------------------------------------------------------------------------

data Tok
  = TLP
  | TRP
  | TAtom   !Text        -- unquoted word, may contain '='
  | TStr    !Text        -- literal (unquoted)
  | TKvStr  !Text !Text  -- key="value"
  deriving (Show)

-- | Tokenize swiftc -dump-ast text. Strips bracketed ranges.
tokenize :: Text -> [Tok]
tokenize = go
  where
    go t
      | T.null t = []
      | otherwise =
          let (c, rest) = (T.head t, T.tail t)
          in case c of
               _ | isSpace c -> go (T.dropWhile isSpace t)
               '(' -> TLP : go rest
               ')' -> TRP : go rest
               '[' -> go (dropBracket 1 rest)
               '"' -> let (s, r) = readStr rest in TStr s : go r
               _   -> let (a, r) = readAtom t
                      in case T.uncons r of
                           -- Handle key="value" where '=' is the last
                           -- char of the atom.
                           Just ('"', r') | T.isSuffixOf "=" a ->
                             let key = T.dropEnd 1 a
                                 (s, r'') = readStr r'
                             in TKvStr key s : go r''
                           _ -> TAtom a : go r

    -- Skip a balanced [...] region. Depth starts at 1 (we just ate '[').
    dropBracket :: Int -> Text -> Text
    dropBracket 0 t = t
    dropBracket d t = case T.uncons t of
      Nothing        -> t
      Just (']', r)  -> dropBracket (d - 1) r
      Just ('[', r)  -> dropBracket (d + 1) r
      Just (_,   r)  -> dropBracket d r

    -- Read a quoted string. The leading '"' has already been consumed.
    readStr :: Text -> (Text, Text)
    readStr = go' T.empty
      where
        go' acc t = case T.uncons t of
          Nothing            -> (acc, t)
          Just ('"',  r)     -> (acc, r)
          Just ('\\', r)     -> case T.uncons r of
            Nothing      -> (acc, r)
            Just (x, r') -> go' (T.snoc acc x) r'
          Just (c, r)        -> go' (T.snoc acc c) r

    -- Read an unquoted atom: run of non-whitespace, non-paren, non-quote
    -- characters. '=' is allowed because many attributes look like k=v.
    readAtom :: Text -> (Text, Text)
    readAtom = T.span ok
      where
        ok c = not (isSpace c) && c /= '(' && c /= ')'
               && c /= '"' && c /= '[' && c /= ']'

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- | Parse the entire swiftc -dump-ast output into a single root Node.
parseSwiftAst :: Text -> Either Text Node
parseSwiftAst txt = case tokenize txt of
  (TLP : rest) -> case parseNode rest of
    Right (n, []) -> Right n
    Right (n, _)  -> Right n  -- allow trailing junk
    Left err      -> Left err
  _ -> Left "parseSwiftAst: expected '('"

-- | Parse a single @(tag ...)@ node. Leading '(' already consumed.
parseNode :: [Tok] -> Either Text (Node, [Tok])
parseNode toks = case toks of
  (TAtom tag : rest) -> parseBody tag [] [] [] rest
  (TStr  tag : rest) -> parseBody tag [] [] [] rest
  _                  -> Left "parseNode: missing head atom after '('"

parseBody
  :: Text                -- tag
  -> [(Text, Text)]      -- attrs (reversed)
  -> [Text]              -- positional strings (reversed)
  -> [Node]              -- children (reversed)
  -> [Tok]
  -> Either Text (Node, [Tok])
parseBody tag attrs strs kids = \case
  []           -> Left $ "parseBody: unterminated node " <> tag
  (TRP : rest) -> Right (Node tag (reverse attrs) (reverse strs) (reverse kids), rest)
  (TLP : rest) -> do
    (kid, rest') <- parseNode rest
    parseBody tag attrs strs (kid : kids) rest'
  (TKvStr k v : rest) -> parseBody tag ((k, v) : attrs) strs kids rest
  (TStr  s    : rest) -> parseBody tag attrs (s : strs) kids rest
  (TAtom a    : rest) ->
    -- Split on the first '=' if present: key=value becomes an attribute.
    case T.breakOn "=" a of
      (k, v') | not (T.null v') ->
        parseBody tag ((k, T.drop 1 v') : attrs) strs kids rest
      _ -> parseBody tag attrs (a : strs) kids rest
