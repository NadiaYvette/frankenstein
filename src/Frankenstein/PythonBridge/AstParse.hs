-- | Python AST parser bridge.
--
-- Drives the @python-bridge/ast_to_sexp.py@ helper script and parses its
-- single-line S-expression output into a small 'PySExpr' tree. The
-- 'CoreTranslate' module turns that tree into Frankenstein Core.
--
-- The pipeline is intentionally simple: CPython's @ast@ module is the
-- canonical Python parser, and we don't want to maintain a Python parser
-- in Haskell. The helper outputs a tightly restricted subset of the AST
-- (see python-bridge/ast_to_sexp.py for the supported nodes).

module Frankenstein.PythonBridge.AstParse
  ( PySExpr(..)
  , parsePython
  , parseSExpr
  , runPyHelper
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException)

-- | A node in the helper's S-expression output.
data PySExpr
  = SAtom !Text          -- ^ bare identifier (e.g. @Module@, @Add@)
  | SStr  !Text          -- ^ quoted string literal
  | SInt  !Integer       -- ^ integer literal
  | SList ![PySExpr]     -- ^ parenthesised list of children
  deriving (Show, Eq)

-- | High-level entry: invoke the helper on a Python source file and parse
-- its S-expression output.
parsePython :: FilePath -> IO (Either Text PySExpr)
parsePython path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Python source file does not exist: " <> T.pack path
    else do
      r <- runPyHelper path
      case r of
        Left err  -> pure $ Left err
        Right txt -> pure $ parseSExpr txt

-- | Run @python3 python-bridge/ast_to_sexp.py <path>@ and return its stdout.
runPyHelper :: FilePath -> IO (Either Text Text)
runPyHelper path = do
  let helper = "python-bridge/ast_to_sexp.py"
  helperExists <- doesFileExist helper
  if not helperExists
    then pure $ Left $ "Helper script not found: " <> T.pack helper
                       <> " (run frankenstein from the project root)"
    else do
      eRes <- try $ readProcessWithExitCode "python3" [helper, path] ""
      case eRes :: Either SomeException (ExitCode, String, String) of
        Left ex -> pure $ Left $ "Failed to run python3: " <> T.pack (show ex)
        Right (ExitFailure n, _out, err) ->
          pure $ Left $ "python3 ast_to_sexp.py exited with " <> T.pack (show n)
                     <> ": " <> T.pack err
        Right (ExitSuccess, out, _err) ->
          pure $ Right $ T.strip $ T.pack out

-- ---------------------------------------------------------------------------
-- S-expression parser
--
-- Tokens: '(' ')' integer "string" atom
-- The helper guarantees one expression per invocation; we parse a single
-- top-level form and reject trailing junk.

parseSExpr :: Text -> Either Text PySExpr
parseSExpr input = case sexpr (T.dropWhile isSpace input) of
  Left err           -> Left err
  Right (e, rest) ->
    let rest' = T.dropWhile isSpace rest
    in if T.null rest'
       then Right e
       else Left $ "Trailing input after S-expression: " <> T.take 40 rest'
  where
    isSpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

-- | Parse a single expression. Returns the parsed value and the remaining text.
sexpr :: Text -> Either Text (PySExpr, Text)
sexpr t = case T.uncons t of
  Nothing -> Left "Unexpected end of input"
  Just ('(', rest) -> sexprList (skipSpace rest) []
  Just (')', _)    -> Left "Unexpected ')'"
  Just ('"', rest) -> sexprString rest []
  Just (c, _)
    | c == '-' || (c >= '0' && c <= '9') -> sexprNumber t
    | otherwise -> sexprAtom t

sexprList :: Text -> [PySExpr] -> Either Text (PySExpr, Text)
sexprList t acc = case T.uncons t of
  Nothing -> Left "Unterminated '(' — expected ')'"
  Just (')', rest) -> Right (SList (reverse acc), rest)
  _ -> case sexpr t of
         Left err -> Left err
         Right (e, rest) -> sexprList (skipSpace rest) (e : acc)

sexprString :: Text -> [Char] -> Either Text (PySExpr, Text)
sexprString t acc = case T.uncons t of
  Nothing -> Left "Unterminated string literal"
  Just ('"', rest) -> Right (SStr (T.pack (reverse acc)), rest)
  Just ('\\', rest) -> case T.uncons rest of
    Nothing -> Left "Trailing backslash in string literal"
    Just (c, rest') ->
      let real = case c of
            'n'  -> '\n'
            't'  -> '\t'
            'r'  -> '\r'
            '"'  -> '"'
            '\\' -> '\\'
            other -> other
      in sexprString rest' (real : acc)
  Just (c, rest) -> sexprString rest (c : acc)

sexprNumber :: Text -> Either Text (PySExpr, Text)
sexprNumber t =
  let (digits, rest) = T.span isNumChar t
  in case reads (T.unpack digits) :: [(Integer, String)] of
       [(n, "")] -> Right (SInt n, rest)
       _         -> Left $ "Invalid integer literal: " <> digits
  where
    isNumChar c = c == '-' || (c >= '0' && c <= '9')

sexprAtom :: Text -> Either Text (PySExpr, Text)
sexprAtom t =
  let (atom, rest) = T.span isAtomChar t
  in if T.null atom
     then Left $ "Expected atom at: " <> T.take 20 t
     else Right (SAtom atom, rest)
  where
    isAtomChar c = c /= '(' && c /= ')' && c /= '"'
                && c /= ' ' && c /= '\n' && c /= '\t' && c /= '\r'

skipSpace :: Text -> Text
skipSpace = T.dropWhile (\c -> c == ' ' || c == '\n' || c == '\t' || c == '\r')
