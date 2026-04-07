-- | Go AST parser bridge.
--
-- Drives the @go-bridge/ast_to_sexp@ helper (a small Go program that uses
-- @go/parser@ from the standard library) and parses its single-line
-- S-expression output. The helper is auto-built on first invocation if
-- the binary is missing but the source is present.

module Frankenstein.GoBridge.AstParse
  ( GoSExpr(..)
  , parseGo
  , parseSExpr
  , runGoHelper
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (doesFileExist)
import Control.Exception (try, SomeException)

data GoSExpr
  = SAtom !Text
  | SStr  !Text
  | SInt  !Integer
  | SList ![GoSExpr]
  deriving (Show, Eq)

parseGo :: FilePath -> IO (Either Text GoSExpr)
parseGo path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "Go source file does not exist: " <> T.pack path
    else do
      r <- runGoHelper path
      case r of
        Left err  -> pure $ Left err
        Right txt -> pure $ parseSExpr txt

-- | Run the Go helper. If the prebuilt binary is missing but the source
-- file is present, build it once via @go build@.
runGoHelper :: FilePath -> IO (Either Text Text)
runGoHelper path = do
  let helperBin = "go-bridge/ast_to_sexp"
      helperSrc = "go-bridge/ast_to_sexp.go"
  binExists <- doesFileExist helperBin
  buildOk <- if binExists
             then pure True
             else do
               srcExists <- doesFileExist helperSrc
               if not srcExists
                 then pure False
                 else do
                   eBuild <- try $ readProcessWithExitCode "go"
                              ["build", "-o", helperBin, helperSrc] ""
                   case eBuild :: Either SomeException (ExitCode, String, String) of
                     Right (ExitSuccess, _, _) -> pure True
                     _ -> pure False
  if not buildOk
    then pure $ Left $ "Go helper missing and could not be built: " <> T.pack helperBin
    else do
      eRes <- try $ readProcessWithExitCode ("./" <> helperBin) [path] ""
      case eRes :: Either SomeException (ExitCode, String, String) of
        Left ex -> pure $ Left $ "Failed to run go helper: " <> T.pack (show ex)
        Right (ExitFailure n, _out, err) ->
          pure $ Left $ "Go helper exited with " <> T.pack (show n) <> ": " <> T.pack err
        Right (ExitSuccess, out, _err) ->
          pure $ Right $ T.strip $ T.pack out

-- ---------------------------------------------------------------------------
-- S-expression parser (same shape as PythonBridge.AstParse.parseSExpr).

parseSExpr :: Text -> Either Text GoSExpr
parseSExpr input = case sexpr (T.dropWhile isSpaceCh input) of
  Left err -> Left err
  Right (e, rest) ->
    let rest' = T.dropWhile isSpaceCh rest
    in if T.null rest'
       then Right e
       else Left $ "Trailing input after S-expression: " <> T.take 40 rest'

isSpaceCh :: Char -> Bool
isSpaceCh c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

sexpr :: Text -> Either Text (GoSExpr, Text)
sexpr t = case T.uncons t of
  Nothing -> Left "Unexpected end of input"
  Just ('(', rest) -> sexprList (skipSpace rest) []
  Just (')', _)    -> Left "Unexpected ')'"
  Just ('"', rest) -> sexprString rest []
  Just (c, _)
    | c == '-' || (c >= '0' && c <= '9') -> sexprNumber t
    | otherwise -> sexprAtom t

sexprList :: Text -> [GoSExpr] -> Either Text (GoSExpr, Text)
sexprList t acc = case T.uncons t of
  Nothing -> Left "Unterminated '(' — expected ')'"
  Just (')', rest) -> Right (SList (reverse acc), rest)
  _ -> case sexpr t of
         Left err -> Left err
         Right (e, rest) -> sexprList (skipSpace rest) (e : acc)

sexprString :: Text -> [Char] -> Either Text (GoSExpr, Text)
sexprString t acc = case T.uncons t of
  Nothing -> Left "Unterminated string literal"
  Just ('"', rest) -> Right (SStr (T.pack (reverse acc)), rest)
  Just ('\\', rest) -> case T.uncons rest of
    Nothing -> Left "Trailing backslash in string literal"
    Just (c, rest') ->
      let real = case c of
            'n' -> '\n'; 't' -> '\t'; 'r' -> '\r'
            '"' -> '"';  '\\' -> '\\'; other -> other
      in sexprString rest' (real : acc)
  Just (c, rest) -> sexprString rest (c : acc)

sexprNumber :: Text -> Either Text (GoSExpr, Text)
sexprNumber t =
  let (digits, rest) = T.span isNumChar t
  in case reads (T.unpack digits) :: [(Integer, String)] of
       [(n, "")] -> Right (SInt n, rest)
       _         -> Left $ "Invalid integer literal: " <> digits
  where
    isNumChar c = c == '-' || (c >= '0' && c <= '9')

sexprAtom :: Text -> Either Text (GoSExpr, Text)
sexprAtom t =
  let (atom, rest) = T.span isAtomChar t
  in if T.null atom
     then Left $ "Expected atom at: " <> T.take 20 t
     else Right (SAtom atom, rest)
  where
    isAtomChar c = c /= '(' && c /= ')' && c /= '"'
                && c /= ' ' && c /= '\n' && c /= '\t' && c /= '\r'

skipSpace :: Text -> Text
skipSpace = T.dropWhile isSpaceCh
