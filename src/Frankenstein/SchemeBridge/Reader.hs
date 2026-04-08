-- | Scheme reader: text → S-expression AST.
--
-- Hand-rolled, no external dependencies. Recognises:
--
--   * Parenthesised lists
--   * Integer literals (with optional unary minus)
--   * Identifiers (Scheme is liberal — almost any non-paren, non-quote
--     character may appear in a symbol)
--   * Line comments (@; ...@)
--
-- This is intentionally a *reader* (Lisp terminology), not a parser of
-- specific Scheme forms — that job belongs to 'CoreTranslate'.

module Frankenstein.SchemeBridge.Reader
  ( SExpr(..)
  , readSchemeFile
  , readScheme
  , readSExprs
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isDigit, isSpace)

data SExpr
  = SInt   !Integer
  | SSym   !Text
  | SList  ![SExpr]
  deriving (Show, Eq)

readSchemeFile :: FilePath -> IO (Either Text [SExpr])
readSchemeFile path = do
  src <- TIO.readFile path
  pure (readScheme src)

readScheme :: Text -> Either Text [SExpr]
readScheme = readSExprs

readSExprs :: Text -> Either Text [SExpr]
readSExprs t = go (skipTrivia t) []
  where
    go s acc
      | T.null s = Right (reverse acc)
      | otherwise = case readOne s of
          Left err -> Left err
          Right (e, rest) -> go (skipTrivia rest) (e : acc)

readOne :: Text -> Either Text (SExpr, Text)
readOne t0 =
  let t = skipTrivia t0
  in case T.uncons t of
       Nothing -> Left "Unexpected end of input"
       Just ('(', rest) -> readSList (skipTrivia rest) []
       Just (')', _)    -> Left "Unexpected ')'"
       Just (c, _)
         | isDigit c -> readInt t
         | c == '-' -> case T.uncons (T.drop 1 t) of
             Just (c', _) | isDigit c' -> readInt t
             _ -> readSymbol t
         | otherwise -> readSymbol t

readSList :: Text -> [SExpr] -> Either Text (SExpr, Text)
readSList t acc = case T.uncons t of
  Nothing -> Left "Unterminated '(' — expected ')'"
  Just (')', rest) -> Right (SList (reverse acc), rest)
  _ -> case readOne t of
         Left err        -> Left err
         Right (e, rest) -> readSList (skipTrivia rest) (e : acc)

readInt :: Text -> Either Text (SExpr, Text)
readInt t =
  let (digs, rest) = T.span isIntChar t
  in case reads (T.unpack digs) :: [(Integer, String)] of
       [(n, "")] -> Right (SInt n, rest)
       _         -> Left $ "Invalid integer literal: " <> digs
  where
    isIntChar c = c == '-' || isDigit c

-- | Scheme symbols are very permissive — anything that isn't whitespace,
-- a paren, a comment marker, or a string quote.
readSymbol :: Text -> Either Text (SExpr, Text)
readSymbol t =
  let (sym, rest) = T.span isSymbolChar t
  in if T.null sym
     then Left $ "Expected symbol at: " <> T.take 20 t
     else Right (SSym sym, rest)
  where
    isSymbolChar c =
      not (isSpace c) && c /= '(' && c /= ')' && c /= ';' && c /= '"'

skipTrivia :: Text -> Text
skipTrivia t = case T.uncons t of
  Nothing -> t
  Just (c, _) | isSpace c -> skipTrivia (T.dropWhile isSpace t)
  Just (';', _) -> skipTrivia (T.dropWhile (/= '\n') t)
  _ -> t
