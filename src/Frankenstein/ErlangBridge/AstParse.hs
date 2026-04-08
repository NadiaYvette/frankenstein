-- | Parser for Erlang term output (a stream of @{...}@ tuples,
-- @[...]@ lists, atoms, integers, and quoted atoms like @'N'@).
--
-- The AST dumper in 'Frankenstein.ErlangBridge.Driver' runs
-- @erl_scan:string/1@ and @erl_parse:parse_form/1@ inside an escript
-- and prints the resulting list of forms with @~p@, so what we parse
-- here is just an Erlang term.
module Frankenstein.ErlangBridge.AstParse
  ( ETerm(..)
  , parseErlangTerms
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit, isAlpha, isAlphaNum, isSpace)

-- | A minimal Erlang term tree.
data ETerm
  = ETTuple   ![ETerm]       -- ^ @{a,b,c}@
  | ETList    ![ETerm]       -- ^ @[a,b,c]@
  | ETAtom    !Text          -- ^ unquoted atom like @fact@ or quoted @'N'@
  | ETInt     !Integer       -- ^ integer literal
  | ETString  !Text          -- ^ double-quoted string (rarely used here)
  deriving (Show, Eq)

-- | Parse one top-level Erlang term. The escript prints a single
-- list of forms, so we only need to parse one term.
parseErlangTerms :: Text -> Either Text ETerm
parseErlangTerms txt = case runP parseTerm (T.unpack (T.strip txt)) of
  Right (t, rest)
    | all isSpace rest -> Right t
    | otherwise        -> Right t  -- tolerate trailing whitespace/newlines
  Left err             -> Left (T.pack err)

-- ---------------------------------------------------------------------------
-- Tiny parser combinator over String (the inputs are small enough).

newtype P a = P { runP :: String -> Either String (a, String) }

instance Functor P where
  fmap f (P p) = P $ \s -> case p s of
    Right (a, s') -> Right (f a, s')
    Left err      -> Left err

parseTerm :: P ETerm
parseTerm = P $ \s0 ->
  let s = dropWhile isSpace s0
  in case s of
       [] -> Left "unexpected end of input"
       ('{':rest) -> do
         (elems, r) <- parseCommaSep '}' rest
         Right (ETTuple elems, r)
       ('[':rest) -> do
         (elems, r) <- parseCommaSep ']' rest
         Right (ETList elems, r)
       ('"':rest) ->
         let (body, r) = span (/= '"') rest
         in case r of
              ('"':r') -> Right (ETString (T.pack body), r')
              _        -> Left "unterminated string"
       ('\'':rest) ->
         let (body, r) = break (== '\'') rest
         in case r of
              ('\'':r') -> Right (ETAtom (T.pack body), r')
              _         -> Left "unterminated quoted atom"
       (c:_)
         | isDigit c || c == '-' -> runP parseInt s
         | isAlpha c || c == '_' -> runP parseAtom s
         | otherwise -> Left ("unexpected char: " <> take 10 s)

-- | Parse a comma-separated list terminated by the given closing char.
-- The opening bracket has already been consumed.
parseCommaSep :: Char -> String -> Either String ([ETerm], String)
parseCommaSep close s0 = go [] s0
  where
    go acc s = case dropWhile isSpace s of
      (c:rest) | c == close -> Right (reverse acc, rest)
      _ -> do
        (t, s') <- runP parseTerm s
        case dropWhile isSpace s' of
          (',':rest) -> go (t : acc) rest
          (c:rest) | c == close -> Right (reverse (t : acc), rest)
          other -> Left ("parseCommaSep: unexpected " <> take 10 other)

parseInt :: P ETerm
parseInt = P $ \s ->
  let (numStr, rest) = span (\c -> isDigit c || c == '-') s
  in case reads numStr :: [(Integer, String)] of
       [(i, "")] -> Right (ETInt i, rest)
       _         -> Left ("bad integer: " <> numStr)

parseAtom :: P ETerm
parseAtom = P $ \s ->
  let (nm, rest) = span (\c -> isAlphaNum c || c == '_' || c == '@') s
  in if null nm
     then Left "empty atom"
     else Right (ETAtom (T.pack nm), rest)
