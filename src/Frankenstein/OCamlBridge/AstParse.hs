-- | Parser for @ocamlc -stop-after parsing -dparsetree@ output.
--
-- The format is an indentation-based tree: every line has some
-- leading-space indent and a head keyword (e.g. @structure_item@,
-- @Pexp_apply@, @Ppat_var "name"@) plus optional quoted-string
-- arguments and a trailing @(file[L,C+O]..[L,C+O])@ location. Child
-- nodes appear on subsequent lines at a deeper indent.
--
-- We build a very flat 'OLine' tree: each node is the cleaned head
-- text of one line, plus the list of child nodes that are indented
-- strictly beneath it. Semantic interpretation lives in
-- 'Frankenstein.OCamlBridge.CoreTranslate'.
module Frankenstein.OCamlBridge.AstParse
  ( OLine(..)
  , parseOCamlParsetree
  , oHead
  , oChildren
  , oString
  , oInt
  , findHead
  , findHeads
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isDigit)

-- | A node in the indentation tree.
data OLine = OLine
  { oLineText :: !Text      -- ^ Cleaned head text of this line (no location).
  , oLineKids :: ![OLine]   -- ^ Child nodes (lines at deeper indent).
  } deriving (Show)

oHead :: OLine -> Text
oHead = oLineText

oChildren :: OLine -> [OLine]
oChildren = oLineKids

-- | Extract the first @"..."@ token from a head line.
oString :: OLine -> Maybe Text
oString l =
  let t = oLineText l
      (_, rest) = T.breakOn "\"" t
  in if T.null rest then Nothing
     else let (s, _) = T.breakOn "\"" (T.drop 1 rest)
          in Just s

-- | Extract a leading integer from the first @(N,...)@ group on the line.
oInt :: OLine -> Maybe Integer
oInt l =
  let t = oLineText l
      (_, rest) = T.breakOn "(" t
  in case T.uncons rest of
       Just ('(', r) ->
         let digits = T.takeWhile (\c -> isDigit c || c == '-') r
         in case reads (T.unpack digits) :: [(Integer, String)] of
              [(i, "")] -> Just i
              _         -> Nothing
       _ -> Nothing

-- | Find the first child whose head starts with the given keyword.
findHead :: Text -> OLine -> Maybe OLine
findHead kw l = case findHeads kw l of
  (x:_) -> Just x
  []    -> Nothing

-- | Find all children whose head starts with the given keyword.
findHeads :: Text -> OLine -> [OLine]
findHeads kw l = filter (T.isPrefixOf kw . oLineText) (oLineKids l)

-- ---------------------------------------------------------------------------
-- Parser

-- | Parse ocamlc -dparsetree output into a single synthetic root
-- 'OLine' whose children are the top-level structure items.
parseOCamlParsetree :: Text -> Either Text OLine
parseOCamlParsetree txt =
  let rawLines = [ (indentOf l, cleanLine l)
                 | l <- T.lines txt
                 , not (T.null (T.strip l))
                 ]
      -- Replace "[" with a synthetic "<list>" marker (so its content
      -- remains grouped as children of the list node rather than
      -- drifting into the preceding sibling's subtree). Drop "]".
      filtered = [ (ind, if cl == "[" then "<list>" else cl)
                 | (ind, cl) <- rawLines
                 , not (T.null cl)
                 , cl /= "]"
                 ]
  in Right $ OLine "<root>" (buildForest 0 filtered [])
  where
    -- buildForest :: minIndent -> remaining -> acc -> [OLine]
    -- (we consume lines whose indent > minIndent as children of
    --  the current parent; siblings at that minIndent are gathered
    --  sequentially)
    buildForest _ [] acc = reverse acc
    buildForest minI rows@((ind, txt'):rest) acc
      | ind < minI  = reverse acc  -- unwind to parent
      | ind == minI =
          let (kids, rest') = collectKids ind rest
              node = OLine txt' kids
          in buildForest minI rest' (node : acc)
      | otherwise =
          -- First row has a deeper indent than expected; treat it as
          -- part of the current level anyway.
          let (kids, rest') = collectKids ind rest
              node = OLine txt' kids
          in buildForest ind rest' (node : acc)

    collectKids parentInd rows =
      case rows of
        ((ind, _):_) | ind > parentInd ->
          let kids = buildForest ind rows []
              rest = dropWhile (\(i,_) -> i > parentInd) rows
          in (kids, rest)
        _ -> ([], rows)

-- | Count leading spaces. OCaml parsetree uses 2-space indentation.
indentOf :: Text -> Int
indentOf = T.length . T.takeWhile (== ' ')

-- | Strip trailing @(file[...]..[...])@ locations and the @ghost@ marker.
cleanLine :: Text -> Text
cleanLine l =
  let stripped = T.strip l
      noGhost = T.replace " ghost" "" stripped
  in T.strip (dropTrailingLoc noGhost)

-- | Drop a trailing " (.../file[...]...)" location group if present.
-- Strategy: find the last " (" in the line; if the suffix starting
-- there contains a '[' and ends with ')', strip it.
dropTrailingLoc :: Text -> Text
dropTrailingLoc t =
  case T.breakOnEnd " (" t of
    (pfx, suf)
      | not (T.null pfx)
      , T.isSuffixOf ")" suf
      , "[" `T.isInfixOf` suf
      -> T.dropEnd 2 pfx  -- drop the " ("
    _ -> t
