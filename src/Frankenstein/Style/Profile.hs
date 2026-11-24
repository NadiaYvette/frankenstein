{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Style.Profile
-- Description: Multi-lingual style profile, deterministic corpus learner, and Crichton cognitive scorer
--
-- Learns structural, naming, and idiomatic preferences from existing project
-- source files (Haskell, Rust, C11, OCaml, Lean 4) and scores semantically
-- equivalent candidates using Will Crichton's working-memory load model so
-- transpiled and transformed code matches human repository style safely.
module Frankenstein.Style.Profile
  ( NamingCase(..)
  , IdiomPrefs(..)
  , StyleProfile(..)
  , CognitiveMetrics(..)
  , defaultStyleForParadigm
  , splitIdentWords
  , formatIdentifier
  , learnStyleProfile
  , stylePenalty
  , rankEquivalentCandidates
  , styleProfileToDhall
  ) where

import Data.Char (isAlpha, isAlphaNum, isLower, isUpper, toLower, toUpper)
import Data.List (foldl', sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Frankenstein.Driver.Registry (ParadigmTag (..))

-- | Target identifier capitalization convention.
data NamingCase
  = SnakeCase
  | CamelCase
  | PascalCase
  | KebabCase
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Continuous preference weights in @[0.0, 1.0]@ steering equivalent idiom choice.
data IdiomPrefs = IdiomPrefs
  { ipPreferEarlyReturn      :: !Double
    -- ^ Favors guard-clause early returns over deeply nested @if@/@else@ trees.
  , ipPreferPipelineOverLoop :: !Double
    -- ^ Favors iterator/combinator pipelines over explicit index loops.
  , ipPreferWhereOverLet     :: !Double
    -- ^ Haskell/ML preference for trailing @where@ clauses vs inline @let@.
  , ipPreferPointFree        :: !Double
    -- ^ Preference for @(.)@/@($)@ composition when live bindings stay bounded.
  } deriving stock (Eq, Show)

-- | Multi-lingual coding style profile shared by idiomatic transpilers and transforms.
data StyleProfile = StyleProfile
  { spProfileName     :: !Text
  , spLineWidth       :: !Int
  , spIndentWidth     :: !Int
  , spMaxLiveBindings :: !Int
    -- ^ Will Crichton working-memory bound (default 5 live bindings).
  , spMaxNestingDepth :: !Int
  , spValueNaming     :: !NamingCase
  , spTypeNaming      :: !NamingCase
  , spIdiomPrefs      :: !IdiomPrefs
  , spExtraWeights    :: !(Map Text Double)
  } deriving stock (Eq, Show)

-- | Language-agnostic cognitive complexity measurements for a candidate AST/block.
data CognitiveMetrics = CognitiveMetrics
  { cmMaxLiveBindings :: !Int
  , cmMaxNestingDepth :: !Int
  , cmDefUseMaxSpan   :: !Int
  , cmUsedEarlyReturn :: !Bool
  , cmUsedPipeline    :: !Bool
  , cmUsedWhereClause :: !Bool
  } deriving stock (Eq, Show)

-- | Sensible paradigm defaults prior to corpus learning.
defaultStyleForParadigm :: ParadigmTag -> StyleProfile
defaultStyleForParadigm paradigm =
  case paradigm of
    ParadigmSystems ->
      StyleProfile
        { spProfileName     = "systems-default"
        , spLineWidth       = 100
        , spIndentWidth     = 4
        , spMaxLiveBindings = 5
        , spMaxNestingDepth = 3
        , spValueNaming     = SnakeCase
        , spTypeNaming      = PascalCase
        , spIdiomPrefs      = IdiomPrefs 0.85 0.60 0.20 0.10
        , spExtraWeights    = Map.empty
        }
    ParadigmFunctional ->
      StyleProfile
        { spProfileName     = "functional-default"
        , spLineWidth       = 100
        , spIndentWidth     = 2
        , spMaxLiveBindings = 5
        , spMaxNestingDepth = 3
        , spValueNaming     = CamelCase
        , spTypeNaming      = PascalCase
        , spIdiomPrefs      = IdiomPrefs 0.65 0.85 0.70 0.55
        , spExtraWeights    = Map.empty
        }
    _ ->
      StyleProfile
        { spProfileName     = "general-default"
        , spLineWidth       = 100
        , spIndentWidth     = 2
        , spMaxLiveBindings = 5
        , spMaxNestingDepth = 3
        , spValueNaming     = SnakeCase
        , spTypeNaming      = PascalCase
        , spIdiomPrefs      = IdiomPrefs 0.70 0.70 0.50 0.30
        , spExtraWeights    = Map.empty
        }

-- | Splits a snake_case, camelCase, PascalCase, or kebab-case identifier into lowercase words.
splitIdentWords :: Text -> [Text]
splitIdentWords raw =
  let chunks = filter (not . T.null) (T.split (\c -> c == '_' || c == '-' || c == '.' || c == ':') raw)
  in concatMap splitCamel chunks
  where
    splitCamel t =
      case T.uncons t of
        Nothing -> []
        Just (c0, rest) -> go [toLower c0] rest

    go acc t =
      case T.uncons t of
        Nothing -> [T.pack (reverse acc)]
        Just (c, rest)
          | isUpper c && not (null acc) ->
              T.pack (reverse acc) : go [toLower c] rest
          | otherwise ->
              go (toLower c : acc) rest

-- | Formats a canonical identifier into the requested 'NamingCase'.
formatIdentifier :: NamingCase -> Text -> Text
formatIdentifier nc raw =
  case splitIdentWords raw of
    [] -> raw
    ws@(w0 : wRest) ->
      case nc of
        SnakeCase  -> T.intercalate "_" ws
        KebabCase  -> T.intercalate "-" ws
        PascalCase -> T.concat (map capitalize ws)
        CamelCase  -> w0 <> T.concat (map capitalize wRest)
  where
    capitalize t =
      case T.uncons t of
        Nothing     -> T.empty
        Just (c, r) -> T.cons (toUpper c) r

-- | Deterministically mines a multi-lingual 'StyleProfile' from existing source files.
learnStyleProfile :: Text -> ParadigmTag -> [Text] -> StyleProfile
learnStyleProfile profName paradigm corpus =
  let base      = defaultStyleForParadigm paradigm
      allLines  = concatMap T.lines corpus
      indents   = [ leadingSpaces l | l <- allLines, leadingSpaces l > 0 ]
      inferredIndent =
        case indents of
          [] -> spIndentWidth base
          xs ->
            let twoCount  = length [n | n <- xs, n `mod` 4 == 2]
                fourCount = length [n | n <- xs, n `mod` 4 == 0]
            in if twoCount > 0 && (twoCount * 2 >= fourCount || fourCount == 0) then 2 else 4
      nonEmptyLens = sort [ T.length l | l <- allLines, not (T.null (T.strip l)) ]
      inferredWidth =
        case nonEmptyLens of
          [] -> spLineWidth base
          xs ->
            let p95 = xs !! ((length xs * 95) `div` 100)
            in max 80 (min 120 (((p95 + 9) `div` 10) * 10))
      (snakes, camels) = foldl' countNaming (0 :: Int, 0 :: Int) (concatMap extractIdents allLines)
      valCase
        | snakes == 0 && camels == 0 = spValueNaming base
        | snakes >= camels           = SnakeCase
        | otherwise                  = CamelCase
      whereHits    = countSubstr " where" allLines
      letHits      = countSubstr "let " allLines
      pipeHits     = countSubstr ".iter()" allLines + countSubstr " . " allLines + countSubstr "|>" allLines
      loopHits     = countSubstr "for " allLines + countSubstr "while " allLines
      guardHits    = countSubstr "return " allLines + countSubstr " | " allLines
      elseHits     = countSubstr "else " allLines
      ratio a b d  = if a + b == 0 then d else fromIntegral a / fromIntegral (a + b)
      prefs0       = spIdiomPrefs base
      prefs' = IdiomPrefs
        { ipPreferEarlyReturn      = ratio guardHits elseHits (ipPreferEarlyReturn prefs0)
        , ipPreferPipelineOverLoop = ratio pipeHits loopHits (ipPreferPipelineOverLoop prefs0)
        , ipPreferWhereOverLet     = ratio whereHits letHits (ipPreferWhereOverLet prefs0)
        , ipPreferPointFree        = ipPreferPointFree prefs0
        }
  in base
       { spProfileName = profName
       , spLineWidth   = inferredWidth
       , spIndentWidth = inferredIndent
       , spValueNaming = valCase
       , spIdiomPrefs  = prefs'
       }
  where
    leadingSpaces = T.length . T.takeWhile (== ' ')

    extractIdents line =
      filter (\tok -> T.length tok >= 3 && maybe False isAlpha (fst <$> T.uncons tok))
             (T.split (not . (\c -> isAlphaNum c || c == '_')) line)

    countNaming (!s, !c) tok
      | T.isInfixOf "_" tok && T.any isLower tok = (s + 1, c)
      | T.any isLower tok && T.any isUpper tok
        && maybe False isLower (fst <$> T.uncons tok) = (s, c + 1)
      | otherwise = (s, c)

    countSubstr needle = length . filter (T.isInfixOf needle)

-- | Computes a lower-is-better stylistic & Crichton cognitive penalty for an equivalent candidate.
stylePenalty :: StyleProfile -> CognitiveMetrics -> Double
stylePenalty sp cm =
  let liveExcess = max 0 (cmMaxLiveBindings cm - spMaxLiveBindings sp)
      nestExcess = max 0 (cmMaxNestingDepth cm - spMaxNestingDepth sp)
      -- Exponential penalty above Crichton's working-memory threshold.
      livePen    = fromIntegral (liveExcess * liveExcess) * 10.0
      nestPen    = fromIntegral nestExcess * 4.0
      spanPen    = fromIntegral (max 0 (cmDefUseMaxSpan cm - 20)) * 0.1
      prefs      = spIdiomPrefs sp
      guardAdj   = if cmUsedEarlyReturn cm then -(ipPreferEarlyReturn prefs * 2.0) else 0.0
      pipeAdj    = if cmUsedPipeline cm then -(ipPreferPipelineOverLoop prefs * 2.0) else 0.0
      whereAdj   = if cmUsedWhereClause cm then -(ipPreferWhereOverLet prefs * 1.5) else 0.0
  in max 0.0 (livePen + nestPen + spanPen + guardAdj + pipeAdj + whereAdj)

-- | Sorts candidate representations by their style penalty ascending (best match first).
rankEquivalentCandidates :: StyleProfile -> [(a, CognitiveMetrics)] -> [a]
rankEquivalentCandidates sp cands =
  map fst $ sortOn (stylePenalty sp . snd) cands

-- | Serializes a 'StyleProfile' to human-readable Dhall representation.
styleProfileToDhall :: StyleProfile -> Text
styleProfileToDhall sp =
  T.unlines
    [ "{"
    , "  profileName = \"" <> spProfileName sp <> "\","
    , "  lineWidth = " <> T.pack (show (spLineWidth sp)) <> ","
    , "  indentWidth = " <> T.pack (show (spIndentWidth sp)) <> ","
    , "  maxLiveBindings = " <> T.pack (show (spMaxLiveBindings sp)) <> ","
    , "  maxNestingDepth = " <> T.pack (show (spMaxNestingDepth sp)) <> ","
    , "  valueNaming = " <> namingToDhall (spValueNaming sp) <> ","
    , "  typeNaming = " <> namingToDhall (spTypeNaming sp) <> ","
    , "  idiomPrefs = {"
    , "    preferEarlyReturn = " <> T.pack (show (ipPreferEarlyReturn (spIdiomPrefs sp))) <> ","
    , "    preferPipelineOverLoop = " <> T.pack (show (ipPreferPipelineOverLoop (spIdiomPrefs sp))) <> ","
    , "    preferWhereOverLet = " <> T.pack (show (ipPreferWhereOverLet (spIdiomPrefs sp))) <> ","
    , "    preferPointFree = " <> T.pack (show (ipPreferPointFree (spIdiomPrefs sp)))
    , "  }"
    , "}"
    ]
  where
    namingToDhall SnakeCase  = "< SnakeCase | CamelCase | PascalCase | KebabCase >.SnakeCase"
    namingToDhall CamelCase  = "< SnakeCase | CamelCase | PascalCase | KebabCase >.CamelCase"
    namingToDhall PascalCase = "< SnakeCase | CamelCase | PascalCase | KebabCase >.PascalCase"
    namingToDhall KebabCase  = "< SnakeCase | CamelCase | PascalCase | KebabCase >.KebabCase"
