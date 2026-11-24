{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Module: Frankenstein.Backpack.HsigBridge
-- Description: Ingestion bridge lowering GHC .hsig files into 1ML OpenRecordSig ModuleSigs
--
-- Enables existing @.hsig@ signature corpora (such as @Smirk.Target.Sig@ and
-- @Cek.Env.Sig@) to be compiled natively through Ingolstadt's in-compiler
-- 'OpenRecordSig' and 'StageInstantiate' pipeline with zero GHC Backpack friction.
module Frankenstein.Backpack.HsigBridge
  ( HsigParseError(..)
  , countArrowArity
  , parseLaw
  , parseHsigToOpenRecord
  ) where

import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Frankenstein.Backpack.OpenRecordSig
  ( ModuleSig (..)
  , TypeFieldSpec (..)
  , ValueFieldSpec (..)
  , emptyModuleSig
  )
import Frankenstein.Core.Interface (MemProfile (..))
import Frankenstein.Verify.LiquidProfile (ArithExpr (..), RefPred (..))

-- | Structured parse error returned when lowering a @.hsig@ source file.
data HsigParseError
  = MissingSignatureHeader
  | MalformedDeclaration !Int !Text
  deriving stock (Eq, Show)

-- | Infers canonical function arity by counting top-level @->@ separators
-- outside parenthesized higher-order arguments.
countArrowArity :: Text -> Int
countArrowArity = go (0 :: Int) (0 :: Int)
  where
    go !depth !arrows txt =
      case T.uncons txt of
        Nothing -> arrows
        Just ('(', rest) -> go (depth + 1) arrows rest
        Just (')', rest) -> go (max 0 (depth - 1)) arrows rest
        Just ('-', rest)
          | depth == 0 && T.isPrefixOf ">" rest ->
              go 0 (arrows + 1) (T.drop 1 rest)
        Just (_, rest) -> go depth arrows rest

-- | Parses a simple linear integer inequality into a 'RefPred'.
parseLaw :: Text -> Maybe RefPred
parseLaw raw =
  case T.words (T.strip raw) of
    ["0", "<=", var, "<", len] ->
      Just (PAnd (PLe (AInt 0) (AVar var)) (PLt (AVar var) (AVar len)))
    ["0", "<=", var] ->
      Just (PLe (AInt 0) (AVar var))
    [var, "<", len] ->
      Just (PLt (AVar var) (AVar len))
    [lhs, "==", rhs] ->
      Just (PEq (AVar lhs) (AVar rhs))
    _ -> Nothing

-- | Parses a GHC @.hsig@ file (with optional @--@ profile:@ and @--@ law:@ annotations)
-- into a 'ModuleSig'.
parseHsigToOpenRecord :: Text -> Either HsigParseError ModuleSig
parseHsigToOpenRecord src =
  let numbered = zip ([1 ..] :: [Int]) (map T.strip (T.lines src))
  in case findHeader numbered of
       Nothing -> Left MissingSignatureHeader
       Just (sigName, restLines) ->
         foldl' parseLine (Right (emptyModuleSig sigName, Profile1ThreadLocalPerceus)) restLines
           >>= \(sig, _) -> Right sig
  where
    findHeader [] = Nothing
    findHeader ((_, line) : ls)
      | Just afterSig <- T.stripPrefix "signature " line =
          let namePart = T.strip (fst (T.breakOn " where" afterSig))
          in Just (namePart, ls)
      | otherwise = findHeader ls

    parseLine (Left err) _ = Left err
    parseLine (Right (sig, curProf)) (lineNo, line)
      | T.null line = Right (sig, curProf)
      | Just profTxt <- T.stripPrefix "--@ profile:" line =
          Right (sig, parseProfile (T.strip profTxt))
      | Just lawTxt <- T.stripPrefix "--@ law:" line =
          case parseLaw (T.strip lawTxt) of
            Just law -> Right (sig { msLaws = msLaws sig ++ [law] }, curProf)
            Nothing  -> Right (sig, curProf)
      | T.isPrefixOf "--" line = Right (sig, curProf)
      | Just rest <- T.stripPrefix "data " line =
          case T.words rest of
            (tName : vars) ->
              let spec = TypeOpaque (length vars)
              in Right (sig { msTypes = Map.insert tName spec (msTypes sig) }, curProf)
            [] -> Left (MalformedDeclaration lineNo line)
      | Just rest <- T.stripPrefix "type " line =
          let (lhs, rhsRaw) = T.breakOn "=" rest
          in case (T.words (T.strip lhs), T.stripPrefix "=" (T.strip rhsRaw)) of
               (tName : vars, Just repTxt) ->
                 let spec = TypeManifest (length vars) (T.strip repTxt)
                 in Right (sig { msTypes = Map.insert tName spec (msTypes sig) }, curProf)
               _ -> Left (MalformedDeclaration lineNo line)
      | T.isInfixOf "::" line =
          let (lhs, rhsRaw) = T.breakOn "::" line
              vName = T.strip lhs
              vType = T.strip (T.drop 2 rhsRaw)
          in if T.null vName || T.null vType
               then Left (MalformedDeclaration lineNo line)
               else
                 let vSpec = ValueFieldSpec
                       { vfsArity       = countArrowArity vType
                       , vfsTypeSummary = vType
                       , vfsMinProfile  = curProf
                       , vfsInlineHint  = True
                       }
                 in Right (sig { msValues = Map.insert vName vSpec (msValues sig) }, curProf)
      | otherwise = Right (sig, curProf)

    parseProfile "freestanding_linear" = Profile0FreestandingLinear
    parseProfile "concurrent_shared"   = Profile2ConcurrentShared
    parseProfile _                     = Profile1ThreadLocalPerceus
