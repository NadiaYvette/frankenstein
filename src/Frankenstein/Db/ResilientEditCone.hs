{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Db.ResilientEditCone
-- Description: Stope-and-winze ParseHole isolation and declaration-level edit-cone diffing
--
-- Isolates mid-edit syntax damage inside localized 'DeclParseHole' spans so
-- every valid sibling declaration retains its cached @OrganIR@ lowering and
-- @.organ-iface@ signature hash during live editing.
module Frankenstein.Db.ResilientEditCone
  ( DeclState(..)
  , ResilientDecl(..)
  , ResilientModule(..)
  , EditConeReport(..)
  , parseResilientModule
  , reparseEditCone
  , validSiblingDecls
  , collectParseHoles
  , syncModuleIntoCodeDb
  , resilientModuleToInterface
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import System.FilePath (takeBaseName)

import Frankenstein.Core.Interface
  ( ExportAbi (..)
  , MemProfile (..)
  , OrganInterface (..)
  , fnv1a64
  )
import Frankenstein.Db.Incremental
  ( CodeDb (..)
  , Durability (..)
  , MemoEntry (..)
  , QueryKey (..)
  , recordMemoWithCutoff
  )
import Frankenstein.Transform.Api (SourceSpan (..))

-- | Resilience classification of a single top-level declaration block.
data DeclState
  = DeclValid
  | DeclParseHole !Text
    -- ^ Isolated syntax error reason (never aborts sibling declarations).
  deriving stock (Eq, Show)

-- | Single top-level declaration with separate signature and body hashes for early cutoff.
data ResilientDecl = ResilientDecl
  { rdName      :: !Text
  , rdSpan      :: !SourceSpan
  , rdSigText   :: !Text
  , rdSigHash   :: {-# UNPACK #-} !Word64
  , rdBodyHash  :: {-# UNPACK #-} !Word64
  , rdState     :: !DeclState
  } deriving stock (Eq, Show)

-- | Resilient full-file module CST indexed by declaration name.
data ResilientModule = ResilientModule
  { rmFilePath :: !FilePath
  , rmDecls    :: ![ResilientDecl]
  , rmByName   :: !(Map Text ResilientDecl)
  } deriving stock (Eq, Show)

-- | Partition of an incremental edit's blast radius across a module.
data EditConeReport = EditConeReport
  { ecrPreservedSiblings :: ![Text]
    -- ^ Untouched declarations reused verbatim from cache.
  , ecrEarlyCutoffDecls  :: ![Text]
    -- ^ Body/comment edited, but @rdSigHash@ unchanged (0 downstream invalidation).
  , ecrInvalidatedDecls  :: ![Text]
    -- ^ Added, removed, or signature-changed declarations requiring downstream recheck.
  , ecrParseHoles        :: ![(Text, SourceSpan, Text)]
    -- ^ Active localized syntax holes @(declName, span, reason)@.
  } deriving stock (Eq, Show)

-- | Parses source text into a 'ResilientModule' split on blank-line declaration blocks,
-- sealing delimiter/quote errors strictly inside the offending declaration's 'DeclParseHole'.
parseResilientModule :: FilePath -> Text -> ResilientModule
parseResilientModule fp src =
  let blocks = splitBlocks 0 (T.lines src)
      decls  = zipWith (classifyBlock fp) ([1 ..] :: [Int]) blocks
      byName = Map.fromList [(rdName d, d) | d <- decls]
  in ResilientModule fp decls byName
  where
    splitBlocks _ [] = []
    splitBlocks !off ls =
      let (blanks, rest) = span (T.null . T.strip) ls
          !off1 = off + sum (map (\l -> T.length l + 1) blanks)
      in case rest of
           [] -> []
           _  ->
             let (blk, tailLs) = break (T.null . T.strip) rest
                 !blkLen = sum (map (\l -> T.length l + 1) blk)
                 !blkEnd = min (T.length src) (off1 + blkLen)
             in (off1, blkEnd, blk) : splitBlocks (off1 + blkLen) tailLs

    classifyBlock filePath idx (sOff, eOff, blkLines) =
      let codeLines = filter (not . isCommentLine) blkLines
          fullCode  = T.unlines codeLines
          header    = case codeLines of
                        (h : _) -> T.strip h
                        []      -> ""
          dName     = extractDeclName idx header
          sigTxt    = extractSignature header codeLines
          st        = checkBlockSyntax fullCode
      in ResilientDecl
           { rdName     = dName
           , rdSpan     = SourceSpan filePath sOff eOff
           , rdSigText  = sigTxt
           , rdSigHash  = fnv1a64 sigTxt
           , rdBodyHash = fnv1a64 fullCode
           , rdState    = st
           }

    isCommentLine l =
      let t = T.stripStart l
      in T.isPrefixOf "--" t || T.isPrefixOf "//" t

    extractDeclName idx header =
      let ws = T.words header
      in case ws of
           ("fn" : n : _)     -> cleanIdent n
           ("pub" : "fn" : n : _) -> cleanIdent n
           ("data" : n : _)   -> cleanIdent n
           ("type" : n : _)   -> cleanIdent n
           (n : "::" : _)     -> cleanIdent n
           (n : _) | not (T.null (cleanIdent n)) -> cleanIdent n
           _                  -> "anon_decl_" <> T.pack (show idx)

    cleanIdent = T.takeWhile (\c -> c /= '(' && c /= ':' && c /= '=' && c /= '{')

    extractSignature header codeLines =
      case filter (T.isInfixOf "::") codeLines of
        (sigL : _) -> T.strip sigL
        []         -> header

    checkBlockSyntax txt
      | T.isInfixOf "SYNTAX_ERROR" txt = DeclParseHole "explicit syntax error marker"
      | otherwise = checkDelims (0 :: Int) (0 :: Int) False txt

    checkDelims !parens !braces !inStr remTxt =
      case T.uncons remTxt of
        Nothing
          | inStr      -> DeclParseHole "unterminated string literal"
          | parens /= 0 -> DeclParseHole "unbalanced parentheses"
          | braces /= 0 -> DeclParseHole "unbalanced braces"
          | otherwise  -> DeclValid
        Just ('"', rest) -> checkDelims parens braces (not inStr) rest
        Just (_, rest) | inStr -> checkDelims parens braces True rest
        Just ('(', rest) -> checkDelims (parens + 1) braces False rest
        Just (')', rest) ->
          if parens <= 0
            then DeclParseHole "unexpected closing parenthesis"
            else checkDelims (parens - 1) braces False rest
        Just ('{', rest) -> checkDelims parens (braces + 1) False rest
        Just ('}', rest) ->
          if braces <= 0
            then DeclParseHole "unexpected closing brace"
            else checkDelims parens (braces - 1) False rest
        Just (_, rest) -> checkDelims parens braces False rest

-- | Computes the exact incremental edit cone between @oldMod@ and @newMod@.
reparseEditCone :: ResilientModule -> ResilientModule -> EditConeReport
reparseEditCone oldMod newMod =
  let (presRev, cutRev, invRev) = foldl' classifyNew ([], [], []) (rmDecls newMod)
      removed =
        [ rdName d
        | d <- rmDecls oldMod
        , not (Map.member (rdName d) (rmByName newMod))
        ]
  in EditConeReport
       { ecrPreservedSiblings = reverse presRev
       , ecrEarlyCutoffDecls  = reverse cutRev
       , ecrInvalidatedDecls  = reverse invRev ++ removed
       , ecrParseHoles        = collectParseHoles newMod
       }
  where
    classifyNew (pres, cut, inv) newD =
      let nm = rdName newD
      in case rdState newD of
           DeclParseHole _ -> (pres, cut, nm : inv)
           DeclValid ->
             case Map.lookup nm (rmByName oldMod) of
               Nothing -> (pres, cut, nm : inv)
               Just oldD
                 | rdState oldD /= DeclValid -> (pres, cut, nm : inv)
                 | rdSigHash oldD == rdSigHash newD && rdBodyHash oldD == rdBodyHash newD ->
                     (nm : pres, cut, inv)
                 | rdSigHash oldD == rdSigHash newD ->
                     (pres, nm : cut, inv)
                 | otherwise ->
                     (pres, cut, nm : inv)

-- | Returns every valid sibling declaration unaffected by localized parse holes.
validSiblingDecls :: ResilientModule -> [ResilientDecl]
validSiblingDecls rm = [d | d <- rmDecls rm, rdState d == DeclValid]

-- | Extracts all active 'DeclParseHole' diagnostics with exact source spans.
collectParseHoles :: ResilientModule -> [(Text, SourceSpan, Text)]
collectParseHoles rm =
  [ (rdName d, rdSpan d, msg)
  | d <- rmDecls rm
  , DeclParseHole msg <- [rdState d]
  ]

-- | Synchronizes valid module declarations into 'CodeDb', recording signature memos
-- and returning an 'EditConeReport'.
syncModuleIntoCodeDb :: ResilientModule -> CodeDb -> (EditConeReport, CodeDb)
syncModuleIntoCodeDb rm db =
  let fp = rmFilePath rm
      keySrc = QSourceText fp
      (pres, cut, inv, updatedDb) = foldl' (syncOne fp keySrc) ([], [], [], db) (rmDecls rm)
      holes = collectParseHoles rm
      report = EditConeReport
        { ecrPreservedSiblings = reverse pres
        , ecrEarlyCutoffDecls  = reverse cut
        , ecrInvalidatedDecls  = reverse inv
        , ecrParseHoles        = holes
        }
  in (report, updatedDb)
  where
    syncOne fp keySrc (pres, cut, inv, curDb) d =
      let nm = rdName d
          keySig = QDeclSignature fp nm
      in case rdState d of
           DeclParseHole _ ->
             (pres, cut, nm : inv, curDb)
           DeclValid ->
             let (hitCutoff, nextDb) =
                   recordMemoWithCutoff keySig DurabilityLow [keySrc] (rdSigText d) curDb
             in if hitCutoff
                  then (pres, nm : cut, inv, nextDb)
                  else
                    case Map.lookup keySig (dbMemos curDb) of
                      Nothing -> (pres, cut, nm : inv, nextDb)
                      Just old | meValueHash old == rdSigHash d -> (nm : pres, cut, inv, nextDb)
                      _ -> (pres, cut, nm : inv, nextDb)

-- | Projects all valid sibling declarations cleanly into an 'OrganInterface'
-- header, discarding parse holes so broken declarations never corrupt module ABI.
resilientModuleToInterface :: ResilientModule -> OrganInterface
resilientModuleToInterface rm =
  let validDecls = validSiblingDecls rm
      exports = [ ExportAbi (rdName d) 0 0 Profile0FreestandingLinear False | d <- validDecls ]
      modName = T.pack (takeBaseName (rmFilePath rm))
  in OrganInterface
       { oiSchemaVersion = "1.0.0"
       , oiModuleName    = modName
       , oiSourceLang    = "haskell"
       , oiCtors         = []
       , oiExports       = exports
       , oiImports       = []
       }
