{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Ide.LiveSession
-- Description: Interactive code-in-database IDE session, live gargamelle Code Actions, and online style learning
--
-- Unifies 'Frankenstein.Db.Incremental', 'Frankenstein.Db.ResilientEditCone',
-- 'Frankenstein.Transform.Api', and 'Frankenstein.Style.Profile' into a single
-- headless session consumable by LSP editors, TUIs, GUIs, or CLI tools.
module Frankenstein.Ide.LiveSession
  ( DiagnosticSeverity(..)
  , LiveDiagnostic(..)
  , StyleAxis(..)
  , StyleFeedbackKind(..)
  , IdeCodeAction(..)
  , LiveIdeSession(..)
  , DidChangeResult(..)
  , initLiveSession
  , applyBufferChange
  , recordStyleFeedback
  , proposeCodeActions
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Frankenstein.Db.Incremental
  ( CodeDb
  , Durability (..)
  , QueryKey (..)
  , emptyCodeDb
  , setInput
  )
import Frankenstein.Db.ResilientEditCone
  ( EditConeReport (..)
  , ResilientDecl (..)
  , ResilientModule (..)
  , parseResilientModule
  , reparseEditCone
  , validSiblingDecls
  )
import Frankenstein.Style.Profile (IdiomPrefs (..), StyleProfile (..))
import Frankenstein.Transform.Api (SourceEdit (..), SourceSpan (..))

-- | LSP-compatible diagnostic severity level.
data DiagnosticSeverity
  = SevError
  | SevWarning
  | SevInfo
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Localized diagnostic emitted by incremental parsing or sheaf verification.
data LiveDiagnostic = LiveDiagnostic
  { ldFile     :: !FilePath
  , ldSpan     :: !SourceSpan
  , ldSeverity :: !DiagnosticSeverity
  , ldCode     :: !Text
  , ldMessage  :: !Text
  } deriving stock (Eq, Show)

-- | Selectable multi-lingual style axis adjusted by online user feedback.
data StyleAxis
  = AxisEarlyReturn
  | AxisPipelineOverLoop
  | AxisWhereOverLet
  | AxisPointFree
  | AxisCustomRule !Text
  deriving stock (Eq, Ord, Show)

-- | Developer reaction to a proposed refactoring or transpilation diff.
data StyleFeedbackKind
  = FeedbackAccepted
  | FeedbackRejected
  deriving stock (Eq, Ord, Show)

-- | Runnable IDE Code Action backed by @gargamelle@ transformations.
data IdeCodeAction = IdeCodeAction
  { icaTitle     :: !Text
  , icaKind      :: !Text
  , icaEdits     :: ![SourceEdit]
  , icaStyleAxis :: !(Maybe StyleAxis)
  } deriving stock (Eq, Show)

-- | Persistent in-memory state of an interactive code-in-database IDE session.
data LiveIdeSession = LiveIdeSession
  { lisCodeDb     :: !CodeDb
  , lisModules    :: !(Map FilePath ResilientModule)
  , lisBuffers    :: !(Map FilePath Text)
  , lisStyle      :: !StyleProfile
  , lisFeedbackN  :: {-# UNPACK #-} !Int
  } deriving stock (Eq, Show)

-- | Incremental outcome returned on every @didOpen@ / @didChange@ keystroke.
data DidChangeResult = DidChangeResult
  { dcrSession     :: !LiveIdeSession
  , dcrEditCone    :: !EditConeReport
  , dcrDiagnostics :: ![LiveDiagnostic]
  } deriving stock (Eq, Show)

-- | Initializes an empty live IDE session seeded with a repository 'StyleProfile'.
initLiveSession :: StyleProfile -> LiveIdeSession
initLiveSession sp =
  LiveIdeSession
    { lisCodeDb    = emptyCodeDb
    , lisModules   = Map.empty
    , lisBuffers   = Map.empty
    , lisStyle     = sp
    , lisFeedbackN = 0
    }

-- | Processes an incremental buffer edit at 'DurabilityLow', computing the exact
-- declaration edit cone and emitting localized 'LiveDiagnostic's without disturbing
-- valid sibling declarations.
applyBufferChange :: FilePath -> Text -> LiveIdeSession -> DidChangeResult
applyBufferChange fp newText sess =
  let db'    = setInput (QSourceText fp) DurabilityLow newText (lisCodeDb sess)
      newMod = parseResilientModule fp newText
      oldMod = Map.findWithDefault (ResilientModule fp [] Map.empty) fp (lisModules sess)
      cone   = reparseEditCone oldMod newMod
      diags  =
        [ LiveDiagnostic
            { ldFile     = fp
            , ldSpan     = sp
            , ldSeverity = SevError
            , ldCode     = "INGOLSTADT-PARSE-HOLE"
            , ldMessage  = "Isolated syntax error in `" <> dName <> "`: " <> reason
            }
        | (dName, sp, reason) <- ecrParseHoles cone
        ]
      sess'  = sess
        { lisCodeDb  = db'
        , lisModules = Map.insert fp newMod (lisModules sess)
        , lisBuffers = Map.insert fp newText (lisBuffers sess)
        }
  in DidChangeResult sess' cone diags

-- | Online bounded EMA update refining 'StyleProfile' weights from accepted/rejected diffs.
recordStyleFeedback :: StyleAxis -> StyleFeedbackKind -> LiveIdeSession -> LiveIdeSession
recordStyleFeedback axis fb sess =
  let !alpha  = 0.25 :: Double
      !target = case fb of
                  FeedbackAccepted -> 1.0
                  FeedbackRejected -> 0.0
      blend x = max 0.0 (min 1.0 (alpha * target + (1.0 - alpha) * x))
      sp0     = lisStyle sess
      p0      = spIdiomPrefs sp0
      sp1     =
        case axis of
          AxisEarlyReturn ->
            sp0 { spIdiomPrefs = p0 { ipPreferEarlyReturn = blend (ipPreferEarlyReturn p0) } }
          AxisPipelineOverLoop ->
            sp0 { spIdiomPrefs = p0 { ipPreferPipelineOverLoop = blend (ipPreferPipelineOverLoop p0) } }
          AxisWhereOverLet ->
            sp0 { spIdiomPrefs = p0 { ipPreferWhereOverLet = blend (ipPreferWhereOverLet p0) } }
          AxisPointFree ->
            sp0 { spIdiomPrefs = p0 { ipPreferPointFree = blend (ipPreferPointFree p0) } }
          AxisCustomRule ruleId ->
            let prev = Map.findWithDefault 0.5 ruleId (spExtraWeights sp0)
            in sp0 { spExtraWeights = Map.insert ruleId (blend prev) (spExtraWeights sp0) }
  in sess
       { lisStyle     = sp1
       , lisFeedbackN = lisFeedbackN sess + 1
       }

-- | Surfaces live @gargamelle@ IDE Code Actions for all valid declarations overlapping @cursorOff@.
proposeCodeActions :: FilePath -> Int -> LiveIdeSession -> [IdeCodeAction]
proposeCodeActions fp cursorOff sess =
  case (Map.lookup fp (lisModules sess), Map.lookup fp (lisBuffers sess)) of
    (Just rm, Just bufTxt) ->
      concatMap (actionsForDecl bufTxt) (filter coversCursor (validSiblingDecls rm))
    _ -> []
  where
    coversCursor d =
      let sp = rdSpan d
      in cursorOff >= ssStart sp && cursorOff <= ssEnd sp

    actionsForDecl bufTxt d =
      let sp     = rdSpan d
          slice  = T.take (ssEnd sp - ssStart sp) (T.drop (ssStart sp) bufTxt)
          holeAct =
            [ IdeCodeAction
                { icaTitle     = "Ingolstadt/Synquid: Synthesize verified body for `" <> rdName d <> "`"
                , icaKind      = "quickfix.ingolstadt.synquid"
                , icaEdits     = [SourceEdit sp slice "Synquid hole synthesis placeholder"]
                , icaStyleAxis = Nothing
                }
            | T.isInfixOf "??" slice
            ]
          refactorAct =
            [ IdeCodeAction
                { icaTitle     = "Ingolstadt/Gargamelle: E-graph polish `" <> rdName d <> "`"
                , icaKind      = "refactor.rewrite.ingolstadt.egraph"
                , icaEdits     = []
                , icaStyleAxis = Just AxisPipelineOverLoop
                }
            ]
      in holeAct ++ refactorAct
