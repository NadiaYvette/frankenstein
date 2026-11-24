{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
-- | Module: Frankenstein.Verify.SynquidHole
-- Description: Fuel-bounded bidirectional hole synthesis for OrganIR refinement goals
--
-- Resolves typed @OrganHole@ (@??@) placeholders during compilation against a
-- catalog @Gamma@ of components, verifies Liquid preconditions via
-- 'Frankenstein.Verify.LiquidProfile', and simplifies solutions with
-- 'Frankenstein.Opt.EGraph'.
module Frankenstein.Verify.SynquidHole
  ( HoleSort(..)
  , SynthComponent(..)
  , HoleGoal(..)
  , SynthOutcome(..)
  , synthesizeHole
  ) where

import Data.Text (Text)
import Frankenstein.Opt.EGraph (OptExpr (..), RewriteStats, saturateOrganExpr)
import Frankenstein.Verify.LiquidProfile (RefPred, proveStaticBounds)

-- | Coarse type sort used to guide bidirectional OrganIR hole enumeration.
data HoleSort
  = SortInt
  | SortArray !Text
    -- ^ Array paired with its symbolic length variable name.
  | SortIndex !Text
    -- ^ Index variable name (checked against an array length variable).
  | SortFrame
  deriving stock (Eq, Ord, Show)

-- | Typed component available in synthesis catalog @Gamma@.
data SynthComponent = SynthComponent
  { scName :: !Text
  , scSort :: !HoleSort
  , scExpr :: !OptExpr
  } deriving stock (Eq, Show)

-- | Typed refinement synthesis goal for a single @??@ hole.
data HoleGoal = HoleGoal
  { hgHoleName      :: !Text
  , hgTargetSort    :: !HoleSort
  , hgPreconditions :: ![RefPred]
  , hgCatalog       :: ![SynthComponent]
  , hgFuel          :: !Int
  } deriving stock (Eq, Show)

-- | Deterministic outcome of attempting to fill an 'HoleGoal'.
data SynthOutcome
  = SynthSolved !OptExpr !RewriteStats
  | SynthUnsatisfiable !Text
  | SynthOutOfFuel !Int
  deriving stock (Eq, Show)

-- | Attempts to synthesize and simplify a verified 'OptExpr' filling @goal@.
synthesizeHole :: HoleGoal -> SynthOutcome
synthesizeHole goal
  | hgFuel goal <= 0 = SynthOutOfFuel 0
  | otherwise =
      case directCandidates of
        (cand : _) ->
          let (optExpr, stats) = saturateOrganExpr (hgFuel goal) cand
          in SynthSolved optExpr stats
        [] ->
          case indexedCandidates of
            (cand : _) ->
              let (optExpr, stats) = saturateOrganExpr (hgFuel goal) cand
              in SynthSolved optExpr stats
            [] ->
              SynthUnsatisfiable (hgHoleName goal)
  where
    directCandidates =
      [ scExpr c
      | c <- hgCatalog goal
      , scSort c == hgTargetSort goal
      ]

    indexedCandidates =
      case hgTargetSort goal of
        SortInt ->
          [ ECheckedIndex (scExpr arrC) (scExpr idxC)
              (proveStaticBounds (hgPreconditions goal) idxVar lenVar)
          | arrC <- hgCatalog goal
          , SortArray lenVar <- [scSort arrC]
          , idxC <- hgCatalog goal
          , SortIndex idxVar <- [scSort idxC]
          ]
        _ -> []
