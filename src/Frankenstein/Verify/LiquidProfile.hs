{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module: Frankenstein.Verify.LiquidProfile
-- Description: Verification-guided Liquid refinement VCs, Profile-0 promotion, and bounds elision
--
-- Bridges @LiquidSig@ / @gargamelle-fol@ refinement predicates directly into
-- @OrganIR@ optimization decisions: automatic promotion from
-- 'Profile1ThreadLocalPerceus' to 'Profile0FreestandingLinear', sound array
-- bounds-check elision, and FIP constant-allocation certification.
module Frankenstein.Verify.LiquidProfile
  ( ArithExpr(..)
  , RefPred(..)
  , LiquidContract(..)
  , OptWitness(..)
  , ProfilePromotion(..)
  , emitSmtLib2Expr
  , emitSmtLib2Pred
  , emitSmtLib2Query
  , proveStaticBounds
  , promoteProfile
  , dischargeLinearityAndBounds
  , runZ3Query
  ) where

import Control.Exception (try, SomeException)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Frankenstein.Core.Interface (MemProfile (..))

-- | Linear integer arithmetic term over refinement variables.
data ArithExpr
  = AInt !Integer
  | AVar !Text
  | AAdd !ArithExpr !ArithExpr
  | ASub !ArithExpr !ArithExpr
  deriving stock (Eq, Ord, Show)

-- | Quantifier-free linear integer refinement predicate (QF_LIA).
data RefPred
  = PTrue
  | PFalse
  | PEq !ArithExpr !ArithExpr
  | PLt !ArithExpr !ArithExpr
  | PLe !ArithExpr !ArithExpr
  | PAnd !RefPred !RefPred
  | PNot !RefPred
  deriving stock (Eq, Ord, Show)

-- | Refinement and resource contract attached to an OrganIR function.
data LiquidContract = LiquidContract
  { lcFuncName       :: !Text
  , lcInitialProfile :: !MemProfile
  , lcPreconditions  :: ![RefPred]
  , lcMaxUseCount    :: !Int
    -- ^ Syntactic/flow-sensitive upper bound on dynamic reference multiplicity.
  , lcEscapesHeap    :: !Bool
    -- ^ True if a reference escapes into a shared/concurrent closure or global.
  , lcFipAllocDelta  :: !Int
    -- ^ Net heap allocations minus Perceus reuse tokens (@<= 0@ certifies FIP).
  } deriving stock (Eq, Show)

-- | Proven optimization witnesses unlocked by discharging Liquid VCs.
data OptWitness
  = WitnessPromotedToProfile0
  | WitnessFipCertified !Int
  | WitnessBoundsCheckElided !Text !Text
    -- ^ @(indexVar, lengthVar)@ proven @0 <= indexVar < lengthVar@.
  deriving stock (Eq, Ord, Show)

-- | Output of the verification-guided profile promotion pass.
data ProfilePromotion = ProfilePromotion
  { ppFinalProfile :: !MemProfile
  , ppWitnesses    :: ![OptWitness]
  , ppRejectedWhy  :: ![Text]
  } deriving stock (Eq, Show)

-- | Renders an 'ArithExpr' as an SMT-LIB2 S-expression.
emitSmtLib2Expr :: ArithExpr -> Text
emitSmtLib2Expr (AInt n)   = if n < 0 then "(- " <> T.pack (show (negate n)) <> ")" else T.pack (show n)
emitSmtLib2Expr (AVar v)   = v
emitSmtLib2Expr (AAdd a b) = "(+ " <> emitSmtLib2Expr a <> " " <> emitSmtLib2Expr b <> ")"
emitSmtLib2Expr (ASub a b) = "(- " <> emitSmtLib2Expr a <> " " <> emitSmtLib2Expr b <> ")"

-- | Renders a 'RefPred' as an SMT-LIB2 Boolean expression.
emitSmtLib2Pred :: RefPred -> Text
emitSmtLib2Pred PTrue      = "true"
emitSmtLib2Pred PFalse     = "false"
emitSmtLib2Pred (PEq a b)  = "(= " <> emitSmtLib2Expr a <> " " <> emitSmtLib2Expr b <> ")"
emitSmtLib2Pred (PLt a b)  = "(< " <> emitSmtLib2Expr a <> " " <> emitSmtLib2Expr b <> ")"
emitSmtLib2Pred (PLe a b)  = "(<= " <> emitSmtLib2Expr a <> " " <> emitSmtLib2Expr b <> ")"
emitSmtLib2Pred (PAnd p q) = "(and " <> emitSmtLib2Pred p <> " " <> emitSmtLib2Pred q <> ")"
emitSmtLib2Pred (PNot p)   = "(not " <> emitSmtLib2Pred p <> ")"

-- | Emits a complete QF_LIA SMT-LIB2 validity query (@unsat@ proves @assumptions => goal@).
emitSmtLib2Query :: [Text] -> [RefPred] -> RefPred -> Text
emitSmtLib2Query vars assumptions goal =
  T.unlines $
    [ "(set-logic QF_LIA)" ]
    ++ [ "(declare-const " <> v <> " Int)" | v <- vars ]
    ++ [ "(assert " <> emitSmtLib2Pred p <> ")" | p <- assumptions ]
    ++ [ "(assert (not " <> emitSmtLib2Pred goal <> "))"
       , "(check-sat)"
       ]

-- | Sound built-in decision check verifying @0 <= idxVar < lenVar@ from preconditions
-- (complementing host Z3 in airgapped sandboxes).
proveStaticBounds :: [RefPred] -> Text -> Text -> Bool
proveStaticBounds preds idxVar lenVar =
  let atoms = concatMap flattenAnd preds
      nonNeg = any isNonNeg atoms
      strictUpper = any isStrictUpper atoms
      aliases = Map.fromList [(a, b) | PEq (AVar a) (AVar b) <- atoms]
      lenNames = lenVar : [b | (a, b) <- Map.toList aliases, a == lenVar]
                        ++ [a | (a, b) <- Map.toList aliases, b == lenVar]
      isNonNeg (PLe (AInt 0) (AVar v)) = v == idxVar
      isNonNeg (PLt (AInt (-1)) (AVar v)) = v == idxVar
      isNonNeg (PAnd a b) = isNonNeg a || isNonNeg b
      isNonNeg _ = False
      isStrictUpper (PLt (AVar v) (AVar lim)) = v == idxVar && lim `elem` lenNames
      isStrictUpper (PAnd a b) = isStrictUpper a || isStrictUpper b
      isStrictUpper _ = False
  in nonNeg && strictUpper
  where
    flattenAnd (PAnd p q) = flattenAnd p ++ flattenAnd q
    flattenAnd p          = [p]

-- | Soundly promotes a function's 'MemProfile' and attaches verified 'OptWitness'es.
promoteProfile :: LiquidContract -> [(Text, Text)] -> ProfilePromotion
promoteProfile lc boundChecks =
  let canPromote0 =
        lcInitialProfile lc /= Profile2ConcurrentShared
          && lcMaxUseCount lc <= 1
          && not (lcEscapesHeap lc)
      finalProf =
        if canPromote0 then Profile0FreestandingLinear else lcInitialProfile lc
      profWitness =
        [ WitnessPromotedToProfile0
        | canPromote0 && lcInitialProfile lc /= Profile0FreestandingLinear
        ]
      fipWitness =
        [ WitnessFipCertified (lcFipAllocDelta lc)
        | lcFipAllocDelta lc <= 0
        ]
      boundsWitnesses =
        [ WitnessBoundsCheckElided idxV lenV
        | (idxV, lenV) <- boundChecks
        , proveStaticBounds (lcPreconditions lc) idxV lenV
        ]
      reasons =
        [ "multiplicity > 1 (aliasing prevents Profile 0 promotion)" | lcMaxUseCount lc > 1 ]
        ++ [ "heap/thread escape prevents Profile 0 promotion" | lcEscapesHeap lc ]
        ++ [ "Profile2ConcurrentShared requires explicit atomic RC" | lcInitialProfile lc == Profile2ConcurrentShared ]
  in ProfilePromotion
       { ppFinalProfile = finalProf
       , ppWitnesses    = profWitness ++ fipWitness ++ boundsWitnesses
       , ppRejectedWhy  = reasons
       }

-- | Discharges linearity and bounds checks, providing the primary entry point for code binding.
dischargeLinearityAndBounds :: LiquidContract -> [(Text, Text)] -> ProfilePromotion
dischargeLinearityAndBounds = promoteProfile

-- | Optional live Z3 execution on the SMT-LIB2 query (returns Right True if unsat/valid).
runZ3Query :: Text -> IO (Either Text Bool)
runZ3Query query = do
  res <- try (readProcessWithExitCode "z3" ["-in", "-smt2"] (T.unpack query))
  case res of
    Left (err :: SomeException) -> pure (Left (T.pack (show err)))
    Right (ExitSuccess, out, _) ->
      let trimmed = T.strip (T.pack out)
      in if trimmed == "unsat"
           then pure (Right True)
           else if trimmed == "sat"
                  then pure (Right False)
                  else pure (Left ("Unexpected Z3 output: " <> trimmed))
    Right (ExitFailure code, out, err) ->
      pure (Left ("Z3 failed with code " <> T.pack (show code) <> ": " <> T.pack err <> " " <> T.pack out))
