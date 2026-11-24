{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE BangPatterns #-}
-- | Module: Frankenstein.Backpack.StageInstantiate
-- Description: Stage-time 1ML partial/full module instantiation, inlining, and sheaf H^1 checks
--
-- Instantiates parameterized @OrganIR@ modules at compile time against concrete
-- 'ModuleImpl' providers, enforcing syntactic + Liquid behavioral satisfaction
-- ('SheafObstruction') and immediately running 'Frankenstein.Opt.EGraph' across
-- the erased module boundary to guarantee zero cross-module call overhead.
module Frankenstein.Backpack.StageInstantiate
  ( ModuleImpl(..)
  , ParameterizedModule(..)
  , SheafObstruction(..)
  , InstantiatedModule(..)
  , checkBehavioralEdge
  , inlineProviderRefs
  , substituteValueRefs
  , verifyModuleGraphSheaf
  , partialInstantiate
  , instantiateAndSpecialize
  ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Frankenstein.Backpack.OpenRecordSig
  ( ModuleSig (..)
  , TypeFieldSpec (..)
  , ValueFieldSpec (..)
  )
import Frankenstein.Core.Interface (MemProfile)
import Frankenstein.Opt.EGraph (OptExpr (..), RewriteStats, saturateOrganExpr)
import Frankenstein.Verify.LiquidProfile (RefPred)

-- | Concrete module implementation filling an open-record 'ModuleSig' hole.
data ModuleImpl = ModuleImpl
  { miName       :: !Text
  , miTypes      :: !(Map Text (Int, Text))
    -- ^ Concrete manifest bindings @(kindArity, canonicalRep)@.
  , miBodies     :: !(Map Text (ValueFieldSpec, OptExpr))
    -- ^ Concrete function ABI descriptors paired with inlinable 'OptExpr' bodies.
  , miProvenLaws :: ![RefPred]
    -- ^ Behavioral Liquid laws discharged for this implementation.
  } deriving stock (Eq, Show)

-- | Parameterized (possibly partially instantiated) @OrganIR@ module.
data ParameterizedModule = ParameterizedModule
  { pmName     :: !Text
  , pmRequires :: !(Map Text ModuleSig)
    -- ^ Remaining uninstantiated hole signatures keyed by hole label.
  , pmBodies   :: !(Map Text OptExpr)
  } deriving stock (Eq, Show)

-- | Non-vanishing cellular sheaf @H^1@ coboundary obstruction at a module edge.
data SheafObstruction
  = UnknownHoleLabel !Text !Text
  | MissingTypeField !Text !Text
  | IncompatibleTypeBinding !Text !Text !TypeFieldSpec !(Int, Text)
  | MissingValueField !Text !Text
  | IncompatibleValueArity !Text !Text !Int !Int
  | ProfileViolation !Text !Text !MemProfile !MemProfile
  | MissingBehavioralLaw !Text !RefPred
  | UnfilledRemainingHoles !Text ![Text]
  deriving stock (Eq, Show)

-- | Fully closed, stage-specialized @OrganIR@ module ready for C11/MLIR emission.
data InstantiatedModule = InstantiatedModule
  { imName     :: !Text
  , imBodies   :: !(Map Text OptExpr)
  , imOptStats :: !(Map Text RewriteStats)
  } deriving stock (Eq, Show)

-- | Verifies that a concrete 'ModuleImpl' satisfies every type, value, profile,
-- and behavioral Liquid law required by @reqSig@ (sheaf @H^1 == 0@ check).
checkBehavioralEdge :: Text -> ModuleSig -> ModuleImpl -> [SheafObstruction]
checkBehavioralEdge edgeLabel reqSig impl =
  typeObs ++ valObs ++ lawObs
  where
    typeObs = concatMap checkType (Map.toList (msTypes reqSig))
    checkType (tName, reqSpec) =
      case Map.lookup tName (miTypes impl) of
        Nothing -> [MissingTypeField edgeLabel tName]
        Just concrete@(kArity, repName) ->
          case reqSpec of
            TypeOpaque reqArity
              | reqArity == kArity -> []
              | otherwise -> [IncompatibleTypeBinding edgeLabel tName reqSpec concrete]
            TypeManifest reqArity reqRep
              | reqArity == kArity && reqRep == repName -> []
              | otherwise -> [IncompatibleTypeBinding edgeLabel tName reqSpec concrete]

    valObs = concatMap checkVal (Map.toList (msValues reqSig))
    checkVal (vName, reqVal) =
      case Map.lookup vName (miBodies impl) of
        Nothing -> [MissingValueField edgeLabel vName]
        Just (provVal, _) ->
          [IncompatibleValueArity edgeLabel vName (vfsArity reqVal) (vfsArity provVal)
          | vfsArity reqVal /= vfsArity provVal]
          ++ [ProfileViolation edgeLabel vName (vfsMinProfile reqVal) (vfsMinProfile provVal)
             | vfsMinProfile provVal > vfsMinProfile reqVal]

    lawObs =
      [ MissingBehavioralLaw edgeLabel law
      | law <- msLaws reqSig
      , law `notElem` miProvenLaws impl
      ]

-- | General recursive expression substitution for variable identifiers.
substituteValueRefs :: Map Text OptExpr -> OptExpr -> OptExpr
substituteValueRefs subst = go
  where
    go (EVar v)               = Map.findWithDefault (EVar v) v subst
    go (ESeq a b)             = ESeq (go a) (go b)
    go (EDup e)               = EDup (go e)
    go (EDrop e)              = EDrop (go e)
    go (EAllocCtor t args)    = EAllocCtor t (map go args)
    go (EInplaceReuse r t a)  = EInplaceReuse r t (map go a)
    go (EAllocThunk e)        = EAllocThunk (go e)
    go (EForce e)             = EForce (go e)
    go (ERowMorph p e)        = ERowMorph p (go e)
    go (ECheckedIndex a i ok) = ECheckedIndex (go a) (go i) ok
    go (EUncheckedIndex a i)  = EUncheckedIndex (go a) (go i)
    go leaf                   = leaf

-- | Substitutes concrete provider 'OptExpr' bodies into caller expressions for
-- every qualified variable reference matching @holeName <> "." <> fieldName@.
inlineProviderRefs :: Text -> ModuleImpl -> OptExpr -> OptExpr
inlineProviderRefs holeName impl = substituteValueRefs inlinable
  where
    inlinable =
      Map.fromList
        [ (holeName <> "." <> fnName, body)
        | (fnName, (spec, body)) <- Map.toList (miBodies impl)
        , vfsInlineHint spec
        ]

-- | Verifies an entire multi-module instantiation graph/DAG, collecting all
-- H^1 SheafObstructions across all caller holes and provider implementations.
verifyModuleGraphSheaf
  :: Map Text ParameterizedModule
  -> Map (Text, Text) ModuleImpl  -- ^ (callerModuleName, holeLabel) -> Provider
  -> [SheafObstruction]
verifyModuleGraphSheaf pms providers =
  concatMap checkCallerHoles (Map.toList pms)
  where
    checkCallerHoles (callerName, pm) =
      concatMap (checkHole callerName) (Map.toList (pmRequires pm))
    checkHole callerName (holeLabel, reqSig) =
      case Map.lookup (callerName, holeLabel) providers of
        Nothing   -> [UnknownHoleLabel callerName holeLabel]
        Just impl -> checkBehavioralEdge (callerName <> "->" <> holeLabel) reqSig impl

-- | Partially instantiates a single named hole of a 'ParameterizedModule',
-- leaving all remaining holes open for downstream specialization.
partialInstantiate
  :: Text
  -> ModuleImpl
  -> ParameterizedModule
  -> Either [SheafObstruction] ParameterizedModule
partialInstantiate holeLabel impl pm =
  case Map.lookup holeLabel (pmRequires pm) of
    Nothing -> Left [UnknownHoleLabel (pmName pm) holeLabel]
    Just reqSig ->
      case checkBehavioralEdge (pmName pm <> "->" <> holeLabel) reqSig impl of
        errs@(_ : _) -> Left errs
        [] ->
          let remaining = Map.delete holeLabel (pmRequires pm)
              inlined   = Map.map (inlineProviderRefs holeLabel impl) (pmBodies pm)
          in Right pm
               { pmRequires = remaining
               , pmBodies   = inlined
               }

-- | Closes all remaining holes via a provider map and runs 'saturateOrganExpr'
-- across every specialized body to eliminate cross-module overhead.
instantiateAndSpecialize
  :: Int
  -> Map Text ModuleImpl
  -> ParameterizedModule
  -> Either [SheafObstruction] InstantiatedModule
instantiateAndSpecialize fuel providers pm0 = do
  pmFinal <- foldProviders (Map.toList providers) pm0
  if not (Map.null (pmRequires pmFinal))
    then Left [UnfilledRemainingHoles (pmName pmFinal) (Map.keys (pmRequires pmFinal))]
    else
      let optPairs = Map.map (saturateOrganExpr fuel) (pmBodies pmFinal)
      in Right InstantiatedModule
           { imName     = pmName pmFinal
           , imBodies   = Map.map fst optPairs
           , imOptStats = Map.map snd optPairs
           }
  where
    foldProviders [] pm = Right pm
    foldProviders ((hole, impl) : rest) pm = do
      pm' <- partialInstantiate hole impl pm
      foldProviders rest pm'
