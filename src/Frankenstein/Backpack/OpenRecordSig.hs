{-# LANGUAGE DerivingStrategies #-}
-- | Module: Frankenstein.Backpack.OpenRecordSig
-- Description: 1ML-style refined open-record module signatures and categorical pushout merging
--
-- Represents module interfaces natively inside @frankenstein@ as open records
-- combining translucent type declarations ('TypeOpaque' vs 'TypeManifest'),
-- canonical value ABI slots, 'MemProfile' bounds, and 'RefPred' behavioral laws.
module Frankenstein.Backpack.OpenRecordSig
  ( TypeFieldSpec(..)
  , ValueFieldSpec(..)
  , ModuleSig(..)
  , SigMorphism(..)
  , SigMergeError(..)
  , emptyModuleSig
  , applySigMorphism
  , mergeModuleSigs
  ) where

import Data.List (foldl', nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Frankenstein.Core.Interface (MemProfile)
import Frankenstein.Verify.LiquidProfile (RefPred)

-- | 1ML translucent type field specification inside a module signature.
data TypeFieldSpec
  = TypeOpaque {-# UNPACK #-} !Int
    -- ^ Abstract type constructor with kind arity (0 for @Type@, 1 for @Type -> Type@).
  | TypeManifest {-# UNPACK #-} !Int !Text
    -- ^ Transparent/manifest type definition @(kindArity, canonicalRepName)@.
  deriving stock (Eq, Ord, Show)

-- | Value/function operation required or provided by a module signature.
data ValueFieldSpec = ValueFieldSpec
  { vfsArity       :: {-# UNPACK #-} !Int
  , vfsTypeSummary :: !Text
  , vfsMinProfile  :: !MemProfile
  , vfsInlineHint  :: !Bool
    -- ^ True requests mandatory stage-time specialization/inlining across the module seam.
  } deriving stock (Eq, Ord, Show)

-- | 1ML open-record module signature carrying syntactic types, ABI slots, and Liquid laws.
data ModuleSig = ModuleSig
  { msName   :: !Text
  , msTypes  :: !(Map Text TypeFieldSpec)
  , msValues :: !(Map Text ValueFieldSpec)
  , msLaws   :: ![RefPred]
  } deriving stock (Eq, Show)

-- | Signature morphism (renaming + sub-signature projection, mirroring Cabal @mixins:@).
data SigMorphism = SigMorphism
  { smRenameTypes  :: !(Map Text Text)
  , smRenameValues :: !(Map Text Text)
  } deriving stock (Eq, Show)

-- | Obstruction raised when two signatures fail to merge into a coherent pushout.
data SigMergeError
  = KindArityConflict !Text !Int !Int
  | ManifestTypeConflict !Text !Text !Text
  | ValueArityConflict !Text !Int !Int
  | ValueTypeConflict !Text !Text !Text
  deriving stock (Eq, Show)

-- | Empty unit signature (initial object in @Sign@).
emptyModuleSig :: Text -> ModuleSig
emptyModuleSig name = ModuleSig name Map.empty Map.empty []

-- | Applies a 'SigMorphism' renaming across type and value field labels.
applySigMorphism :: SigMorphism -> ModuleSig -> ModuleSig
applySigMorphism morph sig =
  sig
    { msTypes  = Map.mapKeys (\k -> Map.findWithDefault k k (smRenameTypes morph)) (msTypes sig)
    , msValues = Map.mapKeys (\k -> Map.findWithDefault k k (smRenameValues morph)) (msValues sig)
    }

-- | Computes the categorical signature pushout (Backpack merge) of two 'ModuleSig's.
mergeModuleSigs :: Text -> ModuleSig -> ModuleSig -> Either [SigMergeError] ModuleSig
mergeModuleSigs mergedName s1 s2 =
  let (mergedTypes, typeErrs) = foldl' mergeType (msTypes s1, []) (Map.toList (msTypes s2))
      (mergedVals, valErrs)   = foldl' mergeVal (msValues s1, []) (Map.toList (msValues s2))
      allErrs                 = reverse typeErrs ++ reverse valErrs
  in if null allErrs
       then Right ModuleSig
              { msName   = mergedName
              , msTypes  = mergedTypes
              , msValues = mergedVals
              , msLaws   = nub (msLaws s1 ++ msLaws s2)
              }
       else Left allErrs
  where
    mergeType (acc, errs) (k, spec2) =
      case Map.lookup k acc of
        Nothing    -> (Map.insert k spec2 acc, errs)
        Just spec1 ->
          case unifyTypeField k spec1 spec2 of
            Left err   -> (acc, err : errs)
            Right spec -> (Map.insert k spec acc, errs)

    mergeVal (acc, errs) (k, v2) =
      case Map.lookup k acc of
        Nothing -> (Map.insert k v2 acc, errs)
        Just v1 ->
          case unifyValueField k v1 v2 of
            Left err -> (acc, err : errs)
            Right v  -> (Map.insert k v acc, errs)

unifyTypeField :: Text -> TypeFieldSpec -> TypeFieldSpec -> Either SigMergeError TypeFieldSpec
unifyTypeField k (TypeOpaque a1) (TypeOpaque a2)
  | a1 == a2  = Right (TypeOpaque a1)
  | otherwise = Left (KindArityConflict k a1 a2)
unifyTypeField k (TypeOpaque a1) (TypeManifest a2 rep)
  | a1 == a2  = Right (TypeManifest a2 rep)
  | otherwise = Left (KindArityConflict k a1 a2)
unifyTypeField k (TypeManifest a1 rep) (TypeOpaque a2)
  | a1 == a2  = Right (TypeManifest a1 rep)
  | otherwise = Left (KindArityConflict k a1 a2)
unifyTypeField k (TypeManifest a1 r1) (TypeManifest a2 r2)
  | a1 /= a2  = Left (KindArityConflict k a1 a2)
  | r1 /= r2  = Left (ManifestTypeConflict k r1 r2)
  | otherwise = Right (TypeManifest a1 r1)

unifyValueField :: Text -> ValueFieldSpec -> ValueFieldSpec -> Either SigMergeError ValueFieldSpec
unifyValueField k v1 v2
  | vfsArity v1 /= vfsArity v2 =
      Left (ValueArityConflict k (vfsArity v1) (vfsArity v2))
  | vfsTypeSummary v1 /= vfsTypeSummary v2 =
      Left (ValueTypeConflict k (vfsTypeSummary v1) (vfsTypeSummary v2))
  | otherwise =
      Right ValueFieldSpec
        { vfsArity       = vfsArity v1
        , vfsTypeSummary = vfsTypeSummary v1
        , vfsMinProfile  = min (vfsMinProfile v1) (vfsMinProfile v2)
        , vfsInlineHint  = vfsInlineHint v1 || vfsInlineHint v2
        }
