-- VERBATIM_CODE_SEED: TP-007E
-- TARGET_REPO: ~/src/frankenstein
-- TARGET_PACKAGE: frankenstein
-- TARGET_FILES:
--   src/Frankenstein/Funnel/MVP.hs

{-# LANGUAGE BangPatterns, DataKinds, GADTs, KindSignatures, MagicHash, OverloadedStrings #-}
{-# LANGUAGE PolyKinds, RankNTypes, ScopedTypeVariables #-}

module Frankenstein.Funnel.MVP
  ( MVPAst(..)
  , SeparateObjectABI(..)
  , haskellMVPPlugin
  , c99MVPPlugin
  , rustMirMVPPlugin
  , compileSeparateObjectABI
  ) where

import Data.Primitive.PrimArray (sizeofPrimArray, indexPrimArray)
import Data.Text (Text)
import qualified Data.Text as T

import Polytropos (RecordF)
import Polytropos.RowMorph (RowMorph(..), unsafeMkRowMorph)
import Penelope.Core.Syntax (Penelope(..))
import Penelope.Backend.Plugin (TargetLayout, BackendPlugin, compileWithBackend)
import Penelope.Perceus (RTSProfile(..))
import Kogaki.Text.VirtualCharList (UTF8Rope, charListPrism, packUTF8Literal, unpackVirtualCharList)
import OrganBank.Plugin
  ( IdiomCost(..)
  , DPOSpan(..)
  , FeatureShard(..)
  , LanguagePlugin(..)
  )

-- | Unified MVP AST representation for the Flagship Triad:
--   * Haskell (`ghc-shim` Core + Virtual `[Char]` + `#Delay`/`#Force`)
--   * C99 (`c-shim` freestanding pointers/structs)
--   * Rust (`rustc-shim` MIR with linear/affine borrow scopes)
data MVPAst
  = MVPHaskellCore !Text ![Text]
  | MVPC99Unit     !Text ![Text]
  | MVPRustMirJSON !Text ![Text]
  deriving (Show, Eq)

-- | Separate Compilation Module Artifact with `%ev_vec` Evidence Vector C-ABI
--   and contravariant `ScopePenelope` reduct slicing table (no whole-program IR linking!).
data SeparateObjectABI artifact = SeparateObjectABI
  { soaModuleName       :: !Text
  , soaRTSProfile       :: !RTSProfile
  , soaEvidenceSlots    :: ![Int]
    -- ^ Static permutation indices synthesized for `H' |_ sigma` at module boundary!
  , soaCHeader          :: !Text
  , soaCompiledArtifact :: !artifact
  } deriving (Show, Eq)

-- | Shared identity DPO shard for the MVP core pipeline.
mvpCoreShard :: Text -> FeatureShard '[] ef_core
mvpCoreShard !shardName = FeatureShard
  { fsId   = shardName
  , fsLaws =
      [ DPOSpan
          { dpoRuleName = shardName <> "-id"
          , dpoForward  = id
          , dpoBackward = \g -> [g]
          }
      ]
  }

idIdentityRoute :: Penelope '[] ef ins outs
idIdentityRoute = Route (unsafeMkRowMorph [])

-- | 1. Flagship Haskell Front-End Funnel (`ghc-shim` + Virtual `[Char]` + `RTSLean4ThreadLocal`).
haskellMVPPlugin :: LanguagePlugin MVPAst '[] ef_core
haskellMVPPlugin = LanguagePlugin
  { lpLanguageName    = "Haskell"
  , lpFeatureColimit  = [mvpCoreShard "haskell-lazy-delay", mvpCoreShard "kogaki-virtual-charlist"]
  , lpStringPrism     = charListPrism
  , lpIngressParse    = \rope ->
      let !src = T.pack (unpackVirtualCharList rope)
      in Right (MVPHaskellCore "Main" (T.lines src))
  , lpLowerToPenelope = \_ast -> idIdentityRoute
  , lpIdiomCostModel  = \_    -> IdiomCost 1
  , lpPrettyEmit      = \_    -> MVPHaskellCore "Main" ["-- Resugared Haskell 2010 Module"]
  , lpRenderSource    = \ast  -> case ast of
      MVPHaskellCore _ lns -> packUTF8Literal (T.unpack (T.unlines lns))
      _                    -> packUTF8Literal ""
  }

-- | 2. Freestanding C99 Front-End Funnel (`c-shim` + `RTSFreestandingLinear`).
c99MVPPlugin :: LanguagePlugin MVPAst '[] ef_core
c99MVPPlugin = LanguagePlugin
  { lpLanguageName    = "C99"
  , lpFeatureColimit  = [mvpCoreShard "c99-freestanding-linear"]
  , lpStringPrism     = charListPrism
  , lpIngressParse    = \rope ->
      let !src = T.pack (unpackVirtualCharList rope)
      in Right (MVPC99Unit "c_unit" (T.lines src))
  , lpLowerToPenelope = \_ast -> idIdentityRoute
  , lpIdiomCostModel  = \_    -> IdiomCost 5
  , lpPrettyEmit      = \_    -> MVPC99Unit "c_unit" ["/* Emitted Freestanding C99 */"]
  , lpRenderSource    = \ast  -> case ast of
      MVPC99Unit _ lns -> packUTF8Literal (T.unpack (T.unlines lns))
      _                -> packUTF8Literal ""
  }

-- | 3. Rust MIR Front-End Funnel (`rustc-shim` + `RTSFreestandingLinear` / Affine Borrows).
rustMirMVPPlugin :: LanguagePlugin MVPAst '[] ef_core
rustMirMVPPlugin = LanguagePlugin
  { lpLanguageName    = "Rust"
  , lpFeatureColimit  = [mvpCoreShard "rust-mir-linear-borrow"]
  , lpStringPrism     = charListPrism
  , lpIngressParse    = \rope ->
      let !src = T.pack (unpackVirtualCharList rope)
      in Right (MVPRustMirJSON "rust_crate" (T.lines src))
  , lpLowerToPenelope = \_ast -> idIdentityRoute
  , lpIdiomCostModel  = \_    -> IdiomCost 2
  , lpPrettyEmit      = \_    -> MVPRustMirJSON "rust_crate" ["// Resugared Rust Module"]
  , lpRenderSource    = \ast  -> case ast of
      MVPRustMirJSON _ lns -> packUTF8Literal (T.unpack (T.unlines lns))
      _                    -> packUTF8Literal ""
  }

-- | True Separate Compilation via the Institution Satisfaction Condition:
--   $$\text{eval}_{\Sigma_2}(\mathcal{H}', \sigma_*(P)) \equiv \text{eval}_{\Sigma_1}(\mathcal{H}' \upharpoonright_\sigma, P)$$
--   Emits a standalone `.o`-ready target artifact and the stack-allocated `%ev_vec`
--   contravariant reduct projection shim so callers link via standard `lld` with zero IR linking!
compileSeparateObjectABI
  :: Text
  -> TargetLayout
  -> RTSProfile
  -> RowMorph ef_local ef_ambient
  -> BackendPlugin val carrier artifact
  -> RecordF val ins
  -> Penelope '[] ef_local ins outs
  -> SeparateObjectABI artifact
compileSeparateObjectABI !modName !layout !rts (RowMorph !permArr) !backend !initFrame !prog =
  let !nSlots    = sizeofPrimArray permArr
      !slots     = [ indexPrimArray permArr i | i <- [0 .. nSlots - 1] ]
      !artifact  = compileWithBackend backend layout rts prog initFrame
      !cHeader   = T.unlines
        [ "/* Auto-generated Frankenstein 2.0 Separate Compilation ABI Header */"
        , "/* Module: " <> modName <> " | RTS Profile: " <> T.pack (show rts) <> " */"
        , "static inline void __organ_slice_ev_" <> modName
          <> "(struct PenelopeEffVec* dst, const struct PenelopeEffVec* ambient) {"
        , T.unlines
            [ "  dst->handlers[" <> T.pack (show i) <> "] = ambient->handlers[" <> T.pack (show s) <> "];"
            | (i, s) <- zip [0 :: Int ..] slots
            ]
        , "}"
        ]
  in SeparateObjectABI
       { soaModuleName       = modName
       , soaRTSProfile       = rts
       , soaEvidenceSlots    = slots
       , soaCHeader          = cHeader
       , soaCompiledArtifact = artifact
       }
