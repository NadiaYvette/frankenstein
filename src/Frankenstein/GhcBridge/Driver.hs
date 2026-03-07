-- | GHC Bridge Driver
--
-- Uses the GHC API to compile Haskell source through GHC's frontend
-- and extract GHC Core with demand/strictness annotations.
--
-- This module wraps the GHC API session management. The actual
-- GHC Core -> Frankenstein Core translation is in CoreTranslate.

module Frankenstein.GhcBridge.Driver
  ( compileToCore
  , GhcCoreResult(..)
  ) where

-- When we add the `ghc` package dependency, these will resolve:
-- import GHC
-- import GHC.Core
-- import GHC.Core.Opt.DmdAnal (dmdAnalProgram)
-- import GHC.Driver.Session
-- import GHC.Types.Id
-- import GHC.Unit.Module

import Data.Text (Text)
import qualified Data.Text as T

-- | Result of compiling through GHC's frontend
data GhcCoreResult = GhcCoreResult
  { gcrModuleName :: !Text
  , gcrCoreJson   :: !Text  -- Serialized GHC Core (temporary: until we link ghc API)
  } deriving (Show)

-- | Compile a Haskell file through GHC and get Core output.
--
-- Strategy: For now, invoke `ghc -ddump-simpl -ddump-stg-final -dsuppress-all`
-- as a subprocess and parse the textual Core output.
-- Phase 2: Link against the `ghc` library directly and get typed Core AST.
compileToCore :: FilePath -> IO (Either Text GhcCoreResult)
compileToCore inputPath = do
  -- Phase 1: subprocess approach
  -- ghc -ddump-simpl gives us Core after optimization (including demand analysis)
  -- ghc -ddump-stg-final gives us STG (where strictness is fully resolved)
  --
  -- For the translation, we want post-demand-analysis Core:
  --   ghc -O -ddump-simpl -dsuppress-uniques -dsuppress-module-prefixes
  --
  -- The output format is human-readable but parseable enough for bootstrap.
  --
  -- TODO: Replace with direct GHC API calls:
  --   runGhc (Just libdir) $ do
  --     dflags <- getSessionDynFlags
  --     setSessionDynFlags dflags
  --     target <- guessTarget inputPath Nothing Nothing
  --     setTargets [target]
  --     load LoadAllTargets
  --     modGraph <- getModuleGraph
  --     -- extract desugared + optimized Core from each module
  pure $ Left "GHC bridge not yet connected — need ghc package dependency"
