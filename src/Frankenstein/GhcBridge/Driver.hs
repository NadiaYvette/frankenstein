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

import GHC
import GHC.Core (CoreProgram)
import GHC.Driver.Session (updOptLevel)
import GHC.Unit.Module.ModGuts (ModGuts(..))

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcess)

import Frankenstein.GhcBridge.CoreTranslate (translateProgram)
import Frankenstein.Core.Types (Program)

-- | Result of compiling through GHC's frontend
data GhcCoreResult = GhcCoreResult
  { gcrModuleName :: !Text
  , gcrProgram    :: !Program
  } deriving (Show)

-- | Detect the GHC library directory by running @ghc --print-libdir@.
detectLibDir :: IO FilePath
detectLibDir = do
  raw <- readProcess "ghc" ["--print-libdir"] ""
  pure (filter (/= '\n') raw)

-- | Compile a Haskell file through GHC and get Core output.
--
-- Uses the GHC API directly: parse -> typecheck -> desugar -> extract Core.
compileToCore :: FilePath -> IO (Either Text GhcCoreResult)
compileToCore inputPath = do
  libdir <- detectLibDir
  result <- runGhcCompile libdir inputPath
  pure result

-- | Run a GHC session that compiles the input file and extracts Core.
runGhcCompile :: FilePath -> FilePath -> IO (Either Text GhcCoreResult)
runGhcCompile libdir inputPath = do
  runGhc (Just libdir) $ do
    -- Set up session flags: enable optimisation so demand analysis runs
    dflags <- getSessionDynFlags
    let dflags' = updOptLevel 1 dflags
    setSessionDynFlags dflags'

    -- Add the target file
    target <- guessTarget inputPath Nothing Nothing
    setTargets [target]

    -- Load (compile) all targets
    _successFlag <- load LoadAllTargets

    -- Get the module graph and find our module
    modGraph <- getModuleGraph
    let summaries = mgModSummaries modGraph
    case summaries of
      [] -> pure $ Left "No modules found in module graph"
      (modSummary:_) -> do
        -- Parse, typecheck, and desugar
        parsed    <- parseModule modSummary
        typecked  <- typecheckModule parsed
        desugared <- desugarModule typecked

        -- Extract the Core program from ModGuts
        let modGuts  = dm_core_module desugared
            coreProg = mg_binds modGuts
            modName  = moduleNameString (moduleName (ms_mod modSummary))
            modNameT = T.pack modName

        -- Translate GHC Core -> Frankenstein Core
        case translateProgram modNameT coreProg of
          Left err   -> pure $ Left $ "Core translation error: " <> err
          Right prog -> pure $ Right $ GhcCoreResult
            { gcrModuleName = modNameT
            , gcrProgram    = prog
            }
