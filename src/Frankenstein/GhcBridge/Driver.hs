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
import GHC.Driver.Session (updOptLevel, xopt_set)
import GHC.Unit.Module.ModGuts (ModGuts(..))
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.LanguageExtensions.Type as LangExt

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcess)
import System.Directory (doesFileExist)

import Frankenstein.GhcBridge.CoreTranslate (translateProgram)
import Frankenstein.Core.Types (Program)

-- | Result of compiling through GHC's frontend
data GhcCoreResult = GhcCoreResult
  { gcrModuleName :: !Text
  , gcrProgram    :: !Program
  } deriving (Show)

-- | Detect the GHC library directory by running @ghc --print-libdir@.
-- We try GHC 9.14.1 first (matches our build-depends constraint),
-- then fall back to whatever 'ghc' is on PATH.
detectLibDir :: IO FilePath
detectLibDir = do
  let ghc914 = "/usr/lib64/ghc-9.14.1/bin/ghc"
  exists <- doesFileExist ghc914
  let ghcCmd = if exists then ghc914 else "ghc"
  raw <- readProcess ghcCmd ["--print-libdir"] ""
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
    -- Set up session flags: enable optimisation so demand analysis runs.
    -- Also add common source roots so the file can import other Frankenstein modules.
    -- Enable language extensions matching frankenstein.cabal so source files
    -- that rely on default-extensions (e.g. OverloadedStrings) compile.
    dflags <- getSessionDynFlags
    -- Make the `ghc` package visible so our own modules that import
    -- GHC.Core, GHC.Driver.Session, etc. can self-host.
    let dflags1 = (updOptLevel 1 dflags)
          { DynFlags.importPaths = ["src", "."] ++ DynFlags.importPaths dflags
          , DynFlags.packageFlags =
              [ DynFlags.ExposePackage pkg (DynFlags.PackageArg pkg)
                  (DynFlags.ModRenaming True [])
              | pkg <- ["ghc", "koka", "organ-ir"]
              ] ++ DynFlags.packageFlags dflags
          , DynFlags.packageDBFlags =
              -- Cabal store (shared across projects) + project inplace db.
              -- Order matters: inplace must come after store so its
              -- `-inplace` units override any installed copies.
              [ DynFlags.PackageDB (DynFlags.PkgDbPath p)
              | p <- [ "/home/nyc/.local/state/cabal/store/ghc-9.14.1-0dcc/package.db"
                     , "dist-newstyle/packagedb/ghc-9.14.1"
                     ]
              ] ++ DynFlags.packageDBFlags dflags
          }
        -- Match frankenstein.cabal `default-extensions` + common extensions
        -- that our own source files depend on.
        dflags2 = foldr (flip xopt_set) dflags1
          [ LangExt.OverloadedStrings
          , LangExt.LambdaCase
          , LangExt.BangPatterns
          , LangExt.TupleSections
          , LangExt.ScopedTypeVariables
          , LangExt.DeriveFunctor
          , LangExt.DeriveFoldable
          , LangExt.DeriveTraversable
          , LangExt.GeneralizedNewtypeDeriving
          , LangExt.FlexibleContexts
          , LangExt.FlexibleInstances
          , LangExt.RecordWildCards
          , LangExt.NamedFieldPuns
          , LangExt.MultiParamTypeClasses
          ]
    setSessionDynFlags dflags2

    -- Add the target file
    target <- guessTarget inputPath Nothing Nothing
    setTargets [target]

    -- Load (compile) all targets
    _successFlag <- load LoadAllTargets

    -- Get the module graph and find the module matching our target file.
    -- When the file imports other Frankenstein modules, GHC loads them too;
    -- taking the head of the graph would pick a random dependency.
    modGraph <- getModuleGraph
    let summaries = mgModSummaries modGraph
        matchFile s = case ml_hs_file (ms_location s) of
                        Just p  -> p == inputPath
                        Nothing -> False
        targetSummary = case filter matchFile summaries of
                          (s:_) -> Just s
                          []    -> case summaries of
                                     (s:_) -> Just s
                                     []    -> Nothing
    case targetSummary of
      Nothing -> pure $ Left "No modules found in module graph"
      Just modSummary -> do
        -- Parse, typecheck, and desugar
        parsed    <- parseModule modSummary
        typecked  <- typecheckModule parsed
        desugared <- desugarModule typecked

        -- Extract the Core program and type constructors from ModGuts
        let modGuts  = dm_core_module desugared
            coreProg = mg_binds modGuts
            tyCons   = mg_tcs modGuts
            modName  = moduleNameString (moduleName (ms_mod modSummary))
            modNameT = T.pack modName

        -- Translate GHC Core -> Frankenstein Core (including data types)
        case translateProgram modNameT coreProg tyCons of
          Left err   -> pure $ Left $ "Core translation error: " <> err
          Right prog -> pure $ Right $ GhcCoreResult
            { gcrModuleName = modNameT
            , gcrProgram    = prog
            }
