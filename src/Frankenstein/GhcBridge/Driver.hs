-- | GHC Bridge Driver
--
-- Uses the GHC API to compile Haskell source through GHC's frontend
-- and extract GHC Core with demand/strictness annotations.
--
-- This module wraps the GHC API session management. The actual
-- GHC Core -> Frankenstein Core translation is in CoreTranslate.

module Frankenstein.GhcBridge.Driver
  ( compileToCore
  , compileToCoreWith
  , compileToCoreMulti
  , GhcCoreResult(..)
  ) where

import GHC
import GHC.Driver.Session (updOptLevel, xopt_set, gopt_unset, gopt_set)
import GHC.Driver.Flags (GeneralFlag(Opt_EnableRewriteRules, Opt_ExposeAllUnfoldings, Opt_SpecialiseAggressively, Opt_CrossModuleSpecialise, Opt_FullLaziness, Opt_WorkerWrapper))
import GHC.Driver.Main (hscSimplify)
import GHC.Unit.Module.ModGuts (ModGuts(..))
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.LanguageExtensions.Type as LangExt
import qualified GHC.Unit.Types as UnitTypes

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr)
import System.Process (readProcess)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)

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
compileToCore = compileToCoreWith True

compileToCoreWith :: Bool -> FilePath -> IO (Either Text GhcCoreResult)
compileToCoreWith simplify inputPath = do
  libdir <- detectLibDir
  result <- runGhcCompile simplify libdir inputPath
  pure result

-- | Run a GHC session that compiles the input file and extracts Core.
runGhcCompile :: Bool -> FilePath -> FilePath -> IO (Either Text GhcCoreResult)
runGhcCompile simplify libdir inputPath = do
  runGhc (Just libdir) $ do
    -- Set up session flags: enable optimisation so demand analysis runs.
    -- Also add common source roots so the file can import other Frankenstein modules.
    -- Enable language extensions matching frankenstein.cabal so source files
    -- that rely on default-extensions (e.g. OverloadedStrings) compile.
    dflags <- getSessionDynFlags
    -- Make the `ghc` package visible so our own modules that import
    -- GHC.Core, GHC.Driver.Session, etc. can self-host.
    let inputDir = takeDirectory inputPath
        dflags1 = (updOptLevel 1 dflags)
          { DynFlags.importPaths = [inputDir, "src", "."] ++ DynFlags.importPaths dflags
          , DynFlags.packageFlags =
              -- Hide text-2.1.4 from the cabal store: the project (and
              -- organ-ir) is built against the GHC 9.14.1 global text-2.1.3,
              -- and exposing both leads to two distinct `Data.Text.Text`
              -- types when self-hosting modules that import both.
              [ DynFlags.HidePackage "text-2.1.4" ] ++
              [ DynFlags.ExposePackage "text-2.1.3" (DynFlags.UnitIdArg
                  (UnitTypes.RealUnit (UnitTypes.Definite
                     (UnitTypes.stringToUnitId "text-2.1.3-d6b3"))))
                  (DynFlags.ModRenaming True [])
              ] ++
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
        -- Disable RULES pragmas so standard list/Maybe constructors
        -- survive intact instead of being fused into build/foldr.
        dflags1' = gopt_unset (gopt_unset (gopt_unset dflags1
          Opt_EnableRewriteRules)
          Opt_FullLaziness)
          Opt_WorkerWrapper
        -- Force aggressive specialization and inlining of Prelude functions
        -- (map, filter, foldr, etc.) across module boundaries.
        dflags1'' = foldr (flip gopt_set) dflags1'
          [ Opt_ExposeAllUnfoldings
          , Opt_SpecialiseAggressively
          , Opt_CrossModuleSpecialise
          ]
        dflags2 = foldr (flip xopt_set) dflags1''
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
        -- Parse, typecheck, desugar, and run the Core simplifier
        parsed    <- parseModule modSummary
        typecked  <- typecheckModule parsed
        desugared <- desugarModule typecked

        -- Optionally run GHC's Core-to-Core optimization pipeline (simplifier,
        -- specializer, worker-wrapper, etc.) so that cross-module
        -- Prelude functions (map, filter, foldr, etc.) get inlined.
        -- Skip for self-hosted modules where the simplified Core exposes
        -- lambda-lifting capture bugs (local go bindings promoted to top-level
        -- with extra params that call sites don't pass).
        let rawGuts = dm_core_module desugared
        modGuts <- if simplify
          then do
            hscEnv <- getSession
            liftIO $ hscSimplify hscEnv [] rawGuts
          else pure rawGuts

        -- Extract the Core program and type constructors from ModGuts
        let coreProg = mg_binds modGuts
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

-- | Compile a Haskell file and all its local (home-package) imports.
-- Returns one GhcCoreResult per module in the dependency graph.
-- Library/package modules (from base, etc.) are NOT included — only
-- modules whose source files are in the project.
compileToCoreMulti :: Bool -> FilePath -> IO (Either Text [GhcCoreResult])
compileToCoreMulti simplify inputPath = do
  libdir <- detectLibDir
  runGhcCompileMulti simplify libdir inputPath

runGhcCompileMulti :: Bool -> FilePath -> FilePath -> IO (Either Text [GhcCoreResult])
runGhcCompileMulti simplify libdir inputPath = do
  runGhc (Just libdir) $ do
    -- Same session setup as runGhcCompile, plus the target file's
    -- directory in import paths so GHC can find sibling modules.
    dflags <- getSessionDynFlags
    let inputDir = takeDirectory inputPath
        dflags1 = (updOptLevel 1 dflags)
          { DynFlags.importPaths = [inputDir, "src", "."] ++ DynFlags.importPaths dflags
          , DynFlags.packageFlags =
              [ DynFlags.HidePackage "text-2.1.4" ] ++
              [ DynFlags.ExposePackage "text-2.1.3" (DynFlags.UnitIdArg
                  (UnitTypes.RealUnit (UnitTypes.Definite
                     (UnitTypes.stringToUnitId "text-2.1.3-d6b3"))))
                  (DynFlags.ModRenaming True [])
              ] ++
              [ DynFlags.ExposePackage pkg (DynFlags.PackageArg pkg)
                  (DynFlags.ModRenaming True [])
              | pkg <- ["ghc", "koka", "organ-ir"]
              ] ++ DynFlags.packageFlags dflags
          , DynFlags.packageDBFlags =
              [ DynFlags.PackageDB (DynFlags.PkgDbPath p)
              | p <- [ "/home/nyc/.local/state/cabal/store/ghc-9.14.1-0dcc/package.db"
                     , "dist-newstyle/packagedb/ghc-9.14.1"
                     ]
              ] ++ DynFlags.packageDBFlags dflags
          }
        dflags1' = gopt_unset (gopt_unset (gopt_unset dflags1
          Opt_EnableRewriteRules)
          Opt_FullLaziness)
          Opt_WorkerWrapper
        dflags1'' = foldr (flip gopt_set) dflags1'
          [ Opt_ExposeAllUnfoldings
          , Opt_SpecialiseAggressively
          , Opt_CrossModuleSpecialise
          ]
        dflags2 = foldr (flip xopt_set) dflags1''
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

    target <- guessTarget inputPath Nothing Nothing
    setTargets [target]
    _successFlag <- load LoadAllTargets

    -- Every ModSummary in the module graph is a home-package module
    -- (user source files). Library/package modules are NOT in the graph.
    modGraph <- getModuleGraph
    let summaries = mgModSummaries modGraph

    -- Translate each module independently
    results <- mapM (translateModSummary simplify) summaries

    let successes = [ gcr | Right gcr <- results ]
        failures  = [ (mn, err) | Left (mn, err) <- results ]

    -- Log any partial failures
    liftIO $ mapM_ (\(mn, err) ->
      TIO.hPutStrLn stderr $ "  Warning: skipping module " <> mn <> ": " <> err) failures

    if null successes
      then pure $ Left $ "All modules failed to translate"
      else pure $ Right successes

-- | Translate a single ModSummary through parse/typecheck/desugar/translate.
-- Returns Left (moduleName, error) on failure.
translateModSummary :: Bool -> ModSummary -> Ghc (Either (Text, Text) GhcCoreResult)
translateModSummary simplify modSummary = do
  let modNameT = T.pack $ moduleNameString (moduleName (ms_mod modSummary))
  parsed    <- parseModule modSummary
  typecked  <- typecheckModule parsed
  desugared <- desugarModule typecked
  let rawGuts = dm_core_module desugared
  modGuts <- if simplify
    then do
      hscEnv <- getSession
      liftIO $ hscSimplify hscEnv [] rawGuts
    else pure rawGuts
  let coreProg = mg_binds modGuts
      tyCons   = mg_tcs modGuts
  case translateProgram modNameT coreProg tyCons of
    Left err   -> pure $ Left (modNameT, "Core translation error: " <> err)
    Right prog -> pure $ Right $ GhcCoreResult
      { gcrModuleName = modNameT
      , gcrProgram    = prog
      }
