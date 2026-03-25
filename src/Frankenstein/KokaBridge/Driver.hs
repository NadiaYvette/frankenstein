-- | Koka compiler driver for Frankenstein
--
-- Compiles .kk files using the Koka compiler library directly,
-- extracts Core IR, and translates to Frankenstein Core.

module Frankenstein.KokaBridge.Driver
  ( compileKokaFile
  ) where

import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.IORef

import Core.Core qualified as KC
import Compile.Options (Terminal(..), Flags(..), flagsNull)
import Compile.Build (runBuild)
import Compile.BuildContext (BuildContext(..), buildcEmpty, buildcAddRootSources, buildcTypeCheck)
import Compile.Module (Module(..), modCore, modName)
import Common.Error (errors)
import Common.Name (showPlain)
import System.FilePath (takeDirectory, (</>))
import System.Directory (doesDirectoryExist, getCurrentDirectory, makeAbsolute)

import Frankenstein.Core.Types qualified as F
import Frankenstein.KokaBridge.CoreTranslate (translateProgram)

-- | Compile a .kk file and translate to Frankenstein Core
compileKokaFile :: FilePath -> IO (Either Text F.Program)
compileKokaFile inputFile = do
  -- Resolve absolute path and set up include paths
  absFile <- makeAbsolute inputFile
  let srcDir = takeDirectory absFile
  kokaLibDir <- findKokaLib
  let flags = flagsNull { includePath = [srcDir] ++ kokaLibDir }

  -- Collect errors for reporting
  errRef <- newIORef ([] :: [String])
  let term = Terminal
        { termError    = \emsg -> modifyIORef errRef (show emsg :)
        , termTrace    = \_ -> pure ()
        , termProgress = \_ -> pure ()
        , termPhase    = \_ -> pure ()
        , termInfo     = \_ -> pure ()
        }

  -- Run the Koka compilation pipeline
  result <- runBuild term flags $ do
    (bc, rootNames) <- buildcAddRootSources [absFile] (buildcEmpty flags)
    bc' <- buildcTypeCheck [] bc
    let mods = buildcModules bc'
        -- Find the root module by matching against rootNames
        rootMods = [m | m <- mods, modName m `elem` rootNames]
    case rootMods of
      [] -> pure $ Left "No root module found after compilation"
      (rootMod:_) ->
        case modCore rootMod of
          Just core -> pure $ Right core
          Nothing ->
            -- Type checking failed — report errors from the module
            let modErrs = errors (modErrors rootMod)
                errMsgs = map (T.pack . show) modErrs
            in pure $ Left $ "Koka type error:\n" <> T.intercalate "\n" errMsgs

  case result of
    Left errs -> do
      collectedErrs <- readIORef errRef
      let errMsgs = map (T.pack . show) (errors errs) ++ map T.pack collectedErrs
      pure $ Left $ "Koka compilation failed:\n" <> T.intercalate "\n" (take 5 errMsgs)
    Right (inner, _warnings) ->
      case inner of
        Left err -> pure $ Left err
        Right core ->
          case translateProgram core of
            Left err   -> pure $ Left $ "Koka Core translation error: " <> err
            Right prog -> pure $ Right prog

-- | Find the Koka standard library directory
-- Looks in common locations relative to CWD and the source tree
findKokaLib :: IO [FilePath]
findKokaLib = do
  cwd <- getCurrentDirectory
  let candidates =
        [ cwd </> "donors/koka/lib"
        , cwd </> "../donors/koka/lib"
        , cwd </> "../../donors/koka/lib"
        -- Common install locations
        , "/usr/local/share/koka/lib"
        , "/usr/share/koka/lib"
        ]
  found <- mapM doesDirectoryExist candidates
  let dirs = [p | (p, True) <- zip candidates found]
  -- Also include std/ subdirectory directly for module resolution
  stdDirs <- mapM (\d -> do
    let stdPath = d </> "std"
    exists <- doesDirectoryExist stdPath
    pure (if exists then [d, stdPath] else [d])) dirs
  pure (concat stdDirs)
