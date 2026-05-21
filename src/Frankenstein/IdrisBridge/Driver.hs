-- | Idris 2 bridge driver.
--
-- Preferred path: shell out to the @frankenstein-idris2@ shim, which
-- registers a custom @organir@ codegen in Idris2's pipeline.  The shim
-- emits a complete OrganIR JSON document from Idris2's elaborated
-- NamedDef list — far richer than what our in-tree surface parser can
-- reach.  See @idris2-shim/src/Main.idr@.
--
-- Fallback path: the legacy in-tree @parseIdris@ source parser
-- (@Frankenstein.IdrisBridge.Parse@), used only if the shim binary is
-- missing or fails.  It only handles a small @Int@-only subset.
module Frankenstein.IdrisBridge.Driver
  ( readIdrisFile
  , compileIdrisFileViaShim
  , defaultShimPath
  , defaultIdris2Prefix
  ) where

import Control.Exception (try, IOException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (copyFile, doesFileExist)
import System.Environment (getEnvironment, lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))

import Frankenstein.IdrisBridge.Parse

-- | Default path to the @frankenstein-idris2@ shim binary.
-- Override with the @FRANKENSTEIN_IDRIS2_SHIM@ environment variable.
defaultShimPath :: FilePath
defaultShimPath = "/home/nyc/src/frankenstein/idris2-shim/build/exec/frankenstein-idris2"

-- | Default @IDRIS2_PREFIX@ pointing at the system Idris2 install.
-- Override with the @IDRIS2_PREFIX@ environment variable.
defaultIdris2Prefix :: String
defaultIdris2Prefix = "/usr/lib64"

-- | Legacy: read an @.idr@ source file and parse it with our tiny
-- in-tree source parser.  Used only as a fallback when the shim is
-- unavailable.  See 'Frankenstein.IdrisBridge.Parse' for what we accept.
readIdrisFile :: FilePath -> IO (Either Text [IDecl])
readIdrisFile path = do
  src <- TIO.readFile path
  pure (parseIdris src)

-- | Invoke the @frankenstein-idris2@ shim with @--cg organir@ on the
-- given source file and return the produced OrganIR JSON text.
--
-- Runs the shim in a fresh temp directory so the shim's relative
-- @build/exec/<name>.organ.json@ output path can't collide with the
-- caller's working directory.
compileIdrisFileViaShim :: FilePath -> IO (Either Text Text)
compileIdrisFileViaShim inputFile = do
  shimPathOverride <- lookupEnv "FRANKENSTEIN_IDRIS2_SHIM"
  let shimPath = case shimPathOverride of
        Just p | not (null p) -> p
        _                     -> defaultShimPath
  shimExists <- doesFileExist shimPath
  if not shimExists
     then pure $ Left $ "Idris2 shim binary not found at " <> T.pack shimPath
     else withSystemTempDirectory "frankenstein-idris2" $ \tmpDir -> do
       prefixOverride <- lookupEnv "IDRIS2_PREFIX"
       parentEnv <- getEnvironment
       let prefixVal = case prefixOverride of
             Just p | not (null p) -> p
             _                     -> defaultIdris2Prefix
           childEnv =
             ("IDRIS2_PREFIX", prefixVal)
               : filter ((/= "IDRIS2_PREFIX") . fst) parentEnv
           outName = "idris_shim_out"
           -- Idris2 insists the source file live inside its source dir.
           -- Copy the input into tmpDir and reference it by basename so
           -- Idris2's source-dir check (the tmpDir cwd) passes.
           inputBasename = takeFileName inputFile
           inputCopy = tmpDir </> inputBasename
       copyFile inputFile inputCopy
       let prc = (proc shimPath
                       [ "--cg", "organir"
                       , "-o", outName
                       , inputBasename
                       ])
                 { cwd = Just tmpDir
                 , env = Just childEnv
                 }
       result <- try $ readCreateProcessWithExitCode prc ""
       case result :: Either IOException (ExitCode, String, String) of
         Left exc -> pure $ Left $
           "Failed to invoke Idris2 shim: " <> T.pack (show exc)
         Right (ExitFailure code, _stdout_, stderr_) ->
           pure $ Left $
             "Idris2 shim exited " <> T.pack (show code) <> ":\n"
               <> T.pack stderr_
         Right (ExitSuccess, _, _) -> do
           let jsonPath = tmpDir </> "build" </> "exec"
                                 </> (outName ++ ".organ.json")
           exists <- doesFileExist jsonPath
           if exists
              then Right <$> TIO.readFile jsonPath
              else pure $ Left $
                "Idris2 shim produced no JSON at " <> T.pack jsonPath
