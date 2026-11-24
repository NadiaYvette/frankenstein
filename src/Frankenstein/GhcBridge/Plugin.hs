{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.GhcBridge.Plugin
-- Description: Non-invasive stock GHC Core plugin emitting OrganIR and .organ-iface artifacts
--
-- Hooks at the end of GHC's Core simplification pipeline via
-- @-fplugin=Frankenstein.GhcBridge.Plugin -fplugin-opt=Frankenstein.GhcBridge.Plugin:out=<dir>@,
-- translating 'ModGuts' to @OrganIR@ + @.organ-iface@ while leaving GHC compilation untouched.
module Frankenstein.GhcBridge.Plugin
  ( PluginConfig(..)
  , defaultPluginConfig
  , parsePluginOpts
  , plugin
  , installOrganCorePass
  , emitModuleOrganArtifacts
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.List (stripPrefix)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import GHC.Plugins
  ( CommandLineOption
  , CoreM
  , CoreToDo (..)
  , ModGuts (..)
  , Plugin (..)
  , defaultPlugin
  , moduleName
  , moduleNameString
  , purePlugin
  , putMsgS
  , tyConDataCons
  , isAlgTyCon
  , dataConOrigArgTys
  , getOccString
  , isId
  )
import GHC.Core (flattenBinds)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Frankenstein.GhcBridge.CoreTranslate (translateProgram)
import Frankenstein.OrganIR.Emitter (emitProgram)
import Frankenstein.Core.Interface
  ( OrganInterface(..)
  , CtorDescriptor(..)
  , ExportAbi(..)
  , MemProfile(..)
  , mkContentTag
  , encodeOrganInterfaceJson
  )

-- | Runtime configuration passed via @-fplugin-opt=Frankenstein.GhcBridge.Plugin:...@.
data PluginConfig = PluginConfig
  { pcOutDir  :: !FilePath
  , pcVerbose :: !Bool
  } deriving stock (Eq, Show)

-- | Default output settings emitting into @.frankenstein-out/@.
defaultPluginConfig :: PluginConfig
defaultPluginConfig = PluginConfig
  { pcOutDir  = ".frankenstein-out"
  , pcVerbose = False
  }

-- | Parses @out=<dir>@ and @verbose@ plugin options.
parsePluginOpts :: [CommandLineOption] -> PluginConfig
parsePluginOpts = foldl step defaultPluginConfig
  where
    step cfg opt
      | Just dir <- stripPrefix "out=" opt = cfg { pcOutDir = dir }
      | opt == "verbose"                   = cfg { pcVerbose = True }
      | otherwise                          = cfg

-- | Standard GHC plugin descriptor (requires zero GHC patches or forks).
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installOrganCorePass
  , pluginRecompile  = purePlugin
  }

-- | Appends Frankenstein's read-only Core extraction pass after GHC simplification.
installOrganCorePass :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installOrganCorePass opts todos =
  let cfg = parsePluginOpts opts
  in pure (todos ++ [CoreDoPluginPass "Frankenstein.OrganIR.Emit" (runOrganPass cfg)])

-- | Runs the Organ pass: translates ModGuts to OrganIR and .organ-iface, writing files to pcOutDir.
runOrganPass :: PluginConfig -> ModGuts -> CoreM ModGuts
runOrganPass cfg guts = do
  liftIO $ emitModuleOrganArtifacts cfg guts
  if pcVerbose cfg
    then do
      let modStr = moduleNameString (moduleName (mg_module guts))
      putMsgS $ "[frankenstein-ghc-plugin] extracted " ++ modStr
                ++ " (" ++ show (length (mg_binds guts)) ++ " top binds) -> " ++ pcOutDir cfg
    else pure ()
  pure guts

-- | Pure/IO extraction and serialization of .organ-ir.json and .organ-iface.json
emitModuleOrganArtifacts :: PluginConfig -> ModGuts -> IO ()
emitModuleOrganArtifacts cfg guts = do
  let modStr    = moduleNameString (moduleName (mg_module guts))
      modText   = T.pack modStr
      outDir    = pcOutDir cfg
      irPath    = outDir </> (modStr ++ ".organ-ir.json")
      ifacePath = outDir </> (modStr ++ ".organ-iface.json")

  createDirectoryIfMissing True outDir

  -- 1. Emit OrganIR
  case translateProgram modText (mg_binds guts) (mg_tcs guts) of
    Left _err -> pure () -- If translation encounters unsupported constructs, fall back gracefully
    Right prog -> do
      let irText = emitProgram prog
      TIO.writeFile irPath irText

  -- 2. Emit .organ-iface
  let ctors =
        [ CtorDescriptor
            { cdQualifiedName = modText <> "." <> T.pack (getOccString dc)
            , cdTag           = mkContentTag modText (T.pack (getOccString tc)) (T.pack (getOccString dc))
            , cdFieldCount    = length (dataConOrigArgTys dc)
            , cdIsLazy        = False
            }
        | tc <- mg_tcs guts
        , isAlgTyCon tc
        , dc <- tyConDataCons tc
        ]
      exports =
        [ ExportAbi
            { eaQualifiedName = modText <> "." <> T.pack (getOccString b)
            , eaArity         = 0
            , eaEvParams      = 0
            , eaProfile       = Profile1ThreadLocalPerceus
            , eaIsSynthetic   = False
            }
        | (b, _) <- flattenBinds (mg_binds guts)
        , isId b
        ]
      iface = OrganInterface
        { oiSchemaVersion = "1.0.0"
        , oiModuleName    = modText
        , oiSourceLang    = "Haskell"
        , oiCtors         = ctors
        , oiExports       = exports
        , oiImports       = []
        }
      ifaceText = encodeOrganInterfaceJson iface
  TIO.writeFile ifacePath ifaceText
