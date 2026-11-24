{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Driver.Registry
-- Description: Data-driven language driver plugin registry (drivers/*.dhall)
--
-- Loads declarative ingestion, emission, outbound-FFI, and formatter hook
-- manifests so Ingolstadt scales cleanly across the 15-language interop roster
-- without creating a separate Hackage package per language.
module Frankenstein.Driver.Registry
  ( ParadigmTag(..)
  , IngestStrategy(..)
  , EmitStrategy(..)
  , FfiTypeRule(..)
  , FormatterHook(..)
  , DriverManifest(..)
  , DriverRegistry(..)
  , DriverError(..)
  , emptyRegistry
  , builtInDrivers
  , loadBuiltInRegistry
  , registerDriver
  , upsertDriverOverride
  , lookupById
  , lookupByExtension
  , validateRegistry
  ) where

import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Frankenstein.Core.Interface (MemProfile (..))

-- | High-level language paradigm classification for FFI and style defaults.
data ParadigmTag
  = ParadigmSystems
  | ParadigmFunctional
  | ParadigmTheoremProver
  | ParadigmLogic
  | ParadigmScriptingFfi
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | How a driver lifts source code into @OrganIR@ + @.organ-iface@.
data IngestStrategy
  = IngestExternalOrganJson !Text ![Text] !Text
    -- ^ @(command, args, versionProbeCmd)@ invoking an @organ-bank@ shim or plugin.
  | IngestDeclarativeGrammar !FilePath !FilePath
    -- ^ @(smirkGrammarPath, dpoLoweringRulesPath)@ interpreted purely from data files.
  | IngestNone
    -- ^ Outbound-only driver (e.g., Python C-API or Fortran FFI wrapper emitter).
  deriving stock (Eq, Show)

-- | How a driver emits target code or FFI bindings from @OrganIR@ / @.organ-iface@.
data EmitStrategy
  = EmitIdiomaticTarget !Text !(Maybe FilePath)
    -- ^ @(targetFamily, optionalPolishRulesPath)@ e.g. @"rust"@, @"c11"@, @"rocq"@.
  | EmitOutboundFfiWrapper !Text ![Text]
    -- ^ @(wrapperKind, linkFlags)@ e.g. @"python-capi"@, @"fortran-iso-c"@.
  | EmitNone
  deriving stock (Eq, Show)

-- | Maps a source/target type into its canonical OrganIR ABI & Patterson FFI profile.
data FfiTypeRule = FfiTypeRule
  { ftrSourceType  :: !Text
  , ftrOrganRep    :: !Text
  , ftrMinProfile  :: !MemProfile
  } deriving stock (Eq, Ord, Show)

-- | Optional external surface formatter run post-emission (with built-in Wadler-Lindig fallback).
data FormatterHook = FormatterHook
  { fhCommand       :: !Text
  , fhArgs          :: ![Text]
  , fhSupportsRange :: !Bool
  } deriving stock (Eq, Show)

-- | Complete declarative manifest loaded from @drivers/<lang>.dhall@.
data DriverManifest = DriverManifest
  { dmId          :: !Text
  , dmDisplayName :: !Text
  , dmParadigm    :: !ParadigmTag
  , dmExtensions  :: ![Text]
    -- ^ Normalized lowercase dotted extensions (e.g. @[".hs", ".lhs"]@).
  , dmIngest      :: !IngestStrategy
  , dmEmit        :: !EmitStrategy
  , dmFfiRules    :: ![FfiTypeRule]
  , dmFormatter   :: !(Maybe FormatterHook)
  } deriving stock (Eq, Show)

-- | Indexed registry of active language drivers.
data DriverRegistry = DriverRegistry
  { drById  :: !(Map Text DriverManifest)
  , drByExt :: !(Map Text Text)
    -- ^ Maps normalized file extension -> @dmId@.
  } deriving stock (Eq, Show)

-- | Structured registry validation or registration error.
data DriverError
  = DuplicateDriverId !Text
  | ExtensionCollision !Text !Text !Text
    -- ^ @(extension, existingDriverId, newDriverId)@
  | EmptyDriverCapabilities !Text
  | InvalidExtensionFormat !Text !Text
  deriving stock (Eq, Show)

-- | Empty driver registry.
emptyRegistry :: DriverRegistry
emptyRegistry = DriverRegistry Map.empty Map.empty

normalizeExt :: Text -> Text
normalizeExt = T.toLower . T.strip

validateManifest :: DriverManifest -> Either DriverError ()
validateManifest dm
  | dmIngest dm == IngestNone && dmEmit dm == EmitNone =
      Left (EmptyDriverCapabilities (dmId dm))
  | Just bad <- find (not . T.isPrefixOf ".") (map normalizeExt (dmExtensions dm)) =
      Left (InvalidExtensionFormat (dmId dm) bad)
  | otherwise = Right ()

-- | Registers a built-in driver, rejecting duplicate IDs and colliding extensions.
registerDriver :: DriverManifest -> DriverRegistry -> Either DriverError DriverRegistry
registerDriver dm reg = do
  validateManifest dm
  if Map.member (dmId dm) (drById reg)
    then Left (DuplicateDriverId (dmId dm))
    else do
      let exts = map normalizeExt (dmExtensions dm)
      checkExtCollisions exts (drByExt reg)
      let byId'  = Map.insert (dmId dm) dm (drById reg)
          byExt' = foldr (\e m -> Map.insert e (dmId dm) m) (drByExt reg) exts
      Right (DriverRegistry byId' byExt')
  where
    checkExtCollisions [] _ = Right ()
    checkExtCollisions (e : es) extMap =
      case Map.lookup e extMap of
        Just prev -> Left (ExtensionCollision e prev (dmId dm))
        Nothing   -> checkExtCollisions es extMap

-- | Applies a user-directory driver override, replacing any prior entry with the same ID.
upsertDriverOverride :: DriverManifest -> DriverRegistry -> Either DriverError DriverRegistry
upsertDriverOverride dm reg = do
  validateManifest dm
  let strippedExt = Map.filter (/= dmId dm) (drByExt reg)
      exts        = map normalizeExt (dmExtensions dm)
      byId'       = Map.insert (dmId dm) dm (drById reg)
      byExt'      = foldr (\e m -> Map.insert e (dmId dm) m) strippedExt exts
  Right (DriverRegistry byId' byExt')

-- | Resolves a driver by its canonical identifier (e.g. @"rust"@, @"python-ext"@).
lookupById :: Text -> DriverRegistry -> Maybe DriverManifest
lookupById key reg = Map.lookup key (drById reg)

-- | Resolves a driver by file extension (e.g. @".hs"@, @".rs"@, @".f90"@).
lookupByExtension :: Text -> DriverRegistry -> Maybe DriverManifest
lookupByExtension ext reg = do
  did <- Map.lookup (normalizeExt ext) (drByExt reg)
  Map.lookup did (drById reg)

-- | Verifies internal consistency across every registered driver manifest.
validateRegistry :: DriverRegistry -> [DriverError]
validateRegistry reg =
  [ err
  | dm <- Map.elems (drById reg)
  , Left err <- [validateManifest dm]
  ]

-- | Default manifests for the core 6 languages.
builtInDrivers :: [DriverManifest]
builtInDrivers =
  [ DriverManifest
      { dmId          = "haskell"
      , dmDisplayName = "GHC Haskell"
      , dmParadigm    = ParadigmFunctional
      , dmExtensions  = [".hs", ".lhs"]
      , dmIngest      = IngestExternalOrganJson "ghc" ["-fplugin=Frankenstein.GhcBridge.Plugin", "-dump-organ-json"] "ghc --numeric-version"
      , dmEmit        = EmitNone
      , dmFfiRules    =
          [ FfiTypeRule "Int64" "i64" Profile0FreestandingLinear
          , FfiTypeRule "Text" "byteslice" Profile1ThreadLocalPerceus
          ]
      , dmFormatter   = Just (FormatterHook "fourmolu" ["-m", "inplace"] False)
      }
  , DriverManifest
      { dmId          = "c11"
      , dmDisplayName = "C11 (ISO C 2011)"
      , dmParadigm    = ParadigmSystems
      , dmExtensions  = [".c", ".h"]
      , dmIngest      = IngestExternalOrganJson "clang" ["-emit-llvm", "-c"] "clang --version"
      , dmEmit        = EmitIdiomaticTarget "c11" Nothing
      , dmFfiRules    =
          [ FfiTypeRule "int64_t" "i64" Profile0FreestandingLinear
          , FfiTypeRule "const char*" "ptr" Profile0FreestandingLinear
          ]
      , dmFormatter   = Just (FormatterHook "clang-format" ["-i"] True)
      }
  , DriverManifest
      { dmId          = "rust"
      , dmDisplayName = "Rust 2024"
      , dmParadigm    = ParadigmSystems
      , dmExtensions  = [".rs"]
      , dmIngest      = IngestExternalOrganJson "rustc" ["--emit=mir"] "rustc --version"
      , dmEmit        = EmitIdiomaticTarget "rust" Nothing
      , dmFfiRules    =
          [ FfiTypeRule "i64" "i64" Profile0FreestandingLinear
          , FfiTypeRule "&str" "byteslice" Profile0FreestandingLinear
          ]
      , dmFormatter   = Just (FormatterHook "rustfmt" [] False)
      }
  , DriverManifest
      { dmId          = "python-ext"
      , dmDisplayName = "Python C-API Extension"
      , dmParadigm    = ParadigmScriptingFfi
      , dmExtensions  = [".py"]
      , dmIngest      = IngestNone
      , dmEmit        = EmitOutboundFfiWrapper "python-capi" ["-lpython3"]
      , dmFfiRules    =
          [ FfiTypeRule "int" "i64" Profile1ThreadLocalPerceus
          , FfiTypeRule "str" "byteslice" Profile1ThreadLocalPerceus
          ]
      , dmFormatter   = Just (FormatterHook "black" ["-q"] False)
      }
  , DriverManifest
      { dmId          = "fortran-ffi"
      , dmDisplayName = "Fortran ISO_C_BINDING FFI"
      , dmParadigm    = ParadigmSystems
      , dmExtensions  = [".f90", ".f95"]
      , dmIngest      = IngestNone
      , dmEmit        = EmitOutboundFfiWrapper "fortran-iso-c" ["-lgfortran"]
      , dmFfiRules    =
          [ FfiTypeRule "integer(c_int64_t)" "i64" Profile0FreestandingLinear
          , FfiTypeRule "type(c_ptr)" "ptr" Profile0FreestandingLinear
          ]
      , dmFormatter   = Just (FormatterHook "fprettify" ["-i"] False)
      }
  , DriverManifest
      { dmId          = "rocq"
      , dmDisplayName = "Rocq / Coq / Iris Specification"
      , dmParadigm    = ParadigmTheoremProver
      , dmExtensions  = [".v"]
      , dmIngest      = IngestNone
      , dmEmit        = EmitIdiomaticTarget "rocq" Nothing
      , dmFfiRules    =
          [ FfiTypeRule "Z" "i64" Profile0FreestandingLinear
          ]
      , dmFormatter   = Nothing
      }
  ]

-- | Loads the standard 6 drivers into a verified registry.
loadBuiltInRegistry :: Either DriverError DriverRegistry
loadBuiltInRegistry = foldl' (\acc dm -> acc >>= registerDriver dm) (Right emptyRegistry) builtInDrivers
