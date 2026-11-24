let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "haskell"
      , displayName = "GHC Haskell"
      , paradigm = Schema.ParadigmTag.Functional
      , extensions = [ ".hs", ".lhs" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "ghc"
          , args = [ "-fplugin=Frankenstein.GhcBridge.Plugin", "-dump-organ-json" ]
          , versionProbeCmd = "ghc --numeric-version"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules =
        [ { sourceType = "Int64"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        , { sourceType = "Text"
          , organRep = "byteslice"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = Some
          { command = "fourmolu"
          , args = [ "-m", "inplace" ]
          , supportsRange = False
          }
      }

in  manifest
