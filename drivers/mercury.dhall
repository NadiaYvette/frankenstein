let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "mercury"
      , displayName = "Mercury (mmc HLDS Determinism Ingest)"
      , paradigm = Schema.ParadigmTag.Logic
      , extensions = [ ".m" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "mmc"
          , args = [ "--dump-hlds" ]
          , versionProbeCmd = "mmc --version"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "int64"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
