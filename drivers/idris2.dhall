let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "idris2"
      , displayName = "Idris 2 (QTT Multiplicities)"
      , paradigm = Schema.ParadigmTag.TheoremProver
      , extensions = [ ".idr" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "idris2-organ"
          , args = [] : List Text
          , versionProbeCmd = "idris2 --version"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "Int64"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
