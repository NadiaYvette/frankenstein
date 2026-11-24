let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "lean4"
      , displayName = "Lean 4 (Perceus-Native LCNF)"
      , paradigm = Schema.ParadigmTag.TheoremProver
      , extensions = [ ".lean" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "lean-organ"
          , args = [] : List Text
          , versionProbeCmd = "lean --version"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "Int"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
