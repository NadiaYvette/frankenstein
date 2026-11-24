let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "rocq"
      , displayName = "Rocq / Coq / Iris Specification"
      , paradigm = Schema.ParadigmTag.TheoremProver
      , extensions = [ ".v" ]
      , ingest = Schema.IngestStrategy.None
      , emit = Schema.EmitStrategy.IdiomaticTarget
          { targetFamily = "rocq"
          , polishRulesPath = None Text
          }
      , ffiRules =
        [ { sourceType = "Z"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
