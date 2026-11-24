let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "sml"
      , displayName = "Standard ML (Declarative / Isabelle Extraction)"
      , paradigm = Schema.ParadigmTag.Functional
      , extensions = [ ".sml", ".sig" ]
      , ingest = Schema.IngestStrategy.DeclarativeGrammar
          { smirkGrammarPath = "drivers/grammars/sml.smirk"
          , dpoLoweringRulesPath = "drivers/rules/sml-organ.dhall"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "Int64.int"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
