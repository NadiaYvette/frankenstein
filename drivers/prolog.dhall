let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "prolog"
      , displayName = "ISO Prolog (Declarative Grammar)"
      , paradigm = Schema.ParadigmTag.Logic
      , extensions = [ ".pl", ".pro" ]
      , ingest = Schema.IngestStrategy.DeclarativeGrammar
          { smirkGrammarPath = "drivers/grammars/prolog.smirk"
          , dpoLoweringRulesPath = "drivers/rules/prolog-organ.dhall"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "integer"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
