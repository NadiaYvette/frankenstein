let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "scheme"
      , displayName = "R7RS Scheme (Declarative Grammar)"
      , paradigm = Schema.ParadigmTag.Functional
      , extensions = [ ".scm", ".sld" ]
      , ingest = Schema.IngestStrategy.DeclarativeGrammar
          { smirkGrammarPath = "drivers/grammars/scheme.smirk"
          , dpoLoweringRulesPath = "drivers/rules/scheme-organ.dhall"
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
