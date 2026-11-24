let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "datalog"
      , displayName = "Datalog / Souffle (Declarative Grammar)"
      , paradigm = Schema.ParadigmTag.Logic
      , extensions = [ ".dl" ]
      , ingest = Schema.IngestStrategy.DeclarativeGrammar
          { smirkGrammarPath = "drivers/grammars/datalog.smirk"
          , dpoLoweringRulesPath = "drivers/rules/datalog-organ.dhall"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "number"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = None Schema.FormatterHook
      }

in  manifest
