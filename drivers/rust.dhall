let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "rust"
      , displayName = "Rust 2024"
      , paradigm = Schema.ParadigmTag.Systems
      , extensions = [ ".rs" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "rustc"
          , args = [ "--emit=mir" ]
          , versionProbeCmd = "rustc --version"
          }
      , emit = Schema.EmitStrategy.IdiomaticTarget
          { targetFamily = "rust"
          , polishRulesPath = None Text
          }
      , ffiRules =
        [ { sourceType = "i64"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        , { sourceType = "&str"
          , organRep = "byteslice"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = Some
          { command = "rustfmt"
          , args = [] : List Text
          , supportsRange = False
          }
      }

in  manifest
