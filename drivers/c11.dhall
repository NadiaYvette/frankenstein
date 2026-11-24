let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "c11"
      , displayName = "C11 (ISO C 2011)"
      , paradigm = Schema.ParadigmTag.Systems
      , extensions = [ ".c", ".h" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "clang"
          , args = [ "-emit-llvm", "-c" ]
          , versionProbeCmd = "clang --version"
          }
      , emit = Schema.EmitStrategy.IdiomaticTarget
          { targetFamily = "c11"
          , polishRulesPath = None Text
          }
      , ffiRules =
        [ { sourceType = "int64_t"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        , { sourceType = "const char*"
          , organRep = "ptr"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = Some
          { command = "clang-format"
          , args = [ "-i" ]
          , supportsRange = True
          }
      }

in  manifest
