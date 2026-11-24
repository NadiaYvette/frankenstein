let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "cpp"
      , displayName = "C++20 (C-ABI Boundary)"
      , paradigm = Schema.ParadigmTag.Systems
      , extensions = [ ".cpp", ".cc", ".hpp" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "clang-organ"
          , args = [ "--std=c++20" ]
          , versionProbeCmd = "clang++ --version"
          }
      , emit = Schema.EmitStrategy.OutboundFfiWrapper
          { wrapperKind = "cpp-extern-c"
          , linkFlags = [ "-lstdc++" ]
          }
      , ffiRules = [ { sourceType = "int64_t"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = Some
          { command = "clang-format"
          , args = [] : List Text
          , supportsRange = True
          }
      }

in  manifest
