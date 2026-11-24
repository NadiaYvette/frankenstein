let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "python-ext"
      , displayName = "Python C-API Extension"
      , paradigm = Schema.ParadigmTag.ScriptingFfi
      , extensions = [ ".py" ]
      , ingest = Schema.IngestStrategy.None
      , emit = Schema.EmitStrategy.OutboundFfiWrapper
          { wrapperKind = "python-capi"
          , linkFlags = [ "-lpython3" ]
          }
      , ffiRules =
        [ { sourceType = "int"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        , { sourceType = "str"
          , organRep = "byteslice"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = Some
          { command = "black"
          , args = [ "-q" ]
          , supportsRange = False
          }
      }

in  manifest
