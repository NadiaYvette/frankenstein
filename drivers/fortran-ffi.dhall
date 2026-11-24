let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "fortran-ffi"
      , displayName = "Fortran ISO_C_BINDING FFI"
      , paradigm = Schema.ParadigmTag.Systems
      , extensions = [ ".f90", ".f95" ]
      , ingest = Schema.IngestStrategy.None
      , emit = Schema.EmitStrategy.OutboundFfiWrapper
          { wrapperKind = "fortran-iso-c"
          , linkFlags = [ "-lgfortran" ]
          }
      , ffiRules =
        [ { sourceType = "integer(c_int64_t)"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        , { sourceType = "type(c_ptr)"
          , organRep = "ptr"
          , minProfile = Schema.MemProfile.Profile0FreestandingLinear
          }
        ]
      , formatter = Some
          { command = "fprettify"
          , args = [ "-i" ]
          , supportsRange = False
          }
      }

in  manifest
