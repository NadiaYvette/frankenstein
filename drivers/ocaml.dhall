let Schema = ./schema/DriverSchema.dhall

let manifest : Schema.DriverManifest =
      { id = "ocaml"
      , displayName = "OCaml (-dlambda Ingest)"
      , paradigm = Schema.ParadigmTag.Functional
      , extensions = [ ".ml", ".mli" ]
      , ingest = Schema.IngestStrategy.ExternalOrganJson
          { command = "ocaml-organ"
          , args = [ "-dlambda" ]
          , versionProbeCmd = "ocamlopt -version"
          }
      , emit = Schema.EmitStrategy.None
      , ffiRules = [ { sourceType = "int64"
          , organRep = "i64"
          , minProfile = Schema.MemProfile.Profile1ThreadLocalPerceus
          }
        ]
      , formatter = Some
          { command = "ocamlformat"
          , args = [ "-" ]
          , supportsRange = False
          }
      }

in  manifest
