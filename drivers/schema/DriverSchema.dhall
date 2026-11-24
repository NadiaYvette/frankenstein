let ParadigmTag =
      < Systems | Functional | TheoremProver | Logic | ScriptingFfi >

let IngestStrategy =
      < ExternalOrganJson :
          { command : Text, args : List Text, versionProbeCmd : Text }
      | DeclarativeGrammar :
          { smirkGrammarPath : Text, dpoLoweringRulesPath : Text }
      | None
      >

let EmitStrategy =
      < IdiomaticTarget :
          { targetFamily : Text, polishRulesPath : Optional Text }
      | OutboundFfiWrapper : { wrapperKind : Text, linkFlags : List Text }
      | None
      >

let MemProfile =
      < Profile0FreestandingLinear
      | Profile1ThreadLocalPerceus
      | Profile2ConcurrentShared
      >

let FfiTypeRule =
      { sourceType : Text, organRep : Text, minProfile : MemProfile }

let FormatterHook =
      { command : Text, args : List Text, supportsRange : Bool }

let DriverManifest =
      { id : Text
      , displayName : Text
      , paradigm : ParadigmTag
      , extensions : List Text
      , ingest : IngestStrategy
      , emit : EmitStrategy
      , ffiRules : List FfiTypeRule
      , formatter : Optional FormatterHook
      }

in  { ParadigmTag
    , IngestStrategy
    , EmitStrategy
    , MemProfile
    , FfiTypeRule
    , FormatterHook
    , DriverManifest
    }
