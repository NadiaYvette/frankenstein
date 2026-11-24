-- Interface Witness Σ_C emitted by Yamaarashi.Campaign.Unfold
{
  interfaceName = "Backend.C.Box",
  targetRepoDir = "/home/nyc/src/frankenstein",
  modules = [ "Backend.C.Box", "Backend.C.FromCore", "Backend.C.Parc", "Backend.C.ParcReuse", "Backend.C.ParcReuseSpec", "Backend.CSharp.FromCore", "Backend.JavaScript.FromCore", "Common.ColorScheme", "Common.Error", "Common.Failure", "Common.File", "Common.Id", "Common.IdMap", "Common.IdNice", "Common.IdSet", "Common.Message", "Common.Name", "Common.NameMap", "Common.NamePrim", "Common.NameSet", "Common.QNameMap", "Common.Range", "Common.ResumeKind", "Common.Syntax", "Common.Unique", "Compile.Build", "Compile.CodeGen", "Compile.Module", "Compile.Optimize", "Compile.Package", "Compile.TypeCheck", "Compile.Options", "Compile.BuildContext", "Core.AnalysisCCtx", "Core.AnalysisMatch", "Core.AnalysisResume", "Core.BindingGroups", "Core.Borrowed", "Core.CTail", "Core.Check", "Core.CheckFBIP", "Core.Core", "Core.CoreVar", "Core.Divergent", "Core.FunLift", "Core.Inline", "Core.Inlines", "Core.Monadic", "Core.MonadicLift", "Core.OpenResolve", "Core.Parse", "Core.Pretty", "Core.Simplify", "Core.Specialize", "Core.UnReturn", "Core.Uniquefy", "Core.Unroll", "Interpreter.Command", "Interpreter.Interpret", "Kind.Assumption", "Kind.Constructors", "Kind.ImportMap", "Kind.Infer", "Kind.InferKind", "Kind.InferMonad", "Kind.Kind", "Kind.Newtypes", "Kind.Pretty", "Kind.Repr", "Kind.Synonym", "Kind.Unify", "Lib.JSON", "Lib.PPrint", "Lib.Printer", "Lib.Scc", "Lib.Trace", "Main.Run", "LanguageServer.Conversions", "LanguageServer.Handler.CodeAction", "LanguageServer.Handler.Commands", "LanguageServer.Handler.Completion", "LanguageServer.Handler.Definition", "LanguageServer.Handler.DocumentSymbol", "LanguageServer.Handler.Folding", "LanguageServer.Handler.Hover", "LanguageServer.Handler.InlayHints", "LanguageServer.Handler.Pretty", "LanguageServer.Handler.SignatureHelp", "LanguageServer.Handler.TextDocument", "LanguageServer.Handlers", "LanguageServer.Monad", "LanguageServer.Run", "Main", "Main", "Platform.Config", "Platform.Console", "Platform.Filetime", "Platform.GetOptions", "Platform.ReadLine", "Platform.Runtime", "Platform.Var", "Platform.Config", "Platform.Console", "Platform.GetOptions", "Platform.ReadLine", "Platform.Runtime", "Platform.Var", "Platform.Config", "Platform.Console", "Platform.Filetime", "Platform.GetOptions", "Platform.ReadLine", "Platform.Runtime", "Platform.Var", "Static.BindingGroups", "Static.FixityResolve", "Syntax.Colorize", "Syntax.GenDoc", "Syntax.Highlight", "Syntax.Layout", "Syntax.Lexeme", "Syntax.Parse", "Syntax.Pretty", "Syntax.Promote", "Syntax.RangeMap", "Syntax.Syntax", "Type.Assumption", "Type.InfGamma", "Type.Infer", "Type.InferMonad", "Type.Kind", "Type.Operations", "Type.Pretty", "Type.Type", "Type.TypeVar", "Type.Unify", "Main", "ZipSet", "Frankenstein.Core.FlattenPatterns", "Frankenstein.Core.DeriveSelectors", "Frankenstein.Core.KokaCore", "Frankenstein.Core.Types", "Frankenstein.Core.CpsConvert", "Frankenstein.Core.NormalizePatterns", "Frankenstein.Core.Liveness", "Frankenstein.Core.CycleAnalysis", "Frankenstein.Core.PromoteRecursiveLambdas", "Frankenstein.Core.TypeCheck", "Frankenstein.Core.Linker", "Frankenstein.Core.ElideConsumedDrops", "Frankenstein.Core.Perceus", "Frankenstein.Core.ConTags", "Frankenstein.Core.ConsumingUses", "Frankenstein.Core.EffectOpt", "Frankenstein.Core.Evidence", "Frankenstein.Core.EvidenceEvv", "Frankenstein.GhcBridge.Driver", "Frankenstein.GhcBridge.CoreTranslate", "Frankenstein.MercuryBridge.CoreTranslate", "Frankenstein.MercuryBridge.HldsParse", "Frankenstein.RustBridge.CoreTranslate", "Frankenstein.RustBridge.MirParse", "Frankenstein.MlirEmit.Dialects", "Frankenstein.MlirEmit.PostProcess", "Frankenstein.MlirEmit.Emitter", "Frankenstein.OrganIR.Emitter", "Frankenstein.OrganIR.Consumer", "Frankenstein.KokaBridge.CoreTranslate", "Frankenstein.KokaBridge.Driver", "Frankenstein.PythonBridge.AstParse", "Frankenstein.PythonBridge.CoreTranslate", "Frankenstein.GoBridge.AstParse", "Frankenstein.GoBridge.CoreTranslate", "Frankenstein.FutharkBridge.CoreTranslate", "Frankenstein.FutharkBridge.Parser", "Frankenstein.SchemeBridge.Reader", "Frankenstein.SchemeBridge.CoreTranslate", "Frankenstein.SwiftBridge.AstParse", "Frankenstein.SwiftBridge.Driver", "Frankenstein.SwiftBridge.CoreTranslate", "Frankenstein.OCamlBridge.Driver", "Frankenstein.OCamlBridge.AstParse", "Frankenstein.OCamlBridge.CoreTranslate", "Frankenstein.ErlangBridge.Driver", "Frankenstein.ErlangBridge.AstParse", "Frankenstein.ErlangBridge.CoreTranslate", "Frankenstein.FSharpBridge.Driver", "Frankenstein.FSharpBridge.CoreTranslate", "Frankenstein.IdrisBridge.Parse", "Frankenstein.IdrisBridge.CoreTranslate", "Frankenstein.IdrisBridge.Driver", "Frankenstein.Debug.DumpProgram", "Frankenstein.Debug.PerceusCounts", "Frankenstein.Funnel.MVP", "OrganIR.Types", "OrganIR.Build", "OrganIR.Json", "OrganIR.Validate", "OrganIR.Pretty", "OrganIR.Schema", "OrganIR.Transform", "OrganIR.Binary", "OrganIR.Parse", "Factorial", "Arith", "FactorialBench", "TypeclassTest", "UnboxedTest", "WorkerWrapperTest", "JoinPointTest", "KOracle", "BridgeBisim", "Main", "Main", "Main", "Scoring", "Scoring", "Fib", "CrossModuleLib", "CrossModuleMain", "StdlibHaskell", "FfiImport", "Tree", "MaybeSum", "ListSum", "Nested", "AdtMain", "AllocStress", "HigherOrder", "MutualRec", "Closure", "MultiAdt", "ExhaustTail", "StdlibList", "StdlibMaybe", "StdlibBool", "StdlibTuple", "PreludeHof", "CrossModuleLib", "CrossModule", "SinglePatCon", "MultiPatConExhaustive", "PatConWithPatVar", "ChainedPatBindGuards", "BoolGuardsWhere", "SinglePatLit", "WrongDefaultBody", "ChainedIO", "Hello", "HelloUtf8", "ShowCompound", "ShowDerived", "ShowInt", "ShowMixed", "ShowTuple", "Fib", "Tak", "Ack", "Main" ],
  exportedSymbols = [ "boxCore", "cFromCore", "parcCore", "getDataDef'", "getDataInfo'", "needsDupDropData", "parcReuseCore", "getFixedDataAllocSize", "Reusable", "trace", "csharpFromCore", "ArityMap", "ExtMap", "arityMapInit", "externalMapInit", "javascriptFromCore", "ColorScheme", "failure", "assertion", "todo", "matchFailure", "raise", "raiseIO", "catchIO", "HasCallStack", "getEnvVar", "searchPaths", "searchPathsSuffixes", "searchPathsEx", "searchPathsCanonical", "getMaximalPrefixPath", "searchProgram", "runSystem", "runSystemRaw", "runCmd", "runCmdRead", "runCmdEnv", "getProgramPath", "startsWith", "endsWith", "splitOn", "trim", "FileName", "basename", "notdir", "notext", "joinPath", "joinPaths", "extname", "dirname", "noexts", "splitPath", "undelimPaths", "isPathSep", "isPathDelimiter", "findMaximalPrefixPath", "isAbsolute", "commonPathPrefix", "normalizeWith", "normalize", "isLiteralDoc", "ensureExt", "FileTime", "fileTime0", "maxFileTime", "maxFileTimes", "showTimeDiff", "fileTimeCompare", "getFileTime", "getFileTimeOrCurrent", "getCurrentTime", "readTextFile", "writeTextFile", "copyTextFile", "copyTextIfNewer", "copyTextIfNewerWith", "copyTextFileWith", "copyBinaryFile", "copyBinaryIfNewer", "removeFileIfExists", "realPath", "doesFileExistAndNotEmpty", "makeRelativeToPaths", "getCwd", "relativeToPath", "seqList", "seqMaybe", "seqEither", "seqTuple2", "seqString", "seqqList", "seqqMaybe", "seqqEither", "seqqTuple2", "seqqString", "Id", "Ids", "showId", "genId", "newId", "newIdFromId", "idNil", "idNumber", "IdMap", "module", "Nice", "niceEmpty", "niceExtend", "niceShow", "nicePretty", "IdSet", "module", "tablex", "ppRange", "docFromRange", "docsFromRanges", "Name", "Names", "ModuleName", "showName", "showPlain", "showTupled", "readTupled", "readQualified", "readQualifiedName", "labelNameCompare", "toHiddenUniqueName", "newName", "newModuleName", "newQualified", "newLocallyQualified", "nameAsModuleName", "nameNil", "nameIsNil", "nameStartsWith", "nameCaseEqual", "nameCaseOverlap", "isSameNamespace", "nameCaseEqualPrefixOf", "nameCaseOverlapPrefixOf", "qualify", "unqualify", "isQualified", "qualifier", "nameModule", "nameStem", "nameLocal", "nameLocalQual", "isModuleName", "newPaddingName", "isPaddingName", "isCCtxName", "newFieldName", "isFieldName", "isWildcard", "unWildcard", "nameIsEtaHole", "typeQualifiedName", "typeQualifiedNameOf", "typeQualifiedGetTypeName", "newHiddenExternalName", "isHiddenExternalName", "newHiddenName", "isHiddenName", "hiddenNameStartsWith", "makeHiddenName", "makeFreshHiddenName", "newHiddenNameEx", "toUniqueName", "newImplicitTypeVarName", "isImplicitTypeVarName", "newCreatorName", "isCreatorName", "toHandlerName", "fromHandlerName", "isHandlerName", "isHandlerConName", "toOpSelectorName", "fromOpSelectorName", "isOpSelectorName", "toOperationsName", "fromOperationsName", "isOperationsName", "toEffectTagName", "toHandleName", "isHandleName", "toOpsConName", "toOpConName", "toOpTypeName", "toConstructorName", "isConstructorName", "toVarName", "toHandlerConName", "toOpenTagName", "isOpenTagName", "toLazyIndirectConName", "isLazyIndirectConName", "toValueOperationName", "isValueOperationName", "fromValueOperationsName", "toBasicOperationsName", "splitModuleName", "unsplitModuleName", "mergeCommonPath", "splitLocalQualName", "missingQualifier", "isEarlyBindName", "toImplicitParamName", "isImplicitParamName", "splitImplicitParamName", "fromImplicitParamName", "prepend", "postpend", "asciiEncode", "moduleNameToPath", "pathToModuleName", "canonicalSep", "canonicalName", "nonCanonicalName", "canonicalSplit", "prettyName", "prettyCoreName", "requalifyLocally", "qualifyLocally", "unqualifyFull", "isLocallyQualified", "fullQualifier", "unqualifyAsModuleName", "unqualifyLocally", "nameMapStem", "isInDefaultNameSpace", "isInWrongNameSpace", "showHex", "showBinary", "showHexFloat", "NameMap", "module", "find", "nameMain", "nameType", "nameInteractiveModule", "nameSystemCore", "nameCoreTypes", "nameCoreDebug", "isSystemCoreName", "shortenSystemCoreName", "isPrimitiveModule", "nameCoreHnd", "isPrimitiveName", "nameOpExpr", "nameCoreFileLine", "nameCoreFileModule", "nameCoreFileFile", "namePatternMatchError", "nameMainConsole", "nameCopy", "copyNameOf", "nameAssign", "nameRefSet", "nameByref", "nameDeref", "nameIndex", "nameDecreasing", "nameSubStr1", "nameAssert", "nameUnit", "nameReturn", "nameTrace", "nameLog", "nameEffectOpen", "nameToAny", "nameBind", "nameBind2", "nameIsValidK", "nameLift", "nameInject", "nameInjectExn", "nameInjectResource", "nameTpResourceTag", "nameConResourceTag", "nameConEv", "nameIntAdd", "nameIntSub", "nameTpHTag", "nameHTag", "nameTpClause", "namePerform", "nameTpEvv", "nameEvvAt", "nameEvvIndex", "nameEvvIndexMask", "nameOpenAt", "nameOpen", "nameOpenNone", "nameTpEv", "nameHandle", "nameNamedHandle", "nameTpResumeContext", "nameClause", "nameIdentity", "nameMaskAt", "nameMaskBuiltin", "isClauseTailName", "nameClauseTailNoOp", "nameTpEvIndex", "nameYielding", "nameYieldExtend", "nameEvvIsAffine", "nameInitially", "nameFinally", "nameUnsafeTotal", "nameIntConst", "nameByte", "nameInt16", "nameInt64", "nameInternalInt32", "nameInt32", "nameInternalSSizeT", "nameSSizeT", "nameIntPtrT", "nameTpBox", "nameUnbox", "nameBox", "nameBoxCon", "nameDup", "nameDrop", "nameIsUnique", "nameFree", "nameDecRef", "nameKeepMatch", "nameDropMatch", "nameReuseMatch", "nameTpReuse", "nameDropReuse", "nameFreeReuse", "nameReuseNull", "nameAssignReuse", "nameReuse", "nameReuseIsValid", "nameAllocAt", "nameConFieldsAssign", "nameConTagFieldsAssign", "nameConTagScanFieldsAssign", "nameReuseDrop", "nameDropSpecial", "nameKeep", "nameSetTag", "constructor", "nameTpCCtxx", "nameTpCCtx", "nameCCtxCreate", "nameCCtxHoleCreate", "nameCCtxEmpty", "nameCCtxApply", "nameCCtxExtend", "nameCCtxCompose", "nameCCtxComposeExtend", "nameCCtxSetCtxPath", "nameTpFieldAddr", "nameFieldAddrOf", "nameTpCField", "nameTpCTailAcc", "nameCFieldHole", "nameCFieldSet", "nameCFieldOf", "nameCTailUnit", "nameCTailCompose", "nameCTailApply", "nameCTailSetCtxPath", "nameTrue", "nameFalse", "nameJust", "nameNothing", "nameOptional", "nameOptionalNone", "nameTpDelay", "nameListNil", "nameCons", "nameTpList", "nameLazyMemoizeTarget", "nameLazyMemoize", "nameLazyEnter", "nameLazyLeave", "nameLazyIsWhnf", "nameLazyPtrIsWhnf", "nameLazyIndirectCompress", "nameDataTypePtrIsUnique", "nameDataTypePtrIsThreadShared", "nameEffectEmpty", "nameEffectExtend", "nameEffectAppend", "nameTpBool", "nameTpInt", "nameTpChar", "nameTpFloat", "nameTpFloat32", "nameTpFloat16", "nameTpString", "nameTpByte", "nameTpInt8", "nameTpInt16", "nameTpInt32", "nameTpInt64", "nameTpSSizeT", "nameTpIntPtrT", "nameTpAny", "nameTpException", "nameTpMaybe", "makeTpHandled", "nameTpHandled", "nameTpHandled1", "nameTpNHandled", "nameTpNHandled1", "nameTpMarker", "nameTpOperation", "nameYieldOp", "nameTpCps", "nameTpYld", "nameTpCont", "nameInCps", "nameTpHandlerBranch0", "nameTpHandlerBranch1", "nameMakeNull", "nameConstNull", "nameReturnNull", "nameReturnNull1", "nameTpNull", "nameTpValueOp", "nameValueOp", "nameTpNamed", "nameTpScope", "nameTpAsync", "nameTpAsyncX", "nameApplyK", "nameMakeHandler", "nameMakeHandlerRet", "nameMakeContextTp", "nameTpOpMatch", "nameOpMatch", "nameOpNoMatch", "nameTpMDict", "nameTpDict", "nameTpBuilder", "nameTpUnit", "nameTpVoid", "nameTpRef", "nameRef", "nameTpLocalVar", "nameTpLocal", "nameLocalVar", "nameRunLocal", "nameLocalSet", "nameLocalGet", "nameLocalNew", "nameTpOptional", "nameTpArray", "nameTpVector", "nameVector", "nameVectorFromList", "nameVectorUnsafeCreate", "nameTpTotal", "nameTpDiv", "nameTpPartial", "nameTpPure", "nameTpST", "nameTpWrite", "nameTpRead", "nameTpIO", "nameTpAlloc", "nameTuple", "isNameTuple", "nameTpTuple", "isNameTpTuple", "nameAnd", "nameOr", "nameTypeHeapDiv", "nameEvHeapDiv", "nameEvHeapNoDiv", "nameHeapDiv", "nameKindStar", "nameKindFun", "nameKindLabel", "nameKindEffect", "nameKindHeap", "nameKindScope", "nameKindHandled1", "nameKindHandled", "namesSameSize", "NameSet", "module", "QNameMap", "Lookup", "Pos", "ResumeKind", "Visibility", "setUnique", "unique", "uniques", "uniqueId", "uniqueIds", "uniqueName", "uniqueNameFrom", "Build", "VFS", "codeGen", "Link", "LinkResult", "Module", "coreOptimize", "Packages", "PackageName", "searchPackages", "discoverPackages", "packageInfoFromDir", "packagesEmpty", "joinPkg", "typeCheck", "importMapFromCoreImports", "processOptions", "Mode", "BuildContext", "analyzeCCtx", "makeCCtxEmpty", "makeCCtxCreate", "makeCCtxSetContextPath", "makeFieldAddrOf", "analyzeBranches", "analyzeResume", "regroup", "borrowedNew", "borrowedEmpty", "borrowedExtend", "borrowedExtends", "borrowedLookup", "borrowedLookupFip", "borrowedExtendICore", "ppBorrowed", "extractBorrowDefs", "extractBorrowDef", "extractBorrowExternals", "extractBorrowed", "borrowedCompose", "ctailOptimize", "uctailOptimize", "checkCore", "checkFBIP", "HasExprVar", "analyzeDivergence", "liftFunctions", "inlineDefs", "inlinesNew", "inlinesEmpty", "inlinesExtend", "inlinesExtends", "inlinesLookup", "inlinesToList", "inlinesMerge", "inlinesFilter", "ppInlines", "extractInlineDefGroups", "extractInlineDefs", "monTransform", "monMakeBind", "monadicLift", "openResolve", "parseCore", "prettyCore", "prettyExpr", "prettyPattern", "prettyDef", "prettyDefs", "prettyDefGroup", "source", "simplify", "uniqueSimplify", "simplifyDefs", "specialize", "extractSpecializeDefs", "unreturn", "uniquefy", "uniquefyDefGroup", "uniquefyExpr", "uniquefyExprWith", "uniquefyExprU", "uniquefyDefGroups", "unrollDefs", "Command", "interpret", "kgammaInit", "kgammaNew", "kgammaNewNub", "kgammaEmpty", "kgammaIsEmpty", "kgammaExtend", "kgammaLookup", "kgammaLookupQ", "kgammaFind", "kgammaList", "kgammaUnion", "kgammaUnionLeftBias", "ppKGamma", "kgammaFilter", "extractKGamma", "ConInfo", "ImportMap", "importsEmpty", "importsExtend", "importsExpand", "importsAlias", "importsList", "inferKinds", "InfKind", "KInfer", "runKindInfer", "addError", "addWarning", "freshKind", "freshTypeVar", "subst", "getKGamma", "addSynonym", "getSynonyms", "getAllNewtypes", "getPlatform", "extendInfGamma", "extendKGamma", "extendKSub", "findInfKind", "getColorScheme", "lookupSynInfo", "lookupDataInfo", "qualifyDef", "addRangeInfo", "infQualifiedName", "checkExternal", "withDataEffects", "lookupDataEffect", "DataInfo", "ppKind", "niceKinds", "colorKind", "kindColon", "orderConFields", "createDataDef", "Context", "readJSON", "readJSONFromFile", "parseJSON", "parseJSONFromFile", "JsValue", "Doc", "Docs", "Pretty", "prettyList", "n", "trace", "traceDoc", "ctrace", "Color", "runPlain", "runWithLS", "runWith", "toLspRange", "toLspLocation", "toLspLocationLink", "toLspDiagnostics", "toLspErrorDiagnostics", "toLspWarningDiagnostic", "makeDiagnostic", "filePathToUri", "errorMessageToDiagnostic", "fromLspRange", "fromLspLocation", "fromLspUri", "codeActionHandler", "commandHandler", "completionHandler", "definitionHandler", "documentSymbolHandler", "foldingHandler", "hoverHandler", "formatRangeInfoHover", "inlayHintsHandler", "ppComment", "asKokaCode", "signatureHelpHandler", "didOpenHandler", "didChangeHandler", "didSaveHandler", "didCloseHandler", "rebuildUri", "ReactorInput", "LSState", "runLanguageServer", "runLanguageServer", "runPlain", "since", "setColor", "setBackColor", "setReverse", "setUnderline", "withConsole", "bracketConsole", "getProgramPath", "FileTime", "getCurrentTime", "getFileTime", "getFileTimeOrCurrent", "setFileTime", "fileTime0", "fileTimeToPicoseconds", "showTimeDiff", "usageInfo", "ArgOrder", "withReadLine", "readLine", "readLineEx", "addHistory", "exCatch", "unsafePerformIO", "finally", "copyBinaryFile", "showHFloat", "Var", "newVar", "takeVar", "putVar", "setColor", "setBackColor", "setReverse", "setUnderline", "withConsole", "bracketConsole", "usageInfo", "ArgOrder", "withReadLine", "readLine", "readLineEx", "addHistory", "exCatch", "unsafePerformIO", "finally", "copyBinaryFile", "Var", "newVar", "takeVar", "putVar", "setColor", "setBackColor", "setReverse", "setUnderline", "withConsole", "bracketConsole", "FileTime", "getCurrentTime", "getFileTime", "getFileTimeOrCurrent", "fileTime0", "usageInfo", "ArgOrder", "withReadLine", "readLine", "readLineEx", "addHistory", "exCatch", "unsafePerformIO", "finally", "copyBinaryFile", "Var", "newVar", "takeVar", "putVar", "groupProgramBindings", "groupTypeDefBindings", "groupBindings", "hasFreeVar", "fixityResolve", "Fixities", "findFixity", "fixitiesEmpty", "fixitiesCompose", "fixitiesNew", "resolve", "colorize", "tag", "span", "cspan", "escapes", "escape", "prefix", "htmlHeader", "htmlFooter", "signature", "linkModule", "popup", "fmtHtml", "fmtName", "fmtTypeName", "fmtNameString", "fmtModuleName", "linkFromModName", "linkEncode", "fmtLiterate", "showDoc", "capitalize", "endWithDot", "removeComment", "kindSignature", "genDoc", "Context", "layout", "combineLineComments", "lexSource", "Lexeme", "parseProgramFromFile", "parseProgramFromString", "parseValueDef", "parseTypeDef", "parseExpression", "parseType", "parseDependencies", "parseProgramFromLexemes", "lexParse", "parseLex", "LexParser", "parseLexemes", "parseInline", "ignoreSyntaxWarnings", "visibility", "modulepath", "importAlias", "tbinderId", "funid", "paramid", "braced", "semiBraces", "semis", "semiColons1", "semiBraced", "angles", "anglesCommas", "parensCommas", "parens", "curlies", "semiColon", "lparen", "rparen", "langle", "rangle", "comma", "lapp", "lidx", "bar", "qtypeid", "qvarid", "qconid", "qidop", "identifier", "qoperator", "varid", "idop", "op", "integer", "charLit", "floatLit", "stringLit", "special", "specialId", "specialOp", "specialConId", "wildcard", "keyword", "dockeyword", "docconid", "typeDeclKind", "paramInfo", "fromMaybe", "isJust", "promote", "promoteType", "quantify", "promoteFree", "RangeMap", "RangeInfo", "nameTrue", "nameSystemCore", "NameInfo", "infgammaNew", "infgammaSingle", "infgammaEmpty", "infgammaIsEmpty", "infgammaExtend", "infgammaExtends", "infgammaLookupEx", "infgammaLookup", "infgammaMap", "infgammaList", "infgammaDelete", "ppInfGamma", "infgammaUnion", "infgammaExtendX", "inferTypes", "coreVarInfoFromNameInfo", "Inf", "InfGamma", "runInfer", "tryRun", "traceDoc", "traceDefDoc", "traceIndent", "zapSubst", "subst", "extendSub", "getGamma", "extendGamma", "extendGammaCore", "extendInfGamma", "extendInfGammaEx", "extendInfGammaCore", "withGammaType", "resolveName", "resolveRhsName", "resolveFunName", "resolveConName", "resolveConPatternName", "resolveImplicitName", "lookupAppName", "lookupFunName", "lookupLocalName", "lookupNameCtx", "lookupInfName", "NameContext", "HasKind", "instantiate", "instantiateEx", "instantiateNoEx", "extend", "skolemize", "skolemizeEx", "freshTVar", "freshTVars", "freshEffect", "freshStar", "freshSub", "isOptionalOrImplicit", "splitOptionalImplicit", "requiresImplicits", "hasOptionalOrImplicits", "ppScheme", "ppTypeVar", "ppDataInfo", "ppSynInfo", "prettyDataInfo", "prettyConInfo", "prettyDefFunType", "ppSchemeEffect", "ppDeclType", "niceTypeInitial", "niceTypeExtend", "niceTypeExtendVars", "precTop", "precArrow", "precApp", "precAtom", "pparens", "Env", "ftv", "btv", "ftc", "Unify", "UnifyError", "ZipSet", "empty", "member", "insert", "flattenPatterns", "flattenPatternsExpr", "deriveSelectors", "recordSelectors", "Perceus", "evidence", "inlining", "cpsExpr", "cpsExprFresh", "cpsTopLevel", "runCps", "Cps", "freshK", "normalizePatterns", "in", "CycleInfo", "promoteRecursiveLambdas", "promoteRecursiveLambdasProgram", "typeCheckProgram", "TypeError", "same", "elideConsumedDrops", "elideConsumedDropsProgram", "insertPerceus", "analyzeUsage", "freeVars", "UsageInfo", "e", "consumingUseCount", "consumingUseCounts", "effectOptimize", "inlineLocalHandlers", "eliminateIdentityHandlers", "annotateTailResumptive", "isAbortHandler", "HandlerKind", "evidencePass", "evidencePassDef", "collectGlobalEffects", "evidencePassGlobal", "evidencePassEvv", "evidencePassEvvGlobal", "collectTopNames", "hashEffectName", "compileToCore", "compileToCoreWith", "compileToCoreMulti", "GhcCoreResult", "translateProgram", "translateExpr", "translateTyCons", "translateHlds", "translateMultiHlds", "translatePred", "MercuryHLDS", "translateMir", "translateBody", "MirProgram", "MlirOp", "postProcessFile", "postProcessText", "emitProgram", "emitProgramText", "emitProgramWasm", "emitProgramWithEffects", "compileToExecutable", "compileToWasm", "CompileTarget", "emitOrganIR", "emitModule", "emitProgram", "OrganIR", "translateProgram", "translateProgramMulti", "translateExpr", "translateType", "compileKokaFile", "see", "translatePythonAst", "translatePyModule", "GoSExpr", "translateGoAst", "translateGoFile", "translateFuthark", "p1", "SExpr", "translateScheme", "translateSchemeFile", "Node", "readSwiftFile", "emitSilCounts", "SilCounts", "translateSwift", "readOCamlFile", "OLine", "translateOCaml", "readErlangFile", "ETerm", "translateErlang", "readFSharpFile", "ignored", "translateIdris", "readIdrisFile", "compileIdrisFileViaShim", "defaultShimPath", "defaultIdris2Prefix", "dumpProgram", "analyzeProgram", "LambdaSite", "MVPAst", "Text", "Text", "renderOrganIR", "validateOrganIR", "Warning", "ppOrganIR", "ppDefinition", "ppExpr", "ppTy", "ppDataType", "ppEffectDecl", "schemaCheck", "SchemaError", "runPasses", "mapDefs", "mapExprs", "foldExpr", "constantFold", "deadDefElim", "inlineTrivial", "simplifyCase", "etaReduce", "encodeOrganIR", "decodeOrganIR", "parseOrganIR", "JVal", "n", "3", "n", "desugars", "n", "strict", "kOracleTests", "genExpr", "exprToK", "runKOracle", "runMlirPipeline", "bridgeBisimTests", "main", "main", "main", "n", "x", "Koka", "map", "Python", "Go", "Node", "MyJust", "Cons", "newtype", "Node", "N", "Int", "n", "Int", "MyJust", "MkPair", "Just", "which", "Real", "x", "double", "triple", "sumList", "auto", "Just", "Just", "all", "multi", "integer", "hPutStr2", "the", "builds", "n2", "n3", "parens", "with", "enum", "sum", "3", "4", "5", "n", "tak", "m", "Map" ],
  typeSignatures =
    [ { symbol = "boxCore", signature = "HasCallStack => Core -> Unique Core" }
    , { symbol = "boxDefGroups", signature = "HasCallStack => DefGroups -> Unique DefGroups" }
    , { symbol = "boxDefGroup", signature = "HasCallStack => DefGroup -> Unique DefGroup" }
    , { symbol = "boxDef", signature = "HasCallStack => Def -> Unique Def" }
    , { symbol = "boxExpr", signature = "HasCallStack => BoxType -> Expr -> Unique Expr" }
    , { symbol = "boxBranch", signature = "HasCallStack => [BoxType] -> BoxType -> Branch -> Unique Branch" }
    , { symbol = "boxGuard", signature = "HasCallStack => BoxType -> (Expr -> Expr) -> Guard -> Unique Guard" }
    , { symbol = "boxPattern", signature = "BoxType -> Pattern -> Unique (Pattern, [Def])" }
    , { symbol = "boxPatternX", signature = "BoxType -> Pattern -> Unique (Pattern,[Def])" }
    , { symbol = "bcoerce", signature = "Type -> Type -> Expr -> Unique Expr" }
    , { symbol = "bcoerceX", signature = "Type -> Type -> Expr -> Unique (Maybe Expr)" }
    , { symbol = "boxCoerceFun", signature = "[(Name,Type)] -> Effect -> Type -> [(Name,Type)] -> Effect -> Type  -> Expr -> Unique Expr" }
    , { symbol = "boxBindExprAsValue", signature = "Type -> Type -> Expr -> (Expr -> Unique Expr) -> Unique Expr" }
    , { symbol = "makeYielding", signature = "Expr" }
    , { symbol = "makeYieldExtend", signature = "Type -> Type -> Expr -> Unique Expr" }
    , { symbol = "tvarA,tvarB,tvarE", signature = "TypeVar" }
    , { symbol = "boxTypeOf", signature = "HasCallStack => Expr -> BoxType" }
    , { symbol = "boxType", signature = "Type -> BoxType" }
    , { symbol = "boxedFunType", signature = "Type -> Unique Type" }
    , { symbol = "boxedTypeVar", signature = "Unique Type" }
    , { symbol = "typeBox", signature = "Kind -> BoxType" }
    , { symbol = "cType", signature = "Type -> CType" }
    , { symbol = "isBoxPat", signature = "Pattern -> Bool" }
    , { symbol = "patBox", signature = "Type -> Type -> Pattern -> Pattern" }
    , { symbol = "boxConRepr", signature = "ConRepr" }
    , { symbol = "boxConInfo", signature = "ConInfo" }
    , { symbol = "debug", signature = "Bool" }
    , { symbol = "externalNames", signature = "[(TName, Doc)]" }
    , { symbol = "cFromCore", signature = "Bool -> CTarget -> BuildType -> FilePath -> Pretty.Env -> Platform -> Newtypes -> Borrowed -> Int -> Bool -> Bool -> Bool -> Bool -> Bool -> Int -> Maybe (Name,Bool) -> String -> Core -> (Doc,Doc,Maybe Doc,Core)" }
    , { symbol = "contextDoc", signature = "Doc" }
    , { symbol = "contextParam", signature = "Doc" }
    , { symbol = "genModule", signature = "Bool -> CTarget -> BuildType -> FilePath -> Pretty.Env -> Platform -> Newtypes -> Borrowed -> Bool -> Bool -> Bool -> Bool -> Int -> Maybe (Name,Bool) -> String -> Core -> Asm (Core,Maybe Doc)" }
    , { symbol = "moduleImport", signature = "Import -> Doc" }
    , { symbol = "includeExternalC", signature = "CTarget -> BuildType -> External -> [Doc]" }
    , { symbol = "includeExternalH", signature = "CTarget -> BuildType -> External -> [Doc]" }
    , { symbol = "includeEndExternalH", signature = "CTarget -> BuildType -> External -> [Doc]" }
    , { symbol = "importExternalInclude", signature = "CTarget -> BuildType -> FilePath -> External -> [Doc]" }
    , { symbol = "genMain", signature = "[Doc] -> Name -> Platform -> Int -> Maybe (Name,Bool) -> String -> Maybe Doc" }
    , { symbol = "genLocalGroups", signature = "[DefGroup] -> Asm [Doc]" }
    , { symbol = "genLocalGroup", signature = "DefGroup -> Asm Doc" }
    , { symbol = "genLocalDef", signature = "Def -> Asm Doc" }
    , { symbol = "userComment", signature = "String -> Doc" }
    , { symbol = "genTopGroups", signature = "[DefGroup] -> Asm ()" }
    , { symbol = "genTopGroup", signature = "DefGroup -> Asm ()" }
    , { symbol = "genFunTopDefSig", signature = "Def -> Asm ()" }
    , { symbol = "genFunDefSig", signature = "Bool -> Def -> Doc" }
    , { symbol = "genFunSig", signature = "Bool -> Visibility -> Name -> Expr -> Doc" }
    , { symbol = "genLamSig", signature = "Bool -> Visibility -> Name -> [TName] -> Expr -> Doc" }
    , { symbol = "genTopDef", signature = "Bool -> Bool -> Def -> Asm ()" }
    , { symbol = "genTopDefDecl", signature = "Bool -> Bool -> Def -> Asm ()" }
    , { symbol = "unitSemi", signature = "Type -> Doc" }
    , { symbol = "genTopLevelStringLiteral", signature = "Name -> Visibility -> String -> Asm ()" }
    , { symbol = "genTypeDefs", signature = "TypeDefGroups -> Asm ()" }
    , { symbol = "genTypeDefGroup", signature = "TypeDefGroup -> Asm ()" }
    , { symbol = "genTypeDefPre", signature = "TypeDef -> Asm ()" }
    , { symbol = "genTypeDefPost", signature = "TypeDef -> Asm ()" }
    , { symbol = "maxScanCountOf", signature = "[(ConInfo,ConRepr,[(Name,Type)],Int)] -> Int" }
    , { symbol = "minScanCountOf", signature = "[(ConInfo,ConRepr,[(Name,Type)],Int)] -> Int" }
    , { symbol = "genConstructorType", signature = "DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()" }
    , { symbol = "ppConField", signature = "(Name,Type) -> Doc" }
    , { symbol = "genConstructor", signature = "DataInfo -> DataRepr -> Int -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()" }
    , { symbol = "genConstructorTest", signature = "DataInfo -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Asm ()" }
    , { symbol = "genConstructorTestX", signature = "DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()" }
    , { symbol = "genConstructorCreate", signature = "DataInfo -> DataRepr -> ConInfo -> ConRepr -> [(Name,Type)] -> Int -> Int -> Asm ()" }
    , { symbol = "genConstructorBaseCast", signature = "DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()" }
    , { symbol = "genConstructorAccess", signature = "DataInfo -> DataRepr -> ConInfo -> ConRepr -> Asm ()" }
    , { symbol = "genBoxUnbox", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "dataStructAsMaybeSplit", signature = "[ConInfo] -> (ConInfo,ConInfo)" }
    , { symbol = "genDupDrop", signature = "Name -> DataInfo -> DataRepr -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Asm ()" }
    , { symbol = "genIsUnique", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genFree", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genDecRef", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genDropReuseFun", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genDropNFun", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genReuse", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genHole", signature = "Name -> DataInfo -> DataRepr -> Asm ()" }
    , { symbol = "genScanFields", signature = "Name -> DataInfo -> DataRepr -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Asm ()" }
    , { symbol = "genScanFieldTests", signature = "Int -> ((ConInfo,ConRepr,[(Name,Type)],Int),Int) -> Doc" }
    , { symbol = "genDupDropX", signature = "Bool -> Name -> DataInfo -> DataRepr -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Asm ()" }
    , { symbol = "genDupDropIso", signature = "Bool -> (ConInfo,ConRepr,[(Name,Type)],Int) -> Doc" }
    , { symbol = "genDupDropMaybe", signature = "Bool -> [(ConInfo,ConRepr,[(Name,Type)],Int)] -> Doc" }
    , { symbol = "genDupDropValue", signature = "Bool -> DataRepr -> Int -> [Doc]" }
    , { symbol = "genDupDropMatch", signature = "[(Doc,[Doc])] -> [Doc]" }
    , { symbol = "genDupDropTests", signature = "Bool -> DataRepr -> (ConInfo,ConRepr,[(Name,Type)],Int) -> (Doc,[Doc])" }
    , { symbol = "genDupDropTestsX", signature = "Bool -> DataRepr -> Int -> ((ConInfo,ConRepr,[(Name,Type)],Int),Int) -> Doc" }
    , { symbol = "genDupDropFields", signature = "Bool -> DataRepr -> ConInfo -> [(Name,Type)] -> [Doc]" }
    , { symbol = "genDupDropCall", signature = "Bool -> Type -> Doc -> [Doc]" }
    , { symbol = "genIsUniqueCall", signature = "Type -> Doc -> [Doc]" }
    , { symbol = "genFreeCall", signature = "Type -> Doc -> [Doc]" }
    , { symbol = "genDecRefCall", signature = "Type -> Doc -> [Doc]" }
    , { symbol = "genDropReuseCall", signature = "Type -> [Doc] -> [Doc]" }
    , { symbol = "genReuseCall", signature = "Type -> Doc -> [Doc]" }
    , { symbol = "genDropNCall", signature = "Type -> [Doc] -> [Doc]" }
    , { symbol = "genHoleCall", signature = "Type -> Doc" }
    , { symbol = "conBaseCastNameInfo", signature = "ConInfo -> Doc" }
    , { symbol = "conBaseCastName", signature = "Name -> Doc" }
    , { symbol = "conCreateNameInfo", signature = "ConInfo -> Doc" }
    , { symbol = "conCreateName", signature = "Name -> Doc" }
    , { symbol = "conSingletonName", signature = "ConInfo -> Doc" }
    , { symbol = "conAsName", signature = "ConInfo -> Doc" }
    , { symbol = "openTagName", signature = "Name -> Doc" }
    , { symbol = "ntparameters", signature = "[(Name,Type)] -> Doc" }
    , { symbol = "parameters", signature = "[Doc] -> Doc" }
    , { symbol = "arguments", signature = "[Doc] -> Doc" }
    , { symbol = "ppVis", signature = "Visibility -> Doc" }
    , { symbol = "typeConClassName", signature = "Name -> Name" }
    , { symbol = "typeClassName", signature = "Name -> Name" }
    , { symbol = "ppDefName", signature = "Name -> Doc" }
    , { symbol = "dataReprMayHaveSingletons", signature = "DataRepr -> Bool" }
    , { symbol = "genLambda", signature = "[TName] -> Effect -> Expr -> Asm Doc" }
    , { symbol = "ppType", signature = "Type -> Doc" }
    , { symbol = "cType", signature = "Type -> CType" }
    , { symbol = "cPrimCanBeBoxed", signature = "String -> Bool" }
    , { symbol = "getResult", signature = "Result -> Doc -> Doc" }
    , { symbol = "tryTailCall", signature = "Result -> Expr -> Asm (Maybe Doc)" }
    , { symbol = "genStat", signature = "Result -> Expr -> Asm Doc" }
    , { symbol = "genExprStat", signature = "Result -> Expr -> Asm Doc" }
    , { symbol = "genMatch", signature = "Result -> [Doc] -> [Branch] -> Asm Doc" }
    , { symbol = "genBranch", signature = "Result -> [Doc] -> Bool -> Branch -> Asm Doc" }
    , { symbol = "genGuards", signature = "Result -> [Guard] -> Bindings -> Asm Doc" }
    , { symbol = "genGuard", signature = "Result -> ([Doc], Bindings) -> Guard -> Asm ([Doc], Bindings)" }
    , { symbol = "parensIf", signature = "Doc -> Doc -- avoid parens if already parenthesized" }
    , { symbol = "genPattern", signature = "Bool -> Bool -> Bindings -> [(Doc,Pattern)] -> (Bindings -> Asm Doc) -> Asm Doc" }
    , { symbol = "genPatternTest", signature = "Bool -> Bool -> (Doc,Pattern) -> Asm [([Doc],[Doc],Bindings,[(Doc,Pattern)])]" }
    , { symbol = "genNextPatterns", signature = "(Doc -> Doc -> Doc) -> Doc -> Type -> [Pattern] -> [(Doc,Pattern)]" }
    , { symbol = "genExpr", signature = "Expr -> Asm ([Doc],Doc)" }
    , { symbol = "genExprs", signature = "[Expr] -> Asm ([Doc],[Doc])" }
    , { symbol = "genInlineableExprs", signature = "[Expr] -> Asm ([Doc],[Doc])" }
    , { symbol = "genInlineableExpr", signature = "Expr -> Asm ([Doc],Doc)" }
    , { symbol = "genVarBinding", signature = "Expr -> Asm (Doc, TName)" }
    , { symbol = "genPure", signature = "Expr -> Asm Doc" }
    , { symbol = "genLambda", signature = "Expr -> ([TypeVar],[Pred],[(Name,Type)],Effect,Type) -> Asm Doc" }
    , { symbol = "isPat", signature = "Bool -> Pattern -> Bool" }
    , { symbol = "genInline", signature = "Expr -> Asm Doc" }
    , { symbol = "genApp", signature = "Expr -> [Expr] -> Asm ([Doc],Doc)" }
    , { symbol = "genAppNormal", signature = "Expr -> [Expr] -> Asm ([Doc],Doc)" }
    , { symbol = "ppCtxPath", signature = "ConRepr -> TName -> Bool -> [Doc]" }
    , { symbol = "genAssignFields", signature = "Doc -> TName -> ConRepr -> TName -> [Name] -> [Expr] -> Asm ([Doc], Doc, [Doc], Doc)" }
    , { symbol = "genFieldAddress", signature = "TName -> Name -> Name -> Doc" }
    , { symbol = "genCCtxSetContextPath", signature = "TName -> Name -> Name -> Doc" }
    , { symbol = "genAppSpecial", signature = "Expr -> [Expr] -> Asm (Maybe Doc)" }
    , { symbol = "genAppInline", signature = "Expr -> [Expr] -> Asm Doc" }
    , { symbol = "extractExtern", signature = "Expr -> Maybe (TName,[(Target,String)])" }
    , { symbol = "genInlineExternal", signature = "TName -> [(Target,String)] -> [Doc] -> Asm Doc" }
    , { symbol = "genExprExternal", signature = "TName -> [(Target,String)] -> [Doc] -> Asm ([Doc],Doc)" }
    , { symbol = "getFormat", signature = "TName -> [(Target,String)] -> String" }
    , { symbol = "genDefName", signature = "TName -> Asm Doc" }
    , { symbol = "genVarName", signature = "String -> Asm Doc" }
    , { symbol = "genVarNames", signature = "Int -> Asm [Doc]" }
    , { symbol = "genCommentTName", signature = "TName -> Asm Doc" }
    , { symbol = "extractExternal", signature = "Expr -> Maybe (TName, String, [Expr])" }
    , { symbol = "isFunExpr", signature = "Expr -> Bool" }
    , { symbol = "isReuseNull", signature = "Expr -> Bool" }
    , { symbol = "isInlineableExpr", signature = "Expr -> Bool" }
    , { symbol = "isPureExpr", signature = "Expr -> Bool" }
    , { symbol = "isTailCalling", signature = "Expr -> Name -> Bool" }
    , { symbol = "runAsm", signature = "Int -> Env -> Asm a -> (a,Doc,Doc)" }
    , { symbol = "getInit", signature = "Asm Doc" }
    , { symbol = "getDone", signature = "Asm Doc" }
    , { symbol = "getTop", signature = "Asm Doc" }
    , { symbol = "getDefToHeader", signature = "Asm Bool" }
    , { symbol = "withDef", signature = "Name -> Bool -> Asm a -> Asm a" }
    , { symbol = "newVarName", signature = "String -> Asm Name" }
    , { symbol = "newVarNames", signature = "Int -> Asm [Name]" }
    , { symbol = "getModule", signature = "Asm Name" }
    , { symbol = "newDefVarName", signature = "String -> Asm Name" }
    , { symbol = "getPrettyEnv", signature = "Asm Pretty.Env" }
    , { symbol = "getEagerPatBind", signature = "Asm Bool" }
    , { symbol = "withTypeVars", signature = "[TypeVar] -> Asm a -> Asm a" }
    , { symbol = "withNameSubstitutions", signature = "[(TName, Doc)] -> Asm a -> Asm a" }
    , { symbol = "getNewtypes", signature = "Asm Newtypes" }
    , { symbol = "getPlatform", signature = "Asm Platform" }
    , { symbol = "ppLit", signature = "Lit -> Doc" }
    , { symbol = "cstring", signature = "String -> (Doc,Int)" }
    , { symbol = "genLitInt32", signature = "Integer -> Doc" }
    , { symbol = "genLitInt64", signature = "Integer -> Doc" }
    , { symbol = "genLitUInt8", signature = "Integer -> Doc" }
    , { symbol = "genLitSSizeT", signature = "Integer -> Doc" }
    , { symbol = "genLitIntPtrT", signature = "Integer -> Doc" }
    , { symbol = "ppName", signature = "Name -> Doc" }
    , { symbol = "ppQName", signature = "Name -> Name -> Doc" }
    , { symbol = "ppModName", signature = "Name -> Doc" }
    , { symbol = "encode", signature = "Bool -> Name -> Doc" }
    , { symbol = "isReserved", signature = "String -> Bool" }
    , { symbol = "reserved", signature = "S.Set String" }
    , { symbol = "inlineblock", signature = "Doc -> Doc" }
    , { symbol = "block", signature = "Doc -> Doc" }
    , { symbol = "tblock", signature = "Doc -> Doc -> Doc" }
    , { symbol = "tcoBlock", signature = "Doc -> Doc -> Doc" }
    , { symbol = "tailcall", signature = "Doc" }
    , { symbol = "object", signature = "[(Doc, Doc)] -> Doc" }
    , { symbol = "tab", signature = "Doc -> Doc" }
    , { symbol = "comment", signature = "Doc -> Doc" }
    , { symbol = "linecomment", signature = "Doc -> Doc" }
    , { symbol = "debugComment", signature = "String -> Doc" }
    , { symbol = "debugWrap", signature = "String -> Doc -> Doc" }
    , { symbol = "tagField", signature = "Doc" }
    , { symbol = "constdecl", signature = "Doc" }
    , { symbol = "tparameters", signature = "[TName] -> Doc" }
    , { symbol = "resultType", signature = "Type -> Type" }
    , { symbol = "parcCore", signature = "Pretty.Env -> Platform -> Newtypes -> Borrowed -> Bool -> Core -> Unique Core" }
    , { symbol = "parcDefGroups", signature = "Bool -> DefGroups -> Parc DefGroups" }
    , { symbol = "parcDefGroup", signature = "Bool -> DefGroup -> Parc DefGroup" }
    , { symbol = "parcDef", signature = "Bool -> Def -> Parc Def" }
    , { symbol = "parcTopLevelExpr", signature = "DefSort -> Expr -> Parc Expr" }
    , { symbol = "parcExpr", signature = "Expr -> Parc Expr" }
    , { symbol = "parcLam", signature = "Expr -> S.Set TName -> Expr -> Parc ([Maybe Expr], Expr)" }
    , { symbol = "parcBorrowApp", signature = "TName -> [Expr] -> Expr -> Parc Expr" }
    , { symbol = "parcBorrowArg", signature = "Expr -> ParamInfo -> Parc ([DefGroup], Maybe Expr, Expr)" }
    , { symbol = "varNames", signature = "[Expr] -> [TName]" }
    , { symbol = "parcBranches", signature = "[TName] -> [Branch] -> Parc [Branch]" }
    , { symbol = "parcBranch", signature = "[TName] -> Live -> Branch -> Parc (Live -> Parc Branch)" }
    , { symbol = "parcGuard", signature = "[TName] -> [Pattern] -> Live -> Guard -> Parc (Live -> Parc Guard)" }
    , { symbol = "parcGuardRC", signature = "Dups -> Drops -> Expr -> Parc Expr" }
    , { symbol = "mchildrenOf", signature = "ShapeMap -> TName -> Maybe TNames" }
    , { symbol = "childrenOf", signature = "ShapeMap -> TName -> TNames" }
    , { symbol = "conNameOf", signature = "ShapeMap -> TName -> Maybe Name" }
    , { symbol = "optimizeGuard", signature = "Bool {-specialize?-} -> Dups -> Drops -> Parc [Maybe Expr]" }
    , { symbol = "optimizeDupDrops", signature = "ShapeMap -> Dups -> Drops -> Parc [Maybe Expr]" }
    , { symbol = "specializeDrop", signature = "ShapeMap -> Dups -> TName -> Parc [Maybe Expr]" }
    , { symbol = "fuseDupDrops", signature = "ShapeMap -> Dups -> Drops -> Parc (Dups, Drops)" }
    , { symbol = "fuseAliases", signature = "ShapeMap -> Dups -> Drops -> Parc (Dups,Drops)" }
    , { symbol = "forwardingChild", signature = "Platform -> Newtypes -> ShapeMap -> Dups -> TName -> Maybe TName" }
    , { symbol = "inferShapes", signature = "[TName] -> [Pattern] -> ShapeMap" }
    , { symbol = "mergeShapeInfo", signature = "ShapeInfo -> ShapeInfo -> ShapeInfo   -- prefer most info" }
    , { symbol = "caseIsNormalized", signature = "[Expr] -> [Branch] -> Bool" }
    , { symbol = "normalizeCase", signature = "Expr -> Parc Expr" }
    , { symbol = "caseExpandExpr", signature = "Expr -> Parc (Expr, Maybe DefGroup)" }
    , { symbol = "normalizeBranch", signature = "[Expr] -> Branch -> Branch" }
    , { symbol = "useTName", signature = "TName -> Parc (Maybe Expr)" }
    , { symbol = "useTNameBorrowed", signature = "TName -> Parc (Maybe Expr)" }
    , { symbol = "getBoxForm'", signature = "Platform -> Newtypes -> Type -> BoxForm" }
    , { symbol = "getBoxForm", signature = "Type -> Parc BoxForm" }
    , { symbol = "genDecRef", signature = "TName -> Parc (Maybe Expr)" }
    , { symbol = "needsDupDrop", signature = "Type -> Parc Bool" }
    , { symbol = "needsDupDropData", signature = "DataInfo -> Bool" }
    , { symbol = "isValueType", signature = "Type -> Parc Bool" }
    , { symbol = "getValueForm'", signature = "Newtypes -> Type -> Maybe ValueForm" }
    , { symbol = "getValueForm", signature = "Type -> Parc (Maybe ValueForm)" }
    , { symbol = "getFieldTypes", signature = "Type -> Maybe Name -> Parc (Maybe [Type])" }
    , { symbol = "genDupDrop", signature = "Bool -> TName -> Maybe (ConRepr,Name) -> Maybe Int -> Parc (Maybe Expr)" }
    , { symbol = "isIsoBox", signature = "Type -> Bool" }
    , { symbol = "dupDropFun", signature = "Bool -> Type -> Maybe (ConRepr,Name) -> Maybe Int -> Expr -> Expr" }
    , { symbol = "exprIsNotRefcounted", signature = "Expr -> Parc Bool" }
    , { symbol = "reverseMapM", signature = "Monad m => (a -> m b) -> [a] -> m [b]" }
    , { symbol = "foldMapM", signature = "(Monad m, Foldable t) => (a -> m b) -> t a -> m [b]" }
    , { symbol = "maybeStats", signature = "[Maybe Expr] -> Expr -> Expr" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Parc a -> Parc a" }
    , { symbol = "getEnv", signature = "Parc Env" }
    , { symbol = "updateSt", signature = "(ParcState -> ParcState) -> Parc ()" }
    , { symbol = "getSt", signature = "Parc ParcState" }
    , { symbol = "runParc", signature = "Pretty.Env -> Platform -> Newtypes -> Borrowed -> Bool -> Parc a -> Unique a" }
    , { symbol = "getCurrentDef", signature = "Parc [Def]" }
    , { symbol = "withCurrentDef", signature = "Def -> Parc a -> Parc a" }
    , { symbol = "getPrettyEnv", signature = "Parc Pretty.Env" }
    , { symbol = "withPrettyEnv", signature = "(Pretty.Env -> Pretty.Env) -> Parc a -> Parc a" }
    , { symbol = "allowSpecialize", signature = "Parc Bool" }
    , { symbol = "getNewtypes", signature = "Parc Newtypes" }
    , { symbol = "withNewtypes", signature = "(Newtypes -> Newtypes) -> Parc a -> Parc a" }
    , { symbol = "getPlatform", signature = "Parc Platform" }
    , { symbol = "getOwned", signature = "Parc Owned" }
    , { symbol = "updateOwned", signature = "(Owned -> Owned) -> Parc a -> Parc a" }
    , { symbol = "getUniq", signature = "Parc Int" }
    , { symbol = "modifyUniq", signature = "(Int -> Int) -> Parc ()" }
    , { symbol = "setUniq", signature = "Int -> Parc ()" }
    , { symbol = "getLive", signature = "Parc Live" }
    , { symbol = "modifyLive", signature = "(Live -> Live) -> Parc ()" }
    , { symbol = "setLive", signature = "Live -> Parc ()" }
    , { symbol = "isOwned", signature = "TName -> Parc Bool" }
    , { symbol = "isBorrowed", signature = "TName -> Parc Bool" }
    , { symbol = "withOwned", signature = "Owned -> Parc a -> Parc a" }
    , { symbol = "extendOwned", signature = "Owned -> Parc a -> Parc a" }
    , { symbol = "extendShapes", signature = "ShapeMap -> Parc a -> Parc a" }
    , { symbol = "getShapeMap", signature = "Parc ShapeMap" }
    , { symbol = "getShapeInfo", signature = "TName -> Parc ShapeInfo" }
    , { symbol = "getParamInfos", signature = "Name -> Parc [ParamInfo]" }
    , { symbol = "markLive", signature = "TName -> Parc ()" }
    , { symbol = "markLives", signature = "Foldable t => t TName -> Parc ()" }
    , { symbol = "forget", signature = "TNames -> Parc ()" }
    , { symbol = "isLive", signature = "TName -> Parc Bool" }
    , { symbol = "isolated", signature = "Parc a -> Parc (a, Live)" }
    , { symbol = "isolated_", signature = "Parc a -> Parc a" }
    , { symbol = "isolateWith", signature = "Live -> Parc a -> Parc (a, Live)" }
    , { symbol = "scoped", signature = "TNames -> Parc a -> Parc a" }
    , { symbol = "ownedInScope", signature = "TNames -> Parc Expr -> Parc Expr" }
    , { symbol = "parcTraceDoc", signature = "(Pretty.Env -> Doc) -> Parc ()" }
    , { symbol = "parcTrace", signature = "String -> Parc ()" }
    , { symbol = "getDataInfo'", signature = "Newtypes -> Type -> Maybe DataInfo" }
    , { symbol = "getDataDef'", signature = "Newtypes -> Type -> Maybe DataDef" }
    , { symbol = "getDataInfo", signature = "Type -> Parc (Maybe DataInfo)" }
    , { symbol = "getIsDataAsMaybe", signature = "Type -> Parc Bool" }
    , { symbol = "getDataDef", signature = "Type -> Parc DataDef" }
    , { symbol = "extractDataDefType", signature = "Type -> Maybe Name" }
    , { symbol = "isBoxType", signature = "Type -> Bool" }
    , { symbol = "parcReuseCore", signature = "Pretty.Env -> Bool -> Platform -> Newtypes -> Core -> Unique Core" }
    , { symbol = "ruDefGroups", signature = "DefGroups -> Reuse DefGroups" }
    , { symbol = "ruDefGroup", signature = "DefGroup -> Reuse DefGroup" }
    , { symbol = "ruTopLevelDef", signature = "Def -> Reuse Def" }
    , { symbol = "getConInfo", signature = "Type -> Name -> Reuse (Maybe ConInfo)" }
    , { symbol = "extractDataName", signature = "Type -> Maybe Name" }
    , { symbol = "ruToAssign", signature = "Match -> Reuse ([DefGroup],(Expr,Bool {-is match?-}))" }
    , { symbol = "extractCon", signature = "Expr -> Maybe (TName, ConRepr)" }
    , { symbol = "ruSpecCon'", signature = "HasCallStack => TName -> TName -> ConRepr -> ConInfo -> Maybe (Int,Maybe Int) -> [Match] -> Reuse Expr" }
    , { symbol = "stripLets", signature = "Expr -> ([DefGroup], Expr)" }
    , { symbol = "makeLets", signature = "[DefGroup] -> Expr -> Expr" }
    , { symbol = "ruExpr", signature = "Expr -> Reuse Expr" }
    , { symbol = "ruLam", signature = "[TName] -> Effect -> Expr -> Reuse Expr" }
    , { symbol = "ruLet", signature = "Def -> Expr -> Reuse Expr" }
    , { symbol = "ruLetExpr", signature = "Expr -> Reuse (Reused -> ([TName], Expr -> Expr))" }
    , { symbol = "ruLet'", signature = "Def -> Reuse (Reused -> ([TName], Expr -> Expr))" }
    , { symbol = "ruMakeAvailable", signature = "TName -> Reuse (Maybe TName)" }
    , { symbol = "ruGetScan", signature = "TName -> Reuse (Maybe Int)" }
    , { symbol = "ruBranches", signature = "[TName] -> [Branch] -> Reuse [Branch]" }
    , { symbol = "ruBranch", signature = "[TName] -> Branch -> Reuse (Map.Map TName Expr -> Branch, Reused, Available)" }
    , { symbol = "addDeconstructed", signature = "(TName, Maybe Pattern, Int, Int) -> Reuse ()" }
    , { symbol = "ruPattern", signature = "TName -> Pattern -> Reuse [(TName, Maybe Pattern, Int {-byte size-}, Int {-scan fields-})]" }
    , { symbol = "ruGuard", signature = "Guard -> Reuse (Map.Map TName Expr -> Guard, Available)" }
    , { symbol = "ruIsReusable", signature = "ConRepr -> Platform -> Reusable" }
    , { symbol = "ruTryReuseCon", signature = "TName -> ConRepr -> Expr -> Reuse Expr" }
    , { symbol = "ruLazyMemoize", signature = "HasCallStack => TName -> Expr -> Reuse Expr" }
    , { symbol = "genDecRef", signature = "TName -> Expr" }
    , { symbol = "genDup", signature = "TName -> Expr" }
    , { symbol = "genDropReuse", signature = "TName -> Expr {- : int32 -} -> Expr" }
    , { symbol = "genAllocAt", signature = "ReuseInfo -> Expr -> Type -> Expr" }
    , { symbol = "genIsUnique", signature = "TName -> Expr" }
    , { symbol = "genWhitehole", signature = "TName -> Expr" }
    , { symbol = "genConsWhitehole", signature = "TName -> Expr" }
    , { symbol = "genFree", signature = "TName -> Expr" }
    , { symbol = "genReuseDrop", signature = "TName -> Expr" }
    , { symbol = "genReuseNull", signature = "Expr" }
    , { symbol = "genReuseAddress", signature = "TName -> Expr" }
    , { symbol = "genReuseAssignWith", signature = "TName -> Expr -> Expr" }
    , { symbol = "uniqueReuseName", signature = "Type -> Reuse TName" }
    , { symbol = "uniqueTName", signature = "String -> Type -> Reuse TName" }
    , { symbol = "foldMapM", signature = "(Monad m, Foldable t) => (a -> m b) -> t a -> m [b]" }
    , { symbol = "maybeStats", signature = "[Maybe Expr] -> Expr -> Expr" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Reuse a -> Reuse a" }
    , { symbol = "getEnv", signature = "Reuse Env" }
    , { symbol = "updateSt", signature = "(ReuseState -> ReuseState) -> Reuse ()" }
    , { symbol = "getSt", signature = "Reuse ReuseState" }
    , { symbol = "runReuse", signature = "Pretty.Env -> Bool -> Platform -> Newtypes -> Reuse a -> Unique a" }
    , { symbol = "getCurrentDef", signature = "Reuse [Def]" }
    , { symbol = "withCurrentDef", signature = "Def -> Reuse a -> Reuse a" }
    , { symbol = "getPrettyEnv", signature = "Reuse Pretty.Env" }
    , { symbol = "withPrettyEnv", signature = "(Pretty.Env -> Pretty.Env) -> Reuse a -> Reuse a" }
    , { symbol = "getNewtypes", signature = "Reuse Newtypes" }
    , { symbol = "withNewtypes", signature = "(Newtypes -> Newtypes) -> Reuse a -> Reuse a" }
    , { symbol = "getPlatform", signature = "Reuse Platform" }
    , { symbol = "getUniq", signature = "Reuse Int" }
    , { symbol = "modifyUniq", signature = "(Int -> Int) -> Reuse ()" }
    , { symbol = "setUniq", signature = "Int -> Reuse ()" }
    , { symbol = "getAvailable", signature = "Reuse Available" }
    , { symbol = "updateAvailable", signature = "(Available -> Available) -> Reuse ()" }
    , { symbol = "setAvailable", signature = "Available -> Reuse ()" }
    , { symbol = "setAvailableIntersect", signature = "[Available] -> Reuse ()" }
    , { symbol = "getDeconstructed", signature = "Reuse Deconstructed" }
    , { symbol = "updateDeconstructed", signature = "(Deconstructed -> Deconstructed) -> Reuse ()" }
    , { symbol = "setDeconstructed", signature = "Deconstructed -> Reuse ()" }
    , { symbol = "deconstructedIntersect", signature = "[Deconstructed] -> Deconstructed" }
    , { symbol = "markReused", signature = "TName -> Reuse ()" }
    , { symbol = "askAndDeleteReused", signature = "TName -> Reuse Bool" }
    , { symbol = "getReused", signature = "Reuse Reused" }
    , { symbol = "setReused", signature = "Reused -> Reuse ()" }
    , { symbol = "reusedUnion", signature = "[Reused] -> Reused" }
    , { symbol = "getEnableReuse", signature = "Reuse Bool" }
    , { symbol = "registerLazyCon", signature = "TName -> Reuse ()" }
    , { symbol = "getRegisteredLazyCon", signature = "Reuse (Maybe TName)" }
    , { symbol = "deconstructLazyCon", signature = "TName -> Reuse ()" }
    , { symbol = "getDeconstructedLazyCon", signature = "HasCallStack => Reuse (TName, TName)" }
    , { symbol = "maybeMarkDeconstructedLazyConAsReused", signature = "HasCallStack => TName -> Reuse ()" }
    , { symbol = "withNone", signature = "Reuse a -> Reuse a" }
    , { symbol = "isolateAvailable", signature = "Reuse a -> Reuse a" }
    , { symbol = "isolateGetAvailable", signature = "Reuse a -> Reuse (a,Available)" }
    , { symbol = "isolateDeconstructed", signature = "Reuse a -> Reuse a" }
    , { symbol = "isolateGetReused", signature = "Reuse a -> Reuse (a, Reused)" }
    , { symbol = "getConstructorSize", signature = "TName -> ConRepr -> Reuse Int" }
    , { symbol = "ruTraceDoc", signature = "(Pretty.Env -> Doc) -> Reuse ()" }
    , { symbol = "ruTrace", signature = "String -> Reuse ()" }
    , { symbol = "ruError", signature = "HasCallStack => String -> Reuse a" }
    , { symbol = "getRuFixedDataAllocSize", signature = "Type -> Reuse (Maybe (Int, Int))" }
    , { symbol = "getFixedDataAllocSize", signature = "Platform -> Newtypes -> Type -> Maybe (Int, Int)" }
    , { symbol = "getLazyIndirectCon", signature = "TName -> Reuse (Maybe (ConInfo,ConRepr))" }
    , { symbol = "getDataInfo", signature = "Newtypes -> Type -> Maybe (Name,DataInfo)" }
    , { symbol = "getConInfo", signature = "Newtypes -> TName -> Maybe ConInfo" }
    , { symbol = "constructorSizeOf", signature = "Platform -> Newtypes -> ConInfo -> ConRepr -> (Int {- byte size -}, Int {- scan fields -})" }
    , { symbol = "constructorSizeOfX", signature = "Platform -> Newtypes -> TName -> ConRepr -> (Int {- byte size -}, Int {- scan fields -})" }
    , { symbol = "constructorSize", signature = "Platform -> Newtypes -> DataRepr -> [Type] -> (Int {- byte size -}, Int {- scan fields -})" }
    , { symbol = "orderConFieldsEx", signature = "Platform -> Newtypes -> Bool -> [(Name,Type)] -> ([(Name,Type)],Int,Int)" }
    , { symbol = "newtypesDataDefRepr", signature = "Newtypes -> Type -> Maybe (DataDef,DataRepr)" }
    , { symbol = "extractDataDefType", signature = "Type -> Maybe Name" }
    , { symbol = "hasTagField", signature = "DataRepr -> Bool" }
    , { symbol = "parcReuseSpecialize", signature = "Pretty.Env -> Newtypes -> Core -> Unique Core" }
    , { symbol = "ruDefGroups", signature = "DefGroups -> Reuse DefGroups" }
    , { symbol = "ruDefGroup", signature = "DefGroup -> Reuse DefGroup" }
    , { symbol = "ruDef", signature = "Def -> Reuse Def" }
    , { symbol = "ruExpr", signature = "Expr -> Reuse Expr" }
    , { symbol = "ruBranches", signature = "[Branch] -> Reuse [Branch]" }
    , { symbol = "ruBranch", signature = "Branch -> Reuse Branch" }
    , { symbol = "ruGuard", signature = "Guard -> Reuse Guard" }
    , { symbol = "ruSpecialize", signature = "TName -> VarInfo -> Expr -> Reuse (Maybe Expr)" }
    , { symbol = "ruSpecCon", signature = "TName -> TName -> ConRepr -> ConInfo -> Bool -> Int -> Type -> ([Expr] -> Expr) -> [Match] -> Reuse Expr" }
    , { symbol = "isMatch", signature = "Match -> Bool" }
    , { symbol = "tryMatch", signature = "Expr -> Pattern -> Match" }
    , { symbol = "ruToAssign", signature = "Match -> Reuse ([DefGroup],(Expr,Bool {-is match?-}))" }
    , { symbol = "genReuseIsValid", signature = "TName -> Expr" }
    , { symbol = "genConTagScanFieldsAssign", signature = "Type -> TName -> ConRepr -> TName -> Int -> Int -> [(Name,Expr)] -> Expr" }
    , { symbol = "genConTagFieldsAssign", signature = "Type -> TName -> ConRepr -> TName -> Int -> [(Name,Expr)] -> Expr" }
    , { symbol = "genConFieldsAssign", signature = "Type -> TName -> ConRepr -> TName -> [(Name,Expr)] -> Expr" }
    , { symbol = "getConInfo", signature = "Type -> Name -> Reuse (Maybe ConInfo)" }
    , { symbol = "uniqueTName", signature = "Type -> Reuse TName" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Reuse a -> Reuse a" }
    , { symbol = "getEnv", signature = "Reuse Env" }
    , { symbol = "updateSt", signature = "(ReuseState -> ReuseState) -> Reuse ()" }
    , { symbol = "getSt", signature = "Reuse ReuseState" }
    , { symbol = "runReuse", signature = "Pretty.Env -> Newtypes -> Reuse a -> Unique a" }
    , { symbol = "getCurrentDef", signature = "Reuse [Def]" }
    , { symbol = "withCurrentDef", signature = "Def -> Reuse a -> Reuse a" }
    , { symbol = "getNewtypes", signature = "Reuse Newtypes" }
    , { symbol = "withNewtypes", signature = "(Newtypes -> Newtypes) -> Reuse a -> Reuse a" }
    , { symbol = "getPrettyEnv", signature = "Reuse Pretty.Env" }
    , { symbol = "withPrettyEnv", signature = "(Pretty.Env -> Pretty.Env) -> Reuse a -> Reuse a" }
    , { symbol = "getUniq", signature = "Reuse Int" }
    , { symbol = "modifyUniq", signature = "(Int -> Int) -> Reuse ()" }
    , { symbol = "setUniq", signature = "Int -> Reuse ()" }
    , { symbol = "ruTraceDoc", signature = "(Pretty.Env -> Doc) -> Reuse ()" }
    , { symbol = "ruTrace", signature = "String -> Reuse ()" }
    , { symbol = "csharpFromCore", signature = "BuildType -> Bool -> Maybe (Name,Type) -> Core -> Doc" }
    , { symbol = "includeExternal", signature = "BuildType -> External -> [Doc]" }
    , { symbol = "ppMain", signature = "Name -> Type -> Doc" }
    , { symbol = "genProgram", signature = "Core -> Asm ()" }
    , { symbol = "isStruct", signature = "DataRepr -> Bool" }
    , { symbol = "hasTagField", signature = "DataRepr -> Bool" }
    , { symbol = "genTypeDef", signature = "TypeDef -> Asm ()" }
    , { symbol = "genConstructor", signature = "DataInfo -> DataRepr -> (ConInfo,ConRepr) -> Asm ()" }
    , { symbol = "ppExistsApplyType", signature = "Int -> Doc -> Doc" }
    , { symbol = "ppConField", signature = "ModuleName -> (Name,Type) -> Doc" }
    , { symbol = "ppConConstructor", signature = "ModuleName -> ConInfo -> ConRepr -> [(Name,Type)] -> [Doc]" }
    , { symbol = "ppConConstructorEx", signature = "ModuleName -> ConInfo -> ConRepr -> [(Name,Type)] -> [(Name,Type)] -> [Doc]" }
    , { symbol = "ppFunctionHeaderGen", signature = "ModuleName -> Name -> [TypeVar] -> Type -> Doc" }
    , { symbol = "ppParams", signature = "ModuleName -> [(Name,Type)] -> Doc" }
    , { symbol = "typeConClassName", signature = "Name -> Name" }
    , { symbol = "genDef", signature = "Bool -> Def -> Asm ()" }
    , { symbol = "genDefZ", signature = "Bool -> Def -> Asm ()" }
    , { symbol = "genDefX", signature = "Bool -> Def -> Asm ()" }
    , { symbol = "extractArgs", signature = "Name -> Expr -> ([Type],[Name])" }
    , { symbol = "etaExpand", signature = "Expr -> [Expr] -> Int -> Asm Expr" }
    , { symbol = "tetaExpand", signature = "Expr -> [Type] -> Int -> Asm Expr" }
    , { symbol = "genReturnExpr", signature = "Bool -> Expr -> Asm ()" }
    , { symbol = "genArguments", signature = "[Expr] -> Asm [Doc]" }
    , { symbol = "genInline", signature = "Expr -> Asm Doc" }
    , { symbol = "genExpr", signature = "Expr  -> Asm ()" }
    , { symbol = "genExternal", signature = "TName -> [(Target,String)] -> [Type] -> [Expr] -> Asm ()" }
    , { symbol = "kindCast", signature = "ModuleName -> [Type] -> Type -> (Doc -> Doc)" }
    , { symbol = "genStatic", signature = "TName -> Int -> Int -> [Type] -> Maybe [Expr] -> Asm ()" }
    , { symbol = "genDynamic", signature = "Expr -> [Expr] -> Asm ()" }
    , { symbol = "genTailCall", signature = "Expr -> TName -> [Type] -> [Expr] -> Asm ()" }
    , { symbol = "extractResultType", signature = "Type -> Type" }
    , { symbol = "genCon", signature = "TName -> ConRepr -> [Type] -> [Expr] -> Asm ()" }
    , { symbol = "ppConEnum", signature = "ModuleName -> TName -> Doc" }
    , { symbol = "ppConSingleton", signature = "ModuleName -> Name -> TName -> [Type] -> Doc" }
    , { symbol = "ppExternal", signature = "Name -> TName -> [(Target,String)] -> Doc -> [Doc] -> [Doc] -> Doc" }
    , { symbol = "genExprBasic", signature = "Expr -> Asm ()" }
    , { symbol = "genLetDef", signature = "Bool -> Def -> Asm ()" }
    , { symbol = "isStatement", signature = "Expr -> Bool" }
    , { symbol = "isLambda", signature = "Expr -> Bool" }
    , { symbol = "genAtomic", signature = "Expr -> Asm Doc" }
    , { symbol = "genBranches", signature = "Doc -> [Doc] -> [Branch] -> Asm ()" }
    , { symbol = "genTag", signature = "(Doc,[Pattern]) -> Asm (Maybe Doc)" }
    , { symbol = "genBranch", signature = "[Maybe Doc] -> [Doc] -> Bool -> Branch -> Asm ()" }
    , { symbol = "genGuard", signature = "Expr -> Expr -> Asm ()" }
    , { symbol = "genScoped", signature = "Expr -> Asm ()" }
    , { symbol = "genPattern", signature = "Bool -> [(Maybe Doc,Doc,Pattern)] -> (Doc,[(Name,Type)],[TypeVar]) -> Asm () -> Asm ()" }
    , { symbol = "genExistsApply", signature = "Doc -> Doc -> Doc -> Name -> [TypeVar] -> [TypeVar] -> [(Name,Type)] -> Asm () -> Asm ()" }
    , { symbol = "genExistsApplyMethod", signature = "Doc -> Doc -> Name -> [TypeVar] -> Asm() -> Asm ()" }
    , { symbol = "genPatternTest", signature = "Bool -> (Maybe Doc,Doc,Pattern) -> Asm [([Doc],[Doc],[(Maybe Doc,Doc,Pattern)],[(Doc,Doc,Name,[TypeVar])])]" }
    , { symbol = "genNextPatterns", signature = "Doc -> Type -> [Pattern] -> [(Maybe Doc,Doc,Pattern)]" }
    , { symbol = "tnames", signature = "TNames -> [(Name,Type)]" }
    , { symbol = "genClass", signature = "Name -> [TypeVar] -> [(Name,Type)] -> Doc -> Asm () -> Asm ()" }
    , { symbol = "genTypeApplyMethod", signature = "[TypeVar] -> Expr -> Asm ()" }
    , { symbol = "genApplyMethod", signature = "Bool -> [TName] -> Expr -> Asm ()" }
    , { symbol = "genBody", signature = "Bool -> Bool -> Expr -> Asm ()" }
    , { symbol = "ppLit", signature = "Lit -> Doc" }
    , { symbol = "isSmallInt", signature = "Integer -> Bool" }
    , { symbol = "ppType", signature = "ModuleName -> Type -> Doc" }
    , { symbol = "ppTypeEx", signature = "ModuleName -> Type -> [Type] -> Doc" }
    , { symbol = "ppTypeSub", signature = "ModuleName -> [(TypeVar,Doc)] -> Type -> Doc" }
    , { symbol = "runAsm", signature = "Env -> Asm () -> Doc" }
    , { symbol = "withTopLevel", signature = "([Doc] -> [Doc]) -> Asm ()" }
    , { symbol = "onTopLevel", signature = "Asm () -> Asm ()" }
    , { symbol = "withDoc", signature = "(Doc -> Doc) -> Asm a -> Asm a" }
    , { symbol = "onLocals", signature = "Asm a -> Asm ([Doc],a)" }
    , { symbol = "putLn", signature = "Doc -> Asm ()" }
    , { symbol = "put", signature = "Doc -> Asm ()" }
    , { symbol = "indented", signature = "Asm a -> Asm a" }
    , { symbol = "useCps", signature = "Asm Bool" }
    , { symbol = "getModule", signature = "Asm Name" }
    , { symbol = "getCurrentDef", signature = "Asm Name" }
    , { symbol = "getCurrentArgs", signature = "Asm (Maybe ([Type],[Name]))" }
    , { symbol = "result", signature = "Doc -> Asm ()" }
    , { symbol = "getResultKind", signature = "Asm ResultKind" }
    , { symbol = "withReturn", signature = "Bool -> Asm () -> Asm ()" }
    , { symbol = "withId", signature = "Asm () -> Asm [Doc]" }
    , { symbol = "withIdOne", signature = "Asm () -> Asm Doc" }
    , { symbol = "withAssign", signature = "((Doc -> Doc) -> Doc -> Doc) -> Asm () -> Asm ()" }
    , { symbol = "genName", signature = "Name -> Asm Name" }
    , { symbol = "putLocal", signature = "Doc -> Asm ()" }
    , { symbol = "captureLocals", signature = "Asm a -> Asm (a,[Doc])" }
    , { symbol = "newVarNames", signature = "Int -> Asm [Name]" }
    , { symbol = "newVarName", signature = "String -> Asm Name" }
    , { symbol = "ppDefName", signature = "Name -> Doc" }
    , { symbol = "ppQName", signature = "Name -> Name -> Doc" }
    , { symbol = "ppName", signature = "Name -> Doc" }
    , { symbol = "ppModName", signature = "Name -> Doc" }
    , { symbol = "encode", signature = "Bool -> Name -> Doc" }
    , { symbol = "isReserved", signature = "String -> Bool" }
    , { symbol = "reserved", signature = "[String]" }
    , { symbol = "xcat", signature = "[Doc] -> Doc" }
    , { symbol = "debug", signature = "Bool" }
    , { symbol = "externalNames", signature = "[(TName, Doc)]" }
    , { symbol = "javascriptFromCore", signature = "BuildType -> Maybe (Name,Bool) -> [Import] -> Core -> Doc" }
    , { symbol = "genModule", signature = "BuildType -> Maybe (Name,Bool) -> [Import] -> Core -> Asm Doc" }
    , { symbol = "moduleImport", signature = "Import -> Doc" }
    , { symbol = "includeExternal", signature = "BuildType -> External -> [Doc]" }
    , { symbol = "importExternal", signature = "BuildType -> External -> [(Doc,Doc)]" }
    , { symbol = "genGroups", signature = "Bool -> [DefGroup] -> Asm Doc" }
    , { symbol = "genGroup", signature = "Bool -> DefGroup -> Asm Doc" }
    , { symbol = "genDef", signature = "Bool -> Def -> Asm Doc" }
    , { symbol = "tryFunDef", signature = "Name -> CommentDoc -> Expr -> Asm (Maybe Doc)" }
    , { symbol = "genTypeDefs", signature = "TypeDefGroups -> Asm Doc" }
    , { symbol = "genTypeDefGroup", signature = "TypeDefGroup -> Asm Doc" }
    , { symbol = "genTypeDef", signature = "TypeDef -> Asm Doc" }
    , { symbol = "getResult", signature = "Result -> Doc -> Doc" }
    , { symbol = "tryTailCall", signature = "Result -> Expr -> Asm (Maybe Doc)" }
    , { symbol = "genStat", signature = "Result -> Expr -> Asm Doc" }
    , { symbol = "genMatch", signature = "Result -> [Doc] -> [Branch] -> Asm Doc" }
    , { symbol = "genExpr", signature = "Expr -> Asm (Doc,Doc)" }
    , { symbol = "extractList", signature = "Expr -> Maybe ([Expr],Expr)" }
    , { symbol = "genList", signature = "[Expr] -> Expr -> Asm (Doc,Doc)" }
    , { symbol = "genExternalExpr", signature = "TName -> String -> [Expr] -> Asm (Doc,Doc)" }
    , { symbol = "genExprs", signature = "[Expr] -> Asm ([Doc],[Doc])" }
    , { symbol = "genVarBinding", signature = "Expr -> Asm (Doc, TName)" }
    , { symbol = "genPure", signature = "Expr -> Asm Doc" }
    , { symbol = "isPat", signature = "Bool -> Pattern -> Bool" }
    , { symbol = "genInline", signature = "Expr -> Asm Doc" }
    , { symbol = "extractExtern", signature = "Expr -> Maybe (TName,[(Target,String)])" }
    , { symbol = "genWrapExternal", signature = "TName -> [(Target,String)] -> Asm Doc" }
    , { symbol = "genInlineExternal", signature = "TName -> [(Target,String)] -> [Doc] -> Asm Doc" }
    , { symbol = "genExprExternal", signature = "TName -> [(Target,String)] -> [Doc] -> Asm ([Doc],Doc)" }
    , { symbol = "genExprExternalPrim", signature = "TName -> [(Target,String)] -> [Doc] -> Asm ([Doc],Doc)" }
    , { symbol = "getFormat", signature = "TName -> [(Target,String)] -> String" }
    , { symbol = "genDefName", signature = "TName -> Asm Doc" }
    , { symbol = "genTName", signature = "TName -> Asm Doc" }
    , { symbol = "genName", signature = "Name -> Asm Doc" }
    , { symbol = "genVarName", signature = "String -> Asm Doc" }
    , { symbol = "genVarNames", signature = "Int -> Asm [Doc]" }
    , { symbol = "genCommentTName", signature = "TName -> Asm Doc" }
    , { symbol = "extractExternal", signature = "Expr -> Maybe (TName, String, [Expr])" }
    , { symbol = "isFunExpr", signature = "Expr -> Bool" }
    , { symbol = "isInlineableExpr", signature = "Expr -> Bool" }
    , { symbol = "isPureExpr", signature = "Expr -> Bool" }
    , { symbol = "isTailCalling", signature = "Expr -> Name -> Bool" }
    , { symbol = "runAsm", signature = "Env -> Asm Doc -> Doc" }
    , { symbol = "newVarName", signature = "String -> Asm Name" }
    , { symbol = "newVarNames", signature = "Int -> Asm [Name]" }
    , { symbol = "getModule", signature = "Asm Name" }
    , { symbol = "getPrettyEnv", signature = "Asm Pretty.Env" }
    , { symbol = "withTypeVars", signature = "[TypeVar] -> Asm a -> Asm a" }
    , { symbol = "withNameSubstitutions", signature = "[(TName, Doc)] -> Asm a -> Asm a" }
    , { symbol = "withStatement", signature = "Asm a -> Asm a" }
    , { symbol = "getInStatement", signature = "Asm Bool" }
    , { symbol = "ppLit", signature = "Lit -> Doc" }
    , { symbol = "ppName", signature = "Name -> Doc" }
    , { symbol = "ppQName", signature = "Name -> Name -> Doc" }
    , { symbol = "ppModName", signature = "Name -> Doc" }
    , { symbol = "encode", signature = "Bool -> Name -> Doc" }
    , { symbol = "isReserved", signature = "String -> Bool" }
    , { symbol = "reserved", signature = "S.Set String" }
    , { symbol = "block", signature = "Doc -> Doc" }
    , { symbol = "tcoBlock", signature = "Doc -> Doc" }
    , { symbol = "tailcall", signature = "Doc" }
    , { symbol = "object", signature = "[(Doc, Doc)] -> Doc" }
    , { symbol = "tab", signature = "Doc -> Doc" }
    , { symbol = "comment", signature = "Doc -> Doc" }
    , { symbol = "linecomment", signature = "Doc -> Doc" }
    , { symbol = "debugComment", signature = "String -> Doc" }
    , { symbol = "debugWrap", signature = "String -> Doc -> Doc" }
    , { symbol = "tagField", signature = "Doc" }
    , { symbol = "constdecl", signature = "Doc" }
    , { symbol = "colorThemes", signature = "[(String,ColorScheme)]" }
    , { symbol = "defaultColor", signature = "Color -> Color -> Color" }
    , { symbol = "defaultTo", signature = "ColorScheme -> Color -> ColorScheme" }
    , { symbol = "readColorFlags", signature = "String -> ColorScheme -> ColorScheme" }
    , { symbol = "readColorFlag", signature = "String -> ColorScheme -> ColorScheme" }
    , { symbol = "readUpdate", signature = "String -> Maybe (Color -> ColorScheme -> ColorScheme)" }
    , { symbol = "readColor", signature = "String -> Maybe Color" }
    , { symbol = "norm", signature = "String -> String" }
    , { symbol = "errorsNil", signature = "Errors" }
    , { symbol = "errorsSingle", signature = "ErrorMessage -> Errors" }
    , { symbol = "errorsAdd", signature = "[ErrorMessage] -> Errors -> Errors" }
    , { symbol = "mergeErrors", signature = "Errors -> Errors -> Errors" }
    , { symbol = "ppErrorSeverity", signature = "ColorScheme -> ErrorKind -> ErrorSeverity -> Doc" }
    , { symbol = "ppErrorMessage", signature = "FilePath -> Bool -> ColorScheme -> ErrorMessage -> Doc" }
    , { symbol = "ppErrors", signature = "FilePath -> Bool -> ColorScheme -> Errors -> Doc" }
    , { symbol = "toWarning", signature = "ErrorKind -> (Range,Doc) -> ErrorMessage" }
    , { symbol = "checkError", signature = "Error b a -> Either Errors (a,Warnings)" }
    , { symbol = "checkPartial", signature = "Error b a -> Either (Errors, Maybe b) (a, Warnings, Maybe b)" }
    , { symbol = "setPartial", signature = "Maybe c -> Error b a -> Error c a" }
    , { symbol = "handleError", signature = "Error b a -> (Errors -> Error b a) -> Error b a" }
    , { symbol = "ok", signature = "a -> Error b a" }
    , { symbol = "errorMsgs", signature = "[ErrorMessage] -> Error b a" }
    , { symbol = "errorMsg", signature = "ErrorMessage -> Error b a" }
    , { symbol = "errorMsgsPartial", signature = "[ErrorMessage] -> Maybe b -> Error b a" }
    , { symbol = "errorMsgPartial", signature = "ErrorMessage -> Maybe b -> Error b a" }
    , { symbol = "addErrorMsg", signature = "ErrorMessage -> Error b ()" }
    , { symbol = "warningMsgs", signature = "[ErrorMessage] -> Error b ()" }
    , { symbol = "warningMsg", signature = "ErrorMessage -> Error b ()" }
    , { symbol = "addWarnings", signature = "[ErrorMessage] -> Error b a -> Error b a" }
    , { symbol = "addPartialResult", signature = "Error b a -> Maybe b -> Error b a" }
    , { symbol = "overridePartialResult", signature = "Error b a -> Maybe b -> Error b a" }
    , { symbol = "ignoreWarnings", signature = "Error b a -> Error b a" }
    , { symbol = "assertion", signature = "HasCallStack => String -> Bool -> a -> a" }
    , { symbol = "failure", signature = "HasCallStack => String -> a" }
    , { symbol = "todo", signature = "HasCallStack => String -> a" }
    , { symbol = "matchFailure", signature = "HasCallStack => String -> a" }
    , { symbol = "raise", signature = "HasCallStack => String -> a" }
    , { symbol = "raiseIO", signature = "HasCallStack => String -> IO a" }
    , { symbol = "catchIO", signature = "HasCallStack => IO a -> (String -> IO a) -> IO a" }
    , { symbol = "seqList", signature = "[a] -> b -> b" }
    , { symbol = "seqMaybe", signature = "Maybe a -> b -> b" }
    , { symbol = "seqEither", signature = "Either a b -> c -> c" }
    , { symbol = "seqTuple2", signature = "(a,b) -> c -> c" }
    , { symbol = "seqString", signature = "String -> a -> a" }
    , { symbol = "seqqList", signature = "[a] -> [a]" }
    , { symbol = "seqqMaybe", signature = "Maybe a -> Maybe a" }
    , { symbol = "seqqEither", signature = "Either a b -> Either a b" }
    , { symbol = "seqqTuple2", signature = "(a,b) -> (a,b)" }
    , { symbol = "seqqString", signature = "String -> String" }
    , { symbol = "isLiteralDoc", signature = "FileName -> Bool" }
    , { symbol = "basename", signature = "FileName -> FileName" }
    , { symbol = "extname", signature = "FileName -> FileName" }
    , { symbol = "ensureExt", signature = "FileName -> String -> FileName" }
    , { symbol = "dirname", signature = "FileName -> FileName" }
    , { symbol = "notdir", signature = "FileName -> FileName" }
    , { symbol = "notext", signature = "FileName -> FileName" }
    , { symbol = "noexts", signature = "FileName -> FileName" }
    , { symbol = "undelimPaths", signature = "String -> [FilePath]" }
    , { symbol = "splitPath", signature = "FilePath -> [FilePath]" }
    , { symbol = "onWindows", signature = "Bool" }
    , { symbol = "joinPath", signature = "FilePath -> FilePath -> FilePath" }
    , { symbol = "joinPaths", signature = "[FilePath] -> FilePath" }
    , { symbol = "normalize", signature = "FilePath -> FilePath" }
    , { symbol = "normalizeWith", signature = "Char -> FilePath -> FilePath" }
    , { symbol = "isPathSep", signature = "Char -> Bool" }
    , { symbol = "isPathDelimiter", signature = "Char -> Bool" }
    , { symbol = "getCwd", signature = "IO FilePath" }
    , { symbol = "runSystemRaw", signature = "String -> IO ()" }
    , { symbol = "runSystem", signature = "String -> IO ()" }
    , { symbol = "runCmd", signature = "String -> [String] -> IO ()" }
    , { symbol = "runCmdRead", signature = "[(String,String)] -> String -> [String] -> IO (String,String)" }
    , { symbol = "runCmdEnv", signature = "[(String,String)] -> String -> [String] -> IO ()" }
    , { symbol = "buildEnv", signature = "[(String,String)] -> IO (Maybe [(String,String)])" }
    , { symbol = "fileTimeCompare", signature = "FilePath -> FilePath -> IO Ordering" }
    , { symbol = "maxFileTime", signature = "FileTime -> FileTime -> FileTime" }
    , { symbol = "maxFileTimes", signature = "[FileTime] -> FileTime" }
    , { symbol = "doesFileExistAndNotEmpty", signature = "FilePath -> IO Bool" }
    , { symbol = "readTextFile", signature = "FilePath -> IO (Maybe String)" }
    , { symbol = "writeTextFile", signature = "FilePath -> String -> IO ()" }
    , { symbol = "copyTextFile", signature = "FilePath -> FilePath -> IO ()" }
    , { symbol = "copyTextFileWith", signature = "FilePath -> FilePath -> (String -> String) -> IO ()" }
    , { symbol = "copyBinaryFile", signature = "FilePath -> FilePath -> IO ()" }
    , { symbol = "copyBinaryIfNewer", signature = "Bool -> FilePath -> FilePath -> IO ()" }
    , { symbol = "copyTextIfNewer", signature = "Bool -> FilePath -> FilePath -> IO ()" }
    , { symbol = "copyTextIfNewerWith", signature = "Bool -> FilePath -> FilePath -> (String -> String) -> IO ()" }
    , { symbol = "removeFileIfExists", signature = "FilePath -> IO ()" }
    , { symbol = "getProgramPath", signature = "IO FilePath" }
    , { symbol = "commonPathPrefix", signature = "FilePath -> FilePath -> FilePath" }
    , { symbol = "relativeToPath", signature = "FilePath -> FilePath -> FilePath" }
    , { symbol = "mbRelativeToPath", signature = "FilePath -> FilePath -> Maybe FilePath" }
    , { symbol = "isAbsolute", signature = "FilePath -> Bool" }
    , { symbol = "findMaximalPrefixPath", signature = "[FilePath] -> FilePath -> Maybe (FilePath,FilePath)" }
    , { symbol = "getMaximalPrefixPath", signature = "[FilePath] -> FilePath -> (FilePath,FilePath)" }
    , { symbol = "findMaximalPrefix", signature = "[String] -> String -> Maybe (Int,String)" }
    , { symbol = "findMaximal", signature = "(a -> Maybe Int) -> [a] -> Maybe (Int,a)" }
    , { symbol = "searchPaths", signature = "[FilePath] -> [String] -> String -> IO (Maybe (FilePath))" }
    , { symbol = "searchPathsSuffixes", signature = "[FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath))" }
    , { symbol = "searchPathsCanonical", signature = "FilePath -> [FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath,FilePath))" }
    , { symbol = "searchPathsEx", signature = "[FilePath] -> [String] -> [String] -> String -> IO (Maybe (FilePath,FilePath))" }
    , { symbol = "makeRelativeToPaths", signature = "[FilePath] -> FilePath -> (FilePath,FilePath)" }
    , { symbol = "getEnvPaths", signature = "String -> IO [FilePath]" }
    , { symbol = "getEnvVar", signature = "String -> IO String" }
    , { symbol = "realPath", signature = "FilePath -> IO FilePath" }
    , { symbol = "searchProgram", signature = "FilePath -> IO (Maybe FilePath)" }
    , { symbol = "splitPath", signature = "String -> [String]" }
    , { symbol = "showId", signature = "Id -> String" }
    , { symbol = "genId", signature = "String -> Int -> Id" }
    , { symbol = "newId", signature = "Int -> Id" }
    , { symbol = "newIdFromId", signature = "Id -> Id" }
    , { symbol = "idNil", signature = "Id" }
    , { symbol = "idNumber", signature = "Id -> Int" }
    , { symbol = "niceEmpty", signature = "Nice" }
    , { symbol = "niceExtend", signature = "Ids -> [String] -> Nice -> Nice" }
    , { symbol = "niceShow", signature = "Nice -> Id -> String" }
    , { symbol = "nicePretty", signature = "Nice -> Id -> Doc" }
    , { symbol = "ppRange", signature = "FilePath -> Bool -> ColorScheme -> Range -> Doc" }
    , { symbol = "table", signature = "[(Doc,Doc)] -> Doc" }
    , { symbol = "sourceFromRanges", signature = "[Range] -> [String]" }
    , { symbol = "docsFromRanges", signature = "ColorScheme -> [Range] -> [Doc]" }
    , { symbol = "docFromRange", signature = "ColorScheme -> Range -> Doc" }
    , { symbol = "nameLocal", signature = "Name -> String" }
    , { symbol = "nameCaseOverlap", signature = "Name -> Name -> Bool" }
    , { symbol = "nameCaseOverlapPrefixOf", signature = "Name -> Name -> Bool" }
    , { symbol = "stemIsEqual", signature = "Name -> Name -> Bool" }
    , { symbol = "isIdChar", signature = "Char -> Bool" }
    , { symbol = "isIdStartChar", signature = "Char -> Bool" }
    , { symbol = "isIdEndChar", signature = "Char -> Bool" }
    , { symbol = "isSymbolId", signature = "String -> Bool" }
    , { symbol = "wrapId", signature = "String -> String" }
    , { symbol = "showName", signature = "Bool -> Name -> String" }
    , { symbol = "prettyNameEx", signature = "String -> ColorScheme -> Name -> Doc  -- explicit /# if needed" }
    , { symbol = "prettyName", signature = "ColorScheme -> Name -> Doc      -- not explicit /#" }
    , { symbol = "prettyCoreName", signature = "ColorScheme -> Name -> Doc  -- explicit /# if needed" }
    , { symbol = "newName", signature = "String -> Name" }
    , { symbol = "newModuleName", signature = "String -> Name" }
    , { symbol = "newQualified", signature = "String -> String -> Name" }
    , { symbol = "newLocallyQualified", signature = "String -> String -> String -> Name" }
    , { symbol = "hash", signature = "String -> Int" }
    , { symbol = "nameMapStem", signature = "Name -> (String -> String) -> Name" }
    , { symbol = "readQualifiedName", signature = "String -> Name" }
    , { symbol = "splitLocalName", signature = "String -> (String,String)" }
    , { symbol = "isModuleName", signature = "Name -> Bool" }
    , { symbol = "isQualified", signature = "Name -> Bool" }
    , { symbol = "isLocallyQualified", signature = "Name -> Bool" }
    , { symbol = "isSymbolName", signature = "Name -> Bool" }
    , { symbol = "isConstructorName", signature = "Name -> Bool" }
    , { symbol = "isWildcard", signature = "Name -> Bool" }
    , { symbol = "unWildcard", signature = "String -> Name -> Name" }
    , { symbol = "nameIsEtaHole", signature = "Name -> Bool" }
    , { symbol = "isHiddenName", signature = "Name -> Bool" }
    , { symbol = "missingQualifier", signature = "Name -> Name -> Name -> String" }
    , { symbol = "nameSplit", signature = "Name -> (String,String,String)" }
    , { symbol = "missingQualifier", signature = "Name -> Name -> String" }
    , { symbol = "nameNil", signature = "Name" }
    , { symbol = "nameIsNil", signature = "Name -> Bool" }
    , { symbol = "qualify", signature = "HasCallStack => Name -> Name -> Name" }
    , { symbol = "unqualify", signature = "Name -> Name" }
    , { symbol = "qualifier", signature = "Name -> Name" }
    , { symbol = "nameAsModuleName", signature = "Name -> Name" }
    , { symbol = "qualifyLocally", signature = "Name -> Name -> Name" }
    , { symbol = "requalifyLocally", signature = "Name -> Name" }
    , { symbol = "unqualifyFull", signature = "Name -> Name" }
    , { symbol = "fullQualifier", signature = "Name -> String" }
    , { symbol = "unqualifyLocally", signature = "Name -> Name" }
    , { symbol = "unqualifyAsModuleName", signature = "Name -> Name" }
    , { symbol = "isInSpecialNameSpace", signature = "Name -> String -> Bool" }
    , { symbol = "splitModuleName", signature = "Name -> [String]" }
    , { symbol = "unsplitModuleName", signature = "[String] -> Name" }
    , { symbol = "mergeCommonPath", signature = "Name -> Name -> Name" }
    , { symbol = "splitLocalQualName", signature = "Name -> [String]" }
    , { symbol = "toConstructorName", signature = "Name -> Name" }
    , { symbol = "toVarName", signature = "Name -> Name" }
    , { symbol = "toHandlerConName", signature = "Name -> Name" }
    , { symbol = "isHandlerConName", signature = "Name -> Bool" }
    , { symbol = "nameStartsWith", signature = "Name -> String -> Bool" }
    , { symbol = "prepend", signature = "String -> Name -> Name" }
    , { symbol = "postpend", signature = "String -> Name -> Name" }
    , { symbol = "typeQualifiedName", signature = "Name -> String -> Name" }
    , { symbol = "typeQualifiedNameOf", signature = "Name -> Name -> Name" }
    , { symbol = "typeQualifiedGetTypeName", signature = "HasCallStack => Name -> Name" }
    , { symbol = "makeHidden", signature = "Name -> Name" }
    , { symbol = "makeHiddenName", signature = "String -> Name -> Name" }
    , { symbol = "unmakeHidden", signature = "HasCallStack => String -> Name -> Name" }
    , { symbol = "newHiddenNameEx", signature = "String -> String -> Name" }
    , { symbol = "newHiddenName", signature = "String -> Name" }
    , { symbol = "toUniqueName", signature = "Int -> Name -> Name" }
    , { symbol = "toHiddenUniqueName", signature = "Int -> String -> Name -> Name" }
    , { symbol = "hiddenNameStartsWith", signature = "Name -> String -> Bool" }
    , { symbol = "newCreatorName", signature = "Name ->  Name" }
    , { symbol = "isCreatorName", signature = "Name -> Bool" }
    , { symbol = "toHandlerName", signature = "Name -> Name" }
    , { symbol = "isHandlerName", signature = "Name -> Bool" }
    , { symbol = "fromHandlerName", signature = "HasCallStack => Name -> Name" }
    , { symbol = "toHandleName", signature = "Name -> Name" }
    , { symbol = "isHandleName", signature = "Name -> Bool" }
    , { symbol = "toOperationsName", signature = "Name -> Name" }
    , { symbol = "isOperationsName", signature = "Name -> Bool" }
    , { symbol = "fromOperationsName", signature = "Name -> Name" }
    , { symbol = "toOpSelectorName", signature = "Name -> Name" }
    , { symbol = "isOpSelectorName", signature = "Name -> Bool" }
    , { symbol = "fromOpSelectorName", signature = "Name -> Name" }
    , { symbol = "toEffectTagName", signature = "Name -> Name" }
    , { symbol = "toOpTypeName", signature = "Name -> Name" }
    , { symbol = "toOpConName", signature = "Name -> Name" }
    , { symbol = "toOpsConName", signature = "Name -> Name" }
    , { symbol = "toLazyIndirectConName", signature = "Name -> Name" }
    , { symbol = "isLazyIndirectConName", signature = "Name -> Bool" }
    , { symbol = "toOpenTagName", signature = "Name -> Name" }
    , { symbol = "isOpenTagName", signature = "Name -> Bool" }
    , { symbol = "toValueOperationName", signature = "Name -> Name" }
    , { symbol = "isValueOperationName", signature = "Name -> Bool" }
    , { symbol = "fromValueOperationsName", signature = "Name -> Name" }
    , { symbol = "toBasicOperationsName", signature = "Name -> Name" }
    , { symbol = "implicitNameSpace", signature = "String" }
    , { symbol = "isImplicitParamName", signature = "Name -> Bool" }
    , { symbol = "toImplicitParamName", signature = "Name -> Name" }
    , { symbol = "fromImplicitParamName", signature = "Name -> Name" }
    , { symbol = "splitImplicitParamName", signature = "Name -> (Name,Name)" }
    , { symbol = "plainImplicitParamName", signature = "Name -> Name" }
    , { symbol = "namedImplicitParamName", signature = "Name -> Name -> Name" }
    , { symbol = "canonicalName", signature = "Int -> Name -> Name" }
    , { symbol = "nonCanonicalName", signature = "Name -> Name" }
    , { symbol = "canonicalSplit", signature = "Name -> (Name,String)" }
    , { symbol = "moduleNameToPath", signature = "Name -> FilePath" }
    , { symbol = "pathToModuleName", signature = "FilePath -> Name" }
    , { symbol = "asciiEncode", signature = "Bool -> String -> String" }
    , { symbol = "showHex", signature = "Int -> Int -> String" }
    , { symbol = "showBinary", signature = "Int -> Int -> String" }
    , { symbol = "showHexFloat", signature = "Double -> String" }
    , { symbol = "find", signature = "HasCallStack => Name -> NameMap a -> a" }
    , { symbol = "copyNameOf", signature = "Name -> Name" }
    , { symbol = "nameLazyEnter", signature = "Name" }
    , { symbol = "isClauseTailName", signature = "Name -> Maybe Int" }
    , { symbol = "makeTpHandled", signature = "Bool -> Bool -> Name" }
    , { symbol = "nameTuple", signature = "Int -> Name" }
    , { symbol = "nameTpTuple", signature = "Int -> Name" }
    , { symbol = "isNameTuple", signature = "Name -> Bool" }
    , { symbol = "isNameTpTuple", signature = "Name -> Bool" }
    , { symbol = "empty", signature = "QNameMap a" }
    , { symbol = "isEmpty", signature = "QNameMap a -> Bool" }
    , { symbol = "single", signature = "Name -> a -> QNameMap a" }
    , { symbol = "fromList", signature = "[(Name,a)] -> QNameMap a" }
    , { symbol = "lookupQ", signature = "Name -> QNameMap a -> Maybe a" }
    , { symbol = "lookup", signature = "Name -> Name -> QNameMap a -> Lookup a" }
    , { symbol = "filterNames", signature = "(Name -> Bool) -> QNameMap a -> QNameMap a" }
    , { symbol = "insert", signature = "HasCallStack => Name -> a -> QNameMap a -> QNameMap a" }
    , { symbol = "union", signature = "HasCallStack => QNameMap a -> QNameMap a -> QNameMap a" }
    , { symbol = "unionLeftBias", signature = "HasCallStack => QNameMap a -> QNameMap a -> QNameMap a" }
    , { symbol = "unions", signature = "HasCallStack => [QNameMap a] -> QNameMap a" }
    , { symbol = "toAscList", signature = "QNameMap a -> [(Name,a)]" }
    , { symbol = "safeCombine", signature = "HasCallStack => String -> [(Name,a)] -> [(Name,a)] -> [(Name,a)]" }
    , { symbol = "readInput", signature = "HasCallStack => FilePath -> IO BString" }
    , { symbol = "utfDecode", signature = "B.ByteString -> T.Text" }
    , { symbol = "extractLiterate", signature = "BString -> BString" }
    , { symbol = "startsWith", signature = "BC.ByteString -> String -> Bool" }
    , { symbol = "bigLine", signature = "Int" }
    , { symbol = "makePos", signature = "Source -> Int -> Int -> Int -> Pos" }
    , { symbol = "posMoves8", signature = "Pos -> BString -> Pos" }
    , { symbol = "posMove8", signature = "Pos -> Char -> Pos" }
    , { symbol = "tabSize", signature = "Int" }
    , { symbol = "showCompactRange", signature = "Range -> String" }
    , { symbol = "showRange", signature = "FilePath -> Bool -> Range -> String" }
    , { symbol = "after", signature = "Range -> Range -> Bool" }
    , { symbol = "rangeNull", signature = "Range" }
    , { symbol = "rangeIsNull", signature = "Range -> Bool" }
    , { symbol = "showFullRange", signature = "FilePath -> Range -> String" }
    , { symbol = "makeRange", signature = "Pos -> Pos -> Range" }
    , { symbol = "makeSourceRange", signature = "FilePath -> Int -> Int -> Int -> Int -> Range" }
    , { symbol = "rangeLength", signature = "Range -> Int" }
    , { symbol = "rangeSource", signature = "Range -> Source" }
    , { symbol = "combineRange", signature = "Range -> Range -> Range" }
    , { symbol = "combineRanges", signature = "[Range] -> Range" }
    , { symbol = "rangeHide", signature = "Range -> Range" }
    , { symbol = "minPos", signature = "Pos -> Pos -> Pos" }
    , { symbol = "maxPos", signature = "Pos -> Pos -> Pos" }
    , { symbol = "extendRange", signature = "Range -> Int -> Range" }
    , { symbol = "endOfRange", signature = "Range -> Range" }
    , { symbol = "startOfRange", signature = "Range -> Range" }
    , { symbol = "combineRangeds", signature = "Ranged a => [a] -> Range" }
    , { symbol = "combineRanged", signature = "(Ranged a, Ranged b) => a -> b -> Range" }
    , { symbol = "sourceFromRange", signature = "Range -> String" }
    , { symbol = "rawSourceFromRange", signature = "Range -> String" }
    , { symbol = "isTargetWasm", signature = "Target -> Bool" }
    , { symbol = "alignedSum", signature = "Int -> [Int] -> Int" }
    , { symbol = "alignedAdd", signature = "Int -> Int -> Int" }
    , { symbol = "alignUp", signature = "Int -> Int -> Int" }
    , { symbol = "opSortString", signature = "OperationSort -> String" }
    , { symbol = "readOperationSort", signature = "String -> Maybe OperationSort" }
    , { symbol = "dataDefIsExtend", signature = "DataDef -> Bool" }
    , { symbol = "dataDefSize", signature = "Platform -> DataDef -> Int" }
    , { symbol = "valueReprSizeScan", signature = "Platform -> ValueRepr -> (Int,Int)" }
    , { symbol = "valueReprSize", signature = "Platform -> ValueRepr -> Int" }
    , { symbol = "valueReprIsMixed", signature = "ValueRepr -> Bool" }
    , { symbol = "valueReprIsRaw", signature = "ValueRepr -> Bool" }
    , { symbol = "valueReprNew", signature = "Int -> Int -> Int -> ValueRepr" }
    , { symbol = "valueReprZero", signature = "ValueRepr" }
    , { symbol = "valueReprRaw", signature = "Int -> ValueRepr" }
    , { symbol = "valueReprScan", signature = "Int -> ValueRepr" }
    , { symbol = "defFunEx", signature = "[ParamInfo] -> Fip -> DefSort" }
    , { symbol = "defFun", signature = "[ParamInfo] -> DefSort" }
    , { symbol = "defSortShowFull", signature = "DefSort -> String" }
    , { symbol = "fipSubsumes", signature = "Fip -> Fip -> Bool" }
    , { symbol = "fipIsTail", signature = "Fip -> Bool" }
    , { symbol = "fipAlloc", signature = "Fip -> FipAlloc" }
    , { symbol = "sepBySpace", signature = "[String] -> String" }
    , { symbol = "fipMax", signature = "Fip -> Fip -> Fip" }
    , { symbol = "fipAllocMax", signature = "FipAlloc -> FipAlloc -> FipAlloc" }
    , { symbol = "memberDoc", signature = "String -> String -> [String] -> String" }
    , { symbol = "runUniqueWith", signature = "[Id] -> Unique a -> a" }
    , { symbol = "runUnique", signature = "Int -> Unique a -> (a,Int)" }
    , { symbol = "withUnique", signature = "HasUnique m => (Int -> (a,Int)) -> m a" }
    , { symbol = "liftUnique", signature = "HasUnique m => Unique a -> m a" }
    , { symbol = "runUniqueT", signature = "Int -> UniqueT m a -> m (a, Int)" }
    , { symbol = "modulesFullBuild", signature = "Bool -> [ModuleName] -> [Name] -> [Module] -> [Module] -> Build [Module]" }
    , { symbol = "modulesBuild", signature = "[Name] -> [Module] -> Build [Module]" }
    , { symbol = "modulesTypeCheck", signature = "[Module] -> Build [Module]" }
    , { symbol = "modulesReValidate", signature = "Bool -> [ModuleName] -> [Module] -> [Module] -> Build [Module]" }
    , { symbol = "workNeeded", signature = "ModulePhase -> [Module] -> Int" }
    , { symbol = "phaseTotalWork", signature = "ModulePhase -> Int" }
    , { symbol = "phaseWorkUnit", signature = "ModulePhase -> Int" }
    , { symbol = "modmapPut", signature = "ModuleMap -> Module -> Build ()" }
    , { symbol = "modmapTryPut", signature = "ModuleMap -> Module -> Build Bool" }
    , { symbol = "modmapRead", signature = "HasCallStack => ModuleMap -> ModuleName -> Build Module" }
    , { symbol = "modmapCreate", signature = "[Module] -> Build ModuleMap" }
    , { symbol = "moduleGuard", signature = "ModulePhase -> ModulePhase -> ModuleMap -> (a -> Module) -> (Module -> b)" }
    , { symbol = "modmapClear", signature = "ModuleMap -> Build ()" }
    , { symbol = "moduleCompile", signature = "HasCallStack => [Name] -> ModuleMap -> ModuleMap -> ModuleMap -> ModuleMap -> ModuleMap -> [ModuleName] -> Module -> Build Module" }
    , { symbol = "orderByBuildOrder", signature = "HasCallStack => [ModuleName] -> [Module] -> [Module]" }
    , { symbol = "moduleCodeGen", signature = "HasCallStack => [Name] -> ModuleMap -> ModuleMap -> ModuleMap -> ModuleMap -> Module -> Build (Bool, Link, Module)" }
    , { symbol = "getMainEntry", signature = "Gamma -> [Name] -> Module -> Build (Maybe (Name,Type))" }
    , { symbol = "moduleWaitForImports", signature = "HasCallStack => Bool -> ModuleMap -> [ModuleName] -> [ModuleName] -> Build [Module]" }
    , { symbol = "moduleOptimize", signature = "ModuleMap -> ModuleMap -> ModuleMap -> Module -> Build Module" }
    , { symbol = "moduleWaitForInlineImports", signature = "HasCallStack => ModuleMap -> [ModuleName] -> Build [Module]" }
    , { symbol = "moduleTypeCheck", signature = "ModuleMap -> ModuleMap -> Module -> Build Module" }
    , { symbol = "moduleWaitForPubImports", signature = "ModuleMap -> [ModuleName] -> [(Bool,ModuleName)] -> Build [Module]" }
    , { symbol = "coreImportsFromModules", signature = "[LexImport] -> [Module] -> [Core.Import]" }
    , { symbol = "moduleParse", signature = "ModuleMap -> Module -> Build Module" }
    , { symbol = "modulesResolveDependencies", signature = "Bool -> [ModuleName] -> [Module] -> [Module] -> Build [Module]" }
    , { symbol = "modulesResolveDeps", signature = "Bool -> [ModuleName] -> [Module] -> [Module] -> [Module] -> Build [Module]" }
    , { symbol = "toBuildOrder", signature = "[Module] -> Build [Module]" }
    , { symbol = "validateDependencies", signature = "[Module] -> Build [Module]" }
    , { symbol = "modulesFlushErrors", signature = "[Module] -> Build [Module]" }
    , { symbol = "moduleFlushErrors", signature = "Module -> Build Module" }
    , { symbol = "moduleLoad", signature = "Bool -> [ModuleName] -> Module -> Build Module" }
    , { symbol = "moduleLex", signature = "HasCallStack => Module -> Build Module" }
    , { symbol = "moduleLoadIface", signature = "Module -> Build Module" }
    , { symbol = "moduleLoadLibIface", signature = "Module -> Build Module" }
    , { symbol = "modFromIface", signature = "Core.Core -> Maybe (Gamma -> Error () [Core.InlineDef]) -> Module -> Module" }
    , { symbol = "copyLibIfaceToOutput", signature = "Flags -> FilePath -> FilePath -> Core.Core -> IO ()" }
    , { symbol = "clibsFromCore", signature = "Flags -> Core -> [String]" }
    , { symbol = "csyslibsFromCore", signature = "Flags -> Core -> [String]" }
    , { symbol = "externalImportKeyFromCore", signature = "Target -> BuildType -> Core.Core -> String -> [String]" }
    , { symbol = "externalImportsFromCore", signature = "Target -> Core.Core -> [[(String,String)]]" }
    , { symbol = "moduleFromSource", signature = "FilePath -> Build Module" }
    , { symbol = "moduleFromModuleName", signature = "FilePath -> Name -> Build Module" }
    , { symbol = "searchSourceFile", signature = "FilePath -> FilePath -> Build (Maybe (FilePath,FilePath))" }
    , { symbol = "searchLibIfaceFile", signature = "FilePath -> Build FilePath  -- can be empty" }
    , { symbol = "modulesValidate", signature = "[Module] -> Build [Module]" }
    , { symbol = "moduleValidate", signature = "Module -> Build Module" }
    , { symbol = "moduleReset", signature = "Module -> Module" }
    , { symbol = "coreReset", signature = "Core -> Core" }
    , { symbol = "throwModuleNotFound", signature = "Range -> Name -> Build a" }
    , { symbol = "throwFileNotFound", signature = "FilePath -> Build a" }
    , { symbol = "nameToPath", signature = "Name -> FilePath" }
    , { symbol = "outputName", signature = "FilePath -> Build FilePath" }
    , { symbol = "ifaceExtension", signature = "FilePath" }
    , { symbol = "noVFS", signature = "VFS" }
    , { symbol = "composeVFS", signature = "VFS -> VFS -> VFS" }
    , { symbol = "withTotalWork", signature = "Int -> Build a -> Build a" }
    , { symbol = "phaseCompleted", signature = "ModulePhase -> Build ()" }
    , { symbol = "getWorkProgress", signature = "Build Double" }
    , { symbol = "runBuildMaybe", signature = "Terminal -> Flags -> Build (Maybe a) -> IO (Maybe a)" }
    , { symbol = "runBuildIO", signature = "Terminal -> Flags -> Bool -> Build a -> IO (Maybe a,Maybe Range)" }
    , { symbol = "runBuild", signature = "Terminal -> Flags -> Build a -> IO (Either Errors (a,Errors))" }
    , { symbol = "forkTerminal", signature = "Terminal -> MVar () -> IO (Terminal, IO ())" }
    , { symbol = "runBuildEnv", signature = "Env -> Build a -> IO (Either Errors (a,Errors))" }
    , { symbol = "checked", signature = "Build a -> Build (Either Errors (a,Errors))" }
    , { symbol = "checkedDefault", signature = "a -> Build a -> Build (a,Errors)" }
    , { symbol = "catchError", signature = "IO a -> (ErrorCall -> IO a) -> IO a" }
    , { symbol = "catchIO", signature = "IO a -> (IOException -> IO a) -> IO a" }
    , { symbol = "catchAny", signature = "IO a -> (SomeException -> IO a) -> IO a" }
    , { symbol = "liftIO", signature = "IO a -> Build a" }
    , { symbol = "liftIOError", signature = "IO (Error () a) -> Build a" }
    , { symbol = "liftError", signature = "Error () a -> Build a" }
    , { symbol = "mapConcurrent", signature = "(a -> Build b) -> [a] -> Build [b]" }
    , { symbol = "pooledIO", signature = "IO a -> Build a" }
    , { symbol = "sequentialIO", signature = "Build (IO a -> IO a)" }
    , { symbol = "withSem", signature = "QSem -> IO a -> IO a" }
    , { symbol = "mapConcurrentModules", signature = "(Module -> Build Module) -> [Module] -> Build [Module]" }
    , { symbol = "onBuildException", signature = "Build b -> Build a -> Build a" }
    , { symbol = "buildFinally", signature = "Build b -> Build a -> Build a" }
    , { symbol = "throwError", signature = "(TP.Env -> ErrorMessage) -> Build a" }
    , { symbol = "throwErrorKind", signature = "ErrorKind -> (TP.Env -> Doc) -> Build a" }
    , { symbol = "getEnv", signature = "Build Env" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Build a -> Build a" }
    , { symbol = "withVFS", signature = "VFS -> Build a -> Build a" }
    , { symbol = "lookupVFS", signature = "FilePath -> Build (Maybe (BString,FileTime))" }
    , { symbol = "getFlags", signature = "Build Flags" }
    , { symbol = "getTerminal", signature = "Build Terminal" }
    , { symbol = "getCurrentRange", signature = "Build Range" }
    , { symbol = "getColorScheme", signature = "Build ColorScheme" }
    , { symbol = "getPrettyEnv", signature = "Build TP.Env" }
    , { symbol = "hasBuildError", signature = "Build (Maybe Range)" }
    , { symbol = "throwOnError", signature = "Build ()" }
    , { symbol = "throwNil", signature = "Build ()" }
    , { symbol = "addErrors", signature = "Errors -> Build ()" }
    , { symbol = "addWarningMessage", signature = "ErrorMessage -> Build ()" }
    , { symbol = "addErrorMessage", signature = "ErrorMessage -> Build ()" }
    , { symbol = "addErrorMessages", signature = "[ErrorMessage] -> Build ()" }
    , { symbol = "addErrorMessageKind", signature = "ErrorKind -> (TP.Env -> Doc) -> Build ()" }
    , { symbol = "phaseTimed", signature = "Int -> String -> (TP.Env -> Doc) -> Build a -> Build a" }
    , { symbol = "phase", signature = "String -> (TP.Env -> Doc) -> Build ()" }
    , { symbol = "phaseVerbose", signature = "Int -> String -> (TP.Env -> Doc) -> Build ()" }
    , { symbol = "phaseShow", signature = "Int -> String -> (TP.Env -> Doc) -> Build ()" }
    , { symbol = "buildDoesFileExist", signature = "FilePath -> Build Bool" }
    , { symbol = "buildDoesFileExistAndNotEmpty", signature = "FilePath -> Build Bool" }
    , { symbol = "getFileTime", signature = "FilePath -> Build FileTime" }
    , { symbol = "maybeGetFileTime", signature = "FilePath -> Build (Maybe FileTime)" }
    , { symbol = "getFileContents", signature = "HasCallStack => FilePath -> Build BString" }
    , { symbol = "withModuleName", signature = "ModuleName -> Build a -> Build a" }
    , { symbol = "withCheckedModule", signature = "Module -> Build Module -> Build Module" }
    , { symbol = "noLink", signature = "Link" }
    , { symbol = "codeGen", signature = "Terminal -> Flags -> (IO () -> IO ()) -> Newtypes -> Borrowed -> KGamma -> Gamma ->" }
    , { symbol = "codeGenCS", signature = "Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link" }
    , { symbol = "codeGenCSDll", signature = "Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link" }
    , { symbol = "codeGenJS", signature = "Terminal -> Flags -> (IO () -> IO ()) -> Maybe (Name,Type) -> FilePath -> Core.Core -> IO Link" }
    , { symbol = "codeGenC", signature = "FilePath -> Newtypes -> Borrowed -> Int" }
    , { symbol = "codeGenLinkC", signature = "Terminal -> Flags -> (IO () -> IO ()) -> CC -> ModuleName -> [Module]" }
    , { symbol = "codeGenLinkExe", signature = "Terminal -> Flags -> Int -> [String] -> [Char] -> IO LinkResult" }
    , { symbol = "ccompile", signature = "Terminal -> Flags -> CC -> FilePath -> [FilePath] -> [FilePath] -> IO ()" }
    , { symbol = "copyCLibrary", signature = "Terminal -> Flags -> (IO () -> IO ()) -> CC -> FilePath -> [(String,String)] -> IO [FilePath] {-include paths-}" }
    , { symbol = "searchCLibrary", signature = "Flags -> CC -> FilePath -> [FilePath] -> IO (Either [Doc] (FilePath {-libPath-},[FilePath] {-include paths-}))" }
    , { symbol = "conanCLibrary", signature = "Terminal -> Flags -> (IO () -> IO ()) -> CC -> [(String,String)] -> FilePath -> String -> IO (Either [Doc] (FilePath,[FilePath]))" }
    , { symbol = "vcpkgCLibrary", signature = "Terminal -> Flags -> (IO () -> IO ()) -> CC -> [(String,String)] -> FilePath -> String -> IO (Either [Doc] (FilePath,[FilePath]))" }
    , { symbol = "externalImportKeyFromCore", signature = "Target -> BuildType -> Core.Core -> String -> [String]" }
    , { symbol = "externalImportsFromCore", signature = "Target -> Core.Core -> [[(String,String)]]" }
    , { symbol = "kklibBuild", signature = "Terminal -> Flags -> (IO () -> IO ()) -> CC -> FilePath -> String -> FilePath -> IO FilePath" }
    , { symbol = "runSystemEcho", signature = "Terminal -> Flags -> String -> IO ()" }
    , { symbol = "runCommand", signature = "Terminal -> Flags -> [String] -> IO ()" }
    , { symbol = "runCommandRead", signature = "Terminal -> Flags -> [(String,String)] -> [String] -> IO String" }
    , { symbol = "runCommandReadAll", signature = "Terminal -> Flags -> [(String,String)] -> [String] -> IO (String,String)" }
    , { symbol = "runCommandEnv", signature = "Terminal -> Flags -> [(String,String)] -> [String] -> IO ()" }
    , { symbol = "shellQuoted", signature = "[String] -> String" }
    , { symbol = "moduleNull", signature = "Name -> Module" }
    , { symbol = "moduleCreateInitial", signature = "Name -> FilePath -> FilePath -> FilePath -> Module" }
    , { symbol = "mergeModules", signature = "[Module] -> [Module] -> [Module]" }
    , { symbol = "mergeModulesLeftBias", signature = "[Module] -> [Module] -> [Module]" }
    , { symbol = "mergeModulesWith", signature = "(Module -> Module -> Module) -> [Module] -> [Module] -> [Module]" }
    , { symbol = "mergeModuleWith", signature = "(Module -> Module -> Module) -> [Module] -> Module -> [Module]" }
    , { symbol = "modCoreImports", signature = "Module -> [Core.Import]" }
    , { symbol = "modImportNames", signature = "Module -> [ModuleName]" }
    , { symbol = "defsNull", signature = "Definitions" }
    , { symbol = "defsNames", signature = "Definitions -> [Name]" }
    , { symbol = "defsMatchNames", signature = "Definitions -> [String]" }
    , { symbol = "defsFromCore", signature = "Bool -> Core.Core -> Definitions" }
    , { symbol = "defsFromModules", signature = "HasCallStack => [Module] -> Definitions" }
    , { symbol = "defsMerge", signature = "HasCallStack => [Definitions] -> Definitions" }
    , { symbol = "defsCompose", signature = "HasCallStack => Definitions -> Definitions -> Definitions" }
    , { symbol = "inlinesFromModules", signature = "[Module] -> Inlines" }
    , { symbol = "phaseProgress", signature = "ModulePhase -> String" }
    , { symbol = "isErrorPhase", signature = "ModulePhase -> Bool" }
    , { symbol = "coreOptimize", signature = "Flags -> Newtypes -> Gamma -> Inlines -> Core.Core -> Error () (Core.Core,[Core.InlineDef])" }
    , { symbol = "traceDefGroups", signature = "Flags -> String -> Core.CorePhase () ()" }
    , { symbol = "packagesEmpty", signature = "Packages" }
    , { symbol = "pkgName", signature = "Package -> PackageName" }
    , { symbol = "isPackageDir", signature = "Packages -> FilePath -> Bool" }
    , { symbol = "packageInfoFromDir", signature = "Packages -> FilePath -> (FilePath,FilePath)" }
    , { symbol = "packageFromDir", signature = "Packages -> FilePath -> Maybe Package" }
    , { symbol = "searchPackages", signature = "Packages -> FilePath -> FilePath -> FilePath -> IO (Maybe FilePath)" }
    , { symbol = "visiblePackages", signature = "Packages -> FilePath -> [Package]" }
    , { symbol = "discoverPackages", signature = "FilePath -> IO Packages" }
    , { symbol = "node_path", signature = "String" }
    , { symbol = "node_modules", signature = "String" }
    , { symbol = "package_json", signature = "String" }
    , { symbol = "typeCheck", signature = "Flags -> Definitions -> [Core.Import] -> UserProgram -> Error () (Core.Core,Maybe RangeMap)" }
    , { symbol = "importMapFromCoreImports", signature = "[Import] -> [Core.Import] -> ImportMap" }
    , { symbol = "prettyEnvFromFlags", signature = "Flags -> TP.Env" }
    , { symbol = "colorSchemeFromFlags", signature = "Flags -> ColorScheme" }
    , { symbol = "prettyIncludePath", signature = "Flags -> Doc" }
    , { symbol = "phaseVerboseIO", signature = "Terminal -> Flags -> Int -> String -> (TP.Env -> Doc) -> IO ()" }
    , { symbol = "phaseShowIO", signature = "Terminal -> Flags -> Int -> String -> (TP.Env -> Doc) -> IO ()" }
    , { symbol = "showTypeSigs", signature = "Flags -> Bool" }
    , { symbol = "flagsHash", signature = "Flags -> String" }
    , { symbol = "flagsNull", signature = "Flags" }
    , { symbol = "optionsAll", signature = "[OptDescr Option]" }
    , { symbol = "options", signature = "([OptDescr Option],[OptDescr Option])" }
    , { symbol = "readHtmlBases", signature = "String -> [(String,String)]" }
    , { symbol = "targets", signature = "[(String,Flags -> Flags)]" }
    , { symbol = "environment", signature = "[ (String, String, (String -> [String]), String) ]" }
    , { symbol = "optionCompletions", signature = "[(String,String)]" }
    , { symbol = "getOptions", signature = "String -> IO (Flags,Mode)" }
    , { symbol = "processExtraOptions", signature = "Flags -> String -> Either String (Flags,Mode)" }
    , { symbol = "processOptions", signature = "Flags -> [String] -> IO (Flags,Mode)" }
    , { symbol = "processDerivedOptions", signature = "Flags -> Flags -> Flags" }
    , { symbol = "processInitialOptions", signature = "Flags -> [String] -> IO (Flags,Mode)" }
    , { symbol = "parseOptions", signature = "Flags -> [String] -> Either String (Flags,Mode)" }
    , { symbol = "getKokaBuildDir", signature = "FilePath -> Bool -> IO FilePath" }
    , { symbol = "getDefaultBuildTag", signature = "IO String" }
    , { symbol = "kkbuild", signature = "String" }
    , { symbol = "getKokaDirs", signature = "FilePath -> FilePath -> FilePath -> IO (FilePath,FilePath,FilePath,FilePath)" }
    , { symbol = "rootDirFrom", signature = "FilePath -> FilePath" }
    , { symbol = "extractFlags", signature = "Flags -> [Option] -> Flags" }
    , { symbol = "extractErrors", signature = "[Option] -> [String]" }
    , { symbol = "getEnvOptions", signature = "IO [String]" }
    , { symbol = "vcpkgFindRoot", signature = "FilePath -> IO (FilePath,FilePath)" }
    , { symbol = "conanSettingsFromFlags", signature = "Flags -> CC -> ([String],[(String,String)])" }
    , { symbol = "outName", signature = "Flags -> FilePath -> FilePath" }
    , { symbol = "fullBuildDir", signature = "Flags -> FilePath    -- usually <buildDir>/windows-x64-v2.x.x/<config>" }
    , { symbol = "buildVersionTag", signature = "Flags -> String" }
    , { symbol = "buildVariant", signature = "Flags -> String   -- for example: clang-debug-4ead5f" }
    , { symbol = "buildLibVariant", signature = "Flags -> String   -- for example: clang-debug, js-release" }
    , { symbol = "buildType", signature = "Flags -> BuildType" }
    , { symbol = "ccFlagsBuildFromFlags", signature = "CC -> Flags -> Args" }
    , { symbol = "ccGcc", signature = "String -> FilePath -> Bool -> CC" }
    , { symbol = "ccMsvc", signature = "String -> FilePath -> CC" }
    , { symbol = "ccFromPath", signature = "Flags -> FilePath -> IO (CC,Bool {-asan-})" }
    , { symbol = "ccCheckExist", signature = "CC -> IO ()" }
    , { symbol = "unquote", signature = "String -> [String]" }
    , { symbol = "onMacOS", signature = "Bool" }
    , { symbol = "onWindows", signature = "Bool" }
    , { symbol = "tripletOsName", signature = "String -> String" }
    , { symbol = "getTargetArch", signature = "IO String" }
    , { symbol = "hostArch", signature = "String" }
    , { symbol = "detectCC", signature = "Target -> IO String" }
    , { symbol = "findCC", signature = "[FilePath] -> [FilePath] -> IO (String,FilePath)" }
    , { symbol = "detectEditor", signature = "IO String" }
    , { symbol = "findEditor", signature = "[FilePath] -> [(String,String)] -> IO String" }
    , { symbol = "invokeError", signature = "[String] -> IO a" }
    , { symbol = "showHelp", signature = "Printer p => Flags -> p -> IO ()" }
    , { symbol = "showEnv", signature = "Printer p => Flags -> p -> IO ()" }
    , { symbol = "commandLineHelp", signature = "Flags -> IO Doc" }
    , { symbol = "showVersion", signature = "Printer p => Flags -> p -> IO ()" }
    , { symbol = "versionMessage", signature = "Flags -> Doc" }
    , { symbol = "buildcEmpty", signature = "Flags -> BuildContext" }
    , { symbol = "buildcAddRootSources", signature = "[FilePath] -> BuildContext -> Build (BuildContext,[ModuleName])" }
    , { symbol = "buildcAddRootModules", signature = "[ModuleName] -> BuildContext -> Build BuildContext" }
    , { symbol = "buildcClearRoots", signature = "BuildContext -> BuildContext" }
    , { symbol = "buildcRemoveRootModule", signature = "ModuleName -> BuildContext -> BuildContext" }
    , { symbol = "buildcLookupModuleName", signature = "FilePath -> BuildContext -> Maybe ModuleName" }
    , { symbol = "buildcRemoveRootSource", signature = "FilePath -> BuildContext -> BuildContext" }
    , { symbol = "buildcGetDefinitions", signature = "[ModuleName] -> BuildContext -> Definitions" }
    , { symbol = "buildcGetVisibleDefinitions", signature = "[ModuleName] -> BuildContext -> Definitions" }
    , { symbol = "buildcGetLexemes", signature = "ModuleName -> BuildContext -> Maybe [Lexeme]" }
    , { symbol = "buildcGetRangeMap", signature = "ModuleName -> BuildContext -> Maybe (RangeMap,[Lexeme])" }
    , { symbol = "buildcLookupProgram", signature = "ModuleName -> BuildContext -> Maybe UserProgram" }
    , { symbol = "buildcPrettyEnvFor", signature = "TP.Env -> ModuleName -> BuildContext -> TP.Env" }
    , { symbol = "buildcGetMatchNames", signature = "[ModuleName] -> BuildContext -> [String]" }
    , { symbol = "buildcModulePaths", signature = "BuildContext -> [(ModuleName,FilePath)]" }
    , { symbol = "buildcFocus", signature = "[ModuleName] -> BuildContext -> ([ModuleName] -> BuildContext -> Build (BuildContext, a)) -> Build (BuildContext, a)" }
    , { symbol = "buildcFreshFromRoots", signature = "BuildContext -> Build BuildContext" }
    , { symbol = "buildcValidateFlags", signature = "BuildContext -> Build BuildContext" }
    , { symbol = "buildcValidate", signature = "Bool -> [ModuleName] -> BuildContext -> Build BuildContext" }
    , { symbol = "buildcSplitRoots", signature = "BuildContext -> ([Module],[Module])" }
    , { symbol = "buildcTypeCheck", signature = "[ModuleName] -> BuildContext -> Build BuildContext" }
    , { symbol = "buildcBuild", signature = "[Name] -> BuildContext -> Build BuildContext" }
    , { symbol = "buildcBuildEx", signature = "Bool -> [ModuleName] -> [Name] -> BuildContext -> Build BuildContext" }
    , { symbol = "buildcGetMainEntry", signature = "ModuleName -> BuildContext -> Maybe (FilePath,IO ())" }
    , { symbol = "buildcRunEntry", signature = "Name -> BuildContext -> Build (Maybe (IO ()), BuildContext)" }
    , { symbol = "buildcRunExpr", signature = "[ModuleName] -> String -> BuildContext -> Build (Maybe (IO ()), BuildContext)" }
    , { symbol = "buildcCompileEntry", signature = "Bool -> Name -> BuildContext -> Build (BuildContext,Maybe (Type, Maybe (FilePath,IO ())))" }
    , { symbol = "buildcCompileExpr", signature = "Bool -> Bool -> [ModuleName] -> String -> BuildContext -> Build (BuildContext, Maybe (Type, Maybe (FilePath,IO ())))" }
    , { symbol = "buildcGetErrorRangeOf", signature = "BuildContext -> Maybe Range" }
    , { symbol = "buildcGetErrorRange", signature = "BuildContext -> Build (Maybe Range)" }
    , { symbol = "buildcHasError", signature = "BuildContext -> Build Bool" }
    , { symbol = "buildcThrowOnError", signature = "BuildContext -> Build ()" }
    , { symbol = "buildcCompileMainBody", signature = "Bool -> String -> [String] -> FilePath -> Name -> Name -> Type -> BuildContext -> Build (BuildContext,Maybe (Type, Maybe (FilePath,IO ())))" }
    , { symbol = "bunlines", signature = "[String] -> BString" }
    , { symbol = "completeMain", signature = "Bool -> Name -> Type -> BuildContext -> Build (Type,String -> String,String,[String])" }
    , { symbol = "withVirtualModule", signature = "FilePath -> BString -> BuildContext -> (ModuleName -> BuildContext -> Build (BuildContext,a)) -> Build (BuildContext,a)" }
    , { symbol = "withVirtualFile", signature = "FilePath -> BString -> (FilePath -> Build a) -> Build a" }
    , { symbol = "buildcFindModule", signature = "HasCallStack => ModuleName -> BuildContext -> Module" }
    , { symbol = "buildcLookupModule", signature = "HasCallStack => ModuleName -> BuildContext -> Maybe Module" }
    , { symbol = "buildcLookupInfo", signature = "Name -> BuildContext -> [NameInfo]" }
    , { symbol = "buildcLookupTypeOf", signature = "Name -> BuildContext -> Maybe Type" }
    , { symbol = "buildcOutputDir", signature = "Build FilePath" }
    , { symbol = "buildcSearchSourceFile", signature = "FilePath -> BuildContext -> Build (Maybe (FilePath,FilePath))" }
    , { symbol = "buildcThrowOnError", signature = "Build ()" }
    , { symbol = "buildcTermInfo", signature = "(TP.Env -> Doc) -> Build ()" }
    , { symbol = "buildcFlags", signature = "Build Flags" }
    , { symbol = "buildLiftIO", signature = "IO a -> Build a" }
    , { symbol = "buildcFlushErrors", signature = "BuildContext -> Build ()" }
    , { symbol = "buildcLiftErrors", signature = "(a -> BuildContext) -> Build a -> Build a" }
    , { symbol = "analyzeCCtx", signature = "Range -> Newtypes -> Expr -> (Int -> ((Expr,[(Range,Doc)]),Int))" }
    , { symbol = "cctxCreate", signature = "Expr -> CCtx Expr" }
    , { symbol = "cctxExpr", signature = "Expr -> CCtx Ctx" }
    , { symbol = "cctxCon", signature = "TName -> ConRepr -> [Type] -> [Expr] -> CCtx Ctx" }
    , { symbol = "cctxConRecurse", signature = "TName -> ConRepr -> [Type] -> [Expr] -> CCtx Ctx" }
    , { symbol = "cctxConFinal", signature = "TName -> ConRepr -> [Type] -> [Expr] -> Expr -> [Expr] -> CCtx Ctx" }
    , { symbol = "cctxCheckNoHole", signature = "Expr -> CCtx ()" }
    , { symbol = "cctxFind", signature = "[(Range,Doc)] -> [Expr] -> [Expr] -> CCtx ([Expr],Ctx,[Expr])" }
    , { symbol = "makeUniqueDef", signature = "Expr -> CCtx (Def,Expr)" }
    , { symbol = "isHole", signature = "Expr -> Bool" }
    , { symbol = "makeCCtxEmpty", signature = "Type -> Expr" }
    , { symbol = "makeCCtxCreate", signature = "Type -> Type -> Expr -> Expr -> Expr" }
    , { symbol = "makeFieldAddrOf", signature = "Expr -> TName -> Name -> Type -> Expr" }
    , { symbol = "makeCCtxSetContextPath", signature = "Expr -> TName -> Name -> Expr" }
    , { symbol = "runCCtx", signature = "Range -> Newtypes -> Int -> CCtx a -> (Either [(Range,Doc)] a,Int)" }
    , { symbol = "getEnv", signature = "CCtx CCtxEnv" }
    , { symbol = "withEnv", signature = "CCtxEnv -> CCtx a -> CCtx a" }
    , { symbol = "updateEnv", signature = "(CCtxEnv -> CCtxEnv) -> CCtx a -> CCtx a" }
    , { symbol = "emitError", signature = "Doc -> CCtx a" }
    , { symbol = "emitErrors", signature = "[(Range,Doc)] -> CCtx a" }
    , { symbol = "try", signature = "CCtx a -> CCtx (Either [(Range,Doc)] a)" }
    , { symbol = "mtrace", signature = "String -> CCtx ()" }
    , { symbol = "getFieldName", signature = "TName -> Int -> CCtx TName" }
    , { symbol = "ensureValidHoleType", signature = "Type -> CCtx ()" }
    , { symbol = "dataTypeNameOf", signature = "Type -> Either Type Name" }
    , { symbol = "lookupFieldName", signature = "TName -> Int -> CCtx (Either String TName)" }
    , { symbol = "analyzeBranches", signature = "Newtypes -> Name -> Range -> [Branch] -> [Type] -> [DataInfo] -> Bool -> (Bool,[(Range,Doc)],[Branch])" }
    , { symbol = "isComplete", signature = "(ConInfo,[Match]) -> Bool" }
    , { symbol = "dataInfoGetConInfos", signature = "Bool -> DataInfo -> [ConInfo]" }
    , { symbol = "matchBranches", signature = "Newtypes -> Name -> Range -> [Branch] -> [Type] -> [DataInfo] -> Bool -> (Bool,[Branch],Warnings)" }
    , { symbol = "matchBranch", signature = "Newtypes -> Name -> Range -> [Match] -> [Type] -> Branch -> ([Match],Branch,Warnings)" }
    , { symbol = "analyzeGuards", signature = "Range -> [Guard] -> Warnings" }
    , { symbol = "matchPattern", signature = "Newtypes -> Name -> Range -> Bool -> (Match,Type,Pattern) -> (Match,Pattern,Warnings)" }
    , { symbol = "matchPatterns", signature = "Newtypes -> Name -> Range -> Bool -> [Match] -> [Type] -> [Pattern] -> ([Match], [Pattern], Warnings)" }
    , { symbol = "makeMatch", signature = "[ConInfo] -> [(ConInfo,[Match])] -> Match" }
    , { symbol = "instantiatePatCon", signature = "Type -> [(Name,Type)] -> Scheme -> Maybe ([Type],Type)" }
    , { symbol = "remove", signature = "[ConInfo] -> [ConInfo] -> [ConInfo]" }
    , { symbol = "lookupDataInfo", signature = "Newtypes -> Name -> Maybe DataInfo" }
    , { symbol = "lookupConInfos", signature = "Newtypes -> Type -> [ConInfo]" }
    , { symbol = "finalBranchIsCatchAll", signature = "[Branch] -> Bool" }
    , { symbol = "analyzeResume", signature = "Name -> Name -> Bool -> Expr -> ResumeKind" }
    , { symbol = "arBranches", signature = "ResumeKind -> [Branch] -> ResumeKind" }
    , { symbol = "arBranch", signature = "ResumeKind -> Branch -> ResumeKind" }
    , { symbol = "arGuard", signature = "ResumeKind -> Guard -> ResumeKind" }
    , { symbol = "arExprsAnd", signature = "[Expr] -> ResumeKind" }
    , { symbol = "rand,ror", signature = "ResumeKind -> ResumeKind -> ResumeKind" }
    , { symbol = "regroup", signature = "[Def] -> [DefGroup]" }
    , { symbol = "find", signature = "TName -> M.Map TName a -> a" }
    , { symbol = "borrowedEmpty", signature = "Borrowed" }
    , { symbol = "borrowedNew", signature = "[BorrowDef] -> Borrowed" }
    , { symbol = "borrowedExtends", signature = "[BorrowDef] -> Borrowed -> Borrowed" }
    , { symbol = "borrowedExtend", signature = "BorrowDef -> Borrowed -> Borrowed" }
    , { symbol = "borrowedLookup", signature = "Name -> Borrowed -> Maybe [ParamInfo]" }
    , { symbol = "borrowedLookupFip", signature = "Name -> Borrowed -> Maybe Fip" }
    , { symbol = "borrowedCompose", signature = "Borrowed -> Borrowed -> Borrowed" }
    , { symbol = "borrowedExtendICore", signature = "Core -> Borrowed -> Borrowed" }
    , { symbol = "extractBorrowed", signature = "Core -> Borrowed" }
    , { symbol = "extractBorrowDefs", signature = "DefGroups -> [BorrowDef]" }
    , { symbol = "extractBorrowExternals", signature = "Externals -> [BorrowDef]" }
    , { symbol = "extractExternal", signature = "External -> Maybe BorrowDef" }
    , { symbol = "extractBorrowDef", signature = "Bool -> Def -> Maybe BorrowDef" }
    , { symbol = "ppBorrowed", signature = "Env -> Borrowed -> Doc" }
    , { symbol = "ctailOptimize", signature = "Pretty.Env -> Newtypes -> Gamma -> Bool -> CorePhase b ()" }
    , { symbol = "uctailOptimize", signature = "Pretty.Env -> Newtypes -> Gamma -> Bool -> DefGroups -> Unique DefGroups" }
    , { symbol = "ctailDefGroups", signature = "Bool -> DefGroups -> CTail DefGroups" }
    , { symbol = "ctailDefGroup", signature = "Bool -> DefGroup -> CTail [DefGroup]" }
    , { symbol = "ctailDef", signature = "Bool -> Def -> CTail [DefGroup]" }
    , { symbol = "makeCDefExpr", signature = "TName -> Expr -> Expr" }
    , { symbol = "hasResultRefType", signature = "Type -> CTail (Maybe ([TypeVar],[(Name,Type)],Effect,Type))" }
    , { symbol = "hasRefType", signature = "Type -> CTail Bool" }
    , { symbol = "ctailWrapper", signature = "TName -> Maybe Expr -> Expr -> CTail Expr" }
    , { symbol = "ctailWrapperBody", signature = "Type -> TName -> Maybe Expr -> [TypeVar] -> [TName] -> CTail Expr" }
    , { symbol = "makeIfIsAffine", signature = "Expr -> Expr -> Expr" }
    , { symbol = "hasCTailCall", signature = "TName -> Bool -> Expr -> Bool" }
    , { symbol = "hasCTailCallArg", signature = "TName -> [Expr] -> Bool" }
    , { symbol = "ctailExpr", signature = "Bool -> Expr -> CTail Expr" }
    , { symbol = "mkConApp", signature = "CtxPath -> Expr -> [Expr] -> Expr" }
    , { symbol = "bindArgs", signature = "[Expr] -> ([Expr] -> CTail ([DefGroup],Expr)) -> CTail ([DefGroup],Expr)" }
    , { symbol = "ctailBranch", signature = "Branch -> CTail Branch" }
    , { symbol = "ctailGuard", signature = "Guard -> CTail (Guard)" }
    , { symbol = "ctailTryArg", signature = "Bool -> TName -> TName -> Maybe TName -> (CtxPath -> [Expr] -> CTail ([DefGroup],Expr)) -> Int -> [Expr] -> CTail (Maybe ([DefGroup],Expr))" }
    , { symbol = "getCtxPath", signature = "Bool -> TName -> Int -> CTail CtxPath" }
    , { symbol = "ctailFoundArg", signature = "TName -> Maybe TName -> ([Expr] -> CTail ([DefGroup],Expr)) -> Int ->" }
    , { symbol = "makeHole", signature = "Type -> Expr" }
    , { symbol = "makeCCtxEmpty", signature = "Type -> Expr" }
    , { symbol = "makeFieldAddrOf", signature = "TName -> TName -> Name -> Type -> Expr" }
    , { symbol = "makeCCtxExtend", signature = "TName -> TName -> TName -> TName -> Name -> Type -> Bool -> Expr" }
    , { symbol = "makeCCtxApply", signature = "Bool {-isMulti-} -> Bool {-isAlwaysAffine-} -> TName -> Expr -> Expr" }
    , { symbol = "uniqueTName", signature = "Type -> CTail TName" }
    , { symbol = "foldMapM", signature = "(Monad m, Foldable t) => (a -> m b) -> t a -> m [b]" }
    , { symbol = "maybeStats", signature = "[Maybe Expr] -> Expr -> Expr" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> CTail a -> CTail a" }
    , { symbol = "getEnv", signature = "CTail Env" }
    , { symbol = "updateSt", signature = "(CTailState -> CTailState) -> CTail ()" }
    , { symbol = "getSt", signature = "CTail CTailState" }
    , { symbol = "ctailRun", signature = "Pretty.Env -> Newtypes -> Gamma -> Bool -> CTail a -> Unique a" }
    , { symbol = "withContext", signature = "TName -> Bool -> Bool -> Maybe TName -> CTail a -> CTail a" }
    , { symbol = "getCTailFun", signature = "CTail Expr" }
    , { symbol = "getCTailSlot", signature = "CTail (Maybe TName)" }
    , { symbol = "getIsMulti", signature = "CTail Bool" }
    , { symbol = "getUseContextPath", signature = "CTail Bool" }
    , { symbol = "getIsAlwaysAffine", signature = "CTail Bool" }
    , { symbol = "getFieldName", signature = "TName -> Int -> CTail (Either String (Expr,TName))" }
    , { symbol = "getCurrentDef", signature = "CTail [Def]" }
    , { symbol = "withCurrentDef", signature = "Def -> CTail a -> CTail a" }
    , { symbol = "getCurrentDefName", signature = "CTail TName" }
    , { symbol = "getPrettyEnv", signature = "CTail Pretty.Env" }
    , { symbol = "withPrettyEnv", signature = "(Pretty.Env -> Pretty.Env) -> CTail a -> CTail a" }
    , { symbol = "getUniq", signature = "CTail Int" }
    , { symbol = "modifyUniq", signature = "(Int -> Int) -> CTail ()" }
    , { symbol = "setUniq", signature = "Int -> CTail ()" }
    , { symbol = "isValueType", signature = "Name -> CTail Bool" }
    , { symbol = "ctailTraceDoc", signature = "(Pretty.Env -> Doc) -> CTail ()" }
    , { symbol = "ctailTrace", signature = "String -> CTail ()" }
    , { symbol = "checkCore", signature = "Bool -> Bool -> Env -> Gamma -> CorePhase b ()" }
    , { symbol = "extendGamma", signature = "[(Name,NameInfo)] -> Check a -> Check a" }
    , { symbol = "withDef", signature = "Def -> Check a -> Check a" }
    , { symbol = "getEnv", signature = "Check CheckEnv" }
    , { symbol = "getGamma", signature = "Check Gamma" }
    , { symbol = "lookupVar", signature = "Name -> Check Scheme" }
    , { symbol = "failDoc", signature = "(Env -> Doc) -> Check a" }
    , { symbol = "ppDefs", signature = "Env -> [Def] -> Doc" }
    , { symbol = "checkTName", signature = "TName -> Check TName" }
    , { symbol = "checkType", signature = "Type -> Check Type" }
    , { symbol = "checkDefGroups", signature = "DefGroups -> Check a -> Check a" }
    , { symbol = "checkDefGroup", signature = "DefGroup -> Check a -> Check a" }
    , { symbol = "checkDef", signature = "Def -> Check ()" }
    , { symbol = "coreNameInfo", signature = "TName -> (Name,NameInfo)" }
    , { symbol = "check", signature = "Expr -> Check Type" }
    , { symbol = "checkBranch", signature = "[Type] -> Branch -> Check [Type]" }
    , { symbol = "checkPattern", signature = "(Type,Pattern) -> Check ()" }
    , { symbol = "findConstrArgs", signature = "(Env -> Doc) -> Type -> Name -> Check [Type]" }
    , { symbol = "matchSub", signature = "String -> (Env -> Doc) -> Type -> Type -> Check ()" }
    , { symbol = "matchSubEff", signature = "String -> (Env -> Doc) -> Type -> Type -> Check ()" }
    , { symbol = "match", signature = "String -> (Env -> Doc) -> Type -> Type -> Check ()" }
    , { symbol = "showCheck", signature = "String -> String -> Type -> Type -> (Env -> Doc) -> Check a" }
    , { symbol = "checkFBIP", signature = "Pretty.Env ->  Platform -> Newtypes -> Borrowed -> Gamma -> CorePhase b ()" }
    , { symbol = "chkDefGroups", signature = "DefGroups -> Chk ()" }
    , { symbol = "chkDefGroup", signature = "DefGroup -> Chk ()" }
    , { symbol = "chkTopLevelDef", signature = "[Name] -> Def -> Chk ()" }
    , { symbol = "chkTopLevelExpr", signature = "[ParamInfo] -> Expr -> Chk ()" }
    , { symbol = "chkExpr", signature = "Expr -> Chk ()" }
    , { symbol = "chkModCons", signature = "[Expr] -> Chk ()" }
    , { symbol = "chkBranches", signature = "[Expr] -> [Branch] -> Chk ()" }
    , { symbol = "isBorrowedScrutinee", signature = "Expr -> Chk ParamInfo" }
    , { symbol = "chkScrutinee", signature = "(ParamInfo, Expr) -> Chk ()" }
    , { symbol = "chkBranch", signature = "[ParamInfo] -> [Type] -> Branch -> Chk ()" }
    , { symbol = "chkGuard", signature = "Guard -> Chk ()" }
    , { symbol = "isPatternMatchError", signature = "Branch -> Bool" }
    , { symbol = "bindPattern", signature = "(Pattern, Type) -> Output -> Chk Output" }
    , { symbol = "chkApp", signature = "Expr -> [Expr] -> Chk ()" }
    , { symbol = "chkArg", signature = "(ParamInfo, Expr) -> Chk ()" }
    , { symbol = "chkLit", signature = "Lit -> Chk ()" }
    , { symbol = "chkWrap", signature = "TName -> VarInfo -> Chk ()" }
    , { symbol = "chkAllocation", signature = "TName -> ConRepr -> Chk ()" }
    , { symbol = "chkEffect", signature = "Tau -> Chk ()" }
    , { symbol = "prettyGammaNm", signature = "Pretty.Env -> Output -> Doc" }
    , { symbol = "prettyCon", signature = "Pretty.Env -> TName -> Int -> Doc" }
    , { symbol = "prettyGammaDia", signature = "Pretty.Env -> Output -> Doc" }
    , { symbol = "runChk", signature = "Pretty.Env -> Int -> Platform -> Newtypes -> Borrowed -> Gamma -> Chk a -> (a,[(Range,Doc)])" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Chk a -> Chk a" }
    , { symbol = "getEnv", signature = "Chk Env" }
    , { symbol = "withInput", signature = "(Input -> Input) -> Chk a -> Chk a" }
    , { symbol = "getInput", signature = "Chk Input" }
    , { symbol = "writeOutput", signature = "Output -> Chk ()" }
    , { symbol = "withFip", signature = "Fip -> Chk a -> Chk a" }
    , { symbol = "getFip", signature = "Chk Fip" }
    , { symbol = "mayRecurse", signature = "Chk Bool" }
    , { symbol = "mayDealloc", signature = "Chk Bool" }
    , { symbol = "mayAlloc", signature = "Chk Bool" }
    , { symbol = "chkFunCallable", signature = "Name -> Chk ()" }
    , { symbol = "chkLazyCon", signature = "TName -> ConRepr -> Chk ()" }
    , { symbol = "extractOutput", signature = "Chk () -> Chk Output" }
    , { symbol = "requireCapability", signature = "Chk Bool -> (Pretty.Env -> Maybe Doc) -> Chk ()" }
    , { symbol = "withNonTail", signature = "Chk a -> Chk a" }
    , { symbol = "withTailModProduct", signature = "[Branch] -> Chk a -> Chk a" }
    , { symbol = "withTailMod", signature = "[Expr] -> Chk a -> Chk a" }
    , { symbol = "withBorrowed", signature = "S.Set Name -> Chk a -> Chk a" }
    , { symbol = "isBorrowed", signature = "TName -> Chk Bool" }
    , { symbol = "markSeen", signature = "TName -> VarInfo -> Chk ()" }
    , { symbol = "markBorrowed", signature = "TName -> VarInfo -> Chk ()" }
    , { symbol = "getAllocation", signature = "TName -> Reusable -> Chk ()" }
    , { symbol = "provideToken", signature = "TName -> Reusable -> Output -> Chk Output" }
    , { symbol = "joinContexts", signature = "[[Pattern]] -> [Output] -> Chk Output" }
    , { symbol = "addTypeReuseInfo", signature = "TName -> Reusable -> Chk Reusable" }
    , { symbol = "bindName", signature = "TName -> Reusable -> Output -> Chk Output" }
    , { symbol = "chkDrop", signature = "Bool -> TName -> Chk ()" }
    , { symbol = "joinBranches", signature = "AllocInLoop -> AllocInLoop -> AllocInLoop" }
    , { symbol = "getAllocCredits", signature = "S.Set Id -> AllocTree -> (FipAlloc, AllocInLoop)" }
    , { symbol = "prettyFipAlloc", signature = "FipAlloc -> String" }
    , { symbol = "checkOutputEmpty", signature = "Output -> Chk ()" }
    , { symbol = "simplifyAllocTree", signature = "AllocTree -> AllocTree" }
    , { symbol = "zipParamInfo", signature = "[ParamInfo] -> [b] -> [(ParamInfo, b)]" }
    , { symbol = "needsDupDrop", signature = "TName -> Chk Bool" }
    , { symbol = "needsDupDropTp", signature = "Type -> Chk Bool" }
    , { symbol = "isFlatType", signature = "TName -> Chk Bool" }
    , { symbol = "dataTypeIsFlat", signature = "Type -> Chk Bool" }
    , { symbol = "getDataInfoTypeName", signature = "Name -> Chk (Maybe DataInfo)" }
    , { symbol = "getDataInfo", signature = "Type -> Chk (Maybe DataInfo)" }
    , { symbol = "getNewtypes", signature = "Chk Newtypes" }
    , { symbol = "getPlatform", signature = "Chk Platform" }
    , { symbol = "withCurrentDef", signature = "Def -> Chk a -> Chk a" }
    , { symbol = "currentDefNames", signature = "Chk [Name]" }
    , { symbol = "getParamInfos", signature = "Name -> Chk [ParamInfo]" }
    , { symbol = "lookupFip", signature = "Name -> Chk Fip" }
    , { symbol = "traceDoc", signature = "(Pretty.Env -> Doc) -> Chk ()" }
    , { symbol = "chkTrace", signature = "String -> Chk ()" }
    , { symbol = "emitDoc", signature = "Range -> Doc -> Chk ()" }
    , { symbol = "emitWarning", signature = "(Pretty.Env -> Doc) -> Chk ()" }
    , { symbol = "getConstructorAllocSize", signature = "ConRepr -> Chk Reusable" }
    , { symbol = "exprUnit", signature = "Expr" }
    , { symbol = "makeIfExpr", signature = "Expr -> Expr -> Expr -> Expr" }
    , { symbol = "makeVector", signature = "Type -> [Expr] -> Expr" }
    , { symbol = "wrapOptional", signature = "Type -> Expr -> Expr" }
    , { symbol = "makeOptionalNone", signature = "Type -> Expr" }
    , { symbol = "makeList", signature = "Type -> [Expr] -> Expr" }
    , { symbol = "makeDef", signature = "HasCallStack => Name -> Expr -> Def" }
    , { symbol = "makeTDef", signature = "TName -> Expr -> Def" }
    , { symbol = "makeDefExpr", signature = "Expr -> Def" }
    , { symbol = "makeStats", signature = "[Expr] -> Expr" }
    , { symbol = "makeDefsLet", signature = "[Def] -> Expr -> Expr" }
    , { symbol = "unzipM", signature = "Monad m => m [(a,b)] -> m ([a],[b])" }
    , { symbol = "makeDropSpecial", signature = "TName -> Expr -> Expr -> Expr -> Expr" }
    , { symbol = "coreName", signature = "Core -> Name" }
    , { symbol = "isCompilerImport", signature = "Import -> Bool" }
    , { symbol = "isUserImport", signature = "Import -> Bool" }
    , { symbol = "makeImport", signature = "HasCallStack => Name -> String -> ImportProvenance -> Visibility -> String -> Import" }
    , { symbol = "externalVis", signature = "External -> Visibility" }
    , { symbol = "externalImportLookup", signature = "Target -> BuildType -> String -> External -> Maybe String" }
    , { symbol = "eimportLookup", signature = "BuildType -> String -> [(String,String)] -> Maybe String" }
    , { symbol = "lookupTarget", signature = "Target -> [(Target,a)] -> Maybe a" }
    , { symbol = "flattenTypeDefGroups", signature = "TypeDefGroups -> [TypeDef]" }
    , { symbol = "conReprHasCtxPath", signature = "ConRepr -> Bool" }
    , { symbol = "conReprCtxPath", signature = "ConRepr -> Maybe CtxPath" }
    , { symbol = "conReprScanCount", signature = "ConRepr -> Int" }
    , { symbol = "conReprAllocSize", signature = "Platform -> ConRepr -> Int" }
    , { symbol = "conReprAllocSizeScan", signature = "Platform -> ConRepr -> (Int,Int)" }
    , { symbol = "dataReprIsValue", signature = "DataRepr -> Bool" }
    , { symbol = "needsTagField", signature = "DataRepr -> Bool" }
    , { symbol = "conReprIsValue", signature = "ConRepr -> Bool" }
    , { symbol = "dataInfoIsValue", signature = "DataInfo -> Bool" }
    , { symbol = "getDataRepr", signature = "DataInfo -> (DataRepr,[ConRepr])" }
    , { symbol = "getConRepr", signature = "DataInfo -> ConInfo -> ConRepr" }
    , { symbol = "getDataReprEx", signature = "(DataInfo -> Bool) -> DataInfo -> (DataRepr,[ConRepr])" }
    , { symbol = "flattenDefGroups", signature = "[DefGroup] -> [Def]" }
    , { symbol = "flattenAllDefGroups", signature = "[DefGroups] -> [Def]" }
    , { symbol = "mapDefGroup", signature = "(Def -> Def) -> DefGroup -> DefGroup" }
    , { symbol = "mapMDefGroup", signature = "(Monad m) => (Def -> m Def) -> DefGroup -> m DefGroup" }
    , { symbol = "defIsVal", signature = "Def -> Bool" }
    , { symbol = "defParamInfos", signature = "Def -> [ParamInfo]" }
    , { symbol = "inlineDefIsSpecialize", signature = "InlineDef -> Bool" }
    , { symbol = "getCoreDefs", signature = "CorePhase b DefGroups" }
    , { symbol = "setCoreDefs", signature = "DefGroups -> CorePhase b ()" }
    , { symbol = "withCoreDefs", signature = "(DefGroups -> a) -> CorePhase b a" }
    , { symbol = "runCorePhase", signature = "Int -> CorePhase b a -> Error b a" }
    , { symbol = "liftCorePhaseUniq", signature = "(Int -> DefGroups -> (DefGroups,Int)) -> CorePhase b ()" }
    , { symbol = "liftCorePhase", signature = "(DefGroups -> DefGroups) -> CorePhase b ()" }
    , { symbol = "liftError", signature = "Error b a -> CorePhase b a" }
    , { symbol = "defTName", signature = "Def -> TName" }
    , { symbol = "defsTNames", signature = "[Def] -> TNames" }
    , { symbol = "defGroupTNames", signature = "DefGroup -> TNames" }
    , { symbol = "defGroupsTNames", signature = "DefGroups -> TNames" }
    , { symbol = "isTotal", signature = "Expr -> Bool" }
    , { symbol = "isTotalFun", signature = "Expr -> Bool" }
    , { symbol = "hasTotalEffect", signature = "Type -> Bool" }
    , { symbol = "isMonType", signature = "Type -> Bool" }
    , { symbol = "isMonEffect", signature = "Effect -> Bool" }
    , { symbol = "isInlineable", signature = "Int -> Def -> Bool" }
    , { symbol = "costInf", signature = "Int" }
    , { symbol = "costDef", signature = "Def -> Int" }
    , { symbol = "costLocalDef", signature = "Def -> Int" }
    , { symbol = "costExpr", signature = "Expr -> Int" }
    , { symbol = "getTypeArityExpr", signature = "Expr -> Int" }
    , { symbol = "getParamArityExpr", signature = "Expr -> Int" }
    , { symbol = "getEffExpr", signature = "Expr -> Effect" }
    , { symbol = "foldMapExpr", signature = "Monoid a => (Expr -> a) -> Expr -> a" }
    , { symbol = "anySubExpr", signature = "(Expr -> Bool) -> Expr -> Bool" }
    , { symbol = "foldExpr", signature = "(Expr -> a -> a) -> a -> Expr -> a" }
    , { symbol = "rewriteBottomUp", signature = "(Expr -> Expr) -> Expr -> Expr" }
    , { symbol = "rewriteBottomUpM", signature = "(Monad m) => (Expr -> m Expr) -> Expr -> m Expr" }
    , { symbol = "rewriteTopDown", signature = "(Expr -> Expr) -> Expr -> Expr" }
    , { symbol = "rewriteTopDownM", signature = "(Monad m) => (Expr -> m Expr) -> Expr -> m Expr" }
    , { symbol = "tnamesEmpty", signature = "TNames" }
    , { symbol = "tnamesIsEmpty", signature = "TNames -> Bool" }
    , { symbol = "tnamesList", signature = "TNames -> [TName]" }
    , { symbol = "tnamesFromList", signature = "[TName] -> TNames" }
    , { symbol = "tnamesInsertAll", signature = "TNames -> [TName] -> TNames" }
    , { symbol = "tnamesUnion", signature = "TNames -> TNames -> TNames" }
    , { symbol = "tnamesUnions", signature = "[TNames] -> TNames" }
    , { symbol = "tnamesDiff", signature = "TNames -> TNames -> TNames" }
    , { symbol = "tnamesRemove", signature = "[TName] -> TNames -> TNames" }
    , { symbol = "tnamesMember", signature = "TName -> TNames -> Bool" }
    , { symbol = "tnamesDisjoint", signature = "TNames -> TNames -> Bool" }
    , { symbol = "makeLet", signature = "[DefGroup] -> Expr -> Expr" }
    , { symbol = "addApps", signature = "[Expr] -> (Expr -> Expr)" }
    , { symbol = "addTypeLambdas", signature = "[TypeVar] -> (Expr -> Expr)" }
    , { symbol = "addLambdas", signature = "[(Name, Type)] -> (Type -> Expr -> Expr)" }
    , { symbol = "addLambdasTName", signature = "[TName] -> (Type -> Expr -> Expr)" }
    , { symbol = "addNonRec", signature = "Name -> Type -> Expr -> (Expr -> Expr)" }
    , { symbol = "isValueExpr", signature = "Expr -> Bool" }
    , { symbol = "addCoreDef", signature = "Core -> Def -> Core" }
    , { symbol = "coreNull", signature = "Name -> Core" }
    , { symbol = "freshName", signature = "HasUnique m => String -> m Name" }
    , { symbol = "openEffectExpr", signature = "Effect -> Effect -> Type -> Type -> Expr -> Expr" }
    , { symbol = "makeInt32", signature = "Integer -> Expr" }
    , { symbol = "makeEvIndex", signature = "Integer -> Expr" }
    , { symbol = "makeSizeT", signature = "Integer -> Expr" }
    , { symbol = "extractSignatures", signature = "Core -> [Type]" }
    , { symbol = "splitFun", signature = "Type -> ([(Name,Type)], Type)" }
    , { symbol = "splitTForall", signature = "Type -> ([TypeVar], Type)" }
    , { symbol = "depTName", signature = "TName -> Deps" }
    , { symbol = "depName", signature = "Name -> Deps" }
    , { symbol = "dependencies", signature = "[InlineDef] -> Core -> Deps" }
    , { symbol = "inlineDependencies", signature = "[InlineDef] -> Deps" }
    , { symbol = "coreDependencies", signature = "Core -> Deps" }
    , { symbol = "depTDef", signature = "TypeDef -> Deps" }
    , { symbol = "depType", signature = "Type -> Deps" }
    , { symbol = "depDef", signature = "Def -> Deps" }
    , { symbol = "depExpr", signature = "Expr -> Deps" }
    , { symbol = "extractDepsFromInlineDefs", signature = "[InlineDef] -> [ModuleName]" }
    , { symbol = "extractDepsFromDefs", signature = "DefGroups -> [ModuleName]" }
    , { symbol = "extractDepsFromExpr", signature = "Expr -> S.Set ModuleName" }
    , { symbol = "extractDepsFromSignatures", signature = "Core -> [ModuleName]" }
    , { symbol = "isTopLevel", signature = "Def -> Bool" }
    , { symbol = "freeLocals", signature = "HasExpVar a => a -> TNames" }
    , { symbol = "addTypeApps", signature = "[TypeVar] -> (Expr -> Expr)" }
    , { symbol = "notIn", signature = "TName -> [(TName, Expr)] -> Bool" }
    , { symbol = "analyzeDivergence", signature = "[Def] -> Bool" }
    , { symbol = "isDivergent", signature = "Def -> Bool" }
    , { symbol = "isDivFun", signature = "Name -> [TName] -> Expr -> Bool" }
    , { symbol = "runDiv", signature = "Name -> [TName] -> Div a -> (a,[Call])" }
    , { symbol = "getRel", signature = "Div Rel" }
    , { symbol = "withRel", signature = "Rel -> Div a -> Div a" }
    , { symbol = "addCall", signature = "Name -> [Size] -> Div ()" }
    , { symbol = "at", signature = "[S.NameSet] -> Int -> S.NameSet" }
    , { symbol = "isRecursiveCall", signature = "Name -> Div Bool" }
    , { symbol = "lookupSize", signature = "Name -> Int -> Name -> Div Size" }
    , { symbol = "addRelation", signature = "Size -> Name -> Name -> Div a -> Div a" }
    , { symbol = "divExpr", signature = "Expr -> Div ()" }
    , { symbol = "divBranch", signature = "[Expr] -> Branch -> Div ()" }
    , { symbol = "divPattern", signature = "Size -> (Maybe Name,Pattern) -> Div (Div a -> Div a)" }
    , { symbol = "divDefGroup", signature = "DefGroup -> Div ()" }
    , { symbol = "argumentSize", signature = "Name -> (Int,Expr) -> Div Size" }
    , { symbol = "compose", signature = "[(a -> a)] -> a -> a" }
    , { symbol = "traceGroups", signature = "[DefGroup] -> String" }
    , { symbol = "liftFunctions", signature = "Pretty.Env -> CorePhase b ()" }
    , { symbol = "liftDefGroups", signature = "Bool   -- top-level functions are allowed" }
    , { symbol = "liftDefGroup", signature = "Bool {-toplevel -} -> DefGroup -> Lift DefGroups" }
    , { symbol = "liftDef", signature = "Bool -> Def -> Lift Def" }
    , { symbol = "liftSort", signature = "Bool -> DefSort -> DefSort" }
    , { symbol = "liftDefExpr", signature = "Bool -> Expr -> Lift Expr" }
    , { symbol = "liftExpr", signature = "Bool" }
    , { symbol = "liftLocalFun", signature = "Expr -> Effect -> Lift Expr" }
    , { symbol = "makeDef", signature = "[TName] -> [TypeVar] -> ([ParamInfo], (TName, (Expr, String))) -> Lift (Expr, Def)" }
    , { symbol = "liftBranch", signature = "Branch -> Lift Branch" }
    , { symbol = "liftGuard", signature = "Guard -> Lift Guard" }
    , { symbol = "uniqueNameCurrentDef", signature = "Lift (Name,DefInline)" }
    , { symbol = "isSimpleFunc", signature = "Expr -> Bool" }
    , { symbol = "runLift", signature = "Pretty.Env -> Int -> Lift a -> (a,Int)" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Lift a -> Lift a" }
    , { symbol = "getEnv", signature = "Lift Env" }
    , { symbol = "updateSt", signature = "(State -> State) -> Lift State" }
    , { symbol = "collectLifted", signature = "Lift a -> Lift (a, DefGroups)" }
    , { symbol = "emitLifted", signature = "DefGroup -> Lift ()" }
    , { symbol = "emitLifteds", signature = "DefGroups -> Lift ()" }
    , { symbol = "withCurrentDef", signature = "Def -> Lift a -> Lift a" }
    , { symbol = "currentDefNames", signature = "Lift [Name]" }
    , { symbol = "traceDoc", signature = "(Pretty.Env -> Doc) -> Lift ()" }
    , { symbol = "liftTrace", signature = "String -> Lift ()" }
    , { symbol = "inlineDefs", signature = "Pretty.Env -> Int -> Inlines -> CorePhase b ()" }
    , { symbol = "inlDefGroups", signature = "DefGroups -> Inl DefGroups" }
    , { symbol = "inlLocalDefGroups", signature = "DefGroups -> Inl DefGroups" }
    , { symbol = "inlDef", signature = "Def -> Inl Def" }
    , { symbol = "inlExpr", signature = "Expr -> Inl Expr" }
    , { symbol = "inlAppExpr", signature = "Expr -> Int -> Int -> Bool -> Inl Expr" }
    , { symbol = "inlBranch", signature = "Branch -> Inl Branch" }
    , { symbol = "inlGuard", signature = "Guard -> Inl Guard" }
    , { symbol = "runInl", signature = "Pretty.Env -> Int -> Int -> Inlines -> Inl a -> (a,Int)" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Inl a -> Inl a" }
    , { symbol = "getEnv", signature = "Inl Env" }
    , { symbol = "updateSt", signature = "(State -> State) -> Inl State" }
    , { symbol = "withCurrentDef", signature = "Def -> Inl a -> Inl a" }
    , { symbol = "inlExtend", signature = "Bool -> [Def] -> Inl a -> Inl a" }
    , { symbol = "inlLookup", signature = "Name -> Inl (Maybe (InlineDef,Int,Int))" }
    , { symbol = "traceDoc", signature = "(Pretty.Env -> Doc) -> Inl ()" }
    , { symbol = "inlTrace", signature = "String -> Inl ()" }
    , { symbol = "verboseDoc", signature = "(Pretty.Env -> Doc) -> Inl ()" }
    , { symbol = "inlinesEmpty", signature = "Inlines" }
    , { symbol = "inlinesNew", signature = "[InlineDef] -> Inlines" }
    , { symbol = "inlinesExtends", signature = "[InlineDef] -> Inlines -> Inlines" }
    , { symbol = "inlinesExtend", signature = "InlineDef -> Inlines -> Inlines" }
    , { symbol = "inlinesLookup", signature = "Name -> Inlines -> Maybe InlineDef" }
    , { symbol = "inlinesToList", signature = "Inlines -> [InlineDef]" }
    , { symbol = "inlinesMerge", signature = "Inlines -> Inlines -> Inlines" }
    , { symbol = "inlinesFilter", signature = "(Name -> Bool) -> Inlines -> Inlines" }
    , { symbol = "extractInlineDefGroups", signature = "Int -> DefGroups -> [InlineDef]" }
    , { symbol = "extractDefGroup", signature = "Int -> DefGroup -> [InlineDef]" }
    , { symbol = "extractInlineDefs", signature = "Int -> Bool -> [Def] -> [InlineDef]" }
    , { symbol = "extractInlineDefRec", signature = "Int -> Def -> Maybe InlineDef" }
    , { symbol = "extractInlineDef", signature = "Int -> Bool -> Def -> Maybe InlineDef" }
    , { symbol = "ppInlines", signature = "Env -> Inlines -> Doc" }
    , { symbol = "monTransform", signature = "Pretty.Env -> CorePhase b ()" }
    , { symbol = "monDefGroups", signature = "DefGroups -> Mon DefGroups" }
    , { symbol = "monDef", signature = "Bool -> Def -> Mon Def" }
    , { symbol = "monExpr", signature = "Expr -> Mon (TransX Expr Expr)" }
    , { symbol = "monExpr'", signature = "Bool -> Expr -> Mon (TransX Expr Expr)" }
    , { symbol = "monBranch", signature = "Branch -> Mon Branch" }
    , { symbol = "monGuard", signature = "Guard -> Mon Guard" }
    , { symbol = "monLetGroups", signature = "DefGroups -> Expr -> Mon (TransX Expr Expr)" }
    , { symbol = "monLetGroup", signature = "DefGroup -> Mon (TransX [DefGroup] Expr)" }
    , { symbol = "monLetDef", signature = "Bool -> Def -> Mon (TransX ([Def],[Def],[Def]) Expr)" }
    , { symbol = "simplify", signature = "Expr -> Expr" }
    , { symbol = "monTrans", signature = "(a -> Mon (TransX b c)) -> [a] -> Mon (TransX [b] c)" }
    , { symbol = "applies", signature = "[Trans a] -> ([a] -> a) -> a" }
    , { symbol = "appBind", signature = "Type -> Effect ->  Type -> Expr -> [Expr] -> Expr -> Expr" }
    , { symbol = "monMakeBind", signature = "Type -> Effect -> Type -> Expr -> Expr -> Expr" }
    , { symbol = "typeBind", signature = "Type" }
    , { symbol = "typeYld", signature = "Type -> Type  -- Yld<a> == a" }
    , { symbol = "tvarA", signature = "TypeVar" }
    , { symbol = "tvarB", signature = "TypeVar" }
    , { symbol = "tvarE", signature = "TypeVar" }
    , { symbol = "isAlwaysMon", signature = "Expr -> Bool" }
    , { symbol = "isNeverMon", signature = "Expr -> Bool" }
    , { symbol = "isMonDef", signature = "Def -> Bool" }
    , { symbol = "isMonExpr", signature = "Expr -> Bool" }
    , { symbol = "runMon", signature = "Pretty.Env -> Int -> Mon a -> (a,Int)" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Mon a -> Mon a" }
    , { symbol = "getEnv", signature = "Mon Env" }
    , { symbol = "updateSt", signature = "(State -> State) -> Mon State" }
    , { symbol = "withCurrentDef", signature = "Def -> Mon a -> Mon a" }
    , { symbol = "withBindContext", signature = "Bool -> Mon a -> Mon a" }
    , { symbol = "isInBindContext", signature = "Mon Bool" }
    , { symbol = "monTraceDoc", signature = "(Pretty.Env -> Doc) -> Mon ()" }
    , { symbol = "monTrace", signature = "String -> Mon ()" }
    , { symbol = "monadicLift", signature = "Pretty.Env -> CorePhase b ()" }
    , { symbol = "liftDefGroups", signature = "Bool   -- top-level function?" }
    , { symbol = "liftDefGroup", signature = "Bool {-toplevel -} -> DefGroup -> Lift DefGroups" }
    , { symbol = "liftDef", signature = "Bool -> Def -> Lift Def" }
    , { symbol = "liftExprInl", signature = "Bool -> Expr -> Lift (Expr,Expr)" }
    , { symbol = "liftBranchInl", signature = "Branch -> Lift (Branch,Branch)" }
    , { symbol = "liftGuardInl", signature = "Guard -> Lift (Guard,Guard)" }
    , { symbol = "liftExpr", signature = "Bool -> Expr -> Lift Expr" }
    , { symbol = "liftLocalFun", signature = "Expr -> Effect -> Lift Expr" }
    , { symbol = "makeDef", signature = "[TName] -> [TypeVar] -> Expr -> Lift (Expr, Def)" }
    , { symbol = "liftBranch", signature = "Branch -> Lift Branch" }
    , { symbol = "liftGuard", signature = "Guard -> Lift Guard" }
    , { symbol = "uniqueNameCurrentDef", signature = "Lift (Name,DefInline)" }
    , { symbol = "runLift", signature = "Pretty.Env -> Int -> Lift a -> (a,Int)" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Lift a -> Lift a" }
    , { symbol = "getEnv", signature = "Lift Env" }
    , { symbol = "updateSt", signature = "(State -> State) -> Lift State" }
    , { symbol = "collectLifted", signature = "Lift a -> Lift (a, DefGroups)" }
    , { symbol = "emitLifted", signature = "DefGroup -> Lift ()" }
    , { symbol = "emitLifteds", signature = "DefGroups -> Lift ()" }
    , { symbol = "withCurrentDef", signature = "Def -> Lift a -> Lift a" }
    , { symbol = "traceDoc", signature = "(Pretty.Env -> Doc) -> Lift ()" }
    , { symbol = "liftTrace", signature = "String -> Lift ()" }
    , { symbol = "openResolve", signature = "Pretty.Env -> Gamma -> CorePhase b ()" }
    , { symbol = "resDefGroups", signature = "Env -> DefGroups -> DefGroups" }
    , { symbol = "resDef", signature = "Env -> Def -> Def" }
    , { symbol = "resExpr", signature = "Env -> Expr -> Expr" }
    , { symbol = "resBranch", signature = "Env -> Branch -> Branch" }
    , { symbol = "resGuard", signature = "Env -> Guard -> Guard" }
    , { symbol = "resOpen", signature = "Env -> Expr -> Effect -> Effect -> Type -> Type -> Expr -> Expr" }
    , { symbol = "addLevels", signature = "[Tau] -> [(Tau,Integer)]" }
    , { symbol = "addLevelsPrev", signature = "(Name,Integer) -> [Tau] -> [(Tau,Integer)]" }
    , { symbol = "isHandlerFree", signature = "Expr -> Bool" }
    , { symbol = "handlerFreeFunType", signature = "Type -> Maybe Bool" }
    , { symbol = "parseCore", signature = "HasCallStack => FilePath -> FilePath -> IO (Error b (Core, ParseInlines))" }
    , { symbol = "parseInlines", signature = "Core -> Source -> Env -> [Lexeme] -> ParseInlines" }
    , { symbol = "pInlines", signature = "Env -> LexParser [InlineDef]" }
    , { symbol = "program", signature = "Int -> FilePath -> Source -> LexParser (Core,ParseInlines)" }
    , { symbol = "pmodule", signature = "Int -> FilePath -> LexParser (Core,Env,[Lexeme])" }
    , { symbol = "localAlias", signature = "Env -> LexParser (SynInfo, Env)" }
    , { symbol = "semisEnv", signature = "Env -> (Env -> LexParser (a,Env)) -> LexParser ([a],Env)" }
    , { symbol = "vispub", signature = "LexParser Visibility" }
    , { symbol = "importDecl", signature = "HasCallStack => LexParser (Import,(Name,Name))" }
    , { symbol = "pimportProvenance", signature = "LexParser ImportProvenance" }
    , { symbol = "fixDecl", signature = "LexParser [FixDef]" }
    , { symbol = "pfixity", signature = "LexParser Fixity" }
    , { symbol = "typeDecl", signature = "Env -> LexParser (TypeDef,Env)" }
    , { symbol = "typeSort", signature = "LexParser (DataDef, Bool, DataEffect, DataKind,String)" }
    , { symbol = "parseTypeMod", signature = "LexParser (DataDef,DataKind,DataEffect)" }
    , { symbol = "parseDataEffect", signature = "LexParser DataEffect" }
    , { symbol = "parseValueRepr", signature = "LexParser ValueRepr" }
    , { symbol = "defDecl", signature = "Env -> LexParser Def" }
    , { symbol = "externDecl", signature = "Env -> LexParser External" }
    , { symbol = "externalBody", signature = "LexParser [(Target,String)]" }
    , { symbol = "externImportDecl", signature = "LexParser External" }
    , { symbol = "externalImportBody", signature = "LexParser [(Target, [(String,String)])]" }
    , { symbol = "inlineDef", signature = "Env -> LexParser InlineDef" }
    , { symbol = "parseExpr", signature = "Env -> LexParser Expr" }
    , { symbol = "parseLet", signature = "Env -> LexParser Expr" }
    , { symbol = "parseForall", signature = "Env -> LexParser Expr" }
    , { symbol = "parseFun", signature = "Env -> LexParser Expr" }
    , { symbol = "parseMatch", signature = "Env -> LexParser Expr" }
    , { symbol = "parseCon", signature = "Env -> LexParser Expr" }
    , { symbol = "parseVar", signature = "Env -> LexParser Expr" }
    , { symbol = "parseLit", signature = "LexParser Lit" }
    , { symbol = "parseDefGroups", signature = "Env -> LexParser (Env,[DefGroup])" }
    , { symbol = "parseDefGroup", signature = "Env -> LexParser (Env,DefGroup)" }
    , { symbol = "prange", signature = "Env -> LexParser Range" }
    , { symbol = "parseBranch", signature = "Env -> LexParser Branch" }
    , { symbol = "parsePatterns1", signature = "Env -> LexParser (Env, [Pattern])" }
    , { symbol = "parseGuard", signature = "Env -> LexParser Guard" }
    , { symbol = "parsePattern", signature = "Env -> LexParser (Env,Pattern)" }
    , { symbol = "parsePatternBasic", signature = "Env -> LexParser (Env,Pattern)" }
    , { symbol = "parsePatCon", signature = "Env -> LexParser (Env,Pattern)" }
    , { symbol = "parsePatternArgs0", signature = "Env -> LexParser (Env,[(Pattern,Type)])" }
    , { symbol = "parsePatternArgs1", signature = "Env -> LexParser (Env,[(Pattern,Type)])" }
    , { symbol = "parsePatternArg", signature = "Env -> LexParser (Env,(Pattern,Type))" }
    , { symbol = "parsePatVar", signature = "Env -> Pattern -> LexParser (Env,Pattern)" }
    , { symbol = "qfunid", signature = "LexParser (Name,Range)" }
    , { symbol = "parameters", signature = "Env -> LexParser (Env, [(Name,Type)])" }
    , { symbol = "parameter", signature = "Env -> Bool -> LexParser ((Name,Type),ParamInfo)" }
    , { symbol = "parameterName", signature = "LexParser Name" }
    , { symbol = "parameterId", signature = "LexParser Name" }
    , { symbol = "typeAnnot", signature = "Env -> LexParser Type" }
    , { symbol = "typeAnnotFull", signature = "Env -> LexParser Type" }
    , { symbol = "tbinders", signature = "Env -> LexParser (Env,[TypeVar])" }
    , { symbol = "tbinder", signature = "LexParser (Name,Kind)" }
    , { symbol = "kindAnnotFull", signature = "LexParser Kind" }
    , { symbol = "ptype", signature = "Env -> LexParser Type" }
    , { symbol = "pdeftype", signature = "Env -> LexParser (Type, [ParamInfo])" }
    , { symbol = "ptypex", signature = "Env -> Bool -> LexParser (Type, [ParamInfo])" }
    , { symbol = "pforall", signature = "Env -> LexParser (Rho -> Type, Env)" }
    , { symbol = "tarrow", signature = "Env -> Bool -> LexParser (Type, [ParamInfo])" }
    , { symbol = "tresult", signature = "Env -> [(Name,Type)] -> LexParser Type" }
    , { symbol = "tatom", signature = "Env -> LexParser Type" }
    , { symbol = "tatomParams", signature = "Env -> Bool -> LexParser (Either ([(Name,Type)], [ParamInfo]) Type)" }
    , { symbol = "tid", signature = "Env -> LexParser Type" }
    , { symbol = "pkind", signature = "LexParser Kind" }
    , { symbol = "envRange", signature = "Env -> Int -> Int -> Int -> Int -> Range" }
    , { symbol = "envInitial", signature = "Name -> FilePath -> ImportMap -> Int -> Env" }
    , { symbol = "envExtend", signature = "Env -> (Name,Kind) -> Env" }
    , { symbol = "envType", signature = "Env -> Name -> Kind -> Type" }
    , { symbol = "envQualify", signature = "Env -> Name -> Name" }
    , { symbol = "envExtendSynonym", signature = "Env -> SynInfo -> Env" }
    , { symbol = "envTypeApp", signature = "Env -> Type -> [Type] -> Type" }
    , { symbol = "envExtendLocal", signature = "Env -> (Name,Type) -> Env" }
    , { symbol = "envLookupLocal", signature = "Env -> Name -> LexParser Type" }
    , { symbol = "envLookupCon", signature = "Env -> Name -> LexParser NameInfo" }
    , { symbol = "envLookupVar", signature = "Env -> Name -> LexParser Expr" }
    , { symbol = "prettyNames", signature = "Bool" }
    , { symbol = "prettyCore", signature = "Env -> Target -> [InlineDef] -> Core -> Doc" }
    , { symbol = "ppImportProvenance", signature = "Env -> ImportProvenance -> Doc" }
    , { symbol = "prettyImportedSyn", signature = "Env -> SynInfo -> Doc" }
    , { symbol = "prettyExternal", signature = "Env -> External -> Doc" }
    , { symbol = "prettyTypeDefGroup", signature = "Env -> TypeDefGroup -> Doc" }
    , { symbol = "prettyTypeDef", signature = "Env -> TypeDef -> Doc" }
    , { symbol = "prettyDefGroup", signature = "Env -> DefGroup -> Doc" }
    , { symbol = "prettyDefs", signature = "Env -> Defs -> Doc" }
    , { symbol = "prettyInlineDefGroup", signature = "Env -> DefGroup -> Doc" }
    , { symbol = "prettyInlineDef", signature = "Env -> Bool -> Def -> Doc" }
    , { symbol = "prettyInlineDef", signature = "Env ->  InlineDef -> Doc" }
    , { symbol = "prettyDef", signature = "Env-> Def -> Doc" }
    , { symbol = "prettyExpr", signature = "Env -> Expr -> Doc" }
    , { symbol = "prettyBranches", signature = "Env -> [Branch] -> Doc" }
    , { symbol = "prettyBranch", signature = "Env -> Branch -> Doc" }
    , { symbol = "prettyGuard", signature = "Env -> Guard -> Doc" }
    , { symbol = "prettyPatterns", signature = "Env -> [Pattern] -> (Env,[Doc])" }
    , { symbol = "prettyPattern", signature = "Env -> Pattern -> (Env,Doc)" }
    , { symbol = "prettyLit", signature = "Env -> Lit -> Doc" }
    , { symbol = "prettyTName", signature = "Env -> TName -> Doc" }
    , { symbol = "prettyModuleName", signature = "Env -> Name -> Doc" }
    , { symbol = "prettyDefName", signature = "Env -> Name -> Doc" }
    , { symbol = "prettyTypeVar", signature = "Env -> TypeVar -> Doc" }
    , { symbol = "decPrec", signature = "Env -> Env" }
    , { symbol = "extendImportMap", signature = "[Import] -> ImportMap -> ImportMap" }
    , { symbol = "extractImportFromSignatures", signature = "Signatures -> [Import]" }
    , { symbol = "extractDepsFromSignatures", signature = "Signatures -> [ModuleName]" }
    , { symbol = "extractImportsFromSynInfo", signature = "SynInfo -> Import" }
    , { symbol = "extractDepsFromSynonyms", signature = "Synonyms -> [ModuleName]" }
    , { symbol = "extractImportsFromSynonyms", signature = "[Import] -> [SynInfo] -> [Import]" }
    , { symbol = "extractImportedSynonyms", signature = "ModuleName -> Signatures -> [SynInfo]" }
    , { symbol = "simplifyDefs", signature = "HasCallStack => Pretty.Env -> Bool -> Bool -> Int -> Int -> CorePhase b ()" }
    , { symbol = "uniqueSimplify", signature = "HasCallStack => (HasUnique m, Simplify a) => Pretty.Env -> Bool -> Bool -> Int -> Int -> a -> m a" }
    , { symbol = "uniquefyDefBodies", signature = "HasCallStack => [DefGroup] -> [DefGroup]" }
    , { symbol = "uniquefyDefGroupBody", signature = "HasCallStack => DefGroup -> DefGroup" }
    , { symbol = "topDown", signature = "HasCallStack => Expr -> Simp Expr" }
    , { symbol = "bindExprs", signature = "[Expr] -> Simp (Expr -> Expr, [Expr])" }
    , { symbol = "bottomUp", signature = "Expr -> Expr" }
    , { symbol = "matchFirst", signature = "[Match a] -> Match a" }
    , { symbol = "matchAll", signature = "[Match a] -> Match [a]" }
    , { symbol = "kmatchBranches", signature = "[Expr] -> [Branch] -> Maybe Expr" }
    , { symbol = "kmatchBranch", signature = "[Expr] -> Branch -> Match Expr" }
    , { symbol = "kmatchPatterns", signature = "[Expr] -> [Pattern] -> Match Defs" }
    , { symbol = "kmatchPattern", signature = "Expr -> Pattern -> Match (Defs, Expr)" }
    , { symbol = "effectLabelFromHandler", signature = "Type -> Name" }
    , { symbol = "effectOffset", signature = "Name -> Type -> Integer" }
    , { symbol = "findMatch", signature = "Integer -> Name -> [Type] -> Integer" }
    , { symbol = "sizeOfExpr", signature = "Expr -> Int" }
    , { symbol = "sizeOfFun", signature = "Expr -> Int   -- for functions we must be conservative or we could lose sharing from allocations" }
    , { symbol = "occursNot", signature = "Name -> Expr -> Bool" }
    , { symbol = "occursAtMostOnce", signature = "Name -> Expr -> Bool" }
    , { symbol = "occursAtMostOnceAndApplied", signature = "Name -> Int -> Int -> Expr -> Bool" }
    , { symbol = "occursOnceApplied", signature = "Name -> Int -> Int -> Expr -> Maybe Int" }
    , { symbol = "occurrencesOf", signature = "Name -> Expr -> Occur" }
    , { symbol = "occurrences", signature = "Expr -> M.NameMap Occur" }
    , { symbol = "occurrencesBranch", signature = "Branch -> M.NameMap Occur" }
    , { symbol = "ounion", signature = "M.NameMap Occur -> M.NameMap Occur -> M.NameMap Occur" }
    , { symbol = "ounions", signature = "[M.NameMap Occur] -> M.NameMap Occur" }
    , { symbol = "occurrencesDefGroup", signature = "DefGroup -> M.NameMap Occur -> M.NameMap Occur" }
    , { symbol = "runSimplify", signature = "Bool -> Bool -> Int -> Int -> Pretty.Env -> Simp a -> (a,Int)" }
    , { symbol = "getEnv", signature = "Simp SEnv" }
    , { symbol = "withEnv", signature = "SEnv -> Simp a -> Simp a" }
    , { symbol = "updateEnv", signature = "(SEnv -> SEnv) -> Simp a -> Simp a" }
    , { symbol = "getUnsafe", signature = "Simp Bool" }
    , { symbol = "getDuplicationMax", signature = "Simp Int" }
    , { symbol = "getNDebug", signature = "Simp Bool" }
    , { symbol = "inDef", signature = "Name -> Simp a -> Simp a" }
    , { symbol = "traceS", signature = "String -> Simp ()" }
    , { symbol = "runSpecM", signature = "Int -> ReadState -> SpecM a -> (a, Int)" }
    , { symbol = "specialize", signature = "Inlines -> Env -> CorePhase b ()" }
    , { symbol = "speclookup", signature = "Name -> SpecM (Maybe InlineDef)" }
    , { symbol = "specOneDefGroup", signature = "DefGroup -> SpecM DefGroup" }
    , { symbol = "specOneDef", signature = "Def -> SpecM Def" }
    , { symbol = "specOneExpr", signature = "Name -> Expr -> SpecM Expr" }
    , { symbol = "filterBools", signature = "[Bool] -> [a] -> [a]" }
    , { symbol = "partitionBools", signature = "[Bool] -> [a] -> ([a], [a])" }
    , { symbol = "specOneCall", signature = "InlineDef -> Expr -> SpecM Expr" }
    , { symbol = "goodArgs", signature = "[Bool] -> [Expr] -> [Maybe Expr]" }
    , { symbol = "goodArg", signature = "Expr -> Maybe Expr" }
    , { symbol = "specInnerCalls", signature = "TName -> TName -> [Bool] -> Expr -> Expr" }
    , { symbol = "specInnerCalls", signature = "TName -> TName -> [Bool] -> [TName] -> Expr -> Expr" }
    , { symbol = "comment", signature = "String -> String" }
    , { symbol = "replaceCall", signature = "Name -> Expr -> DefSort -> [Bool] -> [Expr] -> Maybe [Type] -> SpecM Expr" }
    , { symbol = "fnTypeParams", signature = "Expr -> [TypeVar]" }
    , { symbol = "fnParams", signature = "Expr -> [TName]" }
    , { symbol = "fnEffect", signature = "Expr -> Effect" }
    , { symbol = "fnBody", signature = "Expr -> Expr" }
    , { symbol = "extractSpecializeDefs", signature = "Inlines -> DefGroups -> [InlineDef]" }
    , { symbol = "filterMaybe", signature = "(a -> Bool) -> Maybe a -> Maybe a" }
    , { symbol = "whenJust", signature = "(Applicative f) => Maybe a -> (a -> f ()) -> f ()" }
    , { symbol = "makeSpecialize", signature = "Def -> Maybe InlineDef" }
    , { symbol = "allPassedInSameOrder", signature = "[TName] -> [[Expr]] -> [Maybe TName]" }
    , { symbol = "passedInSameOrder", signature = "[TName] -> [Expr] -> [Maybe TName]" }
    , { symbol = "isFun", signature = "Type -> Bool" }
    , { symbol = "usedInThisDef", signature = "Def -> S.NameSet" }
    , { symbol = "allEq", signature = "HasCallStack => [[Type]] -> Bool" }
    , { symbol = "recursiveCalls", signature = "Def -> (Maybe [[Type]], [[Expr]])" }
    , { symbol = "multiStepInlines", signature = "Inlines -> Inlines -> [Def] -> Inlines" }
    , { symbol = "vars", signature = "Expr -> [TName]" }
    , { symbol = "unreturn", signature = "Pretty.Env -> CorePhase b ()" }
    , { symbol = "urTopDefGroups", signature = "DefGroups -> UR (DefGroups)" }
    , { symbol = "urTopDefGroup", signature = "DefGroup -> UR DefGroup" }
    , { symbol = "urTopDef", signature = "Def -> UR Def" }
    , { symbol = "urDef", signature = "Def -> UR (Expr -> Def, KExpr)" }
    , { symbol = "urExpr", signature = "Expr -> UR KExpr" }
    , { symbol = "urPure", signature = "Expr -> UR Expr" }
    , { symbol = "urLet", signature = "Expr -> [DefGroup] -> KExpr -> UR KExpr" }
    , { symbol = "urCase", signature = "Expr -> [Expr] -> [Branch] -> UR KExpr" }
    , { symbol = "applyK", signature = "(Expr -> Expr) -> KExpr -> Expr" }
    , { symbol = "emapUnK", signature = "Expr -> (Expr -> Expr) -> KExpr -> KExpr" }
    , { symbol = "emapK", signature = "Maybe Expr -> (Expr -> Expr) -> KExpr -> KExpr" }
    , { symbol = "bind", signature = "Maybe Expr -> (Expr -> Expr -> Expr) -> KExpr -> KExpr -> KExpr" }
    , { symbol = "runUR", signature = "Pretty.Env -> Int -> UR a -> (a, Int)" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> UR a -> UR a" }
    , { symbol = "getEnv", signature = "UR Env" }
    , { symbol = "updateSt", signature = "(State -> State) -> UR State" }
    , { symbol = "withCurrentDef", signature = "Def -> UR a -> UR a" }
    , { symbol = "withEffect", signature = "Effect -> UR a -> UR a" }
    , { symbol = "getCurrentEffect", signature = "UR Effect" }
    , { symbol = "urTraceDoc", signature = "(Pretty.Env -> Doc) -> UR ()" }
    , { symbol = "urTrace", signature = "String -> UR ()" }
    , { symbol = "uniquefyExpr", signature = "Expr -> Expr" }
    , { symbol = "uniquefyExprWith", signature = "TNames -> Expr -> Expr" }
    , { symbol = "uniquefyExprU", signature = "HasUnique m => Expr -> m Expr" }
    , { symbol = "uniquefy", signature = "Core -> Core" }
    , { symbol = "uniquefyDefGroups", signature = "[DefGroup] -> [DefGroup]" }
    , { symbol = "uniquefyDefGroup", signature = "DefGroup -> DefGroup" }
    , { symbol = "uniquefyInnerDefGroup", signature = "DefGroup -> Un DefGroup" }
    , { symbol = "uniquefyRecDefs", signature = "[Def] -> Un [Def]" }
    , { symbol = "uniquefyDef", signature = "Def -> Un Def" }
    , { symbol = "uniquefyExprX", signature = "Expr -> Un Expr" }
    , { symbol = "uniquefyTName", signature = "TName -> Un TName" }
    , { symbol = "uniquefyName", signature = "Name -> Un Name" }
    , { symbol = "unrollDefs", signature = "Pretty.Env -> Int -> CorePhase b ()" }
    , { symbol = "unrollDefGroups", signature = "DefGroups -> Unroll DefGroups" }
    , { symbol = "unrollDefGroup", signature = "DefGroup -> Unroll DefGroups" }
    , { symbol = "unrollRecDef", signature = "Def -> Unroll [DefGroup]" }
    , { symbol = "unrollBody", signature = "Def -> [TypeVar] -> [TName] -> Effect -> Expr -> Unroll [DefGroup]" }
    , { symbol = "extractNonRecBranches", signature = "TName -> [Branch] -> [Branch] -> ([Branch],[Branch])" }
    , { symbol = "singletonPat", signature = "Pattern -> Bool" }
    , { symbol = "dontSkip", signature = "Branch -> Branch" }
    , { symbol = "runUnroll", signature = "Pretty.Env -> Int -> Int -> Unroll a -> (a,Int)" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Unroll a -> Unroll a" }
    , { symbol = "getEnv", signature = "Unroll Env" }
    , { symbol = "updateSt", signature = "(State -> State) -> Unroll State" }
    , { symbol = "withCurrentDef", signature = "Def -> Unroll a -> Unroll a" }
    , { symbol = "traceDoc", signature = "(Pretty.Env -> Doc) -> Unroll ()" }
    , { symbol = "unrollTrace", signature = "String -> Unroll ()" }
    , { symbol = "uniqueTNameFrom", signature = "TName -> Unroll TName" }
    , { symbol = "verboseDoc", signature = "(Pretty.Env -> Doc) -> Unroll ()" }
    , { symbol = "readCommand", signature = "String -> Command" }
    , { symbol = "parseCommand", signature = "String -> Command" }
    , { symbol = "command", signature = "Parser Command" }
    , { symbol = "commandHelp", signature = "ColorScheme -> Doc" }
    , { symbol = "expression", signature = "Parser Command" }
    , { symbol = "expr", signature = "Parser String" }
    , { symbol = "shellCommand", signature = "Parser String" }
    , { symbol = "commandLine", signature = "Parser String" }
    , { symbol = "anything", signature = "Parser String" }
    , { symbol = "identifier", signature = "Parser Name" }
    , { symbol = "nat", signature = "Parser Int" }
    , { symbol = "filenames", signature = "Parser [String]" }
    , { symbol = "filename", signature = "Parser String" }
    , { symbol = "special", signature = "String -> Parser ()" }
    , { symbol = "symbol", signature = "String -> Parser ()" }
    , { symbol = "istring", signature = "String -> Parser ()" }
    , { symbol = "oneLineComment", signature = "Parser ()" }
    , { symbol = "idchars", signature = "Parser String" }
    , { symbol = "idchar", signature = "Parser String" }
    , { symbol = "interpret", signature = "ColorPrinter -> Flags -> [FilePath] -> IO ()" }
    , { symbol = "interpreterEx", signature = "Maybe Command -> State -> B.BuildContext -> IO ()" }
    , { symbol = "command", signature = "State -> B.BuildContext -> Command -> IO (Maybe (State,B.BuildContext))" }
    , { symbol = "loadModules", signature = "State -> B.BuildContext -> [FilePath] -> Bool -> Bool -> IO (B.BuildContext, Maybe Range)" }
    , { symbol = "loadModulesEx", signature = "Terminal -> Flags -> B.BuildContext -> [FilePath] -> Bool -> Bool -> IO (Maybe B.BuildContext,Maybe Range)" }
    , { symbol = "buildRunExpr", signature = "State -> B.BuildContext -> String -> IO (State,B.BuildContext)" }
    , { symbol = "buildTypeExpr", signature = "State -> B.BuildContext -> String -> IO (Maybe Type,State,B.BuildContext)" }
    , { symbol = "errorFileNotFound", signature = "Flags -> FilePath -> ErrorMessage" }
    , { symbol = "maybeMessageMarker", signature = "State -> Range -> IO ()" }
    , { symbol = "messageScheme", signature = "State -> Scheme -> IO ()" }
    , { symbol = "messageSchemeEffect", signature = "State -> Scheme -> IO ()" }
    , { symbol = "lastSourceFull", signature = "HasCallStack => State -> IO Source" }
    , { symbol = "mainModuleName", signature = "B.BuildContext -> ModuleName" }
    , { symbol = "getImportMap", signature = "State -> ImportMap" }
    , { symbol = "showCommand", signature = "State -> B.BuildContext -> ShowCommand -> IO ()" }
    , { symbol = "runEditor", signature = "State -> FilePath -> IO ()" }
    , { symbol = "runEditorAt", signature = "State -> FilePath -> Int -> Int -> IO ()" }
    , { symbol = "replace", signature = "Int -> Int -> FilePath -> String -> String" }
    , { symbol = "getCommand", signature = "State -> B.BuildContext -> IO Command" }
    , { symbol = "prompt", signature = "State -> IO ()" }
    , { symbol = "remark", signature = "State -> String -> IO ()" }
    , { symbol = "terminal", signature = "State -> Terminal" }
    , { symbol = "messageErrorMsgLn", signature = "State -> [ErrorMessage] -> IO ()" }
    , { symbol = "messageErrorMsgLnLn", signature = "State -> [ErrorMessage] -> IO ()" }
    , { symbol = "messageError", signature = "State -> String -> IO ()" }
    , { symbol = "messageInfoLnLn", signature = "State -> String -> IO ()" }
    , { symbol = "messageInfoLn", signature = "State -> String -> IO ()" }
    , { symbol = "messageInfo", signature = "State -> String -> IO ()" }
    , { symbol = "messagePrettyLnLn", signature = "State -> Doc -> IO ()" }
    , { symbol = "messagePrettyLn", signature = "State -> Doc -> IO ()" }
    , { symbol = "messagePretty", signature = "State -> Doc -> IO ()" }
    , { symbol = "messageLnLn", signature = "State -> String -> IO ()" }
    , { symbol = "messageLn", signature = "State -> String -> IO ()" }
    , { symbol = "message", signature = "State -> String -> IO ()" }
    , { symbol = "messageMarker", signature = "State -> Range -> IO ()" }
    , { symbol = "putQuote", signature = "State -> IO ()" }
    , { symbol = "quotes", signature = "[(String,String)]" }
    , { symbol = "kgammaInit", signature = "KGamma" }
    , { symbol = "kgammaIsEmpty", signature = "KGamma -> Bool" }
    , { symbol = "kgammaEmpty", signature = "KGamma" }
    , { symbol = "kgammaSingle", signature = "Name -> Kind -> String -> KGamma" }
    , { symbol = "kgammaNew", signature = "[(Name,TypeNameInfo)] -> KGamma" }
    , { symbol = "kgammaNewNub", signature = "[(Name,Kind,String)] -> KGamma" }
    , { symbol = "kgammaExtend", signature = "Name -> Kind -> String -> KGamma -> KGamma" }
    , { symbol = "kgammaLookup", signature = "Name -> Name -> KGamma -> Lookup (Kind,String)" }
    , { symbol = "kgammaLookupQ", signature = "Name -> KGamma -> Maybe TypeNameInfo" }
    , { symbol = "kgammaFind", signature = "Name -> Name -> KGamma -> (Name,Kind,String)" }
    , { symbol = "kgammaList", signature = "KGamma -> [(Name,Kind)]" }
    , { symbol = "kgammaFilter", signature = "Name -> KGamma -> KGamma" }
    , { symbol = "kgammaUnion", signature = "HasCallStack => KGamma -> KGamma -> KGamma" }
    , { symbol = "kgammaUnionLeftBias", signature = "HasCallStack => KGamma -> KGamma -> KGamma" }
    , { symbol = "kgammaUnions", signature = "HasCallStack => [KGamma] -> KGamma" }
    , { symbol = "ppKGamma", signature = "ColorScheme -> Name -> ImportMap -> KGamma -> Doc" }
    , { symbol = "extractKGamma", signature = "Core.Core -> KGamma" }
    , { symbol = "extractTypeDef", signature = "Core.TypeDef -> KGamma" }
    , { symbol = "constructorsEmpty", signature = "Constructors" }
    , { symbol = "constructorsIsEmpty", signature = "Constructors -> Bool" }
    , { symbol = "constructorsExtend", signature = "Name -> ConInfo -> Constructors -> Constructors" }
    , { symbol = "constructorsFromList", signature = "[ConInfo] -> Constructors" }
    , { symbol = "constructorsCompose", signature = "Constructors -> Constructors -> Constructors" }
    , { symbol = "constructorsLookup", signature = "Name -> Constructors -> Maybe ConInfo" }
    , { symbol = "constructorsFind", signature = "Name -> Constructors -> ConInfo" }
    , { symbol = "constructorsFindScheme", signature = "Name -> Constructors -> Scheme" }
    , { symbol = "constructorsSet", signature = "Constructors -> S.NameSet" }
    , { symbol = "constructorsList", signature = "Constructors -> [(Name, ConInfo)]" }
    , { symbol = "extractConstructors", signature = "Core.Core -> Constructors" }
    , { symbol = "extractTypeDef", signature = "(Visibility -> Bool) -> Core.TypeDef -> M.NameMap ConInfo" }
    , { symbol = "importsEmpty", signature = "ImportMap" }
    , { symbol = "importsExtend", signature = "Name -> Name -> ImportMap -> Maybe ImportMap" }
    , { symbol = "importsAlias", signature = "Name -> ImportMap -> Name" }
    , { symbol = "importsList", signature = "ImportMap -> [(Name,Name)]" }
    , { symbol = "importsExpand", signature = "Name -> ImportMap -> Either [Name] (Name,Name)" }
    , { symbol = "importResolvePath", signature = "Name -> [String] -> ImportMap -> Either [Name] (Name,Name)" }
    , { symbol = "importsExpand", signature = "Name -> ImportMap -> Either [Name] (Name,Name)" }
    , { symbol = "unzipEither", signature = "[Either a b] -> ([a],[b])" }
    , { symbol = "extractInfos", signature = "[Core.TypeDefGroup] -> [Either SynInfo DataInfo]" }
    , { symbol = "synTypeDefGroups", signature = "Name -> [LazyExpr] -> [Core.TypeDefGroup] -> KInfer (DefGroups Type)" }
    , { symbol = "synTypeDefGroup", signature = "Name -> [LazyExpr] -> Core.TypeDefGroup -> KInfer (DefGroups Type)" }
    , { symbol = "synTypeDef", signature = "Name -> [LazyExpr] -> Core.TypeDef -> KInfer (DefGroups Type)" }
    , { symbol = "synCopyCon", signature = "Name -> DataInfo -> ConInfo -> DefGroup Type" }
    , { symbol = "hasAccessor", signature = "Name -> Type -> ConInfo -> Bool" }
    , { symbol = "synAccessors", signature = "Name -> DataInfo -> [DefGroup Type]" }
    , { symbol = "synTester", signature = "DataInfo -> ConInfo -> [DefGroup Type]" }
    , { symbol = "synConstrTag", signature = "(ConInfo) -> DefGroup Type" }
    , { symbol = "synLazyTag", signature = "DataInfo -> DefGroup Type" }
    , { symbol = "synLazyForce", signature = "DataInfo -> DefGroup Type" }
    , { symbol = "synLazyWhnf", signature = "DataInfo -> DefGroup Type" }
    , { symbol = "synLazyStep", signature = "DataInfo -> DefGroup Type" }
    , { symbol = "synLazyEval", signature = "DataInfo -> DefGroup Type" }
    , { symbol = "synLazyEvalLocked", signature = "[LazyExpr] -> DataInfo -> KInfer [DefGroup Type]" }
    , { symbol = "lazyConDefCall", signature = "DataInfo -> ConInfo -> [Name] -> Expr Type -> Expr Type -> Expr Type -> KInfer (Def Type,Expr Type)" }
    , { symbol = "lazyAddUpdate", signature = "DataInfo -> ConInfo -> Name -> Expr t -> Expr t -> KInfer (Expr t)" }
    , { symbol = "lazyFip", signature = "HasCallStack => DataInfo -> Fip" }
    , { symbol = "constructorGamma", signature = "(DataInfo -> Bool) -> [DataInfo] -> Gamma" }
    , { symbol = "constructorCheckDuplicates", signature = "ColorScheme -> [ConInfo] -> [(Range,Doc)]" }
    , { symbol = "infImports", signature = "Name -> Range -> [Import] -> KInfer ()" }
    , { symbol = "infImport", signature = "Import -> KInfer ()" }
    , { symbol = "infTypeDefGroups", signature = "[TypeDefGroup UserType UserKind] -> KInfer ([Core.TypeDefGroup],KGamma,Synonyms,Newtypes,[LazyExpr])" }
    , { symbol = "infTypeDefGroup", signature = "TypeDefGroup UserType UserKind -> KInfer (Core.TypeDefGroup,[LazyExpr])" }
    , { symbol = "infTypeDefs", signature = "Bool -> [TypeDef UserType UserType UserKind] -> KInfer (Core.TypeDefGroup,[LazyExpr])" }
    , { symbol = "checkRecursion", signature = "[TypeDef UserType UserType UserKind] -> KInfer ()" }
    , { symbol = "bindTypeDef", signature = "TypeDef UserType UserType UserKind -> KInfer (TypeBinder InfKind,Bool {-not isExtend-})" }
    , { symbol = "bindTypeBinder", signature = "TypeBinder UserKind -> KInfer (TypeBinder InfKind)" }
    , { symbol = "userKindToKind", signature = "UserKind -> KInfer InfKind" }
    , { symbol = "infExternals", signature = "[External] -> KInfer Core.Externals" }
    , { symbol = "infExternal", signature = "[Name] -> External -> KInfer (Core.External,[Name])" }
    , { symbol = "infResolveType", signature = "UserType -> Context -> KInfer Type" }
    , { symbol = "infResolveHX", signature = "UserType -> Context -> KInfer Type" }
    , { symbol = "infResolveX", signature = "UserType -> Context -> Range -> KInfer Type" }
    , { symbol = "infDefGroups", signature = "[DefGroup UserType] -> KInfer [DefGroup Type]" }
    , { symbol = "infDefGroup", signature = "DefGroup UserType -> KInfer (DefGroup Type)" }
    , { symbol = "infDef", signature = "(Def UserType) -> KInfer (Def Type)" }
    , { symbol = "infExpr", signature = "Expr UserType -> KInfer (Expr Type)" }
    , { symbol = "infTypeDef", signature = "(TypeBinder InfKind, TypeDef UserType UserType UserKind) -> KInfer (TypeDef (KUserType InfKind) UserType InfKind)" }
    , { symbol = "infConstructor", signature = "UserCon UserType UserType UserKind -> KInfer (UserCon (KUserType InfKind) UserType InfKind)" }
    , { symbol = "infConValueBinder", signature = "(Visibility,ValueBinder UserType (Maybe (Expr UserType))) -> KInfer (Visibility,ValueBinder (KUserType InfKind) (Maybe (Expr UserType)))" }
    , { symbol = "infUserType", signature = "InfKind -> Context -> UserType -> KInfer (KUserType InfKind)" }
    , { symbol = "getEffectLift", signature = "KUserType InfKind -> KInfer (KUserType k -> Range -> KUserType k)" }
    , { symbol = "userConIsLazy", signature = "UserCon t u k -> Bool" }
    , { symbol = "resolveTypeDef", signature = "Bool -> [Name] -> TypeDef (KUserType InfKind) UserType InfKind -> KInfer (Core.TypeDef,[LazyExpr])" }
    , { symbol = "occursNegativeCon", signature = "[Name] -> ConInfo -> Bool" }
    , { symbol = "occursNegative", signature = "[Name] -> Type -> Bool" }
    , { symbol = "occurs", signature = "[Name] -> Bool -> Type -> Bool" }
    , { symbol = "showTypeBinder", signature = "TypeBinder Kind -> String" }
    , { symbol = "resolveTypeBinderDef", signature = "String -> TypeBinder InfKind -> KInfer (TypeBinder Kind)" }
    , { symbol = "resolveTypeBinder", signature = "String -> TypeBinder InfKind -> KInfer (TypeBinder Kind)" }
    , { symbol = "resolveKind", signature = "InfKind -> KInfer Kind" }
    , { symbol = "resolveConstructor", signature = "Name -> DataKind -> Bool -> Type -> [TypeVar] -> M.NameMap TypeVar -> (UserCon (KUserType InfKind) UserType InfKind,Int) -> KInfer ([(Name,Expr Type)] {-UserCon Type Type Kind-}, ConInfo)" }
    , { symbol = "resolveConParam", signature = "M.NameMap TypeVar -> (Visibility,ValueBinder (KUserType InfKind) (Maybe (Expr UserType))) -> KInfer (Visibility,ValueBinder Type (Maybe (Expr Type)))" }
    , { symbol = "resolveType", signature = "M.NameMap TypeVar -> Bool -> KUserType InfKind -> KInfer Type" }
    , { symbol = "liftKApp", signature = "InfKind -> InfKind" }
    , { symbol = "infExtractKindFun", signature = "InfKind -> ([InfKind],InfKind)" }
    , { symbol = "isInfKindHandled", signature = "InfKind -> Bool" }
    , { symbol = "niceInfKinds", signature = "ColorScheme -> [InfKind] -> [Doc]" }
    , { symbol = "niceKindVars", signature = "[String]" }
    , { symbol = "ppInfKind", signature = "ColorScheme -> Nice -> Prec -> InfKind -> Doc" }
    , { symbol = "pparens", signature = "Prec -> Prec -> Doc -> Doc" }
    , { symbol = "runKindInfer", signature = "ColorScheme -> Platform -> Maybe RangeMap -> Name -> ImportMap -> KGamma -> Synonyms -> Newtypes -> Int -> KInfer a -> ([(Range,Doc)],[(Range,Doc)],Maybe RangeMap,Int,a)" }
    , { symbol = "getKindEnv", signature = "KInfer KEnv" }
    , { symbol = "addError", signature = "Range -> Doc -> KInfer ()" }
    , { symbol = "addWarning", signature = "Range -> Doc -> KInfer ()" }
    , { symbol = "getKSub", signature = "KInfer KSub" }
    , { symbol = "extendKSub", signature = "KSub -> KInfer ()" }
    , { symbol = "getLocalSynonyms", signature = "KInfer Synonyms" }
    , { symbol = "addSynonym", signature = "SynInfo -> KInfer ()" }
    , { symbol = "addRangeInfo", signature = "Range -> RangeInfo -> KInfer ()" }
    , { symbol = "addExternal", signature = "Name -> Range -> KInfer ()" }
    , { symbol = "lookupExternal", signature = "Name -> KInfer (Maybe Range)" }
    , { symbol = "freshKind", signature = "KInfer InfKind" }
    , { symbol = "freshTypeVar", signature = "TypeBinder Kind -> Flavour -> KInfer TypeVar" }
    , { symbol = "subst", signature = "HasKindVar k => k -> KInfer k" }
    , { symbol = "getKGamma", signature = "KInfer KGamma" }
    , { symbol = "getSynonyms", signature = "KInfer Synonyms" }
    , { symbol = "getAllNewtypes", signature = "KInfer Newtypes" }
    , { symbol = "getColorScheme", signature = "KInfer ColorScheme" }
    , { symbol = "getPlatform", signature = "KInfer Platform" }
    , { symbol = "extendInfGamma", signature = "[TypeBinder InfKind] -> KInfer a -> KInfer a" }
    , { symbol = "extendInfGammaUnsafe", signature = "[TypeBinder InfKind] -> KInfer a -> KInfer a" }
    , { symbol = "extendKGamma", signature = "[Range] -> Core.TypeDefGroup -> KInfer a -> KInfer a" }
    , { symbol = "memberDoc", signature = "String -> String -> [String] -> String" }
    , { symbol = "extendKGammaUnsafe", signature = "[Core.TypeDef] -> KInfer a -> KInfer a" }
    , { symbol = "checkExternal", signature = "Name -> Range -> KInfer ()" }
    , { symbol = "infQualifiedName", signature = "Name -> Range -> KInfer Name" }
    , { symbol = "findInfKind", signature = "Name -> Range -> KInfer (Name,InfKind,String)" }
    , { symbol = "ambiguous", signature = "ColorScheme -> [Name] -> Doc" }
    , { symbol = "qualifyDef", signature = "Name -> KInfer Name" }
    , { symbol = "findKind", signature = "Name -> KInfer (Name,Kind,String)" }
    , { symbol = "lookupSynInfo", signature = "Name -> KInfer (Maybe SynInfo)" }
    , { symbol = "lookupDataInfo", signature = "Name -> KInfer (Maybe DataInfo)" }
    , { symbol = "withDataEffects", signature = "[(Name,DataEffect)] -> KInfer a -> KInfer a" }
    , { symbol = "lookupDataEffect", signature = "Name -> KInfer DataEffect" }
    , { symbol = "hasKindStarResult", signature = "Kind -> Bool" }
    , { symbol = "hasKindLabelResult", signature = "Kind -> Bool" }
    , { symbol = "kindStar", signature = "Kind" }
    , { symbol = "kindLabel", signature = "Kind" }
    , { symbol = "kindArrow", signature = "Kind" }
    , { symbol = "kindAddArg", signature = "Kind -> Kind -> Kind" }
    , { symbol = "kindEffect", signature = "Kind" }
    , { symbol = "kindScope", signature = "Kind" }
    , { symbol = "kindHeap", signature = "Kind" }
    , { symbol = "kindLocal", signature = "Kind" }
    , { symbol = "kindHandled", signature = "Kind" }
    , { symbol = "kindHandled1", signature = "Kind" }
    , { symbol = "kindExtend", signature = "Kind" }
    , { symbol = "kindCon", signature = "Int -> Kind" }
    , { symbol = "kindFunN", signature = "[Kind] -> Kind -> Kind" }
    , { symbol = "kindFun", signature = "Kind -> Kind -> Kind" }
    , { symbol = "isKindFun", signature = "Kind -> Bool" }
    , { symbol = "extractKindFun", signature = "Kind -> ([Kind],Kind)" }
    , { symbol = "isKindAnyLabel", signature = "Kind -> Bool" }
    , { symbol = "builtinKinds", signature = "[(Name,Kind)]" }
    , { symbol = "newtypesEmpty", signature = "Newtypes" }
    , { symbol = "newtypesIsEmpty", signature = "Newtypes -> Bool" }
    , { symbol = "newtypesNew", signature = "[DataInfo] -> Newtypes" }
    , { symbol = "newtypesCompose", signature = "Newtypes -> Newtypes -> Newtypes" }
    , { symbol = "newtypesTypeDefs", signature = "Newtypes -> M.NameMap DataInfo" }
    , { symbol = "newtypesExtend", signature = "Name -> DataInfo -> Newtypes -> Newtypes" }
    , { symbol = "newtypesLookupPublic", signature = "Name -> Newtypes -> Maybe DataInfo" }
    , { symbol = "newtypesLookupAny", signature = "Name -> Newtypes -> Maybe DataInfo" }
    , { symbol = "newtypesFind", signature = "Name -> Newtypes -> DataInfo" }
    , { symbol = "extractNewtypes", signature = "Core.Core -> Newtypes" }
    , { symbol = "extractTypeDef", signature = "Core.TypeDef -> [DataInfo]" }
    , { symbol = "niceKinds", signature = "ColorScheme -> [Kind] -> [Doc]" }
    , { symbol = "prettyKind", signature = "ColorScheme -> Kind -> Doc" }
    , { symbol = "pparens", signature = "Prec -> Prec -> Doc -> Doc" }
    , { symbol = "ppKind", signature = "ColorScheme -> Prec -> Kind -> Doc" }
    , { symbol = "createDataDef", signature = "Monad m => (Doc-> m ()) -> (Doc-> m ()) -> (Name -> m (Maybe DataInfo))" }
    , { symbol = "orderConFields", signature = "Monad m => (Doc -> m ()) -> Doc -> (Name -> m (Maybe DataInfo)) -> Platform" }
    , { symbol = "getDataDef", signature = "Monad m => (Name -> m (Maybe DataInfo)) -> Type -> m (Maybe DataDef)" }
    , { symbol = "synonymsEmpty", signature = "Synonyms" }
    , { symbol = "synonymsIsEmpty", signature = "Synonyms -> Bool" }
    , { symbol = "synonymsNew", signature = "[SynInfo] -> Synonyms" }
    , { symbol = "synonymsTypeDefs", signature = "Synonyms -> M.NameMap SynInfo" }
    , { symbol = "synonymsExtend", signature = "SynInfo -> Synonyms -> Synonyms" }
    , { symbol = "synonymsLookup", signature = "Name -> Synonyms -> Maybe SynInfo" }
    , { symbol = "synonymsCompose", signature = "Synonyms -> Synonyms -> Synonyms" }
    , { symbol = "synonymsFind", signature = "Name -> Synonyms -> SynInfo" }
    , { symbol = "synonymsDiff", signature = "Synonyms -> Synonyms -> Synonyms" }
    , { symbol = "synonymsFilter", signature = "Name -> Synonyms -> Synonyms" }
    , { symbol = "synonymsToList", signature = "Synonyms -> [SynInfo]" }
    , { symbol = "extractSynonyms", signature = "Core.Core -> Synonyms" }
    , { symbol = "extractTypeDef", signature = "Core.TypeDef -> M.NameMap SynInfo" }
    , { symbol = "unify", signature = "Context -> Range -> InfKind -> InfKind -> KInfer ()" }
    , { symbol = "mgu", signature = "InfKind -> InfKind -> Unify" }
    , { symbol = "match", signature = "Kind -> Kind -> Bool" }
    , { symbol = "toString", signature = "JsValue -> String" }
    , { symbol = "jsLookup", signature = "JsValue -> [String] -> JsValue" }
    , { symbol = "jsFind", signature = "JsValue -> JsValue -> [String] -> JsValue" }
    , { symbol = "readJSON", signature = "FilePath -> String -> JsValue" }
    , { symbol = "parseJSON", signature = "FilePath -> String -> Either String JsValue" }
    , { symbol = "readJSONFromFile", signature = "FilePath -> IO JsValue" }
    , { symbol = "parseJSONFromFile", signature = "FilePath -> IO (Either String JsValue)" }
    , { symbol = "pValue", signature = "Parser JsValue" }
    , { symbol = "pObject", signature = "Parser JsValue" }
    , { symbol = "pMembers", signature = "Parser [(String,JsValue)]" }
    , { symbol = "pMember", signature = "Parser (String,JsValue)" }
    , { symbol = "pArray", signature = "Parser JsValue" }
    , { symbol = "pLit", signature = "Parser JsValue" }
    , { symbol = "decimalFloat", signature = "Parser JsValue" }
    , { symbol = "digitsToNum", signature = "Integer -> [Char] -> Integer" }
    , { symbol = "stringLit", signature = "Parser JsValue" }
    , { symbol = "hexesc", signature = "Parser Char" }
    , { symbol = "charesc", signature = "Parser Char" }
    , { symbol = "whitespace", signature = "Parser ()" }
    , { symbol = "comment", signature = "Parser ()" }
    , { symbol = "incomment", signature = "Parser ()" }
    , { symbol = "whitechar", signature = "Parser Char" }
    , { symbol = "skip", signature = "Parser a -> Parser ()" }
    , { symbol = "isEmptyDoc", signature = "Doc -> Bool" }
    , { symbol = "bool", signature = "Bool -> Doc" }
    , { symbol = "int", signature = "Int -> Doc" }
    , { symbol = "integer", signature = "Integer -> Doc" }
    , { symbol = "float", signature = "Float -> Doc" }
    , { symbol = "double", signature = "Double -> Doc" }
    , { symbol = "rational", signature = "Rational -> Doc" }
    , { symbol = "putPretty", signature = "Pretty a => a -> IO ()" }
    , { symbol = "makeMarkdown", signature = "Doc -> Doc" }
    , { symbol = "flatten", signature = "Doc -> Doc" }
    , { symbol = "dcontains", signature = "Doc -> (Char -> Bool) -> Bool" }
    , { symbol = "dstartsWith", signature = "Doc -> String -> Bool" }
    , { symbol = "dendsWith", signature = "Doc -> String -> Bool" }
    , { symbol = "texts", signature = "Doc -> [String]  -- lazy list of text fragments" }
    , { symbol = "rtexts", signature = "Doc -> [String]  -- lazy list of reversed text fragments" }
    , { symbol = "renderPrettyB", signature = "Float -> Int -> Doc -> B.Builder" }
    , { symbol = "renderPretty", signature = "Float -> Int -> Doc -> SimpleDoc" }
    , { symbol = "renderCompact", signature = "Doc -> SimpleDoc" }
    , { symbol = "asString", signature = "Doc -> String" }
    , { symbol = "displayS", signature = "SimpleDoc -> ShowS" }
    , { symbol = "displayP", signature = "Printer p => p -> Int -> SimpleDoc -> IO ()" }
    , { symbol = "writePretty", signature = "Printer p => p -> Doc -> IO ()" }
    , { symbol = "writePrettyLn", signature = "Printer p => p -> Doc -> IO ()" }
    , { symbol = "writePrettyW", signature = "Printer p => p -> Int -> Doc -> IO ()" }
    , { symbol = "writeAtomicPrettyLn", signature = "Printer p => p -> Doc -> IO ()" }
    , { symbol = "putDoc", signature = "Doc -> IO ()" }
    , { symbol = "hPutDoc", signature = "Handle -> Doc -> IO ()" }
    , { symbol = "hPutDocW", signature = "Int -> Handle -> Doc -> IO ()" }
    , { symbol = "writeDoc", signature = "FilePath -> Doc -> IO ()" }
    , { symbol = "writeDocW", signature = "Int -> FilePath -> Doc -> IO ()" }
    , { symbol = "spaces", signature = "Int -> String" }
    , { symbol = "defaultWidth", signature = "Int" }
    , { symbol = "sanitize", signature = "String -> String" }
    , { symbol = "sanitizeT", signature = "T.Text -> T.Text" }
    , { symbol = "withMonoPrinter", signature = "Handle -> (MonoPrinter -> IO a) -> IO a" }
    , { symbol = "withFilePrinter", signature = "FilePath -> (FilePrinter -> IO a) -> IO a" }
    , { symbol = "withNewFilePrinter", signature = "FilePath -> (FilePrinter -> IO a) -> IO a" }
    , { symbol = "withAnsiPrinter", signature = "(AnsiPrinter -> IO a) -> IO a" }
    , { symbol = "ansiStringWithConsole", signature = "AnsiStringPrinter -> (AnsiConsole -> AnsiConsole) -> IO a -> IO a" }
    , { symbol = "ansiStringSetConsole", signature = "AnsiStringPrinter -> (AnsiConsole -> AnsiConsole) -> IO AnsiConsole" }
    , { symbol = "ansiWithColor", signature = "Color -> String -> String" }
    , { symbol = "ansiWithConsole", signature = "AnsiPrinter -> (AnsiConsole -> AnsiConsole) -> IO a -> IO a" }
    , { symbol = "ansiSetConsole", signature = "AnsiPrinter -> (AnsiConsole -> AnsiConsole) -> IO AnsiConsole" }
    , { symbol = "ansiEscapeIO", signature = "[T.Text] -> IO ()" }
    , { symbol = "ansiEscape", signature = "[T.Text] -> T.Text" }
    , { symbol = "seqReset", signature = "[T.Text]" }
    , { symbol = "seqUnderline", signature = "Bool -> [T.Text]" }
    , { symbol = "seqColor", signature = "Bool -> Color -> [T.Text]" }
    , { symbol = "ansiColor", signature = "Color -> Int" }
    , { symbol = "withColorPrinter", signature = "(ColorPrinter -> IO b) -> IO b" }
    , { symbol = "withHtmlColorPrinter", signature = "(ColorPrinter -> IO b) -> IO b" }
    , { symbol = "withNoColorPrinter", signature = "Handle -> (ColorPrinter -> IO b) -> IO b" }
    , { symbol = "withFileNoColorPrinter", signature = "FilePath -> (ColorPrinter -> IO b) -> IO b" }
    , { symbol = "isAnsiPrinter", signature = "ColorPrinter -> Bool" }
    , { symbol = "isConsolePrinter", signature = "ColorPrinter -> Bool" }
    , { symbol = "withHtmlPrinter", signature = "(HtmlPrinter -> IO a) -> IO a" }
    , { symbol = "withHtmlTextPrinter", signature = "(HtmlTextPrinter -> IO a) -> IO a" }
    , { symbol = "addHtml", signature = "HtmlTextPrinter -> T.Text -> IO ()" }
    , { symbol = "htmlSpan", signature = "T.Text -> T.Text -> IO a -> IO a" }
    , { symbol = "htmlTextSpan", signature = "HtmlTextPrinter -> T.Text -> T.Text -> IO a -> IO a" }
    , { symbol = "htmlColor", signature = "Color -> T.Text" }
    , { symbol = "htmlColor2", signature = "Color -> T.Text" }
    , { symbol = "nodes", signature = "Graph v -> [Node v]" }
    , { symbol = "graph", signature = "Ord v => [Node v] -> Graph v" }
    , { symbol = "edges", signature = "Graph v -> [Edge v]" }
    , { symbol = "vertices", signature = "Graph v -> [v]" }
    , { symbol = "successors", signature = "Ord v => v -> Graph v -> [v]" }
    , { symbol = "transpose", signature = "Ord v => Graph v -> Graph v" }
    , { symbol = "dff", signature = "Ord v => Graph v -> Forest v" }
    , { symbol = "dfs", signature = "Ord v => Graph v -> [v] -> Forest v" }
    , { symbol = "tree", signature = "Ord v => Graph v -> v -> Tree v" }
    , { symbol = "prune", signature = "Ord v => Forest v -> Forest v" }
    , { symbol = "preorder", signature = "Ord v => Graph v -> [v]" }
    , { symbol = "postorder", signature = "Ord v => Graph v -> [v]" }
    , { symbol = "scc", signature = "Ord v => [(v,[v])] -> [[v]]" }
    , { symbol = "sccG", signature = "Ord v => Graph v -> [[v]]" }
    , { symbol = "sccF", signature = "Ord v => Graph v -> Forest v" }
    , { symbol = "tgraph0", signature = "Graph Int" }
    , { symbol = "arbgraph", signature = "(Ord v,Arbitrary v) => Int -> Gen (Graph v)" }
    , { symbol = "prop_ValidGraph", signature = "Graph Int -> Bool" }
    , { symbol = "prop_SccComplete", signature = "Graph Int -> Bool" }
    , { symbol = "prop_SccForward", signature = "Graph Int -> Bool" }
    , { symbol = "prop_SccConnected", signature = "Graph Int -> Bool" }
    , { symbol = "trace", signature = "String -> a -> a" }
    , { symbol = "traceShowM", signature = "(Applicative m, Show s) => s -> m ()" }
    , { symbol = "traceM", signature = "(Applicative m) => String -> m ()" }
    , { symbol = "traceId", signature = "String -> String" }
    , { symbol = "traceShow", signature = "(Show s) => s -> a -> a" }
    , { symbol = "traceShowId", signature = "(Show a) => a -> a" }
    , { symbol = "traceDoc", signature = "Doc -> a -> a" }
    , { symbol = "traceEq", signature = "(Show a) => String -> a -> a" }
    , { symbol = "ctrace", signature = "Color -> String -> a -> a" }
    , { symbol = "runPlain", signature = "IO ()" }
    , { symbol = "runWith", signature = "String -> IO ()" }
    , { symbol = "runWithLS", signature = "(ColorPrinter -> Flags -> [FilePath] -> IO ()) -> IO ()" }
    , { symbol = "plainLS", signature = "ColorPrinter -> Flags -> [FilePath] -> IO ()" }
    , { symbol = "runWithLSArgs", signature = "(ColorPrinter -> Flags -> [FilePath] -> IO ()) -> String -> IO ()" }
    , { symbol = "mainMode", signature = "(ColorPrinter -> Flags -> [FilePath] -> IO ()) -> Flags -> Mode -> ColorPrinter -> IO ()" }
    , { symbol = "compileAll", signature = "ColorPrinter -> Flags -> [FilePath] -> IO Bool" }
    , { symbol = "compileEntry", signature = "BuildContext -> Name -> Build (IO ())" }
    , { symbol = "compileShowInfo", signature = "BuildContext -> ModuleName -> Build ()" }
    , { symbol = "toLspPos", signature = "R.Pos -> J.Position" }
    , { symbol = "toLspRange", signature = "R.Range -> J.Range" }
    , { symbol = "toLspLocation", signature = "R.Range -> J.Location" }
    , { symbol = "toLspLocationLink", signature = "RangeInfo -> R.Range -> J.LocationLink" }
    , { symbol = "toLspDiagnostics", signature = "J.NormalizedUri -> Maybe T.Text -> E.Error b a -> M.Map J.NormalizedUri [J.Diagnostic]" }
    , { symbol = "errorMessageToDiagnostic", signature = "Maybe T.Text -> J.NormalizedUri -> E.ErrorMessage -> (J.NormalizedUri, [J.Diagnostic])" }
    , { symbol = "toLspErrorDiagnostics", signature = "J.NormalizedUri -> Maybe T.Text -> E.ErrorMessage -> M.Map J.NormalizedUri [J.Diagnostic]" }
    , { symbol = "uriFromRange", signature = "R.Range -> J.NormalizedUri -> J.NormalizedUri" }
    , { symbol = "toLspWarningDiagnostic", signature = "Maybe T.Text -> R.Range -> Doc -> J.Diagnostic" }
    , { symbol = "makeDiagnostic", signature = "J.DiagnosticSeverity -> Maybe T.Text -> R.Range -> Doc -> J.Diagnostic" }
    , { symbol = "fromLspPos", signature = "J.NormalizedUri -> J.Position -> IO R.Pos" }
    , { symbol = "fromLspRange", signature = "J.NormalizedUri -> J.Range -> IO R.Range" }
    , { symbol = "fromLspLocation", signature = "J.Location -> IO R.Range" }
    , { symbol = "toLspUri", signature = "FilePath -> J.NormalizedUri" }
    , { symbol = "fromLspUri", signature = "J.NormalizedUri -> IO (Maybe FilePath)" }
    , { symbol = "filePathToUri", signature = "FilePath -> J.Uri" }
    , { symbol = "codeActionHandler", signature = "Handlers LSM" }
    , { symbol = "findType", signature = "Definitions -> Range -> Maybe DataInfo" }
    , { symbol = "toCodeAction", signature = "Env -> J.Uri -> Range -> String ->  Error b (Def UserType) -> Maybe J.CodeAction" }
    , { symbol = "isStarType_", signature = "Type -> Bool" }
    , { symbol = "isStarType", signature = "Type -> Bool" }
    , { symbol = "isStarTypeVar", signature = "TypeVar -> Bool" }
    , { symbol = "mkBind", signature = "Name -> Range -> ValueBinder (Maybe UserType) (Maybe (Expr UserType))" }
    , { symbol = "mkBindt", signature = "Name -> UserType -> Range -> ValueBinder (Maybe UserType) (Maybe (Expr UserType))" }
    , { symbol = "tpForall", signature = "[UserType] -> UserType -> UserType" }
    , { symbol = "tpString", signature = "UserType" }
    , { symbol = "tpBool", signature = "UserType" }
    , { symbol = "tpVarName", signature = "UserType -> Name" }
    , { symbol = "userTp", signature = "Nice -> Type -> UserType" }
    , { symbol = "appendOp", signature = "Expr UserType" }
    , { symbol = "appendStr", signature = "Expr UserType -> Expr UserType -> Expr UserType" }
    , { symbol = "synGeneralUnary", signature = "Name -> Name -> String -> DataInfo -> (TypeVar, UserType) -> ((Bool, UserType, Int) -> UserType) -> SynUnaryBranch -> (Def UserType)" }
    , { symbol = "synOverloaded", signature = "Name -> String -> DataInfo -> Core.CorePhase b (Def UserType)" }
    , { symbol = "synMap", signature = "Name -> String -> DataInfo -> Core.CorePhase b (Def UserType)" }
    , { symbol = "synShowString", signature = "Name -> DataInfo -> Core.CorePhase b (Def UserType)" }
    , { symbol = "hasSingleCon", signature = "DataInfo -> Bool" }
    , { symbol = "tupleBranch", signature = "Pattern UserType -> Pattern UserType -> Expr UserType -> Range -> Branch UserType" }
    , { symbol = "synBinaryOp", signature = "Name -> Name -> Bool -> String -> DataInfo -> (TypeVar, UserType) -> (UserType -> UserType) -> [Branch UserType] -> SynBinaryBranch -> Def UserType" }
    , { symbol = "synEquality", signature = "Name -> DataInfo -> Core.CorePhase b (Def UserType)" }
    , { symbol = "synOrd", signature = "Name -> DataInfo -> Core.CorePhase b (Def UserType)" }
    , { symbol = "synOrder2", signature = "Name -> DataInfo -> Core.CorePhase b (Def UserType) -- TODO: Make this actually fip" }
    , { symbol = "commandHandler", signature = "Handlers LSM" }
    , { symbol = "getNewFlags", signature = "T.Text -> LSM Flags" }
    , { symbol = "completionHandler", signature = "Handlers LSM" }
    , { symbol = "isTypeCompletion", signature = "CompletionKind -> Bool" }
    , { symbol = "getCompletionInfo", signature = "MonadIO m => J.Position -> VirtualFile -> RangeMap -> J.NormalizedUri -> [Lexeme] -> m (Maybe CompletionInfo)" }
    , { symbol = "filterInfix", signature = "(Name,CompletionInfo) -> Bool -- hide hidden names unless they are handlers" }
    , { symbol = "searchAllHandlers", signature = "Name -> CompletionInfo -> Bool" }
    , { symbol = "searchNameMatch", signature = "Name -> CompletionInfo -> Bool" }
    , { symbol = "findCompletions", signature = "Definitions -> ModuleName -> CompletionInfo -> [J.CompletionItem]" }
    , { symbol = "typeUnifies", signature = "Type -> Maybe Type -> Name -> Bool" }
    , { symbol = "valueCompletions", signature = "Name -> Gamma -> CompletionInfo -> [(Name, J.CompletionItem)]" }
    , { symbol = "constructorCompletions", signature = "Name -> Constructors -> [(Name, J.CompletionItem)]" }
    , { symbol = "synonymCompletions", signature = "CompletionInfo -> Name -> Synonyms -> [(Name, J.CompletionItem)]" }
    , { symbol = "datatypeCompletions", signature = "CompletionInfo -> Name -> Newtypes -> [(Name, J.CompletionItem)]" }
    , { symbol = "keywordCompletions", signature = "CompletionInfo -> Name -> [J.CompletionItem]" }
    , { symbol = "makeCompletionItem", signature = "Name -> Name -> J.CompletionItemKind -> String -> J.CompletionItem" }
    , { symbol = "makeFunctionCompletionItem", signature = "Name -> Name -> String -> Type -> Bool -> J.Range -> T.Text-> J.CompletionItem" }
    , { symbol = "makeHandlerCompletionItem", signature = "Name -> ConInfo -> String -> J.Range -> T.Text -> J.CompletionItem" }
    , { symbol = "handlerArgs", signature = "T.Text -> Type -> [Type]" }
    , { symbol = "makeSimpleCompletionItem", signature = "Name -> String -> J.CompletionItemKind -> J.CompletionItem" }
    , { symbol = "definitionHandler", signature = "Handlers LSM" }
    , { symbol = "findDefLinks", signature = "Definitions -> [(ModuleName,FilePath)] -> RangeInfo -> [J.DefinitionLink]" }
    , { symbol = "documentSymbolHandler", signature = "Handlers LSM" }
    , { symbol = "makeSymbolSelect", signature = "Name -> J.SymbolKind -> R.Range -> R.Range -> [J.DocumentSymbol] -> [J.DocumentSymbol]" }
    , { symbol = "makeSymbol", signature = "Name -> J.SymbolKind -> R.Range -> [J.DocumentSymbol] -> [J.DocumentSymbol]" }
    , { symbol = "foldingHandler", signature = "Handlers LSM" }
    , { symbol = "makeFoldingNoName", signature = "R.Range -> [J.FoldingRange]" }
    , { symbol = "makeFolding", signature = "R.Range -> Name -> [J.FoldingRange]" }
    , { symbol = "rangeSpansLine", signature = "R.Range -> Bool" }
    , { symbol = "hoverHandler", signature = "Handlers LSM" }
    , { symbol = "formatRangeInfoHover", signature = "Env -> [(ModuleName,FilePath)] -> RangeInfo -> Doc" }
    , { symbol = "inlayHintsHandler", signature = "Handlers LSM" }
    , { symbol = "createInlayHints", signature = "InlayHintOptions -> Env -> ModuleName -> [Lexeme] -> (Range, RangeInfo) -> [J.InlayHint]" }
    , { symbol = "typeHint", signature = "Env -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]" }
    , { symbol = "qualifierHint", signature = "Env -> Name -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]" }
    , { symbol = "generalHint", signature = "Env -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]" }
    , { symbol = "implicitsHint", signature = "Env -> [Lexeme] -> Range -> RangeInfo -> [J.InlayHint]" }
    , { symbol = "finalCallRange", signature = "[Lexeme] -> (Range, Doc -> Doc)" }
    , { symbol = "newInlayHint", signature = "Range -> String -> J.InlayHintKind -> Bool -> J.InlayHint" }
    , { symbol = "ppComment", signature = "String -> Doc" }
    , { symbol = "asKokaCode", signature = "Doc -> Doc" }
    , { symbol = "signatureHelpHandler", signature = "Handlers LSM" }
    , { symbol = "getSignatureInformation", signature = "Env -> (Name,NameInfo) -> LSM [(Name, J.SignatureInformation)]" }
    , { symbol = "didOpenHandler", signature = "Handlers LSM" }
    , { symbol = "didChangeHandler", signature = "Handlers LSM" }
    , { symbol = "didSaveHandler", signature = "Handlers LSM" }
    , { symbol = "didCloseHandler", signature = "Handlers LSM" }
    , { symbol = "diffVFS", signature = "Map J.NormalizedUri (ByteString, FileTime, J.Int32) -> Map J.NormalizedUri VirtualFileEntry -> LSM (Map J.NormalizedUri (ByteString, FileTime, J.Int32))" }
    , { symbol = "updateVFS", signature = "LSM (Map J.NormalizedUri (ByteString, FileTime, J.Int32))" }
    , { symbol = "rebuildUri", signature = "Maybe Flags -> Maybe Name -> J.NormalizedUri -> LSM (Maybe FilePath)" }
    , { symbol = "fileNameFromPath", signature = "FilePath -> LSM FilePath" }
    , { symbol = "rebuildFile", signature = "Maybe Flags -> Maybe Name -> J.NormalizedUri -> FilePath -> LSM (Maybe FilePath)" }
    , { symbol = "liftBuildDiag", signature = "Maybe Flags -> J.NormalizedUri -> (BuildContext -> Build (BuildContext,a)) -> LSM (Maybe a)" }
    , { symbol = "diagnoseErrors", signature = "J.NormalizedUri -> [ErrorMessage] -> LSM ()" }
    , { symbol = "diagSourceKoka", signature = "Maybe T.Text" }
    , { symbol = "handlers", signature = "Handlers LSM" }
    , { symbol = "configurationChangeHandler", signature = "Handlers LSM" }
    , { symbol = "initializedHandler", signature = "Handlers LSM" }
    , { symbol = "cancelHandler", signature = "Handlers LSM" }
    , { symbol = "runLSM", signature = "LSM a -> MVar LSState -> LanguageContextEnv () -> IO a" }
    , { symbol = "newLSStateVar", signature = "Flags -> IO (MVar LSState)" }
    , { symbol = "getLSState", signature = "LSM LSState" }
    , { symbol = "modifyLSState", signature = "(LSState -> LSState) -> LSM ()" }
    , { symbol = "defaultLSState", signature = "Flags -> IO LSState" }
    , { symbol = "htmlTextColorPrinter", signature = "Doc -> IO T.Text" }
    , { symbol = "setProgress", signature = "Maybe (J.ProgressAmount -> LSM ()) -> LSM ()" }
    , { symbol = "getProgress", signature = "LSM (Maybe (J.ProgressAmount -> LSM ()))" }
    , { symbol = "updateConfig", signature = "A.Value -> LSM ()" }
    , { symbol = "updateColorScheme", signature = "Colors -> LSM ()" }
    , { symbol = "updateSignatureContext", signature = "SignatureContext -> LSM ()" }
    , { symbol = "clearSignatureContext", signature = "LSM ()" }
    , { symbol = "getSignatureContext", signature = "LSM (Maybe SignatureContext)" }
    , { symbol = "getInlayHintOptions", signature = "LSM InlayHintOptions" }
    , { symbol = "getVirtualFileVersion", signature = "J.NormalizedUri -> LSM (Maybe J.Int32)" }
    , { symbol = "getTerminal", signature = "LSM Terminal" }
    , { symbol = "getFlags", signature = "LSM Flags" }
    , { symbol = "getHtmlPrinter", signature = "LSM (Doc -> IO T.Text)" }
    , { symbol = "getColorScheme", signature = "LSM ColorScheme" }
    , { symbol = "maybeContents", signature = "Map J.NormalizedUri (ByteString, FileTime, J.Int32) -> FilePath -> Maybe (ByteString, FileTime)" }
    , { symbol = "liftBuildWith", signature = "Maybe Flags -> (BuildContext -> Build (BuildContext,a)) -> LSM (Either Errors (a,Errors))" }
    , { symbol = "liftBuild", signature = "(BuildContext -> Build (BuildContext,a)) -> LSM (Either Errors (a,Errors))" }
    , { symbol = "getBuildContext", signature = "LSM BuildContext" }
    , { symbol = "lookupModuleName", signature = "J.NormalizedUri -> LSM (Maybe (FilePath,ModuleName))" }
    , { symbol = "lookupLexemes", signature = "ModuleName -> LSM (Maybe [Lexeme])" }
    , { symbol = "lookupRangeMap", signature = "ModuleName -> LSM (Maybe (RangeMap,[Lexeme]))" }
    , { symbol = "lookupProgram", signature = "ModuleName -> LSM (Maybe UserProgram)" }
    , { symbol = "getPrettyEnv", signature = "LSM TP.Env" }
    , { symbol = "getPrettyEnvFor", signature = "ModuleName -> LSM TP.Env" }
    , { symbol = "prettyMarkdown", signature = "Doc -> LSM T.Text" }
    , { symbol = "emitInfo", signature = "(TP.Env -> Doc) -> LSM ()" }
    , { symbol = "emitNotification", signature = "(TP.Env -> Doc) -> LSM ()" }
    , { symbol = "lookupDefinitions", signature = "[ModuleName] -> LSM Definitions" }
    , { symbol = "lookupVisibleDefinitions", signature = "[ModuleName] -> LSM Definitions" }
    , { symbol = "lookupModulePaths", signature = "LSM [(ModuleName,FilePath)]" }
    , { symbol = "runLanguageServer", signature = "Flags -> [FilePath] -> IO ()" }
    , { symbol = "messageHandler", signature = "TChan (String, J.MessageType) -> LanguageContextEnv () -> MVar LSState -> IO ()" }
    , { symbol = "progressHandler", signature = "TChan (Double,String) -> LanguageContextEnv () -> MVar LSState -> IO ()" }
    , { symbol = "reactor", signature = "TChan ReactorInput -> IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "programName", signature = "String" }
    , { symbol = "version", signature = "String" }
    , { symbol = "compilerBuildVariant", signature = "String" }
    , { symbol = "compiler", signature = "String" }
    , { symbol = "exeExtension", signature = "String" }
    , { symbol = "pathSep,pathDelimiter", signature = "Char" }
    , { symbol = "sourceExtension", signature = "String" }
    , { symbol = "buildDate", signature = "String" }
    , { symbol = "buildTime", signature = "String" }
    , { symbol = "setColor", signature = "Enum c => c -> IO ()" }
    , { symbol = "setBackColor", signature = "Enum c => c -> IO ()" }
    , { symbol = "setReverse", signature = "Bool -> IO ()" }
    , { symbol = "setUnderline", signature = "Bool -> IO ()" }
    , { symbol = "withConsole", signature = "(Bool -> IO a) -> IO a" }
    , { symbol = "bracketConsole", signature = "IO a -> IO a" }
    , { symbol = "getProgramPath", signature = "IO String" }
    , { symbol = "getCurrentTime", signature = "IO FileTime" }
    , { symbol = "fileTime0", signature = "FileTime" }
    , { symbol = "showTimeDiff", signature = "FileTime -> FileTime -> String" }
    , { symbol = "fileTimeToPicoseconds", signature = "FileTime -> Integer" }
    , { symbol = "diffTimeToPicoseconds", signature = "T.DiffTime -> Integer" }
    , { symbol = "getCurrentTime", signature = "IO FileTime" }
    , { symbol = "fileTime0", signature = "FileTime" }
    , { symbol = "fileTimeToPicoseconds", signature = "FileTime -> Integer" }
    , { symbol = "getFileTime", signature = "FilePath -> IO FileTime" }
    , { symbol = "setFileTime", signature = "FilePath -> FileTime -> IO ()" }
    , { symbol = "getFileTimeOrCurrent", signature = "FilePath -> IO FileTime" }
    , { symbol = "withReadLine", signature = "FilePath -> IO a -> IO a" }
    , { symbol = "readLineEx", signature = "ColorScheme -> [FilePath] -> [String] -> [(String,String)] -> String -> IO () -> IO (Maybe String)" }
    , { symbol = "addHistory", signature = "String -> IO ()" }
    , { symbol = "continueLine", signature = "Monad m => Maybe String -> m (Maybe String) -> m (Maybe String)" }
    , { symbol = "vhistory", signature = "IORef H.History" }
    , { symbol = "completeLine", signature = "[FilePath] -> [String] -> C.CompletionFunc IO" }
    , { symbol = "listNames", signature = "[String] -> String -> IO [C.Completion]" }
    , { symbol = "listModules", signature = "[FilePath] -> String -> IO [C.Completion]" }
    , { symbol = "completer", signature = "[FilePath] -> [String] -> [(String,String)] -> CompletionEnv -> String -> IO ()" }
    , { symbol = "completeModules", signature = "[FilePath] -> CompletionEnv -> String ->IO ()" }
    , { symbol = "completeOptions", signature = "[(String,String)] -> CompletionEnv -> String -> IO ()" }
    , { symbol = "completeOptionFlags", signature = "[(String,String)] -> String -> [Completion]" }
    , { symbol = "completeIdentifiers", signature = "[String] -> CompletionEnv -> String -> IO ()" }
    , { symbol = "completeNames", signature = "[String] -> String -> [Completion]" }
    , { symbol = "highlighter", signature = "ColorScheme -> String -> Fmt" }
    , { symbol = "exCatch", signature = "IO a -> (String -> IO a) -> IO a" }
    , { symbol = "copyBinaryFile", signature = "FilePath -> FilePath -> IO ()" }
    , { symbol = "finally", signature = "IO a -> IO b -> IO a" }
    , { symbol = "exCatch", signature = "IO a -> (String -> IO a) -> IO a" }
    , { symbol = "copyBinaryFile", signature = "FilePath -> FilePath -> IO ()" }
    , { symbol = "showHFloat", signature = "Double -> String -> String" }
    , { symbol = "newVar", signature = "a -> IO (Var a)" }
    , { symbol = "takeVar", signature = "Var a -> IO a" }
    , { symbol = "putVar", signature = "Var a -> a -> IO ()" }
    , { symbol = "newVar", signature = "a -> IO (Var a)" }
    , { symbol = "putVar", signature = "Var a -> a -> IO ()" }
    , { symbol = "takeVar", signature = "Var a -> IO a" }
    , { symbol = "programName", signature = "String" }
    , { symbol = "version", signature = "String" }
    , { symbol = "exeExtension", signature = "String" }
    , { symbol = "pathSep", signature = "Char" }
    , { symbol = "pathDelimiter", signature = "Char" }
    , { symbol = "sourceExtension", signature = "String" }
    , { symbol = "buildDate", signature = "String" }
    , { symbol = "buildTime", signature = "String" }
    , { symbol = "buildVariant", signature = "String" }
    , { symbol = "compiler", signature = "String" }
    , { symbol = "setColor", signature = "Enum c => c -> IO ()" }
    , { symbol = "setBackColor", signature = "Enum c => c -> IO ()" }
    , { symbol = "setReverse", signature = "Bool -> IO ()" }
    , { symbol = "setUnderline", signature = "Bool -> IO ()" }
    , { symbol = "withConsole", signature = "(Bool -> IO a) -> IO a" }
    , { symbol = "bracketConsole", signature = "IO a -> IO a" }
    , { symbol = "getOpt", signature = "ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])" }
    , { symbol = "usageInfo", signature = "String -> [OptDescr a] -> String" }
    , { symbol = "withReadLine", signature = "IO a -> IO a" }
    , { symbol = "readLine", signature = "String -> IO (Maybe String)" }
    , { symbol = "addHistory", signature = "String -> IO ()" }
    , { symbol = "unsafePerformIO", signature = "IO a -> a" }
    , { symbol = "finally", signature = "IO a -> IO b -> IO a" }
    , { symbol = "exCatch", signature = "IO a -> (String -> IO a) -> IO a" }
    , { symbol = "copyBinaryFile", signature = "FilePath -> FilePath -> IO ()" }
    , { symbol = "newVar", signature = "a -> IO (Var a)" }
    , { symbol = "putVar", signature = "Var a -> a -> IO ()" }
    , { symbol = "takeVar", signature = "Var a -> IO a" }
    , { symbol = "programName", signature = "String" }
    , { symbol = "version", signature = "String" }
    , { symbol = "exeExtension", signature = "String" }
    , { symbol = "pathSep", signature = "Char" }
    , { symbol = "pathDelimiter", signature = "Char" }
    , { symbol = "sourceExtension", signature = "String" }
    , { symbol = "buildDate", signature = "String" }
    , { symbol = "buildTime", signature = "String" }
    , { symbol = "buildVariant", signature = "String" }
    , { symbol = "compiler", signature = "String" }
    , { symbol = "setColor", signature = "Enum c => c -> IO ()" }
    , { symbol = "setBackColor", signature = "Enum c => c -> IO ()" }
    , { symbol = "setReverse", signature = "Bool -> IO ()" }
    , { symbol = "setUnderline", signature = "Bool -> IO ()" }
    , { symbol = "withConsole", signature = "(Bool -> IO a) -> IO a" }
    , { symbol = "bracketConsole", signature = "IO a -> IO a" }
    , { symbol = "getCurrentTime", signature = "IO FileTime" }
    , { symbol = "fileTime0", signature = "FileTime" }
    , { symbol = "getFileTime", signature = "FilePath -> IO FileTime" }
    , { symbol = "getFileTimeOrCurrent", signature = "FilePath -> IO FileTime" }
    , { symbol = "withReadLine", signature = "IO a -> IO a" }
    , { symbol = "readLine", signature = "String -> IO (Maybe String)" }
    , { symbol = "addHistory", signature = "String -> IO ()" }
    , { symbol = "finally", signature = "IO a -> IO b -> IO a" }
    , { symbol = "exCatch", signature = "IO a -> (String -> IO a) -> IO a" }
    , { symbol = "copyBinaryFile", signature = "FilePath -> FilePath -> IO ()" }
    , { symbol = "newVar", signature = "a -> IO (Var a)" }
    , { symbol = "putVar", signature = "Var a -> a -> IO ()" }
    , { symbol = "takeVar", signature = "Var a -> IO a" }
    , { symbol = "groupProgramBindings", signature = "UserProgram -> UserProgram" }
    , { symbol = "groupTypeDefBindings", signature = "Name -> [UserTypeDefGroup] -> [UserTypeDefGroup]" }
    , { symbol = "dependencyTypeDef", signature = "Name -> UserTypeDef -> (Name,S.NameSet)" }
    , { symbol = "groupBindings", signature = "Name -> [DefGroup t] -> [DefGroup t]" }
    , { symbol = "extractDefGroups", signature = "[DefGroup t] -> ExtraDeps" }
    , { symbol = "eedDef", signature = "Def t -> ExtraDeps" }
    , { symbol = "bindingsDefGroup", signature = "Name -> DefGroup t -> ([Def t], Deps)" }
    , { symbol = "dependencies", signature = "Name -> [Def t] -> ([Def t], Deps)" }
    , { symbol = "dependencyDef", signature = "Name -> Def t -> (Def t, Deps)" }
    , { symbol = "dependencyBinding", signature = "Name -> ValueBinder () (Expr t) -> (ValueBinder () (Expr t), Deps)" }
    , { symbol = "dependencyDefFv", signature = "Name -> Def t -> (Def t, FreeVar)" }
    , { symbol = "dependencyDefGroupFv", signature = "Name -> DefGroup t -> ([DefGroup t],FreeVar,S.NameSet)" }
    , { symbol = "dependencyExpr", signature = "Name -> Expr t -> (Expr t, FreeVar)" }
    , { symbol = "dependencyHandlerBranch", signature = "Name -> HandlerBranch t -> (HandlerBranch t, FreeVar)" }
    , { symbol = "dependencyBranch", signature = "Name -> Branch t -> (Branch t, FreeVar)" }
    , { symbol = "dependencyGuard", signature = "Name -> Guard t -> (Guard t, FreeVar)" }
    , { symbol = "dependencyLamBinders", signature = "Name -> FreeVar -> [ValueBinder (Maybe t) (Maybe (Expr t))] -> ([ValueBinder (Maybe t) (Maybe (Expr t))], FreeVar)" }
    , { symbol = "dependencyLamBinder", signature = "Name -> ValueBinder (Maybe t) (Maybe (Expr t)) -> (ValueBinder (Maybe t) (Maybe (Expr t)), FreeVar)" }
    , { symbol = "freeBoundVar", signature = "DefGroup t -> (FreeVar,FreeVar)" }
    , { symbol = "hasFreeVar", signature = "Expr t -> Name -> Bool" }
    , { symbol = "group", signature = "[Def t] -> Deps -> [DefGroup t]" }
    , { symbol = "groupTypeDefs", signature = "[UserTypeDef] -> Deps -> [UserTypeDefGroup]" }
    , { symbol = "group", signature = "Defs -> Deps -> DefGroups" }
    , { symbol = "groupTypeDefBindings", signature = "TypeDefs -> Deps -> TypeDefGroups" }
    , { symbol = "fixityResolve", signature = "ColorScheme -> Fixities -> UserProgram -> Error b (UserProgram,Fixities)" }
    , { symbol = "extractFixMap", signature = "[FixDef] -> Fixities" }
    , { symbol = "resolveTypeDefs", signature = "TypeDefGroups UserType UserKind -> FixM (TypeDefGroups UserType UserKind)" }
    , { symbol = "resolveTypeDef", signature = "UserTypeDef -> FixM UserTypeDef" }
    , { symbol = "resolveConstr", signature = "UserUserCon -> FixM UserUserCon" }
    , { symbol = "resolveDefs", signature = "DefGroups UserType -> FixM (DefGroups UserType)" }
    , { symbol = "resolveExpr", signature = "UserExpr -> FixM UserExpr" }
    , { symbol = "fixitiesEmpty", signature = "Fixities" }
    , { symbol = "findFixity", signature = "Fixities -> Name -> Fixity" }
    , { symbol = "fixitiesCompose", signature = "Fixities -> Fixities -> Fixities" }
    , { symbol = "fixitiesNew", signature = "[(Name,Fixity)] -> Fixities" }
    , { symbol = "runFixM", signature = "Fixities -> FixM a -> Error b a" }
    , { symbol = "getFixities", signature = "FixM Fixities" }
    , { symbol = "emitError", signature = "Range -> Doc -> FixM ()" }
    , { symbol = "resolve", signature = "[UserExpr] -> FixM UserExpr" }
    , { symbol = "toTerm", signature = "Fixities -> UserExpr -> Term" }
    , { symbol = "resolveTerms", signature = "ExprStack -> OpStack -> [Term] -> FixM UserExpr" }
    , { symbol = "ambigious", signature = "Fixity -> Fixity -> UserExpr -> FixM ()" }
    , { symbol = "colorize", signature = "Printer p => Maybe RangeMap -> Env -> KGamma -> Gamma -> Bool -> FilePath -> Int -> BString -> p -> IO ()" }
    , { symbol = "transform", signature = "Bool -> Range -> RangeMap -> Env -> Lexeme -> String -> ([Range], RangeMap, String)" }
    , { symbol = "fmtHtml", signature = "Token Lexeme -> Lexeme -> String -> String" }
    , { symbol = "fmtComment", signature = "TokenComment String -> String" }
    , { symbol = "showDoc", signature = "Env -> KGamma -> Gamma -> String -> String" }
    , { symbol = "showLexemes", signature = "Env -> KGamma -> Gamma -> [Lexeme] -> [String]" }
    , { symbol = "fmtLiterate", signature = "Maybe RangeMap -> Env -> KGamma -> Gamma -> Token Lexeme -> Lexeme -> String -> String" }
    , { symbol = "linkFromId", signature = "Env -> Name -> String -> Gamma -> String" }
    , { symbol = "fmtTypeName", signature = "Name -> String" }
    , { symbol = "fmtModuleName", signature = "Name -> String" }
    , { symbol = "fmtName", signature = "Name -> String" }
    , { symbol = "fmtNameString", signature = "String -> String" }
    , { symbol = "fmtOpString", signature = "String -> String" }
    , { symbol = "linkFromTypeId", signature = "Env -> Name -> KGamma -> String -> String" }
    , { symbol = "fmtComment", signature = "Maybe RangeMap -> Env -> KGamma -> Gamma -> TokenComment Lexeme -> String" }
    , { symbol = "kindSignature", signature = "Env -> Name -> Kind -> String -> String" }
    , { symbol = "prefixtag", signature = "String -> String -> String  -> String" }
    , { symbol = "writeSpan", signature = "Printer p => p -> String -> String -> IO ()" }
    , { symbol = "prefix", signature = "String" }
    , { symbol = "shorthands", signature = "[(String,String)]" }
    , { symbol = "genDoc", signature = "Printer p => Env -> KGamma -> Gamma -> Core -> p -> IO ()" }
    , { symbol = "fmtImports", signature = "Env -> KGamma -> Gamma -> [Import] -> String" }
    , { symbol = "fmtImportEntry", signature = "Env -> KGamma -> Gamma -> [String] -> Import -> ([String],String)" }
    , { symbol = "fmtImport", signature = "Env -> KGamma -> Gamma -> [String] -> String -> (Import) -> String" }
    , { symbol = "synopsis", signature = "Env -> KGamma -> Gamma -> String -> String" }
    , { symbol = "showModuleDoc", signature = "Env -> KGamma -> Gamma -> String -> String" }
    , { symbol = "splitModuleDoc", signature = "String -> (String,String)" }
    , { symbol = "fmtTypeDefTOC", signature = "(TypeDef,[Def]) -> [String]" }
    , { symbol = "fmtConTOC", signature = "ConInfo -> String" }
    , { symbol = "fmtDefTOC", signature = "Bool -> Def -> String" }
    , { symbol = "fmtPublicImport", signature = "Env -> KGamma -> Gamma -> Import -> String" }
    , { symbol = "fmtPrivateImports", signature = "Env -> KGamma -> Gamma -> [Import] -> String" }
    , { symbol = "fmtPrivateImport", signature = "Env -> KGamma -> Gamma -> Import -> String" }
    , { symbol = "fmtTypeDef", signature = "Env -> KGamma -> Gamma -> (TypeDef,[Def]) -> String" }
    , { symbol = "fmtDef", signature = "Env -> KGamma -> Gamma -> Def -> String" }
    , { symbol = "highlightType", signature = "Env -> KGamma -> Gamma -> String -> String" }
    , { symbol = "highlightInput", signature = "ColorScheme -> String -> IC.Fmt" }
    , { symbol = "fmtAttr", signature = "ColorScheme -> Token Lexeme -> Lexeme -> String -> IC.Fmt" }
    , { symbol = "highlightPrint", signature = "Printer p => ColorScheme -> FilePath -> Int -> BString -> p -> IO ()" }
    , { symbol = "fmtPrint", signature = "Printer p => ColorScheme -> p -> Token (Lexeme) -> Lexeme -> String -> IO ()" }
    , { symbol = "showLexeme", signature = "Lexeme -> String" }
    , { symbol = "normalize", signature = "String -> String" }
    , { symbol = "isKeywordOp", signature = "String -> Bool" }
    , { symbol = "commentFlatten", signature = "(String -> a) -> [TokenComment a] -> [a]" }
    , { symbol = "highlight", signature = "(Token Lexeme -> Lexeme -> String -> a) -> ([Lexeme] -> [Lexeme]) -> Context -> FilePath -> Int -> BString -> [a]" }
    , { symbol = "highlightLexemes", signature = "([Lexeme] -> [Lexeme]) -> (Token Lexeme -> Lexeme -> String -> a) -> Context -> [Lexeme] -> [a]" }
    , { symbol = "specialToKeywords", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "highlightLexeme", signature = "([Lexeme] -> [Lexeme]) -> (Token Lexeme -> Lexeme -> String -> a) -> Context -> Lexeme -> [Lexeme] -> (Context,a)" }
    , { symbol = "lexComment", signature = "FilePath -> Int -> String -> [TokenComment Lexeme]" }
    , { symbol = "lexSource", signature = "Bool -> Bool -> ([Lexeme]-> [Lexeme]) -> Int -> Source -> [Lexeme]" }
    , { symbol = "layout", signature = "Bool -> Bool -> [Lexeme] -> [Lexeme]" }
    , { symbol = "removeWhite", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "removeWhiteSpace", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "associateComments", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "combineLineComments", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "checkIds", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "checkComments", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "indentLayout", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "brace", signature = "Layout -> [Layout] -> Lexeme -> [Lexeme] -> [Lexeme]" }
    , { symbol = "insertLCurly", signature = "Lexeme -> [Lexeme]" }
    , { symbol = "insertRCurly", signature = "Layout -> Lexeme -> [Lexeme]" }
    , { symbol = "insertSemi", signature = "Lexeme -> [Lexeme]" }
    , { symbol = "isExprContinuation", signature = "Lex -> Lex -> Bool" }
    , { symbol = "isStartContinuationToken", signature = "Lex -> Bool" }
    , { symbol = "isEndContinuationToken", signature = "Lex -> Bool" }
    , { symbol = "lineLayout", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "semiInsert", signature = "Lexeme -> [Lexeme] -> [Lexeme]" }
    , { symbol = "identifyModules", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "replaceModules", signature = "(Name -> Bool) -> [Lexeme] -> [Lexeme]" }
    , { symbol = "scanImports", signature = "Int -> [Name] -> [Lex] -> ([Name],Int)" }
    , { symbol = "isTypeVar", signature = "Name -> Bool" }
    , { symbol = "lexemeIsWhite", signature = "Lexeme -> Bool" }
    , { symbol = "lexIsWhite", signature = "Lex -> Bool" }
    , { symbol = "sameLexeme", signature = "Lexeme -> Lexeme -> Bool" }
    , { symbol = "sameLex", signature = "Lex -> Lex -> Bool" }
    , { symbol = "showLexeme", signature = "Lexeme -> String" }
    , { symbol = "showLex", signature = "Lex -> String" }
    , { symbol = "lexImportNub", signature = "[LexImport] -> [LexImport]" }
    , { symbol = "parseLex", signature = "Lex -> LexParser Lexeme" }
    , { symbol = "parseProgramFromFile", signature = "HasCallStack => Bool -> Bool -> FilePath -> IO (Error a UserProgram)" }
    , { symbol = "logSyntaxWarnings", signature = "[(Range, Doc)] -> IO ()" }
    , { symbol = "parseProgramFromString", signature = "Bool -> Bool -> BString -> FilePath -> Error a UserProgram" }
    , { symbol = "parseValueDef", signature = "Bool -> FilePath -> Int -> String -> Error () UserDef" }
    , { symbol = "parseTypeDef", signature = "Bool -> FilePath -> Int -> String -> Error () (UserTypeDef,[UserDef])" }
    , { symbol = "parseType", signature = "Bool -> FilePath -> Int -> Name -> String -> Error () UserTypeDef" }
    , { symbol = "parseExpression", signature = "Bool -> FilePath -> Int -> Name -> String -> Error () UserDef" }
    , { symbol = "ignoreSyntaxWarnings", signature = "Error b (a, [(String, Range)]) -> Error b a" }
    , { symbol = "lexParseS", signature = "Bool -> (Source -> LexParser b) -> FilePath -> Int -> String -> Error a b" }
    , { symbol = "runStateParser", signature = "LexParser a -> SourceName -> [Lexeme] -> Either ParseError (a, [(String, Range)])" }
    , { symbol = "lexParse", signature = "Bool -> Bool -> ([Lexeme]-> [Lexeme]) -> (Source -> LexParser a) -> FilePath -> Int -> BString -> Error b ((a,[Lexeme]), [(String, Range)])" }
    , { symbol = "parseProgramFromLexemes", signature = "Source -> [Lexeme] -> Error () UserProgram" }
    , { symbol = "parseDependencies", signature = "Source -> [Lexeme] -> Error () [Import]" }
    , { symbol = "parseLexemes", signature = "LexParser a -> Source -> [Lexeme] -> Error () (a, [(String, Range)])" }
    , { symbol = "makeParseError", signature = "(ParseError -> Range) -> ParseError -> Error b a" }
    , { symbol = "errorRangeLexeme", signature = "[Lexeme] -> Source -> ParseError -> Range" }
    , { symbol = "interactive", signature = "LexParser a -> LexParser a" }
    , { symbol = "valueDefinition", signature = "LexParser UserDef" }
    , { symbol = "typeDefinition", signature = "LexParser (UserTypeDef,[UserDef])" }
    , { symbol = "expression", signature = "Name -> LexParser (UserDef) -- ,UserDef)" }
    , { symbol = "userType", signature = "Name -> LexParser UserTypeDef" }
    , { symbol = "program", signature = "Source -> LexParser UserProgram" }
    , { symbol = "pmodule", signature = "Source -> LexParser UserProgram" }
    , { symbol = "pmoduleDecl", signature = "FilePath -> LexParser (Visibility,String,ModuleName,Range)" }
    , { symbol = "pmoduleDeps", signature = "Source -> LexParser [Import]" }
    , { symbol = "pimportDecls", signature = "LexParser [Import]" }
    , { symbol = "semis0", signature = "LexParser a -> LexParser [a]" }
    , { symbol = "programBody", signature = "Visibility -> Source -> Name -> Range -> String -> LexParser UserProgram" }
    , { symbol = "includeStdCore", signature = "Name -> [Import] -> [Import]" }
    , { symbol = "topdef", signature = "Visibility -> LexParser [TopDef]" }
    , { symbol = "importDecl", signature = "LexParser Import" }
    , { symbol = "importAlias", signature = "LexParser (Name,Name,Range,Range)" }
    , { symbol = "visibility", signature = "Visibility -> LexParser (Visibility,Range)" }
    , { symbol = "parseInline", signature = "LexParser DefInline" }
    , { symbol = "externDecl", signature = "Visibility -> LexParser [TopDef]" }
    , { symbol = "externalImport", signature = "Range -> LexParser External" }
    , { symbol = "externalBody", signature = "LexParser ([(Target,ExternalCall)],Range)" }
    , { symbol = "fixDecl", signature = "LexParser FixDefs" }
    , { symbol = "aliasDecl", signature = "Visibility -> LexParser UserTypeDef" }
    , { symbol = "typeDecl,dataTypeDecl,structDecl", signature = "Visibility -> LexParser (UserTypeDef, [UserDef])" }
    , { symbol = "typeDeclKind", signature = "LexParser (DataKind,Range,String,DataDef)" }
    , { symbol = "constructor", signature = "Visibility -> [UserTypeBinder] -> UserType -> LexParser (UserCon UserType UserType UserKind, [UserDef])" }
    , { symbol = "makeUserCon", signature = "Name -> [UserTypeBinder] -> UserType -> [UserTypeBinder] -> [(Visibility,ValueBinder UserType (Maybe UserExpr))] -> Maybe (Fip,UserExpr) -> Range -> Range -> Visibility -> String -> (UserCon UserType UserType UserKind, [UserDef])" }
    , { symbol = "bindExprToVal", signature = "Name -> Range -> UserExpr -> (UserExpr -> UserExpr, [ValueBinder t e] -> UserExpr)" }
    , { symbol = "parseEffectDecl", signature = "Visibility -> LexParser EffectDecl" }
    , { symbol = "makeEffectDecl", signature = "EffectDecl -> [TopDef]" }
    , { symbol = "effectDecl", signature = "Visibility -> LexParser [TopDef]" }
    , { symbol = "parseOpDecl", signature = "Bool -> Visibility -> LexParser OpDecl" }
    , { symbol = "parseValOpDecl", signature = "Visibility -> LexParser OpDecl" }
    , { symbol = "parseFunOpDecl", signature = "Bool -> Visibility -> LexParser OpDecl" }
    , { symbol = "declParams", signature = "Bool -> Bool -> LexParser ([ValueBinder UserType (Maybe UserExpr)],[ParamInfo],Range)" }
    , { symbol = "paramInfo", signature = "LexParser ParamInfo" }
    , { symbol = "operationDecl", signature = "Int -> Visibility -> [UserTypeBinder] -> [UserTypeBinder] ->" }
    , { symbol = "pureDecl", signature = "Visibility -> LexParser UserDef" }
    , { symbol = "parseFipAlloc", signature = "LexParser (FipAlloc,Range)" }
    , { symbol = "parseTailFip", signature = "LexParser (Fip,Range)" }
    , { symbol = "parseFip", signature = "LexParser (Fip,Range)" }
    , { symbol = "parseFipEx", signature = "Range -> Bool -> LexParser (Fip,Range)" }
    , { symbol = "funDef", signature = "Bool -> Bool -> LexParser ([TypeBinder UserKind],[ValueBinder (Maybe UserType) (Maybe UserExpr)], [ParamInfo], Range, Maybe (Maybe UserType, UserType),[UserType], UserExpr -> UserExpr)" }
    , { symbol = "annotRes", signature = "LexParser (Maybe (Maybe UserType,UserType))" }
    , { symbol = "annotResult", signature = "LexParser (UserType,UserType)" }
    , { symbol = "parameters", signature = "Bool -> Bool -> Bool -> LexParser ([(ValueBinder (Maybe UserType) (Maybe UserExpr))], [ParamInfo], UserExpr -> UserExpr, Range)" }
    , { symbol = "parameter", signature = "Bool -> Bool -> Bool -> LexParser (ValueBinder (Maybe UserType) (Maybe UserExpr), ParamInfo, UserExpr -> UserExpr)" }
    , { symbol = "parNormal", signature = "Bool -> LexParser (ValueBinder (Maybe UserType) (Maybe UserExpr), UserExpr -> UserExpr)" }
    , { symbol = "parImplicit", signature = "LexParser (ValueBinder (Maybe UserType) (Maybe UserExpr), UserExpr -> UserExpr)" }
    , { symbol = "block", signature = "LexParser UserExpr" }
    , { symbol = "statement", signature = "LexParser Statement" }
    , { symbol = "withstat", signature = "LexParser (UserExpr -> UserExpr)" }
    , { symbol = "typeAnnotation", signature = "LexParser (UserExpr -> UserExpr)" }
    , { symbol = "bodyexpr", signature = "LexParser UserExpr" }
    , { symbol = "blockexpr", signature = "LexParser UserExpr   -- like expr but a block `{..}` is interpreted as statements" }
    , { symbol = "expr", signature = "LexParser UserExpr" }
    , { symbol = "basicexpr", signature = "LexParser UserExpr" }
    , { symbol = "withexpr", signature = "LexParser UserExpr" }
    , { symbol = "valexpr", signature = "LexParser UserExpr" }
    , { symbol = "matchexpr", signature = "LexParser UserExpr" }
    , { symbol = "handlerClauses", signature = "Range -> Maybe UserType -> HandlerScope -> HandlerOverride -> HandlerSort -> LexParser UserExpr" }
    , { symbol = "applyMaybe", signature = "Range -> Maybe UserExpr -> Maybe UserExpr -> UserExpr -> UserExpr" }
    , { symbol = "extractBinders", signature = "[(Clause, Maybe (UserExpr -> UserExpr))] -> ([Clause], UserExpr -> UserExpr)" }
    , { symbol = "partitionClauses", signature = "[Clause] -> Range -> LexParser (Maybe UserExpr,Maybe UserExpr,Maybe UserExpr,[UserHandlerBranch])" }
    , { symbol = "opClauses", signature = "LexParser ([(Clause, Maybe (UserExpr -> UserExpr))],Range)" }
    , { symbol = "handlerOpX", signature = "LexParser (Clause, Maybe (UserExpr -> UserExpr))" }
    , { symbol = "handlerOp", signature = "LexParser (Clause, Maybe (UserExpr -> UserExpr))" }
    , { symbol = "opParams", signature = "LexParser ([ValueBinder (Maybe UserType) ()],Range)" }
    , { symbol = "opParam", signature = "LexParser (ValueBinder (Maybe UserType) ())" }
    , { symbol = "handlerPar", signature = "LexParser (ValueBinder (Maybe UserType) ())" }
    , { symbol = "handlerReturnDefault", signature = "Range -> UserExpr" }
    , { symbol = "guards", signature = "LexParser [UserGuard]" }
    , { symbol = "ntlexpr", signature = "LexParser UserExpr -- non-trailing-lambda expression" }
    , { symbol = "opexprx", signature = "Bool -> LexParser UserExpr" }
    , { symbol = "etaExpand", signature = "UserExpr -> UserExpr" }
    , { symbol = "etaTransform", signature = "UserExpr -> (UserExpr, [ValueBinder (Maybe UserType) (Maybe UserExpr)])" }
    , { symbol = "prefixexpr", signature = "Bool -> LexParser UserExpr" }
    , { symbol = "appexpr", signature = "Bool -> LexParser UserExpr" }
    , { symbol = "argument", signature = "LexParser (Maybe (Name,Range),UserExpr)" }
    , { symbol = "atom", signature = "LexParser UserExpr" }
    , { symbol = "idcon", signature = "LexParser UserExpr" }
    , { symbol = "tupleExpr", signature = "LexParser UserExpr" }
    , { symbol = "listExpr", signature = "LexParser UserExpr" }
    , { symbol = "cctxExpr", signature = "LexParser UserExpr" }
    , { symbol = "ccontext", signature = "Range -> LexParser UserExpr" }
    , { symbol = "cctxHole", signature = "LexParser UserExpr" }
    , { symbol = "etaHole", signature = "LexParser UserExpr" }
    , { symbol = "injectExpr", signature = "LexParser UserExpr" }
    , { symbol = "injectType", signature = "LexParser (Range, UserExpr -> UserExpr)" }
    , { symbol = "pbinder", signature = "Range -> LexParser (UserExpr -> ValueBinder () UserExpr)" }
    , { symbol = "pattern", signature = "LexParser UserPattern" }
    , { symbol = "patAtom", signature = "LexParser UserPattern" }
    , { symbol = "namedPattern", signature = "LexParser (Maybe (Name,Range),UserPattern)" }
    , { symbol = "maybeTypeAnnot", signature = "a -> (UserType -> a) -> LexParser a" }
    , { symbol = "listPattern", signature = "LexParser UserPattern" }
    , { symbol = "makeNilPat", signature = "Range -> UserPattern" }
    , { symbol = "makeConsPat", signature = "UserPattern -> UserPattern -> UserPattern" }
    , { symbol = "typeAnnot", signature = "LexParser UserType" }
    , { symbol = "typeAnnotPar", signature = "LexParser UserType" }
    , { symbol = "ptypescheme", signature = "LexParser UserType" }
    , { symbol = "ptype", signature = "LexParser UserType" }
    , { symbol = "tarrow", signature = "LexParser UserType" }
    , { symbol = "textend", signature = "LexParser (Range -> UserType, Range {- "|" -})" }
    , { symbol = "tresultTotal", signature = "LexParser (UserType,UserType)" }
    , { symbol = "tresult", signature = "LexParser (Maybe UserType,UserType)" }
    , { symbol = "tatomic", signature = "LexParser UserType" }
    , { symbol = "tuple", signature = "([(Name,UserType)],Range) -> UserType" }
    , { symbol = "tatom", signature = "LexParser ([(Name,UserType)],Range)" }
    , { symbol = "paramType", signature = "LexParser (Name,Range,UserType)" }
    , { symbol = "tbinderDef", signature = "LexParser (UserKind -> TypeBinder UserKind)" }
    , { symbol = "tbinders", signature = "LexParser [TypeBinder UserKind]" }
    , { symbol = "tbinder", signature = "LexParser (TypeBinder UserKind)" }
    , { symbol = "kindAnnot", signature = "LexParser UserKind" }
    , { symbol = "pkind", signature = "LexParser UserKind" }
    , { symbol = "semiBraces", signature = "LexParser a -> LexParser [a]" }
    , { symbol = "semiBracesRanged", signature = "LexParser a -> LexParser ([a],Range)" }
    , { symbol = "semiBracesRanged1", signature = "LexParser a -> LexParser ([a],Range)" }
    , { symbol = "semiBraced", signature = "LexParser a -> LexParser a" }
    , { symbol = "liparen", signature = "LexParser Range" }
    , { symbol = "semiColon", signature = "LexParser Range" }
    , { symbol = "lcurly", signature = "LexParser Range" }
    , { symbol = "rcurly", signature = "LexParser Range" }
    , { symbol = "qidentifier", signature = "LexParser (Name,Range)" }
    , { symbol = "qconstructor", signature = "LexParser (Name,Range)" }
    , { symbol = "qoperator", signature = "LexParser (Name,Range)" }
    , { symbol = "varid", signature = "LexParser (Name,Range)" }
    , { symbol = "idop", signature = "LexParser (Name,Range)" }
    , { symbol = "conid", signature = "LexParser (Name,Range)" }
    , { symbol = "op", signature = "LexParser (Name,Range)" }
    , { symbol = "typeid", signature = "LexParser (Name,Range)" }
    , { symbol = "ensureUnqualified", signature = "String -> LexParser (Name,a) -> LexParser (Name,a)" }
    , { symbol = "qop", signature = "LexParser (Name,Range)" }
    , { symbol = "prefixOp", signature = "LexParser UserExpr" }
    , { symbol = "qvarid", signature = "LexParser (Name,Range)" }
    , { symbol = "qidop", signature = "LexParser (Name,Range)" }
    , { symbol = "qconid", signature = "LexParser (Name,Range)" }
    , { symbol = "modulepath", signature = "LexParser (Name,Range)" }
    , { symbol = "wildcard", signature = "LexParser (Name,Range)" }
    , { symbol = "integer", signature = "LexParser (Integer,Range)" }
    , { symbol = "floatLit", signature = "LexParser (Double,Range)" }
    , { symbol = "charLit", signature = "LexParser (Char,Range)" }
    , { symbol = "stringLit", signature = "LexParser (String,Range)" }
    , { symbol = "specialOp", signature = "String -> LexParser Range" }
    , { symbol = "specialId", signature = "String -> LexParser Range" }
    , { symbol = "specialConId", signature = "String -> LexParser Range" }
    , { symbol = "special", signature = "String -> LexParser Range" }
    , { symbol = "specialIdOr", signature = "String -> [String] -> LexParser Range" }
    , { symbol = "keywordOr", signature = "String -> [String] -> LexParser Range" }
    , { symbol = "dockeywordOr", signature = "String -> [String] -> LexParser (Range,String)" }
    , { symbol = "keyword", signature = "String -> LexParser Range" }
    , { symbol = "dockeyword", signature = "String -> LexParser (Range,String)" }
    , { symbol = "docconid", signature = "LexParser (Name,(Range,String))" }
    , { symbol = "pwarning", signature = "String -> Range -> LexParser ()" }
    , { symbol = "uniqueRngHiddenName", signature = "Range -> String -> Name" }
    , { symbol = "uniqueRngName", signature = "Range -> String -> Name" }
    , { symbol = "adjustRange", signature = "Range -> UserExpr -> UserExpr" }
    , { symbol = "adjustTpRange", signature = "Range -> UserType -> UserType" }
    , { symbol = "allDefs", signature = "S.DefGroup t -> [S.Def t]" }
    , { symbol = "ppLit", signature = "S.Lit -> String" }
    , { symbol = "ppVis", signature = "Env -> Visibility -> Doc" }
    , { symbol = "ppSyntaxDef", signature = "PrettyEnv t => Env -> S.Def t -> Doc" }
    , { symbol = "ppSyntaxDefUserType", signature = "Env -> S.Def UserType -> Doc" }
    , { symbol = "ppSyntaxExtern", signature = "Env -> S.External -> Doc" }
    , { symbol = "returnType", signature = "UserType -> Maybe (UserType, UserType)" }
    , { symbol = "ppFunDef", signature = "Env -> ValueBinder () (S.Expr UserType) -> Doc" }
    , { symbol = "ppValBinder", signature = "PrettyEnv t => Env -> ValueBinder () (S.Expr t) -> Doc" }
    , { symbol = "ppMValBinder", signature = "PrettyEnv t => Env -> ValueBinder (Maybe t) (Maybe (S.Expr t)) -> Doc" }
    , { symbol = "ppValEmptyBinder", signature = "PrettyEnv t => Env -> ValueBinder (Maybe t) () -> Doc" }
    , { symbol = "ppPatBinder", signature = "PrettyEnv t => Env -> ValueBinder (Maybe t) (S.Pattern t) -> Doc" }
    , { symbol = "ppArg", signature = "PrettyEnv t => Env -> (Maybe (Name,Range),S.Expr t) -> Doc" }
    , { symbol = "isConstructorApp", signature = "S.Expr t -> Bool" }
    , { symbol = "ppSyntaxExpr", signature = "PrettyEnv t => Env -> S.Expr t -> Doc" }
    , { symbol = "ppHandlerBranch", signature = "PrettyEnv t => Env -> S.HandlerBranch t -> Doc" }
    , { symbol = "ppMaybeExpr", signature = "PrettyEnv t => Env -> Maybe (Expr t) -> Doc" }
    , { symbol = "contains", signature = "Eq a => [a] -> [a] -> Bool" }
    , { symbol = "ppSyntaxBranch", signature = "PrettyEnv t => Env -> S.Branch t -> Doc" }
    , { symbol = "ppSyntaxPattern", signature = "PrettyEnv t => Env -> S.Pattern t -> Doc" }
    , { symbol = "ppPatternArg", signature = "PrettyEnv t => Env -> (Maybe (Name,Range), S.Pattern t) -> Doc" }
    , { symbol = "alwaysTrue", signature = "Expr t -> Bool" }
    , { symbol = "ppSyntaxGuard", signature = "PrettyEnv t => Env -> S.Guard t -> Doc" }
    , { symbol = "promoteType", signature = "UserType -> UserType" }
    , { symbol = "promoteFree", signature = "[UserTypeBinder] -> [UserType] -> [UserTypeBinder]" }
    , { symbol = "promote", signature = "[TypeBinder UserKind] -> [TypeBinder UserKind] -> [UserType] -> Maybe (Maybe UserType,UserType) -> UserExpr -> UserExpr" }
    , { symbol = "promoteEx", signature = "[TypeBinder UserKind] -> [TypeBinder UserKind] -> [UserType] -> [(Name,Either UserType Range)] -> UserExpr -> UserExpr" }
    , { symbol = "isSomeVar", signature = "Name -> Bool" }
    , { symbol = "extend", signature = "UserType -> UserType" }
    , { symbol = "extract", signature = "UserType -> ([UserType],UserType)" }
    , { symbol = "quantify", signature = "UserQuantifier -> [UserTypeBinder] -> UserType -> UserType" }
    , { symbol = "toTypeBinder", signature = "Name -> UserTypeBinder" }
    , { symbol = "promoteTVars", signature = "S.NameSet -> (Name,Either UserType Range) -> (Name,Either UserType Range)" }
    , { symbol = "promoteTpVars", signature = "S.NameSet -> UserType -> UserType" }
    , { symbol = "mangleConName", signature = "Name -> Name" }
    , { symbol = "mangleTypeName", signature = "Name -> Name" }
    , { symbol = "mangle", signature = "Name -> Type -> Name" }
    , { symbol = "rinfoIsVirtual", signature = "RangeInfo -> Bool" }
    , { symbol = "penalty", signature = "Name -> Int" }
    , { symbol = "rangeMapNew", signature = "RangeMap" }
    , { symbol = "rangeMapInsert", signature = "Range -> RangeInfo -> RangeMap -> RangeMap" }
    , { symbol = "rangeMapAppend", signature = "RangeMap -> RangeMap -> RangeMap" }
    , { symbol = "rangeMapSort", signature = "RangeMap -> RangeMap" }
    , { symbol = "prioritize", signature = "[(Range,RangeInfo)] -> [(Range,RangeInfo)]" }
    , { symbol = "mergeImplicits", signature = "Bool -> [(Range,RangeInfo)] -> [(Range,RangeInfo)]" }
    , { symbol = "rangeMapLookup", signature = "Range -> RangeMap -> ([(Range,RangeInfo)],RangeMap)" }
    , { symbol = "rangeMapFindIn", signature = "Bool -> Range -> RangeMap -> [(Range, RangeInfo)]" }
    , { symbol = "previousLexemesReversed", signature = "[Lexeme] -> Pos -> [Lexeme]" }
    , { symbol = "dropMatchedParensReverse", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "getFunctionIncompleteReverse", signature = "[Lexeme] -> FnSyntax" }
    , { symbol = "getFunctionNameReverse", signature = "[Lexeme] -> FnSyntax" }
    , { symbol = "chain", signature = "Lexeme -> FnSyntax -> FnSyntax" }
    , { symbol = "getCurrentStatementReverse", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "getCurrentBlockReverse", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "dropToLexMatching", signature = "(Lex -> Bool) -> (Lex -> Bool) -> [Lexeme] -> [Lexeme]" }
    , { symbol = "dropToLexMatchingN", signature = "Int -> (Lex -> Bool) -> (Lex -> Bool) -> [Lexeme] -> [Lexeme]" }
    , { symbol = "dropAutoGenClosing", signature = "[Lexeme] -> [Lexeme]" }
    , { symbol = "rangeMapFindAt", signature = "[Lexeme] -> Pos -> RangeMap -> Maybe (Range, RangeInfo)" }
    , { symbol = "lexemesFromPos", signature = "Pos -> [Lexeme] -> [Lexeme]" }
    , { symbol = "rangeMapFind", signature = "Range -> RangeMap -> [(Range, RangeInfo)]" }
    , { symbol = "minimumByList", signature = "Foldable t => (a -> a -> Ordering) -> t a -> [a]" }
    , { symbol = "rangeInfoType", signature = "RangeInfo -> Maybe Type" }
    , { symbol = "rangeInfoDoc", signature = "RangeInfo -> Maybe String" }
    , { symbol = "litRange", signature = "Lit -> Range" }
    , { symbol = "stripExpr", signature = "Expr t -> Expr t" }
    , { symbol = "defBody", signature = "Def t -> Expr t" }
    , { symbol = "defName", signature = "Def t -> Name" }
    , { symbol = "defType", signature = "Def t -> Maybe t" }
    , { symbol = "programNull", signature = "Name -> Program t k" }
    , { symbol = "preludeImport", signature = "Import" }
    , { symbol = "makeProgram", signature = "Name -> [TypeDef t t k] -> (Defs t) -> Program t k" }
    , { symbol = "programAddImports", signature = "Program t k -> [Import] -> Program t k" }
    , { symbol = "programAddDefs", signature = "Program t k -> [TypeDef t t k] -> (Defs t) -> Program t k" }
    , { symbol = "programRemoveAllDefs", signature = "Program t k -> Program t k" }
    , { symbol = "programRemoveDef", signature = "Name -> Program t k -> Program t k" }
    , { symbol = "programFind", signature = "Ranged t => Name -> Program t k -> Maybe Range" }
    , { symbol = "infoSort", signature = "NameInfo -> String" }
    , { symbol = "infoScopeDepth", signature = "NameInfo -> Int" }
    , { symbol = "infoCanonicalName", signature = "Name -> NameInfo -> Name" }
    , { symbol = "isInfoImport", signature = "NameInfo -> Bool" }
    , { symbol = "isInfoCon", signature = "NameInfo -> Bool" }
    , { symbol = "isInfoVal", signature = "NameInfo -> Bool" }
    , { symbol = "isInfoValFunExt", signature = "NameInfo -> Bool" }
    , { symbol = "isInfoFun", signature = "NameInfo -> Bool" }
    , { symbol = "infoAllowImplictMask", signature = "NameInfo -> Bool" }
    , { symbol = "isInfoFunOrExternal", signature = "NameInfo -> Bool" }
    , { symbol = "fipFromNameInfo", signature = "NameInfo -> Fip" }
    , { symbol = "infoDocString", signature = "NameInfo -> String" }
    , { symbol = "infoElement", signature = "NameInfo -> String" }
    , { symbol = "infoIsVisible", signature = "NameInfo -> Bool" }
    , { symbol = "coreVarInfoFromNameInfo", signature = "NameInfo -> Core.VarInfo" }
    , { symbol = "gammaInit", signature = "Gamma" }
    , { symbol = "gammaIsEmpty", signature = "Gamma -> Bool" }
    , { symbol = "gammaSchemes", signature = "Gamma -> [Scheme]" }
    , { symbol = "gammaEmpty", signature = "Gamma" }
    , { symbol = "gammaSingle", signature = "Name -> NameInfo -> Gamma" }
    , { symbol = "gammaNew", signature = "[(Name,NameInfo)] -> Gamma" }
    , { symbol = "gammaExtends", signature = "[(Name,NameInfo)] -> Gamma -> Gamma" }
    , { symbol = "gammaExtend", signature = "Name -> NameInfo -> Gamma -> Gamma" }
    , { symbol = "combine", signature = "[(Name,NameInfo)] -> [(Name,NameInfo)] -> [(Name,NameInfo)]" }
    , { symbol = "gammaLookupCanonical", signature = "Name -> Gamma -> [NameInfo]" }
    , { symbol = "gammaLookupExactCon", signature = "Name -> Gamma -> [NameInfo]" }
    , { symbol = "gammaLookupQ", signature = "Name -> Gamma -> [NameInfo]" }
    , { symbol = "gammaLookup", signature = "Name -> Gamma -> [(Name,NameInfo)]" }
    , { symbol = "matchQualifiers", signature = "Name -> Name -> Bool" }
    , { symbol = "matchRevQualifierPaths", signature = "([String],[String]) -> ([String],[String]) -> Bool" }
    , { symbol = "splitRevQualifiers", signature = "Name -> ([String],[String])" }
    , { symbol = "gammaLookupPrefix", signature = "Name -> Gamma -> [(Name,NameInfo)]" }
    , { symbol = "gammaMap", signature = "(NameInfo -> NameInfo) -> Gamma -> Gamma" }
    , { symbol = "gammaList", signature = "Gamma -> [(Name,NameInfo)]" }
    , { symbol = "gammaRemove", signature = "Name -> Gamma -> Gamma" }
    , { symbol = "gammaUnion", signature = "Gamma -> Gamma -> Gamma" }
    , { symbol = "gammaUnions", signature = "[Gamma] -> Gamma" }
    , { symbol = "gammaFilter", signature = "Name -> Gamma -> Gamma" }
    , { symbol = "gammaNames", signature = "Gamma -> [Name]" }
    , { symbol = "gammaPublicNames", signature = "Gamma -> [Name]" }
    , { symbol = "extractGammaImports", signature = "[(Name,Name)] -> Name -> Gamma" }
    , { symbol = "extractGamma", signature = "(DataInfo -> Bool) -> Bool -> Core.Core -> Gamma" }
    , { symbol = "extractTypeDef", signature = "(DataInfo -> Bool) -> (Visibility -> Visibility) -> Core.TypeDef -> Gamma" }
    , { symbol = "coreDefInfo", signature = "Core.Def -> Int -> (Name,NameInfo)" }
    , { symbol = "createNameInfoX", signature = "Visibility -> Name -> Int -> DefSort -> Range -> Type -> String -> NameInfo" }
    , { symbol = "createNameInfoEx", signature = "Visibility -> Name -> Int -> DefSort -> Bool -> Range -> Type -> String -> NameInfo" }
    , { symbol = "getArity", signature = "Type -> (Int,Int)" }
    , { symbol = "ppGammaInternal", signature = "Bool -> Env -> Gamma -> Doc" }
    , { symbol = "ppGamma", signature = "Env -> Gamma -> Doc" }
    , { symbol = "ppGammaHidden", signature = "Env -> Gamma -> Doc" }
    , { symbol = "showHidden", signature = "Gamma -> String" }
    , { symbol = "infgammaEmpty", signature = "InfGamma" }
    , { symbol = "infgammaIsEmpty", signature = "InfGamma -> Bool" }
    , { symbol = "infgammaSingle", signature = "Name -> Scheme -> Int -> String -> InfGamma" }
    , { symbol = "infgammaNew", signature = "[(Name,Scheme,Int,String)] -> InfGamma" }
    , { symbol = "infgammaExtend", signature = "Name -> NameInfo -> InfGamma -> InfGamma" }
    , { symbol = "combine", signature = "[NameInfo] -> [NameInfo] -> [NameInfo]" }
    , { symbol = "infgammaExtends", signature = "[(Name,Scheme,Int,String)] -> InfGamma -> InfGamma" }
    , { symbol = "infgammaExtendTp", signature = "Name -> Name -> Scheme -> Int -> String -> InfGamma -> InfGamma" }
    , { symbol = "infgammaExtendX", signature = "Name -> Name -> Scheme -> Int -> Range -> Bool -> String -> InfGamma -> InfGamma" }
    , { symbol = "infgammaLookup", signature = "Name -> InfGamma -> Either [(Name,NameInfo)] (Name,NameInfo)" }
    , { symbol = "infgammaLookupEx", signature = "(NameInfo -> Bool) -> Name -> InfGamma -> Either [(Name,NameInfo)] (Name,NameInfo)" }
    , { symbol = "infgammaMap", signature = "(Scheme -> Scheme) -> InfGamma -> InfGamma" }
    , { symbol = "infgammaList", signature = "InfGamma -> [(Name,Scheme)]" }
    , { symbol = "infgammaUnion", signature = "InfGamma -> InfGamma -> InfGamma" }
    , { symbol = "infgammaUnions", signature = "[InfGamma] -> InfGamma" }
    , { symbol = "infgammaDelete", signature = "Name -> InfGamma -> InfGamma" }
    , { symbol = "ppInfGamma", signature = "Env -> InfGamma -> Doc" }
    , { symbol = "inferTypes", signature = "Env -> Maybe RM.RangeMap -> Synonyms -> Newtypes -> Constructors -> ImportMap -> Gamma -> Name -> Bool -> DefGroups Type" }
    , { symbol = "inferDefGroups", signature = "Bool -> DefGroups Type -> Inf (Gamma,Core.DefGroups)" }
    , { symbol = "inferDefGroupX", signature = "HasCallStack => Bool -> DefGroup Type -> Inf (Gamma,Core.DefGroups) -> Inf (Gamma,Core.DefGroups)" }
    , { symbol = "traceCoreDefs", signature = "[Core.Def] -> Inf ()" }
    , { symbol = "traceCoreDefGroups", signature = "[Core.DefGroup] -> Inf ()" }
    , { symbol = "inferDefGroup", signature = "Bool -> DefGroup Type -> Inf a -> Inf ([Core.DefGroup], a)" }
    , { symbol = "checkRecVal", signature = "Core.DefGroup -> Inf ()" }
    , { symbol = "fixCanonicalName", signature = "Bool -> Core.Def -> Inf Core.Def" }
    , { symbol = "mapMDefs", signature = "Monad m => (Core.Def -> m Core.Def) -> Core.DefGroups -> m Core.DefGroups" }
    , { symbol = "mapMDefs_", signature = "Monad m => (Core.Def -> m ()) -> Core.DefGroups -> m ()" }
    , { symbol = "addDivergentEffect", signature = "[Core.Def] -> Inf [Core.Def]" }
    , { symbol = "unskolemize", signature = "Type -> Inf Type" }
    , { symbol = "inferRecDef2", signature = "Bool -> Core.Def -> Bool -> (Def Type,Maybe (Name,Type)) -> Inf (Core.Def)" }
    , { symbol = "inferDef", signature = "Bool -> Expect -> Def Type -> Inf Core.Def" }
    , { symbol = "isAnnotatedBinder", signature = "ValueBinder (Maybe Type) x -> Bool" }
    , { symbol = "inferBindDef", signature = "Def Type -> Inf (Type,Effect,Core.Def)" }
    , { symbol = "inferIsolated", signature = "Range -> Range -> Expr a -> Inf (Type,Effect,Core.Expr) -> Inf (Type,Effect,Core.Expr)" }
    , { symbol = "inferExpr", signature = "HasCallStack => Maybe (Type,Range) -> Expect -> Expr Type -> Inf (Type,Effect,Core.Expr)" }
    , { symbol = "inferHandler", signature = "Maybe (Type,Range) -> Expect -> HandlerSort -> HandlerScope -> Bool" }
    , { symbol = "inferHandledEffect", signature = "Range -> HandlerSort -> Maybe Effect -> [HandlerBranch Type] -> Inf (Effect)" }
    , { symbol = "checkCoverage", signature = "Range -> Effect -> Name -> [HandlerBranch Type] -> Inf ()" }
    , { symbol = "effectNameCore", signature = "Effect -> Range -> Inf (Maybe Core.Expr,Name)" }
    , { symbol = "effectNameFromLabel", signature = "Effect -> Name" }
    , { symbol = "inferApp", signature = "Maybe (Type,Range) -> Expect -> Expr Type -> [(Maybe (Name,Range),Expr Type)] -> Range -> Inf (Type,Effect,Core.Expr)" }
    , { symbol = "getRangeArg", signature = "ArgExpr -> Range" }
    , { symbol = "inferLam", signature = "HasCallStack => Bool -> Maybe (Type,Range) -> Expect -> [ValueBinder (Maybe Type) (Maybe (Expr Type))] -> Expr Type -> Range -> Inf (Type,Effect,Core.Expr)" }
    , { symbol = "inferVar", signature = "HasCallStack => Maybe (Type,Range) -> Expect -> Name -> Range -> Bool -> Inf (Type,Effect,Core.Expr)" }
    , { symbol = "compilationConstants", signature = "[(Name,(Type,Name -> Range -> Core.Expr))]" }
    , { symbol = "etaExpand", signature = "Int -> Range -> ((Expr t -> Expr t) -> Expr t) -> Expr t" }
    , { symbol = "inferCase", signature = "Maybe (Type, Range) -> Expect -> Expr Type -> [Branch Type] -> Bool -> Range -> Inf (Type, Effect, Core.Expr)" }
    , { symbol = "getTypeName", signature = "Type -> Name" }
    , { symbol = "inferBranch", signature = "PatternKind -> Maybe (Type,Range) -> Type -> Range -> [Name] -> Branch Type -> Inf ([(Type,Effect)],Core.Branch)" }
    , { symbol = "inferGuard", signature = "Maybe (Type,Range) -> Range -> Guard Type -> Inf ((Type,Effect),Core.Guard)" }
    , { symbol = "inferPatternNested", signature = "PatternKind -> Type -> Range -> Pattern Type -> Inf (Core.Pattern,[(Name,NameInfo)])" }
    , { symbol = "inferPattern", signature = "HasTypeVar a => PatternKind -> Type -> Range -> Pattern Type -> (Core.Pattern -> a -> Inf b)" }
    , { symbol = "splitConTp", signature = "Type -> ([(Name,Type)],Effect,Type)" }
    , { symbol = "inferBinders", signature = "Int -> [(Name,NameInfo)] -> [ValueBinder Type ()] -> [(Name,NameInfo)]" }
    , { symbol = "inferImplicitParam", signature = "ValueBinder t1 (Maybe (Expr t2)) -> Inf (ValueBinder t1 (Maybe (Expr t2)), Expr Type -> Expr Type)" }
    , { symbol = "qualifyUnpacked", signature = "Name -> Name -> Name" }
    , { symbol = "inferImplicitUnpack", signature = "Range -> Range -> Name -> Name -> Inf (Expr Type -> Expr Type)" }
    , { symbol = "inferOptionals", signature = "Int -> Bool -> Effect -> [(Name,NameInfo)] -> [ValueBinder Type (Maybe (Expr Type))] -> Inf ([(Name,NameInfo)],[(Core.TName,Core.Expr)],[Core.Def])" }
    , { symbol = "inferArgsN", signature = "HasCallStack => Context -> Range -> [(Type,ArgExpr)] -> Inf ([Effect],[Core.Expr])" }
    , { symbol = "etaExpandVarArg", signature = "Type -> Expr Type -> Inf (Expr Type)" }
    , { symbol = "etaExpandExpr", signature = "Name -> Range -> Int -> [(Name,Type)] -> Type -> ([(Maybe (Name,Range),Expr Type)] -> Range -> Expr Type) -> Expr Type -> Inf (Expr Type)" }
    , { symbol = "splitNamedArgs", signature = "Ranged e => [(Maybe (Name,Range),e)] -> Inf ([e],[((Name,Range),e)])" }
    , { symbol = "matchPatterns", signature = "Range -> Range -> Type -> [(Name,Type)] -> [(Maybe (Name,Range),Pattern Type)] -> Inf [Pattern Type]" }
    , { symbol = "ppArgExpr", signature = "Env -> ArgExpr -> Doc" }
    , { symbol = "matchFunTypeArgs", signature = "Range -> Expr Type -> Type -> [(Int,FixedArg)] -> [Expr Type] -> [((Name,Range),Expr Type)]" }
    , { symbol = "addTopMorphisms", signature = "Range -> [(Range,Effect)] -> Inf Effect" }
    , { symbol = "instantiateBinder", signature = "ValueBinder (Maybe Type) e -> Inf (ValueBinder Type e)" }
    , { symbol = "maybeGeneralize", signature = "HasCallStack => Range -> Range -> Expect -> Inf (Type,Effect,Core.Expr) -> Inf (Scheme,Effect,Core.Expr)" }
    , { symbol = "maybeInstantiate", signature = "HasCallStack => Range -> Expect -> Scheme -> Inf (Rho,Core.Expr -> Core.Expr)" }
    , { symbol = "maybeInstantiateOrGeneralize", signature = "HasCallStack => Range -> Range -> Expect -> Inf (Type,Effect,Core.Expr) -> Inf (Type,Effect,Core.Expr)" }
    , { symbol = "matchFun", signature = "HasCallStack => Int -> Maybe (Type,Range) -> Inf ([Maybe (Name,Type)],Maybe (Type,Range), Maybe (Type,Range), [TypeVar], Expect)" }
    , { symbol = "monotonic", signature = "[Int] -> Bool" }
    , { symbol = "find", signature = "Range -> M.Map Range a -> a" }
    , { symbol = "coreVector", signature = "Type -> [Core.Expr] -> Inf Core.Expr" }
    , { symbol = "coreList", signature = "Type -> [Core.Expr] -> Inf Core.Expr" }
    , { symbol = "usesLocals", signature = "S.NameSet -> Expr Type -> Bool" }
    , { symbol = "usesLocalsDefs", signature = "S.NameSet -> DefGroup Type -> Bool" }
    , { symbol = "usesLocalsDef", signature = "S.NameSet -> Def Type -> Bool" }
    , { symbol = "usesLocalsBranch", signature = "S.NameSet -> Branch Type -> Bool" }
    , { symbol = "usesLocalsGuard", signature = "S.NameSet -> Guard Type -> Bool" }
    , { symbol = "usesLocalsMb", signature = "S.NameSet -> Maybe (Expr Type) -> Bool" }
    , { symbol = "usesLocalsOp", signature = "S.NameSet -> HandlerBranch Type -> Bool" }
    , { symbol = "shortCircuit", signature = "Core.Expr -> [Core.Expr] -> Maybe Core.Expr" }
    , { symbol = "generalize", signature = "HasCallStack => Range -> Range -> Bool -> Inf (Rho,Effect,Core.Expr) -> Inf (Scheme,Effect,Core.Expr)" }
    , { symbol = "generalizeX", signature = "HasCallStack => Range -> Range -> Bool -> (Rho,Effect,Core.Expr) -> Inf (Scheme,Effect,Core.Expr )" }
    , { symbol = "improve", signature = "Range -> Range -> Bool -> Effect -> Rho -> Inf (Rho,Effect,Core.Expr -> Core.Expr )" }
    , { symbol = "instantiate", signature = "Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)" }
    , { symbol = "instantiateEx", signature = "Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)" }
    , { symbol = "instantiateNoEx", signature = "Range -> Scheme -> Inf (Rho,[TypeVar],Core.Expr -> Core.Expr)" }
    , { symbol = "isolate", signature = "Range -> Bool -> Tvs -> [ImplicitConstraint] -> Effect -> Inf ([ImplicitConstraint], Effect, Core.Expr -> Core.Expr)" }
    , { symbol = "normalize", signature = "Bool -> Rho -> Inf Rho" }
    , { symbol = "normalizeX", signature = "Bool -> Tvs -> Rho -> Inf Rho" }
    , { symbol = "nicefyEffect", signature = "Effect -> Inf Effect" }
    , { symbol = "findInsts", signature = "[TypeVar] -> [Tau] -> [Tau] -> ([Tau],[Tau])" }
    , { symbol = "isSubset", signature = "[Tau] -> [Tau] -> [Tau] -> Maybe [Tau]" }
    , { symbol = "splitEffect", signature = "Effect -> Inf ([Tau],Effect)" }
    , { symbol = "inferUnify", signature = "Context -> Range -> Type -> Type -> Inf ()" }
    , { symbol = "inferUnifies", signature = "Context -> [(Range,Type)] -> Inf Type" }
    , { symbol = "inferSubsume", signature = "Context -> Range -> Type -> Type -> Inf (Type,Core.Expr -> Core.Expr)" }
    , { symbol = "nofailUnify", signature = "Unify a -> Inf a" }
    , { symbol = "withSkolemized", signature = "Range -> Type -> Maybe Doc -> (Type -> [TypeVar] -> Inf (a,Tvs)) -> Inf a" }
    , { symbol = "checkSkolemEscape", signature = "Range -> Type -> Maybe Doc -> [TypeVar] -> Tvs -> Inf ()" }
    , { symbol = "doUnify", signature = "Unify a -> Inf (Either UnifyError a)" }
    , { symbol = "occursInContext", signature = "TypeVar -> Tvs -> Inf Bool" }
    , { symbol = "unifyError", signature = "Context -> Range -> UnifyError -> Type -> Type -> Inf a" }
    , { symbol = "typeError", signature = "Range -> Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()" }
    , { symbol = "contextError", signature = "Range -> Range -> Doc -> [(Doc,Doc)] -> Inf ()" }
    , { symbol = "termError", signature = "Range -> Doc -> Type -> [(Doc,Doc)] -> Inf ()" }
    , { symbol = "resolveName", signature = "HasCallStack =>  Name -> Maybe (Type,Range) -> Range -> Inf (Name,Type,NameInfo)" }
    , { symbol = "resolveRhsName", signature = "HasCallStack => Name -> (Type,Range) -> Range -> Inf (Name,Type,NameInfo)" }
    , { symbol = "resolveFunName", signature = "Name -> NameContext -> Range -> Range -> Inf (Name,Type,NameInfo)" }
    , { symbol = "resolveConName", signature = "Name -> Maybe (Type) -> Range -> Inf (Name,Type,Core.ConRepr,ConInfo)" }
    , { symbol = "resolveConPatternName", signature = "Name -> Type -> Int -> Range -> Inf (Name,Type,Core.ConRepr,ConInfo)" }
    , { symbol = "resolveNameEx", signature = "HasCallStack => (NameInfo -> Bool) -> Maybe (NameInfo -> Bool) -> Name -> NameContext -> Range -> Range -> Inf (Name,Type,NameInfo)" }
    , { symbol = "lookupAppName", signature = "Bool -> Name -> NameContext -> Range -> Range ->" }
    , { symbol = "resolveImplicitName", signature = "Name -> Type -> Range -> Range -> Inf (Expr Type, Doc)" }
    , { symbol = "ppAmbDocs", signature = "[Doc] -> Doc" }
    , { symbol = "emptyImplicitArg", signature = "ImplicitArg" }
    , { symbol = "prettyImplicitArg", signature = "Pretty.Env -> ImplicitArg -> Doc" }
    , { symbol = "prettyImplicitAssign", signature = "Pretty.Env -> String -> Name -> ImplicitArg -> (Bool -> Doc)" }
    , { symbol = "iargScopeDepth", signature = "Name -> NameInfo -> Int" }
    , { symbol = "iaScopeDepth", signature = "ImplicitArg -> Int" }
    , { symbol = "toImplicitArgExpr", signature = "Range -> ImplicitArg -> Expr Type" }
    , { symbol = "prettyTypedArg", signature = "Pretty.Env -> TypedArg -> Doc" }
    , { symbol = "toImplicitArg", signature = "[(Name, ImplicitArg)] -> TypedArg -> ImplicitArg" }
    , { symbol = "resolveMaxChainDepth", signature = "Int" }
    , { symbol = "allCandidates", signature = "ImplicitSelect -> [ImplicitArg]" }
    , { symbol = "mapCandidates", signature = "(ImplicitArg -> ImplicitArg) -> ImplicitSelect -> ImplicitSelect" }
    , { symbol = "merge", signature = "ImplicitSelect -> ImplicitSelect -> ImplicitSelect" }
    , { symbol = "prettySelect", signature = "Pretty.Env -> ImplicitSelect -> Doc" }
    , { symbol = "resolveImplicitArg", signature = "Bool -> Bool -> NameContext -> Range -> [(NameInfo -> Bool, Name)] -> Inf (Either [Doc] (ImplicitArg))" }
    , { symbol = "resolveImplicitArgEx", signature = "Bool -> Bool -> Bool -> [TypedArg] -> NameContext -> Range -> [(NameInfo -> Bool, Name)] -> Inf ImplicitSelect" }
    , { symbol = "resolveUniquely", signature = "Bool -> Bool -> [TypedArg] -> NameContext -> Range -> ImplicitSelect -> [(TypedArg,[(Name,Type)])] -> Inf ImplicitSelect" }
    , { symbol = "resolveImplicitParameters", signature = "Bool -> Bool -> [TypedArg] -> Range -> (TypedArg,[(Name,Type)]) -> Inf ImplicitSelect" }
    , { symbol = "resolveImplicitParameter", signature = "Bool -> Bool -> [TypedArg] -> Range -> (Name, Type) -> Inf ImplicitSelect" }
    , { symbol = "decreasingWithin", signature = "Int" }
    , { symbol = "isDecreasingChain", signature = "[TypedArg] -> NameContext -> Name -> Type -> Bool" }
    , { symbol = "implicitsToResolve", signature = "NameContext -> TypedArg -> (TypedArg,[(Name,Type)])" }
    , { symbol = "lookupImplicitArg", signature = "Bool -> (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [TypedArg]" }
    , { symbol = "lookupFunName", signature = "HasCallStack => Name -> Maybe (Type,Range) -> Range -> Inf (Maybe (Name,Type,NameInfo))" }
    , { symbol = "lookupNameCtx", signature = "HasCallStack => (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo)]" }
    , { symbol = "lookupNames", signature = "HasCallStack => (NameInfo -> Bool) -> Name -> NameContext -> Range -> Inf [(Name,NameInfo,Rho)]" }
    , { symbol = "lookupLocalName", signature = "(NameInfo -> Bool) -> Name -> Inf (Either [(Name,NameInfo)] (Name,NameInfo))" }
    , { symbol = "lookupGlobalName", signature = "(NameInfo -> Bool) -> Name -> Inf [(Name,NameInfo)]" }
    , { symbol = "filterMatchNameContext", signature = "HasCallStack => Range -> NameContext -> [(Name,NameInfo)] -> Inf [(Name,NameInfo)]" }
    , { symbol = "filterMatchNameContextEx", signature = "HasCallStack => Range -> NameContext -> [(Name,NameInfo)] -> Inf [(Name,NameInfo,Rho)]" }
    , { symbol = "ppNameContext", signature = "Pretty.Env -> NameContext -> Doc" }
    , { symbol = "ppNameCtx", signature = "Pretty.Env -> (Name,NameContext) -> Doc" }
    , { symbol = "pureMatchShapeNameCtx", signature = "NameContext -> NameContext -> Bool" }
    , { symbol = "fixedCountContext", signature = "Maybe (Type,Range) -> Int -> [Name] -> NameContext" }
    , { symbol = "fixedContext", signature = "Maybe (Type,Range) -> [(Int,FixedArg)] -> Int -> [Name] -> Inf NameContext" }
    , { symbol = "implicitTypeContext", signature = "Type -> NameContext" }
    , { symbol = "maybeToContext", signature = "Maybe Type -> NameContext" }
    , { symbol = "maybeRToContext", signature = "Maybe (Type,Range) -> NameContext" }
    , { symbol = "checkCasingOverlaps", signature = "Range -> Name -> [(Name,NameInfo)] -> Inf ()" }
    , { symbol = "checkCasingOverlap", signature = "Range -> Name -> Name -> NameInfo -> Inf ()" }
    , { symbol = "checkCasing", signature = "Range -> Name -> Name -> NameInfo -> Inf ()" }
    , { symbol = "caseOverlaps", signature = "Name -> Name -> NameInfo -> (Maybe Name)" }
    , { symbol = "ppOr", signature = "Pretty.Env -> [Name] -> Doc" }
    , { symbol = "ppAmbiguous", signature = "Env -> String -> [(Name,NameInfo)] -> Doc" }
    , { symbol = "ppCandidates", signature = "Env -> [(Name,NameInfo)] -> Doc" }
    , { symbol = "ppImplicitsHint", signature = "Env -> [(Name,NameInfo)] -> [(Doc, Doc)]" }
    , { symbol = "ppConstraints", signature = "Pretty.Env -> [ImplicitConstraint] -> Doc" }
    , { symbol = "ppConstraint", signature = "Pretty.Env -> ImplicitConstraint -> Doc" }
    , { symbol = "implicitConstraints", signature = "[(Name,Name -> Type -> Maybe (Tvs -> ImplicitConstraint -> Inf Bool, Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type)))]" }
    , { symbol = "checkImplicitConstraint", signature = "Name -> Type -> Range -> Range -> Inf (Maybe TypedArg)" }
    , { symbol = "resolveImplicitConstraints", signature = "Tvs -> [ImplicitConstraint] -> Inf (Core.Expr -> Core.Expr)" }
    , { symbol = "tryResolveImplicitConstraints", signature = "Bool -> Tvs -> Inf (Core.Expr -> Core.Expr)" }
    , { symbol = "heapNeverContainedIn", signature = "Tvs -> Type -> Type -> Bool" }
    , { symbol = "heapAlwaysContainedIn", signature = "Tvs -> Type -> Type -> Bool" }
    , { symbol = "checkHeapDivConstraint", signature = "Name -> Type -> Maybe (Tvs -> ImplicitConstraint -> Inf Bool, Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type))" }
    , { symbol = "ppTvs", signature = "Pretty.Env -> Tvs -> Doc" }
    , { symbol = "canResolveHeapDivConstraint", signature = "Tvs -> ImplicitConstraint -> Inf Bool" }
    , { symbol = "implicitConstraintType", signature = "HasCallStack => ImplicitConstraint -> Inf Type" }
    , { symbol = "resolveHeapDivConstraint", signature = "Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type)" }
    , { symbol = "runInfer", signature = "Pretty.Env -> Maybe RangeMap -> Synonyms -> Newtypes -> ImportMap -> Gamma -> Name -> Bool -> Int -> Inf a -> Error b (a,Int,Maybe RangeMap)" }
    , { symbol = "zapSubst", signature = "HasCallStack => Inf ()" }
    , { symbol = "tryRun", signature = "Inf a -> Inf (Maybe a)" }
    , { symbol = "ignoreErrors", signature = "Inf a -> Inf a -> Inf a" }
    , { symbol = "getEnv", signature = "Inf Env" }
    , { symbol = "withEnv", signature = "(Env -> Env) -> Inf a -> Inf a" }
    , { symbol = "updateSt", signature = "(St -> St) -> Inf St" }
    , { symbol = "infError", signature = "Range -> Doc -> Inf a" }
    , { symbol = "infWarning", signature = "Range -> Doc -> Inf ()" }
    , { symbol = "getPrettyEnv", signature = "Inf Pretty.Env" }
    , { symbol = "lookupSynonym", signature = "Name -> Inf (Maybe SynInfo)" }
    , { symbol = "addRangeInfo", signature = "Range -> RangeInfo -> Inf ()" }
    , { symbol = "withNoRangeInfo", signature = "Inf a -> Inf a" }
    , { symbol = "withLocalScope", signature = "Inf a -> Inf a" }
    , { symbol = "withNoLocalScope", signature = "Inf a -> Inf a" }
    , { symbol = "localScopeDepth", signature = "Inf Int" }
    , { symbol = "withScope", signature = "Inf a -> Inf a" }
    , { symbol = "getScopeDepth", signature = "Inf Int" }
    , { symbol = "withNiceNames", signature = "(Name -> Int -> Doc) -> [Name] -> ([Doc] -> Inf a) -> Inf a" }
    , { symbol = "lookupNiceName", signature = "Name -> Inf (Maybe Doc)" }
    , { symbol = "getSt", signature = "Inf St" }
    , { symbol = "setSt", signature = "St -> Inf St" }
    , { symbol = "allowReturn", signature = "Bool -> Inf a -> Inf a" }
    , { symbol = "withLhs", signature = "Inf a -> Inf a" }
    , { symbol = "withHiddenTermDoc", signature = "Range -> Doc -> Inf a -> Inf a" }
    , { symbol = "inHiddenTermDoc", signature = "Inf Bool" }
    , { symbol = "isLhs", signature = "Inf Bool" }
    , { symbol = "isReturnAllowed", signature = "Inf Bool" }
    , { symbol = "getTermDoc", signature = "String -> Range -> Inf (Doc,Doc)" }
    , { symbol = "useHole", signature = "Inf Bool" }
    , { symbol = "disallowHole", signature = "Inf a -> Inf a" }
    , { symbol = "allowHole", signature = "Inf a -> Inf (a,Bool {- was the hole used? -})" }
    , { symbol = "isImplicitConstraintEvidenceName", signature = "Name -> Bool" }
    , { symbol = "addImplicitConstraint", signature = "Name -> Type -> (Tvs -> ImplicitConstraint -> Inf Bool) -> (Tvs -> ImplicitConstraint -> Inf (Core.Expr, Type)) -> Range -> Range -> Inf TypedArg" }
    , { symbol = "solvedImplicitConstraint", signature = "Name -> Type -> Inf ()" }
    , { symbol = "getImplicitConstraints", signature = "Inf [ImplicitConstraint]" }
    , { symbol = "mapImplicitConstraints", signature = "([ImplicitConstraint] -> Inf (a,[ImplicitConstraint])) -> Inf a" }
    , { symbol = "scopeImplicitConstraints", signature = "Inf a -> Inf a" }
    , { symbol = "substImplicitConstraints", signature = "Sub -> Inf ()" }
    , { symbol = "getSub", signature = "Inf Sub" }
    , { symbol = "subst", signature = "(HasCallStack,HasTypeVar a) => a -> Inf a" }
    , { symbol = "extendSub", signature = "Sub -> Inf ()" }
    , { symbol = "substWatch", signature = "Inf a -> Inf (Bool,a)" }
    , { symbol = "getGamma", signature = "Inf Gamma" }
    , { symbol = "extendGammaCore", signature = "Bool -> [Core.DefGroup] -> Inf a -> Inf (a)" }
    , { symbol = "extendGamma", signature = "Bool -> [(Name,NameInfo)] -> Inf a -> Inf (a)" }
    , { symbol = "extendInfGammaCore", signature = "Bool -> [Core.DefGroup] -> Inf a -> Inf a" }
    , { symbol = "extendInfGamma", signature = "[(Name,NameInfo)] -> Inf a -> Inf a" }
    , { symbol = "extendInfGammaEx", signature = "Bool -> [Name] -> [(Name,NameInfo)] -> Inf a -> Inf a" }
    , { symbol = "withGammaType", signature = "Range -> Type -> Inf a -> Inf a" }
    , { symbol = "currentDefName", signature = "Inf Name" }
    , { symbol = "currentDefNames", signature = "Inf [Name]" }
    , { symbol = "withDefName", signature = "Name -> Inf a -> Inf a" }
    , { symbol = "isNamedLam", signature = "(Bool -> Inf a) -> Inf a" }
    , { symbol = "qualifyName", signature = "Name -> Inf Name" }
    , { symbol = "getModuleName", signature = "Inf Name" }
    , { symbol = "freeInGamma", signature = "Inf Tvs" }
    , { symbol = "getNewtypes", signature = "Inf Newtypes" }
    , { symbol = "getLocalVars", signature = "Inf [(Name,Type)]" }
    , { symbol = "lookupInfName", signature = "Name -> Inf (Maybe (Name,Type))" }
    , { symbol = "findDataInfo", signature = "Name -> Inf DataInfo" }
    , { symbol = "traceIndent", signature = "Inf a -> Inf a" }
    , { symbol = "traceDefDoc", signature = "(Pretty.Env -> Doc) -> Inf ()" }
    , { symbol = "traceDoc", signature = "(Pretty.Env -> Doc) -> Inf ()" }
    , { symbol = "concatMapM", signature = "Monad m => (a -> m [b]) -> [a] -> m [b]" }
    , { symbol = "effectIsAffine", signature = "Effect -> Bool" }
    , { symbol = "labelIsLinear", signature = "Effect -> Bool" }
    , { symbol = "labelIsAffine", signature = "Effect -> Bool" }
    , { symbol = "containsHandledEffect", signature = "Effect -> Bool" }
    , { symbol = "extractHandledEffect", signature = "Type -> ([Type], Tau)" }
    , { symbol = "isHandledEffect", signature = "Type -> Bool" }
    , { symbol = "getOperationEffect", signature = "Type -> Maybe (HandledSort,Name)" }
    , { symbol = "getOperationEffectX", signature = "[Name] -> Type -> Maybe (HandledSort,Name)" }
    , { symbol = "requiresImplicits", signature = "Type -> [(Name, Type)]" }
    , { symbol = "isOptionalOrImplicit", signature = "(Name,Type) -> Bool" }
    , { symbol = "splitOptionalImplicit", signature = "[(Name,Type)] -> ([(Name,Type)],[(Name,Type)],[(Name,Type)])" }
    , { symbol = "hasOptionalOrImplicits", signature = "[(Name,Type)] -> Bool" }
    , { symbol = "instantiate", signature = "(HasCallStack,HasUnique m) => Range -> Type -> m Rho" }
    , { symbol = "instantiateEx", signature = "(HasCallStack,HasUnique m) => Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)" }
    , { symbol = "instantiateNoEx", signature = "(HasCallStack,HasUnique m) => Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)" }
    , { symbol = "extend", signature = "(HasCallStack,HasUnique m) => Rho -> m (Rho, Core.Expr -> Core.Expr)" }
    , { symbol = "skolemize", signature = "(HasCallStack,HasUnique m) => Range -> Type -> m Rho" }
    , { symbol = "skolemizeEx", signature = "(HasCallStack,HasUnique m) => Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)" }
    , { symbol = "instantiateExFl", signature = "(HasCallStack,HasUnique m) => Flavour -> Range -> Type -> m ([TypeVar],Rho,Core.Expr -> Core.Expr)" }
    , { symbol = "freshSub", signature = "HasUnique m => Flavour -> [TypeVar] -> m ([TypeVar],Sub)" }
    , { symbol = "freshSubX", signature = "HasUnique m => (TypeVar -> Type) -> Flavour -> [TypeVar] -> Tvs -> m ([TypeVar],Sub)" }
    , { symbol = "freshTypeVarX", signature = "HasUnique m => Kind -> Flavour -> Tvs -> m TypeVar" }
    , { symbol = "instantiateAnnot", signature = "HasUnique m => Annot -> m Type" }
    , { symbol = "freshSkolems", signature = "HasUnique m => Kind -> Int -> m [Type]" }
    , { symbol = "freshTVars", signature = "HasUnique m => Kind -> Flavour -> Int -> m [Type]" }
    , { symbol = "freshTVar", signature = "HasUnique m => Kind -> Flavour -> m Type" }
    , { symbol = "freshEffect", signature = "HasUnique m => m Effect" }
    , { symbol = "freshStar", signature = "HasUnique m => m Tau" }
    , { symbol = "niceType", signature = "Env -> Type -> Doc" }
    , { symbol = "niceTypes", signature = "Env -> [Type] -> [Doc]" }
    , { symbol = "niceList", signature = "(HasTypeVar a) => (Env -> a -> Doc) -> Env -> [a] -> [Doc]" }
    , { symbol = "niceEnv", signature = "Env -> [TypeVar] -> Env" }
    , { symbol = "keyword", signature = "Env -> String -> Doc" }
    , { symbol = "defaultEnv", signature = "Env" }
    , { symbol = "ppScheme", signature = "Env -> Scheme -> Doc" }
    , { symbol = "ppSchemeEffect", signature = "Env -> Scheme -> Doc" }
    , { symbol = "prettyDefFunType", signature = "Env -> [ParamInfo] -> Scheme -> Doc" }
    , { symbol = "ppDeclType", signature = "Env -> [ParamInfo] -> Scheme -> (Maybe [(Name,ParamInfo,Doc)],Doc,Doc)" }
    , { symbol = "ppSub", signature = "Env -> Sub -> Doc" }
    , { symbol = "precTop,precQuant,precArrow,precApp", signature = "Prec" }
    , { symbol = "pparens", signature = "Prec -> Prec -> Doc -> Doc" }
    , { symbol = "ppType", signature = "Env -> Type -> Doc" }
    , { symbol = "ppParam", signature = "Env -> (Name,Type) -> Doc" }
    , { symbol = "ppName", signature = "Env -> Name -> Doc" }
    , { symbol = "ppTypeName", signature = "Env -> Name -> Doc" }
    , { symbol = "ppNamePlain", signature = "Env -> Name -> Doc" }
    , { symbol = "ppSynonym", signature = "Env -> TypeSyn -> [Tau] -> Doc -> Doc" }
    , { symbol = "ppTypeVar", signature = "Env -> TypeVar -> Doc" }
    , { symbol = "ppTypeCon", signature = "Env -> TypeCon -> Doc" }
    , { symbol = "ppTypeSyn", signature = "Env -> TypeSyn -> Doc" }
    , { symbol = "wrapKind", signature = "Bool -> Env -> Kind -> Doc -> Doc" }
    , { symbol = "niceTypeInitial", signature = "[TypeVar] -> Nice" }
    , { symbol = "niceTypeExtend", signature = "[TypeVar] -> Nice -> Nice" }
    , { symbol = "niceTypeVars", signature = "[String]" }
    , { symbol = "niceEffectVars", signature = "[String]" }
    , { symbol = "niceHeapVars", signature = "[String]" }
    , { symbol = "niceScopeVars", signature = "[String]" }
    , { symbol = "maxSynonymRank", signature = "Type -> SynonymRank" }
    , { symbol = "dataInfoIsLazy", signature = "DataInfo -> Bool" }
    , { symbol = "lazyName", signature = "DataInfo -> String -> Name" }
    , { symbol = "conInfoSize", signature = "Platform -> ConInfo -> Int" }
    , { symbol = "conInfoScanCount", signature = "ConInfo -> Int" }
    , { symbol = "conInfoIsLazy", signature = "ConInfo -> Bool" }
    , { symbol = "conInfoLazyFip", signature = "ConInfo -> Fip" }
    , { symbol = "typeVarId", signature = "TypeVar -> Id" }
    , { symbol = "typeConName", signature = "TypeCon -> Name" }
    , { symbol = "typeSynName", signature = "TypeSyn -> Name" }
    , { symbol = "synonymRank", signature = "TypeSyn -> SynonymRank" }
    , { symbol = "isBound", signature = "TypeVar -> Bool" }
    , { symbol = "isMeta", signature = "TypeVar -> Bool" }
    , { symbol = "isSkolem", signature = "TypeVar -> Bool" }
    , { symbol = "isMonoType", signature = "Type -> Bool" }
    , { symbol = "splitTypeScheme", signature = "Type -> ([TypeVar], Rho)" }
    , { symbol = "expandSyn", signature = "Type -> Type" }
    , { symbol = "canonicalForm", signature = "Type -> Type" }
    , { symbol = "minimalForm", signature = "Type -> Type" }
    , { symbol = "makeScheme", signature = "[TypeVar] -> Rho -> Scheme" }
    , { symbol = "quantifyType", signature = "[TypeVar] -> Scheme -> Scheme" }
    , { symbol = "tForall", signature = "[TypeVar] -> Rho -> Scheme" }
    , { symbol = "getTypeArities", signature = "Type -> (Int,Int)" }
    , { symbol = "splitFunScheme", signature = "Scheme -> Maybe ([TypeVar],[(Name,Tau)],Effect,Tau)" }
    , { symbol = "isTVar", signature = "Type -> Bool" }
    , { symbol = "isTCon", signature = "Type -> Bool" }
    , { symbol = "isRho", signature = "Type -> Bool" }
    , { symbol = "isTau", signature = "Type -> Bool" }
    , { symbol = "isFun", signature = "Type -> Bool" }
    , { symbol = "splitFunType", signature = "Type -> Maybe ([(Name,Type)],Type,Type)" }
    , { symbol = "typeInt", signature = "Tau" }
    , { symbol = "typeInt32", signature = "Tau" }
    , { symbol = "typeEvIndex", signature = "Tau" }
    , { symbol = "typeSSizeT", signature = "Tau" }
    , { symbol = "typeFloat", signature = "Tau" }
    , { symbol = "typeChar", signature = "Tau" }
    , { symbol = "typeString", signature = "Tau" }
    , { symbol = "typeResumeContext", signature = "Tau -> Effect -> Effect -> Tau -> Tau" }
    , { symbol = "typeRef", signature = "Tau" }
    , { symbol = "isTypeLocalVar", signature = "Tau -> Bool" }
    , { symbol = "orderEffect", signature = "HasCallStack => Tau -> Tau" }
    , { symbol = "extractOrderedEffect", signature = "Tau -> ([Tau],Tau)" }
    , { symbol = "labelName", signature = "Tau -> Name" }
    , { symbol = "labelNameFull", signature = "Tau -> Name" }
    , { symbol = "labelNameEx", signature = "Tau -> (Name,Int,[Tau])" }
    , { symbol = "typePartial", signature = "Type" }
    , { symbol = "typeLocal", signature = "Type" }
    , { symbol = "isTypeLocal", signature = "Type -> Bool" }
    , { symbol = "tconHandled", signature = "Type" }
    , { symbol = "tconHandled1", signature = "Type" }
    , { symbol = "wrapHandledFromDataEffect", signature = "HasCallStack => DataEffect -> Type -> Type" }
    , { symbol = "effectEmpty", signature = "Tau" }
    , { symbol = "isEffectEmpty", signature = "HasCallStack => Tau -> Bool" }
    , { symbol = "effectExtendNoDup", signature = "HasCallStack => Tau -> Tau -> Tau" }
    , { symbol = "effectExtendNoDups", signature = "HasCallStack => [Tau] -> Tau -> Tau" }
    , { symbol = "effectExtend", signature = "HasCallStack => Tau -> Tau -> Tau" }
    , { symbol = "tconEffectExtend", signature = "TypeCon" }
    , { symbol = "effectExtends", signature = "HasCallStack => [Tau] -> Tau -> Tau" }
    , { symbol = "effectFixed", signature = "[Tau] -> Tau" }
    , { symbol = "isEffectFixed", signature = "Tau -> Bool" }
    , { symbol = "extractEffectExtend", signature = "HasCallStack => Tau -> ([Tau],Tau)" }
    , { symbol = "shallowExtractEffectExtend", signature = "Tau -> ([Tau],Tau)" }
    , { symbol = "shallowEffectExtend", signature = "HasCallStack => Tau -> Tau -> Tau" }
    , { symbol = "appEffectExtend", signature = "HasCallStack => Type -> Effect -> Effect" }
    , { symbol = "kindOf", signature = "HasCallStack => Tau -> Kind" }
    , { symbol = "typeDivergent", signature = "Tau" }
    , { symbol = "single", signature = "Name -> Effect" }
    , { symbol = "typeTotal", signature = "Tau" }
    , { symbol = "tconTotal", signature = "TypeCon" }
    , { symbol = "isTypeTotal", signature = "Tau -> Bool" }
    , { symbol = "typePartial", signature = "Tau" }
    , { symbol = "typePure", signature = "Tau" }
    , { symbol = "typeBool", signature = "Tau" }
    , { symbol = "typeCCtx", signature = "Tau -> Tau" }
    , { symbol = "tsynCCtx", signature = "TypeSyn" }
    , { symbol = "typeCCtxx", signature = "Tau" }
    , { symbol = "tconCCtxx", signature = "TypeCon" }
    , { symbol = "typeFieldAddr", signature = "Tau" }
    , { symbol = "tconFieldAddr", signature = "TypeCon" }
    , { symbol = "typeVector", signature = "Tau" }
    , { symbol = "typeList", signature = "Tau" }
    , { symbol = "tconList", signature = "TypeCon" }
    , { symbol = "typeEv", signature = "Tau" }
    , { symbol = "tconEv", signature = "TypeCon" }
    , { symbol = "isEvType", signature = "Tau -> Bool" }
    , { symbol = "makeEvType", signature = "Type -> Type" }
    , { symbol = "typeFun", signature = "[(Name,Tau)] -> Tau -> Tau -> Tau" }
    , { symbol = "typeApp", signature = "Tau -> [Tau] -> Tau" }
    , { symbol = "typeUnit", signature = "Tau" }
    , { symbol = "typeVoid", signature = "Tau" }
    , { symbol = "typeReuse", signature = "Tau" }
    , { symbol = "typeAny", signature = "Tau" }
    , { symbol = "typeMakeTuple", signature = "[Tau] -> Tau" }
    , { symbol = "typeTuple", signature = "Int -> Tau" }
    , { symbol = "typeOptional", signature = "Tau" }
    , { symbol = "tconOptional", signature = "TypeCon" }
    , { symbol = "isOptional", signature = "Type -> Bool" }
    , { symbol = "makeOptionalType", signature = "Type -> Type" }
    , { symbol = "unOptional", signature = "Type -> Type" }
    , { symbol = "pruneSyn", signature = "Rho -> Rho" }
    , { symbol = "elemType", signature = "Type -> [Type] -> Bool" }
    , { symbol = "eqType", signature = "HasCallStack => Type -> Type -> Bool" }
    , { symbol = "matchEffect", signature = "HasCallStack => Effect -> Effect -> Bool" }
    , { symbol = "eqTypes", signature = "HasCallStack => [Type] -> [Type] -> Bool" }
    , { symbol = "fuv", signature = "(HasTypeVar t) => t -> Tvs" }
    , { symbol = "fsv", signature = "(HasTypeVar t) => t -> Tvs" }
    , { symbol = "fbv", signature = "(HasTypeVar t) => t -> Tvs" }
    , { symbol = "alltv", signature = "(HasTypeVar t) => t -> Tvs" }
    , { symbol = "subCount", signature = "Sub -> Int" }
    , { symbol = "subNull", signature = "Sub" }
    , { symbol = "subIsNull", signature = "Sub -> Bool" }
    , { symbol = "subNew", signature = "(HasCallStack) => [(TypeVar, Tau)] -> Sub" }
    , { symbol = "subDom", signature = "Sub -> Tvs" }
    , { symbol = "subRange", signature = "Sub -> [Tau]" }
    , { symbol = "subList", signature = "Sub -> [(TypeVar, Tau)]" }
    , { symbol = "subCommon", signature = "Sub -> Sub -> [(TypeVar, (Tau, Tau))]" }
    , { symbol = "subSingle", signature = "(HasCallStack) => TypeVar -> Tau -> Sub" }
    , { symbol = "subLookup", signature = "HasCallStack => TypeVar -> Sub -> Maybe Tau" }
    , { symbol = "subRemove", signature = "[TypeVar] -> Sub -> Sub" }
    , { symbol = "subFind", signature = "(HasCallStack) => TypeVar -> Sub -> Tau" }
    , { symbol = "subCompose", signature = "Sub -> Sub -> Sub" }
    , { symbol = "subExtend", signature = "TypeVar -> Tau -> Sub -> Sub" }
    , { symbol = "subInsert", signature = "TypeVar -> Tau -> Sub -> Sub" }
    , { symbol = "subInserts", signature = "[(TypeVar, Tau)] -> Sub -> Sub" }
    , { symbol = "tvsEmpty", signature = "Tvs" }
    , { symbol = "tvsIsEmpty", signature = "Tvs -> Bool" }
    , { symbol = "tvsSingle", signature = "TypeVar -> Tvs" }
    , { symbol = "tvsInsert", signature = "TypeVar -> Tvs -> Tvs" }
    , { symbol = "tvsInsertAll", signature = "[TypeVar] -> Tvs -> Tvs" }
    , { symbol = "tvsNew", signature = "[TypeVar] -> Tvs" }
    , { symbol = "tvsList", signature = "Tvs -> [TypeVar]" }
    , { symbol = "tvsRemove", signature = "[TypeVar] -> Tvs -> Tvs" }
    , { symbol = "tvsDiff", signature = "Tvs -> Tvs -> Tvs" }
    , { symbol = "tvsUnion", signature = "Tvs -> Tvs -> Tvs" }
    , { symbol = "tvsFilter", signature = "(TypeVar -> Bool) -> Tvs -> Tvs" }
    , { symbol = "tvsUnions", signature = "[Tvs] -> Tvs" }
    , { symbol = "tvsMember", signature = "TypeVar -> Tvs -> Bool" }
    , { symbol = "tvsIntersect", signature = "Tvs -> Tvs -> Tvs" }
    , { symbol = "tvsCommon", signature = "Tvs -> Tvs -> Bool" }
    , { symbol = "tvsDisjoint", signature = "Tvs -> Tvs -> Bool" }
    , { symbol = "tvsIsSubsetOf", signature = "Tvs -> Tvs -> Bool" }
    , { symbol = "assocMax", signature = "[(TypeVar, Tau)] -> Int" }
    , { symbol = "tvarsMax", signature = "[TypeVar] -> Int" }
    , { symbol = "tvsMax", signature = "Tvs -> Int" }
    , { symbol = "ofuv", signature = "(HasOrderedTypeVar a) => a -> [TypeVar]" }
    , { symbol = "oftv", signature = "(HasOrderedTypeVar a) => a -> [TypeVar]" }
    , { symbol = "tcsEmpty", signature = "TypeCons" }
    , { symbol = "tcsSingleton", signature = "TypeCon -> TypeCons" }
    , { symbol = "tcsUnion", signature = "TypeCons -> TypeCons -> TypeCons" }
    , { symbol = "tcsUnions", signature = "[TypeCons] -> TypeCons" }
    , { symbol = "posnegEffects", signature = "Type -> ([TypeVar], [TypeVar])" }
    , { symbol = "posneg", signature = "Bool -> Type -> Tvs" }
    , { symbol = "freshTypeVar", signature = "(HasUnique m) => Kind -> Flavour -> m TypeVar" }
    , { symbol = "matchType", signature = "HasCallStack => Type -> Type -> Bool" }
    , { symbol = "showTypeVar", signature = "TypeVar -> String" }
    , { symbol = "showTp", signature = "Type -> String" }
    , { symbol = "overlaps", signature = "Range -> Tvs -> Type -> Type -> Unify ()" }
    , { symbol = "matchNamed", signature = "Bool -> Range -> Tvs -> Type -> Int -> [Name] -> Maybe Type -> Unify Rho" }
    , { symbol = "matchArguments", signature = "Bool -> Range -> Tvs -> Type -> [Type] -> [(Name,Type)] -> Maybe Type -> Unify Rho" }
    , { symbol = "subsumeSubst", signature = "Range -> Tvs -> Type -> Type -> Unify (Type,Rho, Core.Expr -> Core.Expr)" }
    , { symbol = "matchShape", signature = "Type -> Type -> Unify ()" }
    , { symbol = "pureMatchShape", signature = "Type -> Type -> Bool" }
    , { symbol = "subsume", signature = "HasCallStack => Range -> Tvs -> Type -> Type -> Unify (Type,Rho,Core.Expr -> Core.Expr)" }
    , { symbol = "unify", signature = "HasCallStack => Type -> Type -> Unify ()" }
    , { symbol = "unifyTVar", signature = "HasCallStack => TypeVar -> Type -> Unify ()" }
    , { symbol = "unifies", signature = "HasCallStack => [Type] -> [Type] -> Unify ()" }
    , { symbol = "unifyEffect", signature = "HasCallStack => Type -> Type -> Unify ()" }
    , { symbol = "extractNormalizeEffect", signature = "Type -> Unify ([Type],Type)" }
    , { symbol = "unifyEffectVar", signature = "HasCallStack => TypeVar -> Type -> Unify ()" }
    , { symbol = "unifyLabels", signature = "HasCallStack => [Tau] -> [Tau] -> Bool -> Bool -> Unify ([Tau],[Tau])" }
    , { symbol = "matchKinds", signature = "[Kind] -> [Kind] -> Unify ()" }
    , { symbol = "matchKind", signature = "Kind -> Kind -> Bool" }
    , { symbol = "runUnifyEx", signature = "Int -> Unify a -> (Either UnifyError a,Sub,Int)" }
    , { symbol = "runUnify", signature = "HasUnique m => Unify a -> m (Either UnifyError a,Sub)" }
    , { symbol = "getSubst", signature = "Unify Sub" }
    , { symbol = "extendSub", signature = "TypeVar -> Type -> Unify ()" }
    , { symbol = "unifyError", signature = "UnifyError -> Unify a" }
    , { symbol = "subst", signature = "HasTypeVar a => a -> Unify a" }
    , { symbol = "withError", signature = "(UnifyError -> UnifyError) -> Unify a -> Unify a" }
    , { symbol = "commonFlags", signature = "[String]" }
    , { symbol = "makeCfg", signature = "[String] -> Options -> [String] -> Cfg" }
    , { symbol = "extendCfg", signature = "Cfg -> Cfg -> Cfg" }
    , { symbol = "initialCfg", signature = "Options -> Cfg" }
    , { symbol = "readFlagsFile", signature = "FilePath -> IO [String]" }
    , { symbol = "testSanitize", signature = "FilePath -> String -> String" }
    , { symbol = "expectedSanitize", signature = "String -> String" }
    , { symbol = "runKoka", signature = "Cfg -> FilePath -> FilePath -> IO String" }
    , { symbol = "makeTest", signature = "Cfg -> FilePath -> Spec" }
    , { symbol = "discoverTests", signature = "Cfg -> FilePath -> Spec" }
    , { symbol = "readConfigFile", signature = "Cfg -> FilePath -> IO Cfg" }
    , { symbol = "processOptions", signature = "String -> (Options, [String]) -> (Options,[String])" }
    , { symbol = "getOptions", signature = "IO (Options , [String])" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "depth", signature = "Int -> Int -> [(Int, Int, Int)]" }
    , { symbol = "sumT", signature = "Int -> Int -> Int -> Int" }
    , { symbol = "check", signature = "Tree -> Int" }
    , { symbol = "tailCheck", signature = "Tree -> Int -> Int" }
    , { symbol = "make", signature = "Int -> Tree" }
    , { symbol = "make'", signature = "Int -> Int -> Tree" }
    , { symbol = "mk_expr", signature = "Integer -> Integer -> Expr" }
    , { symbol = "append_add", signature = "Expr -> Expr -> Expr" }
    , { symbol = "append_mul", signature = "Expr -> Expr -> Expr" }
    , { symbol = "reassoc", signature = "Expr -> Expr" }
    , { symbol = "const_folding", signature = "Expr -> Expr" }
    , { symbol = "eval", signature = "Expr -> Integer" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "pown", signature = "Int -> Int -> Int" }
    , { symbol = "add", signature = "Expr -> Expr -> Expr" }
    , { symbol = "mul", signature = "Expr -> Expr -> Expr" }
    , { symbol = "pow", signature = "Expr -> Expr -> Expr" }
    , { symbol = "ln", signature = "Expr -> Expr" }
    , { symbol = "d", signature = "String -> Expr -> Expr" }
    , { symbol = "count", signature = "Expr -> Integer" }
    , { symbol = "nest_aux", signature = "Int -> (Int -> Expr -> IO Expr) -> Int -> Expr -> IO Expr" }
    , { symbol = "deriv", signature = "Int -> Expr -> IO Expr" }
    , { symbol = "badRand", signature = "Elem -> Elem" }
    , { symbol = "mkRandomArray", signature = "Elem -> Int -> Array Int Elem" }
    , { symbol = "checkSortedAux", signature = "Array Int Elem -> Int -> IO ()" }
    , { symbol = "swap", signature = "Int -> Int -> STArray s Int Elem -> ST s ()" }
    , { symbol = "partition", signature = "STArray s Int Elem -> Int -> Int -> ST s Int" }
    , { symbol = "qsort", signature = "STArray s Int Elem -> ST s ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "fold", signature = "(α -> β  -> σ  -> σ) -> Tree α β -> σ  -> σ" }
    , { symbol = "balance1", signature = "Tree α β -> Tree α β -> Tree α β" }
    , { symbol = "balance2", signature = "Tree α β -> Tree α β -> Tree α β" }
    , { symbol = "is_red", signature = "Tree α β -> Bool" }
    , { symbol = "ins", signature = "Ord α => Tree α β -> α -> β -> Tree α β" }
    , { symbol = "set_black", signature = "Tree α β -> Tree α β" }
    , { symbol = "mk_Map_aux", signature = "Int -> Int -> Map -> [Map] -> [Map]" }
    , { symbol = "fold", signature = "(α -> β  -> σ  -> σ) -> Tree α β -> σ  -> σ" }
    , { symbol = "balance_left", signature = "Tree α β -> α -> β -> Tree α β -> Tree α β" }
    , { symbol = "balance_right", signature = "Tree α β -> α -> β -> Tree α β -> Tree α β" }
    , { symbol = "is_red", signature = "Tree α β -> Bool" }
    , { symbol = "ins", signature = "Ord α => Tree α β -> α -> β -> Tree α β" }
    , { symbol = "set_black", signature = "Tree α β -> Tree α β" }
    , { symbol = "mk_Map_aux", signature = "Int -> Int -> Map -> Map" }
    , { symbol = "fold", signature = "(α -> β  -> σ  -> σ) -> Tree α β -> σ  -> σ" }
    , { symbol = "balance1", signature = "Tree α β -> Tree α β -> Tree α β" }
    , { symbol = "balance2", signature = "Tree α β -> Tree α β -> Tree α β" }
    , { symbol = "is_red", signature = "Tree α β -> Bool" }
    , { symbol = "ins", signature = "Ord α => Tree α β -> α -> β -> Tree α β" }
    , { symbol = "set_black", signature = "Tree α β -> Tree α β" }
    , { symbol = "mk_Map_aux", signature = "Int -> Map -> Map" }
    , { symbol = "fold", signature = "(α -> β  -> σ  -> σ) -> Tree α β -> σ  -> σ" }
    , { symbol = "balance1", signature = "Tree α β -> Tree α β -> Tree α β" }
    , { symbol = "balance2", signature = "Tree α β -> Tree α β -> Tree α β" }
    , { symbol = "is_red", signature = "Tree α β -> Bool" }
    , { symbol = "ins", signature = "Ord α => Tree α β -> α -> β -> Tree α β" }
    , { symbol = "set_black", signature = "Tree α β -> Tree α β" }
    , { symbol = "mk_Map_aux", signature = "Integer -> Map -> Map" }
    , { symbol = "gdoyFromMonth", signature = "Year -> Month -> Doy" }
    , { symbol = "gmonthFromDoy", signature = "Year -> Doy -> Month  -- =" }
    , { symbol = "gdoeFromYear", signature = "Year -> Doe" }
    , { symbol = "gestimateYear", signature = "Doe -> Year           -- =" }
    , { symbol = "gregorian", signature = "Calendar" }
    , { symbol = "calendar", signature = "ConvYear -> ConvMonth -> Calendar" }
    , { symbol = "ecalendar", signature = "ConvEstYear -> ConvMonth -> Calendar" }
    , { symbol = "gbuiltin", signature = "Calendar" }
    , { symbol = "forDays", signature = "Year -> (Integer -> IO ()) ->  IO ()" }
    , { symbol = "rcalendar", signature = "(Year -> Days, Days -> Year, Month -> Doy, Doy -> Month) ->" }
    , { symbol = "ecalendar", signature = "(Year -> Days, Days -> Year, (Year,Month) -> Doy, (Year,Doy) -> Month) ->" }
    , { symbol = "calendar", signature = "(Year -> Days, Days -> (Year,Doy), (Year,Month) -> Doy, (Year,Doy) -> Month) ->" }
    , { symbol = "gregdateOf", signature = "Integer -> Date" }
    , { symbol = "gregWeekdateOf", signature = "Integer -> Weekdate" }
    , { symbol = "epoch", signature = "T.Day" }
    , { symbol = "weeksToYear0", signature = "Integer -> Integer" }
    , { symbol = "forWeekdates", signature = "Year -> (Weekdate -> IO ()) ->  IO ()" }
    , { symbol = "forMonthdates", signature = "Year -> (Weekdate -> IO ()) ->  IO ()" }
    , { symbol = "ediv", signature = "Integer -> Integer -> Integer" }
    , { symbol = "emod", signature = "Integer -> Integer -> Integer" }
    , { symbol = "min", signature = "(Integer,Integer) -> Integer" }
    , { symbol = "edivmod", signature = "Integer -> Integer -> (Integer,Integer)" }
    , { symbol = "rcalendar", signature = "(Year -> Days, Days -> Year, Month -> Doy, Doy -> Month) ->" }
    , { symbol = "ecalendar", signature = "(Year -> Days, Days -> Year, (Year,Month) -> Doy, (Year,Doy) -> Month) ->" }
    , { symbol = "calendar", signature = "(Year -> Days, Days -> (Year,Doy), (Year,Month) -> Doy, (Year,Doy) -> Month) ->" }
    , { symbol = "gregdateOf", signature = "Integer -> Date" }
    , { symbol = "gregWeekdateOf", signature = "Integer -> Weekdate" }
    , { symbol = "epoch", signature = "T.Day" }
    , { symbol = "weeksToYear0", signature = "Integer -> Integer" }
    , { symbol = "forWeekdates", signature = "Year -> (Weekdate -> IO ()) ->  IO ()" }
    , { symbol = "forMonthdates", signature = "Year -> (Weekdate -> IO ()) ->  IO ()" }
    , { symbol = "forDays", signature = "Year -> (Integer -> IO ()) ->  IO ()" }
    , { symbol = "ediv", signature = "Integer -> Integer -> Integer" }
    , { symbol = "emod", signature = "Integer -> Integer -> Integer" }
    , { symbol = "min", signature = "(Integer,Integer) -> Integer" }
    , { symbol = "edivmod", signature = "Integer -> Integer -> (Integer,Integer)" }
    , { symbol = "printTestConfig", signature = "TestConfig -> IO ()" }
    , { symbol = "rand", signature = "RandState -> (Double, RandState)" }
    , { symbol = "randList", signature = "TestConfig -> [Int]" }
    , { symbol = "countSet", signature = "Int -> Int -> [Int] -> Int" }
    , { symbol = "countZipSet", signature = "Int -> Int -> [Int] -> Int" }
    , { symbol = "countIntSet", signature = "Int -> Int -> [Int] -> Int" }
    , { symbol = "printElapsed", signature = "(() -> a) -> IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "empty", signature = "ZipSet a" }
    , { symbol = "member", signature = "Ord a => a -> ZipSet a -> Bool" }
    , { symbol = "insert", signature = "Ord a => a -> ZipSet a -> ZipSet a" }
    , { symbol = "fromList", signature = "Ord a => [a] -> ZipSet a" }
    , { symbol = "test", signature = "ZipSet Char" }
    , { symbol = "geo", signature = "Word32 -> Word32" }
    , { symbol = "lowestBit", signature = "Word32 -> Word32" }
    , { symbol = "highestBit", signature = "Word32 -> Word32" }
    , { symbol = "rotl", signature = "Word32 -> Int -> Word32" }
    , { symbol = "next", signature = "RandState -> (Word32, RandState)" }
    , { symbol = "flattenPatterns", signature = "Program -> Program" }
    , { symbol = "flattenDef", signature = "Def -> Def" }
    , { symbol = "flattenPatternsExpr", signature = "Expr -> Expr" }
    , { symbol = "freshName", signature = "Fresh -> Text -> (Name, Fresh)" }
    , { symbol = "freshNames", signature = "Fresh -> Text -> Int -> ([Name], Fresh)" }
    , { symbol = "goExpr", signature = "Fresh -> Expr -> (Expr, Fresh)" }
    , { symbol = "goList", signature = "Fresh -> [Expr] -> ([Expr], Fresh)" }
    , { symbol = "goGroups", signature = "Fresh -> [BindGroup] -> ([BindGroup], Fresh)" }
    , { symbol = "goGroup", signature = "Fresh -> BindGroup -> (BindGroup, Fresh)" }
    , { symbol = "goBranches", signature = "Fresh -> [Branch] -> ([Branch], Fresh)" }
    , { symbol = "goBranch", signature = "Fresh -> Branch -> (Branch, Fresh)" }
    , { symbol = "compileCase", signature = "Fresh -> Expr -> [Branch] -> (Expr, Fresh)" }
    , { symbol = "anyNested", signature = "[Branch] -> Bool" }
    , { symbol = "bindScrut", signature = "Fresh -> Expr -> (Name, Expr -> Expr, Fresh)" }
    , { symbol = "compileMatrix", signature = "Fresh -> [Name] -> [Row] -> (Expr, Fresh)" }
    , { symbol = "stripTrivialCol", signature = "Name -> Row -> Row" }
    , { symbol = "distinctHeadCtors", signature = "[Row] -> [(QName, Int)]" }
    , { symbol = "specializeRow", signature = "Name -> QName -> Int -> Row -> Maybe Row" }
    , { symbol = "anyType", signature = "Type" }
    , { symbol = "deriveSelectors", signature = "Program -> Program" }
    , { symbol = "recordSelectors", signature = "Set.Set T.Text -> DataDecl -> [Def]" }
    , { symbol = "hasRecordFields", signature = "ConDecl -> Bool" }
    , { symbol = "isPositionalName", signature = "Name -> Bool" }
    , { symbol = "dataDeclType", signature = "DataDecl -> Type" }
    , { symbol = "fromKokaCore", signature = "KC.Core -> Either Text F.Program" }
    , { symbol = "toKokaCore", signature = "F.Program -> Either Text KC.Core" }
    , { symbol = "toKokaDef", signature = "F.Def -> Either Text KC.Def" }
    , { symbol = "toKokaExpr", signature = "F.Expr -> Either Text KC.Expr" }
    , { symbol = "toKokaBranch", signature = "F.Branch -> Either Text KC.Branch" }
    , { symbol = "toKokaPattern", signature = "F.Pattern -> Either Text KC.Pattern" }
    , { symbol = "toKokaBindGroup", signature = "F.BindGroup -> Either Text KC.DefGroup" }
    , { symbol = "toKokaBind", signature = "F.Bind -> Either Text KC.Def" }
    , { symbol = "toKokaDataDecl", signature = "F.DataDecl -> KC.TypeDefGroup" }
    , { symbol = "toKokaConInfo", signature = "KN.Name -> [KT.TypeVar] -> Int -> F.ConDecl -> KT.ConInfo" }
    , { symbol = "toKokaEffectDecl", signature = "F.EffectDecl -> KC.TypeDefGroup" }
    , { symbol = "opToConInfo", signature = "KN.Name -> [KT.TypeVar] -> Int -> F.OpDecl -> KT.ConInfo" }
    , { symbol = "toKokaType", signature = "F.Type -> Either Text KT.Type" }
    , { symbol = "toKokaEffect", signature = "F.EffectRow -> Either Text KT.Type" }
    , { symbol = "toKokaTypeVar", signature = "F.TypeVar -> KT.TypeVar" }
    , { symbol = "toKokaTypeCon", signature = "F.TypeCon -> KT.TypeCon" }
    , { symbol = "toKokaKind", signature = "F.Kind -> KK.Kind" }
    , { symbol = "toKokaLit", signature = "F.Lit -> KC.Lit" }
    , { symbol = "toKokaName", signature = "F.Name -> KN.Name" }
    , { symbol = "toKokaQName", signature = "F.QName -> KN.Name" }
    , { symbol = "toKokaDefSort", signature = "F.DefSort -> KS.DefSort" }
    , { symbol = "toKokaVisibility", signature = "F.Visibility -> KS.Visibility" }
    , { symbol = "toKokaTypeUnsafe", signature = "F.Type -> KT.Type" }
    , { symbol = "anyKokaType", signature = "KT.Type" }
    , { symbol = "placeholderConRepr", signature = "KC.ConRepr" }
    , { symbol = "placeholderConInfo", signature = "KT.ConInfo" }
    , { symbol = "runCps", signature = "Cps a -> a" }
    , { symbol = "freshK", signature = "Cps Name" }
    , { symbol = "anyTy", signature = "Type" }
    , { symbol = "cpsExpr", signature = "Expr -> (Expr -> Cps Expr) -> Cps Expr" }
    , { symbol = "cpsExprs", signature = "[Expr] -> ([Expr] -> Cps Expr) -> Cps Expr" }
    , { symbol = "cpsBindGroups", signature = "[BindGroup] -> Expr -> (Expr -> Cps Expr) -> Cps Expr" }
    , { symbol = "cpsTopLevel", signature = "Expr -> Expr" }
    , { symbol = "cpsExprFresh", signature = "Int -> Expr -> (Expr -> Cps Expr) -> (Expr, Int)" }
    , { symbol = "normalizePatterns", signature = "Program -> Program" }
    , { symbol = "normDef", signature = "Def -> Def" }
    , { symbol = "normExpr", signature = "Expr -> Expr" }
    , { symbol = "normBind", signature = "Bind -> Bind" }
    , { symbol = "normBranch", signature = "Branch -> Branch" }
    , { symbol = "normBranches", signature = "[Branch] -> [Branch]" }
    , { symbol = "isBoolBranches", signature = "[Branch] -> Bool" }
    , { symbol = "allPatCon", signature = "[Branch] -> Bool" }
    , { symbol = "boolToPat", signature = "Branch -> Branch" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "lastUseMap", signature = "Expr -> LastUseMap" }
    , { symbol = "lastUseMapPerBranch", signature = "Expr -> Map (Position, BranchIndex) LastUseMap" }
    , { symbol = "useEvents", signature = "Expr -> [UseEvent]" }
    , { symbol = "analyzeCycles", signature = "Program -> [CycleInfo]" }
    , { symbol = "isPotentiallyCyclic", signature = "Program -> QName -> Bool" }
    , { symbol = "analyzeDef", signature = "Def -> CycleInfo" }
    , { symbol = "findSelfRefsInData", signature = "Text -> Expr -> Set Text" }
    , { symbol = "collectVarRefs", signature = "Expr -> [Name]" }
    , { symbol = "findMutualDataCycles", signature = "Set Text -> Expr -> Set Text" }
    , { symbol = "promoteRecursiveLambdasProgram", signature = "Program -> Program" }
    , { symbol = "promoteDef", signature = "Def -> Def" }
    , { symbol = "promoteRecursiveLambdas", signature = "Set Name -> Map Name [Name] -> Expr -> Expr" }
    , { symbol = "promoteBranch", signature = "Set Name -> Map Name [Name] -> Branch -> Branch" }
    , { symbol = "handleLet", signature = "Set Name -> Map Name [Name] -> [BindGroup] -> Expr -> Expr" }
    , { symbol = "isLambdaExpr", signature = "Expr -> Bool" }
    , { symbol = "unwrapLambdaExpr", signature = "Expr -> Expr" }
    , { symbol = "isRecursive", signature = "BindGroup -> Bind -> Bool" }
    , { symbol = "computeCapturesFixpoint", signature = "Set Name -> Set Name -> Map Name [Name] -> [Bind] -> Map Name [Name]" }
    , { symbol = "collectCallTargets", signature = "Expr -> Set Name" }
    , { symbol = "freeVars", signature = "Expr -> Set Name" }
    , { symbol = "branchFreeVars", signature = "Branch -> Set Name" }
    , { symbol = "patternVars", signature = "Pattern -> [(Name, Type)]" }
    , { symbol = "partitionBy", signature = "(a -> Bool) -> [a] -> ([a], [a])" }
    , { symbol = "anyType", signature = "Type" }
    , { symbol = "prettyError", signature = "TypeError -> Text" }
    , { symbol = "qnameToText", signature = "QName -> Text" }
    , { symbol = "typeCheckProgram", signature = "Program -> [TypeError]" }
    , { symbol = "buildEnv", signature = "Program -> Env" }
    , { symbol = "typeArity", signature = "Type -> Int" }
    , { symbol = "opArityOf", signature = "Type -> Int" }
    , { symbol = "checkDef", signature = "Env -> Def -> [TypeError]" }
    , { symbol = "checkExpr", signature = "Env -> Expr -> [TypeError]" }
    , { symbol = "checkBranch", signature = "Env -> Branch -> [TypeError]" }
    , { symbol = "checkPattern", signature = "Env -> Pattern -> [TypeError]" }
    , { symbol = "checkCtorExists", signature = "Env -> QName -> [Expr] -> [TypeError]" }
    , { symbol = "checkPerform", signature = "Env -> QName -> [Expr] -> [TypeError]" }
    , { symbol = "linkPrograms", signature = "[Program] -> Either [LinkError] LinkResult" }
    , { symbol = "linkProgramsWith", signature = "Bool -> [Program] -> Either [LinkError] LinkResult" }
    , { symbol = "qualifyDefs", signature = "Program -> [Def]" }
    , { symbol = "qualifyDef", signature = "Text -> Def -> Def" }
    , { symbol = "findDuplicates", signature = "[Def] -> [LinkError]" }
    , { symbol = "findMain", signature = "[Program] -> Either LinkError Text" }
    , { symbol = "isMainDef", signature = "Def -> Bool" }
    , { symbol = "qnameToText", signature = "QName -> Text" }
    , { symbol = "mangleName", signature = "Text -> Text -> Text" }
    , { symbol = "buildSymbolTable", signature = "[Def] -> SymbolTable" }
    , { symbol = "buildConTable", signature = "[DataDecl] -> SymbolTable" }
    , { symbol = "rewriteNames", signature = "[Def] -> [DataDecl]" }
    , { symbol = "mangleDef", signature = "Def -> Def" }
    , { symbol = "mangleDataDecl", signature = "DataDecl -> DataDecl" }
    , { symbol = "mangleConDecl", signature = "ConDecl -> ConDecl" }
    , { symbol = "resolveName", signature = "Text -> Text -> SymbolTable -> Name" }
    , { symbol = "resolveQName", signature = "Text -> SymbolTable -> QName -> (QName, [Text], [LinkError])" }
    , { symbol = "rewriteExpr", signature = "Text -> Text -> SymbolTable -> SymbolTable -> Expr" }
    , { symbol = "validateProgram", signature = "Program -> [Text]" }
    , { symbol = "primitiveNames", signature = "Set Text" }
    , { symbol = "runtimeNames", signature = "Set Text" }
    , { symbol = "detectUndefinedSymbols", signature = "Program -> [Text]" }
    , { symbol = "collectVarNames", signature = "Expr -> [Text]" }
    , { symbol = "collectConQNames", signature = "Expr -> [(Text, Text)]" }
    , { symbol = "detectArityMismatches", signature = "Program -> [Text]" }
    , { symbol = "countLamParams", signature = "Expr -> Int" }
    , { symbol = "isAnyType", signature = "Type -> Bool" }
    , { symbol = "collectAppArities", signature = "Expr -> [(Text, Int)]" }
    , { symbol = "detectDuplicateExports", signature = "Program -> [Text]" }
    , { symbol = "nubTexts", signature = "[Text] -> [Text]" }
    , { symbol = "elideConsumedDropsProgram", signature = "Program -> Program" }
    , { symbol = "elideConsumedDrops", signature = "Expr -> Expr" }
    , { symbol = "isUnboxedType", signature = "Type -> Bool" }
    , { symbol = "isUnboxedTyCon", signature = "QName -> Bool" }
    , { symbol = "insertPerceus", signature = "Program -> Program" }
    , { symbol = "perceusDefTransform", signature = "Def -> Def" }
    , { symbol = "paramMultiplicity", signature = "Name -> [(Name, Type)] -> Type -> Multiplicity" }
    , { symbol = "perceusExpr", signature = "Map Name Multiplicity -> Expr -> Expr" }
    , { symbol = "perceusBindGroup", signature = "Map Name Multiplicity -> Bind -> Bind" }
    , { symbol = "perceusBranch", signature = "Map Name Multiplicity -> Maybe Name -> Branch -> Branch" }
    , { symbol = "bindMultiplicity", signature = "Bind -> Multiplicity" }
    , { symbol = "wrapRetains", signature = "Name -> Int -> Expr -> Expr" }
    , { symbol = "freeVars", signature = "Expr -> Set Name" }
    , { symbol = "branchFreeVars", signature = "Branch -> Set Name" }
    , { symbol = "patternVars", signature = "Pattern -> [(Name, Type)]" }
    , { symbol = "useNewPerceusUsage", signature = "Bool" }
    , { symbol = "analyzeUsage", signature = "Expr -> Map Name Int" }
    , { symbol = "legacyAnalyzeUsage", signature = "Expr -> Map Name Int" }
    , { symbol = "analyzeUsageNew", signature = "Expr -> Map Name Int" }
    , { symbol = "unitType", signature = "Type" }
    , { symbol = "conKey", signature = "QName -> Text" }
    , { symbol = "stableConTag", signature = "Text -> Int" }
    , { symbol = "assignProgramTags", signature = "Program -> Map Text Int" }
    , { symbol = "kkConsTag", signature = "Int" }
    , { symbol = "kkNilTag", signature = "Int" }
    , { symbol = "collectReferencedCtors", signature = "Program -> Set Text" }
    , { symbol = "ctorsInExpr", signature = "Expr -> Set Text" }
    , { symbol = "consumingUseCount", signature = "Name -> Expr -> Int" }
    , { symbol = "consumingUseCounts", signature = "[Name] -> Expr -> Map Name Int" }
    , { symbol = "emptyStats", signature = "EffectOptStats" }
    , { symbol = "effectOptimize", signature = "Program -> Program" }
    , { symbol = "effectOptimizeWithStats", signature = "Program -> (Program, EffectOptStats)" }
    , { symbol = "foldDefs", signature = "(EffectOptStats -> Expr -> (Expr, EffectOptStats))" }
    , { symbol = "inlineLocalHandlers", signature = "Expr -> Expr" }
    , { symbol = "inlineLocalHandler", signature = "EffectOptStats -> Expr -> (Expr, EffectOptStats)" }
    , { symbol = "countPerforms", signature = "Text -> Expr -> Int" }
    , { symbol = "substitutePerforms", signature = "Text -> Expr -> Expr -> Expr" }
    , { symbol = "eliminateIdentityHandlers", signature = "Expr -> Expr" }
    , { symbol = "elimIdentityHandler", signature = "EffectOptStats -> Expr -> (Expr, EffectOptStats)" }
    , { symbol = "isIdentityHandler", signature = "Expr -> Bool" }
    , { symbol = "annotateTailResumptive", signature = "Expr -> Expr" }
    , { symbol = "annotateTailRes", signature = "EffectOptStats -> Expr -> (Expr, EffectOptStats)" }
    , { symbol = "isTailResumptiveHandler", signature = "Expr -> Bool" }
    , { symbol = "isAbortHandler", signature = "Expr -> Bool" }
    , { symbol = "classifyHandler", signature = "Expr -> HandlerKind" }
    , { symbol = "countAppsOf", signature = "Text -> Expr -> Int" }
    , { symbol = "containsName", signature = "Text -> Expr -> Bool" }
    , { symbol = "isTailCall", signature = "Text -> Expr -> Bool" }
    , { symbol = "effectRowNameText", signature = "EffectRow -> Text" }
    , { symbol = "emptyScope", signature = "Scope" }
    , { symbol = "insertEvidence", signature = "Text -> Name -> Scope -> Scope" }
    , { symbol = "insertOp", signature = "Text -> Text -> Name -> Scope -> Scope" }
    , { symbol = "collectGlobalEffects", signature = "Program -> Map Text [OpDecl]" }
    , { symbol = "evidencePass", signature = "Program -> Program" }
    , { symbol = "evidencePassGlobal", signature = "Map Text [OpDecl] -> Program -> Program" }
    , { symbol = "evidencePassDef", signature = "[EffectDecl] -> Def -> Def" }
    , { symbol = "evidenceExpr", signature = "[EffectDecl] -> Scope -> Int -> Expr -> (Expr, Int)" }
    , { symbol = "mapAccumEvidence", signature = "[EffectDecl] -> Scope -> Int -> [Expr] -> ([Expr], Int)" }
    , { symbol = "mapAccumBindGroups", signature = "[EffectDecl] -> Scope -> Int -> [[Bind]] -> ([[Bind]], Int)" }
    , { symbol = "mapAccumBinds", signature = "[EffectDecl] -> Scope -> Int -> [Bind] -> ([Bind], Int)" }
    , { symbol = "mapAccumBranches", signature = "[EffectDecl] -> Scope -> Int -> [Branch] -> ([Branch], Int)" }
    , { symbol = "mkOpBind", signature = "Text -> Name -> Int -> OpDecl -> (Bind, (Text, Name))" }
    , { symbol = "effectRowName", signature = "EffectRow -> Text" }
    , { symbol = "lookupEffectDecl", signature = "[EffectDecl] -> Text -> Maybe EffectDecl" }
    , { symbol = "isSingleOpEffect", signature = "[EffectDecl] -> Text -> Bool" }
    , { symbol = "lookupOpIndex", signature = "[EffectDecl] -> Text -> Text -> Int" }
    , { symbol = "anyType", signature = "Type" }
    , { symbol = "substEFunRef", signature = "Scope -> Expr -> Expr" }
    , { symbol = "collectTopNames", signature = "[Program] -> Set (Text, Text)" }
    , { symbol = "isFrankensteinModule", signature = "Text -> Bool" }
    , { symbol = "evidencePassEvv", signature = "Program -> Program" }
    , { symbol = "evidencePassEvvGlobal", signature = "Set (Text, Text) -> Program -> Program" }
    , { symbol = "qnameToFlat", signature = "QName -> (Text, Text)" }
    , { symbol = "transformDef", signature = "[EffectDecl] -> Set (Text, Text) -> Def -> Def" }
    , { symbol = "prependEvvType", signature = "Type -> Type" }
    , { symbol = "flatTypeArity", signature = "Type -> Int" }
    , { symbol = "transformExpr", signature = "[EffectDecl] -> Set (Text, Text) -> Name -> Expr -> Expr" }
    , { symbol = "eApp", signature = "Text -> [Expr] -> Expr" }
    , { symbol = "freshName", signature = "Text -> Text -> Name" }
    , { symbol = "hashEffectName", signature = "Text -> Integer" }
    , { symbol = "lookupOpIdx", signature = "[EffectDecl] -> Text -> Text -> Int" }
    , { symbol = "lookupEffectDecl", signature = "[EffectDecl] -> Text -> Maybe EffectDecl" }
    , { symbol = "effectRowName", signature = "EffectRow -> Text" }
    , { symbol = "anyType", signature = "Type" }
    , { symbol = "detectLibDir", signature = "IO FilePath" }
    , { symbol = "compileToCore", signature = "FilePath -> IO (Either Text GhcCoreResult)" }
    , { symbol = "compileToCoreWith", signature = "Bool -> FilePath -> IO (Either Text GhcCoreResult)" }
    , { symbol = "runGhcCompile", signature = "Bool -> FilePath -> FilePath -> IO (Either Text GhcCoreResult)" }
    , { symbol = "compileToCoreMulti", signature = "Bool -> FilePath -> IO (Either Text [GhcCoreResult])" }
    , { symbol = "runGhcCompileMulti", signature = "Bool -> FilePath -> FilePath -> IO (Either Text [GhcCoreResult])" }
    , { symbol = "translateModSummary", signature = "Bool -> ModSummary -> Ghc (Either (Text, Text) GhcCoreResult)" }
    , { symbol = "translateProgram", signature = "Text -> CoreProgram -> [TyCon] -> Either Text F.Program" }
    , { symbol = "disambiguateLetBindings", signature = "[F.Def] -> [F.Def]" }
    , { symbol = "applyMainIfFunctionAlias", signature = "[F.Def] -> [F.Def]" }
    , { symbol = "translateExpr", signature = "CoreExpr -> F.Expr" }
    , { symbol = "translateTopBind", signature = "CoreBind -> [F.Def]" }
    , { symbol = "trExpr", signature = "CoreExpr -> F.Expr" }
    , { symbol = "collectValueLams", signature = "CoreExpr -> ([(F.Name, F.Type)], CoreExpr)" }
    , { symbol = "collectArgs", signature = "CoreExpr -> (CoreExpr, [CoreExpr])" }
    , { symbol = "isDictArg", signature = "CoreExpr -> Bool" }
    , { symbol = "isRealWorldArg", signature = "CoreExpr -> Bool" }
    , { symbol = "isTypeArg", signature = "CoreExpr -> Bool" }
    , { symbol = "ghcIoOutputRuntime", signature = "Var -> Maybe String" }
    , { symbol = "knownShowCharCAF", signature = "Var -> Maybe Char" }
    , { symbol = "knownShowCAF", signature = "Var -> Maybe BS.ByteString" }
    , { symbol = "unpackLitStringToCons", signature = "BS.ByteString -> F.Expr -> F.Expr" }
    , { symbol = "isShowIntListMethod", signature = "Var -> Bool" }
    , { symbol = "collectStaticShowList", signature = "CoreExpr -> [CoreExpr]" }
    , { symbol = "expandTupleShowChain", signature = "F.Expr -> [CoreExpr] -> F.Expr -> F.Expr" }
    , { symbol = "isShowTupleSgo", signature = "Var -> Bool" }
    , { symbol = "isShowIntWorker", signature = "Var -> Bool" }
    , { symbol = "unboxIntCon", signature = "CoreExpr -> F.Expr" }
    , { symbol = "pickShowArgs", signature = "String -> [CoreExpr] -> Maybe (CoreExpr, CoreExpr)" }
    , { symbol = "isStateVar", signature = "Var -> Bool" }
    , { symbol = "isPrefixOf", signature = "String -> String -> Bool" }
    , { symbol = "isInfixOfStr", signature = "String -> String -> Bool" }
    , { symbol = "isDictDef", signature = "F.Def -> Bool" }
    , { symbol = "translateBind", signature = "CoreBind -> [F.BindGroup]" }
    , { symbol = "translateAlt", signature = "Alt Var -> F.Branch" }
    , { symbol = "translateAltCon", signature = "AltCon -> [Var] -> F.Pattern" }
    , { symbol = "translateLit", signature = "Literal -> F.Lit" }
    , { symbol = "translateName", signature = "Var -> F.Name" }
    , { symbol = "normalizeGhcBuiltin", signature = "Var -> Maybe Text" }
    , { symbol = "translateTyVar", signature = "Var -> F.TypeVar" }
    , { symbol = "qualifyName", signature = "Var -> F.QName" }
    , { symbol = "translateType", signature = "Type -> F.Type" }
    , { symbol = "collectForAlls", signature = "Type -> ([ForAllTyBinder], Type)" }
    , { symbol = "forAllBndrToTypeVar", signature = "ForAllTyBinder -> F.TypeVar" }
    , { symbol = "tyConQName", signature = "TyCon -> F.QName" }
    , { symbol = "anyType", signature = "F.Type" }
    , { symbol = "classifyBind", signature = "Var -> F.DefSort" }
    , { symbol = "decideLaziness", signature = "Var -> CoreExpr -> F.Expr" }
    , { symbol = "wrapLazyUses", signature = "Set F.Name -> F.Expr -> F.Expr" }
    , { symbol = "patternBoundNames", signature = "F.Pattern -> Set F.Name" }
    , { symbol = "isCheapValue", signature = "CoreExpr -> Bool" }
    , { symbol = "collectReferencedTyCons", signature = "CoreProgram -> [TyCon]" }
    , { symbol = "dedupTyCons", signature = "[TyCon] -> [TyCon]" }
    , { symbol = "translateTyCons", signature = "[TyCon] -> [F.DataDecl]" }
    , { symbol = "translateTyCon", signature = "TyCon -> F.DataDecl" }
    , { symbol = "translateDataCon", signature = "DataCon -> F.ConDecl" }
    , { symbol = "multiOutputMarkers", signature = "Text -> [MercuryPred] -> Set Text" }
    , { symbol = "semidetPredMarkers", signature = "Text -> [MercuryPred] -> Set Text" }
    , { symbol = "translateHlds", signature = "MercuryHLDS -> Either Text Program" }
    , { symbol = "translateMultiHlds", signature = "[MercuryHLDS] -> Either Text Program" }
    , { symbol = "makeMultiWrapper", signature = "Text -> MercuryPred -> [Def]" }
    , { symbol = "mercuryEffects", signature = "[EffectDecl]" }
    , { symbol = "translateMercuryTypeDecl", signature = "Text -> MercuryTypeDecl -> DataDecl" }
    , { symbol = "translatePred", signature = "Set Text -> Text -> MercuryPred -> Either Text Def" }
    , { symbol = "modeToMult", signature = "MercuryMode -> Multiplicity" }
    , { symbol = "detToEffectRow", signature = "MercuryDet -> EffectRow" }
    , { symbol = "translateGoalAsTest", signature = "Set Text -> Set Text -> MercuryGoal -> Expr" }
    , { symbol = "translateGoal", signature = "MercuryGoal -> Expr" }
    , { symbol = "translateSemidetBody", signature = "Set Text -> Set Text -> MercuryGoal -> Expr -> Expr" }
    , { symbol = "rewriteTypeclassMethod", signature = "Text -> [Text] -> Maybe (Text, [Text])" }
    , { symbol = "isKnownStdlibFunction", signature = "Text -> Bool" }
    , { symbol = "translateGoalK", signature = "Set Text -> Set Text -> MercuryGoal -> Expr -> Expr" }
    , { symbol = "extendBindingsFor", signature = "Set Text -> MercuryGoal -> Set Text" }
    , { symbol = "goalAlwaysAborts", signature = "MercuryGoal -> Bool" }
    , { symbol = "collectCtorsFromPred", signature = "MercuryPred -> [(Text, Text)]" }
    , { symbol = "stdlibCtorNames", signature = "Set Text" }
    , { symbol = "argExpr", signature = "Text -> Expr" }
    , { symbol = "parseMercuryFloatLit", signature = "Text -> Maybe Double" }
    , { symbol = "parseMercuryCharLit", signature = "Text -> Maybe Integer" }
    , { symbol = "bareTag", signature = "Text -> Text" }
    , { symbol = "intTy", signature = "Type" }
    , { symbol = "valueTy", signature = "Type" }
    , { symbol = "boolTy", signature = "Type" }
    , { symbol = "anyTy", signature = "Type" }
    , { symbol = "stringTy", signature = "Type" }
    , { symbol = "parseMercuryStringLit", signature = "Text -> Maybe Text" }
    , { symbol = "dumpHlds", signature = "FilePath -> IO (Either Text Text)" }
    , { symbol = "dumpHldsProgram", signature = "FilePath -> IO (Either Text [(Text, Text)])" }
    , { symbol = "bfsUserImports", signature = "FilePath -> Text -> IO [Text]" }
    , { symbol = "opaqueModules", signature = "Set.Set Text" }
    , { symbol = "readImports", signature = "FilePath -> IO [Text]" }
    , { symbol = "dumpInWorkDir", signature = "FilePath -> FilePath -> IO (Either Text Text)" }
    , { symbol = "dumpInWorkDirWith", signature = "FilePath -> [FilePath] -> FilePath -> IO (Either Text Text)" }
    , { symbol = "parseHldsDump", signature = "Text -> Either Text MercuryHLDS" }
    , { symbol = "extractPredicates", signature = "[Text] -> [MercuryPred]" }
    , { symbol = "isStdlibPredHeader", signature = "Text -> Bool" }
    , { symbol = "stdlibModules", signature = "Set.Set Text" }
    , { symbol = "parsePredBlock", signature = "Text -> [Text] -> MercuryPred" }
    , { symbol = "renameLambdaLhses", signature = "Text -> MercuryGoal -> MercuryGoal" }
    , { symbol = "extractNameArity", signature = "Text -> (Text, Int)" }
    , { symbol = "parseModeDecl", signature = "Text -> ([MercuryMode], MercuryDet)" }
    , { symbol = "parseMode", signature = "Text -> MercuryMode" }
    , { symbol = "parseDet", signature = "Text -> MercuryDet" }
    , { symbol = "extractArgNames", signature = "[Text] -> [Text]" }
    , { symbol = "extractGoalText", signature = "[Text] -> Text" }
    , { symbol = "parseGoalText", signature = "Text -> MercuryGoal" }
    , { symbol = "parseGoalLines", signature = "[Text] -> MercuryGoal" }
    , { symbol = "splitLambda", signature = "[Text] -> Maybe (Text, [Text], [Text], [Text])" }
    , { symbol = "splitNot", signature = "[Text] -> Maybe [Text]" }
    , { symbol = "splitIfThenElse", signature = "[Text] -> Maybe ([Text], [Text], [Text])" }
    , { symbol = "isComment", signature = "Text -> Bool" }
    , { symbol = "splitOnMarker", signature = "Text -> [Text] -> [[Text]]" }
    , { symbol = "stripConjunctionWrapperMaybe", signature = "[Text] -> Maybe [Text]" }
    , { symbol = "parseSwitch", signature = "[Text] -> MercuryGoal" }
    , { symbol = "extractSwitchArms", signature = "[Text] -> [(Text, MercuryGoal)]" }
    , { symbol = "parseSingleGoal", signature = "Text -> MercuryGoal" }
    , { symbol = "parseCtorApp", signature = "Text -> Maybe (Text, [Text])" }
    , { symbol = "isLambdaRhs", signature = "Text -> Bool" }
    , { symbol = "parseQuotedOpCall", signature = "Text -> Maybe (Text, [Text])" }
    , { symbol = "parseListLiteral", signature = "Text -> Text -> Maybe MercuryGoal" }
    , { symbol = "parseTupleLiteral", signature = "Text -> Text -> Maybe MercuryGoal" }
    , { symbol = "parseQualifiedOp", signature = "Text -> Maybe Text" }
    , { symbol = "parseMercuryBuiltin", signature = "Text -> Maybe MercuryGoal" }
    , { symbol = "extractTypeDecls", signature = "[Text] -> [MercuryTypeDecl]" }
    , { symbol = "parseTypeDecl", signature = "Text -> Maybe MercuryTypeDecl" }
    , { symbol = "parseTypeLhs", signature = "Text -> (Text, [Text])" }
    , { symbol = "parseCtorText", signature = "Text -> (Text, [Text])" }
    , { symbol = "splitCtorArgs", signature = "Text -> [Text]" }
    , { symbol = "parseTypeCtorLine", signature = "Text -> [Text] -> (Maybe MercuryTypeDecl, [Text])" }
    , { symbol = "translateMir", signature = "MirProgram -> Either Text Program" }
    , { symbol = "translateBody", signature = "MirBody -> Either Text Def" }
    , { symbol = "localTypeToType", signature = "MirLocalDecl -> Type" }
    , { symbol = "rustTypeToType", signature = "Text -> Type" }
    , { symbol = "intType", signature = "Type" }
    , { symbol = "boolType", signature = "Type" }
    , { symbol = "unitType", signature = "Type" }
    , { symbol = "translateBlocks", signature = "MirBody -> Expr" }
    , { symbol = "translateBlockAt", signature = "MirBody -> Set.Set Int -> Int -> Expr" }
    , { symbol = "translateBlockExpr", signature = "MirBody -> Set.Set Int -> MirBasicBlock -> Expr" }
    , { symbol = "wrapWithBinds", signature = "[BindGroup] -> Expr -> Expr" }
    , { symbol = "translateStmtToBinds", signature = "MirBody -> Text -> [BindGroup]" }
    , { symbol = "translateRvalue", signature = "MirBody -> MirRvalue -> Expr" }
    , { symbol = "translateOperand", signature = "MirBody -> MirOperand -> Expr" }
    , { symbol = "parseConstLit", signature = "Text -> Expr" }
    , { symbol = "parseFloatBits", signature = "Text -> Maybe Integer" }
    , { symbol = "mirBinOpToName", signature = "Text -> Text" }
    , { symbol = "translateTermExpr", signature = "MirBody -> Set.Set Int -> MirTerminator -> Text -> Expr" }
    , { symbol = "parseCallArg", signature = "Text -> Expr" }
    , { symbol = "findCallDest", signature = "Text -> Maybe Text" }
    , { symbol = "argIndex", signature = "Text -> Maybe Int" }
    , { symbol = "findMutRefTarget", signature = "MirBody -> Int -> Maybe Int" }
    , { symbol = "findLocalByName", signature = "MirBody -> Text -> Maybe MirLocalDecl" }
    , { symbol = "mkSwitchBranch", signature = "MirBody -> Set.Set Int -> MirSwitchTarget -> Branch" }
    , { symbol = "findLocal", signature = "MirBody -> Int -> Maybe MirLocalDecl" }
    , { symbol = "lookupBlock", signature = "MirBody -> Int -> Maybe MirBasicBlock" }
    , { symbol = "displayTypeWrapper", signature = "Text -> Maybe Text" }
    , { symbol = "remapRustIntrinsic", signature = "Text -> Text" }
    , { symbol = "skipWs", signature = "Text -> Text" }
    , { symbol = "parseJValue", signature = "Text -> ParseResult JValue" }
    , { symbol = "parseJString", signature = "Text -> ParseResult JValue" }
    , { symbol = "parseStringContents", signature = "Text -> (Text, Text)" }
    , { symbol = "parseJNum", signature = "Text -> ParseResult JValue" }
    , { symbol = "parseJArray", signature = "Text -> ParseResult JValue" }
    , { symbol = "parseArrayElems", signature = "Text -> [JValue] -> ParseResult JValue" }
    , { symbol = "parseJObject", signature = "Text -> ParseResult JValue" }
    , { symbol = "parseObjFields", signature = "Text -> [(Text, JValue)] -> ParseResult JValue" }
    , { symbol = "parseObjField", signature = "Text -> Text -> [(Text, JValue)] -> ParseResult JValue" }
    , { symbol = "jLookup", signature = "Text -> [(Text, JValue)] -> Maybe JValue" }
    , { symbol = "jStr", signature = "JValue -> Maybe Text" }
    , { symbol = "jInt", signature = "JValue -> Maybe Int" }
    , { symbol = "jBool", signature = "JValue -> Maybe Bool" }
    , { symbol = "jArr", signature = "JValue -> Maybe [JValue]" }
    , { symbol = "_jObj", signature = "JValue -> Maybe [(Text, JValue)]" }
    , { symbol = "parseMirJson", signature = "Text -> Either Text MirProgram" }
    , { symbol = "jsonToMirBody", signature = "JValue -> Either Text MirBody" }
    , { symbol = "jsonToLocal", signature = "JValue -> Either Text MirLocalDecl" }
    , { symbol = "jsonToBlock", signature = "JValue -> Either Text MirBasicBlock" }
    , { symbol = "parseStmt", signature = "Text -> MirStmt" }
    , { symbol = "parsePlace", signature = "Text -> Maybe (Int, Text)" }
    , { symbol = "parseRvalue", signature = "Text -> MirRvalue" }
    , { symbol = "parseStructCtor", signature = "Text -> Maybe MirRvalue" }
    , { symbol = "parseEnumTupleCtor", signature = "Text -> Maybe MirRvalue" }
    , { symbol = "parseEnumUnitCtor", signature = "Text -> Maybe MirRvalue" }
    , { symbol = "lastPathSegment", signature = "Text -> Text" }
    , { symbol = "parseStructFields", signature = "Text -> [(Text, MirOperand)]" }
    , { symbol = "looksLikeTypeName", signature = "Text -> Bool" }
    , { symbol = "findMatchingClose", signature = "Char -> Char -> Text -> Maybe Int" }
    , { symbol = "parseBinOpRv", signature = "Text -> Text -> MirRvalue" }
    , { symbol = "parseOverflowRv", signature = "Text -> Text -> MirRvalue" }
    , { symbol = "parseFieldAccess", signature = "Text -> MirRvalue" }
    , { symbol = "stripParens", signature = "Text -> Text" }
    , { symbol = "parseOperand", signature = "Text -> Maybe MirOperand" }
    , { symbol = "splitTopLevel", signature = "Text -> [Text]" }
    , { symbol = "splitOperands", signature = "Text -> [Text]" }
    , { symbol = "parseTerminator", signature = "Text -> MirTerminator" }
    , { symbol = "isCallTerm", signature = "Text -> Bool" }
    , { symbol = "parseCallTerm", signature = "Text -> MirTerminator" }
    , { symbol = "splitAtCallParen", signature = "Text -> (Text, Text)" }
    , { symbol = "parseSwitchTargets", signature = "Text -> [MirSwitchTarget]" }
    , { symbol = "parseOneTarget", signature = "Text -> MirSwitchTarget" }
    , { symbol = "dumpMir", signature = "FilePath -> IO (Either Text Text)" }
    , { symbol = "tryRustcMir", signature = "FilePath -> [String] -> IO (Either String String)" }
    , { symbol = "tryShims", signature = "[FilePath] -> Maybe String -> FilePath -> IO (Either String String)" }
    , { symbol = "readProcessWithExitCodeEnv", signature = "FilePath -> [String] -> String -> Maybe [(String, String)] -> IO (ExitCode, String, String)" }
    , { symbol = "trim", signature = "String -> String" }
    , { symbol = "parseMirText", signature = "Text -> Either Text MirProgram" }
    , { symbol = "extractBodies", signature = "[Text] -> [MirBody]" }
    , { symbol = "collectBlock", signature = "Int -> [Text] -> ([Text], [Text])" }
    , { symbol = "parseBody", signature = "Text -> [Text] -> MirBody" }
    , { symbol = "insertIdx", signature = "Int -> MirLocalDecl -> [MirLocalDecl] -> [MirLocalDecl]" }
    , { symbol = "parseHeaderArg", signature = "Text -> MirLocalDecl" }
    , { symbol = "parseBodyContents", signature = "[Text] -> ([MirLocalDecl], [MirBasicBlock])" }
    , { symbol = "extractBlocks", signature = "[Text] -> [MirBasicBlock]" }
    , { symbol = "parseBbHeader", signature = "Text -> Maybe Int" }
    , { symbol = "collectBlockBody", signature = "[Text] -> ([Text], [Text])" }
    , { symbol = "classifyBlockLines", signature = "[Text] -> ([Text], Maybe Text)" }
    , { symbol = "isSkipLine", signature = "Text -> Bool" }
    , { symbol = "isTerminatorLine", signature = "Text -> Bool" }
    , { symbol = "convertTextLine", signature = "Text -> Text" }
    , { symbol = "parseLocalDecl", signature = "Int -> Text -> MirLocalDecl" }
    , { symbol = "renderModule", signature = "MlirModule -> Text" }
    , { symbol = "renderExtern", signature = "(Text, [MlirType], [MlirType]) -> Text" }
    , { symbol = "renderFunc", signature = "MlirFunc -> Text" }
    , { symbol = "renderFuncArgs", signature = "[(Text, MlirType)] -> Text" }
    , { symbol = "renderBlock", signature = "MlirBlock -> [Text]" }
    , { symbol = "renderAssign", signature = "Maybe Text -> MlirOp -> Text" }
    , { symbol = "renderOp", signature = "MlirOp -> Text" }
    , { symbol = "renderOps", signature = "[MlirOp] -> Text" }
    , { symbol = "renderType", signature = "MlirType -> Text" }
    , { symbol = "renderTypes", signature = "[MlirType] -> Text" }
    , { symbol = "renderRetTypes", signature = "[MlirType] -> Text" }
    , { symbol = "postProcessFile", signature = "FilePath -> IO ()" }
    , { symbol = "postProcessText", signature = "Map FilePath Text -> Text -> Text" }
    , { symbol = "fixIntraModuleCalls", signature = "Text -> Text" }
    , { symbol = "collectStubDollarN", signature = "[Text] -> Map Text (Text, Int, Int, (Int, Int))" }
    , { symbol = "isStubName", signature = "Map Text a -> Text -> Bool" }
    , { symbol = "collectPrivateDollarN", signature = "[Text] -> Map Text (Text, Int, Int)" }
    , { symbol = "collectFuncDefs", signature = "[Text] -> Map Text Int" }
    , { symbol = "mkSimpleAlias", signature = "Text -> Text -> Int -> Text" }
    , { symbol = "mkOversaturated", signature = "Text -> Text -> Int -> Int -> Text" }
    , { symbol = "mkUndersaturated", signature = "Text -> Text -> Int -> Int -> [Text] -> [Text]" }
    , { symbol = "isPrivateInSet", signature = "Set Text -> Text -> Bool" }
    , { symbol = "insertBeforeFinalBrace", signature = "[Text] -> Text -> Text" }
    , { symbol = "fixMlirArity", signature = "Text -> Text" }
    , { symbol = "collectFuncDeclsWithParams", signature = "[Text] -> Map Text (Int, [Text])" }
    , { symbol = "extractParamNames", signature = "Text -> [Text]" }
    , { symbol = "parseFuncCall", signature = "Text -> Maybe FuncCall" }
    , { symbol = "trimPhantoms", signature = "[Text] -> [Text] -> Int -> ([Text], [Text])" }
    , { symbol = "rebuildCall", signature = "FuncCall -> [Text] -> Text -> Text" }
    , { symbol = "knownFunctions", signature = "Set Text" }
    , { symbol = "loadStage1Cache", signature = "FilePath -> IO (Map FilePath Text)" }
    , { symbol = "fixDollar0Refs", signature = "Map FilePath Text -> Text -> Text" }
    , { symbol = "collectDollar0Fixes", signature = "Map FilePath Text -> [Text] -> [Dollar0Fix]" }
    , { symbol = "findReExtractTarget", signature = "Text -> [Text] -> Int -> Maybe Int -> Maybe (Text, Int)" }
    , { symbol = "ssaInScopeAt", signature = "[Text] -> Int -> Int -> Bool" }
    , { symbol = "parseKkFieldProducer", signature = "Text -> Text -> Maybe (Text, Text)" }
    , { symbol = "parseConstIdx", signature = "Text -> Text -> Maybe Int" }
    , { symbol = "findExternalArity", signature = "Text -> Map FilePath Text -> Maybe Int" }
    , { symbol = "parseDollar0", signature = "Text -> Maybe (Text, Text, Text)" }
    , { symbol = "isIdentChar", signature = "Char -> Bool" }
    , { symbol = "findEnclosingFunc", signature = "[Text] -> Int -> (Maybe Text, Maybe Int)" }
    , { symbol = "parseFuncDeclName", signature = "Text -> Maybe Text" }
    , { symbol = "trailingDigits", signature = "Text -> Maybe Text" }
    , { symbol = "fixByTrailingUnique", signature = "Text -> [Text] -> Int -> Maybe Int -> Maybe Text" }
    , { symbol = "parseAliasComment", signature = "Text -> Maybe (Text, Text)" }
    , { symbol = "isInScope", signature = "[Text] -> Int -> Int -> Bool" }
    , { symbol = "findPapWrapper", signature = "Text -> [Text] -> Maybe (Text, Int)" }
    , { symbol = "applyDollar0Fixes", signature = "[Dollar0Fix] -> [Text] -> Text" }
    , { symbol = "mkReExtractBlock", signature = "Int -> Text -> Text -> Int -> [Text]" }
    , { symbol = "externPapName", signature = "Text -> Text" }
    , { symbol = "injectExternWrapper", signature = "Text -> Int -> Text -> Text" }
    , { symbol = "insertAtModuleClose", signature = "Text -> Text -> Text" }
    , { symbol = "mkPapBlock", signature = "Int -> Text -> Int -> [Text]" }
    , { symbol = "findFuncEnd", signature = "[Text] -> Int -> Int" }
    , { symbol = "parsePrivateDollar0", signature = "Text -> Maybe Text" }
    , { symbol = "applyRenames", signature = "Map Text Text -> Text -> Text" }
    , { symbol = "tshow", signature = "Int -> Text" }
    , { symbol = "readInt", signature = "Text -> Maybe Int" }
    , { symbol = "countCommaSep", signature = "Text -> Int" }
    , { symbol = "lastIndexOf", signature = "Text -> Text -> Maybe Int" }
    , { symbol = "firstJust", signature = "(a -> Maybe b) -> [a] -> Maybe b" }
    , { symbol = "defaultEmitConfig", signature = "EmitConfig" }
    , { symbol = "useNewPerceusCount", signature = "Bool" }
    , { symbol = "freshName", signature = "Text -> Emit Text" }
    , { symbol = "addLiftedFn", signature = "Text -> Emit ()" }
    , { symbol = "addLiftedFnOnce", signature = "Text -> Text -> Emit ()" }
    , { symbol = "qualifyTop", signature = "Text -> Emit Text" }
    , { symbol = "addExternDecl", signature = "Text -> Int -> Emit ()" }
    , { symbol = "externMangled", signature = "Text -> Int -> Text" }
    , { symbol = "isKokaBuiltin", signature = "Text -> Bool" }
    , { symbol = "emitKokaBuiltins", signature = "[(Text, Int)] -> Text" }
    , { symbol = "emitPapClosure", signature = "Text -> Int -> [Text] -> Emit ([Text], Text)" }
    , { symbol = "papCallTarget", signature = "Text -> Text" }
    , { symbol = "ensurePapWrapper", signature = "Text -> Int -> Int -> Emit Text" }
    , { symbol = "emitFnAsValue", signature = "Text -> Map Text Int -> Emit ([Text], Text)" }
    , { symbol = "emitFnAsValueWithArgs", signature = "Text -> Int -> [Text] -> Emit ([Text], Text)" }
    , { symbol = "recordType", signature = "Text -> Text -> Emit ()" }
    , { symbol = "f64BitsTag", signature = "Text" }
    , { symbol = "recordF64Bits", signature = "Text -> Emit ()" }
    , { symbol = "lookupType", signature = "Text -> Emit Text" }
    , { symbol = "isF64Bits", signature = "Text -> Emit Bool" }
    , { symbol = "addStringLit", signature = "Text -> Emit Text" }
    , { symbol = "emitProgram", signature = "Program -> Text" }
    , { symbol = "emitProgramText", signature = "Program -> Text" }
    , { symbol = "emitProgramWithEffects", signature = "Program -> Text" }
    , { symbol = "emitProgramWasm", signature = "Program -> Text" }
    , { symbol = "reachableDefs", signature = "[Def] -> [Def]" }
    , { symbol = "builtinOpNames", signature = "Set Text" }
    , { symbol = "builtinWrapperSpec", signature = "Text -> Maybe (Text, Int, Text)" }
    , { symbol = "etaExpandBuiltinAlias", signature = "Def -> Def" }
    , { symbol = "emitDefs", signature = "[Def] -> Emit Text" }
    , { symbol = "emitDef", signature = "Def -> Emit Text" }
    , { symbol = "emitBody", signature = "Expr -> Text -> Emit Text" }
    , { symbol = "emitCycleCandidate", signature = "Text -> Emit [Text]" }
    , { symbol = "lookupArityInMap", signature = "Map Text Int -> Text -> Maybe Int" }
    , { symbol = "lookupNameArity", signature = "Map Text Int -> Name -> Maybe Int" }
    , { symbol = "detectExprArity", signature = "Map Text Int -> Expr -> Maybe Int" }
    , { symbol = "emitSingleClosureCall", signature = "Text -> [Text] -> Emit ([Text], Text)" }
    , { symbol = "emitClosureApp", signature = "Text -> [Text] -> Maybe Int -> Emit ([Text], Text)" }
    , { symbol = "emitExpr", signature = "Expr -> Emit ([Text], Text)" }
    , { symbol = "emitAppVar", signature = "Name -> [Expr] -> Emit ([Text], Text)" }
    , { symbol = "emitAppVarWith3", signature = "Name -> Expr -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitAppVarWith2", signature = "Name -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitAppVarWith1", signature = "Name -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitAppVarWith0", signature = "Name -> Emit ([Text], Text)" }
    , { symbol = "emitAppVarGeneral", signature = "Name -> [Expr] -> Emit ([Text], Text)" }
    , { symbol = "emitCaseDispatch", signature = "Expr -> [Branch] -> Emit ([Text], Text)" }
    , { symbol = "emitLetBindings", signature = "[BindGroup] -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitLambdaLift", signature = "[(Name, Type)] -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitOrderPredTag", signature = "Text -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitOrderPred", signature = "Text -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitParityPred", signature = "Text -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitLibm1", signature = "Text -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitLibm2", signature = "Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitListHOF2", signature = "Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitListHOF3", signature = "Text -> Expr -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitBinOp", signature = "Text -> Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "floatVariant", signature = "Text -> Text" }
    , { symbol = "emitCmpOp", signature = "Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitFloatCmpOp", signature = "Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitIntCase", signature = "[Text] -> Text -> Integer -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitMultiIntCase", signature = "[Text] -> Text -> [(Integer, Expr)] -> Expr -> Emit ([Text], Text)" }
    , { symbol = "isBoolConCase", signature = "[(QName, [Pattern], Expr)] -> Bool" }
    , { symbol = "emitBoolConCase", signature = "[Text] -> Text -> [(QName, [Pattern], Expr)] -> Maybe Expr -> Emit ([Text], Text)" }
    , { symbol = "emitConCase", signature = "[Text] -> Text -> [(QName, [Pattern], Expr)] -> Maybe Expr -> Emit ([Text], Text)" }
    , { symbol = "emitPatternBindings", signature = "Text -> Text -> [Pattern] -> Emit ([Text], [Text])" }
    , { symbol = "emitPatField", signature = "Text -> Text -> (Int, Pattern) -> Emit ([Text], Text)" }
    , { symbol = "emitIfElse", signature = "[Text] -> Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitScfIf", signature = "[Text] -> Text -> Expr -> Expr -> Emit ([Text], Text)" }
    , { symbol = "emitBind", signature = "Bind -> Emit [Text]" }
    , { symbol = "precomputeCaptures", signature = "Text -> [Bind] -> Emit ()" }
    , { symbol = "precomputeCapturesWith", signature = "Text -> [Text] -> [Bind] -> Emit ()" }
    , { symbol = "emitBindAsTopFn", signature = "Text -> Bind -> Emit [Text]" }
    , { symbol = "emitBindAsTopFnWith", signature = "Text -> Text -> Bind -> Emit [Text]" }
    , { symbol = "classifyBranches", signature = "[Branch] -> BranchClass" }
    , { symbol = "isIntLit", signature = "Pattern -> Bool" }
    , { symbol = "isCharLit", signature = "Pattern -> Bool" }
    , { symbol = "isConPat", signature = "Pattern -> Bool" }
    , { symbol = "isDefaultPat", signature = "Pattern -> Bool" }
    , { symbol = "lookupConTag", signature = "QName -> Emit Int" }
    , { symbol = "conStructType", signature = "Int -> Text" }
    , { symbol = "closureStructType", signature = "Int -> Text" }
    , { symbol = "foldInsertFields", signature = "Text -> Text -> [Text] -> Int -> Emit ([Text], Text)" }
    , { symbol = "freeVarsExpr", signature = "Expr -> Set Name" }
    , { symbol = "brFreeVars", signature = "Branch -> Set Name" }
    , { symbol = "patVars", signature = "Pattern -> [(Name, Type)]" }
    , { symbol = "decomposeDefType", signature = "Type -> ([Text], [Type], Type)" }
    , { symbol = "typeToMlir", signature = "Type -> Text" }
    , { symbol = "effectRowNameEmit", signature = "EffectRow -> Text" }
    , { symbol = "sanitizeName", signature = "Text -> Text" }
    , { symbol = "isLambda", signature = "Expr -> Bool" }
    , { symbol = "countLamParams", signature = "Expr -> Int" }
    , { symbol = "isRecLetLambda", signature = "Text -> Bind -> Bool" }
    , { symbol = "dedupeQualN", signature = "Text -> Int -> Emit Text" }
    , { symbol = "qualifyBindName", signature = "Text -> Name -> Text" }
    , { symbol = "unwrapLambda", signature = "Expr -> Expr" }
    , { symbol = "buildTopFnArity", signature = "Text -> [Def] -> Map Text Int" }
    , { symbol = "externalRuntimeFns", signature = "Set Text" }
    , { symbol = "externalRuntimeArity", signature = "Map Text Int" }
    , { symbol = "nameToSsa", signature = "Name -> Text" }
    , { symbol = "escapeMLIRString", signature = "Text -> Text" }
    , { symbol = "runCmd", signature = "String -> [String] -> String -> Text -> IO (Either Text String)" }
    , { symbol = "compileToExecutable", signature = "EmitConfig -> Program -> IO (Either Text FilePath)" }
    , { symbol = "compileToExecutableLink", signature = "EmitConfig -> FilePath -> IO (Either Text FilePath)" }
    , { symbol = "compileToWasm", signature = "EmitConfig -> Program -> IO (Either Text FilePath)" }
    , { symbol = "compileToWasmLink", signature = "EmitConfig -> FilePath -> FilePath -> FilePath -> IO (Either Text FilePath)" }
    , { symbol = "emitProgram", signature = "C.Program -> Text" }
    , { symbol = "emitOrganIR", signature = "C.Program -> O.OrganIR" }
    , { symbol = "emitModule", signature = "C.Program -> O.Module" }
    , { symbol = "defaultMetadata", signature = "O.Metadata" }
    , { symbol = "emitName", signature = "C.Name -> O.Name" }
    , { symbol = "emitQName", signature = "C.QName -> O.QName" }
    , { symbol = "emitVisibility", signature = "C.Visibility -> O.Visibility" }
    , { symbol = "emitMultiplicity", signature = "C.Multiplicity -> O.Multiplicity" }
    , { symbol = "emitKind", signature = "C.Kind -> Maybe Text" }
    , { symbol = "emitTyVar", signature = "C.TypeVar -> O.TyVar" }
    , { symbol = "emitType", signature = "C.Type -> O.Ty" }
    , { symbol = "collectTApp", signature = "C.Type -> (C.Type, [C.Type])" }
    , { symbol = "emitFnArg", signature = "(C.Multiplicity, C.Type) -> O.FnArg" }
    , { symbol = "emitEffectRow", signature = "C.EffectRow -> O.EffectRow" }
    , { symbol = "collectEffectRow", signature = "C.EffectRow -> ([O.QName], Maybe O.Name)" }
    , { symbol = "emitExpr", signature = "C.Expr -> O.Expr" }
    , { symbol = "exprToText", signature = "C.Expr -> Text" }
    , { symbol = "emitLit", signature = "C.Lit -> O.Lit" }
    , { symbol = "emitLamParam", signature = "(C.Name, C.Type) -> O.LamParam" }
    , { symbol = "emitLetBind", signature = "C.Bind -> O.LetBind" }
    , { symbol = "emitBranch", signature = "C.Branch -> O.Branch" }
    , { symbol = "emitPattern", signature = "C.Pattern -> O.Pat" }
    , { symbol = "patToPatBinder", signature = "C.Pattern -> O.PatBinder" }
    , { symbol = "emitSort", signature = "C.DefSort -> O.Sort" }
    , { symbol = "emitDef", signature = "C.Def -> O.Definition" }
    , { symbol = "computeArity", signature = "C.Expr -> Int" }
    , { symbol = "emitDataDecl", signature = "C.DataDecl -> O.DataType" }
    , { symbol = "emitConDecl", signature = "C.ConDecl -> O.Constructor" }
    , { symbol = "emitEffectDecl", signature = "C.EffectDecl -> O.EffectDecl" }
    , { symbol = "emitOpDecl", signature = "C.OpDecl -> O.Operation" }
    , { symbol = "consumeOrganIR", signature = "O.OrganIR -> Either String C.Program" }
    , { symbol = "consumeModule", signature = "O.Module -> Either String C.Program" }
    , { symbol = "synthSConDataDecl", signature = "[O.Definition] -> Maybe C.DataDecl" }
    , { symbol = "consumeProgram", signature = "Text -> Either String C.Program" }
    , { symbol = "consumeName", signature = "O.Name -> C.Name" }
    , { symbol = "consumeQName", signature = "O.QName -> C.QName" }
    , { symbol = "consumeVisibility", signature = "O.Visibility -> C.Visibility" }
    , { symbol = "consumeMultiplicity", signature = "O.Multiplicity -> C.Multiplicity" }
    , { symbol = "defaultMultiplicity", signature = "C.Multiplicity" }
    , { symbol = "consumeKind", signature = "Maybe Text -> C.Kind" }
    , { symbol = "anyType", signature = "C.Type" }
    , { symbol = "consumeTyVar", signature = "O.TyVar -> C.TypeVar" }
    , { symbol = "consumeType", signature = "O.Ty -> C.Type" }
    , { symbol = "consumeFnArg", signature = "O.FnArg -> (C.Multiplicity, C.Type)" }
    , { symbol = "consumeEffectRow", signature = "O.EffectRow -> C.EffectRow" }
    , { symbol = "consumeExpr", signature = "O.Expr -> C.Expr" }
    , { symbol = "consumeLit", signature = "O.Lit -> C.Lit" }
    , { symbol = "consumeLamParam", signature = "O.LamParam -> (C.Name, C.Type)" }
    , { symbol = "consumeLetBind", signature = "O.LetBind -> C.Bind" }
    , { symbol = "consumeBranch", signature = "O.Branch -> C.Branch" }
    , { symbol = "consumePattern", signature = "O.Pat -> C.Pattern" }
    , { symbol = "consumePatBinder", signature = "O.PatBinder -> C.Pattern" }
    , { symbol = "consumeSort", signature = "O.Sort -> C.DefSort" }
    , { symbol = "consumeDef", signature = "O.Definition -> Either String C.Def" }
    , { symbol = "consumeDataType", signature = "O.DataType -> Either String C.DataDecl" }
    , { symbol = "consumeConstructor", signature = "O.Constructor -> C.ConDecl" }
    , { symbol = "consumeEffectDecl", signature = "O.EffectDecl -> Either String C.EffectDecl" }
    , { symbol = "consumeOperation", signature = "O.Operation -> C.OpDecl" }
    , { symbol = "translateProgram", signature = "KC.Core -> Either Text F.Program" }
    , { symbol = "translateProgramMulti", signature = "KC.Core -> [KC.Core] -> Either Text F.Program" }
    , { symbol = "buildExternMap", signature = "[KC.External] -> Map Text Text" }
    , { symbol = "findCTarget", signature = "[(KS.Target, String)] -> Text -> Text" }
    , { symbol = "extractCName", signature = "Text -> Text" }
    , { symbol = "isExternWrapper", signature = "Map Text Text -> F.Def -> Bool" }
    , { symbol = "rewriteExternRefs", signature = "Map Text Text -> F.Def -> F.Def" }
    , { symbol = "rewriteExternExpr", signature = "Map Text Text -> F.Expr -> F.Expr" }
    , { symbol = "translateDef", signature = "KC.Def -> Either Text F.Def" }
    , { symbol = "translateExpr", signature = "KC.Expr -> Either Text F.Expr" }
    , { symbol = "translatePatternBinders", signature = "[Int] -> KC.Pattern -> Either Text (F.Pattern, [AsBinder])" }
    , { symbol = "fieldProject", signature = "F.Expr -> [Int] -> F.Expr" }
    , { symbol = "wrapBodyWithAsBinders", signature = "F.Expr -> [AsBinder] -> F.Expr -> F.Expr" }
    , { symbol = "translateBranchWithScrut", signature = "F.Expr -> KC.Branch -> Either Text F.Branch" }
    , { symbol = "translateBranch", signature = "KC.Branch -> Either Text F.Branch" }
    , { symbol = "translatePattern", signature = "KC.Pattern -> Either Text F.Pattern" }
    , { symbol = "translateType", signature = "KT.Type -> Either Text F.Type" }
    , { symbol = "translateEffect", signature = "KT.Type -> Either Text F.EffectRow" }
    , { symbol = "translateTypeVar", signature = "KT.TypeVar -> F.TypeVar" }
    , { symbol = "translateTypeCon", signature = "KT.TypeCon -> F.TypeCon" }
    , { symbol = "translateKind", signature = "KK.Kind -> F.Kind" }
    , { symbol = "translateTypeDefGroup", signature = "KC.TypeDefGroup -> Either Text [F.DataDecl]" }
    , { symbol = "translateTypeDef", signature = "KC.TypeDef -> Either Text [F.DataDecl]" }
    , { symbol = "isEffectDataInfo", signature = "KT.DataInfo -> Bool" }
    , { symbol = "extractEffectDecls", signature = "KC.TypeDefGroup -> [F.EffectDecl]" }
    , { symbol = "extractEffectFromTypeDef", signature = "KC.TypeDef -> Maybe F.EffectDecl" }
    , { symbol = "conInfoToOpDecl", signature = "KT.ConInfo -> F.OpDecl" }
    , { symbol = "translateConInfo", signature = "KT.ConInfo -> Either Text F.ConDecl" }
    , { symbol = "translateDefGroup", signature = "KC.DefGroup -> Either Text F.BindGroup" }
    , { symbol = "translateDefToBind", signature = "KC.Def -> Either Text F.Bind" }
    , { symbol = "translateLit", signature = "KC.Lit -> F.Lit" }
    , { symbol = "translateTNameToName", signature = "KC.TName -> F.Name" }
    , { symbol = "translateTNameToQName", signature = "KC.TName -> F.QName" }
    , { symbol = "translateNameK", signature = "KN.Name -> F.Name" }
    , { symbol = "translateQName", signature = "KN.Name -> F.QName" }
    , { symbol = "translateDefSort", signature = "KS.DefSort -> F.DefSort" }
    , { symbol = "translateVisibility", signature = "KS.Visibility -> F.Visibility" }
    , { symbol = "translateTypeUnsafe", signature = "KT.Type -> F.Type" }
    , { symbol = "anyType", signature = "F.Type" }
    , { symbol = "isKokaNil", signature = "KN.Name -> Bool" }
    , { symbol = "isKokaCons", signature = "KN.Name -> Bool" }
    , { symbol = "isPlusPlusName", signature = "KN.Name -> Bool" }
    , { symbol = "firstParamTypeHead", signature = "KT.Type -> Maybe String" }
    , { symbol = "plusPlusRuntime", signature = "KT.Type -> String" }
    , { symbol = "lengthRuntime", signature = "KT.Type -> String" }
    , { symbol = "lengthRuntimeFor", signature = "KC.TName -> String" }
    , { symbol = "isKokaBuiltinApp", signature = "KN.Name -> Bool" }
    , { symbol = "builtinIntrinsicNames", signature = "[String]" }
    , { symbol = "translateBuiltinApp", signature = "KC.TName -> [KC.Expr] -> Either Text F.Expr" }
    , { symbol = "compileKokaFile", signature = "FilePath -> IO (Either Text F.Program)" }
    , { symbol = "splitOnChar", signature = "Char -> String -> [FilePath]" }
    , { symbol = "isStdlibModule", signature = "KN.Name -> Bool" }
    , { symbol = "findKokaLib", signature = "IO [FilePath]" }
    , { symbol = "parsePython", signature = "FilePath -> IO (Either Text PySExpr)" }
    , { symbol = "runPyHelper", signature = "FilePath -> IO (Either Text Text)" }
    , { symbol = "parseSExpr", signature = "Text -> Either Text PySExpr" }
    , { symbol = "sexpr", signature = "Text -> Either Text (PySExpr, Text)" }
    , { symbol = "sexprList", signature = "Text -> [PySExpr] -> Either Text (PySExpr, Text)" }
    , { symbol = "sexprString", signature = "Text -> [Char] -> Either Text (PySExpr, Text)" }
    , { symbol = "sexprNumber", signature = "Text -> Either Text (PySExpr, Text)" }
    , { symbol = "sexprAtom", signature = "Text -> Either Text (PySExpr, Text)" }
    , { symbol = "skipSpace", signature = "Text -> Text" }
    , { symbol = "translatePythonAst", signature = "Text -> PySExpr -> Either Text Program" }
    , { symbol = "splitTopLevels", signature = "PySExpr -> Either Text ([DataDecl], [PySExpr])" }
    , { symbol = "translateTopLevel", signature = "Text -> Set.Set Text -> PySExpr -> Either Text Def" }
    , { symbol = "translatePyModule", signature = "Text -> PySExpr -> Either Text [Def]" }
    , { symbol = "translateBlock", signature = "Set.Set Text -> [PySExpr] -> Either Text Expr" }
    , { symbol = "translateStmtTail", signature = "Set.Set Text -> PySExpr -> Either Text Expr" }
    , { symbol = "translateExpr", signature = "Set.Set Text -> PySExpr -> Either Text Expr" }
    , { symbol = "pyBinOp", signature = "Text -> Either Text Text" }
    , { symbol = "pyCmpOp", signature = "Text -> Either Text Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "exnEffectRow", signature = "EffectRow" }
    , { symbol = "pythonEffects", signature = "[EffectDecl]" }
    , { symbol = "pythonExnDefault", signature = "Def" }
    , { symbol = "expectStr", signature = "PySExpr -> Either Text Text" }
    , { symbol = "showHead", signature = "PySExpr -> String" }
    , { symbol = "parseGo", signature = "FilePath -> IO (Either Text GoSExpr)" }
    , { symbol = "runGoHelper", signature = "FilePath -> IO (Either Text Text)" }
    , { symbol = "parseSExpr", signature = "Text -> Either Text GoSExpr" }
    , { symbol = "isSpaceCh", signature = "Char -> Bool" }
    , { symbol = "sexpr", signature = "Text -> Either Text (GoSExpr, Text)" }
    , { symbol = "sexprList", signature = "Text -> [GoSExpr] -> Either Text (GoSExpr, Text)" }
    , { symbol = "sexprString", signature = "Text -> [Char] -> Either Text (GoSExpr, Text)" }
    , { symbol = "sexprNumber", signature = "Text -> Either Text (GoSExpr, Text)" }
    , { symbol = "sexprAtom", signature = "Text -> Either Text (GoSExpr, Text)" }
    , { symbol = "skipSpace", signature = "Text -> Text" }
    , { symbol = "translateGoAst", signature = "GoSExpr -> Either Text Program" }
    , { symbol = "translateGoFile", signature = "Text -> GoSExpr -> Either Text Program" }
    , { symbol = "translateDecl", signature = "Text -> GoSExpr -> Either Text [Def]" }
    , { symbol = "translateBlock", signature = "GoSExpr -> Either Text Expr" }
    , { symbol = "translateStmts", signature = "[GoSExpr] -> Either Text Expr" }
    , { symbol = "translateStmtTail", signature = "GoSExpr -> Either Text Expr" }
    , { symbol = "translateBranchOrFallthrough", signature = "GoSExpr -> Expr -> Either Text Expr" }
    , { symbol = "translateExpr", signature = "GoSExpr -> Either Text Expr" }
    , { symbol = "goBinOp", signature = "Text -> Either Text Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "expectStr", signature = "GoSExpr -> Either Text Text" }
    , { symbol = "showHead", signature = "GoSExpr -> String" }
    , { symbol = "translateFuthark", signature = "Text -> FutModule -> Either Text Program" }
    , { symbol = "translateDef", signature = "Text -> FutDef -> Either Text Def" }
    , { symbol = "translateExpr", signature = "FutExpr -> Either Text Expr" }
    , { symbol = "futBinOp", signature = "Text -> Either Text Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "parseFutharkFile", signature = "FilePath -> IO (Either Text FutModule)" }
    , { symbol = "parseFuthark", signature = "Text -> Text -> Either Text FutModule" }
    , { symbol = "failP", signature = "Text -> P a" }
    , { symbol = "peek", signature = "P (Maybe Char)" }
    , { symbol = "advance", signature = "P ()" }
    , { symbol = "skipTrivia", signature = "Text -> Text" }
    , { symbol = "ws", signature = "P ()" }
    , { symbol = "eatChar", signature = "Char -> P ()" }
    , { symbol = "eatString", signature = "Text -> P ()" }
    , { symbol = "keyword", signature = "Text -> P ()" }
    , { symbol = "isIdentStart", signature = "Char -> Bool" }
    , { symbol = "isIdentTail", signature = "Char -> Bool" }
    , { symbol = "ident", signature = "P Text" }
    , { symbol = "reservedWords", signature = "[Text]" }
    , { symbol = "intLit", signature = "P Integer" }
    , { symbol = "topLevel", signature = "P [FutDef]" }
    , { symbol = "definition", signature = "P FutDef" }
    , { symbol = "paramSpec", signature = "P Text" }
    , { symbol = "typeText", signature = "P ()" }
    , { symbol = "balanced", signature = "Int -> P ()" }
    , { symbol = "expr", signature = "P FutExpr" }
    , { symbol = "letOrIfOrBin", signature = "P FutExpr" }
    , { symbol = "letExpr", signature = "P FutExpr" }
    , { symbol = "ifExpr", signature = "P FutExpr" }
    , { symbol = "binExpr", signature = "Int -> P FutExpr" }
    , { symbol = "binLoop", signature = "Int -> FutExpr -> P FutExpr" }
    , { symbol = "nextOp", signature = "Text -> Maybe (Text, Int, Text)" }
    , { symbol = "unary", signature = "P FutExpr" }
    , { symbol = "appExpr", signature = "P FutExpr" }
    , { symbol = "atomMaybe", signature = "P (Maybe FutExpr)" }
    , { symbol = "atom", signature = "P FutExpr" }
    , { symbol = "many", signature = "P a -> P [a]" }
    , { symbol = "optional", signature = "P a -> P (Maybe a)" }
    , { symbol = "readSchemeFile", signature = "FilePath -> IO (Either Text [SExpr])" }
    , { symbol = "readScheme", signature = "Text -> Either Text [SExpr]" }
    , { symbol = "readSExprs", signature = "Text -> Either Text [SExpr]" }
    , { symbol = "readOne", signature = "Text -> Either Text (SExpr, Text)" }
    , { symbol = "readSList", signature = "Text -> [SExpr] -> Either Text (SExpr, Text)" }
    , { symbol = "readInt", signature = "Text -> Either Text (SExpr, Text)" }
    , { symbol = "readSymbol", signature = "Text -> Either Text (SExpr, Text)" }
    , { symbol = "skipTrivia", signature = "Text -> Text" }
    , { symbol = "translateSchemeFile", signature = "Text -> [SExpr] -> Either Text Program" }
    , { symbol = "translateScheme", signature = "Text -> [SExpr] -> Either Text Program" }
    , { symbol = "fresh", signature = "Text -> M Name" }
    , { symbol = "failM", signature = "Text -> M a" }
    , { symbol = "reifyK", signature = "MetaK -> M Expr" }
    , { symbol = "translateForm", signature = "Text -> SExpr -> M [Def]" }
    , { symbol = "defineValue", signature = "Text -> Text -> SExpr -> M Def" }
    , { symbol = "cps", signature = "SExpr -> MetaK -> M Expr" }
    , { symbol = "cpsCallCC", signature = "SExpr -> MetaK -> M Expr" }
    , { symbol = "cpsArgs", signature = "[SExpr] -> ([Expr] -> M Expr) -> M Expr" }
    , { symbol = "primOp", signature = "Text -> Maybe Text" }
    , { symbol = "primArity", signature = "Text -> Int" }
    , { symbol = "expectSym", signature = "SExpr -> M Text" }
    , { symbol = "extractLetBinding", signature = "SExpr -> M (Text, SExpr)" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "anyT", signature = "Type" }
    , { symbol = "showHead", signature = "SExpr -> String" }
    , { symbol = "nodeTag", signature = "Node -> Text" }
    , { symbol = "nodeAttr", signature = "Text -> Node -> Maybe Text" }
    , { symbol = "nodeStrings", signature = "Node -> [Text]" }
    , { symbol = "nodeChildren", signature = "Node -> [Node]" }
    , { symbol = "findChildren", signature = "Text -> Node -> [Node]" }
    , { symbol = "findChild", signature = "Text -> Node -> Maybe Node" }
    , { symbol = "tokenize", signature = "Text -> [Tok]" }
    , { symbol = "parseSwiftAst", signature = "Text -> Either Text Node" }
    , { symbol = "parseNode", signature = "[Tok] -> Either Text (Node, [Tok])" }
    , { symbol = "readSwiftFile", signature = "FilePath -> IO (Either Text Node)" }
    , { symbol = "emitSilCounts", signature = "FilePath -> IO (Either Text SilCounts)" }
    , { symbol = "translateSwift", signature = "Text -> Node -> Either Text Program" }
    , { symbol = "translateFuncDecl", signature = "Text -> Node -> Either Text Def" }
    , { symbol = "cleanFuncName", signature = "Text -> Text" }
    , { symbol = "translateGlobalLet", signature = "Text -> Node -> Either Text Def" }
    , { symbol = "translateBrace", signature = "Node -> Either Text Expr" }
    , { symbol = "stmtsToExpr", signature = "[Node] -> Either Text Expr" }
    , { symbol = "seqExpr", signature = "Expr -> Expr -> Expr" }
    , { symbol = "translateStmt", signature = "Node -> Either Text Expr" }
    , { symbol = "translateIfCond", signature = "Node -> Either Text Expr" }
    , { symbol = "translateExpr", signature = "Node -> Either Text Expr" }
    , { symbol = "unwrap", signature = "Node -> Either Text Expr" }
    , { symbol = "isExprNode", signature = "Node -> Bool" }
    , { symbol = "translateBinary", signature = "Node -> Either Text Expr" }
    , { symbol = "translateCall", signature = "Node -> Either Text Expr" }
    , { symbol = "argExpr", signature = "Node -> Either Text Expr" }
    , { symbol = "mapOp", signature = "Text -> Text" }
    , { symbol = "lastSegment", signature = "Text -> Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "readOCamlFile", signature = "FilePath -> IO (Either Text OLine)" }
    , { symbol = "oHead", signature = "OLine -> Text" }
    , { symbol = "oChildren", signature = "OLine -> [OLine]" }
    , { symbol = "oString", signature = "OLine -> Maybe Text" }
    , { symbol = "oInt", signature = "OLine -> Maybe Integer" }
    , { symbol = "findHead", signature = "Text -> OLine -> Maybe OLine" }
    , { symbol = "findHeads", signature = "Text -> OLine -> [OLine]" }
    , { symbol = "parseOCamlParsetree", signature = "Text -> Either Text OLine" }
    , { symbol = "indentOf", signature = "Text -> Int" }
    , { symbol = "cleanLine", signature = "Text -> Text" }
    , { symbol = "dropTrailingLoc", signature = "Text -> Text" }
    , { symbol = "translateOCaml", signature = "Text -> OLine -> Either Text Program" }
    , { symbol = "translateStructureItem", signature = "Text -> OLine -> Either Text StructResult" }
    , { symbol = "translateTypeDecls", signature = "Text -> OLine -> Either Text [DataDecl]" }
    , { symbol = "translateTypeDecl", signature = "Text -> OLine -> Either Text DataDecl" }
    , { symbol = "findPtypeVariant", signature = "OLine -> Maybe OLine" }
    , { symbol = "translateVariant", signature = "Text -> OLine -> Either Text ConDecl" }
    , { symbol = "patternName", signature = "OLine -> Either Text (Maybe Text)" }
    , { symbol = "peelParams", signature = "OLine -> Either Text ([Text], OLine)" }
    , { symbol = "paramVarName", signature = "OLine -> Text" }
    , { symbol = "translateExpr", signature = "OLine -> Either Text Expr" }
    , { symbol = "translateCase", signature = "OLine -> Either Text Branch" }
    , { symbol = "translatePattern", signature = "OLine -> Either Text Pattern" }
    , { symbol = "translateExprHead", signature = "OLine -> Either Text Expr" }
    , { symbol = "isLabel", signature = "OLine -> Bool" }
    , { symbol = "unwrapPrintInt", signature = "Expr -> Expr" }
    , { symbol = "seqExpr", signature = "Expr -> Expr -> Expr" }
    , { symbol = "mapOp", signature = "Text -> Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "dumpAstScript", signature = "Text" }
    , { symbol = "readErlangFile", signature = "FilePath -> IO (Either Text ETerm)" }
    , { symbol = "parseErlangTerms", signature = "Text -> Either Text ETerm" }
    , { symbol = "parseTerm", signature = "P ETerm" }
    , { symbol = "parseCommaSep", signature = "Char -> String -> Either String ([ETerm], String)" }
    , { symbol = "parseInt", signature = "P ETerm" }
    , { symbol = "parseAtom", signature = "P ETerm" }
    , { symbol = "translateErlang", signature = "Text -> ETerm -> Either Text Program" }
    , { symbol = "translateForm", signature = "Text -> ETerm -> Either Text [Def]" }
    , { symbol = "translateFunction", signature = "Text -> Text -> [ETerm] -> Either Text Def" }
    , { symbol = "clausesToExpr", signature = "Text -> [ETerm] -> Either Text ([Text], Expr)" }
    , { symbol = "tryCtorDispatch", signature = "Text -> [ETerm] -> Maybe (Either Text ([Text], Expr))" }
    , { symbol = "translateBody", signature = "[(Text, Text)] -> [ETerm] -> Either Text Expr" }
    , { symbol = "translateExpr", signature = "[(Text, Text)] -> ETerm -> Either Text Expr" }
    , { symbol = "mkCtorQName", signature = "Text -> QName" }
    , { symbol = "collectErlangCtors", signature = "[Def] -> [(Text, Int)]" }
    , { symbol = "ctorsInExpr", signature = "Expr -> [(Text, Int)]" }
    , { symbol = "translateCaseClause", signature = "[(Text, Text)] -> ETerm -> Either Text Branch" }
    , { symbol = "translatePattern", signature = "ETerm -> Either Text Pattern" }
    , { symbol = "patVarBindings", signature = "Pattern -> [(Text, Text)]" }
    , { symbol = "lookupName", signature = "[(Text, Text)] -> Text -> Text" }
    , { symbol = "seqExpr", signature = "Expr -> Expr -> Expr" }
    , { symbol = "mapOp", signature = "Text -> Text" }
    , { symbol = "mapUnaryOp", signature = "Text -> Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "dumpFsx", signature = "Text" }
    , { symbol = "readFSharpFile", signature = "FilePath -> IO (Either Text [SExpr])" }
    , { symbol = "translateFSharp", signature = "Text -> [SExpr] -> Either Text Program" }
    , { symbol = "translateDecl", signature = "Text -> SExpr -> Either Text Def" }
    , { symbol = "translateExpr", signature = "SExpr -> Either Text Expr" }
    , { symbol = "curryApp", signature = "Expr -> [Expr] -> Expr" }
    , { symbol = "mapOp", signature = "Text -> Text" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "parseIdris", signature = "Text -> Either Text [IDecl]" }
    , { symbol = "stripComment", signature = "Text -> Text" }
    , { symbol = "isTypeSig", signature = "Text -> Bool" }
    , { symbol = "joinDefLines", signature = "[Text] -> [Text]" }
    , { symbol = "parseDecl", signature = "Text -> Either Text IDecl" }
    , { symbol = "tokenize", signature = "Text -> Either Text [Tok]" }
    , { symbol = "parseExprFull", signature = "Text -> Either Text IExpr" }
    , { symbol = "pExpr", signature = "[Tok] -> Either Text (IExpr, [Tok])" }
    , { symbol = "pCmp", signature = "[Tok] -> Either Text (IExpr, [Tok])" }
    , { symbol = "mapCmp", signature = "Text -> Text" }
    , { symbol = "pAdd", signature = "[Tok] -> Either Text (IExpr, [Tok])" }
    , { symbol = "pMul", signature = "[Tok] -> Either Text (IExpr, [Tok])" }
    , { symbol = "pApp", signature = "[Tok] -> Either Text (IExpr, [Tok])" }
    , { symbol = "pAtom", signature = "[Tok] -> Either Text (IExpr, [Tok])" }
    , { symbol = "translateIdris", signature = "Text -> [IDecl] -> Either Text Program" }
    , { symbol = "translateDecl", signature = "Text -> IDecl -> Either Text Def" }
    , { symbol = "translateExpr", signature = "IExpr -> Either Text Expr" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "defaultShimPath", signature = "FilePath" }
    , { symbol = "defaultIdris2Prefix", signature = "String" }
    , { symbol = "readIdrisFile", signature = "FilePath -> IO (Either Text [IDecl])" }
    , { symbol = "compileIdrisFileViaShim", signature = "FilePath -> IO (Either Text Text)" }
    , { symbol = "dumpProgram", signature = "Program -> T.Text" }
    , { symbol = "analyzeProgram", signature = "Program -> [LambdaSite]" }
    , { symbol = "likelyLocal", signature = "Name -> Bool" }
    , { symbol = "heuristicCountUses", signature = "Name -> Expr -> Int" }
    , { symbol = "freeVars", signature = "Expr -> Set Name" }
    , { symbol = "branchFreeVars", signature = "Branch -> Set Name" }
    , { symbol = "patternNames", signature = "Pattern -> [Name]" }
    , { symbol = "renderReport", signature = "[LambdaSite] -> Text" }
    , { symbol = "mvpCoreShard", signature = "Text -> FeatureShard '[] ef_core" }
    , { symbol = "idIdentityRoute", signature = "Penelope '[] ef ins outs" }
    , { symbol = "haskellMVPPlugin", signature = "LanguagePlugin MVPAst '[] ef_core" }
    , { symbol = "c99MVPPlugin", signature = "LanguagePlugin MVPAst '[] ef_core" }
    , { symbol = "rustMirMVPPlugin", signature = "LanguagePlugin MVPAst '[] ef_core" }
    , { symbol = "schemaVersion", signature = "Text" }
    , { symbol = "name", signature = "Text -> Name" }
    , { symbol = "localName", signature = "Text -> QName" }
    , { symbol = "qualName", signature = "Text -> Text -> QName" }
    , { symbol = "organIR", signature = "SourceLang -> Text -> Text -> [Definition] -> OrganIR" }
    , { symbol = "simpleOrganIR", signature = "SourceLang -> Text -> Text -> FilePath -> [Definition] -> OrganIR" }
    , { symbol = "organIRWithExports", signature = "SourceLang -> Text -> Text -> FilePath -> [Text] -> [Definition] -> OrganIR" }
    , { symbol = "organIRWithImports", signature = "SourceLang -> Text -> Text -> FilePath -> [Text] -> [QName] -> [Definition] -> OrganIR" }
    , { symbol = "organIRFull", signature = "Metadata -> Module -> OrganIR" }
    , { symbol = "funDef", signature = "Text -> Ty -> Expr -> Int -> Definition" }
    , { symbol = "valDef", signature = "Text -> Ty -> Expr -> Definition" }
    , { symbol = "conDef", signature = "Text -> Ty -> Int -> Definition" }
    , { symbol = "tAny", signature = "Ty" }
    , { symbol = "tCon", signature = "Text -> Ty" }
    , { symbol = "tApp", signature = "Text -> [Ty] -> Ty" }
    , { symbol = "tFn", signature = "[Ty] -> Ty -> Ty" }
    , { symbol = "tVar", signature = "Text -> Ty" }
    , { symbol = "pureEffect", signature = "EffectRow" }
    , { symbol = "eVar", signature = "Text -> Expr" }
    , { symbol = "eInt", signature = "Integer -> Expr" }
    , { symbol = "eFloat", signature = "Double -> Expr" }
    , { symbol = "eString", signature = "Text -> Expr" }
    , { symbol = "eBool", signature = "Bool -> Expr" }
    , { symbol = "eApp", signature = "Expr -> [Expr] -> Expr" }
    , { symbol = "eApp1", signature = "Expr -> Expr -> Expr" }
    , { symbol = "eLam", signature = "[Text] -> Expr -> Expr" }
    , { symbol = "eLet1", signature = "Text -> Expr -> Expr -> Expr" }
    , { symbol = "pWild", signature = "Pat" }
    , { symbol = "pVar", signature = "Text -> Pat" }
    , { symbol = "pCon", signature = "Text -> [Text] -> Pat" }
    , { symbol = "branch", signature = "Text -> [Text] -> Expr -> Branch" }
    , { symbol = "eIf", signature = "Expr -> Expr -> Expr -> Expr" }
    , { symbol = "eSeq", signature = "[Expr] -> Expr" }
    , { symbol = "eAnd", signature = "[Expr] -> Expr" }
    , { symbol = "eOr", signature = "[Expr] -> Expr" }
    , { symbol = "funDefNA", signature = "Text -> Int -> Expr -> Definition" }
    , { symbol = "eGuarded", signature = "Expr -> Expr -> Expr -> Expr" }
    , { symbol = "eMatch", signature = "Expr -> [(Pat, Expr)] -> Expr" }
    , { symbol = "valDefSimple", signature = "Text -> Expr -> Definition" }
    , { symbol = "eLet", signature = "[(Text, Expr)] -> Expr -> Expr" }
    , { symbol = "eNil", signature = "Expr" }
    , { symbol = "renderOrganIR", signature = "OrganIR -> Text" }
    , { symbol = "renderMetadata", signature = "Metadata -> Text" }
    , { symbol = "renderModule", signature = "Module -> Text" }
    , { symbol = "renderDef", signature = "Definition -> Text" }
    , { symbol = "renderQName", signature = "QName -> Text" }
    , { symbol = "renderName", signature = "Name -> Text" }
    , { symbol = "renderTy", signature = "Ty -> Text" }
    , { symbol = "renderFnArg", signature = "FnArg -> Text" }
    , { symbol = "renderTyVar", signature = "TyVar -> Text" }
    , { symbol = "renderEffectRow", signature = "EffectRow -> Text" }
    , { symbol = "renderExpr", signature = "Expr -> Text" }
    , { symbol = "renderLit", signature = "Lit -> Text" }
    , { symbol = "renderLamParam", signature = "LamParam -> Text" }
    , { symbol = "renderLetBind", signature = "LetBind -> Text" }
    , { symbol = "renderBranch", signature = "Branch -> Text" }
    , { symbol = "renderPat", signature = "Pat -> Text" }
    , { symbol = "renderPatBinder", signature = "PatBinder -> Text" }
    , { symbol = "renderDataType", signature = "DataType -> Text" }
    , { symbol = "renderCtor", signature = "Constructor -> Text" }
    , { symbol = "renderEffectDecl", signature = "EffectDecl -> Text" }
    , { symbol = "renderOp", signature = "Operation -> Text" }
    , { symbol = "obj", signature = "[(Text, Text)] -> Text" }
    , { symbol = "arr", signature = "[Text] -> Text" }
    , { symbol = "str", signature = "Text -> Text" }
    , { symbol = "int", signature = "Integer -> Text" }
    , { symbol = "opt", signature = "Text -> (a -> Text) -> Maybe a -> [(Text, Text)]" }
    , { symbol = "optArr", signature = "Text -> (a -> Text) -> [a] -> [(Text, Text)]" }
    , { symbol = "sourceLangText", signature = "SourceLang -> Text" }
    , { symbol = "sortText", signature = "Sort -> Text" }
    , { symbol = "visText", signature = "Visibility -> Text" }
    , { symbol = "mulText", signature = "Multiplicity -> Text" }
    , { symbol = "validateOrganIR", signature = "OrganIR -> [Warning]" }
    , { symbol = "validateMetadata", signature = "Metadata -> [Warning]" }
    , { symbol = "validateModule", signature = "Module -> [Warning]" }
    , { symbol = "validateImports", signature = "Module -> [Warning]" }
    , { symbol = "validateDef", signature = "Text -> Definition -> [Warning]" }
    , { symbol = "validateExpr", signature = "Text -> Expr -> [Warning]" }
    , { symbol = "validateTy", signature = "Text -> Ty -> [Warning]" }
    , { symbol = "validateDataType", signature = "Text -> DataType -> [Warning]" }
    , { symbol = "validateConstructor", signature = "Text -> Constructor -> [Warning]" }
    , { symbol = "ppOrganIR", signature = "OrganIR -> Text" }
    , { symbol = "ppSourceLang", signature = "SourceLang -> Text" }
    , { symbol = "ppDefinition", signature = "Definition -> Text" }
    , { symbol = "ppTy", signature = "Ty -> Text" }
    , { symbol = "ppFnArg", signature = "FnArg -> Text" }
    , { symbol = "ppEffectRow", signature = "EffectRow -> Text" }
    , { symbol = "ppTyVar", signature = "TyVar -> Text" }
    , { symbol = "ppExprIndent", signature = "Int -> Expr -> Text" }
    , { symbol = "ppExpr", signature = "Expr -> Text" }
    , { symbol = "ppLit", signature = "Lit -> Text" }
    , { symbol = "ppPat", signature = "Pat -> Text" }
    , { symbol = "ppLamParam", signature = "LamParam -> Text" }
    , { symbol = "ppQName", signature = "QName -> Text" }
    , { symbol = "ppDataType", signature = "DataType -> Text" }
    , { symbol = "ppConstructor", signature = "Constructor -> Text" }
    , { symbol = "ppEffectDecl", signature = "EffectDecl -> Text" }
    , { symbol = "ppOperation", signature = "Operation -> Text" }
    , { symbol = "ppName", signature = "Name -> Text" }
    , { symbol = "schemaCheck", signature = "JVal -> [SchemaError]" }
    , { symbol = "checkTopLevel", signature = "[(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkMetadata", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkSourceLang", signature = "Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkModule", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkDefinitions", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkDefinition", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkQName", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkName", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkType", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkForallType", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkFnType", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkFnArgs", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkFnArg", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkAppType", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkConType", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkSynType", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkTypeArray", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkTyVarsArray", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkTyVar", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkEffectRow", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkQNameArray", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkExpr", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkExprTag", signature = "Text -> Text -> JVal -> [SchemaError]" }
    , { symbol = "checkLit", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkECon", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkEApp", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkELam", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkLamParams", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkLamParam", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkELet", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkLetBinds", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkLetBind", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkECase", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkBranches", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkBranch", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkPattern", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkETypeApp", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkETypeLam", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkEPerform", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkEHandle", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkExprArray", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkOptExprArray", signature = "Text -> Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkDataType", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkConstructors", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkConstructor", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkEffectDecl", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "checkOperations", signature = "Text -> Maybe JVal -> [SchemaError]" }
    , { symbol = "checkOperation", signature = "Text -> JVal -> [SchemaError]" }
    , { symbol = "requiredFields", signature = "Text -> [Text] -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "extraFields", signature = "Text -> [Text] -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkStr", signature = "Text -> Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkOptStr", signature = "Text -> Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkOptInt", signature = "Text -> Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkOptStrArray", signature = "Text -> Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkOptQNameArray", signature = "Text -> Text -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkEnum", signature = "Text -> Text -> [Text] -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkOptEnum", signature = "Text -> Text -> [Text] -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "checkOptArray", signature = "Text -> Text -> (Int -> JVal -> [SchemaError]) -> [(Text, JVal)] -> [SchemaError]" }
    , { symbol = "arrElem", signature = "Text -> Text -> Int -> Text" }
    , { symbol = "arrElem'", signature = "Text -> Int -> Text" }
    , { symbol = "quote", signature = "Text -> Text" }
    , { symbol = "runPasses", signature = "[Pass] -> OrganIR -> OrganIR" }
    , { symbol = "mapDefs", signature = "(Definition -> Definition) -> Pass" }
    , { symbol = "mapExprs", signature = "(Expr -> Expr) -> Definition -> Definition" }
    , { symbol = "foldExpr", signature = "(Expr -> a -> a) -> a -> Expr -> a" }
    , { symbol = "constantFold", signature = "Pass" }
    , { symbol = "deadDefElim", signature = "Pass" }
    , { symbol = "inlineTrivial", signature = "Pass" }
    , { symbol = "simplifyCase", signature = "Pass" }
    , { symbol = "etaReduce", signature = "Pass" }
    , { symbol = "nameKey", signature = "Name -> NameKey" }
    , { symbol = "defKey", signature = "Definition -> NameKey" }
    , { symbol = "freeVars", signature = "Expr -> Set NameKey" }
    , { symbol = "refsInExpr", signature = "Expr -> Set NameKey" }
    , { symbol = "transitiveClosure", signature = "Set NameKey -> (NameKey -> Set NameKey) -> Set NameKey" }
    , { symbol = "substTrivials", signature = "[(Name, Expr)] -> Expr -> Expr" }
    , { symbol = "runBuilder", signature = "Builder -> ByteString" }
    , { symbol = "emit", signature = "ByteString -> Builder" }
    , { symbol = "emitByte", signature = "Word8 -> Builder" }
    , { symbol = "emitVarUInt", signature = "Integer -> Builder" }
    , { symbol = "emitVarInt", signature = "Integer -> Builder" }
    , { symbol = "emitText", signature = "Text -> Builder" }
    , { symbol = "emitBool", signature = "Bool -> Builder" }
    , { symbol = "emitList", signature = "(a -> Builder) -> [a] -> Builder" }
    , { symbol = "emitMaybe", signature = "(a -> Builder) -> Maybe a -> Builder" }
    , { symbol = "emitDouble", signature = "Double -> Builder" }
    , { symbol = "encodeOrganIR", signature = "OrganIR -> ByteString" }
    , { symbol = "putOrganIR", signature = "OrganIR -> Builder" }
    , { symbol = "putMetadata", signature = "Metadata -> Builder" }
    , { symbol = "putSourceLang", signature = "SourceLang -> Builder" }
    , { symbol = "sourceLangTag", signature = "SourceLang -> Word8" }
    , { symbol = "putModule", signature = "Module -> Builder" }
    , { symbol = "putName", signature = "Name -> Builder" }
    , { symbol = "putQName", signature = "QName -> Builder" }
    , { symbol = "putDefinition", signature = "Definition -> Builder" }
    , { symbol = "putSort", signature = "Sort -> Builder" }
    , { symbol = "putVisibility", signature = "Visibility -> Builder" }
    , { symbol = "putTy", signature = "Ty -> Builder" }
    , { symbol = "putFnArg", signature = "FnArg -> Builder" }
    , { symbol = "putMultiplicity", signature = "Multiplicity -> Builder" }
    , { symbol = "putTyVar", signature = "TyVar -> Builder" }
    , { symbol = "putEffectRow", signature = "EffectRow -> Builder" }
    , { symbol = "putExpr", signature = "Expr -> Builder" }
    , { symbol = "putLit", signature = "Lit -> Builder" }
    , { symbol = "putLamParam", signature = "LamParam -> Builder" }
    , { symbol = "putLetBind", signature = "LetBind -> Builder" }
    , { symbol = "putBranch", signature = "Branch -> Builder" }
    , { symbol = "putPat", signature = "Pat -> Builder" }
    , { symbol = "putPatBinder", signature = "PatBinder -> Builder" }
    , { symbol = "putDataType", signature = "DataType -> Builder" }
    , { symbol = "putConstructor", signature = "Constructor -> Builder" }
    , { symbol = "putEffectDecl", signature = "EffectDecl -> Builder" }
    , { symbol = "putOperation", signature = "Operation -> Builder" }
    , { symbol = "decodeOrganIR", signature = "ByteString -> Either String OrganIR" }
    , { symbol = "getByte", signature = "Get Word8" }
    , { symbol = "getVarUInt", signature = "Get Integer" }
    , { symbol = "getVarInt", signature = "Get Integer" }
    , { symbol = "getText", signature = "Get Text" }
    , { symbol = "getBool", signature = "Get Bool" }
    , { symbol = "getDouble", signature = "Get Double" }
    , { symbol = "getList", signature = "Get a -> Get [a]" }
    , { symbol = "getMaybe", signature = "Get a -> Get (Maybe a)" }
    , { symbol = "getOrganIR", signature = "Get OrganIR" }
    , { symbol = "getMetadata", signature = "Get Metadata" }
    , { symbol = "getSourceLang", signature = "Get SourceLang" }
    , { symbol = "getModule", signature = "Get Module" }
    , { symbol = "getName", signature = "Get Name" }
    , { symbol = "getQName", signature = "Get QName" }
    , { symbol = "getDefinition", signature = "Get Definition" }
    , { symbol = "getSort", signature = "Get Sort" }
    , { symbol = "getVisibility", signature = "Get Visibility" }
    , { symbol = "getTy", signature = "Get Ty" }
    , { symbol = "getFnArg", signature = "Get FnArg" }
    , { symbol = "getMultiplicity", signature = "Get Multiplicity" }
    , { symbol = "getTyVar", signature = "Get TyVar" }
    , { symbol = "getEffectRow", signature = "Get EffectRow" }
    , { symbol = "getExpr", signature = "Get Expr" }
    , { symbol = "getLit", signature = "Get Lit" }
    , { symbol = "getLamParam", signature = "Get LamParam" }
    , { symbol = "getLetBind", signature = "Get LetBind" }
    , { symbol = "getBranch", signature = "Get Branch" }
    , { symbol = "getPat", signature = "Get Pat" }
    , { symbol = "getPatBinder", signature = "Get PatBinder" }
    , { symbol = "getDataType", signature = "Get DataType" }
    , { symbol = "getConstructor", signature = "Get Constructor" }
    , { symbol = "getEffectDecl", signature = "Get EffectDecl" }
    , { symbol = "getOperation", signature = "Get Operation" }
    , { symbol = "maxNestingDepth", signature = "Int" }
    , { symbol = "parseJSON", signature = "Text -> Either Text JVal" }
    , { symbol = "pValueD", signature = "Int -> P JVal" }
    , { symbol = "pValue", signature = "P JVal" }
    , { symbol = "pLit", signature = "Text -> JVal -> P JVal" }
    , { symbol = "pString", signature = "P Text" }
    , { symbol = "pStrBody", signature = "Text -> P Text" }
    , { symbol = "hexToInt", signature = "Text -> Int" }
    , { symbol = "pNumber", signature = "P JVal" }
    , { symbol = "pObject", signature = "P [(Text, JVal)]" }
    , { symbol = "pObjectD", signature = "Int -> P [(Text, JVal)]" }
    , { symbol = "pObjEntries", signature = "P [(Text, JVal)]" }
    , { symbol = "pObjEntriesD", signature = "Int -> P [(Text, JVal)]" }
    , { symbol = "pColon", signature = "Text -> Either Text Text" }
    , { symbol = "pArray", signature = "P [JVal]" }
    , { symbol = "pArrayD", signature = "Int -> P [JVal]" }
    , { symbol = "pArrElems", signature = "P [JVal]" }
    , { symbol = "pArrElemsD", signature = "Int -> P [JVal]" }
    , { symbol = "skip", signature = "Text -> Text" }
    , { symbol = "field", signature = "Text -> [(Text, JVal)] -> Either Text JVal" }
    , { symbol = "optField", signature = "Text -> [(Text, JVal)] -> Maybe JVal" }
    , { symbol = "asObj", signature = "D [(Text, JVal)]" }
    , { symbol = "asArr", signature = "D [JVal]" }
    , { symbol = "asStr", signature = "D Text" }
    , { symbol = "asInteger", signature = "D Integer" }
    , { symbol = "asInt", signature = "D Int" }
    , { symbol = "asBool", signature = "D Bool" }
    , { symbol = "parseOrganIR", signature = "Text -> Either Text OrganIR" }
    , { symbol = "decodeOrganIR", signature = "D OrganIR" }
    , { symbol = "decodeMetadata", signature = "D Metadata" }
    , { symbol = "decodeModule", signature = "D Module" }
    , { symbol = "decodeDef", signature = "D Definition" }
    , { symbol = "decodeQName", signature = "D QName" }
    , { symbol = "decodeName", signature = "D Name" }
    , { symbol = "decodeTy", signature = "D Ty" }
    , { symbol = "decodeTyTagged", signature = "[(Text, JVal)] -> Text -> JVal -> Either Text Ty" }
    , { symbol = "decodeFnArg", signature = "D FnArg" }
    , { symbol = "decodeTyVar", signature = "D TyVar" }
    , { symbol = "decodeEffectRow", signature = "D EffectRow" }
    , { symbol = "decodeExpr", signature = "D Expr" }
    , { symbol = "decodeExprTagged", signature = "[(Text, JVal)] -> Text -> JVal -> Either Text Expr" }
    , { symbol = "decodeLit", signature = "D Lit" }
    , { symbol = "decodeLitTagged", signature = "[(Text, JVal)] -> Text -> JVal -> Either Text Lit" }
    , { symbol = "decodeLamParam", signature = "D LamParam" }
    , { symbol = "decodeLetBind", signature = "D LetBind" }
    , { symbol = "decodeBranch", signature = "D Branch" }
    , { symbol = "decodePat", signature = "D Pat" }
    , { symbol = "decodePatTagged", signature = "[(Text, JVal)] -> Text -> JVal -> Either Text Pat" }
    , { symbol = "decodePatBinder", signature = "D PatBinder" }
    , { symbol = "decodeDataType", signature = "D DataType" }
    , { symbol = "decodeConstructor", signature = "D Constructor" }
    , { symbol = "decodeEffectDecl", signature = "D EffectDecl" }
    , { symbol = "decodeOperation", signature = "D Operation" }
    , { symbol = "decodeSourceLang", signature = "Text -> Either Text SourceLang" }
    , { symbol = "decodeSort", signature = "Text -> Either Text Sort" }
    , { symbol = "decodeVisibility", signature = "Text -> Either Text Visibility" }
    , { symbol = "decodeMultiplicity", signature = "Text -> Either Text Multiplicity" }
    , { symbol = "factorial", signature = "Int -> Int" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "compute", signature = "Int -> Int" }
    , { symbol = "factorial", signature = "Int -> Int" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "double", signature = "Int -> Int" }
    , { symbol = "isZero", signature = "Int -> Bool" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "sumTo", signature = "Int -> Int" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "fib", signature = "Int -> Int" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "classify", signature = "Int -> Int" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "krunPath", signature = "FilePath" }
    , { symbol = "kDefinition", signature = "FilePath" }
    , { symbol = "kOracleTests", signature = "TestTree" }
    , { symbol = "prop_arithKrunEqualsMlir", signature = "Property" }
    , { symbol = "prop_letCaseKrunEqualsMlir", signature = "Property" }
    , { symbol = "prop_effectKrunEqualsMlir", signature = "Property" }
    , { symbol = "prop_krunEqualsMlir", signature = "Property" }
    , { symbol = "oracleProperty", signature = "Expr -> Property" }
    , { symbol = "varNames", signature = "[Text]" }
    , { symbol = "mkN", signature = "Text -> Name" }
    , { symbol = "mkQ", signature = "Text -> Text -> QName" }
    , { symbol = "intT", signature = "Type" }
    , { symbol = "genArithExpr", signature = "Gen Expr" }
    , { symbol = "genArith", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genLitSmall", signature = "Gen Expr" }
    , { symbol = "genBinOp", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genNegate", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genLetCaseExpr", signature = "Gen Expr" }
    , { symbol = "genLetCase", signature = "[Text] -> Int -> Int -> Gen Expr" }
    , { symbol = "genLeaf", signature = "[Text] -> Gen Expr" }
    , { symbol = "genLetExpr", signature = "[Text] -> Int -> Int -> Gen Expr" }
    , { symbol = "genCaseExpr", signature = "[Text] -> Int -> Int -> Gen Expr" }
    , { symbol = "genBinOpWithEnv", signature = "[Text] -> Int -> Int -> Gen Expr" }
    , { symbol = "genEffectExpr", signature = "Gen Expr" }
    , { symbol = "genEffect", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genHandleAbort", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genAbortBody", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genHandleResume", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genResumeBody", signature = "Int -> Int -> Gen Expr" }
    , { symbol = "genExpr", signature = "Gen Expr" }
    , { symbol = "briefExpr", signature = "Expr -> String" }
    , { symbol = "exprToK", signature = "Expr -> Text" }
    , { symbol = "nameToK", signature = "Name -> Text" }
    , { symbol = "qnameToK", signature = "QName -> Text" }
    , { symbol = "typeToK", signature = "Type -> Text" }
    , { symbol = "paramToK", signature = "(Name, Type) -> Text" }
    , { symbol = "exprListToK", signature = "[Expr] -> Text" }
    , { symbol = "bgListToK", signature = "[BindGroup] -> Text" }
    , { symbol = "bgToK", signature = "BindGroup -> Text" }
    , { symbol = "bindToK", signature = "Bind -> Text" }
    , { symbol = "defSortToK", signature = "DefSort -> Text" }
    , { symbol = "patternToK", signature = "Pattern -> Text" }
    , { symbol = "litToK", signature = "Lit -> Text" }
    , { symbol = "branchListToK", signature = "[Branch] -> Text" }
    , { symbol = "branchToK", signature = "Branch -> Text" }
    , { symbol = "guardToK", signature = "Maybe Expr -> Text" }
    , { symbol = "effRowToK", signature = "EffectRow -> Text" }
    , { symbol = "runKOracle", signature = "Expr -> IO (Either Text Integer)" }
    , { symbol = "parseKrunOutput", signature = "Text -> Either Text Integer" }
    , { symbol = "extractIntFromKOutput", signature = "Text -> Maybe Integer" }
    , { symbol = "runMlirPipeline", signature = "Expr -> IO (Either Text Integer)" }
    , { symbol = "krunPath", signature = "FilePath" }
    , { symbol = "kDefinition", signature = "FilePath" }
    , { symbol = "testDir", signature = "FilePath" }
    , { symbol = "ghcPath", signature = "FilePath" }
    , { symbol = "bridgeBisimTests", signature = "TestTree" }
    , { symbol = "ghcBisimTests", signature = "TestTree" }
    , { symbol = "compileGhc", signature = "FilePath -> IO Program" }
    , { symbol = "kokaBisimTests", signature = "TestTree" }
    , { symbol = "rustBisimTests", signature = "TestTree" }
    , { symbol = "compileRust", signature = "FilePath -> IO Program" }
    , { symbol = "mercuryBisimTests", signature = "TestTree" }
    , { symbol = "pythonBisimTests", signature = "TestTree" }
    , { symbol = "containsEHandle", signature = "Expr -> Bool" }
    , { symbol = "containsEPerform", signature = "Expr -> Bool" }
    , { symbol = "compilePython", signature = "FilePath -> IO Program" }
    , { symbol = "goBisimTests", signature = "TestTree" }
    , { symbol = "compileGo", signature = "FilePath -> IO Program" }
    , { symbol = "futharkBisimTests", signature = "TestTree" }
    , { symbol = "compileFuthark", signature = "FilePath -> IO Program" }
    , { symbol = "schemeBisimTests", signature = "TestTree" }
    , { symbol = "compileScheme", signature = "FilePath -> IO Program" }
    , { symbol = "cleanExpr", signature = "Expr -> Expr" }
    , { symbol = "builtinOps", signature = "Set.Set Text" }
    , { symbol = "normalizeBuiltins", signature = "Expr -> Expr" }
    , { symbol = "reorderBranches", signature = "Expr -> Expr" }
    , { symbol = "simplifyGhcBoxing", signature = "Expr -> Expr" }
    , { symbol = "stripDelay", signature = "Expr -> Expr" }
    , { symbol = "sdBind", signature = "Bind -> Bind" }
    , { symbol = "sdBranch", signature = "Branch -> Branch" }
    , { symbol = "stripPerceus", signature = "Expr -> Expr" }
    , { symbol = "isPerceusBinding", signature = "Bind -> Bool" }
    , { symbol = "spBind", signature = "Bind -> Bind" }
    , { symbol = "spBranch", signature = "Branch -> Branch" }
    , { symbol = "findDef", signature = "Text -> Program -> Maybe Def" }
    , { symbol = "findDefOrFail", signature = "Text -> Program -> IO Def" }
    , { symbol = "applyDef", signature = "Def -> [Expr] -> Expr" }
    , { symbol = "wrapRecDef", signature = "Name -> Def -> Expr -> Expr" }
    , { symbol = "anyType", signature = "Type" }
    , { symbol = "rewriteSelfCalls", signature = "Name -> Name -> Expr -> Expr" }
    , { symbol = "runKOracleExpr", signature = "Expr -> IO (Either Text Integer)" }
    , { symbol = "runKOracleExprOrFail", signature = "Expr -> IO Integer" }
    , { symbol = "assertKrunEquals", signature = "Expr -> Integer -> IO ()" }
    , { symbol = "parseKrunOutput", signature = "Text -> Either Text Integer" }
    , { symbol = "extractIntFromKOutput", signature = "Text -> Maybe Integer" }
    , { symbol = "runGhcNative", signature = "FilePath -> String -> String -> IO Integer" }
    , { symbol = "runRustNative", signature = "FilePath -> String -> IO Integer" }
    , { symbol = "isPrefixOfAny", signature = "[String] -> String -> Bool" }
    , { symbol = "mkName", signature = "Text -> Name" }
    , { symbol = "mkQName", signature = "Text -> Text -> QName" }
    , { symbol = "intType", signature = "Type" }
    , { symbol = "anyType", signature = "Type" }
    , { symbol = "unitType", signature = "Type" }
    , { symbol = "mkProgram", signature = "Text -> Text -> [Def] -> Program" }
    , { symbol = "mkFunDef", signature = "Text -> Text -> Expr -> Type -> Def" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "coreIRTests", signature = "TestTree" }
    , { symbol = "perceusTests", signature = "TestTree" }
    , { symbol = "containsDrop", signature = "Name -> Expr -> Bool" }
    , { symbol = "evidenceTests", signature = "TestTree" }
    , { symbol = "containsHandle", signature = "Expr -> Bool" }
    , { symbol = "containsPerform", signature = "Expr -> Bool" }
    , { symbol = "isEApp", signature = "Expr -> Bool" }
    , { symbol = "isELet", signature = "Expr -> Bool" }
    , { symbol = "containsVarNamed", signature = "Text -> Expr -> Bool" }
    , { symbol = "effectOptTests", signature = "TestTree" }
    , { symbol = "linkerTests", signature = "TestTree" }
    , { symbol = "isDuplicateError", signature = "LinkError -> Bool" }
    , { symbol = "isNoMainError", signature = "LinkError -> Bool" }
    , { symbol = "isMultipleMainError", signature = "LinkError -> Bool" }
    , { symbol = "mlirEmitTests", signature = "TestTree" }
    , { symbol = "deriveSelectorsTests", signature = "TestTree" }
    , { symbol = "flattenPatternsTests", signature = "TestTree" }
    , { symbol = "mkDataDeclSimple", signature = "Text -> [Text] -> DataDecl" }
    , { symbol = "conTagsTests", signature = "TestTree" }
    , { symbol = "shapeHlds", signature = "MercuryHLDS" }
    , { symbol = "hasECon", signature = "Expr -> Bool" }
    , { symbol = "hasPatCon", signature = "Expr -> Bool" }
    , { symbol = "mercuryAdtTests", signature = "TestTree" }
    , { symbol = "wasmEmitTests", signature = "TestTree" }
    , { symbol = "demoFactorialWithMain", signature = "Program" }
    , { symbol = "myMaybeProgram", signature = "Program" }
    , { symbol = "cpsConvertTests", signature = "TestTree" }
    , { symbol = "typeCheckTests", signature = "TestTree" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "defaultFlags", signature = "Flags" }
    , { symbol = "parseArgs", signature = "[String] -> Command" }
    , { symbol = "isFlag", signature = "String -> Bool" }
    , { symbol = "removeArgValues", signature = "[String] -> [String]" }
    , { symbol = "parseFlags", signature = "[String] -> Flags" }
    , { symbol = "compileFile", signature = "Bool -> Bool -> FilePath -> IO (Either (FilePath, Text) [Program])" }
    , { symbol = "compileHaskell", signature = "Bool -> FilePath -> IO (Either Text Program)" }
    , { symbol = "compileHaskellMulti", signature = "Bool -> FilePath -> IO (Either Text [Program])" }
    , { symbol = "compileMercury", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileKoka", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileRust", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compilePython", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileGo", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileFuthark", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileScheme", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileSwift", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileOCaml", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileErlang", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileFSharp", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "compileIdris", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "countPerceusOps", signature = "Program -> (Int, Int, Int)" }
    , { symbol = "swiftCrossCheck", signature = "FilePath -> IO ()" }
    , { symbol = "compileOrganIR", signature = "FilePath -> IO (Either Text Program)" }
    , { symbol = "handleOutput", signature = "Program -> Flags -> IO ()" }
    , { symbol = "partitionResults", signature = "[Either (FilePath, Text) [Program]] -> ([(FilePath, Text)], [[Program]])" }
    , { symbol = "formatLinkError", signature = "LinkError -> Text" }
    , { symbol = "printHelp", signature = "IO ()" }
    , { symbol = "demoFactorialWithMain", signature = "Program" }
    , { symbol = "demoEffectProgram", signature = "Program" }
    , { symbol = "demoAbortProgram", signature = "Program" }
    , { symbol = "demoMultiEffectProgram", signature = "Program" }
    , { symbol = "demoHaskellProgram", signature = "Program" }
    , { symbol = "demoMercuryProgram", signature = "Program" }
    , { symbol = "demoKokaProgram", signature = "Program" }
    , { symbol = "demoRustProgram", signature = "Program" }
    , { symbol = "prettyProgram", signature = "Program -> Text" }
    , { symbol = "prettyDef", signature = "Def -> [Text]" }
    , { symbol = "prettyType", signature = "Type -> Text" }
    , { symbol = "prettyMult", signature = "Multiplicity -> Text" }
    , { symbol = "prettyEff", signature = "EffectRow -> Text" }
    , { symbol = "prettyExpr", signature = "Expr -> Text" }
    , { symbol = "ppQName", signature = "QName -> Text" }
    , { symbol = "score", signature = "Int -> Int -> Int" }
    , { symbol = "bonus", signature = "Int -> Int" }
    , { symbol = "score", signature = "Int -> Int -> Int" }
    , { symbol = "bonus", signature = "Int -> Int" }
    , { symbol = "fibonacci", signature = "Int -> Int" }
    , { symbol = "double", signature = "Int -> Int" }
    , { symbol = "triple", signature = "Int -> Int" }
    , { symbol = "sumList", signature = "[Int] -> Int" }
    , { symbol = "analyze", signature = "Int -> Int" }
    , { symbol = "mkRange", signature = "Int -> Int -> [Int]" }
    , { symbol = "sumEvenSquares", signature = "[Int] -> Int" }
    , { symbol = "buildRange", signature = "Int -> [Int]" }
    , { symbol = "compute", signature = "Int -> Int" }
    , { symbol = "squaredGcd", signature = "Int -> Int -> Int" }
    , { symbol = "treeSum", signature = "Tree -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "orZero", signature = "MyMaybe -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "sumList", signature = "IntList -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "sumBoxTree", signature = "BoxTree -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "main", signature = "Tree" }
    , { symbol = "buildList", signature = "Int -> IntList" }
    , { symbol = "sumList", signature = "IntList -> Int" }
    , { symbol = "sumMany", signature = "Int -> Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "myMap", signature = "(Int -> Int) -> IntList -> IntList" }
    , { symbol = "myFoldl", signature = "(Int -> Int -> Int) -> Int -> IntList -> Int" }
    , { symbol = "double", signature = "Int -> Int" }
    , { symbol = "addN", signature = "Int -> Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "isEven", signature = "Int -> Int" }
    , { symbol = "isOdd", signature = "Int -> Int" }
    , { symbol = "countEven", signature = "Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "apply", signature = "(Int -> Int) -> Int -> Int" }
    , { symbol = "addN", signature = "Int -> Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "fromMaybe", signature = "Int -> MyMaybe -> Int" }
    , { symbol = "fstPair", signature = "Pair -> Int" }
    , { symbol = "sndPair", signature = "Pair -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "getPairSum", signature = "Pair -> Int" }
    , { symbol = "getTripleSum", signature = "Triple -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "sumList", signature = "[Int] -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "fromMaybe", signature = "Int -> Maybe Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "abs'", signature = "Int -> Int" }
    , { symbol = "sign", signature = "Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "swap", signature = "(Int, Int) -> (Int, Int)" }
    , { symbol = "addPair", signature = "(Int, Int) -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "myMap", signature = "(Int -> Int) -> [Int] -> [Int]" }
    , { symbol = "myFilter", signature = "(Int -> Bool) -> [Int] -> [Int]" }
    , { symbol = "myFoldr", signature = "(Int -> Int -> Int) -> Int -> [Int] -> Int" }
    , { symbol = "double", signature = "Int -> Int" }
    , { symbol = "isPositive", signature = "Int -> Bool" }
    , { symbol = "add", signature = "Int -> Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "double", signature = "Int -> Int" }
    , { symbol = "triple", signature = "Int -> Int" }
    , { symbol = "sumList", signature = "[Int] -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "getSnd", signature = "Pair -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "intOfColor", signature = "Color -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "unwrapOr", signature = "Int -> Maybe2 -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "classify", signature = "[MaybeI] -> Maybe MaybeI -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "classify", signature = "[MaybeI] -> Maybe MaybeI -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "classify", signature = "Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "f2", signature = "T1 -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "main", signature = "IO ()" }
    , { symbol = "fib", signature = "Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "tak", signature = "Int -> Int -> Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "ack", signature = "Int -> Int -> Int" }
    , { symbol = "main", signature = "Int" }
    , { symbol = "prettyProg", signature = "String -> Prog -> String" }
    , { symbol = "prettyData", signature = "DataDecl -> String" }
    , { symbol = "prettyFnSig", signature = "FnDef -> String" }
    , { symbol = "prettyTy", signature = "ArgTy -> String" }
    , { symbol = "prettyFnDef", signature = "FnDef -> String" }
    , { symbol = "prettyExpr", signature = "Expr -> String" }
    , { symbol = "prettyBranch", signature = "Branch -> String" }
    , { symbol = "prettyPat", signature = "Pat -> String" }
    , { symbol = "prettyMain", signature = "MainExpr -> String" }
    , { symbol = "genProg", signature = "QC.Gen Prog" }
    , { symbol = "genDataDecl", signature = "Int -> QC.Gen DataDecl" }
    , { symbol = "genFnDef", signature = "[DataDecl] -> Int -> QC.Gen FnDef" }
    , { symbol = "genBody", signature = "[DataDecl] -> [(String, ArgTy)] -> QC.Gen Expr" }
    , { symbol = "genBranches", signature = "[(String, ArgTy)] -> [(String, Int)] -> QC.Gen [Branch]" }
    , { symbol = "genConBranch", signature = "[(String, ArgTy)] -> (String, Int) -> QC.Gen Branch" }
    , { symbol = "genIntExpr", signature = "[String] -> QC.Gen Expr" }
    , { symbol = "genMainExpr", signature = "[DataDecl] -> [FnDef] -> QC.Gen MainExpr" }
    , { symbol = "genArgValue", signature = "[DataDecl] -> ArgTy -> QC.Gen Expr" }
    , { symbol = "shrinkProg", signature = "Prog -> [Prog]" }
    , { symbol = "validRefs", signature = "[DataDecl] -> [FnDef] -> MainExpr -> Bool" }
    , { symbol = "validMain", signature = "[DataDecl] -> [FnDef] -> MainExpr -> Bool" }
    , { symbol = "rebindFns", signature = "[DataDecl] -> [FnDef] -> [FnDef]" }
    , { symbol = "rebindMain", signature = "[DataDecl] -> [FnDef] -> MainExpr -> MainExpr" }
    , { symbol = "shrinkList", signature = "(a -> [a]) -> [a] -> [[a]]" }
    , { symbol = "shrinkOne", signature = "(a -> [a]) -> [a] -> [[a]]" }
    , { symbol = "shrinkFnBranches", signature = "FnDef -> [FnDef]" }
    , { symbol = "shrinkExpr", signature = "Expr -> [Expr]" }
    , { symbol = "shrinkMain", signature = "MainExpr -> [MainExpr]" }
    , { symbol = "parseMode", signature = "String -> Maybe Mode" }
    , { symbol = "discoverTools", signature = "IO Tools" }
    , { symbol = "hostMlirDirect", signature = "CompileStep" }
    , { symbol = "emitOrgan", signature = "Tools -> FilePath -> IO (Either String String)" }
    , { symbol = "stageEmit", signature = "FilePath -> Tools -> String -> FilePath -> IO (Either String BS.ByteString)" }
    , { symbol = "runDifferential", signature = "Mode -> Tools -> Prog -> IO DiffResult" }
    , { symbol = "compareRuntime", signature = "Tools -> FilePath -> FilePath -> FilePath -> IO DiffResult" }
    , { symbol = "compareTwoStages", signature = "Tools -> FilePath -> FilePath -> FilePath -> FilePath" }
    , { symbol = "emptyStats", signature = "Stats" }
    , { symbol = "diffFingerprint", signature = "BS.ByteString -> BS.ByteString -> String" }
    , { symbol = "isInfixOfStr", signature = "String -> String -> Bool" }
    , { symbol = "classifyFail", signature = "String -> String" }
    , { symbol = "normalizeLine", signature = "BS.ByteString -> BS.ByteString" }
    , { symbol = "runBatch", signature = "Mode -> Tools -> Int -> Int -> IO ()" }
    , { symbol = "progSize", signature = "Prog -> Int" }
    , { symbol = "exprSize", signature = "Expr -> Int" }
    , { symbol = "reportResults", signature = "Stats -> Map String (Int, Prog) -> IO ()" }
    , { symbol = "shrinkDivergent", signature = "Mode -> Tools -> Prog -> IO Prog" }
    , { symbol = "parseArgs", signature = "[String] -> Args" }
    , { symbol = "main", signature = "IO ()" }
    ]
}
