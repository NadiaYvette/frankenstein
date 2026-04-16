-- | Frankenstein Core -> MLIR Emitter
--
-- Emits textual MLIR, then invokes mlir-opt → mlir-translate → clang
-- to produce a native executable.

module Frankenstein.MlirEmit.Emitter
  ( emitProgram
  , emitProgramText
  , emitProgramWasm
  , emitProgramWithEffects
  , compileToExecutable
  , compileToWasm
  , CompileTarget(..)
  , EmitConfig(..)
  , defaultEmitConfig
  ) where

import Frankenstein.Core.Types
import Frankenstein.Core.ConTags (assignProgramTags, conKey)
import Frankenstein.MlirEmit.Dialects (MlirOp(..), renderOp)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.IORef
import System.Process (readProcessWithExitCode, readProcess)
import System.Exit (ExitCode(..))
import Control.Monad (forM_)
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition)
-- import Debug.Trace (trace)
import Text.Printf (printf)

data CompileTarget = TargetNative | TargetWasm32 deriving (Show, Eq)

data EmitConfig = EmitConfig
  { ecMlirOptPath       :: !FilePath
  , ecMlirTranslatePath :: !FilePath
  , ecClangPath         :: !FilePath
  , ecKokaRuntimePath   :: !(Maybe FilePath)
  , ecOptLevel          :: !Int
  , ecOutputPath        :: !FilePath
  , ecTarget            :: !CompileTarget
  } deriving (Show)

defaultEmitConfig :: EmitConfig
defaultEmitConfig = EmitConfig
  { ecMlirOptPath       = "mlir-opt"
  , ecMlirTranslatePath = "mlir-translate"
  , ecClangPath         = "clang"
  , ecKokaRuntimePath   = Nothing
  , ecOptLevel          = 0
  , ecOutputPath        = "a.out"
  , ecTarget            = TargetNative
  }

-- Emission state: tracks SSA counter and collected top-level functions
data EmitState = EmitState
  { esCounter       :: !Int
  , esLiftedFns     :: ![Text]  -- accumulated lifted lambda functions
  , esTypeEnv       :: !(Map Text Text)  -- SSA name -> MLIR type
  , esStringLits    :: ![(Text, Text)]   -- global name -> string content
  , esEvidenceScope :: !(Map Text Text)  -- effect name -> evidence SSA variable name
  , esAliases       :: !(Map Text Text)  -- name alias: let x = y → x maps to y
  , esEffectDialect :: !Bool              -- emit frankenstein.* dialect ops for effects
  , esTopFns        :: !(Set Text)        -- MLIR names of top-level func.func defs
  , esTopFnArity    :: !(Map Text Int)    -- arity of each top-level function
  , esPapWrappers   :: !(Set Text)        -- PAP wrapper names already emitted
  , esConTags       :: !(Map Text Int)    -- constructor name -> deterministic tag (see Core.ConTags.assignProgramTags)
  , esModulePrefix  :: !Text              -- module prefix for lifted lambda/thunk names (avoids cross-module symbol collisions)
  , esExternDecls   :: !(Map Text Int)         -- MLIR symbol name -> param count for func.func private declarations
  , esPromotedFns      :: !(Map Text Text)        -- nameToSsa key -> promoted function MLIR name (for let-bound lambdas)
  , esPromotedCaptures :: !(Map Text [Text])      -- promoted MLIR name -> extra capture SSA names to pass at call site
  }

type Emit a = State EmitState a

freshName :: Text -> Emit Text
freshName prefix = do
  s <- get
  let n = esCounter s
  put s { esCounter = n + 1 }
  pure $ prefix <> T.pack (show n)

addLiftedFn :: Text -> Emit ()
addLiftedFn fn = modify (\s -> s { esLiftedFns = fn : esLiftedFns s })

-- | Module-qualify a sanitized top-level function name using esModulePrefix.
-- E.g. with prefix "Frankenstein_Core_Perceus_", "anyType" -> "Frankenstein_Core_Perceus_anyType"
-- This prevents cross-module symbol collisions when linking multiple self-hosted .o files.
qualifyTop :: Text -> Emit Text
qualifyTop name = do
  pfx <- gets esModulePrefix
  pure (pfx <> name)

-- | Record an external function declaration (for unresolved imports).
-- Stores the actual MLIR symbol name (may be arity-mangled or module-qualified)
-- and the number of parameters for the func.func declaration.
addExternDecl :: Text -> Int -> Emit ()
addExternDecl mlirName nArgs =
  modify (\s -> s { esExternDecls = Map.insert mlirName nArgs (esExternDecls s) })

-- | Mangle an external name with its call-site arity: "map" with 2 args -> "map$2"
externMangled :: Text -> Int -> Text
externMangled name nArgs = name <> "$" <> T.pack (show nArgs)

-- | Recognize Koka stdlib builtins that should be emitted inline as
-- MLIR function definitions rather than declared as externals.
isKokaBuiltin :: Text -> Bool
isKokaBuiltin _ = False  -- currently unused; reserved for future Koka builtin inlining

-- | Emit inline MLIR function definitions for known Koka builtins.
emitKokaBuiltins :: [(Text, Int)] -> Text
emitKokaBuiltins _ = ""

-- | Emit a PAP (partial application) closure for an undersaturated call
-- to a top-level function. Allocates a heap closure via kk_alloc_con whose
-- field 0 is a wrapper fn pointer and fields 1..nSupplied are the supplied
-- args. A wrapper fn is synthesized (once per (fnName,nSupplied) pair) that
-- unpacks the captured args and calls the original fn with all args.
--
-- Returns (ops, resultSsaName) where resultSsaName holds the i64 closure ptr.
emitPapClosure :: Text -> Int -> [Text] -> Emit ([Text], Text)
emitPapClosure fnName arity suppliedArgs = do
  let nSupplied = length suppliedArgs
      nRemaining = arity - nSupplied
  wrapperName <- ensurePapWrapper fnName arity nSupplied
  -- Allocate closure: 1 slot for fptr + nSupplied slots for captured args
  let nFields = 1 + nSupplied
  tagName <- freshName "v"
  nfieldsName <- freshName "v"
  ptrName <- freshName "v"
  fptrAddrName <- freshName "v"
  fptrPtrName <- freshName "v"
  fptrName <- freshName "v"
  idxZeroName <- freshName "v"
  let wrapperParamTys = T.intercalate ", " (replicate (1 + nRemaining) "i64")
      wrapperFnTy = "(" <> wrapperParamTys <> ") -> i64"
  -- Cast func -> !llvm.ptr -> i64. The intermediate ptr cast survives
  -- --convert-func-to-llvm cleanly (the func.constant becomes
  -- llvm.mlir.addressof, and reconcile-unrealized-casts collapses the
  -- ptr->ptr cast). A direct func->i64 cast would otherwise leak through
  -- mlir-translate as an LLVM-incompatible type.
  let allocOps =
        [ "// PAP for @" <> fnName <> " (arity " <> T.pack (show arity)
          <> ", supplied " <> T.pack (show nSupplied) <> ")"
        , "%" <> tagName <> " = arith.constant 1129074515 : i64  // KK_CLOSURE_TAG 'CLOS'"
        , "%" <> nfieldsName <> " = arith.constant " <> T.pack (show nFields) <> " : i64"
        , "%" <> ptrName <> " = func.call @kk_alloc_con(%" <> tagName <> ", %" <> nfieldsName <> ") : (i64, i64) -> i64"
        , "%" <> fptrAddrName <> " = func.constant @" <> wrapperName <> " : " <> wrapperFnTy
        , "%" <> fptrPtrName <> " = builtin.unrealized_conversion_cast %" <> fptrAddrName <> " : " <> wrapperFnTy <> " to !llvm.ptr"
        , "%" <> fptrName <> " = llvm.ptrtoint %" <> fptrPtrName <> " : !llvm.ptr to i64"
        , "%" <> idxZeroName <> " = arith.constant 0 : i64"
        , "func.call @kk_set_field(%" <> ptrName <> ", %" <> idxZeroName <> ", %" <> fptrName <> ") : (i64, i64, i64) -> ()"
        ]
  -- Store each supplied arg into slots 1..nSupplied
  setOps <- fmap concat $ mapM (\(i, a) -> do
      idxN <- freshName "v"
      pure
        [ "%" <> idxN <> " = arith.constant " <> T.pack (show i) <> " : i64"
        , "func.call @kk_set_field(%" <> ptrName <> ", %" <> idxN <> ", %" <> a <> ") : (i64, i64, i64) -> ()"
        ]
    ) (zip [(1 :: Int)..] suppliedArgs)
  pure (allocOps ++ setOps, ptrName)

-- | Ensure a PAP wrapper function exists for (fnName, nSupplied). Emits the
-- wrapper lazily and returns its MLIR symbol name. The wrapper takes
-- (closure, remaining_args...) and dispatches to @fnName(captured..., remaining...).
ensurePapWrapper :: Text -> Int -> Int -> Emit Text
ensurePapWrapper fnName arity nSupplied = do
  let wrapperName = "pap_" <> fnName <> "_" <> T.pack (show nSupplied)
      nRemaining = arity - nSupplied
  existing <- gets esPapWrappers
  if Set.member wrapperName existing
    then pure wrapperName
    else do
      modify (\s -> s { esPapWrappers = Set.insert wrapperName (esPapWrappers s) })
      -- Wrapper signature: (i64 closure, i64 remaining_0, ..., i64 remaining_{nRemaining-1}) -> i64
      let remainingParams = [ "%r" <> T.pack (show i) <> ": i64" | i <- [0 .. nRemaining - 1] ]
          paramList = T.intercalate ", " ("%clos: i64" : remainingParams)
          origParamTys = T.intercalate ", " (replicate arity "i64")
      -- Body: extract captured args from closure fields 1..nSupplied, then call original
      let captureLoads = concat
            [ [ "    %cidx" <> T.pack (show i) <> " = arith.constant " <> T.pack (show i) <> " : i64"
              , "    %c" <> T.pack (show i) <> " = func.call @kk_field(%clos, %cidx" <> T.pack (show i) <> ") : (i64, i64) -> i64"
              ]
            | i <- [1 .. nSupplied]
            ]
          capturedArgRefs = [ "%c" <> T.pack (show i) | i <- [1 .. nSupplied] ]
          remainingArgRefs = [ "%r" <> T.pack (show i) | i <- [0 .. nRemaining - 1] ]
          allArgRefs = T.intercalate ", " (capturedArgRefs ++ remainingArgRefs)
          callLine = "    %result = func.call @" <> fnName <> "(" <> allArgRefs
                     <> ") : (" <> origParamTys <> ") -> i64"
          wrapperText = T.unlines $
            [ "  func.func @" <> wrapperName <> "(" <> paramList <> ") -> i64 {"
            ] ++ captureLoads ++
            [ callLine
            , "    func.return %result : i64"
            , "  }"
            ]
      addLiftedFn wrapperText
      pure wrapperName

-- | Emit a top-level function used as a first-class value.
-- Arity 0 (CAF/thunk): call the function directly — its result IS the value.
-- Arity > 0: build a zero-capture PAP closure with a trampoline wrapper.
emitFnAsValue :: Text -> Map Text Int -> Emit ([Text], Text)
emitFnAsValue fnName arityMap =
  emitFnAsValueWithArgs fnName (Map.findWithDefault 1 fnName arityMap) []

-- | Emit a function as value, optionally pre-supplying captured arguments.
emitFnAsValueWithArgs :: Text -> Int -> [Text] -> Emit ([Text], Text)
emitFnAsValueWithArgs fnName arity suppliedArgs
  | arity == 0, null suppliedArgs = do
      -- Arity-0 function (CAF): call it to get the value it produces.
      -- In GHC, top-level functions like `ctorsInExpr = go where go ...`
      -- compile to 0-arity functions that return a closure.
      resultName <- freshName "v"
      pure ([ "// fn-as-value (CAF): call @" <> fnName <> " to get its result"
            , "%" <> resultName <> " = func.call @" <> fnName <> "() : () -> i64"
            ], resultName)
  | otherwise = emitPapClosure fnName arity suppliedArgs

-- | Record the MLIR type for an SSA name
recordType :: Text -> Text -> Emit ()
recordType name ty = modify (\s -> s { esTypeEnv = Map.insert name ty (esTypeEnv s) })

-- | Look up the MLIR type for an SSA name (default: "i64")
lookupType :: Text -> Emit Text
lookupType name = do
  env <- gets esTypeEnv
  pure $ Map.findWithDefault "i64" name env

-- | Collect a string literal, returning its global name
addStringLit :: Text -> Emit Text
addStringLit str = do
  s <- get
  let idx = length (esStringLits s)
      globalName = "str_" <> T.pack (show idx)
  put s { esStringLits = esStringLits s ++ [(globalName, str)] }
  pure globalName

-- | Emit a Frankenstein Core program as MLIR text
emitProgram :: Program -> Text
emitProgram = emitProgramText

emitProgramText :: Program -> Text
emitProgramText prog =
  let -- Rename user's "main" to "_frankenstein_main" so we can generate our own entry point
      defs = progDefs prog
      hasMain = any (\d -> nameText (qnameName (defName d)) == "main") defs
      -- Rename main and strip EDelay wrapper (GHC marks main as lazy, but
      -- the entry point should be evaluated eagerly, not thunked)
      renamedDefs = if hasMain
        then map (\d -> if nameText (qnameName (defName d)) == "main"
                        then d { defName = QName "" (Name "_frankenstein_main" 99)
                               , defExpr = stripTopDelay (defExpr d) }
                        else d) defs
        else defs
      stripTopDelay (EDelay e) = e
      stripTopDelay e           = e
      modPrefix = let m = qnameModule (progName prog)
                    in if T.null m then "" else sanitizeName m <> "_"
      qualMainName = modPrefix <> "_frankenstein_main"
      -- Top-level names may already be module-mangled by the Linker
      -- (e.g. "Frankenstein.Core.Perceus_perceusExpr") — in that case
      -- sanitizeName alone produces the fully qualified symbol.  Only
      -- prepend modPrefix when the name is still short (demo path or
      -- names the linker left unmangled).
      qualifyDefName d = let t = nameText (qnameName (defName d))
                             san = sanitizeName t
                         in if T.any (== '/') t || T.isPrefixOf modPrefix san
                            then san else modPrefix <> san
      qualifiedTopNames = Set.fromList (map qualifyDefName renamedDefs)
      initState = EmitState 0 [] Map.empty [] Map.empty Map.empty False
                         (qualifiedTopNames `Set.union` externalRuntimeFns)
                         (buildTopFnArity modPrefix renamedDefs `Map.union` externalRuntimeArity)
                         Set.empty
                         (assignProgramTags prog)
                         modPrefix
                         Map.empty
                         Map.empty
                         Map.empty
      (bodyText, finalState) = runState (emitDefs renamedDefs) initState
      liftedFns = T.unlines (reverse (esLiftedFns finalState))
      externDecls = Map.toList (esExternDecls finalState)
      -- Partition externals into Koka builtins (emit inline) vs real externals
      (kokaBuiltins, realExterns) = partition (isKokaBuiltin . fst) externDecls
      externDeclText = if null realExterns then ""
        else T.unlines
          (  ["  // External import declarations (resolved at link time)"]
          ++ [ "  func.func private @" <> nm <> "("
               <> T.intercalate ", " (replicate arity "i64")
               <> ") -> i64"
             | (nm, arity) <- realExterns ]
          ++ [""])
      kokaBuiltinText = emitKokaBuiltins kokaBuiltins
      stringGlobals = T.unlines
        [ "  llvm.mlir.global internal constant @" <> gn <> "(\""
          <> escapeMLIRString s <> "\\00\") {addr_space = 0 : i32}"
        | (gn, s) <- esStringLits finalState ]
      -- Check if _frankenstein_main already prints (i.e. main calls print/putStrLn).
      -- If so, the wrapper should not print the return value.
      mainPrints = any (\d -> nameText (qnameName (defName d)) == "main"
                         && exprCallsPrint (defExpr d)) defs
      mainReturnsADT = any (\d -> nameText (qnameName (defName d)) == "main"
                              && returnsDataType prog d) defs
      mainReturnsString = any (\d -> nameText (qnameName (defName d)) == "main"
                                  && returnsStringType d) defs
      -- The @main wrapper takes (argc, argv) and hands them off to
      -- kk_args_init so programs can read command-line arguments via
      -- the args_count / args_get intrinsics.
      mainHeader =
        [ "  func.func @main(%argc: i32, %argv: !llvm.ptr) -> i32 {"
        , "    func.call @kk_args_init(%argc, %argv) : (i32, !llvm.ptr) -> ()"
        ]
      mainWrapper = if hasMain
        then if mainPrints
          then T.unlines $ mainHeader ++
            [ "    func.call @" <> qualMainName <> "() : () -> i64"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else if mainReturnsString
          then T.unlines $ mainHeader ++
            [ "    %result = func.call @" <> qualMainName <> "() : () -> i64"
            , "    func.call @kk_println_str(%result) : (i64) -> ()"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else if mainReturnsADT
          then T.unlines $ mainHeader ++
            [ "    %result = func.call @" <> qualMainName <> "() : () -> i64"
            , "    func.call @kk_println_con(%result) : (i64) -> ()"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else T.unlines $ mainHeader ++
            [ "    %result = func.call @" <> qualMainName <> "() : () -> i64"
            , "    %fmtaddr = llvm.mlir.addressof @fmt_int : !llvm.ptr"
            , "    llvm.call @printf(%fmtaddr, %result) vararg(!llvm.func<i32 (ptr, ...)>) : (!llvm.ptr, i64) -> i32"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
        else ""
      exprCallsPrint (EApp (EVar fn) _) =
        nameText fn `elem` [ "print", "println_str", "putStrLn", "print_str" ]
      exprCallsPrint (EApp f args)       = exprCallsPrint f || any exprCallsPrint args
      exprCallsPrint (EDelay e)          = exprCallsPrint e
      exprCallsPrint (ELet _ body)       = exprCallsPrint body
      exprCallsPrint (ECase _ bs)        = any (\(Branch _ _ b) -> exprCallsPrint b) bs
      exprCallsPrint (ELam _ body)       = exprCallsPrint body
      exprCallsPrint _                   = False
      -- True iff the def's return type is a TCon whose name matches a
      -- DataDecl in the program. Used to pick the s-expression printer
      -- over printf("%ld") in the main wrapper.
      returnsDataType p d =
        let (_, _, ret) = decomposeDefType (defType d)
            dataNames = [ nameText (qnameName (dataName dd)) | dd <- progData p ]
            tconName (TCon (TypeCon qn _)) = Just (nameText (qnameName qn))
            tconName (TApp t _)            = tconName t
            tconName (TSyn _ _ t)          = tconName t
            tconName (TForall _ t)         = tconName t
            tconName _                     = Nothing
        in case tconName ret of
             Just n  -> n `elem` dataNames
             Nothing -> False
      -- True iff the def's return type is the first-class string type
      -- ('std.string'). Used to pick kk_println_str over printf("%ld")
      -- for the main wrapper.
      returnsStringType d =
        let (_, _, ret) = decomposeDefType (defType d)
            tconName (TCon (TypeCon qn _)) = Just (nameText (qnameName qn))
            tconName (TApp t _)            = tconName t
            tconName (TSyn _ _ t)          = tconName t
            tconName (TForall _ t)         = tconName t
            tconName _                     = Nothing
        in tconName ret == Just "string"
  in T.unlines
    [ "module {"
    , ""
    , "  // External declarations"
    , "  llvm.func @printf(!llvm.ptr, ...) -> i32"
    , "  llvm.mlir.global internal constant @fmt_int(\"%ld\\n\\00\") {addr_space = 0 : i32}"
    , "  llvm.mlir.global internal constant @fmt_str(\"%s\\n\\00\") {addr_space = 0 : i32}"
    , ""
    , "  // String literals"
    , stringGlobals
    , ""
    , "  // Perceus runtime declarations"
    , "  func.func private @kk_drop(i64) -> ()"
    , "  func.func private @kk_retain(i64) -> ()"
    , "  func.func private @kk_release(i64) -> ()"
    , "  func.func private @kk_reuse(i64) -> i64"
    , ""
    , "  // Boxed value runtime declarations"
    , "  func.func private @kk_alloc_con(i64, i64) -> i64"
    , "  func.func private @kk_set_field(i64, i64, i64) -> ()"
    , "  func.func private @kk_tag(i64) -> i64"
    , "  func.func private @kk_field(i64, i64) -> i64"
    , "  func.func private @kk_println_con(i64) -> ()"
    , "  // List constructors"
    , "  func.func private @kk_cons(i64, i64) -> i64"
    , "  func.func private @kk_nil() -> i64"
    , ""
    , "  // First-class string runtime declarations (rope, UTF-8)"
    , "  func.func private @kk_string_from_literal(i64, i64) -> i64"
    , "  func.func private @kk_string_from_cstr(i64) -> i64"
    , "  func.func private @kk_string_empty() -> i64"
    , "  func.func private @kk_println_str(i64) -> ()"
    , "  func.func private @kk_print_str(i64) -> ()"
    , "  func.func private @kk_str_concat(i64, i64) -> i64"
    , "  func.func private @kk_str_len(i64) -> i64"
    , "  func.func private @kk_str_char_len(i64) -> i64"
    , "  func.func private @kk_str_eq(i64, i64) -> i64"
    , "  func.func private @kk_str_flatten(i64) -> i64"
    , "  func.func private @kk_str_show_int(i64) -> i64"
    , "  func.func private @kk_str_retain(i64) -> ()"
    , "  func.func private @kk_str_drop(i64) -> ()"
    , "  // ByteString runtime declarations (byte-oriented)"
    , "  func.func private @kk_bytes_from_literal(i64, i64) -> i64"
    , "  func.func private @kk_bytes_len(i64) -> i64"
    , "  func.func private @kk_bytes_concat(i64, i64) -> i64"
    , "  func.func private @kk_bytes_index(i64, i64) -> i64"
    , "  func.func private @kk_bytes_eq(i64, i64) -> i64"
    , ""
    , "  // File I/O, process, environment runtime declarations"
    , "  func.func private @kk_read_file(i64) -> i64"
    , "  func.func private @kk_write_file(i64, i64) -> i64"
    , "  func.func private @kk_file_exists(i64) -> i64"
    , "  func.func private @kk_read_line() -> i64"
    , "  func.func private @kk_system(i64) -> i64"
    , "  func.func private @kk_getenv(i64) -> i64"
    , "  func.func private @kk_args_count() -> i64"
    , "  func.func private @kk_args_get(i64) -> i64"
    , "  func.func private @kk_args_progname() -> i64"
    , "  func.func private @kk_args_init(i32, !llvm.ptr) -> ()"
    , "  func.func private @kk_exit(i64) -> ()"
    , ""
    , "  // IORef runtime declarations (mutable single-cell)"
    , "  func.func private @kk_ref_new(i64) -> i64"
    , "  func.func private @kk_ref_get(i64) -> i64"
    , "  func.func private @kk_ref_set(i64, i64) -> i64"
    , ""
    , "  // Thunk runtime declarations (lazy evaluation)"
    , "  func.func private @kk_thunk_create(i64) -> i64"
    , "  func.func private @kk_thunk_force(i64) -> i64"
    , ""
    , "  // Evidence vector runtime declarations (algebraic effects)"
    , "  func.func private @kk_evv_create(i64) -> i64"
    , "  func.func private @kk_evv_set(i64, i64, i64) -> ()"
    , "  func.func private @kk_evv_get(i64, i64) -> i64"
    , "  func.func private @kk_unhandled_effect() -> i64"
    , ""
    , "  // Mercury choice effect runtime (multi-shot backtracking)"
    , "  func.func private @mercury_choose() -> i64"
    , "  func.func private @mercury_collect_choices(i64) -> i64"
    , ""
    , externDeclText
    , kokaBuiltinText
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    , mainWrapper
    , "}"
    ]

-- | Emit MLIR with frankenstein.* dialect ops for EHandle/EPerform.
-- Call this WITHOUT running the evidence pass first — effects remain
-- as high-level dialect operations in the output.
emitProgramWithEffects :: Program -> Text
emitProgramWithEffects prog =
  let defs = progDefs prog
      hasMain = any (\d -> nameText (qnameName (defName d)) == "main") defs
      renamedDefs = if hasMain
        then map (\d -> if nameText (qnameName (defName d)) == "main"
                        then d { defName = QName "" (Name "_frankenstein_main" 99)
                               , defExpr = stripTopDelay (defExpr d) }
                        else d) defs
        else defs
      stripTopDelay (EDelay e) = e
      stripTopDelay e           = e
      -- Key difference: esEffectDialect = True
      modPrefix = let m = qnameModule (progName prog)
                    in if T.null m then "" else sanitizeName m <> "_"
      qualifyDefName d = let t = nameText (qnameName (defName d))
                             san = sanitizeName t
                         in if T.any (== '/') t || T.isPrefixOf modPrefix san
                            then san else modPrefix <> san
      qualifiedTopNames = Set.fromList (map qualifyDefName renamedDefs)
      initState = EmitState 0 [] Map.empty [] Map.empty Map.empty True
                         (qualifiedTopNames `Set.union` externalRuntimeFns)
                         (buildTopFnArity modPrefix renamedDefs `Map.union` externalRuntimeArity)
                         Set.empty
                         (assignProgramTags prog)
                         modPrefix
                         Map.empty
                         Map.empty
                         Map.empty
      (bodyText, finalState) = runState (emitDefs renamedDefs) initState
      liftedFns = T.unlines (reverse (esLiftedFns finalState))
      externDecls = Map.toList (esExternDecls finalState)
      externDeclText = if null externDecls then ""
        else T.unlines
          (  ["  // External import declarations (resolved at link time)"]
          ++ [ "  func.func private @" <> nm <> "("
               <> T.intercalate ", " (replicate arity "i64")
               <> ") -> i64"
             | (nm, arity) <- externDecls ]
          ++ [""])
      stringGlobals = T.unlines
        [ "  llvm.mlir.global internal constant @" <> gn <> "(\""
          <> escapeMLIRString s <> "\\00\") {addr_space = 0 : i32}"
        | (gn, s) <- esStringLits finalState ]
  in T.unlines
    [ "// Frankenstein Effect Dialect MLIR"
    , "// Use: mlir-opt --allow-unregistered-dialect to validate"
    , "module {"
    , ""
    , "  // External declarations"
    , "  llvm.func @printf(!llvm.ptr, ...) -> i32"
    , ""
    , "  // String literals"
    , stringGlobals
    , ""
    , "  // Runtime declarations (same as standard emitter)"
    , "  func.func private @kk_drop(i64) -> ()"
    , "  func.func private @kk_retain(i64) -> ()"
    , "  func.func private @kk_release(i64) -> ()"
    , "  func.func private @kk_reuse(i64) -> i64"
    , ""
    , "  func.func private @kk_alloc_con(i64, i64) -> i64"
    , "  func.func private @kk_set_field(i64, i64, i64) -> ()"
    , "  func.func private @kk_tag(i64) -> i64"
    , "  func.func private @kk_field(i64, i64) -> i64"
    , "  func.func private @kk_println_con(i64) -> ()"
    , ""
    , "  func.func private @kk_string_from_literal(i64, i64) -> i64"
    , "  func.func private @kk_string_from_cstr(i64) -> i64"
    , "  func.func private @kk_string_empty() -> i64"
    , "  func.func private @kk_println_str(i64) -> ()"
    , "  func.func private @kk_print_str(i64) -> ()"
    , "  func.func private @kk_str_concat(i64, i64) -> i64"
    , "  func.func private @kk_str_len(i64) -> i64"
    , "  func.func private @kk_str_char_len(i64) -> i64"
    , "  func.func private @kk_str_eq(i64, i64) -> i64"
    , "  func.func private @kk_str_flatten(i64) -> i64"
    , "  func.func private @kk_str_show_int(i64) -> i64"
    , "  func.func private @kk_str_retain(i64) -> ()"
    , "  func.func private @kk_str_drop(i64) -> ()"
    , ""
    , "  func.func private @kk_thunk_create(i64) -> i64"
    , "  func.func private @kk_thunk_force(i64) -> i64"
    , ""
    , "  func.func private @kk_evv_create(i64) -> i64"
    , "  func.func private @kk_evv_set(i64, i64, i64) -> ()"
    , "  func.func private @kk_evv_get(i64, i64) -> i64"
    , "  func.func private @kk_unhandled_effect() -> i64"
    , ""
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    , "}"
    ]

-- | Emit MLIR for Wasm target: no printf, _frankenstein_main exported directly.
-- The JS/Wasm host reads the return value of _frankenstein_main().
emitProgramWasm :: Program -> Text
emitProgramWasm prog =
  let defs = progDefs prog
      hasMain = any (\d -> nameText (qnameName (defName d)) == "main") defs
      renamedDefs = if hasMain
        then map (\d -> if nameText (qnameName (defName d)) == "main"
                        then d { defName = QName "" (Name "_frankenstein_main" 99)
                               , defExpr = stripTopDelay (defExpr d) }
                        else d) defs
        else defs
      stripTopDelay (EDelay e) = e
      stripTopDelay e           = e
      modPrefix = let m = qnameModule (progName prog)
                    in if T.null m then "" else sanitizeName m <> "_"
      qualifyDefName d = let t = nameText (qnameName (defName d))
                             san = sanitizeName t
                         in if T.any (== '/') t || T.isPrefixOf modPrefix san
                            then san else modPrefix <> san
      qualifiedTopNames = Set.fromList (map qualifyDefName renamedDefs)
      initState = EmitState 0 [] Map.empty [] Map.empty Map.empty False
                         qualifiedTopNames
                         (buildTopFnArity modPrefix renamedDefs)
                         Set.empty
                         (assignProgramTags prog)
                         modPrefix
                         Map.empty
                         Map.empty
                         Map.empty
      (bodyText, finalState) = runState (emitDefs renamedDefs) initState
      liftedFns = T.unlines (reverse (esLiftedFns finalState))
      externDecls = Map.toList (esExternDecls finalState)
      externDeclText = if null externDecls then ""
        else T.unlines
          (  ["  // External import declarations (resolved at link time)"]
          ++ [ "  func.func private @" <> nm <> "("
               <> T.intercalate ", " (replicate arity "i64")
               <> ") -> i64"
             | (nm, arity) <- externDecls ]
          ++ [""])
      stringGlobals = T.unlines
        [ "  llvm.mlir.global internal constant @" <> gn <> "(\""
          <> escapeMLIRString s <> "\\00\") {addr_space = 0 : i32}"
        | (gn, s) <- esStringLits finalState ]
  in T.unlines
    [ "module {"
    , ""
    , "  // External declarations (printf provided by Wasm host)"
    , "  llvm.func @printf(!llvm.ptr, ...) -> i32"
    , "  llvm.mlir.global internal constant @fmt_int(\"%ld\\n\\00\") {addr_space = 0 : i32}"
    , "  llvm.mlir.global internal constant @fmt_str(\"%s\\n\\00\") {addr_space = 0 : i32}"
    , ""
    , "  // String literals"
    , stringGlobals
    , ""
    , "  // Perceus runtime declarations"
    , "  func.func private @kk_drop(i64) -> ()"
    , "  func.func private @kk_retain(i64) -> ()"
    , "  func.func private @kk_release(i64) -> ()"
    , "  func.func private @kk_reuse(i64) -> i64"
    , ""
    , "  // Boxed value runtime declarations"
    , "  func.func private @kk_alloc_con(i64, i64) -> i64"
    , "  func.func private @kk_set_field(i64, i64, i64) -> ()"
    , "  func.func private @kk_tag(i64) -> i64"
    , "  func.func private @kk_field(i64, i64) -> i64"
    , "  func.func private @kk_println_con(i64) -> ()"
    , ""
    , "  // Thunk runtime declarations"
    , "  func.func private @kk_thunk_create(i64) -> i64"
    , "  func.func private @kk_thunk_force(i64) -> i64"
    , ""
    , "  // Evidence vector runtime declarations"
    , "  func.func private @kk_evv_create(i64) -> i64"
    , "  func.func private @kk_evv_set(i64, i64, i64) -> ()"
    , "  func.func private @kk_evv_get(i64, i64) -> i64"
    , "  func.func private @kk_unhandled_effect() -> i64"
    , ""
    , "  // Mercury choice effect runtime"
    , "  func.func private @mercury_choose() -> i64"
    , "  func.func private @mercury_collect_choices(i64) -> i64"
    , ""
    , externDeclText
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    , "}"
    ]

emitDefs :: [Def] -> Emit Text
emitDefs defs = do
  texts <- mapM emitDef defs
  pure $ T.unlines texts

emitDef :: Def -> Emit Text
emitDef def = do
  let name = nameText (qnameName (defName def))
      (_argNames, _argTypes, retType) = decomposeDefType (defType def)
      -- Strip ETypeLam wrappers so the arity buildTopFnArity sees matches
      -- the arity we actually emit. Otherwise a `ETypeLam (ELam ps b)` def
      -- would emit a 0-param func while the call site uses arity = length ps.
      stripTypeLam (ETypeLam _ e) = stripTypeLam e
      stripTypeLam e              = e
  qualName <- do
    let san = sanitizeName name
    pfx <- gets esModulePrefix
    pure $ if T.any (== '/') name || T.isPrefixOf pfx san then san else pfx <> san
  case stripTypeLam (defExpr def) of
    ELam params body -> do
      -- Use uniform i64 for all top-level fn params (matches the closure ABI
      -- and avoids type mismatches when params flow into kk_* runtime calls
      -- or PAP wrappers that assume i64 throughout).
      let mlirArgs = T.intercalate ", "
            [ "%" <> nameToSsa pn <> ": i64" | (pn, _) <- params ]
          mlirRetTy = "i64"
      -- Install parameters as identity aliases so EVar lookups find them.
      savedA <- gets esAliases
      let paramAliases = [ (nameToSsa pn, nameToSsa pn) | (pn, _) <- params ]
      modify (\s -> s { esAliases = foldr (\(k,v) m -> Map.insert k v m)
                                          (esAliases s) paramAliases })
      bodyText <- emitBody body mlirRetTy
      modify (\s -> s { esAliases = savedA })
      pure $ T.unlines
        [ "  func.func @" <> qualName <> "(" <> mlirArgs <> ") -> " <> mlirRetTy <> " {"
        , bodyText
        , "  }"
        ]
    -- Non-lambda top-level: emit as nullary function (uniform i64 return)
    expr -> do
      let mlirRetTy = "i64"
      bodyText <- emitBody expr mlirRetTy
      pure $ T.unlines
        [ "  func.func @" <> qualName <> "() -> " <> mlirRetTy <> " {"
        , bodyText
        , "  }"
        ]

-- | Emit a function body, producing MLIR operations ending with func.return
emitBody :: Expr -> Text -> Emit Text
emitBody expr retTy = do
  (ops, resultName) <- emitExpr expr
  pure $ T.unlines $
    map ("    " <>) ops ++
    [ "    func.return %" <> resultName <> " : " <> retTy ]

-- | Emit a Core expression. Returns (list of MLIR ops, result SSA name)
emitExpr :: Expr -> Emit ([Text], Text)
emitExpr (ELit (LitInt n)) = do
  name <- freshName "v"
  pure (["%" <> name <> " = arith.constant " <> T.pack (show n) <> " : i64"], name)

emitExpr (ELit (LitFloat n)) = do
  name <- freshName "v"
  recordType name "f64"
  pure (["%" <> name <> " = arith.constant " <> T.pack (show n) <> " : f64"], name)

emitExpr (ELit (LitChar c)) = do
  name <- freshName "v"
  pure (["%" <> name <> " = arith.constant " <> T.pack (show (fromEnum c)) <> " : i64"], name)

emitExpr (ELit (LitString s)) = do
  globalName <- addStringLit s
  ptrName <- freshName "v"
  intName <- freshName "v"
  lenName <- freshName "v"
  strName <- freshName "v"
  -- Wrap the static .rodata bytes in a kk_string_t leaf via the runtime.
  -- byte_len is the UTF-8 byte length, not the Char count.
  let byteLen = BS.length (TE.encodeUtf8 s)
  pure ( [ "%" <> ptrName <> " = llvm.mlir.addressof @" <> globalName <> " : !llvm.ptr"
         , "%" <> intName <> " = llvm.ptrtoint %" <> ptrName <> " : !llvm.ptr to i64"
         , "%" <> lenName <> " = arith.constant " <> T.pack (show byteLen) <> " : i64"
         , "%" <> strName <> " = func.call @kk_string_from_literal(%" <> intName <> ", %" <> lenName <> ") : (i64, i64) -> i64"
         ]
       , strName)

-- kk_nil used as a bare variable (not inside EApp)
emitExpr (EVar n)
  | nameText n == "kk_nil" = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_nil() : () -> i64" ]
           , resultName)

emitExpr (EVar n) = do
  -- Variable reference — look up in alias map; if not found and not a known
  -- top-level function, emit a direct func.call (for cross-module or external refs).
  let sname = nameToSsa n
      sanitized = sanitizeName (nameText n)
  aliases <- gets esAliases
  topFns <- gets esTopFns
  -- If the name already has a module qualifier or was linker-mangled, it's
  -- already fully qualified — don't prepend esModulePrefix again.
  qualSanitized <- do
    pfx <- gets esModulePrefix
    pure $ if T.any (== '/') (nameText n) || T.isPrefixOf pfx sanitized
           then sanitized else pfx <> sanitized
  arityMap <- gets esTopFnArity
  case Map.lookup sname aliases of
    Just target -> pure ([], target)
    Nothing
      | Set.member qualSanitized topFns ->
          emitFnAsValue qualSanitized arityMap
      | Set.member sanitized topFns ->
          emitFnAsValue sanitized arityMap
      | otherwise -> do
          -- Check if this is a promoted let-bound lambda used as a value.
          promoted <- gets esPromotedFns
          case Map.lookup sname promoted of
            Just promotedName -> do
              -- Promoted fn used as value: build closure with captures.
              capKeys <- gets (Map.findWithDefault [] promotedName . esPromotedCaptures)
              capAliases <- gets esAliases
              let capSsaNames = [ Map.findWithDefault k k capAliases | k <- capKeys ]
                  arity = Map.findWithDefault (length capSsaNames + 1) promotedName arityMap
              emitFnAsValueWithArgs promotedName arity capSsaNames
            Nothing -> do
              -- Unresolved external: emit as direct func.call so it becomes
              -- a real linker symbol. Called with 0 args (value reference).
              let callName = externMangled qualSanitized 0
              addExternDecl callName 0
              stubName <- freshName "v"
              pure (["%" <> stubName <> " = func.call @" <> callName
                     <> "() : () -> i64"], stubName)

-- Constructor reference: allocate a boxed value via the runtime
emitExpr (ECon qn) = do
  tag <- lookupConTag qn
  tagName <- freshName "v"
  nfieldsName <- freshName "v"
  resultName <- freshName "v"
  pure ([ "%" <> tagName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
        , "%" <> nfieldsName <> " = arith.constant 0 : i64"
        , "%" <> resultName <> " = func.call @kk_alloc_con(%" <> tagName <> ", %" <> nfieldsName <> ") : (i64, i64) -> i64"
        ], resultName)

-- Constructor application: allocate via runtime, set fields
emitExpr (EApp (ECon qn) args) = do
  tag <- lookupConTag qn
  let nFields = length args
  -- Emit all argument expressions
  argResults <- mapM emitExpr args
  let allOps = concatMap fst argResults
      argNames = map snd argResults
  -- Allocate the constructor
  tagName <- freshName "v"
  nfieldsName <- freshName "v"
  ptrName <- freshName "v"
  let allocOps = [ "%" <> tagName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
                 , "%" <> nfieldsName <> " = arith.constant " <> T.pack (show nFields) <> " : i64"
                 , "%" <> ptrName <> " = func.call @kk_alloc_con(%" <> tagName <> ", %" <> nfieldsName <> ") : (i64, i64) -> i64"
                 ]
  -- Set each field via kk_set_field
  setOps <- mapM (\(i, aName) -> do
    idxName <- freshName "v"
    pure [ "%" <> idxName <> " = arith.constant " <> T.pack (show i) <> " : i64"
         , "func.call @kk_set_field(%" <> ptrName <> ", %" <> idxName <> ", %" <> aName <> ") : (i64, i64, i64) -> ()"
         ]) (zip [(0::Int)..] argNames)
  pure (allOps ++ allocOps ++ concat setOps, ptrName)

-- Float binary ops
emitExpr (EApp (EVar fn) [a, b])
  | nameText fn == "+f" || nameText fn == "addf" = emitBinOp "arith.addf" "f64" a b
  | nameText fn == "-f" || nameText fn == "subf" = emitBinOp "arith.subf" "f64" a b
  | nameText fn == "*f" || nameText fn == "mulf" = emitBinOp "arith.mulf" "f64" a b
  | nameText fn == "/f" || nameText fn == "divf" = emitBinOp "arith.divf" "f64" a b

-- Float comparisons
emitExpr (EApp (EVar fn) [a, b])
  | nameText fn == "==f" || nameText fn == "eqf" = emitFloatCmpOp "oeq" a b
  | nameText fn == "/=f" || nameText fn == "nef" = emitFloatCmpOp "one" a b
  | nameText fn == "<f"  || nameText fn == "ltf" = emitFloatCmpOp "olt" a b
  | nameText fn == ">f"  || nameText fn == "gtf" = emitFloatCmpOp "ogt" a b
  | nameText fn == "<=f" || nameText fn == "lef" = emitFloatCmpOp "ole" a b
  | nameText fn == ">=f" || nameText fn == "gef" = emitFloatCmpOp "oge" a b

-- Integer binary ops (including GHC primops with # suffix)
emitExpr (EApp (EVar fn) [a, b])
  | n `elem` ["+", "add", "+#", "$fNumInt_$c+"] = emitBinOp "arith.addi" "i64" a b
  | n `elem` ["-", "sub", "-#", "$fNumInt_$c-"] = emitBinOp "arith.subi" "i64" a b
  | n `elem` ["*", "mul", "*#", "$fNumInt_$c*"] = emitBinOp "arith.muli" "i64" a b
  | n `elem` ["/", "div", "quot#", "quotInt#"]  = emitBinOp "arith.divsi" "i64" a b
  | n `elem` ["mod", "rem#", "remInt#"]          = emitBinOp "arith.remsi" "i64" a b
  | n `elem` ["==", "eq", "==#"]                 = emitCmpOp "eq" a b
  | n `elem` ["/=", "ne", "/=#"]                 = emitCmpOp "ne" a b
  | n `elem` ["<", "lt", "<#"]                   = emitCmpOp "slt" a b
  | n `elem` [">", "gt", ">#"]                   = emitCmpOp "sgt" a b
  | n `elem` ["<=", "le", "<=#"]                 = emitCmpOp "sle" a b
  | n `elem` [">=", "ge", ">=#"]                 = emitCmpOp "sge" a b
  | n `elem` ["andI#", "and#"]                    = emitBinOp "arith.andi" "i64" a b
  | n `elem` ["orI#", "or#"]                      = emitBinOp "arith.ori" "i64" a b
  | n `elem` ["xorI#", "xor#"]                    = emitBinOp "arith.xori" "i64" a b
  where n = nameText fn

-- Unary integer operations
emitExpr (EApp (EVar fn) [arg])
  | nameText fn `elem` ["negate", "negateInt#", "$fNumInt_$cnegate"] = do
      (argOps, argName) <- emitExpr arg
      zeroName <- freshName "v"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> zeroName <> " = arith.constant 0 : i64"
        , "%" <> resultName <> " = arith.subi %" <> zeroName <> ", %" <> argName <> " : i64"
        ], resultName)
  | nameText fn == "abs" = do
      (argOps, argName) <- emitExpr arg
      zeroName <- freshName "v"
      negName <- freshName "v"
      cmpName <- freshName "v"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> zeroName <> " = arith.constant 0 : i64"
        , "%" <> negName <> " = arith.subi %" <> zeroName <> ", %" <> argName <> " : i64"
        , "%" <> cmpName <> " = arith.cmpi slt, %" <> argName <> ", %" <> zeroName <> " : i64"
        , "%" <> resultName <> " = arith.select %" <> cmpName <> ", %" <> negName <> ", %" <> argName <> " : i64"
        ], resultName)

-- Futhark array primitives → MLIR linalg.
-- These light up the linalg/memref dialects in the lowering pipeline.
-- The pattern is always: allocate a dynamic memref, fill it via scf.for
-- (iota or iota²), reduce it via linalg.generic, load the scalar.
emitExpr (EApp (EVar fn) [nArg])
  | nameText fn `elem` ["sum_iota", "dot_iota"] = do
      let isSquare = nameText fn == "dot_iota"
      (nOps, nName) <- emitExpr nArg
      idxName  <- freshName "n"
      bufName  <- freshName "xs"
      iName    <- freshName "i"
      ivName   <- freshName "iv"
      sqName   <- freshName "sq"
      lo       <- freshName "lo"
      step     <- freshName "step"
      zeroI    <- freshName "z"
      accBuf   <- freshName "acc"
      inN      <- freshName "in"
      accN     <- freshName "ac"
      sumN     <- freshName "s"
      result   <- freshName "v"
      let storeVal = if isSquare then sqName else ivName
          squareOp = if isSquare
            then ["          %" <> sqName <> " = arith.muli %" <> ivName
                  <> ", %" <> ivName <> " : i64"]
            else []
      pure (nOps ++
        [ "    %" <> idxName <> " = arith.index_cast %" <> nName <> " : i64 to index"
        , "    %" <> bufName <> " = memref.alloca(%" <> idxName <> ") : memref<?xi64>"
        , "    %" <> lo   <> " = arith.constant 0 : index"
        , "    %" <> step <> " = arith.constant 1 : index"
        , "    scf.for %" <> iName <> " = %" <> lo <> " to %" <> idxName
            <> " step %" <> step <> " {"
        , "      %" <> ivName <> " = arith.index_cast %" <> iName <> " : index to i64"
        ] ++ squareOp ++
        [ "      memref.store %" <> storeVal <> ", %" <> bufName
            <> "[%" <> iName <> "] : memref<?xi64>"
        , "    }"
        , "    %" <> zeroI  <> " = arith.constant 0 : i64"
        , "    %" <> accBuf <> " = memref.alloca() : memref<i64>"
        , "    memref.store %" <> zeroI <> ", %" <> accBuf <> "[] : memref<i64>"
        , "    linalg.generic {"
        , "      indexing_maps = [affine_map<(d0) -> (d0)>, affine_map<(d0) -> ()>],"
        , "      iterator_types = [\"reduction\"]"
        , "    } ins(%" <> bufName <> " : memref<?xi64>) outs(%" <> accBuf
            <> " : memref<i64>) {"
        , "    ^bb0(%" <> inN <> ": i64, %" <> accN <> ": i64):"
        , "      %" <> sumN <> " = arith.addi %" <> inN <> ", %" <> accN <> " : i64"
        , "      linalg.yield %" <> sumN <> " : i64"
        , "    }"
        , "    %" <> result <> " = memref.load %" <> accBuf <> "[] : memref<i64>"
        ], result)

-- Haskell print/putStrLn: emit as printf call
-- GHC desugars print to dictionary-passing, but after stripping dicts
-- we get a bare call to print with one argument.
emitExpr (EApp (EVar fn) [arg])
  | nameText fn == "print" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      fmtName <- freshName "v"
      pure (argOps ++
        [ "%" <> fmtName <> " = llvm.mlir.addressof @fmt_int : !llvm.ptr"
        , "llvm.call @printf(%" <> fmtName <> ", %" <> argName <> ") vararg(!llvm.func<i32 (ptr, ...)>) : (!llvm.ptr, i64) -> i32"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)

-- First-class string intrinsics: lower to runtime helpers. Bridges emit
-- these as plain EVar references; the function names below are the
-- canonical intrinsic surface that all frontends share.
emitExpr (EApp (EVar fn) [arg])
  | nameText fn `elem` ["println_str", "putStrLn", "print_str"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_println_str(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  | nameText fn `elem` ["str_len", "strlen", "bytes_len"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_len(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["str_char_len", "char_len", "char_count", "length"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_char_len(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["str_flatten", "flatten"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_flatten(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["show", "show_int", "str_show_int"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_show_int(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- File I/O, process, environment intrinsics (1-arg)
  | nameText fn `elem` ["read_file", "readFile"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_read_file(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["file_exists", "fileExists"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_file_exists(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["system", "shell"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_system(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["getenv", "getEnv"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_getenv(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- IORef intrinsics (1-arg)
  | nameText fn `elem` ["new_ref", "newIORef", "ref"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_ref_new(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["get_ref", "readIORef", "deref"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_ref_get(%" <> argName <> ") : (i64) -> i64"
        ], resultName)

-- Zero-arg intrinsics (e.g. stdin read_line, args_count)
-- List constructors: Koka Nil/Cons mapped to runtime calls
emitExpr (EApp (EVar fn) [h, t])
  | nameText fn == "kk_cons" = do
      (hOps, hName) <- emitExpr h
      (tOps, tName) <- emitExpr t
      resultName <- freshName "v"
      pure (hOps ++ tOps ++
        [ "%" <> resultName <> " = func.call @kk_cons(%" <> hName <> ", %" <> tName <> ") : (i64, i64) -> i64"
        ], resultName)

emitExpr (EApp (EVar fn) [])
  | nameText fn == "kk_nil" = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_nil() : () -> i64" ]
           , resultName)

emitExpr (EApp (EVar fn) [])
  | nameText fn `elem` ["read_line", "getLine"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_read_line() : () -> i64" ]
           , resultName)
  | nameText fn `elem` ["args_count", "numArgs"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_args_count() : () -> i64" ]
           , resultName)
  | nameText fn `elem` ["args_progname", "getProgName"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_args_progname() : () -> i64" ]
           , resultName)

-- Single-arg command-line / exit intrinsics
emitExpr (EApp (EVar fn) [arg])
  | nameText fn `elem` ["args_get", "getArg"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_args_get(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["exit", "exitWith"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_exit(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64  // unreachable (exit)"
        ], resultName)

emitExpr (EApp (EVar fn) [a, b])
  | nameText fn `elem` ["str_concat", "++s", "concat_str", "bytes_concat"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_str_concat(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["str_eq", "==s", "bytes_eq"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_str_eq(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  | nameText fn `elem` ["bytes_index", "byte_at"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_bytes_index(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- File I/O (2-arg): write_file(path, content) -> 0/-1
  | nameText fn `elem` ["write_file", "writeFile"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_write_file(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- IORef set: returns 0 (kk_ref_set is void in C, wrapper returns 0)
  | nameText fn `elem` ["set_ref", "writeIORef"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_ref_set(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)

-- Evidence intrinsic: evv_select(evv, idx) -> kk_evv_get(evv, idx)
emitExpr (EApp (EVar fn) [evvArg, idxArg])
  | nameText fn == "evv_select" = do
      (evvOps, evvName) <- emitExpr evvArg
      (idxOps, idxName) <- emitExpr idxArg
      resultName <- freshName "v"
      let callOp = "%" <> resultName <> " = func.call @kk_evv_get(%" <> evvName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
      pure (evvOps ++ idxOps ++ [callOp], resultName)

emitExpr (EApp (EVar fn) args) = do
  -- General function call. If fn names a top-level function, emit a direct
  -- func.call; otherwise treat it as a local closure value (heap-allocated
  -- via kk_alloc_con) and dispatch indirectly through field 0 (fptr).
  argResults <- mapM emitExpr args
  let allOps = concatMap fst argResults
      argNames = map snd argResults
      argList = T.intercalate ", " ["%" <> n | n <- argNames]
      sanitized = sanitizeName (nameText fn)
      hasModule = T.any (== '/') (nameText fn)
  argTypes <- mapM lookupType argNames
  let argTypeList = T.intercalate ", " argTypes
  topFns <- gets esTopFns
  arityMap <- gets esTopFnArity
  -- If the name already has a module qualifier or was linker-mangled, it's
  -- already fully qualified — don't prepend esModulePrefix again.
  qualSanitized <- do
    pfx <- gets esModulePrefix
    pure $ if hasModule || T.isPrefixOf pfx sanitized then sanitized else pfx <> sanitized
  let nArgs = length args
      mArity = Map.lookup qualSanitized arityMap
  if Set.member qualSanitized topFns
    then case mArity of
      Just arity | nArgs < arity -> do
        -- Undersaturated: build a PAP closure.
        (papOps, resultName) <- emitPapClosure qualSanitized arity argNames
        pure (allOps ++ papOps, resultName)
      Just arity | nArgs > arity -> do
        -- Oversaturated: call the top-level fn with the first `arity` args
        -- to obtain a closure i64, then dispatch through that closure for
        -- the remaining args via the closure-indirect path.
        let (satArgs, extraArgs) = splitAt arity argNames
            (satTys, extraTys)   = splitAt arity argTypes
            satList = T.intercalate ", " ["%" <> n | n <- satArgs]
            satTyList = T.intercalate ", " satTys
        rawClosName <- freshName "v"
        closName <- freshName "v"
        let topCallTy = if arity == 0 then "() -> i64" else "(" <> satTyList <> ") -> i64"
            topCallOp = "%" <> rawClosName <> " = func.call @" <> qualSanitized
                        <> "(" <> satList <> ") : " <> topCallTy
            -- Force the result: arity-0 functions (CAFs) return thunks whose
            -- field layout differs from closures. kk_thunk_force is a no-op
            -- on non-thunks, so this is always safe.
            forceOp = "%" <> closName <> " = func.call @kk_thunk_force(%" <> rawClosName <> ") : (i64) -> i64"
        -- Closure-indirect call with the remaining args
        idxZeroName <- freshName "v"
        fptrIntName <- freshName "v"
        fptrPtrName <- freshName "v"
        resultName  <- freshName "v"
        let closArgList = T.intercalate ", " (("%" <> closName) : ["%" <> n | n <- extraArgs])
            closArgTypes = T.intercalate ", " ("i64" : extraTys)
            extractOps =
              [ "%" <> idxZeroName <> " = arith.constant 0 : i64"
              , "%" <> fptrIntName <> " = func.call @kk_field(%" <> closName <> ", %" <> idxZeroName <> ") : (i64, i64) -> i64"
              , "%" <> fptrPtrName <> " = llvm.inttoptr %" <> fptrIntName <> " : i64 to !llvm.ptr"
              , "%" <> resultName <> " = llvm.call %" <> fptrPtrName
                <> "(" <> closArgList <> ") : !llvm.ptr, (" <> closArgTypes <> ") -> i64"
              ]
        pure (allOps ++ [topCallOp, forceOp] ++ extractOps, resultName)
      _ -> do
        resultName <- freshName "v"
        let callOp = "%" <> resultName <> " = func.call @" <> qualSanitized
                     <> "(" <> argList <> ") : (" <> argTypeList <> ") -> i64"
        pure (allOps ++ [callOp], resultName)
    else do
      -- The function name is not in esTopFns. Two cases:
      -- (a) It's a local variable holding a closure → closure-indirect call
      -- (b) It's an unresolved external import → emit direct func.call
      aliases <- gets esAliases
      let rawName = nameToSsa fn
      case Map.lookup rawName aliases of
        Just closName -> do
          -- Case (a): local closure-indirect call
          idxZeroName <- freshName "v"
          fptrIntName <- freshName "v"
          fptrPtrName <- freshName "v"
          resultName  <- freshName "v"
          let closArgList = T.intercalate ", " (("%" <> closName) : ["%" <> n | n <- argNames])
              closArgTypes = T.intercalate ", " ("i64" : argTypes)
              extractOps =
                [ "%" <> idxZeroName <> " = arith.constant 0 : i64"
                , "%" <> fptrIntName <> " = func.call @kk_field(%" <> closName <> ", %" <> idxZeroName <> ") : (i64, i64) -> i64"
                , "%" <> fptrPtrName <> " = llvm.inttoptr %" <> fptrIntName <> " : i64 to !llvm.ptr"
                , "%" <> resultName <> " = llvm.call %" <> fptrPtrName
                  <> "(" <> closArgList <> ") : !llvm.ptr, (" <> closArgTypes <> ") -> i64"
                ]
          pure (allOps ++ extractOps, resultName)
        Nothing -> do
          -- Check if this is a promoted let-bound lambda (e.g. go, goBranch).
          promoted <- gets esPromotedFns
          case Map.lookup rawName promoted of
            Just promotedName -> do
              -- Promoted let-bound lambda: emit direct func.call to the
              -- top-level function we created via emitBindAsTopFn.
              -- Prepend captured values as extra leading arguments.
              capKeys <- gets (Map.findWithDefault [] promotedName . esPromotedCaptures)
              capAliases <- gets esAliases
              let capSsaNames = [ Map.findWithDefault k k capAliases | k <- capKeys ]
                  allArgNames = capSsaNames ++ argNames
                  allArgList = T.intercalate ", " ["%" <> n | n <- allArgNames]
              allArgTypes <- mapM lookupType allArgNames
              let allArgTypeList = T.intercalate ", " allArgTypes
                  totalArgs = length allArgNames
                  pArity = Map.lookup promotedName arityMap
              case pArity of
                Just ar | totalArgs < ar -> do
                  (papOps, resultName) <- emitPapClosure promotedName ar allArgNames
                  pure (allOps ++ papOps, resultName)
                Just ar | totalArgs > ar -> do
                  let (satArgs, extraArgs) = splitAt ar allArgNames
                      (satTys, extraTys)   = splitAt ar allArgTypes
                      satList = T.intercalate ", " ["%" <> n | n <- satArgs]
                      satTyList = T.intercalate ", " satTys
                  rawClosName <- freshName "v"
                  closName <- freshName "v"
                  let topCallTy = if ar == 0 then "() -> i64" else "(" <> satTyList <> ") -> i64"
                      topCallOp = "%" <> rawClosName <> " = func.call @" <> promotedName
                                  <> "(" <> satList <> ") : " <> topCallTy
                      forceOp = "%" <> closName <> " = func.call @kk_thunk_force(%" <> rawClosName <> ") : (i64) -> i64"
                  idxZeroName <- freshName "v"
                  fptrIntName <- freshName "v"
                  fptrPtrName <- freshName "v"
                  resultName  <- freshName "v"
                  let closArgList = T.intercalate ", " (("%" <> closName) : ["%" <> n | n <- extraArgs])
                      closArgTypes = T.intercalate ", " ("i64" : extraTys)
                      extractOps =
                        [ "%" <> idxZeroName <> " = arith.constant 0 : i64"
                        , "%" <> fptrIntName <> " = func.call @kk_field(%" <> closName <> ", %" <> idxZeroName <> ") : (i64, i64) -> i64"
                        , "%" <> fptrPtrName <> " = llvm.inttoptr %" <> fptrIntName <> " : i64 to !llvm.ptr"
                        , "%" <> resultName <> " = llvm.call %" <> fptrPtrName
                          <> "(" <> closArgList <> ") : !llvm.ptr, (" <> closArgTypes <> ") -> i64"
                        ]
                  pure (allOps ++ [topCallOp, forceOp] ++ extractOps, resultName)
                _ -> do
                  resultName <- freshName "v"
                  let callOp = "%" <> resultName <> " = func.call @" <> promotedName
                               <> "(" <> allArgList <> ") : (" <> allArgTypeList <> ") -> i64"
                  pure (allOps ++ [callOp], resultName)
            Nothing -> do
              -- Unresolved external — emit direct func.call as a real
              -- linker symbol so C shims or GHC library objects can satisfy it.
              let callName = externMangled qualSanitized nArgs
              addExternDecl callName nArgs
              resultName <- freshName "v"
              let callOp = "%" <> resultName <> " = func.call @" <> callName
                           <> "(" <> argList <> ") : (" <> argTypeList <> ") -> i64"
              pure (allOps ++ [callOp], resultName)

-- Application of a non-var, non-con expression (e.g. the result of a
-- closure allocation or a let-bound closure value). The function value
-- here is always an i64 heap pointer to a closure, so we fetch the
-- code pointer via kk_field just like the var-indirect path above, and
-- thread the closure itself as the leading argument.
-- Strip ETypeApp wrapper from callee so it can match the EApp (EVar fn) path
emitExpr (EApp (ETypeApp fn _) args) = emitExpr (EApp fn args)
-- Strip ETypeLam wrapper from callee (type abstraction applied to args)
emitExpr (EApp (ETypeLam _ fn) args) = emitExpr (EApp fn args)

emitExpr (EApp fn args) = do
  (fnOps, fnName) <- emitExpr fn
  argResults <- mapM emitExpr args
  let allArgOps = concatMap fst argResults
      argNames = map snd argResults
      nArgs = length argNames
      argTys = replicate nArgs "i64"
  idxZeroName <- freshName "v"
  fptrIntName <- freshName "v"
  fptrPtrName <- freshName "v"
  resultName  <- freshName "v"
  let closArgList  = T.intercalate ", " (("%" <> fnName) : ["%" <> n | n <- argNames])
      closArgTypes = T.intercalate ", " ("i64" : argTys)
      extractOps =
        [ "%" <> idxZeroName <> " = arith.constant 0 : i64"
        , "%" <> fptrIntName <> " = func.call @kk_field(%" <> fnName <> ", %" <> idxZeroName <> ") : (i64, i64) -> i64"
        , "%" <> fptrPtrName <> " = llvm.inttoptr %" <> fptrIntName <> " : i64 to !llvm.ptr"
        , "%" <> resultName <> " = llvm.call %" <> fptrPtrName
          <> "(" <> closArgList <> ") : !llvm.ptr, (" <> closArgTypes <> ") -> i64"
        ]
  pure (fnOps ++ allArgOps ++ extractOps, resultName)

emitExpr (ECase scrut branches) = do
  -- Pattern matching
  (scrutOps, scrutName) <- emitExpr scrut
  -- Pre-register any PatVar bindings: a PatVar in a branch means
  -- "bind the scrutinee to this variable".  We do this before
  -- classification so that IntLitCase/BoolCase/ConCase defaults
  -- all see the binding.
  forM_ branches $ \(Branch pat _ _) -> case pat of
    PatVar n _ -> do
      let varSsa = nameToSsa n
      modify (\s -> s { esAliases = Map.insert varSsa scrutName (esAliases s) })
    _ -> pure ()
  case classifyBranches branches of
    -- Integer literal cases (existing behavior)
    IntLitCase litVal thenExpr elseExpr ->
      emitIntCase scrutOps scrutName litVal thenExpr elseExpr

    -- Multi-way integer literal case
    MultiIntLitCase litBranches defaultExpr ->
      emitMultiIntCase scrutOps scrutName litBranches defaultExpr

    -- Single exhaustive constructor: emit field extraction + body inline
    SingleConCase _qn pats body -> do
      (fieldOps, _) <- emitPatternBindings scrutName "i64" pats
      (bodyOps, bodyName) <- emitExpr body
      pure (scrutOps ++ fieldOps ++ bodyOps, bodyName)

    -- Constructor case: extract tag and chain scf.if
    ConCase conBranches mDefaultExpr
      -- Koka Bool case: comparison results are i64 0/1, not boxed constructors
      | isBoolConCase conBranches ->
          emitBoolConCase scrutOps scrutName conBranches mDefaultExpr
      | otherwise ->
          emitConCase scrutOps scrutName conBranches mDefaultExpr

    -- PatVar: bind scrutinee to variable, emit body
    VarCase varName body -> do
      let varSsa = nameToSsa varName
      modify (\s -> s { esAliases = Map.insert varSsa scrutName (esAliases s) })
      (bodyOps, bodyName) <- emitExpr body
      let bindOp = "// let " <> sanitizeName (nameText varName) <> " = %" <> scrutName
      pure (scrutOps ++ [bindOp] ++ bodyOps, bodyName)

    -- Single branch wildcard/default
    SingleCase body -> do
      (bodyOps, bodyName) <- emitExpr body
      pure (scrutOps ++ bodyOps, bodyName)

    -- Two branches, first pattern is truthy test
    BoolCase thenExpr elseExpr ->
      emitIfElse scrutOps scrutName thenExpr elseExpr

    -- Char literal cases: convert to integer comparison
    CharLitCase charBranches defaultExpr ->
      emitMultiIntCase scrutOps scrutName
        [(toInteger (fromEnum c), e) | (c, e) <- charBranches] defaultExpr

    -- Fallback: emit unreachable to catch codegen bugs at runtime
    UnhandledCase -> do
      -- Don't emit llvm.unreachable here: this expression may be inside an
      -- scf.if/scf.for region whose terminator must be scf.yield. A trapping
      -- runtime call would be ideal but a 0 sentinel keeps blocks well-formed.
      name <- freshName "v"
      pure (scrutOps ++ ["// unhandled case with " <> T.pack (show (length branches)) <> " branches"
                         , "%" <> name <> " = arith.constant 0 : i64"], name)

emitExpr (ELet [binds] body) = do
  -- Save aliases so let-bindings don't leak out of this scope
  -- (e.g. into sibling scf.if branches).
  savedA <- gets esAliases
  savedTopFns <- gets esTopFns
  savedArity  <- gets esTopFnArity
  savedPromoted <- gets esPromotedFns
  savedCaptures <- gets esPromotedCaptures
  -- Promote recursive let-bound lambdas to top-level functions.
  -- GHC floats where-bound helpers (go, goBranch, etc.) to the module
  -- scope, but they remain as let-bindings in the Core IR. When these
  -- are recursive, the lambda-lifting pass can't capture the self/mutual
  -- references (the alias isn't registered yet). Fix: pre-register them
  -- as top-level functions so the body emits direct func.call for
  -- the recursion, and emitBindAsTopFn emits them as real func.func defs.
  modPfx <- gets esModulePrefix
  let (recBinds, plainBinds) = partition (isRecLetLambda modPfx) binds
  -- Pre-register the recursive binds in esTopFns/esTopFnArity/esPromotedFns.
  forM_ recBinds $ \bnd -> do
    let qualN = qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd)
        arity = countLamParams (bindExpr bnd)
        ssaKey = nameToSsa (Frankenstein.Core.Types.bindName bnd)
    modify (\s -> s { esTopFns      = Set.insert qualN (esTopFns s)
                    , esTopFnArity  = Map.insert qualN arity (esTopFnArity s)
                    , esPromotedFns = Map.insert ssaKey qualN (esPromotedFns s) })
  -- Pre-compute captures for all promoted binds (fixed-point iteration).
  precomputeCaptures modPfx recBinds
  -- Emit recursive binds as top-level func.func definitions.
  recOps <- concat <$> mapM (emitBindAsTopFn modPfx) recBinds
  -- Emit remaining binds normally.
  plainOps <- concat <$> mapM emitBind plainBinds
  (bodyOps, bodyName) <- emitExpr body
  modify (\s -> s { esAliases = savedA, esTopFns = savedTopFns
                  , esTopFnArity = savedArity, esPromotedFns = savedPromoted
                  , esPromotedCaptures = savedCaptures })
  pure (recOps ++ plainOps ++ bodyOps, bodyName)

emitExpr (ELet (bg:bgs) body) = do
  savedA <- gets esAliases
  savedTopFns <- gets esTopFns
  savedArity  <- gets esTopFnArity
  savedPromoted <- gets esPromotedFns
  savedCaptures <- gets esPromotedCaptures
  modPfx <- gets esModulePrefix
  let (recBinds, plainBinds) = partition (isRecLetLambda modPfx) bg
  forM_ recBinds $ \bnd -> do
    let qualN = qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd)
        arity = countLamParams (bindExpr bnd)
        ssaKey = nameToSsa (Frankenstein.Core.Types.bindName bnd)
    modify (\s -> s { esTopFns      = Set.insert qualN (esTopFns s)
                    , esTopFnArity  = Map.insert qualN arity (esTopFnArity s)
                    , esPromotedFns = Map.insert ssaKey qualN (esPromotedFns s) })
  precomputeCaptures modPfx recBinds
  recOps <- concat <$> mapM (emitBindAsTopFn modPfx) recBinds
  plainOps <- concat <$> mapM emitBind plainBinds
  (restOps, restName) <- emitExpr (ELet bgs body)
  modify (\s -> s { esAliases = savedA, esTopFns = savedTopFns
                  , esTopFnArity = savedArity, esPromotedFns = savedPromoted
                  , esPromotedCaptures = savedCaptures })
  pure (recOps ++ plainOps ++ restOps, restName)

emitExpr (ELet [] body) = emitExpr body

emitExpr (ELam params body) = do
  -- Lambda lifting with heap-allocated closures.
  --
  -- Closure ABI (all values are i64):
  --   Layout:   field 0 = function pointer (as i64), fields 1..n = captured vars
  --   Signature: lifted fn takes (closure, arg0, arg1, ...) -> i64
  --   Prologue:  lifted fn extracts captured fields from %closure via kk_field
  --   Call site: extract field 0 (fptr), cast to ptr, llvm.call %fptr(%closure, args)
  --
  -- This avoids MLIR SSA struct values leaking into i64 contexts (HOFs, lets).
  let bodyFree = freeVarsExpr body
      paramNames = Set.fromList (map fst params)
      candidateCaptures = Set.toList (bodyFree `Set.difference` paramNames)
  -- Only capture names that are actually in scope (aliased to an SSA value).
  -- Anything else is an external reference (top-level fn or unresolved import);
  -- it will be resolved at the reference site inside the lambda body.
  currentAliases <- gets esAliases
  topFns <- gets esTopFns
  promoted <- gets esPromotedFns
  promotedCaps <- gets esPromotedCaptures
  modPfx <- gets esModulePrefix
  let qualName n = let san = sanitizeName (nameText n)
                   in if T.any (== '/') (nameText n) then san else modPfx <> san
      isInScope n = let s = nameToSsa n
                    in Map.member s currentAliases
                       || Set.member (qualName n) topFns
                       || Map.member s promoted
      captured = filter (\n -> isInScope n
                            && not (Set.member (qualName n) topFns)
                            && not (Map.member (nameToSsa n) promoted))
                        candidateCaptures
      -- Also capture values needed by promoted function calls in the body.
      -- When the body calls a promoted fn that has captures, those capture
      -- SSA keys must be available in the lambda body. Add them as extra
      -- captures if they're aliased in the current scope.
      promotedRefs = filter (\n -> Map.member (nameToSsa n) promoted) candidateCaptures
      extraCapKeys = concatMap (\n -> case Map.lookup (nameToSsa n) promoted of
                                        Just pName -> Map.findWithDefault [] pName promotedCaps
                                        Nothing    -> []) promotedRefs
      capturedSsaKeys = Set.fromList (map nameToSsa captured)
      extraCaps = filter (\k -> not (Set.member k capturedSsaKeys)
                              && Map.member k currentAliases) extraCapKeys
      -- Deduplicate extra captures (SSA keys for promoted fn capture deps)
      extraCapsUniq = Set.toList (Set.fromList extraCaps)
      nCaptured = length captured + length extraCapsUniq
  liftedName <- freshName (modPfx <> "lambda")
  -- Allocate fresh SSA param names (closure + regular params).
  closFresh <- freshName "clos"
  paramFresh <- mapM (\_ -> freshName "p") params
  -- For each captured variable, allocate a fresh SSA name we'll bind in the prologue.
  capFresh <- mapM (\_ -> freshName "cap") captured
  extraCapFresh <- mapM (\_ -> freshName "cap") extraCapsUniq
  -- Save aliases; install body-local aliases (originals → fresh names).
  savedAliases <- gets esAliases
  let capAliases = zip (map nameToSsa captured) capFresh
      extraCapAliases = zip extraCapsUniq extraCapFresh
      paramAliases = zip (map (nameToSsa . fst) params) paramFresh
  modify (\s -> s { esAliases = foldr (\(k,v) m -> Map.insert k v m)
                                      (esAliases s)
                                      (capAliases ++ extraCapAliases ++ paramAliases) })
  -- Build prologue ops that extract captured fields from %closure.
  let allCapFresh = capFresh ++ extraCapFresh
      prologue = concat
        [ [ "%idx_" <> cfn <> " = arith.constant " <> T.pack (show i) <> " : i64"
          , "%" <> cfn <> " = func.call @kk_field(%" <> closFresh <> ", %idx_" <> cfn <> ") : (i64, i64) -> i64"
          ]
        | (i, cfn) <- zip [(1::Int)..] allCapFresh
        ]
  (bodyOps, bodyResult) <- emitExpr body
  -- Restore alias map (body-local aliases shouldn't leak out).
  modify (\s -> s { esAliases = savedAliases })
  -- Closure ABI: all regular params flow as i64 through the closure dispatch,
  -- matching the call site's assumption. Ignore per-param types.
  let regularParams = [ "%" <> fn <> ": i64"
                      | fn <- paramFresh ]
      allParams = ("%" <> closFresh <> ": i64") : regularParams
      mlirArgs = T.intercalate ", " allParams
      mlirRetTy = "i64"
      fnText = T.unlines $
        [ "  func.func @" <> liftedName <> "(" <> mlirArgs <> ") -> " <> mlirRetTy <> " {" ] ++
        map ("    " <>) (prologue ++ bodyOps) ++
        [ "    func.return %" <> bodyResult <> " : " <> mlirRetTy
        , "  }" ]
  addLiftedFn fnText
  -- Allocate the closure as a boxed heap value via kk_alloc_con.
  -- Field 0 = fptr, fields 1..n = captured values.
  let nFields = nCaptured + 1
  tagName <- freshName "v"
  nfieldsName <- freshName "v"
  ptrName <- freshName "v"
  fptrAddrName <- freshName "v"
  fptrPtrName <- freshName "v"
  fptrName <- freshName "v"
  idxZeroName <- freshName "v"
  -- Get the lifted fn's address as i64. Since @liftedName is a `func.func`
  -- (not an `llvm.func`), `llvm.mlir.addressof` would fail the verifier.
  -- Cast func -> !llvm.ptr -> i64 so reconcile-unrealized-casts can erase
  -- the intermediate after func-to-llvm rewrites the function.
  let paramTyList = T.intercalate ", " ("i64" : replicate (length params) "i64")
      fnCastTy = "(" <> paramTyList <> ") -> i64"
  let allocOps =
        [ "// closure for @" <> liftedName <> " capturing " <> T.pack (show nCaptured) <> " vars"
        , "%" <> tagName <> " = arith.constant 1129074515 : i64  // KK_CLOSURE_TAG 'CLOS'"
        , "%" <> nfieldsName <> " = arith.constant " <> T.pack (show nFields) <> " : i64"
        , "%" <> ptrName <> " = func.call @kk_alloc_con(%" <> tagName <> ", %" <> nfieldsName <> ") : (i64, i64) -> i64"
        , "%" <> fptrAddrName <> " = func.constant @" <> liftedName <> " : " <> fnCastTy
        , "%" <> fptrPtrName <> " = builtin.unrealized_conversion_cast %" <> fptrAddrName <> " : " <> fnCastTy <> " to !llvm.ptr"
        , "%" <> fptrName <> " = llvm.ptrtoint %" <> fptrPtrName <> " : !llvm.ptr to i64"
        , "%" <> idxZeroName <> " = arith.constant 0 : i64"
        , "func.call @kk_set_field(%" <> ptrName <> ", %" <> idxZeroName <> ", %" <> fptrName <> ") : (i64, i64, i64) -> ()"
        ]
  -- Store each captured variable at field index (1 + i).
  capturedNames <- mapM (\cn -> do
                            aliases <- gets esAliases
                            let sname = nameToSsa cn
                            pure (Map.findWithDefault sname sname aliases)) captured
  -- Resolve extra capture SSA keys from current aliases (before they're saved).
  let extraCapturedNames = map (\k -> Map.findWithDefault k k currentAliases) extraCapsUniq
      allCapturedNames = capturedNames ++ extraCapturedNames
  capSetOps <- mapM (\(i, cnName) -> do
    idxN <- freshName "v"
    pure [ "%" <> idxN <> " = arith.constant " <> T.pack (show i) <> " : i64"
         , "func.call @kk_set_field(%" <> ptrName <> ", %" <> idxN <> ", %" <> cnName <> ") : (i64, i64, i64) -> ()"
         ]) (zip [(1::Int)..] allCapturedNames)
  pure (allocOps ++ concat capSetOps, ptrName)

-- Perceus operations — emit real runtime calls
emitExpr (EDrop e) = do
  (eOps, eName) <- emitExpr e
  voidName <- freshName "v"
  pure (eOps ++
        [ "func.call @kk_drop(%" <> eName <> ") : (i64) -> ()"
        , "%" <> voidName <> " = arith.constant 0 : i64  // drop result"
        ], voidName)

emitExpr (ERetain e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++
        [ "func.call @kk_retain(%" <> eName <> ") : (i64) -> ()"
        ], eName)

emitExpr (ERelease e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++
        [ "func.call @kk_release(%" <> eName <> ") : (i64) -> ()"
        ], eName)

emitExpr (EReuse ref alloc) = do
  (refOps, refName) <- emitExpr ref
  (allocOps, _allocName) <- emitExpr alloc
  resultName <- freshName "v"
  pure (refOps ++ allocOps ++
        [ "%" <> resultName <> " = func.call @kk_reuse(%" <> refName <> ") : (i64) -> i64"
        ], resultName)

-- Laziness (thunks) — lambda-lift the delayed expression and call kk_thunk_create
-- The lifted body is a regular func.func. We use func.constant + index_cast to
-- get a function pointer as i64, avoiding llvm.mlir.addressof incompatibility.
emitExpr (EDelay e) = do
  -- Lambda-lift e to a zero-arg function.
  -- If the body has free variables that would have to be captured,
  -- we'd need a closure-carrying thunk (distinct runtime ABI); for now,
  -- fall back to eager evaluation by inlining the body into the caller.
  -- This is semantically incorrect only for programs that rely on
  -- laziness for termination — GHC's demand analyzer removes most such
  -- thunks before we see them.
  let bodyFree = freeVarsExpr e
  currentAliases <- gets esAliases
  topFns <- gets esTopFns
  modPfx <- gets esModulePrefix
  let qualName n = let san = sanitizeName (nameText n)
                   in if T.any (== '/') (nameText n) then san else modPfx <> san
      isCaptured n = let s = nameToSsa n
                     in Map.member s currentAliases
                        && not (Set.member (qualName n) topFns)
      hasCaptures = any isCaptured (Set.toList bodyFree)
  if hasCaptures
    then do
      -- Inline the body; wrap the result in a fake "already-forced" thunk
      -- so EForce downstream still works. The simplest shim: emit the body
      -- directly and return its value. EForce will call kk_thunk_force on
      -- a plain i64, which is incorrect runtime-wise but at least parses.
      (eOps, eName) <- emitExpr e
      pure ( ("// degraded thunk (had captures): inlined body" : eOps)
           , eName)
    else do
      modPfx <- gets esModulePrefix
      liftedName <- freshName (modPfx <> "thunk_body")
      let mlirRetTy = "i64"
      (bodyOps, bodyResult) <- emitExpr e
      let fnText = T.unlines $
            [ "  func.func @" <> liftedName <> "() -> " <> mlirRetTy <> " {" ] ++
            map ("    " <>) bodyOps ++
            [ "    func.return %" <> bodyResult <> " : " <> mlirRetTy
            , "  }" ]
      addLiftedFn fnText
      addrName <- freshName "v"
      ptrName <- freshName "v"
      fptrName <- freshName "v"
      resultName <- freshName "v"
      pure ([ "// delay (thunk) -> @" <> liftedName
            , "%" <> addrName <> " = func.constant @" <> liftedName <> " : () -> i64"
            , "%" <> ptrName <> " = builtin.unrealized_conversion_cast %" <> addrName <> " : () -> i64 to !llvm.ptr"
            , "%" <> fptrName <> " = llvm.ptrtoint %" <> ptrName <> " : !llvm.ptr to i64"
            , "%" <> resultName <> " = func.call @kk_thunk_create(%" <> fptrName <> ") : (i64) -> i64"
            ], resultName)

emitExpr (EForce e) = do
  (eOps, eName) <- emitExpr e
  resultName <- freshName "v"
  pure (eOps ++
        [ "%" <> resultName <> " = func.call @kk_thunk_force(%" <> eName <> ") : (i64) -> i64"
        ], resultName)

-- Type application / abstraction: pass through to inner expr
emitExpr (ETypeApp e _) = emitExpr e
emitExpr (ETypeLam _ e) = emitExpr e

-- Function reference: get a function pointer as i64
-- Uses func.constant to get a reference to a func.func-defined function,
-- then converts to i64 via an intermediate unrealized_conversion_cast.
emitExpr (EFunRef qn) = do
  let fname = sanitizeName (nameText (qnameName qn))
      qualName = if T.null (qnameModule qn) then fname
                 else sanitizeName (qnameModule qn) <> "_" <> fname
  refName <- freshName "v"
  fptrName <- freshName "v"
  -- func.constant produces a value of function type (() -> i64)
  -- We cast it to !llvm.ptr then to i64 to pass as a regular argument.
  ptrName <- freshName "v"
  pure ([ "// funref @" <> qualName
        , "%" <> refName <> " = func.constant @" <> qualName <> " : () -> i64"
        , "%" <> ptrName <> " = builtin.unrealized_conversion_cast %" <> refName <> " : () -> i64 to !llvm.ptr"
        , "%" <> fptrName <> " = llvm.ptrtoint %" <> ptrName <> " : !llvm.ptr to i64"
        ], fptrName)

-- Effect operations: after the evidence pass, EPerform/EHandle should be
-- desugared to plain ELet/EApp. These cases handle any residual nodes
-- (e.g. if the evidence pass didn't fully desugar).

emitExpr (EPerform qn args) = do
  effectMode <- gets esEffectDialect
  if effectMode
    then do
      -- Effect dialect mode: emit frankenstein.perform
      argResults <- mapM emitExpr args
      let allOps = concatMap fst argResults
          argNames = map snd argResults
          effName = qnameModule qn
          opName' = nameText (qnameName qn)
      resultName <- freshName "v"
      let dialectOp = "%" <> resultName <> " = "
            <> renderOp (FrankPerform effName opName' argNames)
      pure (allOps ++ [dialectOp], resultName)
    else do
      -- Lowered mode: evidence-passing indirect calls
      let effName = qnameModule qn
      evScope <- gets esEvidenceScope
      argResults <- mapM emitExpr args
      let allOps = concatMap fst argResults
          argNames = map snd argResults
      case Map.lookup effName evScope of
        Just evVarName -> do
          fptrName <- freshName "v"
          resultName <- freshName "v"
          let argList = T.intercalate ", " ["%" <> n | n <- argNames]
              argTys = T.intercalate ", " (replicate (length argNames) "i64")
              ops = [ "// perform " <> nameText (qnameName qn) <> " via evidence"
                    , "%" <> fptrName <> " = llvm.inttoptr %" <> evVarName <> " : i64 to !llvm.ptr"
                    , "%" <> resultName <> " = llvm.call %" <> fptrName
                        <> "(" <> argList <> ") : !llvm.ptr, (" <> argTys <> ") -> i64"
                    ]
          pure (allOps ++ ops, resultName)
        Nothing -> do
          resultName <- freshName "v"
          pure (allOps ++
                [ "// perform " <> nameText (qnameName qn) <> " -- no handler in scope"
                , "%" <> resultName <> " = func.call @kk_unhandled_effect() : () -> i64"
                ], resultName)

emitExpr (EHandle effRow handler body) = do
  effectMode <- gets esEffectDialect
  if effectMode
    then do
      -- Effect dialect mode: emit frankenstein.handle
      let effName = effectRowNameEmit effRow
      (handlerOps, handlerName) <- emitExpr handler
      -- Emit body with effect dialect (performs become frankenstein.perform)
      (bodyOps, bodyName) <- emitExpr body
      resultName <- freshName "v"
      let dialectOps =
            [ "// frankenstein.handle @" <> effName
            , renderOp (FrankHandle effName handlerName bodyName)
            , "%" <> resultName <> " = arith.constant 0 : i64  // handle result placeholder"
            ]
      -- The body result is the handle result
      pure (handlerOps ++ bodyOps ++ dialectOps, bodyName)
    else do
      -- Lowered mode: install evidence, emit body, restore scope
      (handlerOps, handlerName) <- emitExpr handler
      let effName = effectRowNameEmit effRow
      oldScope <- gets esEvidenceScope
      modify (\s -> s { esEvidenceScope = Map.insert effName handlerName (esEvidenceScope s) })
      (bodyOps, bodyName) <- emitExpr body
      modify (\s -> s { esEvidenceScope = oldScope })
      pure (handlerOps ++ bodyOps, bodyName)

-- Catch-all removed: all Expr constructors are handled above

-- Helpers

emitBinOp :: Text -> Text -> Expr -> Expr -> Emit ([Text], Text)
emitBinOp op ty a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  resultName <- freshName "v"
  recordType resultName ty
  let binOp = "%" <> resultName <> " = " <> op <> " %" <> aName <> ", %" <> bName <> " : " <> ty
  pure (aOps ++ bOps ++ [binOp], resultName)

emitCmpOp :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitCmpOp pred' a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  -- Use the tracked type of the left operand for the comparison
  aTy <- lookupType aName
  cmpName <- freshName "cmp"
  resultName <- freshName "v"
  let cmpOp = "%" <> cmpName <> " = arith.cmpi " <> pred' <> ", %" <> aName <> ", %" <> bName <> " : " <> aTy
      extOp = "%" <> resultName <> " = arith.extui %" <> cmpName <> " : i1 to i64"
  pure (aOps ++ bOps ++ [cmpOp, extOp], resultName)

emitFloatCmpOp :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitFloatCmpOp pred' a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  cmpName <- freshName "cmp"
  resultName <- freshName "v"
  let cmpOp = "%" <> cmpName <> " = arith.cmpf " <> pred' <> ", %" <> aName <> ", %" <> bName <> " : f64"
      extOp = "%" <> resultName <> " = arith.extui %" <> cmpName <> " : i1 to i64"
  pure (aOps ++ bOps ++ [cmpOp, extOp], resultName)

emitIntCase :: [Text] -> Text -> Integer -> Expr -> Expr -> Emit ([Text], Text)
emitIntCase scrutOps scrutName litVal thenExpr elseExpr = do
  -- Compare scrutinee to literal value
  zeroName <- freshName "v"
  cmpName2 <- freshName "cmp"
  let cmpOps = [ "%" <> zeroName <> " = arith.constant " <> T.pack (show litVal) <> " : i64"
               , "%" <> cmpName2 <> " = arith.cmpi eq, %" <> scrutName <> ", %" <> zeroName <> " : i64"
               ]
  emitScfIf (scrutOps ++ cmpOps) cmpName2 thenExpr elseExpr

-- | Emit a multi-way integer literal case as a chain of scf.if/else
emitMultiIntCase :: [Text] -> Text -> [(Integer, Expr)] -> Expr -> Emit ([Text], Text)
emitMultiIntCase scrutOps _scrutName [] defaultExpr = do
  (defOps, defName') <- emitExpr defaultExpr
  pure (scrutOps ++ defOps, defName')
emitMultiIntCase scrutOps scrutName [(litVal, body)] defaultExpr =
  emitIntCase scrutOps scrutName litVal body defaultExpr
emitMultiIntCase scrutOps scrutName ((litVal, body):rest) defaultExpr = do
  -- Compare to this literal
  constName <- freshName "v"
  cmpName <- freshName "cmp"
  let cmpOps = [ "%" <> constName <> " = arith.constant " <> T.pack (show litVal) <> " : i64"
               , "%" <> cmpName <> " = arith.cmpi eq, %" <> scrutName <> ", %" <> constName <> " : i64"
               ]
  -- Then branch: this literal matched
  (thenOps, thenResult) <- emitExpr body
  -- Else branch: recurse on remaining branches
  -- We need to wrap the rest in an scf.if chain
  (restOps, restResult) <- emitMultiIntCase [] scrutName rest defaultExpr
  resultName <- freshName "v"
  let ifOps =
        [ "%" <> resultName <> " = scf.if %" <> cmpName <> " -> i64 {" ] ++
        map ("  " <>) thenOps ++
        [ "  scf.yield %" <> thenResult <> " : i64"
        , "} else {"
        ] ++
        map ("  " <>) restOps ++
        [ "  scf.yield %" <> restResult <> " : i64"
        , "}"
        ]
  pure (scrutOps ++ cmpOps ++ ifOps, resultName)

-- | Detect Koka Bool constructor case (True/False branches)
isBoolConCase :: [(QName, [Pattern], Expr)] -> Bool
isBoolConCase branches =
  let names = [nameText (qnameName qn) | (qn, _, _) <- branches]
  in any (`elem` ["True", "False"]) names

-- | Emit a Bool constructor case: scrutinee is i64 0 (False) or 1 (True),
--   not a boxed constructor, so skip kk_tag and compare directly.
emitBoolConCase :: [Text] -> Text -> [(QName, [Pattern], Expr)] -> Maybe Expr -> Emit ([Text], Text)
emitBoolConCase scrutOps scrutName conBranches mDefault = do
  -- Find which branch is True and which is False
  let findBranch nm = [e | (qn, _, e) <- conBranches, nameText (qnameName qn) == nm]
      fallback = case mDefault of
        Just e  -> e
        Nothing -> case conBranches of
          []            -> ELit (LitInt 0)
          ((_,_,e) : _) -> e
      trueBranch  = case findBranch "True"  of { (e:_) -> e; [] -> fallback }
      falseBranch = case findBranch "False" of { (e:_) -> e; [] -> fallback }
  -- scrutName is i64: 1 = True, 0 = False. Compare != 0 to get i1.
  zeroName <- freshName "v"
  cmpName <- freshName "cmp"
  let toI1 = [ "%" <> zeroName <> " = arith.constant 0 : i64"
             , "%" <> cmpName <> " = arith.cmpi ne, %" <> scrutName <> ", %" <> zeroName <> " : i64"
             ]
  emitScfIf (scrutOps ++ toI1) cmpName trueBranch falseBranch

-- | Emit constructor case: extract tag via kk_tag, chain scf.if on tag values
emitConCase :: [Text] -> Text -> [(QName, [Pattern], Expr)] -> Maybe Expr -> Emit ([Text], Text)
emitConCase scrutOps scrutName conBranches mDefaultExpr = do
  -- Extract tag from the scrutinee via runtime call
  tagName <- freshName "v"
  let extractTag = "%" <> tagName <> " = func.call @kk_tag(%" <> scrutName <> ") : (i64) -> i64"
      structTy = "i64"  -- not used for extraction anymore, kept for API compat
  -- Build chain of comparisons
  (chainOps, chainResult) <- emitConChain tagName scrutName structTy conBranches mDefaultExpr
  pure (scrutOps ++ [extractTag] ++ chainOps, chainResult)

emitConChain
  :: Text -> Text -> Text
  -> [(QName, [Pattern], Expr)]
  -> Maybe Expr
  -> Emit ([Text], Text)
emitConChain _ _ _ [] (Just defaultExpr) = emitExpr defaultExpr
emitConChain _ _ _ [] Nothing            =
  -- No branches and no default: emit a zero placeholder. This should
  -- only be reachable in degenerate (empty) cases.
  pure (["// warning: empty case with no default"
       , "%vzero = arith.constant 0 : i64"], "vzero")
-- Exhaustive tail: one remaining constructor branch with no default
-- means \"treat this branch as unconditional\" — skip the tag test and
-- emit the pattern bindings + body in the outer region so the pattern
-- variables stay in scope.
emitConChain _ scrutName structTy [(_qn, pats, body)] Nothing = do
  savedA <- gets esAliases
  (fieldOps, _) <- emitPatternBindings scrutName structTy pats
  (bodyOps, bodyResult) <- emitExpr body
  modify (\s -> s { esAliases = savedA })
  pure (fieldOps ++ bodyOps, bodyResult)
emitConChain tagName scrutName structTy [(qn, pats, body)] (Just defaultExpr) = do
  -- Last constructor branch: compare tag, if match do body, else default
  tag <- lookupConTag qn
  constName <- freshName "v"
  cmpName <- freshName "cmp"
  let cmpOps = [ "%" <> constName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
               , "%" <> cmpName <> " = arith.cmpi eq, %" <> tagName <> ", %" <> constName <> " : i64"
               ]
  -- Extract fields for pattern variables. Save/restore aliases around each
  -- branch: pattern-bound names are scoped to their branch's scf.if region,
  -- so leaking them into the outer alias map causes cross-region SSA refs.
  savedA <- gets esAliases
  (fieldOps, _) <- emitPatternBindings scrutName structTy pats
  (thenOps, thenResult) <- emitExpr body
  modify (\s -> s { esAliases = savedA })
  (elseOps, elseResult) <- emitExpr defaultExpr
  modify (\s -> s { esAliases = savedA })
  resultName <- freshName "v"
  let ifOps =
        [ "%" <> resultName <> " = scf.if %" <> cmpName <> " -> i64 {" ] ++
        map ("  " <>) (fieldOps ++ thenOps) ++
        [ "  scf.yield %" <> thenResult <> " : i64"
        , "} else {"
        ] ++
        map ("  " <>) elseOps ++
        [ "  scf.yield %" <> elseResult <> " : i64"
        , "}"
        ]
  pure (cmpOps ++ ifOps, resultName)
emitConChain tagName scrutName structTy ((qn, pats, body):rest) mDefaultExpr = do
  tag <- lookupConTag qn
  constName <- freshName "v"
  cmpName <- freshName "cmp"
  let cmpOps = [ "%" <> constName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
               , "%" <> cmpName <> " = arith.cmpi eq, %" <> tagName <> ", %" <> constName <> " : i64"
               ]
  savedA <- gets esAliases
  (fieldOps, _) <- emitPatternBindings scrutName structTy pats
  (thenOps, thenResult) <- emitExpr body
  modify (\s -> s { esAliases = savedA })
  (restOps, restResult) <- emitConChain tagName scrutName structTy rest mDefaultExpr
  modify (\s -> s { esAliases = savedA })
  resultName <- freshName "v"
  let ifOps =
        [ "%" <> resultName <> " = scf.if %" <> cmpName <> " -> i64 {" ] ++
        map ("  " <>) (fieldOps ++ thenOps) ++
        [ "  scf.yield %" <> thenResult <> " : i64"
        , "} else {"
        ] ++
        map ("  " <>) restOps ++
        [ "  scf.yield %" <> restResult <> " : i64"
        , "}"
        ]
  pure (cmpOps ++ ifOps, resultName)

-- | Extract fields from a constructor struct and bind pattern variables
emitPatternBindings :: Text -> Text -> [Pattern] -> Emit ([Text], [Text])
emitPatternBindings scrutName structTy pats = do
  opsAndNames <- mapM (emitPatField scrutName structTy) (zip [1..] pats)
  let allOps = concatMap fst opsAndNames
      allNames = map snd opsAndNames
  pure (allOps, allNames)

emitPatField :: Text -> Text -> (Int, Pattern) -> Emit ([Text], Text)
emitPatField scrutName _structTy (idx, PatVar n _) = do
  let varName = nameToSsa n
  -- Fields are 0-indexed from kk_field's perspective.
  -- Pattern index 1 corresponds to field 0 (index 0 is the tag in the old scheme).
  let fieldIdx = idx - 1
  idxName <- freshName "v"
  fieldName <- freshName "v"
  let extractOps = [ "%" <> idxName <> " = arith.constant " <> T.pack (show fieldIdx) <> " : i64"
                   , "%" <> fieldName <> " = func.call @kk_field(%" <> scrutName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
                   ]
      aliasOp = "// let " <> varName <> " = %" <> fieldName
  -- Register alias so subsequent EVar references resolve to the field SSA value
  modify (\s -> s { esAliases = Map.insert varName fieldName (esAliases s) })
  pure (extractOps ++ [aliasOp], fieldName)
emitPatField _ _ (_, PatWild _) = do
  name <- freshName "v"
  pure (["// wildcard field ignored"], name)
emitPatField scrutName _structTy (idx, _) = do
  let fieldIdx = idx - 1
  idxName <- freshName "v"
  fieldName <- freshName "v"
  let extractOps = [ "%" <> idxName <> " = arith.constant " <> T.pack (show fieldIdx) <> " : i64"
                   , "%" <> fieldName <> " = func.call @kk_field(%" <> scrutName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
                   ]
  pure (extractOps, fieldName)

emitIfElse :: [Text] -> Text -> Expr -> Expr -> Emit ([Text], Text)
emitIfElse condOps condName thenExpr elseExpr = do
  -- condName should be i1; if it's i64, compare != 0
  zeroName <- freshName "v"
  cmpName <- freshName "cmp"
  let toI1 = [ "%" <> zeroName <> " = arith.constant 0 : i64"
             , "%" <> cmpName <> " = arith.cmpi ne, %" <> condName <> ", %" <> zeroName <> " : i64"
             ]
  emitScfIf (condOps ++ toI1) cmpName thenExpr elseExpr

emitScfIf :: [Text] -> Text -> Expr -> Expr -> Emit ([Text], Text)
emitScfIf preOps condName thenExpr elseExpr = do
  resultName <- freshName "v"
  (thenOps, thenResult) <- emitExpr thenExpr
  (elseOps, elseResult) <- emitExpr elseExpr
  let ifOps =
        [ "%" <> resultName <> " = scf.if %" <> condName <> " -> i64 {" ] ++
        map ("  " <>) thenOps ++
        [ "  scf.yield %" <> thenResult <> " : i64"
        , "} else {"
        ] ++
        map ("  " <>) elseOps ++
        [ "  scf.yield %" <> elseResult <> " : i64"
        , "}"
        ]
  pure (preOps ++ ifOps, resultName)

emitBind :: Bind -> Emit [Text]
emitBind bnd = do
  (ops, resultName) <- emitExpr (bindExpr bnd)
  let bname = nameToSsa (Frankenstein.Core.Types.bindName bnd)
  if bname == resultName
    then pure ops
    else do
      -- Register alias so subsequent references to bname resolve to resultName
      modify (\s -> s { esAliases = Map.insert bname resultName (esAliases s) })
      pure $ ops ++ ["// let " <> bname <> " = %" <> resultName]

-- | Pre-compute captures for all promoted binds in a Rec group.
-- We iterate to a fixed point because transitive captures (A calls B,
-- B's captures must also be A's captures) require multiple passes when
-- the processing order doesn't match the dependency order.
precomputeCaptures :: Text -> [Bind] -> Emit ()
precomputeCaptures modPfx recBinds = do
  currentAliases <- gets esAliases
  topFns <- gets esTopFns
  promoted <- gets esPromotedFns
  priorCaps <- gets esPromotedCaptures
  let qualName n = let san = sanitizeName (nameText n)
                   in if T.any (== '/') (nameText n) then san else modPfx <> san
      isInScope' n = let s = nameToSsa n
                     in Map.member s currentAliases
                        || Set.member (qualName n) topFns
                        || Map.member s promoted
      -- For each bind, compute direct captures (free vars that are in scope
      -- but NOT top-level fns and NOT other promoted fns).
      bindInfo = [ (qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd),
                    bnd, directCaps, promotedRefs', paramSsaNames)
                 | bnd <- recBinds
                 , let ELam params body = unwrapLambda (bindExpr bnd)
                       bodyFree = freeVarsExpr body
                       paramNames = Set.fromList (map fst params)
                       paramSsaNames = Set.fromList (map (nameToSsa . fst) params)
                       candidates = Set.toList (bodyFree `Set.difference` paramNames)
                       directCaps = filter (\n -> isInScope' n
                                               && not (Set.member (qualName n) topFns)
                                               && not (Map.member (nameToSsa n) promoted))
                                           candidates
                       promotedRefs' = filter (\n -> Map.member (nameToSsa n) promoted) candidates
                 ]
      -- Iterate: resolve transitive captures until the map stabilizes.
      -- Look up transitive captures in both the current iteration's accumulator
      -- AND the global esPromotedCaptures (for promoted fns from earlier binding groups).
      iterate' caps =
        let caps' = foldl (\acc (qualN, _bnd, directCaps, promotedRefs', paramSsas) ->
                      let directKeys = map nameToSsa directCaps
                          -- Transitive: for each promoted fn we call, include its captures.
                          -- Check acc first (current group), then priorCaps (earlier groups).
                          extraKeys = concatMap (\n -> case Map.lookup (nameToSsa n) promoted of
                                                         Just pName -> case Map.lookup pName acc of
                                                           Just ks -> ks
                                                           Nothing -> Map.findWithDefault [] pName priorCaps
                                                         Nothing    -> []) promotedRefs'
                          directSet = Set.fromList directKeys
                          -- Exclude keys that collide with this function's lambda params
                          -- (they'd cause "region entry argument already in use").
                          extras = filter (\k -> not (Set.member k directSet)
                                              && not (Set.member k paramSsas)
                                              && Map.member k currentAliases) extraKeys
                          allKeys = directKeys ++ Set.toList (Set.fromList extras)
                      in Map.insert qualN allKeys acc) caps bindInfo
        in if caps' == caps then caps else iterate' caps'
      finalCaps = iterate' Map.empty
  -- Register captures and update arities.
  forM_ bindInfo $ \(qualN, bnd, _directCaps, _promotedRefs', _paramSsas) -> do
    let ELam params _body = unwrapLambda (bindExpr bnd)
        capKeys = Map.findWithDefault [] qualN finalCaps
        totalArity = length capKeys + length params
    modify (\s -> s { esTopFnArity       = Map.insert qualN totalArity (esTopFnArity s)
                    , esPromotedCaptures = Map.insert qualN capKeys (esPromotedCaptures s) })

-- | Emit a let-bound lambda as a top-level func.func definition.
-- Used for recursive where-bound helpers (go, goBranch, etc.) that GHC
-- floated to the module scope. These are pre-registered in esTopFns so
-- recursive self/mutual references resolve to direct func.call.
-- Captures are pre-computed by precomputeCaptures.
emitBindAsTopFn :: Text -> Bind -> Emit [Text]
emitBindAsTopFn modPfx bnd = do
  let qualN = qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd)
  case unwrapLambda (bindExpr bnd) of
    ELam params body -> do
      -- Captures were pre-computed by precomputeCaptures.
      capSsaKeys <- Map.findWithDefault [] qualN <$> gets esPromotedCaptures
      -- Build MLIR parameter list: captures first, then regular params.
      let capArgs = [ "%" <> k <> ": i64" | k <- capSsaKeys ]
          paramArgs = [ "%" <> nameToSsa pn <> ": i64" | (pn, _) <- params ]
          mlirArgs = T.intercalate ", " (capArgs ++ paramArgs)
          mlirRetTy = "i64"
      -- Install identity aliases for captures + parameters.
      savedA <- gets esAliases
      let capAliases = [ (k, k) | k <- capSsaKeys ]
          paramAliases = [ (nameToSsa pn, nameToSsa pn) | (pn, _) <- params ]
      modify (\s -> s { esAliases = foldr (\(k,v) m -> Map.insert k v m)
                                          (esAliases s) (capAliases ++ paramAliases) })
      bodyText <- emitBody body mlirRetTy
      modify (\s -> s { esAliases = savedA })
      -- Emit as a lifted function (appended to esLiftedFns).
      addLiftedFn $ T.unlines
        [ "  func.func @" <> qualN <> "(" <> mlirArgs <> ") -> " <> mlirRetTy <> " {"
        , bodyText
        , "  }"
        ]
      pure ["// rec let " <> qualN <> " promoted to top-level"]
    _ -> do
      -- Shouldn't happen (isRecLetLambda guards), fall back to normal emitBind.
      emitBind bnd

-------------------------------------------------------------------------------
-- Branch classification
-------------------------------------------------------------------------------

-- | Classify case branches for dispatch
data BranchClass
  = IntLitCase Integer Expr Expr          -- single int literal + default
  | MultiIntLitCase [(Integer, Expr)] Expr -- multiple int literals + default
  | ConCase [(QName, [Pattern], Expr)] (Maybe Expr)
    -- ^ Constructor patterns + optional default. When the default is
    -- @Nothing@ we assume the branch set is exhaustive (the frontend
    -- has already checked) and the last constructor branch becomes
    -- unconditional during lowering.
  | SingleConCase QName [Pattern] Expr     -- single constructor (exhaustive, no default needed)
  | VarCase Name Expr                      -- single variable binding
  | SingleCase Expr                        -- single branch (wildcard or sole)
  | BoolCase Expr Expr                     -- two branches, truthy test
  | CharLitCase [(Char, Expr)] Expr        -- char literal patterns + default
  | UnhandledCase

classifyBranches :: [Branch] -> BranchClass
classifyBranches [Branch (PatLit (LitInt n)) _ thenExpr, Branch _ _ elseExpr] =
  IntLitCase n thenExpr elseExpr
classifyBranches branches
  -- All PatLit with optional default
  | allIntLits, length intLitBranches >= 2 =
      let defaultBody = case defaultBranch of
            Just b  -> branchBody b
            Nothing -> branchBody (last branches)  -- last branch as default
          litPairs = [(n, branchBody b) | b <- intLitBranches, PatLit (LitInt n) <- [branchPattern b]]
      in MultiIntLitCase litPairs defaultBody
  -- Single constructor branch with no real default → exhaustive, no scf.if
  | [b] <- conBranches, Nothing <- defaultBranch, length branches == 1
  , PatCon qn pats <- branchPattern b =
      SingleConCase qn pats (branchBody b)
  -- Constructor patterns.
  | not (null conBranches) =
      let conData = [(qn, pats, branchBody b)
                    | b <- conBranches
                    , PatCon qn pats <- [branchPattern b] ]
      in case defaultBranch of
           Just b  -> ConCase conData (Just (branchBody b))
           -- No explicit default: treat the branch set as exhaustive.
           -- The last constructor branch becomes unconditional; its own
           -- pattern-bound variables stay in scope because we emit its
           -- bindings and body together rather than reusing the body as
           -- a separate default expression in an inner scf.if region.
           Nothing -> ConCase conData Nothing
  where
    intLitBranches = [b | b <- branches, isIntLit (branchPattern b)]
    conBranches = [b | b <- branches, isConPat (branchPattern b)]
    defaultBranch = case [b | b <- branches, isDefaultPat (branchPattern b)] of
                      (b:_) -> Just b
                      []    -> Nothing
    allIntLits = all (\b -> isIntLit (branchPattern b) || isDefaultPat (branchPattern b)) branches
                 && not (null intLitBranches)

-- Char literal patterns
classifyBranches branches
  | not (null charBranches)
  , all (\b -> isCharLit (branchPattern b) || isDefaultPat (branchPattern b)) branches =
      let defaultBody = case [b | b <- branches, isDefaultPat (branchPattern b)] of
            (b:_) -> branchBody b
            []    -> branchBody (last branches)
          charPairs = [(c, branchBody b) | b <- charBranches, PatLit (LitChar c) <- [branchPattern b]]
      in CharLitCase charPairs defaultBody
  where
    charBranches = [b | b <- branches, isCharLit (branchPattern b)]

classifyBranches [Branch (PatVar n _) _ body] = VarCase n body
classifyBranches [Branch _ _ body] = SingleCase body
classifyBranches [Branch _ _ thenExpr, Branch _ _ elseExpr] = BoolCase thenExpr elseExpr
classifyBranches _ = UnhandledCase

isIntLit :: Pattern -> Bool
isIntLit (PatLit (LitInt _)) = True
isIntLit _ = False

isCharLit :: Pattern -> Bool
isCharLit (PatLit (LitChar _)) = True
isCharLit _ = False

isConPat :: Pattern -> Bool
isConPat (PatCon _ _) = True
isConPat _ = False

isDefaultPat :: Pattern -> Bool
isDefaultPat (PatWild _) = True
isDefaultPat (PatVar _ _) = True
isDefaultPat _ = False

-------------------------------------------------------------------------------
-- Constructor helpers
-------------------------------------------------------------------------------

-- | Deterministic tag for a constructor.
--
-- The tag table is populated up-front by 'assignProgramTags' from
-- "Frankenstein.Core.ConTags": every constructor reachable from
-- 'progData' or from any 'ECon' / 'PatCon' node in 'progDefs' has
-- an entry. Declared ctors get their intra-'DataDecl' index
-- (0..n-1); orphan ctors (referenced but not declared) get fresh
-- tags starting after the largest declared tag. A miss here
-- therefore indicates a compiler bug — a constructor that appeared
-- after tag assignment — so we surface it as a very visible
-- sentinel rather than silently hashing.
lookupConTag :: QName -> Emit Int
lookupConTag qn = do
  tbl <- gets esConTags
  case Map.lookup (conKey qn) tbl of
    Just t  -> pure t
    Nothing -> pure (-1)   -- sentinel: forces failure in downstream dispatch

-- | LLVM struct type for a constructor with n payload fields
-- Layout: (i64_tag, i64_field1, i64_field2, ...)
conStructType :: Int -> Text
conStructType 0 = "!llvm.struct<(i64)>"
conStructType n = "!llvm.struct<(i64" <> T.concat (replicate n ", i64") <> ")>"

-- | Closure struct type: (i64_fptr, i64_cap1, i64_cap2, ...)
closureStructType :: Int -> Text
closureStructType nCaptured = "!llvm.struct<(i64" <> T.concat (replicate nCaptured ", i64") <> ")>"

-- | Fold over fields, inserting each into a struct at increasing positions
foldInsertFields :: Text -> Text -> [Text] -> Int -> Emit ([Text], Text)
foldInsertFields currentName _ [] _ = pure ([], currentName)
foldInsertFields currentName structTy (fieldName:rest) idx = do
  nextName <- freshName "v"
  -- MLIR llvm.insertvalue: %out = llvm.insertvalue %value, %container[idx] : !type
  let op = "%" <> nextName <> " = llvm.insertvalue %" <> fieldName <> ", %" <> currentName <> "[" <> T.pack (show idx) <> "] : " <> structTy
  (restOps, finalName) <- foldInsertFields nextName structTy rest (idx + 1)
  pure (op : restOps, finalName)

-------------------------------------------------------------------------------
-- Free variable analysis (for closure conversion)
-------------------------------------------------------------------------------

freeVarsExpr :: Expr -> Set Name
freeVarsExpr (EVar n)         = Set.singleton n
freeVarsExpr (ELit _)         = Set.empty
freeVarsExpr (ECon _)         = Set.empty
freeVarsExpr (EApp f args)    = Set.unions (freeVarsExpr f : map freeVarsExpr args)
freeVarsExpr (ELam ps body)   = freeVarsExpr body `Set.difference` Set.fromList (map fst ps)
freeVarsExpr (ELet bgs body)  =
  let bound = Set.fromList [Frankenstein.Core.Types.bindName b | bg <- bgs, b <- bg]
      bindFvs = Set.unions [freeVarsExpr (bindExpr b) | bg <- bgs, b <- bg]
  in (bindFvs `Set.union` freeVarsExpr body) `Set.difference` bound
freeVarsExpr (ECase s brs)    = Set.unions (freeVarsExpr s : map brFreeVars brs)
freeVarsExpr (ERetain e)      = freeVarsExpr e
freeVarsExpr (ERelease e)     = freeVarsExpr e
freeVarsExpr (EDrop e)        = freeVarsExpr e
freeVarsExpr (EReuse a b)     = freeVarsExpr a `Set.union` freeVarsExpr b
freeVarsExpr (EDelay e)       = freeVarsExpr e
freeVarsExpr (EForce e)       = freeVarsExpr e
freeVarsExpr (ETypeApp e _)   = freeVarsExpr e
freeVarsExpr (ETypeLam _ e)   = freeVarsExpr e
freeVarsExpr (EPerform _ args) = Set.unions (map freeVarsExpr args)
freeVarsExpr (EHandle _ h b)  = freeVarsExpr h `Set.union` freeVarsExpr b
freeVarsExpr (EFunRef _)      = Set.empty

brFreeVars :: Branch -> Set Name
brFreeVars br =
  let patBound = Set.fromList (map fst (patVars (branchPattern br)))
      guardFvs = maybe Set.empty freeVarsExpr (branchGuard br)
  in (freeVarsExpr (branchBody br) `Set.union` guardFvs) `Set.difference` patBound

patVars :: Pattern -> [(Name, Type)]
patVars (PatVar n t)    = [(n, t)]
patVars (PatCon _ pats) = concatMap patVars pats
patVars (PatWild _)     = []
patVars (PatLit _)      = []

-------------------------------------------------------------------------------
-- Type mapping
-------------------------------------------------------------------------------

-- Type decomposition
decomposeDefType :: Type -> ([Text], [Type], Type)
decomposeDefType (TFun args _eff ret) =
  ( [T.pack ("arg" ++ show i) | i <- [0..length args - 1]]
  , map snd args
  , ret )
decomposeDefType (TForall _ body) = decomposeDefType body
decomposeDefType t = ([], [], t)

-- | Map Core types to MLIR type strings
typeToMlir :: Type -> Text
typeToMlir (TCon tc)
  | n == "int" || n == "i64" || n == "integer" || n == "Int" || n == "Int64" || n == "Integer"
    = "i64"
  | n == "i32" || n == "Int32"
    = "i32"
  | n == "float" || n == "f64" || n == "Float64" || n == "Double" || n == "double"
    = "f64"
  | n == "f32" || n == "Float" || n == "Float32"
    = "f32"
  | n == "bool" || n == "Bool"
    = "i64"  -- represent Bool as i64 (0/1) consistently throughout the pipeline
  | n == "ptr" || n == "Ptr"
    = "!llvm.ptr"
  | n == "unit" || n == "Unit" || n == "()" || n == "void" || n == "Void"
    = "i64"  -- represent unit as i64 (0) for simplicity
  | otherwise
    = "i64"  -- default to i64 for unrecognized types
  where n = nameText (qnameName (tcName tc))
typeToMlir (TFun _args _ _ret) =
  -- Function type → function pointer type
  "!llvm.ptr"  -- function pointers are opaque pointers in modern LLVM
typeToMlir (TForall _ body) = typeToMlir body
typeToMlir (TApp _ _) = "i64"
typeToMlir (TVar _) = "i64"
typeToMlir (TSyn _ _ expansion) = typeToMlir expansion

-- | Extract the primary effect name from an effect row (for emitter use).
effectRowNameEmit :: EffectRow -> Text
effectRowNameEmit (EffectRowExtend qn _) = qnameModule qn <> nameText (qnameName qn)
effectRowNameEmit (EffectRowVar tv)       = nameText (tvName tv)
effectRowNameEmit EffectRowEmpty          = "pure"

-- Sanitize names for MLIR (replace special chars)
sanitizeName :: Text -> Text
sanitizeName = T.map (\c -> if c `elem` ("+*-/=<>!@#$%^&|~.,()[]{}'\"\\ \t" :: [Char]) then '_' else c)

-- | Check if an expression is a lambda (possibly wrapped in EDelay/ETypeLam).
isLambda :: Expr -> Bool
isLambda (ELam _ _)     = True
isLambda (ETypeLam _ e) = isLambda e
isLambda (EDelay e)     = isLambda e
isLambda _              = False

-- | Count the number of value parameters in a (possibly wrapped) lambda.
countLamParams :: Expr -> Int
countLamParams (ELam ps _)     = length ps
countLamParams (ETypeLam _ e)  = countLamParams e
countLamParams (EDelay e)      = countLamParams e
countLamParams _               = 0

-- | Check if a let-binding should be promoted to a top-level function.
-- Any let-bound lambda gets promoted: this handles recursive where-bound
-- helpers (go, goBranch, etc.) that can't capture their own recursive
-- reference through the normal lambda-lifting closure mechanism.
isRecLetLambda :: Text -> Bind -> Bool
isRecLetLambda _modPfx bnd = isLambda (bindExpr bnd)

-- | Compute the qualified name for a let-bound function being promoted
-- to top-level. Uses module prefix + unique to avoid collisions between
-- multiple let-bound functions with the same short name (e.g. multiple
-- 'go' in different where-clauses of the same module).
qualifyBindName :: Text -> Name -> Text
qualifyBindName modPfx n =
  let san = sanitizeName (nameText n)
  in if T.any (== '/') (nameText n) || T.isPrefixOf modPfx san
     then san
     else modPfx <> san <> "_u" <> T.pack (show (nameUnique n))

-- | Strip EDelay/ETypeLam wrappers to get to the inner ELam.
unwrapLambda :: Expr -> Expr
unwrapLambda (EDelay e)     = unwrapLambda e
unwrapLambda (ETypeLam _ e) = unwrapLambda e
unwrapLambda e              = e

-- | Build arity map from top-level defs: sanitized-name -> number of ELam params.
-- Nullary defs (arity 0) are also recorded so oversaturated calls can be
-- detected and routed through the closure-indirect path.
buildTopFnArity :: Text -> [Def] -> Map Text Int
buildTopFnArity modPfx defs = Map.fromList
  [ (qualKey, topLamArity (defExpr d))
  | d <- defs
  , let t = nameText (qnameName (defName d))
        san = sanitizeName t
        qualKey = if T.any (== '/') t || T.isPrefixOf modPfx san then san else modPfx <> san
  ]
  where
    topLamArity (ELam ps _)     = length ps
    topLamArity (ETypeLam _ e)  = topLamArity e
    topLamArity _               = 0

-- | External C runtime functions that are declared as `func.func private`
-- in MLIR and must be called directly (not through closure-indirect dispatch).
externalRuntimeFns :: Set Text
externalRuntimeFns = Set.fromList
  [ "kk_drop", "kk_retain", "kk_release", "kk_reuse", "kk_is_unique"
  , "kk_alloc_con", "kk_set_field", "kk_field", "kk_tag"
  , "kk_thunk_create", "kk_thunk_force"
  , "kk_evv_create", "kk_evv_set", "kk_evv_get", "kk_unhandled_effect"
  , "printf", "puts", "exit", "exitWith", "malloc", "free"
  , "println_str", "print_str", "putStrLn"
  , "str_len", "str_concat", "str_eq", "str_flatten", "show_int"
  , "read_line", "getLine", "read_file", "write_file"
  , "args_count", "args_get", "args_progname"
  , "new_ref", "get_ref", "set_ref"
  , "kk_println_con"
  -- Mercury choice effect runtime
  , "mercury_choose", "mercury_collect_choices"
  , "mercury_exn_fail", "mercury_fail"
  ]

externalRuntimeArity :: Map Text Int
externalRuntimeArity = Map.fromList
  [ ("kk_drop", 1), ("kk_retain", 1), ("kk_release", 1)
  , ("kk_reuse", 2), ("kk_is_unique", 1)
  , ("kk_alloc_con", 2), ("kk_set_field", 3), ("kk_field", 2), ("kk_tag", 1)
  , ("kk_thunk_create", 1), ("kk_thunk_force", 1)
  , ("kk_evv_create", 1), ("kk_evv_set", 3), ("kk_evv_get", 2)
  , ("kk_unhandled_effect", 0)
  , ("printf", 2), ("puts", 1), ("exit", 1), ("exitWith", 1)
  , ("println_str", 1), ("print_str", 1), ("putStrLn", 1)
  , ("str_len", 1), ("str_concat", 2), ("str_eq", 2), ("str_flatten", 1)
  , ("show_int", 1)
  , ("read_line", 0), ("getLine", 0)
  , ("read_file", 1), ("write_file", 2)
  , ("args_count", 0), ("args_get", 1), ("args_progname", 0)
  , ("new_ref", 1), ("get_ref", 1), ("set_ref", 2)
  , ("kk_println_con", 3)
  , ("mercury_choose", 0), ("mercury_collect_choices", 1)
  , ("mercury_exn_fail", 0), ("mercury_fail", 0)
  ]

-- | Convert a Name to a unique MLIR SSA name.
-- For names like "_" or "ds" that commonly collide, append the unique ID.
nameToSsa :: Name -> Text
nameToSsa n
  | t == "_" || t == "ds" || t == "wild" = t <> T.pack (show (nameUnique n))
  | otherwise = sanitizeName t
  where t = nameText n

-- | Escape a string for MLIR string literal (handle backslashes, quotes, newlines)
-- | Escape a Text into MLIR's string-literal byte format. We iterate over
-- UTF-8 *bytes*, not Unicode codepoints, so multi-byte chars are emitted
-- as the correct sequence of \HH escapes — matching the byte length the
-- runtime sees from kk_string_from_literal.
escapeMLIRString :: Text -> Text
escapeMLIRString t = T.concat (map escByte (BS.unpack (TE.encodeUtf8 t)))
  where
    escByte :: Word8 -> Text
    escByte 0x5C = "\\\\"           -- backslash
    escByte 0x22 = "\\22"           -- double quote
    escByte b
      | b >= 0x20 && b < 0x7F = T.singleton (toEnum (fromIntegral b))
      | otherwise = T.pack ('\\' : printf "%02X" b)

-- | Full compilation pipeline
compileToExecutable :: EmitConfig -> Program -> IO (Either Text FilePath)
compileToExecutable config prog = do
  let mlirText = emitProgramText prog
      mlirPath = ecOutputPath config ++ ".mlir"
      optPath = ecOutputPath config ++ ".opt.mlir"
      llPath = ecOutputPath config ++ ".ll"

  -- Write MLIR
  TIO.writeFile mlirPath mlirText

  -- mlir-opt: lower to LLVM dialect
  -- linalg/memref passes are no-ops when no array ops are present, so they
  -- are safe to always include. They light up the Futhark array path.
  (ec1, out1, err1) <- readProcessWithExitCode (ecMlirOptPath config)
    ["--convert-linalg-to-loops",
     "--expand-strided-metadata",
     "--finalize-memref-to-llvm",
     "--convert-scf-to-cf", "--convert-func-to-llvm", "--convert-arith-to-llvm",
     "--convert-cf-to-llvm", "--reconcile-unrealized-casts", mlirPath] ""
  case ec1 of
    ExitFailure _ -> pure $ Left $ "mlir-opt failed: " <> T.pack err1
    ExitSuccess -> do
      writeFile optPath out1

      -- mlir-translate: MLIR → LLVM IR
      (ec2, out2, err2) <- readProcessWithExitCode (ecMlirTranslatePath config)
        ["--mlir-to-llvmir", optPath] ""
      case ec2 of
        ExitFailure _ -> pure $ Left $ "mlir-translate failed: " <> T.pack err2
        ExitSuccess -> do
          writeFile llPath out2

          -- clang: LLVM IR + runtime → executable
          -- We must compile the runtime C file separately, then link,
          -- because -x ir would apply to all inputs.
          case ecKokaRuntimePath config of
            Just rtPath -> do
              -- Derive cycle collector path from runtime path
              let rtDir = reverse . dropWhile (/= '/') . reverse $ rtPath
                  cyclePath = rtDir ++ "kk_cycle.c"
                  arenaPath = rtDir ++ "kk_arena.c"
                  rtObjPath = ecOutputPath config ++ ".rt.o"
                  cycleObjPath = ecOutputPath config ++ ".cycle.o"
                  arenaObjPath = ecOutputPath config ++ ".arena.o"
                  optFlag = "-O" ++ show (ecOptLevel config)
                  includeFlag = "-I" ++ rtDir
              -- Compile runtime C files to .o
              (ec3a, _, err3a) <- readProcessWithExitCode (ecClangPath config)
                ["-c", rtPath, "-o", rtObjPath, includeFlag, optFlag] ""
              case ec3a of
                ExitFailure _ -> pure $ Left $ "clang (runtime) failed: " <> T.pack err3a
                ExitSuccess -> do
                  (ec3c, _, err3c) <- readProcessWithExitCode (ecClangPath config)
                    ["-c", cyclePath, "-o", cycleObjPath, includeFlag, optFlag] ""
                  case ec3c of
                    ExitFailure _ -> pure $ Left $ "clang (cycle) failed: " <> T.pack err3c
                    ExitSuccess -> do
                      (ec3d, _, err3d) <- readProcessWithExitCode (ecClangPath config)
                        ["-c", arenaPath, "-o", arenaObjPath, includeFlag, optFlag] ""
                      case ec3d of
                        ExitFailure _ -> pure $ Left $ "clang (arena) failed: " <> T.pack err3d
                        ExitSuccess -> do
                          -- Link LLVM IR + runtime .o + cycle .o + arena .o → executable
                          (ec3b, _, err3b) <- readProcessWithExitCode (ecClangPath config)
                            ["-x", "ir", llPath, "-x", "none", rtObjPath, cycleObjPath, arenaObjPath,
                             "-o", ecOutputPath config, optFlag] ""
                          case ec3b of
                            ExitFailure _ -> pure $ Left $ "clang (link) failed: " <> T.pack err3b
                            ExitSuccess -> pure $ Right $ ecOutputPath config
            Nothing -> do
              (ec3, _, err3) <- readProcessWithExitCode (ecClangPath config)
                [llPath, "-x", "ir", "-o", ecOutputPath config,
                 "-O" ++ show (ecOptLevel config)] ""
              case ec3 of
                ExitFailure _ -> pure $ Left $ "clang failed: " <> T.pack err3
                ExitSuccess -> pure $ Right $ ecOutputPath config

-- | Compile to WebAssembly (.wasm)
-- Pipeline: MLIR → mlir-opt → mlir-translate → llc (wasm32) → wasm-ld
compileToWasm :: EmitConfig -> Program -> IO (Either Text FilePath)
compileToWasm config prog = do
  let mlirText = emitProgramWasm prog
      outBase = ecOutputPath config
      mlirPath = outBase ++ ".mlir"
      optPath = outBase ++ ".opt.mlir"
      llPath = outBase ++ ".ll"
      wasmObjPath = outBase ++ ".wasm.o"
      wasmRtObjPath = outBase ++ ".wasm.rt.o"
      wasmPath = if ".wasm" `isSuffixOf` outBase then outBase else outBase ++ ".wasm"

  -- Write MLIR (no printf, no fmt_int)
  TIO.writeFile mlirPath mlirText

  -- mlir-opt: lower to LLVM dialect
  (ec1, out1, err1) <- readProcessWithExitCode (ecMlirOptPath config)
    ["--convert-scf-to-cf", "--convert-func-to-llvm", "--convert-arith-to-llvm",
     "--convert-cf-to-llvm", "--reconcile-unrealized-casts", mlirPath] ""
  case ec1 of
    ExitFailure _ -> pure $ Left $ "mlir-opt failed: " <> T.pack err1
    ExitSuccess -> do
      writeFile optPath out1

      -- mlir-translate: MLIR → LLVM IR
      (ec2, out2, err2) <- readProcessWithExitCode (ecMlirTranslatePath config)
        ["--mlir-to-llvmir", optPath] ""
      case ec2 of
        ExitFailure _ -> pure $ Left $ "mlir-translate failed: " <> T.pack err2
        ExitSuccess -> do
          writeFile llPath out2

          -- llc: LLVM IR → wasm32 object
          (ec3, _, err3) <- readProcessWithExitCode "llc"
            ["-mtriple=wasm32-unknown-unknown", "-filetype=obj", "-O2",
             llPath, "-o", wasmObjPath] ""
          case ec3 of
            ExitFailure _ -> pure $ Left $ "llc (wasm32) failed: " <> T.pack err3
            ExitSuccess -> do
              -- Compile wasm runtime
              case ecKokaRuntimePath config of
                Just rtPath -> do
                  let rtDir = reverse . dropWhile (/= '/') . reverse $ rtPath
                      wasmRtSrc = rtDir ++ "kk_runtime_wasm.c"
                  (ec4, _, err4) <- readProcessWithExitCode (ecClangPath config)
                    ["--target=wasm32-unknown-unknown", "-O2", "-nostdlib",
                     "-c", wasmRtSrc, "-o", wasmRtObjPath] ""
                  case ec4 of
                    ExitFailure _ -> pure $ Left $ "clang (wasm runtime) failed: " <> T.pack err4
                    ExitSuccess -> do
                      -- wasm-ld: link program + runtime → .wasm
                      (ec5, _, err5) <- readProcessWithExitCode "wasm-ld"
                        ["--no-entry", "--export=_frankenstein_main",
                         "--allow-undefined",
                         wasmObjPath, wasmRtObjPath,
                         "-o", wasmPath] ""
                      case ec5 of
                        ExitFailure _ -> pure $ Left $ "wasm-ld failed: " <> T.pack err5
                        ExitSuccess -> pure $ Right wasmPath
                Nothing -> do
                  -- No runtime — link program only with undefined symbols allowed
                  (ec5, _, err5) <- readProcessWithExitCode "wasm-ld"
                    ["--no-entry", "--export=_frankenstein_main",
                     "--allow-undefined",
                     wasmObjPath, "-o", wasmPath] ""
                  case ec5 of
                    ExitFailure _ -> pure $ Left $ "wasm-ld failed: " <> T.pack err5
                    ExitSuccess -> pure $ Right wasmPath
  where
    isSuffixOf suf s = drop (length s - length suf) s == suf
