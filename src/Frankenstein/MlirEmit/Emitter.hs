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
import Frankenstein.Core.CycleAnalysis (analyzeCycles, CycleInfo(..))
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
import Control.Monad (forM, forM_)
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
  , ecOptLevel          = 2
  , ecOutputPath        = "a.out"
  , ecTarget            = TargetNative
  }

-- Emission state: tracks SSA counter and collected top-level functions
data EmitState = EmitState
  { esCounter       :: !Int
  , esLiftedFns     :: ![Text]  -- accumulated lifted lambda functions
  , esLiftedNames   :: !(Set Text)  -- names of already-emitted lifted fns (dedup)
  , esTypeEnv       :: !(Map Text Text)  -- SSA name -> MLIR type
  , esStringLits    :: ![(Text, Text)]   -- global name -> string content
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
  , esCyclicDefs       :: !(Set Text)             -- mangled names of defs that may create reference cycles
  , esCurDef           :: !Text                   -- current definition being emitted (for cycle candidate check)
  , esExtRuntimeFns    :: !(Set Text)             -- cached externalRuntimeFns (avoid per-call Set.fromList)
  , esExtRuntimeArity  :: !(Map Text Int)         -- cached externalRuntimeArity
  , esCurrentEvv       :: !(Maybe Text)           -- SSA name of the in-scope evv (set in plotkin-transformed defs); used to pre-supply evv when emitting a plotkin'd top-level fn as a value, so HOFs invoke through the trampoline path that re-injects the captured evv
  , esScopeSsa         :: !(Set Text)             -- SSA names valid in the current MLIR function's scope (params + captures + closure self); saved at every function-entry boundary (emitDef, emitLambdaLift, emitBindAsTopFn). emitFnAsValue uses this to decide whether the resolved evv SSA is reachable from the current emission point — if not, the PAP is allocated without pre-supplied evv (the trampoline reads 0 from the unset field, which is safe for non-effectful callees in bootstrap modules).
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

-- | Add a lifted function only if a function with the given name hasn't been emitted yet.
addLiftedFnOnce :: Text -> Text -> Emit ()
addLiftedFnOnce name fn = do
  already <- gets esLiftedNames
  if Set.member name already
    then pure ()
    else modify (\s -> s { esLiftedFns = fn : esLiftedFns s
                         , esLiftedNames = Set.insert name (esLiftedNames s) })

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
    ) (zip [(1 :: Int)..length suppliedArgs] suppliedArgs)
  pure (allocOps ++ setOps, ptrName)

-- | The MLIR symbol a PAP wrapper should call.  For intrinsics
-- backed by a runtime helper (str_concat → kk_str_concat,
-- read_line → kk_read_line, …) the wrapper must use the kk_-prefixed
-- name; the linker only declares the runtime symbols.  For
-- user-defined and top-level functions, the wrapper uses the name as
-- given.  Mirrors the intrinsic remaps in `emitAppVarWith*` so the
-- PAP path resolves the same symbols.
papCallTarget :: Text -> Text
papCallTarget n
  | n `elem`
      [ "str_concat", "++s", "concat_str", "bytes_concat"
      , "str_len", "strlen", "bytes_len"
      , "str_char_len", "char_len", "char_count", "length"
      , "str_eq", "==s", "bytes_eq"
      , "str_flatten", "flatten"
      , "show_int", "str_show_int"
      , "println_str", "print_str"
      , "read_file", "readFile"
      , "write_file", "writeFile"
      , "file_exists", "fileExists"
      , "read_line", "getLine"
      , "string_empty"
      , "system", "shell"
      , "getenv", "getEnv"
      , "args_count", "numArgs"
      , "args_get", "getArg"
      , "args_progname", "getProgName"
      , "println_haskell_chars", "print_haskell_chars"
      , "haskell_chars_concat"
      , "int_to_haskell_chars", "int_list_to_haskell_chars"
      ] = "kk_" <> stripDuplicateNames n
  | otherwise = n
  where
    -- For the synonyms list above, map the canonical kk_ name.  e.g.
    -- "writeFile" and "write_file" both alias to "kk_write_file".
    stripDuplicateNames "writeFile"     = "write_file"
    stripDuplicateNames "readFile"      = "read_file"
    stripDuplicateNames "fileExists"    = "file_exists"
    stripDuplicateNames "getLine"       = "read_line"
    stripDuplicateNames "getEnv"        = "getenv"
    stripDuplicateNames "numArgs"       = "args_count"
    stripDuplicateNames "getArg"        = "args_get"
    stripDuplicateNames "getProgName"   = "args_progname"
    stripDuplicateNames "shell"         = "system"
    stripDuplicateNames "strlen"        = "str_len"
    stripDuplicateNames "bytes_len"     = "str_len"
    stripDuplicateNames "char_len"      = "str_char_len"
    stripDuplicateNames "char_count"    = "str_char_len"
    stripDuplicateNames "length"        = "str_char_len"
    stripDuplicateNames "++s"           = "str_concat"
    stripDuplicateNames "concat_str"    = "str_concat"
    stripDuplicateNames "bytes_concat"  = "str_concat"
    stripDuplicateNames "==s"           = "str_eq"
    stripDuplicateNames "bytes_eq"      = "str_eq"
    stripDuplicateNames "flatten"       = "str_flatten"
    stripDuplicateNames "str_show_int"  = "str_show_int"
    stripDuplicateNames other           = other

-- | Ensure a PAP wrapper function exists for (fnName, arity, nSupplied).
-- Emits the wrapper lazily and returns its MLIR symbol name.  The
-- wrapper takes (closure, remaining_args...) and dispatches to
-- @fnName(captured..., remaining...).
--
-- The wrapper name encodes BOTH @nSupplied@ and @arity@.  Encoding
-- arity in the symbol prevents the cross-arity-reuse bug: the
-- wrapper's body is generated from the caller's @arity@, and its
-- internal call to the wrapped fn uses @origParamTys = replicate
-- arity "i64"@.  If a later caller asked for a different arity and
-- got the cached wrapper (named only by @nSupplied@), its
-- @func.constant @wrapperName : (...)@ declaration would use a
-- different signature than the wrapper actually has — mlir-opt
-- would then reject the call as "incorrect number of operands for
-- callee".  Distinct names per arity sidestep the inconsistency at
-- the cost of (rare) duplicate wrapper functions.
ensurePapWrapper :: Text -> Int -> Int -> Emit Text
ensurePapWrapper fnName arity nSupplied = do
  let wrapperName = "pap_" <> fnName
                  <> "_a" <> T.pack (show arity)
                  <> "_" <> T.pack (show nSupplied)
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
          -- Bridge intrinsics (str_concat, read_line, etc.) live in the
          -- runtime as kk_-prefixed symbols.  The direct-call path
          -- (emitAppVarWith*) rewrites these at the call site; the PAP
          -- wrapper, which generates a call from the wrapper's own
          -- body, would otherwise emit `@str_concat` and break linking.
          -- Remap to the runtime name when the unprefixed name is a
          -- known intrinsic.
          callTarget = papCallTarget fnName
      -- Body: extract captured args from closure fields 1..nSupplied, then call original.
      -- Each capture must be retained because PAP wrappers may be called multiple
      -- times (e.g. via mapM). Without retain, the callee's Perceus-inserted drops
      -- free the captured value after the first call, causing use-after-free on
      -- subsequent calls.
      let captureLoads = concat
            [ [ "    %cidx" <> T.pack (show i) <> " = arith.constant " <> T.pack (show i) <> " : i64"
              , "    %c" <> T.pack (show i) <> " = func.call @kk_field(%clos, %cidx" <> T.pack (show i) <> ") : (i64, i64) -> i64"
              , "    func.call @kk_retain(%c" <> T.pack (show i) <> ") : (i64) -> ()"
              ]
            | i <- [1 .. nSupplied]
            ]
          capturedArgRefs = [ "%c" <> T.pack (show i) | i <- [1 .. nSupplied] ]
          remainingArgRefs = [ "%r" <> T.pack (show i) | i <- [0 .. nRemaining - 1] ]
          allArgRefs = T.intercalate ", " (capturedArgRefs ++ remainingArgRefs)
          callLine = "    %result = func.call @" <> callTarget <> "(" <> allArgRefs
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
--
-- In plotkin mode ('esCurrentEvv' is 'Just'), Frankenstein top-level fns
-- have been transformed to take 'evv' as a prepended first arg. When such
-- a fn is passed to a higher-order function (map, fromList, etc.) the HOF
-- was compiled against the fn's *original* arity and would invoke it with
-- one too few args, producing a PAP closure instead of the value the HOF
-- expects. To make the boundary transparent, we pre-supply the in-scope
-- 'evv' so the PAP's remaining-arity matches the HOF's call shape.
emitFnAsValue :: Text -> Map Text Int -> Emit ([Text], Text)
emitFnAsValue fnName arityMap = do
  curEvv    <- gets esCurrentEvv
  aliases   <- gets esAliases
  scopeSsa  <- gets esScopeSsa
  extRt     <- gets esExtRuntimeFns
  let arity        = Map.findWithDefault 1 fnName arityMap
      -- A top-level fn that's been plotkin-transformed in the current
      -- Program. Under plotkin mode (esCurrentEvv is Just), every
      -- Frankenstein top-level def gets an evv-prepended ABI — INCLUDING
      -- user-module fns like HigherOrder_addN, Closure_..., etc. The
      -- earlier "Frankenstein_" infix check was too narrow and silently
      -- dropped pre-supply for user examples, breaking HOFs.
      --
      -- Exclusions (NOT plotkin-transformed, so DON'T pre-supply):
      --   * extRuntimeFns (kk_*, mercury_*, etc.)
      --   * builtin wrappers (__kk_builtin_add/sub/eq/... — synthesized
      --     by builtinWrapperSpec for binary ops used as values; they
      --     keep their original arithmetic arity)
      --   * intra-module $N split-compilation wrappers (those carry the
      --     original undecorated arity for split-compiled .organ.json)
      isPlotkinFn  = case curEvv of
        Just _ -> not (Set.member fnName extRt)
                  && not ("__kk_builtin_" `T.isPrefixOf` fnName)
        Nothing -> False
      -- Resolve evv through the alias map (handles capture rebinds in
      -- lifted lambdas) and verify the resolved SSA is in the current
      -- MLIR function's scope. If it's an outer-scope name leaked
      -- through the alias map's insert-without-shadow, fall back to
      -- the no-evv path: the PAP's trampoline reads 0 from the
      -- unset evv field, which is the empty-evv sentinel — safe for
      -- non-effectful callees (the common bootstrap case).
      resolved =
        case curEvv >>= \e -> Map.lookup e aliases of
          Just r | Set.member r scopeSsa -> Just r
          _                              -> Nothing
  case resolved of
    Just evvSsa | isPlotkinFn && arity > 0 ->
      emitFnAsValueWithArgs fnName arity [evvSsa]
    _ | isPlotkinFn && arity > 0 -> do
      -- Fallback: when no in-scope evv is resolvable (e.g. inside a lifted
      -- lambda whose self-host-emitted body lost track of the outer evv
      -- capture), pre-supply 0 as the empty-evv sentinel. The PAP wrapper
      -- still has the right arity (matching plotkin defs), so HOFs invoking
      -- it via call1/call2 get a saturated dispatch. The captured 0 is
      -- safe for non-effectful callees (the bootstrap case).
      zeroSsa <- freshName "v_zero_evv"
      (ops, ret) <- emitFnAsValueWithArgs fnName arity [zeroSsa]
      pure ( ("%" <> zeroSsa <> " = arith.constant 0 : i64") : ops, ret )
    _ ->
      emitFnAsValueWithArgs fnName arity []

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

-- | Mark an SSA name as semantically holding a float64 value, even
-- though its MLIR type is `i64` (the value is the IEEE bit pattern).
-- emitBinOp checks this tag to decide whether to dispatch float
-- arith (`arith.mulf`) instead of integer arith (`arith.muli`).
-- The tag is stored as `f64bits` in esTypeEnv; `lookupType` strips
-- it to keep call-site MLIR types accurate.
f64BitsTag :: Text
f64BitsTag = "f64bits"

recordF64Bits :: Text -> Emit ()
recordF64Bits name = recordType name f64BitsTag

-- | Look up the MLIR type for an SSA name (default: "i64").
-- Strips the `f64bits` semantic tag so call signatures use i64.
lookupType :: Text -> Emit Text
lookupType name = do
  env <- gets esTypeEnv
  let t = Map.findWithDefault "i64" name env
  pure $ if t == f64BitsTag then "i64" else t

-- | Is the SSA value semantically a float64 (with i64 MLIR type)?
isF64Bits :: Text -> Emit Bool
isF64Bits name = do
  env <- gets esTypeEnv
  pure $ Map.lookup name env == Just f64BitsTag

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
      -- Main's arity at the MLIR level. After D3 plotkin transform, main
      -- takes (evv) as its first parameter; for non-plotkin compiles main
      -- is 0-arg. We read the arity from the renamed Def's type.
      mainArity = case [ d | d <- renamedDefs
                           , nameText (qnameName (defName d)) == "_frankenstein_main" ] of
                    (d:_) -> typeArity (defType d)
                    _     -> 0
      typeArity (TForall _ t)    = typeArity t
      typeArity (TFun args _ _)  = length args
      typeArity _                = 0
      -- MLIR call-site signature for main and the args we pass.
      mainCallSig = if mainArity == 0
                    then "() -> i64"
                    else "(" <> T.intercalate ", " (replicate mainArity "i64") <> ") -> i64"
      mainCallArgs = T.intercalate ", " (replicate mainArity "%c0_evv")
      mainCallPrelude = if mainArity == 0
                        then ""
                        else "    %c0_evv = arith.constant 0 : i64\n"
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
      -- Cycle analysis: identify defs that may create reference cycles.
      -- analyzeCycles returns one CycleInfo per def, in the same order.
      cyclicDefs = Set.fromList
        [ qualifyDefName d
        | (d, ci) <- zip renamedDefs (analyzeCycles prog)
        , ciCyclic ci
        ]
      extRtFns = externalRuntimeFns
      extRtArity = externalRuntimeArity
      initState = EmitState 0 [] Set.empty Map.empty [] Map.empty False
                         (qualifiedTopNames `Set.union` extRtFns)
                         (buildTopFnArity modPrefix renamedDefs `Map.union` extRtArity)
                         Set.empty
                         (assignProgramTags prog)
                         modPrefix
                         Map.empty
                         Map.empty
                         Map.empty
                         cyclicDefs
                         ""
                         extRtFns
                         extRtArity
                         Nothing
                         Set.empty
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
      -- If so, the wrapper should not print the return value.  We follow
      -- EVar references through one level of definitions so that
      -- `main = main$N` and `main$N = print_str(...)` (the post-GHC
      -- simplifier shape for `main = putStrLn "..."`) is recognised.
      defBodyByName = Map.fromList
        [ (nameText (qnameName (defName d)), defExpr d) | d <- defs ]
      mainPrints = any (\d -> nameText (qnameName (defName d)) == "main"
                         && exprCallsPrintWith defBodyByName (defExpr d)) defs
      mainReturnsADT = any (\d -> nameText (qnameName (defName d)) == "main"
                              && returnsDataType prog d) defs
      mainReturnsString = any (\d -> nameText (qnameName (defName d)) == "main"
                                  && returnsStringType d) defs
      -- Haskell's `String = [Char]` arrives as the cons-list type
      -- `List Char`.  When main returns that, walk the list and print
      -- each char rather than treating the head pointer as an Int.
      mainReturnsHaskellString = any (\d -> nameText (qnameName (defName d)) == "main"
                                          && returnsListChar d) defs
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
            [ mainCallPrelude <> "    func.call @" <> qualMainName <> "(" <> mainCallArgs <> ") : " <> mainCallSig
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else if mainReturnsString
          then T.unlines $ mainHeader ++
            [ mainCallPrelude <> "    %result = func.call @" <> qualMainName <> "(" <> mainCallArgs <> ") : " <> mainCallSig
            , "    func.call @kk_println_str(%result) : (i64) -> ()"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else if mainReturnsHaskellString
          then T.unlines $ mainHeader ++
            [ mainCallPrelude <> "    %result_raw = func.call @" <> qualMainName <> "(" <> mainCallArgs <> ") : " <> mainCallSig
            , "    %result = func.call @kk_thunk_force(%result_raw) : (i64) -> i64"
            , "    func.call @kk_println_haskell_chars(%result) : (i64) -> ()"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else if mainReturnsADT
          then T.unlines $ mainHeader ++
            [ mainCallPrelude <> "    %result = func.call @" <> qualMainName <> "(" <> mainCallArgs <> ") : " <> mainCallSig
            , "    func.call @kk_println_con(%result) : (i64) -> ()"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
          else T.unlines $ mainHeader ++
            [ mainCallPrelude <> "    %result_raw = func.call @" <> qualMainName <> "(" <> mainCallArgs <> ") : " <> mainCallSig
            -- Force in case main's body is wrapped in kk_thunk_create_forced
            -- (EDelay lowering). Without this, printf prints the thunk pointer
            -- as a giant integer.
            , "    %result = func.call @kk_thunk_force(%result_raw) : (i64) -> i64"
            , "    %fmtaddr = llvm.mlir.addressof @fmt_int : !llvm.ptr"
            , "    llvm.call @printf(%fmtaddr, %result) vararg(!llvm.func<i32 (ptr, ...)>) : (!llvm.ptr, i64) -> i32"
            , "    %zero = arith.constant 0 : i32"
            , "    func.return %zero : i32"
            , "  }"
            ]
        else ""
      -- Wrapper that follows a single level of EVar indirection through
      -- the def-by-name map.  Bounded depth (3) avoids accidental cycles.
      exprCallsPrintWith :: Map Text Expr -> Expr -> Bool
      exprCallsPrintWith m = go (3 :: Int)
        where
          go d e | d <= 0 = exprCallsPrint e
          go d (EVar fn)
            | Just body <- Map.lookup (nameText fn) m = go (d - 1) body
            | otherwise = False
          go d (EApp (EVar fn) args)
            | exprCallsPrint (EApp (EVar fn) args) = True
            | Just body <- Map.lookup (nameText fn) m = go (d - 1) body
          go d (EApp f args) = go d f || any (go d) args
          go d (EDelay e)    = go d e
          go d (EForce e)    = go d e
          go d (ELet bgs body) =
            go d body || or [ go d (bindExpr b) | bg <- bgs, b <- bg ]
          go d (ECase _ bs) = any (\(Branch _ _ b) -> go d b) bs
          go d (ELam _ body) = go d body
          go _ _ = False
      exprCallsPrint (EApp (EVar fn) _) =
        nameText fn `elem`
          [ "print", "println_str", "putStrLn", "print_str"
          , "println_haskell_chars", "print_haskell_chars"
          , "rust_print_dispatch"
          -- Mercury bridge synthesises a no-arg `main` alias that calls
          -- the user's `main(io::di, io::uo) is det` predicate
          -- (renamed to `main_io_impl` to avoid the alias collision,
          -- then mangled by the Linker to `mercury_main_io_impl`).
          -- That predicate threads through io.write_string etc., so
          -- the wrapper must not print the alias's Int return value.
          , "main_io_impl", "mercury_main_io_impl"
          -- Idris2 bridge: `putStrLn`/`putStr` in user code desugar to
          -- `Prelude.IO/prim__putStr`; the Linker mangles `/` → `_`,
          -- so by the time the wrapper detection runs we see the
          -- underscore form.  Also include our runtime stub for paths
          -- that bypass the prelude wrapper.
          , "Prelude.IO_prim__putStr"
          , "idris2_putStr"
          ]
      exprCallsPrint (EApp f args)       = exprCallsPrint f || any exprCallsPrint args
      exprCallsPrint (EDelay e)          = exprCallsPrint e
      exprCallsPrint (EForce e)          = exprCallsPrint e
      -- A `let _ = print_str("...") in ()` form (Rust's println! after
      -- our bridge remap) was previously missed because we only checked
      -- the body of the let, not the binder RHSs.  Check both.
      exprCallsPrint (ELet bgs body)     =
        exprCallsPrint body
          || or [ exprCallsPrint (bindExpr b) | bg <- bgs, b <- bg ]
      exprCallsPrint (ECase _ bs)        = any (\(Branch _ _ b) -> exprCallsPrint b) bs
      exprCallsPrint (ELam _ body)       = exprCallsPrint body
      exprCallsPrint _                   = False
      -- True iff the def's return type is a TCon whose name matches a
      -- DataDecl in the program. Used to pick the s-expression printer
      -- over printf("%ld") in the main wrapper.
      returnsDataType p d =
        let (_, _, ret) = decomposeDefType (defType d)
            -- Exclude primitive types that have TyCon entries but should be
            -- printed as integers, not as boxed constructors.
            primitiveTypes = ["Int", "Bool", "Char", "Double", "Float",
                              "Int#", "Word#", "Int64#", "Word64#"]
            dataNames = [ nameText (qnameName (dataName dd)) | dd <- progData p ]
            tconName (TCon (TypeCon qn _)) = Just (nameText (qnameName qn))
            tconName (TApp t _)            = tconName t
            tconName (TSyn _ _ t)          = tconName t
            tconName (TForall _ t)         = tconName t
            tconName _                     = Nothing
        in case tconName ret of
             Just n  -> n `elem` dataNames && n `notElem` primitiveTypes
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
      -- True iff the def's return type is the Haskell `List Char`
      -- (i.e. `[Char]` / `String`) cons-list.  The GHC bridge encodes
      -- this as TApp (TCon "[]") (TCon "Char") for the plain list type,
      -- or as TCon "String" / TSyn "String" when the source uses the
      -- synonym directly.
      returnsListChar d =
        let (_, _, ret) = decomposeDefType (defType d)
            unwrap (TSyn _ _ t)  = unwrap t
            unwrap (TForall _ t) = unwrap t
            unwrap t             = t
            tconText (TCon (TypeCon qn _)) = Just (nameText (qnameName qn))
            tconText _                     = Nothing
            listConNames = ["[]", "List"]
            isListApp outer inner =
              maybe False (`elem` listConNames) (tconText (unwrap outer))
                && tconText (unwrap inner) == Just "Char"
        in case unwrap ret of
             TApp outer inner -> isListApp outer inner
             TCon tc          -> nameText (qnameName (tcName tc)) == "String"
             _                -> False
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
    , "  func.func private @kk_cycle_candidate(i64) -> ()"
    , "  func.func private @kk_set_field(i64, i64, i64) -> ()"
    , "  func.func private @kk_tag(i64) -> i64"
    , "  func.func private @kk_field(i64, i64) -> i64"
    , "  func.func private @kk_structural_eq(i64, i64) -> i64"
    , "  func.func private @kk_println_con(i64) -> ()"
    , "  func.func private @kk_println_haskell_chars(i64) -> ()"
    , "  func.func private @kk_print_haskell_chars(i64) -> ()"
    , "  func.func private @kk_int_to_haskell_chars(i64, i64) -> i64"
    , "  func.func private @kk_int_list_to_haskell_chars(i64, i64) -> i64"
    , "  func.func private @kk_haskell_chars_concat(i64, i64) -> i64"
    -- PAP wrapper alias: the emitter constructs PAPs using the bare
    -- intrinsic name (without the `kk_` prefix), and mlir-opt requires
    -- a func.func declaration with that exact name to validate the call.
    -- The C runtime declares haskell_chars_concat as an alias of
    -- kk_haskell_chars_concat (see runtime/kk_runtime.c).
    , "  func.func private @haskell_chars_concat(i64, i64) -> i64"
    , "  func.func private @dummy_show_caf() -> i64"
    , "  func.func private @kk_rust_args_pack(i64, i64) -> i64"
    , "  func.func private @kk_rust_print_dispatch(i64) -> i64"
    , "  func.func private @kk_rust_field_safe(i64, i64) -> i64"
    , "  func.func private @kk_rust_arg_debug(i64) -> i64"
    , "  func.func private @kk_rust_arg_lower_hex(i64) -> i64"
    , "  func.func private @kk_rust_arg_upper_hex(i64) -> i64"
    , "  func.func private @kk_rust_arg_octal(i64) -> i64"
    , "  func.func private @kk_rust_arg_binary(i64) -> i64"
    , "  func.func private @kk_rust_arg_u32(i64) -> i64"
    , "  func.func private @kk_rust_arg_i32(i64) -> i64"
    , "  func.func private @kk_rust_arg_u64(i64) -> i64"
    , "  func.func private @kk_rust_arg_u16(i64) -> i64"
    , "  func.func private @kk_rust_arg_i16(i64) -> i64"
    , "  func.func private @kk_rust_arg_u8(i64) -> i64"
    , "  func.func private @kk_rust_arg_i8(i64) -> i64"
    , "  func.func private @kk_rust_arg_f64(i64) -> i64"
    , "  func.func private @kk_rust_arg_f32(i64) -> i64"
    , "  func.func private @kk_rust_struct_0(i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_1(i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_2(i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_3(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_4(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_5(i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_6(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_7(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_8(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_args_pack(i64, i64) -> i64"
    , "  func.func private @rust_print_dispatch(i64) -> i64"
    , "  func.func private @rust_field_safe(i64, i64) -> i64"
    , "  func.func private @rust_arg_debug(i64) -> i64"
    , "  func.func private @rust_arg_lower_hex(i64) -> i64"
    , "  func.func private @rust_arg_upper_hex(i64) -> i64"
    , "  func.func private @rust_arg_octal(i64) -> i64"
    , "  func.func private @rust_arg_binary(i64) -> i64"
    , "  func.func private @rust_arg_u32(i64) -> i64"
    , "  func.func private @rust_arg_i32(i64) -> i64"
    , "  func.func private @rust_arg_u64(i64) -> i64"
    , "  func.func private @rust_arg_u16(i64) -> i64"
    , "  func.func private @rust_arg_i16(i64) -> i64"
    , "  func.func private @rust_arg_u8(i64) -> i64"
    , "  func.func private @rust_arg_i8(i64) -> i64"
    , "  func.func private @rust_arg_f64(i64) -> i64"
    , "  func.func private @rust_arg_f32(i64) -> i64"
    , "  func.func private @rust_struct_0(i64, i64) -> i64"
    , "  func.func private @rust_struct_1(i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_2(i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_3(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_4(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_5(i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_6(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_7(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_8(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  // List constructors"
    , "  func.func private @kk_cons(i64, i64) -> i64"
    , "  func.func private @kk_nil() -> i64"
    , ""
    , "  // First-class string runtime declarations (rope, UTF-8)"
    , "  func.func private @kk_string_from_literal(i64, i64) -> i64"
    , "  func.func private @kk_string_from_cstr(i64) -> i64"
    , "  func.func private @kk_string_empty() -> i64"
    , "  func.func private @kk_string_from_char(i64) -> i64"
    , "  func.func private @kk_list_map(i64, i64) -> i64"
    , "  func.func private @kk_list_filter(i64, i64) -> i64"
    , "  func.func private @kk_list_foldl(i64, i64, i64) -> i64"
    , "  func.func private @kk_list_all(i64, i64) -> i64"
    , "  func.func private @kk_list_any(i64, i64) -> i64"
    , "  func.func private @kk_list_drop(i64, i64) -> i64"
    , "  func.func private @kk_list_take(i64, i64) -> i64"
    , "  func.func private @kk_list_flatmap(i64, i64) -> i64"
    , "  func.func private @kk_list_filter_map(i64, i64) -> i64"
    , "  func.func private @kk_list_foreach(i64, i64) -> i64"
    , "  func.func private @kk_list_concat(i64, i64) -> i64"
    , "  func.func private @kk_list_length(i64) -> i64"
    , "  func.func private @kk_is_nil(i64) -> i64"
    , "  func.func private @cos(f64) -> f64"
    , "  func.func private @sin(f64) -> f64"
    , "  func.func private @tan(f64) -> f64"
    , "  func.func private @sqrt(f64) -> f64"
    , "  func.func private @log(f64) -> f64"
    , "  func.func private @exp(f64) -> f64"
    , "  func.func private @atan2(f64, f64) -> f64"
    , "  func.func private @pow(f64, f64) -> f64"
    , "  func.func private @fmod(f64, f64) -> f64"
    , "  func.func private @kk_range_list(i64, i64) -> i64"
    , "  func.func private @kk_list_zip(i64, i64) -> i64"
    , "  func.func private @kk_list_map_indexed(i64, i64) -> i64"
    , "  func.func private @kk_joinsep_join(i64, i64) -> i64"
    , "  func.func private @kk_unjust(i64) -> i64"
    , "  func.func private @kk_maybe_head(i64) -> i64"
    , "  func.func private @kk_throw(i64) -> i64"
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
    , "  func.func private @kk_thunk_create_forced(i64) -> i64"
    , "  func.func private @kk_thunk_force(i64) -> i64"
    , ""
    , "  // Evidence vector runtime declarations (algebraic effects)"
    , "  func.func private @kk_evv_create(i64) -> i64"
    , "  func.func private @kk_evv_set(i64, i64, i64) -> ()"
    , "  func.func private @kk_evv_get(i64, i64) -> i64"
    , "  func.func private @kk_unhandled_effect() -> i64"
    , ""
    , "  // Abort effect runtime (setjmp/longjmp)"
    , "  func.func private @kk_handler_exec(i64, i64) -> i64"
    , "  func.func private @kk_handler_abort(i64, i64) -> i64"
    , ""
    , "  // Plotkin-style evidence-vector dispatch"
    , "  func.func private @kk_evv_extend(i64, i64, i64) -> i64"
    , "  func.func private @kk_evv_lookup(i64, i64) -> i64"
    , "  func.func private @kk_optab_create(i64) -> i64"
    , "  func.func private @kk_optab_set(i64, i64, i64) -> i64"
    , "  func.func private @kk_optab_get(i64, i64) -> i64"
    , ""
    , "  // Mercury choice effect runtime (multi-shot backtracking)"
    , "  func.func private @mercury_choose() -> i64"
    , "  func.func private @mercury_collect_choices(i64) -> i64"
    , "  func.func private @mercury_fail() -> i64"
    , "  func.func private @mercury_exn_fail() -> i64"
    , ""
    , "  // Idris2 bridge runtime stubs"
    , "  func.func private @idris2_putStr(i64, i64) -> i64"
    , "  func.func private @idris_str_head(i64) -> i64"
    , "  func.func private @idris_crash(i64, i64) -> i64"
    , "  func.func private @_raise(i64) -> i64"
    , "  func.func private @idris_double_sin(i64) -> i64"
    , "  func.func private @idris_double_cos(i64) -> i64"
    , "  func.func private @idris_double_tan(i64) -> i64"
    , "  func.func private @idris_double_asin(i64) -> i64"
    , "  func.func private @idris_double_acos(i64) -> i64"
    , "  func.func private @idris_double_atan(i64) -> i64"
    , "  func.func private @idris_double_sqrt(i64) -> i64"
    , "  func.func private @idris_double_exp(i64) -> i64"
    , "  func.func private @idris_double_log(i64) -> i64"
    , "  func.func private @idris_double_floor(i64) -> i64"
    , "  func.func private @idris_double_ceiling(i64) -> i64"
    , "  func.func private @idris_double_pow(i64, i64) -> i64"
    , "  func.func private @idris_double_add(i64, i64) -> i64"
    , "  func.func private @idris_double_sub(i64, i64) -> i64"
    , "  func.func private @idris_double_mul(i64, i64) -> i64"
    , "  func.func private @idris_double_div(i64, i64) -> i64"
    , "  func.func private @idris_double_neg(i64) -> i64"
    , "  func.func private @idris_double_lt(i64, i64) -> i64"
    , "  func.func private @idris_double_lte(i64, i64) -> i64"
    , "  func.func private @idris_double_eq(i64, i64) -> i64"
    , "  func.func private @idris_double_gte(i64, i64) -> i64"
    , "  func.func private @idris_double_gt(i64, i64) -> i64"
    , "  func.func private @idris2_newIORef(i64, i64, i64) -> i64"
    , "  func.func private @idris2_readIORef(i64, i64, i64) -> i64"
    , "  func.func private @idris2_writeIORef(i64, i64, i64, i64) -> i64"
    , "  func.func private @cast_Integer_Double(i64) -> i64"
    , "  func.func private @cast_Double_Int(i64) -> i64"
    , "  func.func private @cast_Double_String(i64) -> i64"
    , ""
    , "  // Mercury surd-mercury integer-module substitution (i64-based)"
    , "  func.func private @integer_zero() -> i64"
    , "  func.func private @integer_one() -> i64"
    , "  func.func private @integer_is_zero(i64) -> i64"
    , "  func.func private @integer_zl(i64, i64) -> i64"
    , "  func.func private @integer_zg(i64, i64) -> i64"
    , "  func.func private @integer_zezl(i64, i64) -> i64"
    , "  func.func private @integer_zgze(i64, i64) -> i64"
    , "  func.func private @integer_zp(i64, i64) -> i64"
    , "  func.func private @integer_zm(i64, i64) -> i64"
    , "  func.func private @integer_zt(i64, i64) -> i64"
    , "  func.func private @integer_zs(i64, i64) -> i64"
    , "  func.func private @integer_abs(i64) -> i64"
    , "  func.func private @integer_signum(i64) -> i64"
    , "  func.func private @integer_float(i64) -> i64"
    , "  func.func private @integer_to_string(i64) -> i64"
    , "  func.func private @integer___(i64, i64) -> i64"
    , "  func.func private @integer_rem(i64, i64) -> i64"
    , "  func.func private @integer_mod(i64, i64) -> i64"
    , "  func.func private @integer_neg(i64) -> i64"
    , "  func.func private @int_zp(i64, i64) -> i64"
    , "  func.func private @int_zm(i64, i64) -> i64"
    , "  func.func private @int_zt(i64, i64) -> i64"
    , "  func.func private @int_zs(i64, i64) -> i64"
    , "  func.func private @int_zl(i64, i64) -> i64"
    , "  func.func private @int_zg(i64, i64) -> i64"
    , "  func.func private @int_zezl(i64, i64) -> i64"
    , "  func.func private @int_zgze(i64, i64) -> i64"
    , "  func.func private @int_zezeze(i64, i64) -> i64"
    , "  func.func private @int_rem(i64, i64) -> i64"
    , "  func.func private @int_mod(i64, i64) -> i64"
    , "  func.func private @int_max(i64, i64) -> i64"
    , "  func.func private @int_min(i64, i64) -> i64"
    , "  func.func private @int_abs(i64) -> i64"
    , "  func.func private @int_neg(i64) -> i64"
    , "  func.func private @integer_divide_with_rem(i64, i64, i64) -> i64"
    , "  func.func private @unify(i64, i64) -> i64"
    , "  func.func private @io_format(i64, i64, i64) -> i64"
    , "  func.func private @require_error(i64) -> i64"
    , "  func.func private @require_func_error(i64, i64, i64) -> i64"
    , "  func.func private @integer_integer(i64) -> i64"
    , "  func.func private @rational() -> i64"
    , "  func.func private @apply__2(i64, i64) -> i64"
    , "  func.func private @apply__3(i64, i64, i64) -> i64"
    , "  func.func private @call__2(i64, i64) -> i64"
    , "  func.func private @call__3(i64, i64, i64) -> i64"
    , "  func.func private @class_method_call__2(i64, i64) -> i64"
    , "  func.func private @class_method_call__3(i64, i64, i64) -> i64"
    , "  func.func private @list_foldl(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @list_map(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_filter(i64, i64, i64) -> i64"
    , "  func.func private @list_length(i64, i64) -> i64"
    , "  func.func private @list_append(i64, i64, i64) -> i64"
    , "  func.func private @list_reverse(i64, i64) -> i64"
    , "  func.func private @list_condense(i64, i64) -> i64"
    , "  func.func private @list_duplicate(i64, i64, i64) -> i64"
    , "  func.func private @list_last(i64, i64) -> i64"
    , "  func.func private @list_member(i64, i64, i64) -> i64"
    , "  func.func private @list_det_index0(i64, i64, i64) -> i64"
    , "  func.func private @list_det_replace_nth(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_filter_map(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_sort(i64, i64) -> i64"
    , "  func.func private @list_foldl2(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @apply__4(i64, i64, i64, i64) -> i64"
    , "  func.func private @call__4(i64, i64, i64, i64) -> i64"
    , "  func.func private @class_method_call__4(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_zpzp(i64, i64, i64) -> i64"
    , "  func.func private @string_zpzp(i64, i64) -> i64"
    , "  func.func private @char_det_from_int(i64) -> i64"
    , "  func.func private @char_to_int(i64) -> i64"
    , "  func.func private @float_zp(i64, i64) -> i64"
    , "  func.func private @float_zm(i64, i64) -> i64"
    , "  func.func private @float_zt(i64, i64) -> i64"
    , "  func.func private @float_zs(i64, i64) -> i64"
    , "  func.func private @float_zl(i64, i64) -> i64"
    , "  func.func private @float_zg(i64, i64) -> i64"
    , "  func.func private @float_zezl(i64, i64) -> i64"
    , "  func.func private @float_zgze(i64, i64) -> i64"
    , "  func.func private @float_zezeze(i64, i64) -> i64"
    , "  func.func private @float_abs(i64) -> i64"
    , "  func.func private @float_neg(i64) -> i64"
    , "  func.func private @float_max(i64, i64) -> i64"
    , "  func.func private @float_min(i64, i64) -> i64"
    , "  func.func private @float_float(i64) -> i64"
    , "  func.func private @float_round(i64) -> i64"
    , "  func.func private @float_truncate(i64) -> i64"
    , "  func.func private @float_floor(i64) -> i64"
    , "  func.func private @float_ceiling(i64) -> i64"
    , "  func.func private @float_is_nan(i64) -> i64"
    , "  func.func private @float_is_inf(i64) -> i64"
    , "  func.func private @math_pi() -> i64"
    , "  func.func private @math_e() -> i64"
    , "  func.func private @math_sqrt(i64) -> i64"
    , "  func.func private @math_sin(i64) -> i64"
    , "  func.func private @math_cos(i64) -> i64"
    , "  func.func private @math_tan(i64) -> i64"
    , "  func.func private @math_asin(i64) -> i64"
    , "  func.func private @math_acos(i64) -> i64"
    , "  func.func private @math_atan(i64) -> i64"
    , "  func.func private @math_atan2(i64, i64) -> i64"
    , "  func.func private @math_exp(i64) -> i64"
    , "  func.func private @math_ln(i64) -> i64"
    , "  func.func private @math_log10(i64) -> i64"
    , "  func.func private @math_log2(i64) -> i64"
    , "  func.func private @math_pow(i64, i64) -> i64"
    , "  func.func private @math_sinh(i64) -> i64"
    , "  func.func private @math_cosh(i64) -> i64"
    , "  func.func private @math_tanh(i64) -> i64"
    , "  func.func private @require_unexpected(i64, i64, i64) -> i64"
    , "  func.func private @private_builtin_type_info_from_typeclass_info(i64, i64) -> i64"
    , "  func.func private @private_builtin_superclass_from_typeclass_info(i64, i64) -> i64"
    , "  func.func private @private_builtin_instance_constructor_from_typeclass_info(i64, i64) -> i64"
    , "  func.func private @string_length(i64) -> i64"
    , "  func.func private @string_append(i64, i64) -> i64"
    , "  func.func private @string_from_char(i64) -> i64"
    , "  func.func private @string_int_to_string(i64) -> i64"
    , "  func.func private @string_append_list(i64) -> i64"
    , "  func.func private @string_join_list(i64, i64) -> i64"
    , "  func.func private @string_index(i64, i64) -> i64"
    , "  func.func private @string_contains_char(i64, i64) -> i64"
    , "  func.func private @string_duplicate_char(i64, i64) -> i64"
    , "  func.func private @string_to_int(i64) -> i64"
    , "  func.func private @string_to_float(i64) -> i64"
    , "  func.func private @string_sub_string_search(i64, i64) -> i64"
    , "  func.func private @list_all_true(i64, i64) -> i64"
    , "  func.func private @list_drop(i64, i64) -> i64"
    , "  func.func private @list_delete_all(i64, i64, i64) -> i64"
    , "  func.func private @list_det_tail(i64, i64) -> i64"
    , "  func.func private @list_det_split_last(i64, i64) -> i64"
    , "  func.func private @list_take_upto(i64, i64, i64) -> i64"
    , "  func.func private @list_sort_and_remove_dups(i64, i64) -> i64"
    , "  func.func private @list_map_corresponding(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @int_even(i64) -> i64"
    , "  func.func private @int_odd(i64) -> i64"
    , "  func.func private @integer_pow(i64, i64) -> i64"
    , "  func.func private @integer_det_to_int(i64) -> i64"
    , "  func.func private @float_round_to_int(i64) -> i64"
    , "  func.func private @float_truncate_to_int(i64) -> i64"
    , "  func.func private @mercury_not(i64) -> i64"
    , "  func.func private @builtin_compare(i64, i64, i64, i64) -> i64"
    , "  func.func private @builtin_ordering(i64, i64, i64) -> i64"
    , "  func.func private @list_range(i64, i64) -> i64"
    , "  func.func private @map_init(i64) -> i64"
    , "  func.func private @map_search(i64, i64, i64) -> i64"
    , "  func.func private @map_lookup(i64, i64, i64) -> i64"
    , "  func.func private @map_contains(i64, i64, i64) -> i64"
    , "  func.func private @map_count(i64, i64) -> i64"
    , "  func.func private @map_set(i64, i64, i64, i64) -> i64"
    , "  func.func private @map_det_insert(i64, i64, i64, i64) -> i64"
    , "  func.func private @map_det_update(i64, i64, i64, i64) -> i64"
    , "  func.func private @map_delete(i64, i64, i64) -> i64"
    , "  func.func private @map_from_assoc_list(i64, i64) -> i64"
    , "  func.func private @map_values(i64, i64) -> i64"
    , "  func.func private @map_keys(i64, i64) -> i64"
    , "  func.func private @map_foldl(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @set_init(i64) -> i64"
    , "  func.func private @set_member(i64, i64, i64) -> i64"
    , "  func.func private @set_insert(i64, i64, i64) -> i64"
    , "  func.func private @set_delete(i64, i64, i64) -> i64"
    , "  func.func private @set_contains(i64, i64, i64) -> i64"
    , "  func.func private @set_make_singleton_set(i64, i64) -> i64"
    , "  func.func private @set_count(i64, i64) -> i64"
    , "  func.func private @set_to_sorted_list(i64, i64) -> i64"
    , "  func.func private @set_from_list(i64, i64) -> i64"
    , "  func.func private @set_is_empty(i64, i64) -> i64"
    , "  func.func private @set_union(i64, i64, i64) -> i64"
    , "  func.func private @set_intersect(i64, i64, i64) -> i64"
    , "  func.func private @set_difference(i64, i64, i64) -> i64"
    , ""
    , externDeclText
    , kokaBuiltinText
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    , mainWrapper
    ] <> "}\n"

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
      cyclicDefs = Set.fromList
        [ qualifyDefName d
        | (d, ci) <- zip renamedDefs (analyzeCycles prog)
        , ciCyclic ci
        ]
      extRtFns = externalRuntimeFns
      extRtArity = externalRuntimeArity
      initState = EmitState 0 [] Set.empty Map.empty [] Map.empty True
                         (qualifiedTopNames `Set.union` extRtFns)
                         (buildTopFnArity modPrefix renamedDefs `Map.union` extRtArity)
                         Set.empty
                         (assignProgramTags prog)
                         modPrefix
                         Map.empty
                         Map.empty
                         Map.empty
                         cyclicDefs
                         ""
                         extRtFns
                         extRtArity
                         Nothing
                         Set.empty
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
    , "  func.func private @kk_cycle_candidate(i64) -> ()"
    , "  func.func private @kk_set_field(i64, i64, i64) -> ()"
    , "  func.func private @kk_tag(i64) -> i64"
    , "  func.func private @kk_field(i64, i64) -> i64"
    , "  func.func private @kk_structural_eq(i64, i64) -> i64"
    , "  func.func private @kk_println_con(i64) -> ()"
    , "  func.func private @kk_println_haskell_chars(i64) -> ()"
    , "  func.func private @kk_print_haskell_chars(i64) -> ()"
    , "  func.func private @kk_int_to_haskell_chars(i64, i64) -> i64"
    , "  func.func private @kk_int_list_to_haskell_chars(i64, i64) -> i64"
    , "  func.func private @kk_haskell_chars_concat(i64, i64) -> i64"
    -- PAP wrapper alias: the emitter constructs PAPs using the bare
    -- intrinsic name (without the `kk_` prefix), and mlir-opt requires
    -- a func.func declaration with that exact name to validate the call.
    -- The C runtime declares haskell_chars_concat as an alias of
    -- kk_haskell_chars_concat (see runtime/kk_runtime.c).
    , "  func.func private @haskell_chars_concat(i64, i64) -> i64"
    , "  func.func private @dummy_show_caf() -> i64"
    , "  func.func private @kk_rust_args_pack(i64, i64) -> i64"
    , "  func.func private @kk_rust_print_dispatch(i64) -> i64"
    , "  func.func private @kk_rust_field_safe(i64, i64) -> i64"
    , "  func.func private @kk_rust_arg_debug(i64) -> i64"
    , "  func.func private @kk_rust_arg_lower_hex(i64) -> i64"
    , "  func.func private @kk_rust_arg_upper_hex(i64) -> i64"
    , "  func.func private @kk_rust_arg_octal(i64) -> i64"
    , "  func.func private @kk_rust_arg_binary(i64) -> i64"
    , "  func.func private @kk_rust_arg_u32(i64) -> i64"
    , "  func.func private @kk_rust_arg_i32(i64) -> i64"
    , "  func.func private @kk_rust_arg_u64(i64) -> i64"
    , "  func.func private @kk_rust_arg_u16(i64) -> i64"
    , "  func.func private @kk_rust_arg_i16(i64) -> i64"
    , "  func.func private @kk_rust_arg_u8(i64) -> i64"
    , "  func.func private @kk_rust_arg_i8(i64) -> i64"
    , "  func.func private @kk_rust_arg_f64(i64) -> i64"
    , "  func.func private @kk_rust_arg_f32(i64) -> i64"
    , "  func.func private @kk_rust_struct_0(i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_1(i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_2(i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_3(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_4(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_5(i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_6(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_7(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_8(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_args_pack(i64, i64) -> i64"
    , "  func.func private @rust_print_dispatch(i64) -> i64"
    , "  func.func private @rust_field_safe(i64, i64) -> i64"
    , "  func.func private @rust_arg_debug(i64) -> i64"
    , "  func.func private @rust_arg_lower_hex(i64) -> i64"
    , "  func.func private @rust_arg_upper_hex(i64) -> i64"
    , "  func.func private @rust_arg_octal(i64) -> i64"
    , "  func.func private @rust_arg_binary(i64) -> i64"
    , "  func.func private @rust_arg_u32(i64) -> i64"
    , "  func.func private @rust_arg_i32(i64) -> i64"
    , "  func.func private @rust_arg_u64(i64) -> i64"
    , "  func.func private @rust_arg_u16(i64) -> i64"
    , "  func.func private @rust_arg_i16(i64) -> i64"
    , "  func.func private @rust_arg_u8(i64) -> i64"
    , "  func.func private @rust_arg_i8(i64) -> i64"
    , "  func.func private @rust_arg_f64(i64) -> i64"
    , "  func.func private @rust_arg_f32(i64) -> i64"
    , "  func.func private @rust_struct_0(i64, i64) -> i64"
    , "  func.func private @rust_struct_1(i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_2(i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_3(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_4(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_5(i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_6(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_7(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_8(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , ""
    , "  func.func private @kk_string_from_literal(i64, i64) -> i64"
    , "  func.func private @kk_string_from_cstr(i64) -> i64"
    , "  func.func private @kk_string_empty() -> i64"
    , "  func.func private @kk_string_from_char(i64) -> i64"
    , "  func.func private @kk_list_map(i64, i64) -> i64"
    , "  func.func private @kk_list_filter(i64, i64) -> i64"
    , "  func.func private @kk_list_foldl(i64, i64, i64) -> i64"
    , "  func.func private @kk_list_all(i64, i64) -> i64"
    , "  func.func private @kk_list_any(i64, i64) -> i64"
    , "  func.func private @kk_list_drop(i64, i64) -> i64"
    , "  func.func private @kk_list_take(i64, i64) -> i64"
    , "  func.func private @kk_list_flatmap(i64, i64) -> i64"
    , "  func.func private @kk_list_filter_map(i64, i64) -> i64"
    , "  func.func private @kk_list_foreach(i64, i64) -> i64"
    , "  func.func private @kk_list_concat(i64, i64) -> i64"
    , "  func.func private @kk_list_length(i64) -> i64"
    , "  func.func private @kk_is_nil(i64) -> i64"
    , "  func.func private @cos(f64) -> f64"
    , "  func.func private @sin(f64) -> f64"
    , "  func.func private @tan(f64) -> f64"
    , "  func.func private @sqrt(f64) -> f64"
    , "  func.func private @log(f64) -> f64"
    , "  func.func private @exp(f64) -> f64"
    , "  func.func private @atan2(f64, f64) -> f64"
    , "  func.func private @pow(f64, f64) -> f64"
    , "  func.func private @fmod(f64, f64) -> f64"
    , "  func.func private @kk_range_list(i64, i64) -> i64"
    , "  func.func private @kk_list_zip(i64, i64) -> i64"
    , "  func.func private @kk_list_map_indexed(i64, i64) -> i64"
    , "  func.func private @kk_joinsep_join(i64, i64) -> i64"
    , "  func.func private @kk_unjust(i64) -> i64"
    , "  func.func private @kk_maybe_head(i64) -> i64"
    , "  func.func private @kk_throw(i64) -> i64"
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
    , "  func.func private @kk_handler_exec(i64, i64) -> i64"
    , "  func.func private @kk_handler_abort(i64, i64) -> i64"
    , ""
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    ] <> "}\n"

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
      cyclicDefs = Set.fromList
        [ qualifyDefName d
        | (d, ci) <- zip renamedDefs (analyzeCycles prog)
        , ciCyclic ci
        ]
      extRtFns = externalRuntimeFns
      extRtArity = externalRuntimeArity
      initState = EmitState 0 [] Set.empty Map.empty [] Map.empty False
                         qualifiedTopNames
                         (buildTopFnArity modPrefix renamedDefs)
                         Set.empty
                         (assignProgramTags prog)
                         modPrefix
                         Map.empty
                         Map.empty
                         Map.empty
                         cyclicDefs
                         ""
                         extRtFns
                         extRtArity
                         Nothing
                         Set.empty
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
    , "  func.func private @kk_cycle_candidate(i64) -> ()"
    , "  func.func private @kk_set_field(i64, i64, i64) -> ()"
    , "  func.func private @kk_tag(i64) -> i64"
    , "  func.func private @kk_field(i64, i64) -> i64"
    , "  func.func private @kk_structural_eq(i64, i64) -> i64"
    , "  func.func private @kk_println_con(i64) -> ()"
    , "  func.func private @kk_println_haskell_chars(i64) -> ()"
    , "  func.func private @kk_print_haskell_chars(i64) -> ()"
    , "  func.func private @kk_int_to_haskell_chars(i64, i64) -> i64"
    , "  func.func private @kk_int_list_to_haskell_chars(i64, i64) -> i64"
    , "  func.func private @kk_haskell_chars_concat(i64, i64) -> i64"
    -- PAP wrapper alias: the emitter constructs PAPs using the bare
    -- intrinsic name (without the `kk_` prefix), and mlir-opt requires
    -- a func.func declaration with that exact name to validate the call.
    -- The C runtime declares haskell_chars_concat as an alias of
    -- kk_haskell_chars_concat (see runtime/kk_runtime.c).
    , "  func.func private @haskell_chars_concat(i64, i64) -> i64"
    , "  func.func private @dummy_show_caf() -> i64"
    , "  func.func private @kk_rust_args_pack(i64, i64) -> i64"
    , "  func.func private @kk_rust_print_dispatch(i64) -> i64"
    , "  func.func private @kk_rust_field_safe(i64, i64) -> i64"
    , "  func.func private @kk_rust_arg_debug(i64) -> i64"
    , "  func.func private @kk_rust_arg_lower_hex(i64) -> i64"
    , "  func.func private @kk_rust_arg_upper_hex(i64) -> i64"
    , "  func.func private @kk_rust_arg_octal(i64) -> i64"
    , "  func.func private @kk_rust_arg_binary(i64) -> i64"
    , "  func.func private @kk_rust_arg_u32(i64) -> i64"
    , "  func.func private @kk_rust_arg_i32(i64) -> i64"
    , "  func.func private @kk_rust_arg_u64(i64) -> i64"
    , "  func.func private @kk_rust_arg_u16(i64) -> i64"
    , "  func.func private @kk_rust_arg_i16(i64) -> i64"
    , "  func.func private @kk_rust_arg_u8(i64) -> i64"
    , "  func.func private @kk_rust_arg_i8(i64) -> i64"
    , "  func.func private @kk_rust_arg_f64(i64) -> i64"
    , "  func.func private @kk_rust_arg_f32(i64) -> i64"
    , "  func.func private @kk_rust_struct_0(i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_1(i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_2(i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_3(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_4(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_5(i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_6(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_7(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @kk_rust_struct_8(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_args_pack(i64, i64) -> i64"
    , "  func.func private @rust_print_dispatch(i64) -> i64"
    , "  func.func private @rust_field_safe(i64, i64) -> i64"
    , "  func.func private @rust_arg_debug(i64) -> i64"
    , "  func.func private @rust_arg_lower_hex(i64) -> i64"
    , "  func.func private @rust_arg_upper_hex(i64) -> i64"
    , "  func.func private @rust_arg_octal(i64) -> i64"
    , "  func.func private @rust_arg_binary(i64) -> i64"
    , "  func.func private @rust_arg_u32(i64) -> i64"
    , "  func.func private @rust_arg_i32(i64) -> i64"
    , "  func.func private @rust_arg_u64(i64) -> i64"
    , "  func.func private @rust_arg_u16(i64) -> i64"
    , "  func.func private @rust_arg_i16(i64) -> i64"
    , "  func.func private @rust_arg_u8(i64) -> i64"
    , "  func.func private @rust_arg_i8(i64) -> i64"
    , "  func.func private @rust_arg_f64(i64) -> i64"
    , "  func.func private @rust_arg_f32(i64) -> i64"
    , "  func.func private @rust_struct_0(i64, i64) -> i64"
    , "  func.func private @rust_struct_1(i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_2(i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_3(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_4(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_5(i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_6(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_7(i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @rust_struct_8(i64, i64, i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
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
    , "  // Abort effect runtime (setjmp/longjmp)"
    , "  func.func private @kk_handler_exec(i64, i64) -> i64"
    , "  func.func private @kk_handler_abort(i64, i64) -> i64"
    , ""
    , "  // Plotkin-style evidence-vector dispatch"
    , "  func.func private @kk_evv_extend(i64, i64, i64) -> i64"
    , "  func.func private @kk_evv_lookup(i64, i64) -> i64"
    , "  func.func private @kk_optab_create(i64) -> i64"
    , "  func.func private @kk_optab_set(i64, i64, i64) -> i64"
    , "  func.func private @kk_optab_get(i64, i64) -> i64"
    , ""
    , "  // Mercury choice effect runtime"
    , "  func.func private @mercury_choose() -> i64"
    , "  func.func private @mercury_collect_choices(i64) -> i64"
    , "  func.func private @mercury_fail() -> i64"
    , "  func.func private @mercury_exn_fail() -> i64"
    , ""
    , "  // Idris2 bridge runtime stubs"
    , "  func.func private @idris2_putStr(i64, i64) -> i64"
    , "  func.func private @idris_str_head(i64) -> i64"
    , "  func.func private @idris_crash(i64, i64) -> i64"
    , "  func.func private @_raise(i64) -> i64"
    , "  func.func private @idris_double_sin(i64) -> i64"
    , "  func.func private @idris_double_cos(i64) -> i64"
    , "  func.func private @idris_double_tan(i64) -> i64"
    , "  func.func private @idris_double_asin(i64) -> i64"
    , "  func.func private @idris_double_acos(i64) -> i64"
    , "  func.func private @idris_double_atan(i64) -> i64"
    , "  func.func private @idris_double_sqrt(i64) -> i64"
    , "  func.func private @idris_double_exp(i64) -> i64"
    , "  func.func private @idris_double_log(i64) -> i64"
    , "  func.func private @idris_double_floor(i64) -> i64"
    , "  func.func private @idris_double_ceiling(i64) -> i64"
    , "  func.func private @idris_double_pow(i64, i64) -> i64"
    , "  func.func private @idris_double_add(i64, i64) -> i64"
    , "  func.func private @idris_double_sub(i64, i64) -> i64"
    , "  func.func private @idris_double_mul(i64, i64) -> i64"
    , "  func.func private @idris_double_div(i64, i64) -> i64"
    , "  func.func private @idris_double_neg(i64) -> i64"
    , "  func.func private @idris_double_lt(i64, i64) -> i64"
    , "  func.func private @idris_double_lte(i64, i64) -> i64"
    , "  func.func private @idris_double_eq(i64, i64) -> i64"
    , "  func.func private @idris_double_gte(i64, i64) -> i64"
    , "  func.func private @idris_double_gt(i64, i64) -> i64"
    , "  func.func private @idris2_newIORef(i64, i64, i64) -> i64"
    , "  func.func private @idris2_readIORef(i64, i64, i64) -> i64"
    , "  func.func private @idris2_writeIORef(i64, i64, i64, i64) -> i64"
    , "  func.func private @cast_Integer_Double(i64) -> i64"
    , "  func.func private @cast_Double_Int(i64) -> i64"
    , "  func.func private @cast_Double_String(i64) -> i64"
    , ""
    , "  // Mercury surd-mercury integer-module substitution (i64-based)"
    , "  func.func private @integer_zero() -> i64"
    , "  func.func private @integer_one() -> i64"
    , "  func.func private @integer_is_zero(i64) -> i64"
    , "  func.func private @integer_zl(i64, i64) -> i64"
    , "  func.func private @integer_zg(i64, i64) -> i64"
    , "  func.func private @integer_zezl(i64, i64) -> i64"
    , "  func.func private @integer_zgze(i64, i64) -> i64"
    , "  func.func private @integer_zp(i64, i64) -> i64"
    , "  func.func private @integer_zm(i64, i64) -> i64"
    , "  func.func private @integer_zt(i64, i64) -> i64"
    , "  func.func private @integer_zs(i64, i64) -> i64"
    , "  func.func private @integer_abs(i64) -> i64"
    , "  func.func private @integer_signum(i64) -> i64"
    , "  func.func private @integer_float(i64) -> i64"
    , "  func.func private @integer_to_string(i64) -> i64"
    , "  func.func private @integer___(i64, i64) -> i64"
    , "  func.func private @integer_rem(i64, i64) -> i64"
    , "  func.func private @integer_mod(i64, i64) -> i64"
    , "  func.func private @integer_neg(i64) -> i64"
    , "  func.func private @int_zp(i64, i64) -> i64"
    , "  func.func private @int_zm(i64, i64) -> i64"
    , "  func.func private @int_zt(i64, i64) -> i64"
    , "  func.func private @int_zs(i64, i64) -> i64"
    , "  func.func private @int_zl(i64, i64) -> i64"
    , "  func.func private @int_zg(i64, i64) -> i64"
    , "  func.func private @int_zezl(i64, i64) -> i64"
    , "  func.func private @int_zgze(i64, i64) -> i64"
    , "  func.func private @int_zezeze(i64, i64) -> i64"
    , "  func.func private @int_rem(i64, i64) -> i64"
    , "  func.func private @int_mod(i64, i64) -> i64"
    , "  func.func private @int_max(i64, i64) -> i64"
    , "  func.func private @int_min(i64, i64) -> i64"
    , "  func.func private @int_abs(i64) -> i64"
    , "  func.func private @int_neg(i64) -> i64"
    , "  func.func private @integer_divide_with_rem(i64, i64, i64) -> i64"
    , "  func.func private @unify(i64, i64) -> i64"
    , "  func.func private @io_format(i64, i64, i64) -> i64"
    , "  func.func private @require_error(i64) -> i64"
    , "  func.func private @require_func_error(i64, i64, i64) -> i64"
    , "  func.func private @integer_integer(i64) -> i64"
    , "  func.func private @rational() -> i64"
    , "  func.func private @apply__2(i64, i64) -> i64"
    , "  func.func private @apply__3(i64, i64, i64) -> i64"
    , "  func.func private @call__2(i64, i64) -> i64"
    , "  func.func private @call__3(i64, i64, i64) -> i64"
    , "  func.func private @class_method_call__2(i64, i64) -> i64"
    , "  func.func private @class_method_call__3(i64, i64, i64) -> i64"
    , "  func.func private @list_foldl(i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @list_map(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_filter(i64, i64, i64) -> i64"
    , "  func.func private @list_length(i64, i64) -> i64"
    , "  func.func private @list_append(i64, i64, i64) -> i64"
    , "  func.func private @list_reverse(i64, i64) -> i64"
    , "  func.func private @list_condense(i64, i64) -> i64"
    , "  func.func private @list_duplicate(i64, i64, i64) -> i64"
    , "  func.func private @list_last(i64, i64) -> i64"
    , "  func.func private @list_member(i64, i64, i64) -> i64"
    , "  func.func private @list_det_index0(i64, i64, i64) -> i64"
    , "  func.func private @list_det_replace_nth(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_filter_map(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_sort(i64, i64) -> i64"
    , "  func.func private @list_foldl2(i64, i64, i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @apply__4(i64, i64, i64, i64) -> i64"
    , "  func.func private @call__4(i64, i64, i64, i64) -> i64"
    , "  func.func private @class_method_call__4(i64, i64, i64, i64) -> i64"
    , "  func.func private @list_zpzp(i64, i64, i64) -> i64"
    , "  func.func private @string_zpzp(i64, i64) -> i64"
    , "  func.func private @char_det_from_int(i64) -> i64"
    , "  func.func private @char_to_int(i64) -> i64"
    , "  func.func private @float_zp(i64, i64) -> i64"
    , "  func.func private @float_zm(i64, i64) -> i64"
    , "  func.func private @float_zt(i64, i64) -> i64"
    , "  func.func private @float_zs(i64, i64) -> i64"
    , "  func.func private @float_zl(i64, i64) -> i64"
    , "  func.func private @float_zg(i64, i64) -> i64"
    , "  func.func private @float_zezl(i64, i64) -> i64"
    , "  func.func private @float_zgze(i64, i64) -> i64"
    , "  func.func private @float_zezeze(i64, i64) -> i64"
    , "  func.func private @float_abs(i64) -> i64"
    , "  func.func private @float_neg(i64) -> i64"
    , "  func.func private @float_max(i64, i64) -> i64"
    , "  func.func private @float_min(i64, i64) -> i64"
    , "  func.func private @float_float(i64) -> i64"
    , "  func.func private @float_round(i64) -> i64"
    , "  func.func private @float_truncate(i64) -> i64"
    , "  func.func private @float_floor(i64) -> i64"
    , "  func.func private @float_ceiling(i64) -> i64"
    , "  func.func private @float_is_nan(i64) -> i64"
    , "  func.func private @float_is_inf(i64) -> i64"
    , "  func.func private @math_pi() -> i64"
    , "  func.func private @math_e() -> i64"
    , "  func.func private @math_sqrt(i64) -> i64"
    , "  func.func private @math_sin(i64) -> i64"
    , "  func.func private @math_cos(i64) -> i64"
    , "  func.func private @math_tan(i64) -> i64"
    , "  func.func private @math_asin(i64) -> i64"
    , "  func.func private @math_acos(i64) -> i64"
    , "  func.func private @math_atan(i64) -> i64"
    , "  func.func private @math_atan2(i64, i64) -> i64"
    , "  func.func private @math_exp(i64) -> i64"
    , "  func.func private @math_ln(i64) -> i64"
    , "  func.func private @math_log10(i64) -> i64"
    , "  func.func private @math_log2(i64) -> i64"
    , "  func.func private @math_pow(i64, i64) -> i64"
    , "  func.func private @math_sinh(i64) -> i64"
    , "  func.func private @math_cosh(i64) -> i64"
    , "  func.func private @math_tanh(i64) -> i64"
    , "  func.func private @require_unexpected(i64, i64, i64) -> i64"
    , "  func.func private @private_builtin_type_info_from_typeclass_info(i64, i64) -> i64"
    , "  func.func private @private_builtin_superclass_from_typeclass_info(i64, i64) -> i64"
    , "  func.func private @private_builtin_instance_constructor_from_typeclass_info(i64, i64) -> i64"
    , "  func.func private @string_length(i64) -> i64"
    , "  func.func private @string_append(i64, i64) -> i64"
    , "  func.func private @string_from_char(i64) -> i64"
    , "  func.func private @string_int_to_string(i64) -> i64"
    , "  func.func private @string_append_list(i64) -> i64"
    , "  func.func private @string_join_list(i64, i64) -> i64"
    , "  func.func private @string_index(i64, i64) -> i64"
    , "  func.func private @string_contains_char(i64, i64) -> i64"
    , "  func.func private @string_duplicate_char(i64, i64) -> i64"
    , "  func.func private @string_to_int(i64) -> i64"
    , "  func.func private @string_to_float(i64) -> i64"
    , "  func.func private @string_sub_string_search(i64, i64) -> i64"
    , "  func.func private @list_all_true(i64, i64) -> i64"
    , "  func.func private @list_drop(i64, i64) -> i64"
    , "  func.func private @list_delete_all(i64, i64, i64) -> i64"
    , "  func.func private @list_det_tail(i64, i64) -> i64"
    , "  func.func private @list_det_split_last(i64, i64) -> i64"
    , "  func.func private @list_take_upto(i64, i64, i64) -> i64"
    , "  func.func private @list_sort_and_remove_dups(i64, i64) -> i64"
    , "  func.func private @list_map_corresponding(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @int_even(i64) -> i64"
    , "  func.func private @int_odd(i64) -> i64"
    , "  func.func private @integer_pow(i64, i64) -> i64"
    , "  func.func private @integer_det_to_int(i64) -> i64"
    , "  func.func private @float_round_to_int(i64) -> i64"
    , "  func.func private @float_truncate_to_int(i64) -> i64"
    , "  func.func private @mercury_not(i64) -> i64"
    , "  func.func private @builtin_compare(i64, i64, i64, i64) -> i64"
    , "  func.func private @builtin_ordering(i64, i64, i64) -> i64"
    , "  func.func private @list_range(i64, i64) -> i64"
    , "  func.func private @map_init(i64) -> i64"
    , "  func.func private @map_search(i64, i64, i64) -> i64"
    , "  func.func private @map_lookup(i64, i64, i64) -> i64"
    , "  func.func private @map_contains(i64, i64, i64) -> i64"
    , "  func.func private @map_count(i64, i64) -> i64"
    , "  func.func private @map_set(i64, i64, i64, i64) -> i64"
    , "  func.func private @map_det_insert(i64, i64, i64, i64) -> i64"
    , "  func.func private @map_det_update(i64, i64, i64, i64) -> i64"
    , "  func.func private @map_delete(i64, i64, i64) -> i64"
    , "  func.func private @map_from_assoc_list(i64, i64) -> i64"
    , "  func.func private @map_values(i64, i64) -> i64"
    , "  func.func private @map_keys(i64, i64) -> i64"
    , "  func.func private @map_foldl(i64, i64, i64, i64, i64, i64) -> i64"
    , "  func.func private @set_init(i64) -> i64"
    , "  func.func private @set_member(i64, i64, i64) -> i64"
    , "  func.func private @set_insert(i64, i64, i64) -> i64"
    , "  func.func private @set_delete(i64, i64, i64) -> i64"
    , "  func.func private @set_contains(i64, i64, i64) -> i64"
    , "  func.func private @set_make_singleton_set(i64, i64) -> i64"
    , "  func.func private @set_count(i64, i64) -> i64"
    , "  func.func private @set_to_sorted_list(i64, i64) -> i64"
    , "  func.func private @set_from_list(i64, i64) -> i64"
    , "  func.func private @set_is_empty(i64, i64) -> i64"
    , "  func.func private @set_union(i64, i64, i64) -> i64"
    , "  func.func private @set_intersect(i64, i64, i64) -> i64"
    , "  func.func private @set_difference(i64, i64, i64) -> i64"
    , ""
    , externDeclText
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    ] <> "}\n"

-- | Dead code elimination: keep only definitions reachable from main.
-- This prevents the simplifier's dead code from causing link errors
-- (e.g. `addN = +` when `main` is constant-folded and doesn't call addN).
reachableDefs :: [Def] -> [Def]
reachableDefs defs =
  -- Build lookup tables using multiple name forms for each def:
  -- - bare name: "myMap"
  -- - module-qualified with /: "HigherOrder/myMap"
  -- - module-qualified with .: "HigherOrder.myMap"
  let defEntries = concatMap (\d ->
        let n = nameText (qnameName (defName d))
            m = qnameModule (defName d)
            keys = [n] ++
                   [m <> "/" <> n | not (T.null m)] ++
                   [m <> "." <> n | not (T.null m)]
        in [(k, d) | k <- keys]
        ) defs
      defMap = Map.fromList defEntries
      freeVarsExpr :: Expr -> Set Text
      freeVarsExpr (EVar n)          = Set.singleton (nameText n)
      freeVarsExpr (ELit _)          = Set.empty
      freeVarsExpr (ECon qn)         = Set.singleton (nameText (qnameName qn))
      freeVarsExpr (EApp f args)     = Set.unions (freeVarsExpr f : map freeVarsExpr args)
      freeVarsExpr (ELam _ body)     = freeVarsExpr body
      freeVarsExpr (ELet bgs body)   = Set.unions (freeVarsExpr body :
                                          [freeVarsExpr (bindExpr b) | bg <- bgs, b <- bg])
      freeVarsExpr (ECase e brs)     = Set.union (freeVarsExpr e)
                                          (Set.unions [freeVarsExpr (branchBody b) | b <- brs])
      freeVarsExpr (EDelay e)        = freeVarsExpr e
      freeVarsExpr (EForce e)        = freeVarsExpr e
      freeVarsExpr (ETypeLam _ e)    = freeVarsExpr e
      freeVarsExpr (ETypeApp e _)    = freeVarsExpr e
      freeVarsExpr (EPerform _ _)    = Set.empty
      freeVarsExpr (EHandle _ _ e)   = freeVarsExpr e
      freeVarsExpr (ERetain e)       = freeVarsExpr e
      freeVarsExpr (EDrop e)         = freeVarsExpr e
      freeVarsExpr (EReuse _ e)      = freeVarsExpr e
      -- Walk from roots collecting reachable definition names
      walk visited [] = visited
      walk visited (n:ns)
        | Set.member n visited = walk visited ns
        | otherwise = case Map.lookup n defMap of
            Nothing -> walk (Set.insert n visited) ns
            Just d  -> let refs = freeVarsExpr (defExpr d)
                           defN = nameText (qnameName (defName d))
                       in walk (Set.insert defN (Set.insert n visited))
                               (Set.toList refs ++ ns)
      -- Start from _frankenstein_main (after rename)
      roots = ["_frankenstein_main"]
      reachable = walk Set.empty roots
  in filter (\d -> nameText (qnameName (defName d)) `Set.member` reachable) defs

-- | Known builtin operator names that don't need their own function definition.
builtinOpNames :: Set Text
builtinOpNames = Set.fromList
  ["+", "-", "*", "/", "mod", "negate", "abs", "signum",
   "==", "/=", "<", ">", "<=", ">=",
   "+#", "-#", "*#", "remInt#", "quotInt#",
   "==#", "/=#", "<#", ">#", "<=#", ">=#",
   "$fNumInt_$c+", "$fNumInt_$c-", "$fNumInt_$c*",
   "tagToEnum#"]

-- | Wrapper function specs for builtins used as first-class values.
-- Returns (wrapperFnName, arity, MLIR body) for builtins that can be
-- passed to higher-order functions like myFoldl(+, 0, xs).
builtinWrapperSpec :: Text -> Maybe (Text, Int, Text)
builtinWrapperSpec name = case name of
  -- Binary arithmetic
  "+"  -> Just ("__kk_builtin_add", 2, binOp "arith.addi")
  "-"  -> Just ("__kk_builtin_sub", 2, binOp "arith.subi")
  "*"  -> Just ("__kk_builtin_mul", 2, binOp "arith.muli")
  "/"  -> Just ("__kk_builtin_div", 2, binOp "arith.divsi")
  "mod" -> Just ("__kk_builtin_mod", 2, binOp "arith.remsi")
  "+#" -> Just ("__kk_builtin_add", 2, binOp "arith.addi")
  "-#" -> Just ("__kk_builtin_sub", 2, binOp "arith.subi")
  "*#" -> Just ("__kk_builtin_mul", 2, binOp "arith.muli")
  "$fNumInt_$c+" -> Just ("__kk_builtin_add", 2, binOp "arith.addi")
  "$fNumInt_$c-" -> Just ("__kk_builtin_sub", 2, binOp "arith.subi")
  "$fNumInt_$c*" -> Just ("__kk_builtin_mul", 2, binOp "arith.muli")
  -- Binary comparisons (return 0 or 1 as i64)
  "==" -> Just ("__kk_builtin_eq", 2, cmpOp "eq")
  "/=" -> Just ("__kk_builtin_ne", 2, cmpOp "ne")
  "<"  -> Just ("__kk_builtin_lt", 2, cmpOp "slt")
  ">"  -> Just ("__kk_builtin_gt", 2, cmpOp "sgt")
  "<=" -> Just ("__kk_builtin_le", 2, cmpOp "sle")
  ">=" -> Just ("__kk_builtin_ge", 2, cmpOp "sge")
  -- Unary
  "negate" -> Just ("__kk_builtin_negate", 1,
    T.unlines ["    %zero = arith.constant 0 : i64", "    %r = arith.subi %zero, %arg0 : i64", "    func.return %r : i64"])
  "abs" -> Just ("__kk_builtin_abs", 1,
    T.unlines [ "    %c63 = arith.constant 63 : i64"
              , "    %sign = arith.shrsi %arg0, %c63 : i64"
              , "    %xored = arith.xori %arg0, %sign : i64"
              , "    %r = arith.subi %xored, %sign : i64"
              , "    func.return %r : i64" ])
  -- tagToEnum# is identity (Bool 0/1 = Int 0/1)
  "tagToEnum#" -> Just ("__kk_builtin_tagToEnum", 1,
    "    func.return %arg0 : i64")
  -- Koka tuple accessors as first-class values.  When `tuple2/fst`
  -- is passed to a HOF (e.g. `xs.map(tuple2/fst)`), the bridge
  -- needs a real function symbol — synthesise one that extracts
  -- field 0 from its arg via the runtime's kk_field.  Similarly
  -- for tuple2/snd → field 1.
  "tuple2/fst" -> Just ("__kk_builtin_tuple2_fst", 1,
    T.unlines [ "    %idx = arith.constant 0 : i64"
              , "    %r = func.call @kk_field(%arg0, %idx) : (i64, i64) -> i64"
              , "    func.return %r : i64" ])
  "tuple2/snd" -> Just ("__kk_builtin_tuple2_snd", 1,
    T.unlines [ "    %idx = arith.constant 1 : i64"
              , "    %r = func.call @kk_field(%arg0, %idx) : (i64, i64) -> i64"
              , "    func.return %r : i64" ])
  _ -> Nothing
  where
    binOp op = T.unlines ["    %r = " <> op <> " %arg0, %arg1 : i64", "    func.return %r : i64"]
    cmpOp pred' = T.unlines ["    %c = arith.cmpi " <> pred' <> ", %arg0, %arg1 : i64", "    %r = arith.extui %c : i1 to i64", "    func.return %r : i64"]

-- | Eta-expand definitions whose body is just a bare reference to a builtin op.
-- GHC's simplifier eta-reduces `addN x y = x + y` to `addN = (+)`.
-- We need to re-expand these so the emitter can generate proper MLIR ops.
etaExpandBuiltinAlias :: Def -> Def
etaExpandBuiltinAlias d = case defExpr d of
  EVar n | nameText n `Set.member` builtinOpNames ->
    -- Get arity from the function type
    let (_, argTypes, _retTy) = decomposeDefType (defType d)
        argNames = [Name ("_ea" <> T.pack (show i)) (fromIntegral i) | i <- [0..length argTypes - 1]]
        params = zip argNames argTypes
        argRefs = [EVar nm | nm <- argNames]
    in if null params
       then d  -- can't expand a 0-arity alias
       else d { defExpr = ELam params (EApp (EVar n) argRefs) }
  _ -> d

emitDefs :: [Def] -> Emit Text
emitDefs defs = do
  pfx <- gets esModulePrefix
  let expandedDefs = map etaExpandBuiltinAlias defs
      -- Deduplicate by qualified name: multi-module compilation can produce
      -- the same definition from both the importing and imported module
      -- (GHC's cross-module specialiser copies defs into importers).
      qualName d = let san = sanitizeName (nameText (qnameName (defName d)))
                   in if T.any (== '/') (nameText (qnameName (defName d)))
                         || T.isPrefixOf pfx san
                      then san else pfx <> san
      dedup [] _seen = []
      dedup (d:ds) seen =
        let qn = qualName d
        in if Set.member qn seen
           then dedup ds seen
           else d : dedup ds (Set.insert qn seen)
      uniqueDefs = dedup expandedDefs Set.empty
  texts <- mapM emitDef uniqueDefs
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
  modify (\s -> s { esCurDef = qualName })
  case stripTypeLam (defExpr def) of
    ELam params body -> do
      -- Use uniform i64 for all top-level fn params (matches the closure ABI
      -- and avoids type mismatches when params flow into kk_* runtime calls
      -- or PAP wrappers that assume i64 throughout).
      let mlirArgs = T.intercalate ", "
            [ "%" <> nameToSsa pn <> ": i64" | (pn, _) <- params ]
          mlirRetTy = "i64"
      -- Install parameters as identity aliases so EVar lookups find them.
      savedA     <- gets esAliases
      savedEvv   <- gets esCurrentEvv
      savedScope <- gets esScopeSsa
      let paramSsas    = [ nameToSsa pn | (pn, _) <- params ]
          paramAliases = [ (s, s) | s <- paramSsas ]
          -- Detect plotkin-transformed defs: their first parameter is named
          -- "evv_p" (per Frankenstein.Core.EvidenceEvv.transformDef). Record
          -- the SSA name so EVar-as-value emission can pre-supply evv to PAPs
          -- (so HOFs invoke them through the standard closure ABI without
          -- needing to know the function is plotkin-transformed).
          curEvv = case params of
            ((p, _) : _) | nameText p == "evv_p" -> Just (nameToSsa p)
            _ -> Nothing
      modify (\s -> s { esAliases    = foldr (\(k,v) m -> Map.insert k v m)
                                              (esAliases s) paramAliases
                       , esCurrentEvv = curEvv
                       , esScopeSsa   = Set.fromList paramSsas
                       })
      bodyText <- emitBody body mlirRetTy
      modify (\s -> s { esAliases    = savedA
                       , esCurrentEvv = savedEvv
                       , esScopeSsa   = savedScope
                       })
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

-- | Emit a kk_cycle_candidate registration call if the current def is cyclic.
-- Returns empty list if not cyclic, or a single MLIR call op if cyclic.
emitCycleCandidate :: Text -> Emit [Text]
emitCycleCandidate ptrName = do
  curDef <- gets esCurDef
  cyclics <- gets esCyclicDefs
  if Set.member curDef cyclics
    then pure ["func.call @kk_cycle_candidate(%" <> ptrName <> ") : (i64) -> ()"]
    else pure []

-- | Emit a string literal as a [Char] cons-list (for Haskell String = [Char]).
-- | Emit a Core expression. Returns (list of MLIR ops, result SSA name)
emitExpr :: Expr -> Emit ([Text], Text)
emitExpr (ELit (LitInt n)) = do
  name <- freshName "v"
  pure (["%" <> name <> " = arith.constant " <> T.pack (show n) <> " : i64"], name)

emitExpr (ELit (LitFloat n)) = do
  -- Emit the float constant in f64 form, then bit-cast to i64 so the
  -- value flows through the uniformly-i64 closure ABI.  Tag the SSA
  -- name as `f64bits` so emitBinOp dispatches subsequent arithmetic
  -- to the float variants.
  fName    <- freshName "v"
  bitsName <- freshName "v"
  recordF64Bits bitsName
  pure ( [ "%" <> fName    <> " = arith.constant " <> T.pack (show n) <> " : f64"
         , "%" <> bitsName <> " = arith.bitcast %" <> fName <> " : f64 to i64"
         ]
       , bitsName)

emitExpr (ELit (LitChar c)) = do
  name <- freshName "v"
  pure (["%" <> name <> " = arith.constant " <> T.pack (show (fromEnum c)) <> " : i64"], name)

emitExpr (ELit (LitString s)) = do
  -- LitString in the emitter is always a raw Addr# (pointer to .rodata).
  -- The [Char] cons-list for Haskell strings is built in CoreTranslate
  -- (in the unpackCString# handler), not here.
  -- For non-Haskell string types (Koka), wrap in kk_string_from_literal.
  conTags <- gets esConTags
  let hasList = Map.member ":" conTags && Map.member "[]" conTags
  globalName <- addStringLit s
  ptrName <- freshName "v"
  intName <- freshName "v"
  if hasList
    then do
      -- Haskell mode: LitString is an Addr# (raw pointer to null-terminated bytes)
      -- used by indexCharOffAddr#/plusAddr# loops after simplifier inlines unpackCString#
      pure ( [ "%" <> ptrName <> " = llvm.mlir.addressof @" <> globalName <> " : !llvm.ptr"
             , "%" <> intName <> " = llvm.ptrtoint %" <> ptrName <> " : !llvm.ptr to i64"
             ]
           , intName)
    else do
      lenName <- freshName "v"
      strName <- freshName "v"
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

-- Builtin operator used as a first-class value (e.g. passed to myFoldl).
-- Emit a small wrapper function and return a closure pointing to it.
emitExpr (EVar n)
  | Just (wrapperName, wrapperArity, wrapperBody) <- builtinWrapperSpec (nameText n) = do
      -- Emit the wrapper function at module scope (only once)
      addLiftedFnOnce wrapperName $ T.unlines
        [ "  func.func @" <> wrapperName <> "("
          <> T.intercalate ", " ["%arg" <> T.pack (show i) <> ": i64" | i <- [0..wrapperArity-1]]
          <> ") -> i64 {"
        , wrapperBody
        , "  }"
        ]
      -- Register in topFns and arityMap so emitFnAsValue works
      modify (\s -> s { esTopFns     = Set.insert wrapperName (esTopFns s)
                       , esTopFnArity = Map.insert wrapperName wrapperArity (esTopFnArity s)
                       })
      arityMap <- gets esTopFnArity
      emitFnAsValue wrapperName arityMap

emitExpr (EVar n) = do
  -- Variable reference — look up in alias map; if not found and not a known
  -- top-level function, emit a direct func.call (for cross-module or external refs).
  let sname = nameToSsa n
      sanitized = sanitizeName (nameText n)
  aliases <- gets esAliases
  topFns <- gets esTopFns
  -- If the name already has a module qualifier or was linker-mangled, it's
  -- already fully qualified — don't prepend esModulePrefix again.
  initialQual <- do
    pfx <- gets esModulePrefix
    pure $ if T.any (== '/') (nameText n) || T.isPrefixOf pfx sanitized
           then sanitized else pfx <> sanitized
  -- Mirror emitAppVarGeneral's suffix-resolver: when an EVar is used
  -- as a value (HOF arg, PAP, …) and the bare-qualified name isn't a
  -- known top-level fn, look for one whose MLIR symbol ENDS with
  -- "_<initialQual>".  This catches Koka type-qualifier names like
  -- `rational/show` resolving to `surd_rational_show`.
  let qualSanitized =
        if Set.member initialQual topFns
        then initialQual
        else case [ tn | tn <- Set.toList topFns
                       , T.isSuffixOf ("_" <> initialQual) tn ] of
               (resolved:_) -> resolved
               [] -> initialQual
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
emitExpr (ECon qn)
  -- Koka's True/False are unboxed in our representation: i64 1 / 0.
  -- Pattern dispatch (emitBoolConCase) treats the scrutinee that way,
  -- so the construction side must match.  Allocating a heap cell would
  -- produce a non-zero pointer that always reads as True and survives
  -- `.show` as a giant integer.
  | nameText (qnameName qn) == "True" = do
      resultName <- freshName "v"
      pure ([ "%" <> resultName <> " = arith.constant 1 : i64" ], resultName)
  | nameText (qnameName qn) == "False" = do
      resultName <- freshName "v"
      pure ([ "%" <> resultName <> " = arith.constant 0 : i64" ], resultName)
  | otherwise = do
      tag <- lookupConTag qn
      tagName <- freshName "v"
      nfieldsName <- freshName "v"
      resultName <- freshName "v"
      cycleOp <- emitCycleCandidate resultName
      pure ([ "%" <> tagName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
            , "%" <> nfieldsName <> " = arith.constant 0 : i64"
            , "%" <> resultName <> " = func.call @kk_alloc_con(%" <> tagName <> ", %" <> nfieldsName <> ") : (i64, i64) -> i64"
            ] ++ cycleOp, resultName)

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
         ]) (zip [(0::Int)..length argNames - 1] argNames)
  -- Register as cycle candidate if the enclosing def may create cycles
  cycleOp <- emitCycleCandidate ptrName
  pure (allOps ++ allocOps ++ concat setOps ++ cycleOp, ptrName)

-- All EApp (EVar fn) patterns: intrinsics + general function call
-- Builtin operator used as first-class value: EApp (EVar "+") [] comes from
-- GHC's normalizeGhcBuiltin producing a bare operator name with no args.
-- Redirect to the EVar handler which has builtinWrapperSpec.
emitExpr (EApp (EVar fn) [])
  | Just _ <- builtinWrapperSpec (nameText fn) = emitExpr (EVar fn)
emitExpr (EApp (EVar fn) args) = emitAppVar fn args
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
  -- Force the callee: if fn is a thunk (e.g. produced by an upstream
  -- EDelay that we wrapped via kk_thunk_create_forced), reading field 0
  -- directly gives the evaluated flag (1), not the fn pointer. kk_thunk_force
  -- on a non-thunk is a no-op, so always-force is safe and corrects the
  -- thunk-as-closure dispatch case.
  forcedName  <- freshName "v"
  idxZeroName <- freshName "v"
  fptrIntName <- freshName "v"
  fptrPtrName <- freshName "v"
  resultName  <- freshName "v"
  let closArgList  = T.intercalate ", " (("%" <> forcedName) : ["%" <> n | n <- argNames])
      closArgTypes = T.intercalate ", " ("i64" : argTys)
      extractOps =
        [ "%" <> forcedName  <> " = func.call @kk_thunk_force(%" <> fnName <> ") : (i64) -> i64"
        , "%" <> idxZeroName <> " = arith.constant 0 : i64"
        , "%" <> fptrIntName <> " = func.call @kk_field(%" <> forcedName <> ", %" <> idxZeroName <> ") : (i64, i64) -> i64"
        , "%" <> fptrPtrName <> " = llvm.inttoptr %" <> fptrIntName <> " : i64 to !llvm.ptr"
        , "%" <> resultName  <> " = llvm.call %" <> fptrPtrName
          <> "(" <> closArgList <> ") : !llvm.ptr, (" <> closArgTypes <> ") -> i64"
        ]
  pure (fnOps ++ allArgOps ++ extractOps, resultName)

emitExpr (ECase scrut branches) = emitCaseDispatch scrut branches

emitExpr (ELet binds body) = emitLetBindings binds body

emitExpr (ELam params body) = emitLambdaLift params body

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
  -- EDelay wraps a lazy thunk: lambda-lift the body into a top-level
  -- function (taking a closure arg) and box it in a kk_thunk_create
  -- cell.  Forcing extracts the fn pointer from the closure and invokes
  -- fn(closure), so captures from the enclosing scope flow through the
  -- existing closure ABI.
  --
  -- Required for recursive lazy definitions like Idris2's `countFrom`,
  -- where eager evaluation diverges.  The previous design (eager-eval
  -- + kk_thunk_create_forced wrap) was kept here for fear of capture-
  -- analysis bugs in the self-hosted emitter, but the same machinery
  -- already works correctly for plain ELam — there is no separate code
  -- path to get wrong.
  (lambdaOps, closureName) <- emitLambdaLift [] e
  resultName <- freshName "v"
  pure ( lambdaOps ++
         [ "// delay (thunk): closure-based lazy, evaluated on force"
         , "%" <> resultName <> " = func.call @kk_thunk_create(%" <> closureName <> ") : (i64) -> i64"
         ]
       , resultName)

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
      rawQualName = if T.null (qnameModule qn) then fname
                    else sanitizeName (qnameModule qn) <> "_" <> fname
  pfx <- gets esModulePrefix
  let qualName = if T.isPrefixOf pfx rawQualName then rawQualName
                 else pfx <> rawQualName
  -- Declare as external if not defined locally. The `() -> i64` type is the
  -- minimum-arity convention used in the func.constant cast below; the actual
  -- function may have higher arity but MLIR allows function-type aliasing
  -- through the ptrtoint dance.
  topFns <- gets esTopFns
  if Set.member qualName topFns
    then pure ()
    else addExternDecl qualName 0
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

-- Effect operations: the evidence pass desugars EPerform/EHandle to plain
-- ELet/EApp before the emitter sees them. In dialect mode (--emit-effect-mlir)
-- we emit frankenstein.* ops; in lowered mode, residual nodes are a bug.

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
    else
      error $ "emitter bug: residual EPerform after evidence pass: "
           ++ show (qnameModule qn) ++ "/" ++ show (nameText (qnameName qn))

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
    else
      error $ "emitter bug: residual EHandle after evidence pass: "
           ++ show (effectRowNameEmit effRow)

-- Catch-all removed: all Expr constructors are handled above


-- | Emit function application where the function is a variable.
-- Dispatches by arity to specialized handlers for builtin operations,
-- then falls back to the general call handler.
emitAppVar :: Name -> [Expr] -> Emit ([Text], Text)
emitAppVar fn args = case args of
  [d, a, b] -> emitAppVarWith3 fn d a b
  [a, b]    -> emitAppVarWith2 fn a b
  [a]       -> emitAppVarWith1 fn a
  []        -> emitAppVarWith0 fn
  _         -> emitAppVarGeneral fn args

-- | 3-arg builtins: dict-passing binary ops.
-- At --no-simplify, GHC keeps typeclass dictionaries as explicit arguments,
-- so operators like (==) arrive with 3 args instead of 2.
emitAppVarWith3 :: Name -> Expr -> Expr -> Expr -> Emit ([Text], Text)
emitAppVarWith3 fn _dict a b
  -- Koka stdlib foldl: real 3-arg call (list, acc, fn).  Intercept
  -- before the dict-strip path that handles GHC's 2-arg dict-passing
  -- form; foldl's first arg is NOT a dict.
  | nameText fn `elem` ["foldl", "list/foldl"]
  = emitListHOF3 "kk_list_foldl" _dict a b
  | n `elem` ["==", "eq"]   = emitCmpOp "eq" a b
  | n `elem` ["/=", "ne", "!="] = emitCmpOp "ne" a b
  | n `elem` ["<", "lt"]    = emitCmpOp "slt" a b
  | n `elem` [">", "gt"]    = emitCmpOp "sgt" a b
  | n `elem` ["<=", "le"]   = emitCmpOp "sle" a b
  | n `elem` [">=", "ge"]   = emitCmpOp "sge" a b
  | n `elem` ["+", "add"]   = emitBinOp "arith.addi" "i64" a b
  | n `elem` ["-", "sub"]   = emitBinOp "arith.subi" "i64" a b
  | n `elem` ["*", "mul"]   = emitBinOp "arith.muli" "i64" a b
  | n `elem` ["/", "div"]   = emitBinOp "arith.divsi" "i64" a b
  | n `elem` ["mod", "%"]   = emitBinOp "arith.remsi" "i64" a b
  | otherwise = emitAppVarGeneral fn [_dict, a, b]
  where n = nameText fn

-- | 2-arg builtins: dict-passing unary, float, int, string, list, IO ops.
emitAppVarWith2 :: Name -> Expr -> Expr -> Emit ([Text], Text)
emitAppVarWith2 fn a b
  -- Dict-passing unary ops: strip first arg as dict, recurse as 1-arg
  | n == "abs"    = emitAppVar fn [b]
  | n == "negate" = emitAppVar fn [b]
  -- Float binary ops
  | n `elem` ["+f", "addf"] = emitBinOp "arith.addf" "f64" a b
  | n `elem` ["-f", "subf"] = emitBinOp "arith.subf" "f64" a b
  | n `elem` ["*f", "mulf"] = emitBinOp "arith.mulf" "f64" a b
  | n `elem` ["/f", "divf"] = emitBinOp "arith.divf" "f64" a b
  -- Float comparisons
  | n `elem` ["==f", "eqf"] = emitFloatCmpOp "oeq" a b
  | n `elem` ["/=f", "nef"] = emitFloatCmpOp "one" a b
  | n `elem` ["<f", "ltf"]  = emitFloatCmpOp "olt" a b
  | n `elem` [">f", "gtf"]  = emitFloatCmpOp "ogt" a b
  | n `elem` ["<=f", "lef"] = emitFloatCmpOp "ole" a b
  | n `elem` [">=f", "gef"] = emitFloatCmpOp "oge" a b
  -- Integer binary ops (including GHC primops with # suffix)
  | n `elem` ["+", "add", "+#", "$fNumInt_$c+"] = emitBinOp "arith.addi" "i64" a b
  | n `elem` ["-", "sub", "-#", "$fNumInt_$c-"] = emitBinOp "arith.subi" "i64" a b
  | n `elem` ["*", "mul", "*#", "$fNumInt_$c*"] = emitBinOp "arith.muli" "i64" a b
  | n `elem` ["/", "div", "quot#", "quotInt#"]  = emitBinOp "arith.divsi" "i64" a b
  | n `elem` ["mod", "rem#", "remInt#", "%"]     = emitBinOp "arith.remsi" "i64" a b
  | n `elem` ["==", "eq", "==#"]                 = emitCmpOp "eq" a b
  | n `elem` ["/=", "ne", "/=#", "!="]           = emitCmpOp "ne" a b
  | n `elem` ["<", "lt", "<#"]                   = emitCmpOp "slt" a b
  | n `elem` [">", "gt", ">#"]                   = emitCmpOp "sgt" a b
  | n `elem` ["<=", "le", "<=#"]                 = emitCmpOp "sle" a b
  | n `elem` [">=", "ge", ">=#"]                 = emitCmpOp "sge" a b
  | n `elem` ["andI#", "and#"]                    = emitBinOp "arith.andi" "i64" a b
  | n `elem` ["orI#", "or#"]                      = emitBinOp "arith.ori" "i64" a b
  | n `elem` ["xorI#", "xor#"]                    = emitBinOp "arith.xori" "i64" a b
  -- Koka stdlib list HOFs.  We provide these as kk_list_* runtime
  -- shims that walk the cons-list and call the user closure via the
  -- standard ABI (field 0 = fn ptr, takes (closure, args...)).  The
  -- alternative — translating std/core/list through the multi-module
  -- pass — would pull in too much surface area.
  | n `elem` ["map", "list/map"]                   = emitListHOF2 "kk_list_map" a b
  | n `elem` ["filter", "list/filter"]             = emitListHOF2 "kk_list_filter" a b
  | n `elem` ["all", "list/all"]                   = emitListHOF2 "kk_list_all" a b
  | n `elem` ["any", "list/any"]                   = emitListHOF2 "kk_list_any" a b
  | n `elem` ["drop", "list/drop"]                 = emitListHOF2 "kk_list_drop" a b
  | n `elem` ["take", "list/take"]                 = emitListHOF2 "kk_list_take" a b
  | n `elem` ["flatmap", "list/flatmap"]           = emitListHOF2 "kk_list_flatmap" a b
  | n `elem` ["filter-map", "list/filter-map"]     = emitListHOF2 "kk_list_filter_map" a b
  | n `elem` ["foreach", "list/foreach"]           = emitListHOF2 "kk_list_foreach" a b
  | n == "++l"                                      = emitListHOF2 "kk_list_concat" a b
  -- Koka's int/max and int/min: select-based 2-arg primitives.
  -- Match on bare `max` / `min` as well as qualified forms.
  | n `elem` ["max", "int/max"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      cmpName <- freshName "v"
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> cmpName    <> " = arith.cmpi sgt, %" <> aName <> ", %" <> bName <> " : i64"
        , "%" <> resultName <> " = arith.select %" <> cmpName <> ", %" <> aName <> ", %" <> bName <> " : i64"
        ], resultName)
  | n `elem` ["min", "int/min"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      cmpName <- freshName "v"
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> cmpName    <> " = arith.cmpi slt, %" <> aName <> ", %" <> bName <> " : i64"
        , "%" <> resultName <> " = arith.select %" <> cmpName <> ", %" <> aName <> ", %" <> bName <> " : i64"
        ], resultName)
  -- `cmp(a, b)` returns an Order value (Lt | Eq | Gt — see the
  -- synthetic DataDecl injected by KokaBridge for `order`).  Look
  -- up the per-program tag for each constructor, then allocate
  -- one of three pre-built nullary cells via scf.if.  Surd code
  -- pattern-matches on cmp's result against Lt/Eq/Gt, so an
  -- integer encoding doesn't work.
  | n `elem` ["cmp", "int/cmp"] = do
      ltTag <- lookupConTag (QName "std/core/types" (Name "Lt" 0))
      eqTag <- lookupConTag (QName "std/core/types" (Name "Eq" 0))
      gtTag <- lookupConTag (QName "std/core/types" (Name "Gt" 0))
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      ltCmp <- freshName "v"
      gtCmp <- freshName "v"
      ltTagN <- freshName "v"
      eqTagN <- freshName "v"
      gtTagN <- freshName "v"
      zeroFlds <- freshName "v"
      ltCell <- freshName "v"
      eqCell <- freshName "v"
      gtCell <- freshName "v"
      gtOrEq <- freshName "v"
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> ltCmp <> " = arith.cmpi slt, %" <> aName <> ", %" <> bName <> " : i64"
        , "%" <> gtCmp <> " = arith.cmpi sgt, %" <> aName <> ", %" <> bName <> " : i64"
        , "%" <> ltTagN <> " = arith.constant " <> T.pack (show ltTag) <> " : i64"
        , "%" <> eqTagN <> " = arith.constant " <> T.pack (show eqTag) <> " : i64"
        , "%" <> gtTagN <> " = arith.constant " <> T.pack (show gtTag) <> " : i64"
        , "%" <> zeroFlds <> " = arith.constant 0 : i64"
        , "%" <> ltCell <> " = func.call @kk_alloc_con(%" <> ltTagN <> ", %" <> zeroFlds <> ") : (i64, i64) -> i64"
        , "%" <> eqCell <> " = func.call @kk_alloc_con(%" <> eqTagN <> ", %" <> zeroFlds <> ") : (i64, i64) -> i64"
        , "%" <> gtCell <> " = func.call @kk_alloc_con(%" <> gtTagN <> ", %" <> zeroFlds <> ") : (i64, i64) -> i64"
        , "%" <> gtOrEq <> " = arith.select %" <> gtCmp <> ", %" <> gtCell <> ", %" <> eqCell <> " : i64"
        , "%" <> resultName <> " = arith.select %" <> ltCmp <> ", %" <> ltCell <> ", %" <> gtOrEq <> " : i64"
        ], resultName)
  -- 2-arg libm: atan2, pow.
  | n `elem` ["atan2", "double/atan2", "float64/atan2"] = emitLibm2 "atan2" a b
  | n `elem` ["pow", "double/pow", "float64/pow"]       = emitLibm2 "pow" a b
  | n `elem` ["fmod", "double/fmod", "float64/fmod"]    = emitLibm2 "fmod" a b
  -- More list HOFs / generators.
  | n `elem` ["range/list", "list"]              = emitListHOF2 "kk_range_list" a b
  | n `elem` ["zip", "list/zip"]                 = emitListHOF2 "kk_list_zip" a b
  | n `elem` ["map-indexed", "list/map-indexed"] = emitListHOF2 "kk_list_map_indexed" a b
  | n `elem` ["joinsep/join", "joinsep_join", "list/joinsep/join"] = emitListHOF2 "kk_joinsep_join" a b
  -- Koka's `throw(msg, info)` 2-arg form: the second arg is the
  -- effect-handler context, which we ignore.  Routes to kk_throw
  -- with just the message.
  | n `elem` ["throw", "exn/throw"] = do
      (aOps, aName) <- emitExpr a
      (bOps, _)     <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_throw(%" <> aName <> ") : (i64) -> i64"
        ], resultName)
  -- Koka's `show(x, dict)` 2-arg form: when the implicit show
  -- dictionary is explicit (no specialisation), the bridge sees
  -- `show(val, ?show-dict)`.  Default to int show; for non-int
  -- types this loses fidelity but at least links.
  | n `elem` ["show", "show/show"] = do
      (aOps, aName) <- emitExpr a
      (bOps, _)     <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_str_show_int(%" <> aName <> ") : (i64) -> i64"
        ], resultName)
  -- 2-arg `unjust(m, info)`: Koka passes an effect-info second arg
  -- (the file/line context for the panic message).  We ignore it
  -- and use the kk_unjust single-arg form.
  | n `elem` ["unjust", "maybe/unjust"] = do
      (aOps, aName) <- emitExpr a
      (bOps, _)     <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_unjust(%" <> aName <> ") : (i64) -> i64"
        ], resultName)
  -- `file/kk-file-line(file, line)`: Koka's source-location info for
  -- panics / assertions.  The runtime doesn't display source loc,
  -- so just return a placeholder int.
  | n `elem` ["file/kk-file-line"] = do
      (aOps, _) <- emitExpr a
      (bOps, _) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  -- Address arithmetic (from inlined unpackCString#)
  | n == "plusAddr#"                               = emitBinOp "arith.addi" "i64" a b
  -- indexCharOffAddr# addr off: load byte at addr+off, zero-extend to i64
  | n == "indexCharOffAddr#" = do
      (addrOps, addrName) <- emitExpr a
      (offOps, offName) <- emitExpr b
      effAddr <- freshName "v"
      ptrName <- freshName "v"
      byteName <- freshName "v"
      resultName <- freshName "v"
      pure (addrOps ++ offOps ++
        [ "%" <> effAddr <> " = arith.addi %" <> addrName <> ", %" <> offName <> " : i64"
        , "%" <> ptrName <> " = llvm.inttoptr %" <> effAddr <> " : i64 to !llvm.ptr"
        , "%" <> byteName <> " = llvm.load %" <> ptrName <> " : !llvm.ptr -> i8"
        , "%" <> resultName <> " = arith.extui %" <> byteName <> " : i8 to i64"
        ], resultName)
  -- List cons
  | n == "kk_cons" = do
      (hOps, hName) <- emitExpr a
      (tOps, tName) <- emitExpr b
      resultName <- freshName "v"
      pure (hOps ++ tOps ++
        [ "%" <> resultName <> " = func.call @kk_cons(%" <> hName <> ", %" <> tName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- String binary ops
  | n `elem` ["str_concat", "++s", "concat_str", "bytes_concat"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_str_concat(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- List `++` — bridge emits this for list-typed `++` references.  The
  -- runtime helper builds a fresh appended list with proper refcounts.
  | n == "kk_list_concat" = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_list_concat(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- int_to_haskell_chars(value, tail) → kk_int_to_haskell_chars.  Used
  -- by the GHC bridge as the rewrite target for Show Int (via the
  -- $w$cshowsPrec2 worker name).  Builds a [Char] cons-list with the
  -- decimal representation prepended onto the tail list.
  | n == "int_to_haskell_chars" = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_int_to_haskell_chars(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- int_list_to_haskell_chars(list, tail) → kk_int_list_to_haskell_chars.
  -- Used by the GHC bridge as the rewrite target for Show [Int].
  | n == "int_list_to_haskell_chars" = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_int_list_to_haskell_chars(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- haskell_chars_concat(a, b) → kk_haskell_chars_concat.
  -- Used by the GHC bridge as the rewrite target for GHC.Internal.Base.(++)
  -- in derived Show bodies.
  | n == "haskell_chars_concat" = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_haskell_chars_concat(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- rust_args_pack(template, args) → kk_rust_args_pack.  Used by the
  -- Rust bridge for `Arguments::new(template, args)`.
  | n == "rust_args_pack" = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_rust_args_pack(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- rust_field_safe(base, idx) → kk_rust_field_safe.  Used by the
  -- Rust bridge for `_N.M` field accesses (dispatches between heap
  -- tuple field reads and WithOverflow-flattened identity).
  | n == "rust_field_safe" = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_rust_field_safe(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- (rust_arg_debug is handled in the 1-arg dispatcher below.)
  | n `elem` ["str_eq", "==s", "bytes_eq"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_str_eq(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  | n `elem` ["bytes_index", "byte_at"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_bytes_index(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- File I/O (2-arg): write_file(path, content) -> 0/-1
  | n `elem` ["write_file", "writeFile"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_write_file(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- IORef set: returns 0 (kk_ref_set is void in C, wrapper returns 0)
  | n `elem` ["set_ref", "writeIORef"] = do
      (aOps, aName) <- emitExpr a
      (bOps, bName) <- emitExpr b
      resultName <- freshName "v"
      pure (aOps ++ bOps ++
        [ "%" <> resultName <> " = func.call @kk_ref_set(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- Evidence intrinsic: evv_select(evv, idx) -> kk_evv_get(evv, idx)
  | n == "evv_select" = do
      (evvOps, evvName) <- emitExpr a
      (idxOps, idxName) <- emitExpr b
      resultName <- freshName "v"
      let callOp = "%" <> resultName <> " = func.call @kk_evv_get(%" <> evvName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
      pure (evvOps ++ idxOps ++ [callOp], resultName)
  | otherwise = emitAppVarGeneral fn [a, b]
  where n = nameText fn

-- | 1-arg builtins: unary ops, print, string/IO/ref intrinsics.
emitAppVarWith1 :: Name -> Expr -> Emit ([Text], Text)
emitAppVarWith1 fn arg
  -- tagToEnum# converts Int# (0/1) to Bool — identity in our representation
  | n == "tagToEnum#" = emitExpr arg
  -- ord#/chr# are identity in our representation (Char = Int = i64)
  | n `elem` ["ord#", "chr#"] = emitExpr arg
  -- Unary integer operations
  | n `elem` ["negate", "negateInt#", "$fNumInt_$cnegate"] = do
      (argOps, argName) <- emitExpr arg
      zeroName <- freshName "v"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> zeroName <> " = arith.constant 0 : i64"
        , "%" <> resultName <> " = arith.subi %" <> zeroName <> ", %" <> argName <> " : i64"
        ], resultName)
  | n == "abs" = do
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
  -- Futhark array primitives → MLIR linalg
  | n `elem` ["sum_iota", "dot_iota"] = do
      let isSquare = n == "dot_iota"
      (nOps, nName) <- emitExpr arg
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
  | n == "print" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      fmtName <- freshName "v"
      pure (argOps ++
        [ "%" <> fmtName <> " = llvm.mlir.addressof @fmt_int : !llvm.ptr"
        , "llvm.call @printf(%" <> fmtName <> ", %" <> argName <> ") vararg(!llvm.func<i32 (ptr, ...)>) : (!llvm.ptr, i64) -> i32"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  -- First-class string intrinsics.  println_str / putStrLn emit a
  -- trailing newline; print_str writes the bytes verbatim.  Routing
  -- them to the wrong runtime function produced a spurious extra
  -- newline for Mercury's io.write_string (which embeds its own '\n').
  | n `elem` ["println_str", "putStrLn"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_println_str(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  | n == "print_str" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_print_str(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  -- rust_print_dispatch(v) — Rust bridge's std::io::_print target.
  -- Routes through the runtime dispatcher that checks whether v is a
  -- kk_string (no-arg from_str path) or a packed (template, args)
  -- cell from `Arguments::new` (formatted path).
  | n == "rust_print_dispatch" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_rust_print_dispatch(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- rust_arg_debug(v) wraps an arg for Debug format (`{:?}`).  The
  -- runtime dispatcher in kk_rust_print_one_arg unwraps and applies
  -- the debug formatter.
  | n == "rust_arg_debug" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_rust_arg_debug(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- Radix wrappers for `{:x}` / `{:X}` / `{:o}` / `{:b}` and per-type
  -- numeric wrappers for non-i64 Display formats (u32/i32/u64/u16/i16/u8/i8).
  | n `elem` [ "rust_arg_lower_hex", "rust_arg_upper_hex"
             , "rust_arg_octal", "rust_arg_binary"
             , "rust_arg_u32", "rust_arg_i32", "rust_arg_u64"
             , "rust_arg_u16", "rust_arg_i16"
             , "rust_arg_u8", "rust_arg_i8"
             , "rust_arg_f64", "rust_arg_f32"
             ] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      let rt = "kk_" <> nameText fn
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @" <> rt <> "(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- Walk a Haskell [Char] cons-list and print each char.  println_*
  -- adds a trailing newline; print_* does not (matches hPutStr2 with
  -- addNewline False).  Used by the GHC bridge's putStrLn / hPutStr2
  -- rewrite — see ghcIoOutputRuntime in GhcBridge.CoreTranslate.
  | n `elem` ["println_haskell_chars"] = do
      (argOps, argName) <- emitExpr arg
      forced <- freshName "vf"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> forced <> " = func.call @kk_thunk_force(%" <> argName <> ") : (i64) -> i64"
        , "func.call @kk_println_haskell_chars(%" <> forced <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  | n `elem` ["print_haskell_chars"] = do
      (argOps, argName) <- emitExpr arg
      forced <- freshName "vf"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> forced <> " = func.call @kk_thunk_force(%" <> argName <> ") : (i64) -> i64"
        , "func.call @kk_print_haskell_chars(%" <> forced <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  | n `elem` ["str_len", "strlen", "bytes_len"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_len(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- String-specific char-length names go to kk_str_char_len.  Do NOT
  -- include the bare `length` here: it's overloaded between string and
  -- list, and the bridge can't always pin down which at a nested call
  -- site (Koka may emit a curried Application like
  -- `KC.App (KC.App length-spec args) [xs]`, hitting our generic
  -- fallback before the bridge's type-aware dispatch fires).  Bare
  -- `length` defaults to list (the surd code uses it almost exclusively
  -- on lists; string callers say `chars/count`).
  | n `elem` ["str_char_len", "char_len", "char_count"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_char_len(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n == "length" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_list_length(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- List length — bridge picks this for list-typed `length`/`count`.
  | n == "kk_list_length" = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_list_length(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["str_flatten", "flatten"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_flatten(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- Koka tuple accessors: Tuple2(a,b).fst is field 0, .snd is field 1.
  | n `elem` ["tuple2/fst"] = do
      (argOps, argName) <- emitExpr arg
      idxName <- freshName "v"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> idxName <> " = arith.constant 0 : i64"
        , "%" <> resultName <> " = func.call @kk_field(%" <> argName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
        ], resultName)
  | n `elem` ["tuple2/snd"] = do
      (argOps, argName) <- emitExpr arg
      idxName <- freshName "v"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> idxName <> " = arith.constant 1 : i64"
        , "%" <> resultName <> " = func.call @kk_field(%" <> argName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
        ], resultName)
  -- Koka `string/println(s)` and `string/print(s)` reach this path
  -- when the function is referenced bare (as a HOF arg, in a PAP, …)
  -- rather than directly applied: the bridge's App-shape intercept
  -- doesn't fire on bare-EVar references.  Route to kk_println_str /
  -- kk_print_str so the link succeeds.
  | n `elem` ["string/println"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_println_str(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  | n `elem` ["string/print"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_print_str(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64"
        ], resultName)
  -- Koka `chars/count(s)` is the codepoint count — wire to
  -- kk_str_char_len.
  | n `elem` ["chars/count"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_char_len(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- Koka `char/string(c)` makes a 1-char string from a codepoint.
  -- Routes to a new runtime helper that UTF-8 encodes the codepoint.
  | n `elem` ["char/string"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_string_from_char(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- Koka Order predicates.  We encode `order` as a plain integer:
  -- -1 (Lt), 0 (Eq), 1 (Gt) — see emitAppVarWith2's `cmp` intercept.
  -- The predicates are a sign test.
  -- Order predicates: compare the Order cell's tag against the
  -- expected constructor tag.  Replaces the earlier int-sign hack
  -- (which produced false answers when surd code pattern-matched
  -- on the Order value).
  | n `elem` ["is-lt", "order/is-lt"] = emitOrderPredTag "Lt" arg
  | n `elem` ["is-eq", "order/is-eq"] = emitOrderPredTag "Eq" arg
  | n `elem` ["is-gt", "order/is-gt"] = emitOrderPredTag "Gt" arg
  -- Int predicates.
  | n `elem` ["is-even", "int/is-even"] = emitParityPred "eq" arg
  | n `elem` ["is-odd",  "int/is-odd"]  = emitParityPred "ne" arg
  -- `is-empty` for lists: tag == KK_NIL_TAG.  kk_is_nil returns 1 / 0.
  | n `elem` ["is-empty", "list/is-empty"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_is_nil(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- `int/char(c)` and `char/int(n)`: bijection between codepoint
  -- and int in our representation.  Identity.
  | n `elem` ["int/char", "char/int", "char.int"] = emitExpr arg
  -- `int(x)` is the general int conversion — identity at runtime
  -- since everything is i64.
  | n `elem` ["int"] = emitExpr arg
  -- Koka's `Just(x).unjust` returns x (panic on Nothing); we
  -- represent Just as Cons-like with field 0 = x.
  | n `elem` ["unjust", "maybe/unjust"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_unjust(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["maybe/head", "list/head", "head"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_maybe_head(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["throw", "exn/throw"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_throw(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- 1-arg libm math: cos, sin.  Bit-cast i64 → f64, call libm,
  -- bit-cast result back.  The f64bits tag flows through the
  -- existing float-aware binop dispatch.
  | n `elem` ["cos", "double/cos", "float64/cos"] = emitLibm1 "cos" arg
  | n `elem` ["sin", "double/sin", "float64/sin"] = emitLibm1 "sin" arg
  | n `elem` ["tan", "double/tan", "float64/tan"] = emitLibm1 "tan" arg
  | n `elem` ["sqrt", "double/sqrt", "float64/sqrt"] = emitLibm1 "sqrt" arg
  | n `elem` ["log", "double/log", "float64/log"] = emitLibm1 "log" arg
  | n `elem` ["exp", "double/exp", "float64/exp"] = emitLibm1 "exp" arg
  -- Koka bool not: `!x` = `x XOR 1`.  Bools are represented as i64
  -- 0 (False) or 1 (True) in our ABI, so XOR with 1 flips them.
  | n `elem` ["bool/!", "!"] = do
      (argOps, argName) <- emitExpr arg
      oneName <- freshName "v"
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> oneName    <> " = arith.constant 1 : i64"
        , "%" <> resultName <> " = arith.xori %" <> argName <> ", %" <> oneName <> " : i64"
        ], resultName)
  -- Koka `int/float64(i)` and bare `float64(i)`: int → float64
  -- coercion.  Emit `arith.sitofp` (signed int to fp) then bit-cast
  -- to i64 so the value flows through the uniformly-i64 closure ABI;
  -- tag the SSA so downstream arith dispatches to the float
  -- variants (see emitBinOp's f64bits handling).
  | n `elem` ["int/float64", "float64", "Float64", "f64", "toFloat64"] = do
      (argOps, argName) <- emitExpr arg
      fName    <- freshName "v"
      bitsName <- freshName "v"
      recordF64Bits bitsName
      pure (argOps ++
        [ "%" <> fName    <> " = arith.sitofp %" <> argName <> " : i64 to f64"
        , "%" <> bitsName <> " = arith.bitcast %" <> fName <> " : f64 to i64"
        ], bitsName)
  | n `elem` ["show", "show_int", "str_show_int"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_str_show_int(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- File I/O, process, environment intrinsics (1-arg)
  | n `elem` ["read_file", "readFile"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_read_file(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["file_exists", "fileExists"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_file_exists(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["system", "shell"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_system(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["getenv", "getEnv"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_getenv(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- IORef intrinsics (1-arg)
  | n `elem` ["new_ref", "newIORef", "ref"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_ref_new(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["get_ref", "readIORef", "deref"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_ref_get(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  -- Command-line / exit intrinsics
  | n `elem` ["args_get", "getArg"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "%" <> resultName <> " = func.call @kk_args_get(%" <> argName <> ") : (i64) -> i64"
        ], resultName)
  | n `elem` ["exit", "exitWith"] = do
      (argOps, argName) <- emitExpr arg
      resultName <- freshName "v"
      pure (argOps ++
        [ "func.call @kk_exit(%" <> argName <> ") : (i64) -> ()"
        , "%" <> resultName <> " = arith.constant 0 : i64  // unreachable (exit)"
        ], resultName)
  | otherwise = emitAppVarGeneral fn [arg]
  where n = nameText fn

-- | 0-arg builtins: nil, read_line, args_count, args_progname.
emitAppVarWith0 :: Name -> Emit ([Text], Text)
emitAppVarWith0 fn
  | n == "kk_nil" = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_nil() : () -> i64" ]
           , resultName)
  | n `elem` ["read_line", "getLine"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_read_line() : () -> i64" ]
           , resultName)
  | n `elem` ["string_empty"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_string_empty() : () -> i64" ]
           , resultName)
  | n `elem` ["args_count", "numArgs"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_args_count() : () -> i64" ]
           , resultName)
  | n `elem` ["args_progname", "getProgName"] = do
      resultName <- freshName "v"
      pure ( [ "%" <> resultName <> " = func.call @kk_args_progname() : () -> i64" ]
           , resultName)
  | otherwise = emitAppVarGeneral fn []
  where n = nameText fn

-- | General function call handler: top-level calls, closure-indirect,
-- promoted lambdas, and unresolved externals.
emitAppVarGeneral :: Name -> [Expr] -> Emit ([Text], Text)
emitAppVarGeneral fn args = do
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
  -- Also, runtime functions (kk_*, mercury_*) are never module-qualified.
  initialQual <- do
    pfx <- gets esModulePrefix
    extRtSet <- gets esExtRuntimeFns
    pure $ if hasModule || T.isPrefixOf pfx sanitized
              || Set.member sanitized extRtSet
           then sanitized else pfx <> sanitized
  let nArgs = length args
      -- Koka's name resolver picks up overloaded operations via a
      -- type-qualifier prefix (e.g. `rational/==` for the `==` in
      -- module surd/rational).  Our bridge stores those names
      -- verbatim, but the def lives at the longer module path
      -- (`surd_rational_zeze`).  If the direct name isn't a known
      -- top-level fn, look for one whose MLIR symbol ENDS with
      -- "_<initialQual>".  The existing PAP / oversaturation
      -- handling below takes care of the arity dance — the
      -- under-saturated case (nArgs=0 passing the fn as a value
      -- to a HOF) builds a zero-supplied PAP closure.
      qualSanitized =
        if Set.member initialQual topFns
        then initialQual
        else case [ n | n <- Set.toList topFns
                      , T.isSuffixOf ("_" <> initialQual) n ] of
               (resolved:_) -> resolved
               [] -> initialQual
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
          -- Case (a): local closure-indirect call. Force in case the
          -- bound value is a thunk (kk_thunk_force is a no-op on
          -- non-thunks); without it, reading field 0 of an unforced
          -- kk_thunk_create_forced thunk returns the eval flag (1)
          -- instead of the fn pointer — see ABI audit Boundary C.
          forcedName  <- freshName "v"
          idxZeroName <- freshName "v"
          fptrIntName <- freshName "v"
          fptrPtrName <- freshName "v"
          resultName  <- freshName "v"
          let closArgList = T.intercalate ", " (("%" <> forcedName) : ["%" <> n | n <- argNames])
              closArgTypes = T.intercalate ", " ("i64" : argTypes)
              extractOps =
                [ "%" <> forcedName  <> " = func.call @kk_thunk_force(%" <> closName <> ") : (i64) -> i64"
                , "%" <> idxZeroName <> " = arith.constant 0 : i64"
                , "%" <> fptrIntName <> " = func.call @kk_field(%" <> forcedName <> ", %" <> idxZeroName <> ") : (i64, i64) -> i64"
                , "%" <> fptrPtrName <> " = llvm.inttoptr %" <> fptrIntName <> " : i64 to !llvm.ptr"
                , "%" <> resultName  <> " = llvm.call %" <> fptrPtrName
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

-- | Emit case expression dispatch.
emitCaseDispatch :: Expr -> [Branch] -> Emit ([Text], Text)
emitCaseDispatch scrut branches = do
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
      -- The scrutinee is already unboxed (raw codepoint) because GHC Core
      -- case on Char always goes through: case c of C# c# -> case c# of ...
      -- The outer case extracts the codepoint via kk_field(_, 0), so by the
      -- time we reach the inner case on char literals, the value is raw.
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

-- | Emit let bindings with scope save/restore.
emitLetBindings :: [BindGroup] -> Expr -> Emit ([Text], Text)
emitLetBindings [binds] body = do
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
  -- Track any deconfliction renames positionally so the captures-precompute
  -- step and emitBindAsTopFn use the same renamed qualN.
  recBindQualNs <- forM recBinds $ \bnd -> do
    let qualN0 = qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd)
        arity = countLamParams (bindExpr bnd)
        ssaKey = nameToSsa (Frankenstein.Core.Types.bindName bnd)
    qualN <- dedupeQualN qualN0 arity
    modify (\s -> s { esTopFns      = Set.insert qualN (esTopFns s)
                    , esTopFnArity  = Map.insert qualN arity (esTopFnArity s)
                    , esPromotedFns = Map.insert ssaKey qualN (esPromotedFns s) })
    pure qualN
  -- Pre-compute captures for all promoted binds (fixed-point iteration).
  precomputeCapturesWith modPfx recBindQualNs recBinds
  -- Emit recursive binds as top-level func.func definitions.
  recOps <- concat <$> mapM (\(qn, bnd) -> emitBindAsTopFnWith modPfx qn bnd)
                            (zip recBindQualNs recBinds)
  -- Emit remaining binds normally.
  plainOps <- concat <$> mapM emitBind plainBinds
  (bodyOps, bodyName) <- emitExpr body
  modify (\s -> s { esAliases = savedA, esTopFns = savedTopFns
                  , esTopFnArity = savedArity, esPromotedFns = savedPromoted
                  , esPromotedCaptures = savedCaptures })
  pure (recOps ++ plainOps ++ bodyOps, bodyName)

emitLetBindings (bg:bgs) body = do
  savedA <- gets esAliases
  savedTopFns <- gets esTopFns
  savedArity  <- gets esTopFnArity
  savedPromoted <- gets esPromotedFns
  savedCaptures <- gets esPromotedCaptures
  modPfx <- gets esModulePrefix
  let (recBinds, plainBinds) = partition (isRecLetLambda modPfx) bg
  recBindQualNs <- forM recBinds $ \bnd -> do
    let qualN0 = qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd)
        arity = countLamParams (bindExpr bnd)
        ssaKey = nameToSsa (Frankenstein.Core.Types.bindName bnd)
    qualN <- dedupeQualN qualN0 arity
    modify (\s -> s { esTopFns      = Set.insert qualN (esTopFns s)
                    , esTopFnArity  = Map.insert qualN arity (esTopFnArity s)
                    , esPromotedFns = Map.insert ssaKey qualN (esPromotedFns s) })
    pure qualN
  precomputeCapturesWith modPfx recBindQualNs recBinds
  recOps <- concat <$> mapM (\(qn, bnd) -> emitBindAsTopFnWith modPfx qn bnd)
                            (zip recBindQualNs recBinds)
  plainOps <- concat <$> mapM emitBind plainBinds
  (restOps, restName) <- emitLetBindings bgs body
  modify (\s -> s { esAliases = savedA, esTopFns = savedTopFns
                  , esTopFnArity = savedArity, esPromotedFns = savedPromoted
                  , esPromotedCaptures = savedCaptures })
  pure (recOps ++ plainOps ++ restOps, restName)

emitLetBindings [] body = emitExpr body

-- | Emit lambda lifting with heap-allocated closures.
emitLambdaLift :: [(Name, Type)] -> Expr -> Emit ([Text], Text)
emitLambdaLift params body = do
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
  curDef <- gets esCurDef
  -- Use curDef (the current top-level definition's fully-qualified name) as
  -- prefix for lifted lambdas.  This avoids cross-module symbol collisions:
  -- different modules compiled separately share the same esModulePrefix and
  -- counter range, so plain "frankenstein_lambda<N>" names collide at link time.
  let lambdaPfx = if T.null curDef then modPfx <> "lambda" else curDef <> "_lambda"
  liftedName <- freshName lambdaPfx
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
  savedScope <- gets esScopeSsa
  -- The new MLIR function's scope: the closure self, all capture-loaded
  -- SSAs, all rebound params. Names from the outer function are NOT in
  -- scope inside this lambda's body.
  let lambdaScope = Set.fromList
        ( closFresh : capFresh ++ extraCapFresh ++ paramFresh )
  modify (\s -> s { esAliases = foldr (\(k,v) m -> Map.insert k v m)
                                      (esAliases s)
                                      (capAliases ++ extraCapAliases ++ paramAliases)
                   , esScopeSsa = lambdaScope
                   })
  -- Build prologue ops that extract captured fields from %closure.
  -- Each capture must be retained: if the closure is called multiple times
  -- (e.g. as a callback in mapM/map), the Perceus-inserted drops in the body
  -- would free the capture after the first call without this retain.
  let allCapFresh = capFresh ++ extraCapFresh
      prologue = concat
        [ [ "%idx_" <> cfn <> " = arith.constant " <> T.pack (show i) <> " : i64"
          , "%" <> cfn <> " = func.call @kk_field(%" <> closFresh <> ", %idx_" <> cfn <> ") : (i64, i64) -> i64"
          , "func.call @kk_retain(%" <> cfn <> ") : (i64) -> ()"
          ]
        | (i, cfn) <- zip [(1::Int)..length allCapFresh] allCapFresh
        ]
  (bodyOps, bodyResult) <- emitExpr body
  -- Restore alias map and scope set (body-local context shouldn't leak out).
  modify (\s -> s { esAliases  = savedAliases
                   , esScopeSsa = savedScope
                   })
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
         ]) (zip [(1::Int)..length allCapturedNames] allCapturedNames)
  pure (allocOps ++ concat capSetOps, ptrName)


-- Helpers

-- | Order predicate: check the Order cell's tag against the
-- bridge-assigned tag for `Lt`, `Eq`, or `Gt` (via assignProgramTags
-- on the synthetic Order DataDecl).  Result is 0/1 (i64).
emitOrderPredTag :: Text -> Expr -> Emit ([Text], Text)
emitOrderPredTag ctor arg = do
  tagInt <- lookupConTag (QName "std/core/types" (Name ctor 0))
  (argOps, argName) <- emitExpr arg
  tagN <- freshName "v"
  expectN <- freshName "v"
  cmpN <- freshName "v"
  resultName <- freshName "v"
  pure (argOps ++
    [ "%" <> tagN     <> " = func.call @kk_tag(%" <> argName <> ") : (i64) -> i64"
    , "%" <> expectN  <> " = arith.constant " <> T.pack (show tagInt) <> " : i64"
    , "%" <> cmpN     <> " = arith.cmpi eq, %" <> tagN <> ", %" <> expectN <> " : i64"
    , "%" <> resultName <> " = arith.extui %" <> cmpN <> " : i1 to i64"
    ], resultName)

-- | (legacy, unused — kept for symmetry).  cmpi against 0,
-- zero-extend i1 → i64.
emitOrderPred :: Text -> Expr -> Emit ([Text], Text)
emitOrderPred pred' arg = do
  (argOps, argName) <- emitExpr arg
  zeroName <- freshName "v"
  cmpName  <- freshName "v"
  resultName <- freshName "v"
  pure (argOps ++
    [ "%" <> zeroName   <> " = arith.constant 0 : i64"
    , "%" <> cmpName    <> " = arith.cmpi " <> pred' <> ", %" <> argName <> ", %" <> zeroName <> " : i64"
    , "%" <> resultName <> " = arith.extui %" <> cmpName <> " : i1 to i64"
    ], resultName)

-- | is-even / is-odd: take remainder mod 2, compare to 0, then
-- zero-extend i1 → i64.
emitParityPred :: Text -> Expr -> Emit ([Text], Text)
emitParityPred pred' arg = do
  (argOps, argName) <- emitExpr arg
  twoName <- freshName "v"
  remName <- freshName "v"
  zeroName <- freshName "v"
  cmpName  <- freshName "v"
  resultName <- freshName "v"
  pure (argOps ++
    [ "%" <> twoName    <> " = arith.constant 2 : i64"
    , "%" <> remName    <> " = arith.remsi %" <> argName <> ", %" <> twoName <> " : i64"
    , "%" <> zeroName   <> " = arith.constant 0 : i64"
    , "%" <> cmpName    <> " = arith.cmpi " <> pred' <> ", %" <> remName <> ", %" <> zeroName <> " : i64"
    , "%" <> resultName <> " = arith.extui %" <> cmpName <> " : i1 to i64"
    ], resultName)

-- | Emit a call to a 1-arg libm function (cos, sin, sqrt, …).
-- The arg is an i64 bit-pattern (the f64bits ABI); we cast to f64,
-- invoke the libm function, cast back, and tag the result.
emitLibm1 :: Text -> Expr -> Emit ([Text], Text)
emitLibm1 fname arg = do
  (argOps, argName) <- emitExpr arg
  fIn <- freshName "v"
  fOut <- freshName "v"
  iOut <- freshName "v"
  recordF64Bits iOut
  pure (argOps ++
    [ "%" <> fIn  <> " = arith.bitcast %" <> argName <> " : i64 to f64"
    , "%" <> fOut <> " = func.call @" <> fname <> "(%" <> fIn <> ") : (f64) -> f64"
    , "%" <> iOut <> " = arith.bitcast %" <> fOut <> " : f64 to i64"
    ], iOut)

-- | Emit a call to a 2-arg libm function (atan2, pow).
emitLibm2 :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitLibm2 fname a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  fInA <- freshName "v"
  fInB <- freshName "v"
  fOut <- freshName "v"
  iOut <- freshName "v"
  recordF64Bits iOut
  pure (aOps ++ bOps ++
    [ "%" <> fInA <> " = arith.bitcast %" <> aName <> " : i64 to f64"
    , "%" <> fInB <> " = arith.bitcast %" <> bName <> " : i64 to f64"
    , "%" <> fOut <> " = func.call @" <> fname <> "(%" <> fInA <> ", %" <> fInB <> ") : (f64, f64) -> f64"
    , "%" <> iOut <> " = arith.bitcast %" <> fOut <> " : f64 to i64"
    ], iOut)

-- | Emit a 2-arg call to a kk_list_* HOF runtime shim.  Used for
-- map / filter / all / any / drop / take / flatmap / filter-map /
-- foreach / list-concat.
emitListHOF2 :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitListHOF2 rt a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  resultName <- freshName "v"
  pure (aOps ++ bOps ++
    [ "%" <> resultName <> " = func.call @" <> rt <> "(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
    ], resultName)

-- | Emit a 3-arg call to a kk_list_* HOF runtime shim.  Used for foldl.
emitListHOF3 :: Text -> Expr -> Expr -> Expr -> Emit ([Text], Text)
emitListHOF3 rt a b c = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  (cOps, cName) <- emitExpr c
  resultName <- freshName "v"
  pure (aOps ++ bOps ++ cOps ++
    [ "%" <> resultName <> " = func.call @" <> rt <> "(%" <> aName <> ", %" <> bName <> ", %" <> cName <> ") : (i64, i64, i64) -> i64"
    ], resultName)

emitBinOp :: Text -> Text -> Expr -> Expr -> Emit ([Text], Text)
emitBinOp op ty a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  -- If both operands are tagged as f64-bits (LitFloat or a prior
  -- float arith), rewrite the integer-arith opcode to the float
  -- variant and bit-cast the operands and result around the call.
  -- This lets Koka's generic `*`, `+`, etc. work for float64 args
  -- without each call-site knowing the operand type, while keeping
  -- every SSA value at MLIR type `i64` (matching the closure ABI).
  aIsF <- isF64Bits aName
  bIsF <- isF64Bits bName
  if aIsF && bIsF
    then do
      aF        <- freshName "v"
      bF        <- freshName "v"
      rF        <- freshName "v"
      resultName <- freshName "v"
      recordF64Bits resultName
      let fOp = floatVariant op
      pure ( aOps ++ bOps ++
        [ "%" <> aF        <> " = arith.bitcast %" <> aName <> " : i64 to f64"
        , "%" <> bF        <> " = arith.bitcast %" <> bName <> " : i64 to f64"
        , "%" <> rF        <> " = " <> fOp <> " %" <> aF <> ", %" <> bF <> " : f64"
        , "%" <> resultName <> " = arith.bitcast %" <> rF <> " : f64 to i64"
        ]
        , resultName)
    else do
      resultName <- freshName "v"
      recordType resultName ty
      let binOp = "%" <> resultName <> " = " <> op <> " %" <> aName <> ", %" <> bName <> " : " <> ty
      pure (aOps ++ bOps ++ [binOp], resultName)

-- | Rewrite an integer arithmetic op to its float-arith counterpart.
-- For ops without a direct float variant, fall through to the
-- integer op unchanged — the caller will see a type mismatch and
-- we'll know to extend this table.
floatVariant :: Text -> Text
floatVariant "arith.addi"  = "arith.addf"
floatVariant "arith.subi"  = "arith.subf"
floatVariant "arith.muli"  = "arith.mulf"
floatVariant "arith.divsi" = "arith.divf"
floatVariant "arith.divui" = "arith.divf"
floatVariant other         = other

emitCmpOp :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitCmpOp pred' a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  -- Use the tracked type of the left operand for the comparison
  aTy <- lookupType aName
  -- For eq/ne: use structural comparison via runtime function.
  -- This handles boxed values (e.g. Char = C# codepoint) correctly,
  -- since pointer equality (cmpi eq) fails for separately allocated
  -- boxes containing the same value.
  if pred' `elem` ["eq", "ne"]
    then do
      eqName <- freshName "v"
      resultName <- freshName "v"
      let callOp = "%" <> eqName <> " = func.call @kk_structural_eq(%" <> aName <> ", %" <> bName <> ") : (i64, i64) -> i64"
      if pred' == "eq"
        then pure (aOps ++ bOps ++ [callOp], eqName)
        else do
          -- ne: negate the result (1 - eq)
          oneName <- freshName "v"
          let oneOp = "%" <> oneName <> " = arith.constant 1 : i64"
              subOp = "%" <> resultName <> " = arith.subi %" <> oneName <> ", %" <> eqName <> " : i64"
          pure (aOps ++ bOps ++ [callOp, oneOp, subOp], resultName)
    else do
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
  in any (\n -> n == "True" || n == "False") names

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
  -- Dead-branch safety: pattern-bound variables from the then-branch may
  -- be referenced in the default expression (dead code from single-ctor
  -- types that classifyBranches didn't optimize away). Pre-register them
  -- as zero aliases so the else branch doesn't emit broken extern calls.
  deadZero <- freshName "v"
  let patVarNames = [ nameToSsa nm | PatVar nm _ <- pats ]
      deadAliases = foldr (\k m -> Map.insert k deadZero m) savedA patVarNames
      deadZeroOp  = "%" <> deadZero <> " = arith.constant 0 : i64"
  modify (\s -> s { esAliases = deadAliases })
  (elseOps, elseResult) <- emitExpr defaultExpr
  modify (\s -> s { esAliases = savedA })
  resultName <- freshName "v"
  let elseOpsWithZero = if null patVarNames then elseOps
                        else deadZeroOp : elseOps
  let ifOps =
        [ "%" <> resultName <> " = scf.if %" <> cmpName <> " -> i64 {" ] ++
        map ("  " <>) (fieldOps ++ thenOps) ++
        [ "  scf.yield %" <> thenResult <> " : i64"
        , "} else {"
        ] ++
        map ("  " <>) elseOpsWithZero ++
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
  opsAndNames <- mapM (emitPatField scrutName structTy) (zip [1..length pats] pats)
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
-- Nested constructor pattern: extract this slot's value and then
-- recurse into the sub-patterns.  Without the recursion, PatVars
-- inside e.g. `Cons((m2, c2), rest)`'s inner Tuple2 never get
-- aliased — references to m2/c2 in the body would surface as
-- unresolved externs.
emitPatField scrutName structTy (idx, PatCon _ subPats) = do
  let fieldIdx = idx - 1
  idxName <- freshName "v"
  fieldName <- freshName "v"
  let extractOps =
        [ "%" <> idxName <> " = arith.constant " <> T.pack (show fieldIdx) <> " : i64"
        , "%" <> fieldName <> " = func.call @kk_field(%" <> scrutName <> ", %" <> idxName <> ") : (i64, i64) -> i64"
        ]
  -- Recurse: each sub-pattern at index (i+1) using fieldName as the
  -- new scrutinee.  Returned SSA names from inner calls are
  -- discarded (the inner emitPatField calls already register
  -- aliases via esAliases).
  subResults <- mapM (\(i, p) -> emitPatField fieldName structTy (i, p))
                     (zip [(1 :: Int)..] subPats)
  let subOps = concatMap fst subResults
  pure (extractOps ++ subOps, fieldName)
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
  -- MLIR scf.if regions are lexically isolated: SSA values defined in
  -- one branch are not visible in the other.  Save and restore esAliases
  -- around each branch so a let-binding emitted in the then-branch
  -- (e.g. HeadVar__20 <- field(L, 0) introduced by Mercury's ITE
  -- cond-bindings flow) doesn't overshadow the else-branch's mapping
  -- when the same Mercury variable was already bound to an outer SSA.
  -- Without this, the else-branch's references resolve to the
  -- then-branch's SSA name, which is out of scope in the else region
  -- and produces "use of undeclared SSA value".
  savedA <- gets esAliases
  (thenOps, thenResult) <- emitExpr thenExpr
  modify (\s -> s { esAliases = savedA })
  (elseOps, elseResult) <- emitExpr elseExpr
  modify (\s -> s { esAliases = savedA })
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
precomputeCaptures modPfx recBinds =
  precomputeCapturesWith modPfx
    [ qualifyBindName modPfx (Frankenstein.Core.Types.bindName b) | b <- recBinds ]
    recBinds

-- | Variant of precomputeCaptures that honours a per-bind list of
-- deconflicted qualified names supplied by the caller.  The list is
-- aligned positionally with @recBinds@: each element is the qualN that
-- the caller already registered in @esTopFns@ / @esTopFnArity@.  Without
-- this, when two binds in the same group would otherwise share a qualN
-- (the Mercury bridge reuses synthetic names like @V_87@ within one
-- predicate), the captures and arity overwrites land on the wrong
-- registration — and the call-site's arity belief disagrees with the
-- emitted function's signature.
precomputeCapturesWith :: Text -> [Text] -> [Bind] -> Emit ()
precomputeCapturesWith modPfx qualNs recBinds = do
  currentAliases <- gets esAliases
  topFns <- gets esTopFns
  promoted <- gets esPromotedFns
  priorCaps <- gets esPromotedCaptures
  let qualName n = let san = sanitizeName (nameText n)
                   in if T.any (== '/') (nameText n) then san else modPfx <> san
      -- Collect all parameter SSA names across all binds in this group.
      -- A free var in one bind that matches a sibling's parameter means
      -- the bind was originally nested and needs to capture that param.
      allGroupParamSsas = Set.unions
        [ Set.fromList (map (nameToSsa . fst) ps)
        | bnd <- recBinds
        , let ELam ps _body = unwrapLambda (bindExpr bnd) ]
      isInScope' n = let s = nameToSsa n
                     in Map.member s currentAliases
                        || Set.member (qualName n) topFns
                        || Map.member s promoted
                        || Set.member s allGroupParamSsas
      -- For each bind, compute direct captures (free vars that are in scope
      -- but NOT top-level fns and NOT other promoted fns).  The qualN is
      -- positionally supplied by the caller (may differ from
      -- @qualifyBindName modPfx (bindName bnd)@ when deconfliction renamed
      -- this bind to avoid arity collisions with another bind sharing the
      -- raw qualN).
      bindInfo = [ (qualN,
                    bnd, directCaps, promotedRefs', paramSsaNames)
                 | (qualN, bnd) <- zip qualNs recBinds
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
                                              && (Map.member k currentAliases
                                                  || Set.member k allGroupParamSsas)) extraKeys
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
emitBindAsTopFn modPfx bnd =
  emitBindAsTopFnWith modPfx
    (qualifyBindName modPfx (Frankenstein.Core.Types.bindName bnd)) bnd

-- | Variant of emitBindAsTopFn that takes an explicit deconflicted
-- qualN supplied by the caller.  Used to ensure the emitted func.func
-- symbol matches the same renamed name the caller registered in
-- @esTopFns@ / @esTopFnArity@ — necessary when two binds in the same
-- group share a raw qualN (Mercury bridge synthetic-name collisions)
-- but have different arities or captures.
emitBindAsTopFnWith :: Text -> Text -> Bind -> Emit [Text]
emitBindAsTopFnWith _modPfx qualN bnd = do
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
      -- The promoted top-level fn is a NEW MLIR function, so its
      -- scope contains only its captures (now as params) and its
      -- own params — the outer lambda's body-local SSAs are NOT
      -- accessible from here. Replacing esScopeSsa ensures
      -- emitFnAsValue doesn't synthesize references to outer-scope
      -- SSAs that would fail MLIR region-isolation.
      savedA     <- gets esAliases
      savedScope <- gets esScopeSsa
      let paramSsas    = [ nameToSsa pn | (pn, _) <- params ]
          capAliases   = [ (k, k) | k <- capSsaKeys ]
          paramAliases = [ (s, s) | s <- paramSsas ]
      modify (\s -> s { esAliases  = foldr (\(k,v) m -> Map.insert k v m)
                                           (esAliases s) (capAliases ++ paramAliases)
                       , esScopeSsa = Set.fromList (capSsaKeys ++ paramSsas)
                       })
      bodyText <- emitBody body mlirRetTy
      modify (\s -> s { esAliases  = savedA
                       , esScopeSsa = savedScope
                       })
      -- Emit as a lifted function (deduplicated by name).
      addLiftedFnOnce qualN $ T.unlines
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
-- DEFAULT first, then PatLit: GHC emits `case x of { __DEFAULT -> A; n# -> B }`
classifyBranches [Branch pat1 _ elseExpr, Branch (PatLit (LitInt n)) _ thenExpr]
  | isDefaultPat pat1 = IntLitCase n thenExpr elseExpr
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

-- Sanitize names for MLIR.
-- Operator characters get Z-encoded (GHC convention) so distinct operators
-- produce distinct linker symbols.  '.' stays '_' (module separator).
sanitizeName :: Text -> Text
sanitizeName t =
  let s = T.concatMap encodeChar t
  in case T.uncons s of
       -- MLIR identifiers must start with a letter or '_'; HLDS-derived
       -- names occasionally begin with a digit (e.g. '0_then_...' from a
       -- multi-line goal-text leak).  Prepend '_' in that case.
       Just (c, _) | c >= '0' && c <= '9' -> "_" <> s
       _                                   -> s
  where
    encodeChar '$' = "zd"
    encodeChar '+' = "zp"
    encodeChar '*' = "zt"
    encodeChar '-' = "zm"
    encodeChar '=' = "ze"
    encodeChar '<' = "zl"
    encodeChar '>' = "zg"
    encodeChar '!' = "zn"
    encodeChar '@' = "za"
    encodeChar '#' = "zh"
    encodeChar '%' = "zv"
    encodeChar '^' = "zc"
    encodeChar '&' = "zb"
    encodeChar '|' = "zo"
    encodeChar '~' = "zw"
    encodeChar ':' = "zi"
    encodeChar ';' = "zs"
    encodeChar '?' = "zq"
    encodeChar c
      | c `elem` ("/.,()[]{}'\"\\` \t\n\r" :: [Char]) = "_"
      | otherwise = T.singleton c

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

-- | Return a qualified name that's unique with respect to the
-- already-registered top-fn set.  If the proposed @qualN@ is already
-- present, append @_dupN@ suffixes until a fresh name is found.
-- This addresses the Mercury bridge's reuse of synthetic variable
-- names (e.g. @V_87@) for unrelated inner lambdas within one
-- enclosing predicate.  Without deconfliction, addLiftedFnOnce's
-- first-wins dedup silently merges the binds while the arity map's
-- last-wins overwrite leaves callers disagreeing on the arity —
-- callers using the wrong-arity wrapper crash mlir-opt with
-- "incorrect number of operands for callee".  The @arity@ argument
-- is currently unused but retained for callers that want to skip
-- the rename when the existing registration matches; we always
-- rename on collision because two binds with the same qualN
-- normally have different BODIES (lexically shadowed lambdas) so
-- the emitted func.func MUST stay separate.
dedupeQualN :: Text -> Int -> Emit Text
dedupeQualN qualN _arity = do
  topFns <- gets esTopFns
  lifted <- gets esLiftedNames
  -- Check BOTH the current scope's top-fn set AND the cumulative
  -- set of already-emitted lifted function names.  The latter
  -- catches inter-branch collisions: when two sibling match arms
  -- each contain a let-bound lambda with the same raw qualN, the
  -- second arm's emitLetBindings has restored topFns to the outer
  -- state (the first arm's registration is gone) so checking
  -- topFns alone misses the collision.  But esLiftedNames is
  -- global and never restored — it tracks every func.func ever
  -- emitted, so it correctly flags the second arm's bind as a
  -- collision against the first arm's already-emitted body.
  if Set.notMember qualN topFns && Set.notMember qualN lifted
    then pure qualN
    else tryAlt (1 :: Int)
  where
    tryAlt n = do
      tf <- gets esTopFns
      lf <- gets esLiftedNames
      let alt = qualN <> "_dup" <> T.pack (show n)
      if Set.member alt tf || Set.member alt lf
        then tryAlt (n + 1)
        else pure alt

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
  , "kk_alloc_con", "kk_set_field", "kk_field", "kk_tag", "kk_structural_eq"
  , "kk_thunk_create", "kk_thunk_create_forced", "kk_thunk_force"
  , "kk_evv_create", "kk_evv_set", "kk_evv_get", "kk_unhandled_effect"
  , "kk_handler_exec", "kk_handler_abort"
  , "kk_evv_extend", "kk_evv_lookup"
  , "kk_optab_create", "kk_optab_set", "kk_optab_get"
  , "printf", "puts", "exit", "exitWith", "malloc", "free"
  , "println_str", "print_str", "putStrLn"
  , "println_haskell_chars", "print_haskell_chars"
  , "int_to_haskell_chars", "int_list_to_haskell_chars"
  , "haskell_chars_concat"
  , "dummy_show_caf"
  , "rust_args_pack", "rust_print_dispatch", "rust_field_safe"
  , "rust_arg_debug"
  , "rust_arg_lower_hex", "rust_arg_upper_hex"
  , "rust_arg_octal", "rust_arg_binary"
  , "rust_arg_u32", "rust_arg_i32", "rust_arg_u64"
  , "rust_arg_u16", "rust_arg_i16"
  , "rust_arg_u8", "rust_arg_i8"
  , "rust_arg_f64", "rust_arg_f32"
  , "rust_struct_0"
  , "rust_struct_1", "rust_struct_2", "rust_struct_3", "rust_struct_4"
  , "rust_struct_5", "rust_struct_6", "rust_struct_7", "rust_struct_8"
  , "str_len", "str_concat", "str_eq", "str_flatten", "show_int"
  , "kk_list_concat", "kk_list_length"
  , "read_line", "getLine", "read_file", "write_file"
  , "string_empty"
  , "args_count", "args_get", "args_progname"
  , "new_ref", "get_ref", "set_ref"
  , "kk_println_con"
  -- Mercury choice effect runtime
  , "mercury_choose", "mercury_collect_choices"
  , "mercury_exn_fail", "mercury_fail"
  -- Idris2 bridge runtime stubs
  , "idris2_putStr", "idris_str_head"
  , "idris_crash", "_raise"
  , "idris_double_sin", "idris_double_cos", "idris_double_tan"
  , "idris_double_asin", "idris_double_acos", "idris_double_atan"
  , "idris_double_sqrt", "idris_double_exp", "idris_double_log"
  , "idris_double_floor", "idris_double_ceiling", "idris_double_pow"
  , "idris_double_add", "idris_double_sub", "idris_double_mul"
  , "idris_double_div", "idris_double_neg"
  , "idris_double_lt", "idris_double_lte", "idris_double_eq"
  , "idris_double_gte", "idris_double_gt"
  , "idris2_newIORef", "idris2_readIORef", "idris2_writeIORef"
  , "cast_Integer_Double", "cast_Double_Int", "cast_Double_String"
  -- Mercury surd-mercury integer-module substitution (i64-based; lossy
  -- vs arbitrary precision but sufficient for smoke tests).
  , "integer_zero", "integer_one", "integer_is_zero"
  , "integer_zl", "integer_zg", "integer_zezl", "integer_zgze"
  , "integer_zp", "integer_zm", "integer_zt", "integer_zs"
  , "integer_abs", "integer_signum", "integer_float", "integer_to_string"
  , "integer___"   -- integer.// (integer division — name sanitises
                   -- as dot + two slashes → three underscores)
  , "integer_rem"  -- integer.rem
  , "integer_mod"  -- integer.mod
  , "integer_neg"  -- unary integer negation (saturated counterpart to
                   -- the binary integer_zm)
  -- Mercury `int.<op>` aliases (i64 model — identical to integer.*)
  , "int_zp", "int_zm", "int_zt", "int_zs"
  , "int_zl", "int_zg", "int_zezl", "int_zgze", "int_zezeze"
  , "int_rem", "int_mod", "int_max", "int_min", "int_abs", "int_neg"
  , "integer_divide_with_rem"
  , "unify"
  , "io_format"     -- io.format(fmt, args, io) — pretty-printer
  , "require_error"      -- require.error/1 — fatal with message
  , "require_func_error" -- require.func_error/3 — same with type-info
  , "integer_integer"    -- integer.integer/1 — wrap (identity in i64 model)
  , "rational"      -- atom name passed to type_ctor_info (error path)
  -- Mercury higher-order dispatch shims (call into kk_call_closure_N)
  , "apply__2", "apply__3"
  , "call__2", "call__3"
  , "class_method_call__2", "class_method_call__3"
  -- Mercury list HO functions — typeclass dispatch prepends TypeInfo args
  , "list_foldl", "list_map", "list_filter", "list_length"
  , "list_append", "list_reverse", "list_condense", "list_duplicate"
  , "list_last", "list_member", "list_det_index0", "list_det_replace_nth"
  , "list_filter_map", "list_sort", "list_foldl2"
  , "apply__4", "call__4", "class_method_call__4"
  , "list_zpzp", "string_zpzp"
  , "char_det_from_int", "char_to_int"
  -- Mercury float arithmetic (IEEE-754 bit pattern in i64)
  , "float_zp", "float_zm", "float_zt", "float_zs"
  , "float_zl", "float_zg", "float_zezl", "float_zgze", "float_zezeze"
  , "float_abs", "float_neg", "float_max", "float_min", "float_float"
  , "float_round", "float_truncate", "float_floor", "float_ceiling"
  , "float_is_nan", "float_is_inf"
  -- Mercury math.* (float bit pattern in i64)
  , "math_pi", "math_e"
  , "math_sqrt", "math_sin", "math_cos", "math_tan"
  , "math_asin", "math_acos", "math_atan", "math_atan2"
  , "math_exp", "math_ln", "math_log10", "math_log2"
  , "math_pow", "math_sinh", "math_cosh", "math_tanh"
  -- require.unexpected/3 and private_builtin type-info helpers
  , "require_unexpected"
  , "private_builtin_type_info_from_typeclass_info"
  , "private_builtin_superclass_from_typeclass_info"
  , "private_builtin_instance_constructor_from_typeclass_info"
  -- Mercury string.* aliases (forward to kk_str_* helpers)
  , "string_length", "string_append", "string_from_char"
  , "string_int_to_string", "string_append_list", "string_join_list"
  , "string_index", "string_contains_char", "string_duplicate_char"
  , "string_to_int", "string_to_float", "string_sub_string_search"
  -- Mercury list.* (higher-order)
  , "list_all_true", "list_drop"
  , "list_delete_all", "list_det_tail", "list_det_split_last"
  , "list_take_upto", "list_sort_and_remove_dups", "list_map_corresponding"
  -- Mercury int / integer / float extras
  , "int_even", "int_odd"
  , "integer_pow", "integer_det_to_int"
  , "float_round_to_int", "float_truncate_to_int"
  -- Mercury negation + builtin comparison + range literal
  , "mercury_not", "builtin_compare", "builtin_ordering"
  , "list_range"
  -- Mercury map.* (cons-of-pairs implementation)
  , "map_init", "map_search", "map_lookup", "map_contains"
  , "map_count", "map_set", "map_det_insert", "map_det_update"
  , "map_delete", "map_from_assoc_list", "map_values", "map_keys"
  , "map_foldl"
  -- Mercury set.* (flat list with kk_structural_eq membership)
  , "set_init", "set_member", "set_insert", "set_delete"
  , "set_contains", "set_make_singleton_set", "set_count"
  , "set_to_sorted_list", "set_from_list", "set_is_empty"
  , "set_union", "set_intersect", "set_difference"
  ]

externalRuntimeArity :: Map Text Int
externalRuntimeArity = Map.fromList
  [ ("kk_drop", 1), ("kk_retain", 1), ("kk_release", 1)
  , ("kk_reuse", 2), ("kk_is_unique", 1)
  , ("kk_alloc_con", 2), ("kk_set_field", 3), ("kk_field", 2), ("kk_tag", 1), ("kk_structural_eq", 2)
  , ("kk_thunk_create", 1), ("kk_thunk_create_forced", 1), ("kk_thunk_force", 1)
  , ("kk_evv_create", 1), ("kk_evv_set", 3), ("kk_evv_get", 2)
  , ("kk_unhandled_effect", 0)
  , ("kk_handler_exec", 2), ("kk_handler_abort", 2)
  , ("kk_evv_extend", 3), ("kk_evv_lookup", 2)
  , ("kk_optab_create", 1), ("kk_optab_set", 3), ("kk_optab_get", 2)
  , ("printf", 2), ("puts", 1), ("exit", 1), ("exitWith", 1)
  , ("println_str", 1), ("print_str", 1), ("putStrLn", 1)
  , ("println_haskell_chars", 1), ("print_haskell_chars", 1)
  , ("int_to_haskell_chars", 2), ("int_list_to_haskell_chars", 2)
  , ("haskell_chars_concat", 2)
  , ("dummy_show_caf", 0)
  , ("rust_args_pack", 2), ("rust_print_dispatch", 1), ("rust_field_safe", 2)
  , ("rust_arg_debug", 1)
  , ("rust_arg_lower_hex", 1), ("rust_arg_upper_hex", 1)
  , ("rust_arg_octal", 1), ("rust_arg_binary", 1)
  , ("rust_arg_u32", 1), ("rust_arg_i32", 1), ("rust_arg_u64", 1)
  , ("rust_arg_u16", 1), ("rust_arg_i16", 1)
  , ("rust_arg_u8", 1), ("rust_arg_i8", 1)
  , ("rust_arg_f64", 1), ("rust_arg_f32", 1)
  , ("rust_struct_0", 2)
  , ("rust_struct_1", 3), ("rust_struct_2", 4), ("rust_struct_3", 5), ("rust_struct_4", 6)
  , ("rust_struct_5", 7), ("rust_struct_6", 8), ("rust_struct_7", 9), ("rust_struct_8", 10)
  , ("str_len", 1), ("str_concat", 2), ("str_eq", 2), ("str_flatten", 1)
  , ("kk_list_concat", 2), ("kk_list_length", 1)
  , ("show_int", 1)
  , ("read_line", 0), ("getLine", 0), ("string_empty", 0)
  , ("read_file", 1), ("write_file", 2)
  , ("args_count", 0), ("args_get", 1), ("args_progname", 0)
  , ("new_ref", 1), ("get_ref", 1), ("set_ref", 2)
  , ("kk_println_con", 3)
  , ("mercury_choose", 0), ("mercury_collect_choices", 1)
  , ("mercury_exn_fail", 0), ("mercury_fail", 0)
  , ("idris2_putStr", 2), ("idris_str_head", 1)
  , ("idris_crash", 2), ("_raise", 1)
  , ("idris_double_sin", 1), ("idris_double_cos", 1), ("idris_double_tan", 1)
  , ("idris_double_asin", 1), ("idris_double_acos", 1), ("idris_double_atan", 1)
  , ("idris_double_sqrt", 1), ("idris_double_exp", 1), ("idris_double_log", 1)
  , ("idris_double_floor", 1), ("idris_double_ceiling", 1), ("idris_double_pow", 2)
  , ("idris_double_add", 2), ("idris_double_sub", 2), ("idris_double_mul", 2)
  , ("idris_double_div", 2), ("idris_double_neg", 1)
  , ("idris_double_lt", 2), ("idris_double_lte", 2), ("idris_double_eq", 2)
  , ("idris_double_gte", 2), ("idris_double_gt", 2)
  , ("idris2_newIORef", 3), ("idris2_readIORef", 3), ("idris2_writeIORef", 4)
  , ("cast_Integer_Double", 1), ("cast_Double_Int", 1), ("cast_Double_String", 1)
  , ("integer_zero", 0), ("integer_one", 0), ("integer_is_zero", 1)
  , ("integer_zl", 2), ("integer_zg", 2), ("integer_zezl", 2), ("integer_zgze", 2)
  , ("integer_zp", 2), ("integer_zm", 2), ("integer_zt", 2), ("integer_zs", 2)
  , ("integer_abs", 1), ("integer_signum", 1), ("integer_float", 1)
  , ("integer_to_string", 1)
  , ("integer___", 2), ("integer_rem", 2), ("integer_mod", 2), ("integer_neg", 1)
  , ("int_zp", 2), ("int_zm", 2), ("int_zt", 2), ("int_zs", 2)
  , ("int_zl", 2), ("int_zg", 2), ("int_zezl", 2), ("int_zgze", 2), ("int_zezeze", 2)
  , ("int_rem", 2), ("int_mod", 2), ("int_max", 2), ("int_min", 2)
  , ("int_abs", 1), ("int_neg", 1)
  , ("integer_divide_with_rem", 3)
  , ("unify", 2)
  , ("io_format", 3), ("require_error", 1), ("rational", 0)
  , ("require_func_error", 3), ("integer_integer", 1)
  , ("apply__2", 2), ("apply__3", 3)
  , ("call__2", 2),  ("call__3", 3)
  , ("class_method_call__2", 2), ("class_method_call__3", 3)
  , ("list_foldl", 5), ("list_map", 4), ("list_filter", 3), ("list_length", 2)
  , ("list_append", 3), ("list_reverse", 2), ("list_condense", 2)
  , ("list_duplicate", 3), ("list_last", 2), ("list_member", 3)
  , ("list_det_index0", 3), ("list_det_replace_nth", 4)
  , ("list_filter_map", 4), ("list_sort", 2), ("list_foldl2", 8)
  , ("apply__4", 4), ("call__4", 4), ("class_method_call__4", 4)
  , ("list_zpzp", 3), ("string_zpzp", 2)
  , ("char_det_from_int", 1), ("char_to_int", 1)
  , ("float_zp", 2), ("float_zm", 2), ("float_zt", 2), ("float_zs", 2)
  , ("float_zl", 2), ("float_zg", 2), ("float_zezl", 2), ("float_zgze", 2)
  , ("float_zezeze", 2)
  , ("float_abs", 1), ("float_neg", 1)
  , ("float_max", 2), ("float_min", 2), ("float_float", 1)
  , ("float_round", 1), ("float_truncate", 1), ("float_floor", 1), ("float_ceiling", 1)
  , ("float_is_nan", 1), ("float_is_inf", 1)
  , ("math_pi", 0), ("math_e", 0)
  , ("math_sqrt", 1), ("math_sin", 1), ("math_cos", 1), ("math_tan", 1)
  , ("math_asin", 1), ("math_acos", 1), ("math_atan", 1), ("math_atan2", 2)
  , ("math_exp", 1), ("math_ln", 1), ("math_log10", 1), ("math_log2", 1)
  , ("math_pow", 2), ("math_sinh", 1), ("math_cosh", 1), ("math_tanh", 1)
  , ("require_unexpected", 3)
  , ("private_builtin_type_info_from_typeclass_info", 2)
  , ("private_builtin_superclass_from_typeclass_info", 2)
  , ("private_builtin_instance_constructor_from_typeclass_info", 2)
  , ("string_length", 1), ("string_append", 2), ("string_from_char", 1)
  , ("string_int_to_string", 1), ("string_append_list", 1)
  , ("string_join_list", 2), ("string_index", 2)
  , ("string_contains_char", 2), ("string_duplicate_char", 2)
  , ("string_to_int", 1), ("string_to_float", 1)
  , ("string_sub_string_search", 2)
  , ("list_all_true", 2), ("list_drop", 2)
  , ("list_delete_all", 3), ("list_det_tail", 2), ("list_det_split_last", 2)
  , ("list_take_upto", 3), ("list_sort_and_remove_dups", 2)
  , ("list_map_corresponding", 6)
  , ("int_even", 1), ("int_odd", 1)
  , ("integer_pow", 2), ("integer_det_to_int", 1)
  , ("float_round_to_int", 1), ("float_truncate_to_int", 1)
  , ("mercury_not", 1), ("builtin_compare", 4), ("builtin_ordering", 3)
  , ("list_range", 2)
  , ("map_init", 1), ("map_search", 3), ("map_lookup", 3)
  , ("map_contains", 3), ("map_count", 2)
  , ("map_set", 4), ("map_det_insert", 4), ("map_det_update", 4)
  , ("map_delete", 3), ("map_from_assoc_list", 2)
  , ("map_values", 2), ("map_keys", 2)
  , ("map_foldl", 6)
  , ("set_init", 1), ("set_member", 3), ("set_insert", 3)
  , ("set_delete", 3), ("set_contains", 3)
  , ("set_make_singleton_set", 2), ("set_count", 2)
  , ("set_to_sorted_list", 2), ("set_from_list", 2)
  , ("set_is_empty", 2)
  , ("set_union", 3), ("set_intersect", 3), ("set_difference", 3)
  ]

-- | Convert a Name to a unique MLIR SSA name.
-- Always append the unique ID to avoid collisions: GHC Core regularly reuses
-- short names like "v", "a", "x" with different uniques in the same scope.
nameToSsa :: Name -> Text
nameToSsa n = sanitizeName (nameText n) <> T.pack (show (nameUnique n))

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

-- | Run a shell command, returning Left on failure.
-- Flattened helper avoids deeply nested case expressions that cause
-- exponential blowup in the self-hosted emitter.
runCmd :: String -> [String] -> String -> Text -> IO (Either Text String)
runCmd cmd args input label = do
  (ec, out, err) <- readProcessWithExitCode cmd args input
  case ec of
    ExitFailure _ -> pure $ Left $ label <> " failed: " <> T.pack err
    ExitSuccess   -> pure $ Right out

-- | Full compilation pipeline
compileToExecutable :: EmitConfig -> Program -> IO (Either Text FilePath)
compileToExecutable config prog = do
  let mlirText = emitProgramText prog
      mlirPath = ecOutputPath config ++ ".mlir"
      optPath = ecOutputPath config ++ ".opt.mlir"
      llPath = ecOutputPath config ++ ".ll"

  TIO.writeFile mlirPath mlirText

  r1 <- runCmd (ecMlirOptPath config)
    ["--convert-linalg-to-loops",
     "--expand-strided-metadata",
     "--finalize-memref-to-llvm",
     "--convert-scf-to-cf", "--convert-func-to-llvm", "--convert-arith-to-llvm",
     "--convert-cf-to-llvm", "--reconcile-unrealized-casts", mlirPath] "" "mlir-opt"
  case r1 of
    Left e -> pure (Left e)
    Right out1 -> do
      writeFile optPath out1
      r2 <- runCmd (ecMlirTranslatePath config) ["--mlir-to-llvmir", optPath] "" "mlir-translate"
      case r2 of
        Left e -> pure (Left e)
        Right out2 -> do
          writeFile llPath out2
          compileToExecutableLink config llPath

-- | Link step of compileToExecutable, separated to keep nesting shallow.
compileToExecutableLink :: EmitConfig -> FilePath -> IO (Either Text FilePath)
compileToExecutableLink config llPath =
  case ecKokaRuntimePath config of
    Nothing -> do
      r <- runCmd (ecClangPath config)
        [llPath, "-x", "ir", "-o", ecOutputPath config,
         "-O" ++ show (ecOptLevel config)] "" "clang"
      case r of
        Left e  -> pure (Left e)
        Right _ -> pure (Right (ecOutputPath config))
    Just rtPath -> do
      let rtDir = reverse . dropWhile (/= '/') . reverse $ rtPath
          cyclePath = rtDir ++ "kk_cycle.c"
          arenaPath = rtDir ++ "kk_arena.c"
          rtObjPath = ecOutputPath config ++ ".rt.o"
          cycleObjPath = ecOutputPath config ++ ".cycle.o"
          arenaObjPath = ecOutputPath config ++ ".arena.o"
          optFlag = "-O" ++ show (ecOptLevel config)
          includeFlag = "-I" ++ rtDir
      r1 <- runCmd (ecClangPath config)
        ["-c", rtPath, "-o", rtObjPath, includeFlag, optFlag] "" "clang (runtime)"
      case r1 of
        Left e -> pure (Left e)
        Right _ -> do
          r2 <- runCmd (ecClangPath config)
            ["-c", cyclePath, "-o", cycleObjPath, includeFlag, optFlag] "" "clang (cycle)"
          case r2 of
            Left e -> pure (Left e)
            Right _ -> do
              r3 <- runCmd (ecClangPath config)
                ["-c", arenaPath, "-o", arenaObjPath, includeFlag, optFlag] "" "clang (arena)"
              case r3 of
                Left e -> pure (Left e)
                Right _ -> do
                  r4 <- runCmd (ecClangPath config)
                    ["-x", "ir", llPath, "-x", "none", rtObjPath, cycleObjPath, arenaObjPath,
                     "-o", ecOutputPath config, optFlag, "-lm"] "" "clang (link)"
                  case r4 of
                    Left e  -> pure (Left e)
                    Right _ -> pure (Right (ecOutputPath config))

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
      wasmPath = if drop (length outBase - 5) outBase == ".wasm" then outBase else outBase ++ ".wasm"

  TIO.writeFile mlirPath mlirText

  r1 <- runCmd (ecMlirOptPath config)
    ["--convert-scf-to-cf", "--convert-func-to-llvm", "--convert-arith-to-llvm",
     "--convert-cf-to-llvm", "--reconcile-unrealized-casts", mlirPath] "" "mlir-opt"
  case r1 of
    Left e -> pure (Left e)
    Right out1 -> do
      writeFile optPath out1
      r2 <- runCmd (ecMlirTranslatePath config) ["--mlir-to-llvmir", optPath] "" "mlir-translate"
      case r2 of
        Left e -> pure (Left e)
        Right out2 -> do
          writeFile llPath out2
          r3 <- runCmd "llc"
            ["-mtriple=wasm32-unknown-unknown", "-filetype=obj", "-O2",
             llPath, "-o", wasmObjPath] "" "llc (wasm32)"
          case r3 of
            Left e -> pure (Left e)
            Right _ -> compileToWasmLink config outBase wasmObjPath wasmPath

-- | Link step of compileToWasm, separated to keep nesting shallow.
compileToWasmLink :: EmitConfig -> FilePath -> FilePath -> FilePath -> IO (Either Text FilePath)
compileToWasmLink config outBase wasmObjPath wasmPath =
  case ecKokaRuntimePath config of
    Nothing -> do
      r <- runCmd "wasm-ld"
        ["--no-entry", "--export=_frankenstein_main",
         "--allow-undefined", wasmObjPath, "-o", wasmPath] "" "wasm-ld"
      case r of
        Left e  -> pure (Left e)
        Right _ -> pure (Right wasmPath)
    Just rtPath -> do
      let rtDir = reverse . dropWhile (/= '/') . reverse $ rtPath
          wasmRtSrc = rtDir ++ "kk_runtime_wasm.c"
          wasmRtObjPath = outBase ++ ".wasm.rt.o"
      r1 <- runCmd (ecClangPath config)
        ["--target=wasm32-unknown-unknown", "-O2", "-nostdlib",
         "-c", wasmRtSrc, "-o", wasmRtObjPath] "" "clang (wasm runtime)"
      case r1 of
        Left e -> pure (Left e)
        Right _ -> do
          r2 <- runCmd "wasm-ld"
            ["--no-entry", "--export=_frankenstein_main",
             "--allow-undefined", wasmObjPath, wasmRtObjPath,
             "-o", wasmPath] "" "wasm-ld"
          case r2 of
            Left e  -> pure (Left e)
            Right _ -> pure (Right wasmPath)
