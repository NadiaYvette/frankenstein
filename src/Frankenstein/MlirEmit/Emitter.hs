-- | Frankenstein Core -> MLIR Emitter
--
-- Translates Frankenstein Core IR into MLIR textual format, then
-- invokes mlir-opt and mlir-translate to produce LLVM IR, and
-- finally compiles to native code via llc/clang.
--
-- The emitter maps Core constructs to MLIR dialects:
--   EVar, ELit, ECon    -> arith constants, variable references
--   EApp                -> func.call
--   ELam                -> func.func (lifted to top level)
--   ELet                -> SSA value definitions
--   ECase               -> scf.if or arith.cmpi + branch
--   ERetain/ERelease    -> calls to Koka C runtime
--   EDelay/EForce       -> calls to thunk allocator/forcer
--   EPerform/EHandle    -> evidence vector manipulation

module Frankenstein.MlirEmit.Emitter
  ( emitProgram
  , compileToExecutable
  , EmitConfig(..)
  , defaultEmitConfig
  ) where

import Frankenstein.Core.Types
import Frankenstein.MlirEmit.Dialects

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

data EmitConfig = EmitConfig
  { ecMlirOptPath       :: !FilePath
  , ecMlirTranslatePath :: !FilePath
  , ecClangPath         :: !FilePath
  , ecKokaRuntimePath   :: !(Maybe FilePath)
  , ecOptLevel          :: !Int
  , ecOutputPath        :: !FilePath
  } deriving (Show)

defaultEmitConfig :: EmitConfig
defaultEmitConfig = EmitConfig
  { ecMlirOptPath       = "mlir-opt"
  , ecMlirTranslatePath = "mlir-translate"
  , ecClangPath         = "clang"
  , ecKokaRuntimePath   = Nothing
  , ecOptLevel          = 0
  , ecOutputPath        = "a.out"
  }

-- | Emit a Frankenstein Core program as an MLIR module
emitProgram :: Program -> MlirModule
emitProgram prog =
  let -- Collect all definitions and emit as MLIR functions
      funcs = map emitDef (progDefs prog)
      -- External declarations for Koka runtime
      externs = kokaRuntimeExterns
  in MlirModule
    { modFuncs = funcs
    , modExtern = externs
    }

-- | Koka C runtime external declarations
kokaRuntimeExterns :: [(Text, [MlirType], [MlirType])]
kokaRuntimeExterns =
  [ ("kk_retain",    [MlirPtr], [])
  , ("kk_release",   [MlirPtr], [])
  , ("kk_drop",      [MlirPtr], [])
  , ("kk_reuse",     [MlirPtr, MlirI32], [MlirPtr])
  , ("kk_is_unique", [MlirPtr], [MlirI1])
  , ("kk_alloc",     [MlirI64], [MlirPtr])
  , ("kk_thunk_alloc",  [MlirPtr], [MlirPtr])   -- allocate thunk
  , ("kk_thunk_force",  [MlirPtr], [MlirPtr])   -- force thunk
  , ("kk_evv_get",      [],        [MlirPtr])   -- get evidence vector
  , ("kk_evv_push",     [MlirPtr], [])           -- push handler
  , ("kk_evv_pop",      [],        [])           -- pop handler
  , ("kk_evv_lookup",   [MlirI32], [MlirPtr])   -- lookup by effect tag
  ]

-- | Emit a single definition as an MLIR function
emitDef :: Def -> MlirFunc
emitDef def =
  let name = nameText (qnameName (defName def))
      (argNames, argTypes, retType) = decomposeType (defType def)
      mlirArgs = zip (map (\n -> "%" <> n) argNames) (map typeToMlir argTypes)
      mlirRets = [typeToMlir retType]
      -- Emit the body
      (ops, resultName) = emitExpr 0 (defExpr def)
      -- Add return
      retOp = (Nothing, FuncReturn [MlirValue ("%" <> resultName) (typeToMlir retType)])
      block = MlirBlock
        { blockLabel = "entry"
        , blockArgs = []
        , blockOps = ops ++ [retOp]
        }
  in MlirFunc
    { funcName = name
    , funcArgs = mlirArgs
    , funcResults = mlirRets
    , funcBlocks = [block]
    , funcAttrs = []
    }

-- | Decompose a function type into argument names, types, and return type
decomposeType :: Type -> ([Text], [Type], Type)
decomposeType (TFun args _eff ret) =
  let argNames = [T.pack ("arg" ++ show i) | i <- [0..length args - 1]]
      argTypes = map snd args
  in (argNames, argTypes, ret)
decomposeType (TForall _ body) = decomposeType body
decomposeType t = ([], [], t)

-- | Convert Frankenstein Type to MLIR type
typeToMlir :: Type -> MlirType
typeToMlir (TCon tc)
  | nameText (qnameName (tcName tc)) == "int"    = MlirI64
  | nameText (qnameName (tcName tc)) == "i32"    = MlirI32
  | nameText (qnameName (tcName tc)) == "i64"    = MlirI64
  | nameText (qnameName (tcName tc)) == "float"  = MlirF64
  | nameText (qnameName (tcName tc)) == "f32"    = MlirF32
  | nameText (qnameName (tcName tc)) == "f64"    = MlirF64
  | nameText (qnameName (tcName tc)) == "bool"   = MlirI1
  | nameText (qnameName (tcName tc)) == "char"   = MlirI32
  | nameText (qnameName (tcName tc)) == "unit"   = MlirNone
  | nameText (qnameName (tcName tc)) == "string" = MlirPtr
  | otherwise = MlirPtr  -- boxed/heap-allocated value
typeToMlir _ = MlirPtr  -- default: pointer to boxed value

-- | Emit a Core expression, producing MLIR operations and a result name.
-- Returns (operations, result_name) where result_name is the SSA name
-- of the value produced.
emitExpr :: Int -> Expr -> ([(Maybe Text, MlirOp)], Text)
emitExpr counter expr = case expr of
  ELit (LitInt n) ->
    let name = T.pack ("v" ++ show counter)
    in ([(Just name, ArithConstI n MlirI64)], name)

  ELit (LitFloat n) ->
    let name = T.pack ("v" ++ show counter)
    in ([(Just name, ArithConstF n MlirF64)], name)

  EVar n ->
    -- Variable reference: just use the name
    ([], nameText n)

  EApp (EVar fn) args ->
    let -- Emit each argument
        (argOps, argNames) = emitArgs (counter) args
        nextCounter = counter + length args
        resultName = T.pack ("v" ++ show nextCounter)
        callOp = (Just resultName,
                  FuncCall (nameText fn)
                           [MlirValue ("%" <> n) MlirPtr | n <- argNames]
                           [MlirPtr])
    in (argOps ++ [callOp], resultName)

  ELet [binds] body ->
    let -- Emit each binding
        (bindOps, _) = emitBindGroup counter binds
        nextCounter = counter + length binds
        (bodyOps, bodyResult) = emitExpr nextCounter body
    in (bindOps ++ bodyOps, bodyResult)

  ECase scrut branches ->
    let (scrutOps, scrutName) = emitExpr counter scrut
        -- For now: emit as a series of comparisons
        -- Real implementation needs proper pattern match compilation
        resultName = T.pack ("v" ++ show (counter + 1))
    in (scrutOps ++ [(Just resultName, RawMlir $ "// case on %" <> scrutName)], resultName)

  -- Perceus operations
  ERetain e ->
    let (eOps, eName) = emitExpr counter e
    in (eOps ++ [(Nothing, KokaRetain (MlirValue ("%" <> eName) MlirPtr))], eName)

  ERelease e ->
    let (eOps, eName) = emitExpr counter e
    in (eOps ++ [(Nothing, KokaRelease (MlirValue ("%" <> eName) MlirPtr))], eName)

  EDrop e ->
    let (eOps, eName) = emitExpr counter e
    in (eOps ++ [(Nothing, KokaDrop (MlirValue ("%" <> eName) MlirPtr))], eName)

  -- Laziness operations
  EDelay e ->
    let (eOps, eName) = emitExpr counter e
        thunkName = T.pack ("v" ++ show (counter + length eOps))
    in (eOps ++ [(Just thunkName,
                  FuncCall "kk_thunk_alloc"
                           [MlirValue ("%" <> eName) MlirPtr]
                           [MlirPtr])],
        thunkName)

  EForce e ->
    let (eOps, eName) = emitExpr counter e
        resultName = T.pack ("v" ++ show (counter + length eOps))
    in (eOps ++ [(Just resultName,
                  FuncCall "kk_thunk_force"
                           [MlirValue ("%" <> eName) MlirPtr]
                           [MlirPtr])],
        resultName)

  _ ->
    let name = T.pack ("v" ++ show counter)
    in ([(Just name, RawMlir $ "// unimplemented: " <> T.pack (take 80 (show expr)))], name)

emitArgs :: Int -> [Expr] -> ([(Maybe Text, MlirOp)], [Text])
emitArgs _ [] = ([], [])
emitArgs counter (e:es) =
  let (eOps, eName) = emitExpr counter e
      (restOps, restNames) = emitArgs (counter + length eOps + 1) es
  in (eOps ++ restOps, eName : restNames)

emitBindGroup :: Int -> [Bind] -> ([(Maybe Text, MlirOp)], [Text])
emitBindGroup _ [] = ([], [])
emitBindGroup counter (b:bs) =
  let (bOps, bName) = emitExpr counter (bindExpr b)
      -- Alias the binding name to the result
      name = nameText (bindName b)
      aliasOps = if name == bName then [] else [(Just name, RawMlir $ "// alias " <> name <> " = %" <> bName)]
      (restOps, restNames) = emitBindGroup (counter + length bOps + 1) bs
  in (bOps ++ aliasOps ++ restOps, bName : restNames)

-- | Full compilation pipeline: Core -> MLIR text -> mlir-opt -> LLVM IR -> executable
compileToExecutable :: EmitConfig -> Program -> IO (Either Text FilePath)
compileToExecutable config prog = do
  let mlirMod = emitProgram prog
      mlirText = renderModule mlirMod

  -- Write MLIR to temp file
  let mlirPath = ecOutputPath config ++ ".mlir"
  TIO.writeFile mlirPath mlirText

  -- Run mlir-opt for optimization
  let optFlags = if ecOptLevel config > 0
                 then ["--convert-scf-to-cf", "--convert-func-to-llvm",
                       "--convert-arith-to-llvm", "--reconcile-unrealized-casts"]
                 else ["--convert-func-to-llvm", "--convert-arith-to-llvm",
                       "--reconcile-unrealized-casts"]
  (ec1, out1, err1) <- readProcessWithExitCode (ecMlirOptPath config)
    (optFlags ++ [mlirPath]) ""
  case ec1 of
    ExitFailure _ -> pure $ Left $ T.pack $ "mlir-opt failed: " ++ err1
    ExitSuccess -> do
      -- Write optimized MLIR
      let optPath = ecOutputPath config ++ ".opt.mlir"
      writeFile optPath out1

      -- Translate to LLVM IR
      (ec2, out2, err2) <- readProcessWithExitCode (ecMlirTranslatePath config)
        ["--mlir-to-llvmir", optPath] ""
      case ec2 of
        ExitFailure _ -> pure $ Left $ T.pack $ "mlir-translate failed: " ++ err2
        ExitSuccess -> do
          -- Write LLVM IR
          let llPath = ecOutputPath config ++ ".ll"
          writeFile llPath out2

          -- Compile to executable
          let runtimeFlags = case ecKokaRuntimePath config of
                Just p  -> ["-L" ++ p, "-lkklib"]
                Nothing -> []
          (ec3, _, err3) <- readProcessWithExitCode (ecClangPath config)
            ([llPath, "-o", ecOutputPath config, "-O" ++ show (ecOptLevel config)]
             ++ runtimeFlags) ""
          case ec3 of
            ExitFailure _ -> pure $ Left $ T.pack $ "clang failed: " ++ err3
            ExitSuccess -> pure $ Right $ ecOutputPath config
