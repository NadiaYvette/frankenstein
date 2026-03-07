-- | Frankenstein Core -> MLIR Emitter
--
-- Emits textual MLIR, then invokes mlir-opt → mlir-translate → clang
-- to produce a native executable.

module Frankenstein.MlirEmit.Emitter
  ( emitProgram
  , emitProgramText
  , compileToExecutable
  , EmitConfig(..)
  , defaultEmitConfig
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.IORef
import System.Process (readProcessWithExitCode, readProcess)
import System.Exit (ExitCode(..))
import Control.Monad.State

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

-- Emission state: tracks SSA counter and collected top-level functions
data EmitState = EmitState
  { esCounter   :: !Int
  , esLiftedFns :: ![Text]  -- accumulated lifted lambda functions
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

-- | Emit a Frankenstein Core program as MLIR text
emitProgram :: Program -> Text
emitProgram = emitProgramText

emitProgramText :: Program -> Text
emitProgramText prog =
  let -- Rename user's "main" to "_frankenstein_main" so we can generate our own entry point
      defs = progDefs prog
      hasMain = any (\d -> nameText (qnameName (defName d)) == "main") defs
      renamedDefs = if hasMain
        then map (\d -> if nameText (qnameName (defName d)) == "main"
                        then d { defName = QName "" (Name "_frankenstein_main" 99) }
                        else d) defs
        else defs
      initState = EmitState 0 []
      (bodyText, finalState) = runState (emitDefs renamedDefs) initState
      liftedFns = T.unlines (reverse (esLiftedFns finalState))
      mainWrapper = if hasMain
        then T.unlines
          [ "  func.func @main() -> i32 {"
          , "    %result = func.call @_frankenstein_main() : () -> i64"
          , "    %fmtaddr = llvm.mlir.addressof @fmt_int : !llvm.ptr"
          , "    llvm.call @printf(%fmtaddr, %result) vararg(!llvm.func<i32 (ptr, ...)>) : (!llvm.ptr, i64) -> i32"
          , "    %zero = arith.constant 0 : i32"
          , "    func.return %zero : i32"
          , "  }"
          ]
        else ""
  in T.unlines
    [ "module {"
    , ""
    , "  // External declarations"
    , "  llvm.func @printf(!llvm.ptr, ...) -> i32"
    , "  llvm.mlir.global internal constant @fmt_int(\"%ld\\n\\00\") {addr_space = 0 : i32}"
    , "  llvm.mlir.global internal constant @fmt_str(\"%s\\n\\00\") {addr_space = 0 : i32}"
    , ""
    , "  // Lifted functions"
    , liftedFns
    , ""
    , bodyText
    , mainWrapper
    , "}"
    ]

emitDefs :: [Def] -> Emit Text
emitDefs defs = do
  texts <- mapM emitDef defs
  pure $ T.unlines texts

emitDef :: Def -> Emit Text
emitDef def = do
  let name = nameText (qnameName (defName def))
      (argNames, argTypes, retType) = decomposeDefType (defType def)
  case defExpr def of
    ELam params body -> do
      let mlirArgs = T.intercalate ", "
            [ "%" <> nameText pn <> ": " <> typeToMlir pt | (pn, pt) <- params ]
          mlirRetTy = typeToMlir retType
      bodyText <- emitBody body mlirRetTy
      pure $ T.unlines
        [ "  func.func @" <> sanitizeName name <> "(" <> mlirArgs <> ") -> " <> mlirRetTy <> " {"
        , bodyText
        , "  }"
        ]
    -- Non-lambda top-level: emit as nullary function
    expr -> do
      let mlirRetTy = typeToMlir retType
      bodyText <- emitBody expr mlirRetTy
      pure $ T.unlines
        [ "  func.func @" <> sanitizeName name <> "() -> " <> mlirRetTy <> " {"
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
  pure (["%" <> name <> " = arith.constant " <> T.pack (show n) <> " : f64"], name)

emitExpr (EVar n) = do
  -- Variable reference — assume it's already in scope as an SSA value
  pure ([], sanitizeName (nameText n))

emitExpr (EApp (EVar fn) [a, b])
  | nameText fn == "+" || nameText fn == "add" = emitBinOp "arith.addi" "i64" a b
  | nameText fn == "-" || nameText fn == "sub" = emitBinOp "arith.subi" "i64" a b
  | nameText fn == "*" || nameText fn == "mul" = emitBinOp "arith.muli" "i64" a b
  | nameText fn == "==" || nameText fn == "eq" = emitCmpOp "eq" a b
  | nameText fn == "/=" || nameText fn == "ne" = emitCmpOp "ne" a b
  | nameText fn == "<" || nameText fn == "lt"  = emitCmpOp "slt" a b
  | nameText fn == ">" || nameText fn == "gt"  = emitCmpOp "sgt" a b
  | nameText fn == "<=" || nameText fn == "le" = emitCmpOp "sle" a b
  | nameText fn == ">=" || nameText fn == "ge" = emitCmpOp "sge" a b

emitExpr (EApp (EVar fn) args) = do
  -- General function call
  argResults <- mapM emitExpr args
  let allOps = concatMap fst argResults
      argNames = map snd argResults
      argList = T.intercalate ", " ["%" <> n | n <- argNames]
      argTypeList = T.intercalate ", " (replicate (length args) "i64")
  resultName <- freshName "v"
  let callOp = "%" <> resultName <> " = func.call @" <> sanitizeName (nameText fn)
               <> "(" <> argList <> ") : (" <> argTypeList <> ") -> i64"
  pure (allOps ++ [callOp], resultName)

emitExpr (ECase scrut branches) = do
  -- Pattern matching: for integer literals, use cmpi + scf.if
  -- For now: handle the common case of matching on 0 vs default
  (scrutOps, scrutName) <- emitExpr scrut
  case branches of
    [Branch (PatLit (LitInt 0)) _ thenExpr, Branch _ _ elseExpr] ->
      emitIntCase scrutOps scrutName 0 thenExpr elseExpr
    [Branch (PatLit (LitInt n)) _ thenExpr, Branch _ _ elseExpr] ->
      emitIntCase scrutOps scrutName n thenExpr elseExpr
    -- Two branches: treat first as "then" for when scrut is truthy
    [Branch _ _ thenExpr, Branch _ _ elseExpr] -> do
      emitIfElse scrutOps scrutName thenExpr elseExpr
    -- Single branch: just emit the body
    [Branch _ _ body] -> do
      (bodyOps, bodyName) <- emitExpr body
      pure (scrutOps ++ bodyOps, bodyName)
    _ -> do
      name <- freshName "v"
      pure (scrutOps ++ ["// unhandled case with " <> T.pack (show (length branches)) <> " branches",
                          "%" <> name <> " = arith.constant 0 : i64"], name)

emitExpr (ELet [binds] body) = do
  bindOps <- concat <$> mapM emitBind binds
  (bodyOps, bodyName) <- emitExpr body
  pure (bindOps ++ bodyOps, bodyName)

emitExpr (ELet (bg:bgs) body) = do
  bindOps <- concat <$> mapM emitBind bg
  (restOps, restName) <- emitExpr (ELet bgs body)
  pure (bindOps ++ restOps, restName)

emitExpr (ELam params body) = do
  -- Lambda should have been lifted to top level. If we encounter one inline,
  -- lift it now and emit a reference to it.
  liftedName <- freshName "lambda"
  let mlirArgs = T.intercalate ", "
        [ "%" <> nameText pn <> ": i64" | (pn, _) <- params ]
  (bodyOps, bodyResult) <- emitExpr body
  let fnText = T.unlines $
        [ "  func.func @" <> liftedName <> "(" <> mlirArgs <> ") -> i64 {" ] ++
        map ("    " <>) bodyOps ++
        [ "    func.return %" <> bodyResult <> " : i64"
        , "  }" ]
  addLiftedFn fnText
  -- Return the function name as a value (not quite right for MLIR, but functional)
  name <- freshName "v"
  pure (["// lambda lifted as @" <> liftedName,
         "%" <> name <> " = arith.constant 0 : i64"], name)

-- Perceus operations
emitExpr (EDrop e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++ ["// drop %" <> eName], eName)

emitExpr (ERetain e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++ ["// retain %" <> eName], eName)

emitExpr (EDelay e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++ ["// delay (thunk) %" <> eName], eName)

emitExpr (EForce e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++ ["// force (thunk) %" <> eName], eName)

emitExpr _ = do
  name <- freshName "v"
  pure (["%" <> name <> " = arith.constant 0 : i64  // unimplemented expr"], name)

-- Helpers

emitBinOp :: Text -> Text -> Expr -> Expr -> Emit ([Text], Text)
emitBinOp op ty a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  resultName <- freshName "v"
  let binOp = "%" <> resultName <> " = " <> op <> " %" <> aName <> ", %" <> bName <> " : " <> ty
  pure (aOps ++ bOps ++ [binOp], resultName)

emitCmpOp :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitCmpOp pred' a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  resultName <- freshName "v"
  let cmpOp = "%" <> resultName <> " = arith.cmpi " <> pred' <> ", %" <> aName <> ", %" <> bName <> " : i64"
  pure (aOps ++ bOps ++ [cmpOp], resultName)

emitIntCase :: [Text] -> Text -> Integer -> Expr -> Expr -> Emit ([Text], Text)
emitIntCase scrutOps scrutName litVal thenExpr elseExpr = do
  -- Compare scrutinee to literal value
  cmpName <- freshName "cmp"
  let litOps = if litVal == 0
        then ["%" <> cmpName <> " = arith.cmpi eq, %" <> scrutName <> ", %" <> scrutName <> " : i64"
             -- Actually: compare to constant 0
             ]
        else []
  -- Use constant comparison
  zeroName <- freshName "v"
  cmpName2 <- freshName "cmp"
  let cmpOps = [ "%" <> zeroName <> " = arith.constant " <> T.pack (show litVal) <> " : i64"
               , "%" <> cmpName2 <> " = arith.cmpi eq, %" <> scrutName <> ", %" <> zeroName <> " : i64"
               ]
  emitScfIf (scrutOps ++ cmpOps) cmpName2 thenExpr elseExpr

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
  let bname = sanitizeName (nameText (Frankenstein.Core.Types.bindName bnd))
  if bname == resultName
    then pure ops
    else pure $ ops ++ ["// let " <> bname <> " = %" <> resultName]

-- Type decomposition
decomposeDefType :: Type -> ([Text], [Type], Type)
decomposeDefType (TFun args _eff ret) =
  ( [T.pack ("arg" ++ show i) | i <- [0..length args - 1]]
  , map snd args
  , ret )
decomposeDefType (TForall _ body) = decomposeDefType body
decomposeDefType t = ([], [], t)

-- Type to MLIR type string
typeToMlir :: Type -> Text
typeToMlir (TCon tc)
  | n == "int" || n == "i64" || n == "integer" = "i64"
  | n == "i32"    = "i32"
  | n == "float" || n == "f64" = "f64"
  | n == "f32"    = "f32"
  | n == "bool"   = "i1"
  | n == "unit"   = "i64"  -- represent unit as i64 (0) for simplicity
  | otherwise     = "i64"  -- default to i64 for unrecognized types
  where n = nameText (qnameName (tcName tc))
typeToMlir _ = "i64"

-- Sanitize names for MLIR (replace special chars)
sanitizeName :: Text -> Text
sanitizeName = T.map (\c -> if c `elem` ("+*-/=<>!@#$%^&|~" :: [Char]) then '_' else c)

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

          -- clang: LLVM IR → executable
          (ec3, _, err3) <- readProcessWithExitCode (ecClangPath config)
            [llPath, "-x", "ir", "-o", ecOutputPath config,
             "-O" ++ show (ecOptLevel config)] ""
          case ec3 of
            ExitFailure _ -> pure $ Left $ "clang failed: " <> T.pack err3
            ExitSuccess -> pure $ Right $ ecOutputPath config
