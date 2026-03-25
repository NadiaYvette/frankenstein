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
import Data.Set (Set)
import qualified Data.Set as Set

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
    , "  // Perceus runtime declarations"
    , "  func.func private @kk_drop(i64) -> ()"
    , "  func.func private @kk_retain(i64) -> ()"
    , "  func.func private @kk_release(i64) -> ()"
    , "  func.func private @kk_reuse(i64) -> i64"
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
      (_argNames, _argTypes, retType) = decomposeDefType (defType def)
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

emitExpr (ELit (LitChar c)) = do
  name <- freshName "v"
  pure (["%" <> name <> " = arith.constant " <> T.pack (show (fromEnum c)) <> " : i64"], name)

emitExpr (ELit (LitString s)) = do
  -- Strings are represented as i64 0 for now (would need llvm.mlir.global for real strings)
  name <- freshName "v"
  pure (["// string literal: \"" <> s <> "\""
        , "%" <> name <> " = arith.constant 0 : i64"], name)

emitExpr (EVar n) = do
  -- Variable reference — assume it's already in scope as an SSA value
  pure ([], sanitizeName (nameText n))

-- Constructor reference: create a tagged struct
-- Layout: !llvm.struct<(i64)> where first field is the tag
emitExpr (ECon qn) = do
  let tag = conTag qn
  tagName <- freshName "v"
  structName <- freshName "v"
  resultName <- freshName "v"
  pure ([ "%" <> tagName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
        , "%" <> structName <> " = llvm.mlir.undef : !llvm.struct<(i64)>"
        , "%" <> resultName <> " = llvm.insertvalue %" <> structName <> ", %" <> tagName <> "[0] : !llvm.struct<(i64)>"
        ], resultName)

-- Constructor application: create tagged struct with payload
emitExpr (EApp (ECon qn) args) = do
  let tag = conTag qn
      nFields = length args
      structTy = conStructType nFields
  -- Emit all argument expressions
  argResults <- mapM emitExpr args
  let allOps = concatMap fst argResults
      argNames = map snd argResults
  -- Build the struct: undef, insert tag, insert each field
  tagName <- freshName "v"
  baseName <- freshName "v"
  let tagOps = [ "%" <> tagName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
               , "%" <> baseName <> " = llvm.mlir.undef : " <> structTy
               ]
  -- Insert tag at position 0
  afterTagName <- freshName "v"
  let insertTag = "%" <> afterTagName <> " = llvm.insertvalue %" <> baseName <> ", %" <> tagName <> "[0] : " <> structTy
  -- Insert each field at positions 1..n
  (insertOps, finalName) <- foldInsertFields afterTagName structTy argNames 1
  pure (allOps ++ tagOps ++ [insertTag] ++ insertOps, finalName)

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

-- Integer binary ops (existing)
emitExpr (EApp (EVar fn) [a, b])
  | nameText fn == "+" || nameText fn == "add" = emitBinOp "arith.addi" "i64" a b
  | nameText fn == "-" || nameText fn == "sub" = emitBinOp "arith.subi" "i64" a b
  | nameText fn == "*" || nameText fn == "mul" = emitBinOp "arith.muli" "i64" a b
  | nameText fn == "/" || nameText fn == "div" = emitBinOp "arith.divsi" "i64" a b
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

-- Application of a non-var, non-con expression (e.g. closure call)
emitExpr (EApp fn args) = do
  (fnOps, fnName) <- emitExpr fn
  argResults <- mapM emitExpr args
  let allArgOps = concatMap fst argResults
      _argNames = map snd argResults
  resultName <- freshName "v"
  -- Indirect call through closure — extract function pointer from slot 0
  fptrName <- freshName "v"
  let closureTy = closureStructType (length args)
      extractOps = [ "%" <> fptrName <> " = llvm.extractvalue %" <> fnName <> "[0] : " <> closureTy ]
      callComment = "// indirect call via closure %" <> fnName
  -- For now, fall back to a comment + constant (full indirect calls need llvm.call with function pointers)
  pure (fnOps ++ allArgOps ++ extractOps ++
        [ callComment
        , "%" <> resultName <> " = arith.constant 0 : i64  // TODO: indirect closure call"
        ], resultName)

emitExpr (ECase scrut branches) = do
  -- Pattern matching
  (scrutOps, scrutName) <- emitExpr scrut
  case classifyBranches branches of
    -- Integer literal cases (existing behavior)
    IntLitCase litVal thenExpr elseExpr ->
      emitIntCase scrutOps scrutName litVal thenExpr elseExpr

    -- Multi-way integer literal case
    MultiIntLitCase litBranches defaultExpr ->
      emitMultiIntCase scrutOps scrutName litBranches defaultExpr

    -- Constructor case: extract tag and chain scf.if
    ConCase conBranches mDefaultExpr ->
      emitConCase scrutOps scrutName conBranches mDefaultExpr

    -- PatVar: bind scrutinee to variable, emit body
    VarCase varName body -> do
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

    -- Fallback
    UnhandledCase -> do
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

emitExpr (ELet [] body) = emitExpr body

emitExpr (ELam params body) = do
  -- Lambda lifting with closure support
  let bodyFree = freeVarsExpr body
      paramNames = Set.fromList (map fst params)
      captured = Set.toList (bodyFree `Set.difference` paramNames)
  liftedName <- freshName "lambda"
  if null captured
    then do
      -- No free variables: simple lambda lift (existing behavior)
      let mlirArgs = T.intercalate ", "
            [ "%" <> nameText pn <> ": " <> typeToMlir pt | (pn, pt) <- params ]
          mlirRetTy = "i64"
      (bodyOps, bodyResult) <- emitExpr body
      let fnText = T.unlines $
            [ "  func.func @" <> liftedName <> "(" <> mlirArgs <> ") -> " <> mlirRetTy <> " {" ] ++
            map ("    " <>) bodyOps ++
            [ "    func.return %" <> bodyResult <> " : " <> mlirRetTy
            , "  }" ]
      addLiftedFn fnText
      name <- freshName "v"
      pure (["// lambda lifted as @" <> liftedName,
             "%" <> name <> " = arith.constant 0 : i64"], name)
    else do
      -- Has free variables: emit closure struct
      -- The lifted function takes captured vars as extra leading params
      let capturedParams = [ "%" <> sanitizeName (nameText cn) <> ": i64" | cn <- captured ]
          regularParams = [ "%" <> nameText pn <> ": " <> typeToMlir pt | (pn, pt) <- params ]
          allParams = capturedParams ++ regularParams
          mlirArgs = T.intercalate ", " allParams
          mlirRetTy = "i64"
      (bodyOps, bodyResult) <- emitExpr body
      let fnText = T.unlines $
            [ "  func.func @" <> liftedName <> "(" <> mlirArgs <> ") -> " <> mlirRetTy <> " {" ] ++
            map ("    " <>) bodyOps ++
            [ "    func.return %" <> bodyResult <> " : " <> mlirRetTy
            , "  }" ]
      addLiftedFn fnText
      -- Build closure struct: (fptr_as_i64, captured1, captured2, ...)
      let nCaptured = length captured
          closTy = "!llvm.struct<(i64" <> T.concat (replicate nCaptured ", i64") <> ")>"
      baseName <- freshName "v"
      fptrName <- freshName "v"
      let initOps = [ "// closure for @" <> liftedName <> " capturing " <> T.pack (show nCaptured) <> " vars"
                     , "%" <> fptrName <> " = arith.constant 0 : i64  // function pointer placeholder for @" <> liftedName
                     , "%" <> baseName <> " = llvm.mlir.undef : " <> closTy
                     ]
      -- Insert function pointer at position 0
      afterFptrName <- freshName "v"
      let insertFptr = "%" <> afterFptrName <> " = llvm.insertvalue %" <> baseName <> ", %" <> fptrName <> "[0] : " <> closTy
      -- Insert each captured variable
      let capturedNames = map (sanitizeName . nameText) captured
      (insertOps, finalName) <- foldInsertFields afterFptrName closTy capturedNames 1
      pure (initOps ++ [insertFptr] ++ insertOps, finalName)

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

-- Laziness (thunks) — pass through for now
emitExpr (EDelay e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++ ["// delay (thunk) %" <> eName], eName)

emitExpr (EForce e) = do
  (eOps, eName) <- emitExpr e
  pure (eOps ++ ["// force (thunk) %" <> eName], eName)

-- Type application / abstraction: pass through to inner expr
emitExpr (ETypeApp e _) = emitExpr e
emitExpr (ETypeLam _ e) = emitExpr e

-- Effect operations: emit as function call placeholder
emitExpr (EPerform qn args) = do
  argResults <- mapM emitExpr args
  let allOps = concatMap fst argResults
  name <- freshName "v"
  pure (allOps ++ ["// perform " <> nameText (qnameName qn),
                    "%" <> name <> " = arith.constant 0 : i64  // effect operation stub"], name)

emitExpr (EHandle _ _handler body) = do
  -- For now, just emit the body (ignore handler)
  emitExpr body

-- Catch-all removed: all Expr constructors are handled above

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

emitFloatCmpOp :: Text -> Expr -> Expr -> Emit ([Text], Text)
emitFloatCmpOp pred' a b = do
  (aOps, aName) <- emitExpr a
  (bOps, bName) <- emitExpr b
  resultName <- freshName "v"
  let cmpOp = "%" <> resultName <> " = arith.cmpf " <> pred' <> ", %" <> aName <> ", %" <> bName <> " : f64"
  pure (aOps ++ bOps ++ [cmpOp], resultName)

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

-- | Emit constructor case: extract tag from struct, chain scf.if on tag values
emitConCase :: [Text] -> Text -> [(QName, [Pattern], Expr)] -> Expr -> Emit ([Text], Text)
emitConCase scrutOps scrutName conBranches defaultExpr = do
  -- Extract tag from the scrutinee struct
  -- We use a generic struct type for extraction — the tag is always at position 0
  tagName <- freshName "v"
  let maxFields = maximum (map (\(_, pats, _) -> length pats) conBranches)
      structTy = conStructType maxFields
      extractTag = "%" <> tagName <> " = llvm.extractvalue %" <> scrutName <> "[0] : " <> structTy
  -- Build chain of comparisons
  (chainOps, chainResult) <- emitConChain tagName scrutName structTy conBranches defaultExpr
  pure (scrutOps ++ [extractTag] ++ chainOps, chainResult)

emitConChain :: Text -> Text -> Text -> [(QName, [Pattern], Expr)] -> Expr -> Emit ([Text], Text)
emitConChain _ _ _ [] defaultExpr = emitExpr defaultExpr
emitConChain tagName scrutName structTy [(qn, pats, body)] defaultExpr = do
  -- Last constructor branch: compare tag, if match do body, else default
  let tag = conTag qn
  constName <- freshName "v"
  cmpName <- freshName "cmp"
  let cmpOps = [ "%" <> constName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
               , "%" <> cmpName <> " = arith.cmpi eq, %" <> tagName <> ", %" <> constName <> " : i64"
               ]
  -- Extract fields for pattern variables
  (fieldOps, _) <- emitPatternBindings scrutName structTy pats
  (thenOps, thenResult) <- emitExpr body
  (elseOps, elseResult) <- emitExpr defaultExpr
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
emitConChain tagName scrutName structTy ((qn, pats, body):rest) defaultExpr = do
  let tag = conTag qn
  constName <- freshName "v"
  cmpName <- freshName "cmp"
  let cmpOps = [ "%" <> constName <> " = arith.constant " <> T.pack (show tag) <> " : i64"
               , "%" <> cmpName <> " = arith.cmpi eq, %" <> tagName <> ", %" <> constName <> " : i64"
               ]
  (fieldOps, _) <- emitPatternBindings scrutName structTy pats
  (thenOps, thenResult) <- emitExpr body
  (restOps, restResult) <- emitConChain tagName scrutName structTy rest defaultExpr
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
emitPatField scrutName structTy (idx, PatVar n _) = do
  let varName = sanitizeName (nameText n)
  fieldName <- freshName "v"
  let extractOp = "%" <> fieldName <> " = llvm.extractvalue %" <> scrutName <> "[" <> T.pack (show idx) <> "] : " <> structTy
      aliasOp = "// let " <> varName <> " = %" <> fieldName
  pure ([extractOp, aliasOp], fieldName)
emitPatField _ _ (_, PatWild _) = do
  name <- freshName "v"
  pure (["// wildcard field ignored"], name)
emitPatField scrutName structTy (idx, _) = do
  fieldName <- freshName "v"
  let extractOp = "%" <> fieldName <> " = llvm.extractvalue %" <> scrutName <> "[" <> T.pack (show idx) <> "] : " <> structTy
  pure ([extractOp], fieldName)

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

-------------------------------------------------------------------------------
-- Branch classification
-------------------------------------------------------------------------------

-- | Classify case branches for dispatch
data BranchClass
  = IntLitCase Integer Expr Expr          -- single int literal + default
  | MultiIntLitCase [(Integer, Expr)] Expr -- multiple int literals + default
  | ConCase [(QName, [Pattern], Expr)] Expr -- constructor patterns + default
  | VarCase Name Expr                      -- single variable binding
  | SingleCase Expr                        -- single branch (wildcard or sole)
  | BoolCase Expr Expr                     -- two branches, truthy test
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
  -- Constructor patterns
  | not (null conBranches) =
      let defaultBody = case defaultBranch of
            Just b  -> branchBody b
            Nothing -> branchBody (last conBranches)  -- fallback
          conData = [(qn, pats, branchBody b) | b <- conBranches, PatCon qn pats <- [branchPattern b]]
      in ConCase conData defaultBody
  where
    intLitBranches = [b | b <- branches, isIntLit (branchPattern b)]
    conBranches = [b | b <- branches, isConPat (branchPattern b)]
    defaultBranch = case [b | b <- branches, isDefaultPat (branchPattern b)] of
                      (b:_) -> Just b
                      []    -> Nothing
    allIntLits = all (\b -> isIntLit (branchPattern b) || isDefaultPat (branchPattern b)) branches
                 && not (null intLitBranches)

classifyBranches [Branch (PatVar n _) _ body] = VarCase n body
classifyBranches [Branch _ _ body] = SingleCase body
classifyBranches [Branch _ _ thenExpr, Branch _ _ elseExpr] = BoolCase thenExpr elseExpr
classifyBranches _ = UnhandledCase

isIntLit :: Pattern -> Bool
isIntLit (PatLit (LitInt _)) = True
isIntLit _ = False

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

-- | Deterministic tag for a constructor (hash of qualified name)
conTag :: QName -> Int
conTag qn =
  let t = qnameModule qn <> "." <> nameText (qnameName qn)
  in abs (T.foldl' (\acc c -> acc * 31 + fromEnum c) 0 t) `mod` 65536

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
  let op = "%" <> nextName <> " = llvm.insertvalue %" <> currentName <> ", %" <> fieldName <> "[" <> T.pack (show idx) <> "] : " <> structTy
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
    = "i1"
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
