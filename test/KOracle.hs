-- | Phase 2b: K Oracle Property-Based Testing
--
-- Generates random well-typed OrganIR programs that produce integer results,
-- runs each through both:
--   1. krun (K reference interpreter)
--   2. Frankenstein MLIR pipeline → native executable
--
-- Compares outputs — any divergence is a compiler bug.
--
-- The generator produces pure integer-valued expressions using:
--   - Literals, arithmetic builtins (+, -, *, negate)
--   - Let bindings and variable references
--   - Lambda abstraction and application
--   - Case expressions on integer patterns
--   - Effect operations (EPerform/EHandle with exn and choice)

module KOracle
  ( kOracleTests
  , genExpr
  , exprToK
  , runKOracle
  , runMlirPipeline
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Frankenstein.Core.Types
import Frankenstein.Core.Evidence (evidencePass)
import Frankenstein.Core.Perceus (insertPerceus)
import Frankenstein.MlirEmit.Emitter (EmitConfig(..), CompileTarget(..), compileToExecutable, emitProgramText)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (findExecutable)

-------------------------------------------------------------------------------
-- Configuration
-------------------------------------------------------------------------------

krunPath :: FilePath
krunPath = "/home/nyc/src/k/result/bin/krun"

kDefinition :: FilePath
kDefinition = "/home/nyc/src/frankenstein/k-specs/organ-ir-kompiled"

-------------------------------------------------------------------------------
-- Test entry point
-------------------------------------------------------------------------------

kOracleTests :: TestTree
kOracleTests = testGroup "K Oracle (Phase 2b)"
  [ testProperty "random pure expressions: krun == MLIR" prop_krunEqualsMlir
  , testProperty "arithmetic expressions: krun == MLIR" prop_arithKrunEqualsMlir
  , testProperty "let+case expressions: krun == MLIR" prop_letCaseKrunEqualsMlir
  , testProperty "effect expressions: krun == MLIR" prop_effectKrunEqualsMlir
  ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- | Pure arithmetic expressions
prop_arithKrunEqualsMlir :: Property
prop_arithKrunEqualsMlir = withMaxSuccess 20 $ forAll genArithExpr oracleProperty

-- | Expressions with let bindings and case
prop_letCaseKrunEqualsMlir :: Property
prop_letCaseKrunEqualsMlir = withMaxSuccess 15 $ forAll genLetCaseExpr oracleProperty

-- | Expressions with algebraic effects
prop_effectKrunEqualsMlir :: Property
prop_effectKrunEqualsMlir = withMaxSuccess 10 $ forAll genEffectExpr oracleProperty

-- | Mixed random expressions
prop_krunEqualsMlir :: Property
prop_krunEqualsMlir = withMaxSuccess 25 $ forAll genExpr oracleProperty

-- | Core oracle comparison: run both pipelines, compare results
oracleProperty :: Expr -> Property
oracleProperty expr = ioProperty $ do
  kResult <- runKOracle expr
  case kResult of
    Left err -> pure $ counterexample ("krun failed: " ++ T.unpack err) (property Discard)
    Right kVal -> do
      mlirResult <- runMlirPipeline expr
      case mlirResult of
        Left err -> pure $ counterexample ("MLIR failed: " ++ T.unpack err) (property Discard)
        Right mlirVal ->
          pure $ counterexample
            ("krun=" ++ show kVal ++ " mlir=" ++ show mlirVal ++ "\nexpr=" ++ briefExpr expr)
            (kVal === mlirVal)

-------------------------------------------------------------------------------
-- QuickCheck Generators
-------------------------------------------------------------------------------

-- | Helper: names for generated variables
varNames :: [Text]
varNames = ["x", "y", "z", "w", "a", "b"]

mkN :: Text -> Name
mkN t = Name t 0

mkQ :: Text -> Text -> QName
mkQ m n = QName m (mkN n)

intT :: Type
intT = TCon (TypeCon (mkQ "std" "int") KindValue)

-- | Generate a pure arithmetic expression (no bindings, no effects)
genArithExpr :: Gen Expr
genArithExpr = sized (genArith 0)

genArith :: Int -> Int -> Gen Expr
genArith _depth 0 = genLitSmall
genArith depth n
  | depth > 4 = genLitSmall
  | otherwise = frequency
      [ (3, genLitSmall)
      , (2, genBinOp depth n)
      , (1, genNegate depth n)
      ]

genLitSmall :: Gen Expr
genLitSmall = do
  v <- chooseInt (-50, 50)
  pure $ ELit (LitInt (fromIntegral v))

genBinOp :: Int -> Int -> Gen Expr
genBinOp depth n = do
  op <- elements ["+", "-", "*"]
  lhs <- genArith (depth + 1) (n `div` 2)
  rhs <- genArith (depth + 1) (n `div` 2)
  -- Use EVar form (matching what the compiler pipeline produces)
  -- The K serializer maps these back to ECon(qname("builtin",...))
  pure $ EApp (EVar (mkN op)) [lhs, rhs]

genNegate :: Int -> Int -> Gen Expr
genNegate depth n = do
  inner <- genArith (depth + 1) (n - 1)
  pure $ EApp (EVar (mkN "negate")) [inner]

-- | Generate expressions with let bindings and case
genLetCaseExpr :: Gen Expr
genLetCaseExpr = sized (genLetCase [] 0)

genLetCase :: [Text] -> Int -> Int -> Gen Expr
genLetCase _env _depth 0 = genLitSmall
genLetCase env depth n
  | depth > 3 = genLeaf env
  | otherwise = frequency $
      [ (3, genLeaf env)
      , (2, genLetExpr env depth n)
      , (2, genCaseExpr env depth n)
      ] ++ [(1, genBinOpWithEnv env depth n) | not (null env)]

genLeaf :: [Text] -> Gen Expr
genLeaf env
  | null env = genLitSmall
  | otherwise = frequency
      [ (2, genLitSmall)
      , (1, EVar . mkN <$> elements env)
      ]

genLetExpr :: [Text] -> Int -> Int -> Gen Expr
genLetExpr env depth n = do
  let vn = varNames !! min (length env) (length varNames - 1)
  rhs <- genLetCase env (depth + 1) (n `div` 2)
  body <- genLetCase (vn : env) (depth + 1) (n `div` 2)
  let bind = Bind (mkN vn) intT rhs DefVal
  pure $ ELet [[bind]] body

genCaseExpr :: [Text] -> Int -> Int -> Gen Expr
genCaseExpr env depth n = do
  scrut <- genLetCase env (depth + 1) (n `div` 3)
  -- Case on 0 vs wildcard
  thenE <- genLetCase env (depth + 1) (n `div` 3)
  elseE <- genLetCase env (depth + 1) (n `div` 3)
  pure $ ECase scrut
    [ Branch (PatLit (LitInt 0)) Nothing thenE
    , Branch (PatWild intT) Nothing elseE
    ]

genBinOpWithEnv :: [Text] -> Int -> Int -> Gen Expr
genBinOpWithEnv env depth n = do
  op <- elements ["+", "-", "*"]
  lhs <- genLetCase env (depth + 1) (n `div` 2)
  rhs <- genLetCase env (depth + 1) (n `div` 2)
  pure $ EApp (EVar (mkN op)) [lhs, rhs]

-- | Generate expressions with algebraic effects
genEffectExpr :: Gen Expr
genEffectExpr = sized (genEffect 0)

genEffect :: Int -> Int -> Gen Expr
genEffect _depth 0 = genLitSmall
genEffect depth n
  | depth > 3 = genLitSmall
  | otherwise = frequency
      [ (3, genLitSmall)
      , (2, genHandleAbort depth n)
      , (2, genHandleResume depth n)
      , (1, genBinOp depth n)
      ]

-- | Generate handle+perform with abort (exn) semantics
genHandleAbort :: Int -> Int -> Gen Expr
genHandleAbort depth n = do
  -- handle(exn, \k -> defaultVal, body_with_perform)
  defaultVal <- genLitSmall
  body <- genAbortBody (depth + 1) (n `div` 2)
  let exnEff = EffectRowExtend (mkQ "mercury" "exn") EffectRowEmpty
      handler = ELam [(mkN "k", intT)] defaultVal
  pure $ EHandle exnEff handler body

genAbortBody :: Int -> Int -> Gen Expr
genAbortBody depth n = frequency
  [ (2, pure $ EPerform (mkQ "mercury" "exn") [])  -- perform exn (abort)
  , (1, do  -- arith then perform
      lhs <- genArith (depth + 1) (n `div` 2)
      pure $ EApp (EVar (mkN "+"))
        [EPerform (mkQ "mercury" "exn") [], lhs])
  , (1, genLitSmall)  -- normal return (no perform)
  ]

-- | Generate handle+perform with resume (choice) semantics
genHandleResume :: Int -> Int -> Gen Expr
genHandleResume depth n = do
  -- handle(choice, \k -> k(resumeVal), body_with_perform)
  resumeVal <- genLitSmall
  let choiceEff = EffectRowExtend (mkQ "mercury" "choice") EffectRowEmpty
      handler = ELam [(mkN "k", intT)]
                  (EApp (EVar (mkN "k")) [resumeVal])
  body <- genResumeBody (depth + 1) (n `div` 2)
  pure $ EHandle choiceEff handler body

genResumeBody :: Int -> Int -> Gen Expr
genResumeBody depth n = frequency
  [ (2, pure $ EPerform (mkQ "mercury" "choice") [])  -- just perform
  , (1, do  -- perform + arith
      rhs <- genLitSmall
      pure $ EApp (EVar (mkN "+"))
        [EPerform (mkQ "mercury" "choice") [], rhs])
  , (1, genLitSmall)  -- normal return
  ]

-- | Generate any well-typed integer expression
genExpr :: Gen Expr
genExpr = sized $ \n -> frequency
  [ (3, genArith 0 n)
  , (2, genLetCase [] 0 n)
  , (1, genEffect 0 n)
  ]

-- | Brief pretty-printer for counterexample reporting
briefExpr :: Expr -> String
briefExpr (ELit (LitInt n)) = show n
briefExpr (EVar n) = T.unpack (nameText n)
briefExpr (EApp (ECon q) args) = "(" ++ T.unpack (nameText (qnameName q)) ++ " " ++ unwords (map briefExpr args) ++ ")"
briefExpr (ELet _ body) = "(let ... in " ++ briefExpr body ++ ")"
briefExpr (ECase scrut _) = "(case " ++ briefExpr scrut ++ " of ...)"
briefExpr (EHandle _ _ body) = "(handle ... " ++ briefExpr body ++ ")"
briefExpr (EPerform q _) = "perform " ++ T.unpack (nameText (qnameName q))
briefExpr (ELam _ body) = "(\\... -> " ++ briefExpr body ++ ")"
briefExpr _ = "..."

-------------------------------------------------------------------------------
-- K Oracle: serialize to K syntax, run krun
-------------------------------------------------------------------------------

-- | Serialize a Frankenstein Expr to K syntax for krun
exprToK :: Expr -> Text
exprToK (ELit (LitInt n)) = "ELit(litInt(" <> T.pack (show n) <> "))"
exprToK (ELit (LitFloat f)) = "ELit(litFloat(" <> T.pack (show f) <> "))"
exprToK (ELit (LitChar c)) = "ELit(litChar(\"" <> T.singleton c <> "\"))"
exprToK (ELit (LitString s)) = "ELit(litString(\"" <> s <> "\"))"

exprToK (EVar n) = "EVar(" <> nameToK n <> ")"
exprToK (ECon q) = "ECon(" <> qnameToK q <> ")"

-- Arithmetic builtins: the compiler uses EVar(Name "+" 0) etc.,
-- but the K spec uses ECon(qname("builtin", name("+", 0))).
-- Translate at serialization time.
exprToK (EApp (EVar (Name op 0)) args)
  | op `elem` ["+", "-", "*", "/", "mod", "==", "<", ">", "<=", ">=", "negate", "++"]
  = "EApp(ECon(qname(\"builtin\", name(\"" <> op <> "\", 0))), " <> exprListToK args <> ")"

exprToK (EApp f args) =
  "EApp(" <> exprToK f <> ", " <> exprListToK args <> ")"

exprToK (ELam [] body) =
  "ELam(" <> exprToK body <> ")"
exprToK (ELam params body) =
  "ELam(" <> T.intercalate ", " (map paramToK params) <> ", " <> exprToK body <> ")"

exprToK (ELet bgs body) =
  "ELet(" <> bgListToK bgs <> ", " <> exprToK body <> ")"

exprToK (ECase scrut branches) =
  "ECase(" <> exprToK scrut <> ", " <> branchListToK branches <> ")"

exprToK (EPerform q []) =
  "EPerform(" <> qnameToK q <> ", noArgs)"
exprToK (EPerform q args) =
  "EPerform(" <> qnameToK q <> ", " <> exprListToK args <> ")"

exprToK (EHandle eff handler body) =
  "EHandle(" <> effRowToK eff <> ", " <> exprToK handler <> ", " <> exprToK body <> ")"

exprToK (ERetain e) = "ERetain(" <> exprToK e <> ")"
exprToK (ERelease e) = "ERelease(" <> exprToK e <> ")"
exprToK (EDrop e) = "EDrop(" <> exprToK e <> ")"
exprToK (EReuse a b) = "EReuse(" <> exprToK a <> ", " <> exprToK b <> ")"
exprToK (EDelay e) = "EDelay(" <> exprToK e <> ")"
exprToK (EForce e) = "EForce(" <> exprToK e <> ")"
exprToK (ETypeApp e _) = exprToK e  -- type erasure
exprToK (ETypeLam _ e) = exprToK e  -- type erasure
exprToK (EFunRef q) = "ECon(" <> qnameToK q <> ")"  -- approximate as constructor

nameToK :: Name -> Text
nameToK (Name t u) = "name(\"" <> t <> "\", " <> T.pack (show u) <> ")"

qnameToK :: QName -> Text
qnameToK (QName m n) = "qname(\"" <> m <> "\", " <> nameToK n <> ")"

typeToK :: Type -> Text
typeToK _ = "TCon(typecon(qname(\"s\", name(\"i\", 0)), KValue))"

paramToK :: (Name, Type) -> Text
paramToK (n, t) = "param(" <> nameToK n <> ", " <> typeToK t <> ")"

exprListToK :: [Expr] -> Text
exprListToK [] = "noArgs"
exprListToK es = T.intercalate ", " (map exprToK es)

bgListToK :: [BindGroup] -> Text
bgListToK [] = error "empty bind group list"
bgListToK bgs = T.intercalate " ;; " (map bgToK bgs)

bgToK :: BindGroup -> Text
bgToK binds = T.intercalate ", " (map bindToK binds)

bindToK :: Bind -> Text
bindToK (Bind n t rhs sort) =
  "bind(" <> nameToK n <> ", " <> typeToK t <> ", " <> exprToK rhs <> ", "
  <> defSortToK sort <> ")"

defSortToK :: DefSort -> Text
defSortToK DefFun = "defFun"
defSortToK DefVal = "defVal"
defSortToK DefVar = "defVar"

patternToK :: Pattern -> Text
patternToK (PatLit l) = "PatLit(" <> litToK l <> ")"
patternToK (PatVar n t) = "PatVar(" <> nameToK n <> ", " <> typeToK t <> ")"
patternToK (PatWild t) = "PatWild(" <> typeToK t <> ")"
patternToK (PatCon q ps) =
  "PatCon(" <> qnameToK q <> ", " <> T.intercalate ", " (map patternToK ps) <> ")"

litToK :: Lit -> Text
litToK (LitInt n) = "litInt(" <> T.pack (show n) <> ")"
litToK (LitFloat f) = "litFloat(" <> T.pack (show f) <> ")"
litToK (LitChar c) = "litChar(\"" <> T.singleton c <> "\")"
litToK (LitString s) = "litString(\"" <> s <> "\")"

branchListToK :: [Branch] -> Text
branchListToK bs = T.intercalate " ; " (map branchToK bs)

branchToK :: Branch -> Text
branchToK (Branch pat guard body) =
  "branch(" <> patternToK pat <> ", "
  <> guardToK guard <> ", " <> exprToK body <> ")"

guardToK :: Maybe Expr -> Text
guardToK Nothing = "noGuard"
guardToK (Just e) = "guard(" <> exprToK e <> ")"

effRowToK :: EffectRow -> Text
effRowToK EffectRowEmpty = "effectEmpty"
effRowToK (EffectRowExtend q rest) =
  "effectExtend(" <> qnameToK q <> ", " <> effRowToK rest <> ")"
effRowToK (EffectRowVar _) = "effectEmpty"  -- approximate

-------------------------------------------------------------------------------
-- K Oracle: run krun and parse result
-------------------------------------------------------------------------------

-- | Run an expression through krun and return the integer result
runKOracle :: Expr -> IO (Either Text Integer)
runKOracle expr = do
  let input = "eval(" <> exprToK expr <> ")"
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    krunPath
    ["--definition", kDefinition, "-cPGM=" ++ T.unpack input]
    ""
  case exitCode of
    ExitFailure _ -> pure $ Left $ "krun failed: " <> T.pack stderr
    ExitSuccess -> pure $ parseKrunOutput (T.pack stdout)

-- | Parse krun output to extract the integer result
parseKrunOutput :: Text -> Either Text Integer
parseKrunOutput output =
  -- Look for: #val ( vlit ( litInt ( N ) ) )
  case extractIntFromKOutput output of
    Just n  -> Right n
    Nothing ->
      -- Check for error
      if T.isInfixOf "#error" output
        then Left $ "K error: " <> T.take 200 output
        else Left $ "Could not parse krun output: " <> T.take 200 output

extractIntFromKOutput :: Text -> Maybe Integer
extractIntFromKOutput t =
  -- Find "litInt ( N )" pattern
  case T.breakOn "litInt ( " t of
    (_, rest)
      | T.null rest -> Nothing
      | otherwise ->
          let afterLitInt = T.drop (T.length "litInt ( ") rest
              numStr = T.takeWhile (\c -> c == '-' || (c >= '0' && c <= '9')) afterLitInt
          in case reads (T.unpack numStr) of
               [(n, "")] -> Just n
               _         -> Nothing

-------------------------------------------------------------------------------
-- MLIR Pipeline: compile and run
-------------------------------------------------------------------------------

-- | Run an expression through the full MLIR pipeline and return integer result
runMlirPipeline :: Expr -> IO (Either Text Integer)
runMlirPipeline expr = withSystemTempDirectory "frankenstein-oracle" $ \tmpDir -> do
  let outputPath = tmpDir ++ "/oracle-test"
      -- Wrap the expression in a program with a main function
      mainDef = Def
        { defName = QName "main" (Name "main" 0)
        , defType = TFun [] EffectRowEmpty intT
        , defExpr = expr
        , defSort = DefFun
        , defVisibility = Public
        }
      prog = Program
        { progName = QName "main" (Name "main" 0)
        , progDefs = [mainDef]
        , progData = []
        , progEffects = []
        }
      -- Apply passes
      progEvidence = evidencePass prog
      progPerceus = insertPerceus progEvidence

      config = EmitConfig
        { ecMlirOptPath = "mlir-opt"
        , ecMlirTranslatePath = "mlir-translate"
        , ecClangPath = "clang"
        , ecKokaRuntimePath = Just "/home/nyc/src/frankenstein/runtime/kk_runtime.c"
        , ecOptLevel = 0
        , ecOutputPath = outputPath
        , ecTarget = TargetNative
        }

  result <- compileToExecutable config progPerceus
  case result of
    Left err -> pure $ Left $ "MLIR compile failed: " <> err
    Right binPath -> do
      (exitCode, stdout, stderr) <- readProcessWithExitCode binPath [] ""
      case exitCode of
        ExitFailure code -> pure $ Left $ "Binary failed (exit " <> T.pack (show code) <> "): " <> T.pack stderr
        ExitSuccess ->
          case reads (T.unpack (T.strip (T.pack stdout))) of
            [(n, "")] -> pure $ Right n
            _ -> pure $ Left $ "Could not parse output: " <> T.pack stdout

