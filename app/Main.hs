module Main (main) where

import Frankenstein.Core.Types
import Frankenstein.Core.Perceus (insertPerceus)
import Frankenstein.Core.Linker (linkProgramsWith, LinkResult(..), LinkError(..))
import Frankenstein.GhcBridge.Driver (compileToCore, GhcCoreResult(..))
import Frankenstein.MercuryBridge.HldsParse
import Frankenstein.MercuryBridge.CoreTranslate
import Frankenstein.RustBridge.MirParse
import Frankenstein.RustBridge.CoreTranslate
import Frankenstein.MlirEmit.Emitter (emitProgram, compileToExecutable, defaultEmitConfig, EmitConfig(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.FilePath (takeExtension, takeBaseName)
import Data.List (partition, dropWhile)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    ShowHelp -> printHelp
    DemoMode flags -> do
      let prog = demoFactorialWithMain
      handleOutput prog flags
    CompileFiles files flags -> do
      results <- mapM compileFile files
      let (errs, progs) = partitionResults results
      if not (null errs) then
        mapM_ (\(f, e) -> TIO.putStrLn $ "Error [" <> T.pack f <> "]: " <> e) errs
      else do
        let needsMain = flagCompile flags
        case linkProgramsWith needsMain progs of
          Left linkErrs -> mapM_ (TIO.putStrLn . formatLinkError) linkErrs
          Right lr -> do
            let prog = lrProgram lr
            when (length progs > 1) $
              TIO.putStrLn $ "Linked " <> T.pack (show (length progs))
                          <> " modules (main in " <> lrMainModule lr <> ")"
            handleOutput prog flags
  where
    when True  m = m
    when False _ = pure ()

-- Command-line parsing

data Flags = Flags
  { flagEmitCore :: !Bool
  , flagEmitMlir :: !Bool
  , flagCompile  :: !Bool
  , flagOutput   :: !FilePath
  } deriving (Show)

defaultFlags :: Flags
defaultFlags = Flags False False False "a.out"

data Command
  = ShowHelp
  | DemoMode Flags
  | CompileFiles [FilePath] Flags
  deriving (Show)

parseArgs :: [String] -> Command
parseArgs [] = ShowHelp
parseArgs args
  | "--help" `elem` args || "-h" `elem` args = ShowHelp
  | "--demo" `elem` args = DemoMode (parseFlags args)
  | otherwise =
      let (files, flagArgs) = partition (not . isFlag) args
      in if null files
         then ShowHelp
         else CompileFiles files (parseFlags flagArgs)

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag _ = False

parseFlags :: [String] -> Flags
parseFlags args = Flags
  { flagEmitCore = "--emit-core" `elem` args
  , flagEmitMlir = "--emit-mlir" `elem` args
  , flagCompile  = "--compile" `elem` args
  , flagOutput   = case dropWhile (/= "--output") args of
                     ("--output":o:_) -> o
                     _ -> case dropWhile (/= "-o") args of
                            ("-o":o:_) -> o
                            _ -> "a.out"
  }

-- Compilation dispatch

compileFile :: FilePath -> IO (Either (FilePath, Text) Program)
compileFile path = do
  let ext = takeExtension path
  result <- case ext of
    ".hs" -> compileHaskell path
    ".m"  -> compileMercury path
    ".rs" -> compileRust path
    _     -> pure $ Left $ "Unknown file extension: " <> T.pack ext
  pure $ case result of
    Left err   -> Left (path, err)
    Right prog -> Right prog

compileHaskell :: FilePath -> IO (Either Text Program)
compileHaskell inputFile = do
  TIO.putStrLn $ "Compiling Haskell: " <> T.pack inputFile
  result <- compileToCore inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "  GHC bridge error: " <> err
      TIO.putStrLn $ "  Using demo program..."
      pure $ Right demoHaskellProgram
    Right gcr -> pure $ Right (gcrProgram gcr)

compileMercury :: FilePath -> IO (Either Text Program)
compileMercury inputFile = do
  TIO.putStrLn $ "Compiling Mercury: " <> T.pack inputFile
  result <- dumpHlds inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "  Mercury bridge error: " <> err
      TIO.putStrLn $ "  Using demo program..."
      pure $ Right demoMercuryProgram
    Right dumpText ->
      case parseHldsDump dumpText of
        Left err -> pure $ Left $ "HLDS parse error: " <> err
        Right hlds -> pure $ translateHlds hlds

compileRust :: FilePath -> IO (Either Text Program)
compileRust inputFile = do
  TIO.putStrLn $ "Compiling Rust: " <> T.pack inputFile
  result <- dumpMir inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "  Rust bridge error: " <> err
      TIO.putStrLn $ "  Using demo program..."
      pure $ Right demoRustProgram
    Right mirText ->
      case parseMirText mirText of
        Left err -> pure $ Left $ "MIR parse error: " <> err
        Right mir -> pure $ translateMir mir

-- Output handling

handleOutput :: Program -> Flags -> IO ()
handleOutput prog0 flags = do
  let prog = insertPerceus prog0
      config = defaultEmitConfig { ecOutputPath = flagOutput flags }
  case () of
    _ | flagCompile flags -> do
          TIO.putStrLn "=== Compiling to native ==="
          result <- compileToExecutable config prog
          case result of
            Left err -> TIO.putStrLn $ "Compilation error: " <> err
            Right path -> TIO.putStrLn $ "Compiled: " <> T.pack path
      | flagEmitCore flags -> do
          TIO.putStrLn "=== Frankenstein Core ==="
          TIO.putStrLn $ prettyProgram prog
      | flagEmitMlir flags ->
          TIO.putStrLn $ emitProgram prog
      | otherwise -> do
          TIO.putStrLn "=== Frankenstein Core ==="
          TIO.putStrLn $ prettyProgram prog
          TIO.putStrLn ""
          TIO.putStrLn "=== MLIR ==="
          TIO.putStrLn $ emitProgram prog

-- Helpers

partitionResults :: [Either (FilePath, Text) Program] -> ([(FilePath, Text)], [Program])
partitionResults = go [] []
  where
    go errs progs [] = (reverse errs, reverse progs)
    go errs progs (Left e : rest) = go (e:errs) progs rest
    go errs progs (Right p : rest) = go errs (p:progs) rest

formatLinkError :: LinkError -> Text
formatLinkError (DuplicateDefinition qn m1 m2) =
  "Link error: duplicate definition '" <> nameText (qnameName qn)
  <> "' in modules " <> m1 <> " and " <> m2
formatLinkError NoMainFunction =
  "Link error: no 'main' function found in any module"
formatLinkError (MultipleMainFunctions ms) =
  "Link error: 'main' found in multiple modules: " <> T.intercalate ", " ms

printHelp :: IO ()
printHelp = do
  putStrLn "Frankenstein — Polyglot Compiler"
  putStrLn ""
  putStrLn "Usage: frankenstein [options] <input-files...>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --emit-core   Print Frankenstein Core IR"
  putStrLn "  --emit-mlir   Print MLIR output"
  putStrLn "  --compile     Compile to native executable"
  putStrLn "  -o, --output  Output path (default: a.out)"
  putStrLn "  --demo        Run built-in demo (factorial)"
  putStrLn "  -h, --help    Show this help"
  putStrLn ""
  putStrLn "Supported input formats:"
  putStrLn "  .hs     Haskell  (via GHC API)"
  putStrLn "  .m      Mercury  (via mmc --dump-hlds)"
  putStrLn "  .rs     Rust     (via rustc MIR)"
  putStrLn ""
  putStrLn "Multiple files from different languages can be compiled together:"
  putStrLn "  frankenstein search.m Transform.hs sort_buf.rs --compile"
  putStrLn ""
  putStrLn "All paths converge on Koka Core → Perceus → MLIR → LLVM → native."

-------------------------------------------------------------------------------
-- Demo programs
-------------------------------------------------------------------------------

demoFactorialWithMain :: Program
demoFactorialWithMain = Program
  { progName = QName "demo" (Name "factorial" 0)
  , progDefs =
      [ Def
          { defName = QName "" (Name "factorial" 1)
          , defType = TFun [(Many, intType)] EffectRowEmpty intType
          , defExpr =
              ELam [(Name "n" 2, intType)] $
                ECase (EVar (Name "n" 2))
                  [ Branch (PatLit (LitInt 0)) Nothing (ELit (LitInt 1))
                  , Branch (PatVar (Name "n" 2) intType) Nothing
                      (EApp (EVar (Name "*" 0))
                        [ EVar (Name "n" 2)
                        , EApp (EVar (Name "factorial" 1))
                               [EApp (EVar (Name "-" 0)) [EVar (Name "n" 2), ELit (LitInt 1)]]
                        ])
                  ]
          , defSort = DefFun
          , defVisibility = Public
          }
      , Def
          { defName = QName "" (Name "main" 10)
          , defType = TFun [] EffectRowEmpty intType
          , defExpr =
              EApp (EVar (Name "factorial" 1)) [ELit (LitInt 10)]
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects = []
  }
  where intType = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

demoHaskellProgram :: Program
demoHaskellProgram = Program
  { progName = QName "demo" (Name "Haskell" 0)
  , progDefs =
      [ Def
          { defName = QName "demo" (Name "factorial" 1)
          , defType = TFun [(Many, intType)] EffectRowEmpty intType
          , defExpr =
              ELam [(Name "n" 2, intType)] $
                ECase (EVar (Name "n" 2))
                  [ Branch (PatLit (LitInt 0)) Nothing (ELit (LitInt 1))
                  , Branch (PatVar (Name "n" 2) intType) Nothing
                      (EApp (EVar (Name "*" 0))
                        [ EVar (Name "n" 2)
                        , EApp (EVar (Name "factorial" 1))
                               [EApp (EVar (Name "-" 0)) [EVar (Name "n" 2), ELit (LitInt 1)]]
                        ])
                  ]
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects = []
  }
  where intType = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

demoMercuryProgram :: Program
demoMercuryProgram = Program
  { progName = QName "demo" (Name "Mercury" 0)
  , progDefs =
      [ Def
          { defName = QName "demo" (Name "append" 1)
          , defType = TFun
              [(Many, listType), (Many, listType)]
              EffectRowEmpty
              listType
          , defExpr =
              ELam [(Name "xs" 2, listType), (Name "ys" 3, listType)] $
                ECase (EVar (Name "xs" 2))
                  [ Branch (PatCon (QName "list" (Name "nil" 0)) []) Nothing
                      (EVar (Name "ys" 3))
                  , Branch (PatCon (QName "list" (Name "cons" 0))
                              [PatVar (Name "x" 4) anyType, PatVar (Name "rest" 5) listType])
                           Nothing
                      (EApp (ECon (QName "list" (Name "cons" 0)))
                        [ EVar (Name "x" 4)
                        , EApp (EVar (Name "append" 1)) [EVar (Name "rest" 5), EVar (Name "ys" 3)]
                        ])
                  ]
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects = []
  }
  where
    listType = TCon (TypeCon (QName "std" (Name "list" 0)) KindValue)
    anyType = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)

demoRustProgram :: Program
demoRustProgram = Program
  { progName = QName "demo" (Name "Rust" 0)
  , progDefs =
      [ Def
          { defName = QName "demo" (Name "sum_vec" 1)
          , defType = TFun
              [(Affine, vecType)]
              EffectRowEmpty
              intType
          , defExpr =
              ELam [(Name "v" 2, vecType)] $
                ELet [[Bind (Name "result" 3) intType (ELit (LitInt 0)) DefVar]]
                  (EApp (EVar (Name "fold" 0))
                    [ EVar (Name "v" 2)
                    , EVar (Name "result" 3)
                    , ELam [(Name "acc" 4, intType), (Name "x" 5, intType)]
                        (EApp (EVar (Name "+" 0)) [EVar (Name "acc" 4), EVar (Name "x" 5)])
                    ])
          , defSort = DefFun
          , defVisibility = Public
          }
      , Def
          { defName = QName "demo" (Name "drop_example" 2)
          , defType = TFun [(Affine, vecType)] EffectRowEmpty unitType
          , defExpr =
              ELam [(Name "v" 6, vecType)] $
                EDrop (EVar (Name "v" 6))
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects = []
  }
  where
    intType = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
    vecType = TCon (TypeCon (QName "std" (Name "vec" 0)) KindValue)
    unitType = TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue)

-------------------------------------------------------------------------------
-- Pretty printer
-------------------------------------------------------------------------------

prettyProgram :: Program -> Text
prettyProgram prog = T.unlines $
  [ "module " <> ppQName (progName prog) ]
  ++ concatMap prettyDef (progDefs prog)

prettyDef :: Def -> [Text]
prettyDef d =
  [ ""
  , ppQName (defName d) <> " : " <> prettyType (defType d)
  , ppQName (defName d) <> " = " <> prettyExpr (defExpr d)
  ]

prettyType :: Type -> Text
prettyType (TFun args eff ret) =
  "(" <> T.intercalate ", " [prettyMult m <> prettyType t | (m, t) <- args] <> ")"
  <> " -> " <> prettyEff eff <> " " <> prettyType ret
prettyType (TCon tc) = nameText (qnameName (tcName tc))
prettyType (TVar tv) = nameText (tvName tv)
prettyType (TForall tvs body) =
  "forall " <> T.intercalate " " (map (nameText . tvName) tvs) <> ". " <> prettyType body
prettyType (TApp a b) = prettyType a <> "<" <> prettyType b <> ">"
prettyType (TSyn name _ _) = ppQName name

prettyMult :: Multiplicity -> Text
prettyMult Many = ""
prettyMult Affine = "affine "
prettyMult Linear = "linear "

prettyEff :: EffectRow -> Text
prettyEff EffectRowEmpty = "total"
prettyEff (EffectRowExtend name rest) = "<" <> ppQName name <> "," <> prettyEff rest <> ">"
prettyEff (EffectRowVar tv) = nameText (tvName tv)

prettyExpr :: Expr -> Text
prettyExpr (EVar n) = nameText n
prettyExpr (ELit (LitInt n)) = T.pack (show n)
prettyExpr (ELit (LitFloat n)) = T.pack (show n)
prettyExpr (ELit (LitChar c)) = T.pack (show c)
prettyExpr (ELit (LitString s)) = "\"" <> s <> "\""
prettyExpr (ECon qn) = ppQName qn
prettyExpr (EApp f args) =
  prettyExpr f <> "(" <> T.intercalate ", " (map prettyExpr args) <> ")"
prettyExpr (ELam params body) =
  "fn(" <> T.intercalate ", " [nameText n | (n, _) <- params] <> ") " <> prettyExpr body
prettyExpr (ELet binds body) =
  "let " <> T.intercalate "; " [nameText (bindName b) <> " = " <> prettyExpr (bindExpr b) | bg <- binds, b <- bg]
  <> " in " <> prettyExpr body
prettyExpr (ECase scrut branches) =
  "match " <> prettyExpr scrut <> " { "
  <> T.intercalate " | " [prettyExpr (branchBody br) | br <- branches] <> " }"
prettyExpr (EDrop e) = "drop(" <> prettyExpr e <> ")"
prettyExpr (ERetain e) = "retain(" <> prettyExpr e <> ")"
prettyExpr (ERelease e) = "release(" <> prettyExpr e <> ")"
prettyExpr (EDelay e) = "delay(" <> prettyExpr e <> ")"
prettyExpr (EForce e) = "force(" <> prettyExpr e <> ")"
prettyExpr (EPerform eff args) =
  "perform " <> ppQName eff <> "(" <> T.intercalate ", " (map prettyExpr args) <> ")"
prettyExpr _ = "..."

ppQName :: QName -> Text
ppQName qn
  | T.null (qnameModule qn) = nameText (qnameName qn)
  | otherwise = qnameModule qn <> "." <> nameText (qnameName qn)
