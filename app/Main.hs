module Main (main) where

import Frankenstein.Core.Types
import Frankenstein.GhcBridge.Driver
import Frankenstein.MercuryBridge.HldsParse
import Frankenstein.MercuryBridge.CoreTranslate
import Frankenstein.RustBridge.MirParse
import Frankenstein.RustBridge.CoreTranslate
import Frankenstein.MlirEmit.Emitter
import Frankenstein.MlirEmit.Dialects

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.FilePath (takeExtension)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      putStrLn "Frankenstein — Polyglot Compiler"
      putStrLn ""
      putStrLn "Usage: frankenstein <input-file> [--emit-mlir | --emit-core]"
      putStrLn ""
      putStrLn "Supported input formats:"
      putStrLn "  .hs     Haskell  (via GHC API)"
      putStrLn "  .m      Mercury  (via mmc --dump-hlds)"
      putStrLn "  .rs     Rust     (via rustc MIR)"
      putStrLn ""
      putStrLn "All paths converge on Koka Core → MLIR → LLVM → native."

    (inputFile:flags) -> do
      let ext = takeExtension inputFile
          emitMlir = "--emit-mlir" `elem` flags
          emitCore = "--emit-core" `elem` flags

      result <- case ext of
        ".hs" -> compileHaskell inputFile
        ".m"  -> compileMercury inputFile
        ".rs" -> compileRust inputFile
        _     -> pure $ Left $ "Unknown file extension: " <> T.pack ext

      case result of
        Left err -> TIO.putStrLn $ "Error: " <> err
        Right prog -> do
          if emitCore then do
            TIO.putStrLn $ "=== Frankenstein Core ==="
            TIO.putStrLn $ prettyProgram prog
          else if emitMlir then do
            let mlirMod = emitProgram prog
            TIO.putStrLn $ renderModule mlirMod
          else do
            TIO.putStrLn $ "=== Frankenstein Core ==="
            TIO.putStrLn $ prettyProgram prog
            TIO.putStrLn $ ""
            TIO.putStrLn $ "=== MLIR ==="
            let mlirMod = emitProgram prog
            TIO.putStrLn $ renderModule mlirMod

compileHaskell :: FilePath -> IO (Either Text Program)
compileHaskell inputFile = do
  TIO.putStrLn $ "Compiling Haskell: " <> T.pack inputFile
  TIO.putStrLn $ "(GHC bridge — subprocess mode)"
  -- For now: demonstrate with a hardcoded simple program
  -- TODO: invoke GHC API
  result <- compileToCore inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "GHC bridge not yet active: " <> err
      TIO.putStrLn $ "Using demo program..."
      pure $ Right demoHaskellProgram
    Right _ -> pure $ Left "GHC Core translation not yet connected"

compileMercury :: FilePath -> IO (Either Text Program)
compileMercury inputFile = do
  TIO.putStrLn $ "Compiling Mercury: " <> T.pack inputFile
  TIO.putStrLn $ "(mmc --dump-hlds mode)"
  result <- dumpHlds inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "Mercury bridge error: " <> err
      TIO.putStrLn $ "Using demo program..."
      pure $ Right demoMercuryProgram
    Right dumpText -> do
      case parseHldsDump dumpText of
        Left err -> pure $ Left $ "HLDS parse error: " <> err
        Right hlds -> pure $ translateHlds hlds

compileRust :: FilePath -> IO (Either Text Program)
compileRust inputFile = do
  TIO.putStrLn $ "Compiling Rust: " <> T.pack inputFile
  TIO.putStrLn $ "(rustc MIR dump mode)"
  result <- dumpMir inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "Rust bridge error: " <> err
      TIO.putStrLn $ "Using demo program..."
      pure $ Right demoRustProgram
    Right mirText ->
      case parseMirText mirText of
        Left err -> pure $ Left $ "MIR parse error: " <> err
        Right mir -> pure $ translateMir mir

-- Demo programs for testing the pipeline before bridges are connected

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
              EffectRowEmpty  -- det: pure
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
              [(Affine, vecType)]  -- Vec<i64> consumed (ownership transfer)
              EffectRowEmpty
              intType
          , defExpr =
              ELam [(Name "v" 2, vecType)] $
                -- Simplified: just show the ownership pattern
                ELet [[Bind (Name "result" 3) intType (ELit (LitInt 0)) DefVar]]
                  (EApp (EVar (Name "fold" 0))
                    [ EVar (Name "v" 2)     -- moved into fold
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
                EDrop (EVar (Name "v" 6))  -- explicit drop (Rust: drop(v))
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

-- Simple pretty printer for Core programs
prettyProgram :: Program -> Text
prettyProgram prog = T.unlines $
  [ "module " <> qnameToText (progName prog) ]
  ++ concatMap prettyDef (progDefs prog)

prettyDef :: Def -> [Text]
prettyDef d =
  [ ""
  , qnameToText (defName d) <> " : " <> prettyType (defType d)
  , qnameToText (defName d) <> " = " <> prettyExpr 1 (defExpr d)
  ]

prettyType :: Type -> Text
prettyType (TFun args eff ret) =
  "(" <> T.intercalate ", " [prettyMult m <> prettyType t | (m, t) <- args] <> ")"
  <> " -> " <> prettyEff eff <> " " <> prettyType ret
prettyType (TCon tc) = nameText (qnameName (tcName tc))
prettyType (TVar tv) = nameText (tvName tv)
prettyType (TForall tvs body) = "forall " <> T.intercalate " " (map (nameText . tvName) tvs) <> ". " <> prettyType body
prettyType (TApp a b) = prettyType a <> "<" <> prettyType b <> ">"
prettyType (TSyn name _ _) = qnameToText name

prettyMult :: Multiplicity -> Text
prettyMult Many = ""
prettyMult Affine = "affine "
prettyMult Linear = "linear "

prettyEff :: EffectRow -> Text
prettyEff EffectRowEmpty = "total"
prettyEff (EffectRowExtend name rest) = "<" <> qnameToText name <> "," <> prettyEff rest <> ">"
prettyEff (EffectRowVar tv) = nameText (tvName tv)

prettyExpr :: Int -> Expr -> Text
prettyExpr _ (EVar n) = nameText n
prettyExpr _ (ELit (LitInt n)) = T.pack (show n)
prettyExpr _ (ELit (LitFloat n)) = T.pack (show n)
prettyExpr _ (ELit (LitChar c)) = T.pack (show c)
prettyExpr _ (ELit (LitString s)) = "\"" <> s <> "\""
prettyExpr _ (ECon qn) = qnameToText qn
prettyExpr d (EApp f args) = prettyExpr d f <> "(" <> T.intercalate ", " (map (prettyExpr d) args) <> ")"
prettyExpr d (ELam params body) =
  "fn(" <> T.intercalate ", " [nameText n | (n, _) <- params] <> ") " <> prettyExpr d body
prettyExpr d (ELet binds body) =
  "let " <> T.intercalate "; " [nameText (bindName b) <> " = " <> prettyExpr d (bindExpr b) | bg <- binds, b <- bg]
  <> " in " <> prettyExpr d body
prettyExpr d (ECase scrut branches) =
  "match " <> prettyExpr d scrut <> " { " <>
  T.intercalate " | " [prettyExpr d (branchBody br) | br <- branches] <> " }"
prettyExpr d (EDrop e) = "drop(" <> prettyExpr d e <> ")"
prettyExpr d (ERetain e) = "retain(" <> prettyExpr d e <> ")"
prettyExpr d (ERelease e) = "release(" <> prettyExpr d e <> ")"
prettyExpr d (EDelay e) = "delay(" <> prettyExpr d e <> ")"
prettyExpr d (EForce e) = "force(" <> prettyExpr d e <> ")"
prettyExpr d (EPerform eff args) = "perform " <> qnameToText eff <> "(" <> T.intercalate ", " (map (prettyExpr d) args) <> ")"
prettyExpr _ _ = "..."

qnameToText :: QName -> Text
qnameToText qn
  | T.null (qnameModule qn) = nameText (qnameName qn)
  | otherwise = qnameModule qn <> "." <> nameText (qnameName qn)
