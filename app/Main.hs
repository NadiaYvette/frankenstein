module Main (main) where

import Frankenstein.Core.Types
import Frankenstein.Core.Perceus (insertPerceus)
import Frankenstein.Core.CycleAnalysis (analyzeCycles, CycleInfo(..))
import Frankenstein.Core.Evidence (evidencePassGlobal, collectGlobalEffects)
import qualified Frankenstein.Core.EvidenceEvv as EvidenceEvv
import Frankenstein.Core.EffectOpt (effectOptimize, effectOptimizeWithStats, EffectOptStats(..))
import Frankenstein.Core.DeriveSelectors (deriveSelectors)
import Frankenstein.Core.FlattenPatterns (flattenPatterns)
import Frankenstein.Core.NormalizePatterns (normalizePatterns)
import Frankenstein.Debug.DumpProgram (dumpProgram)
import Frankenstein.Core.Linker (linkProgramsWith, LinkResult(..), LinkError(..))
import Frankenstein.GhcBridge.Driver (compileToCore, compileToCoreWith, compileToCoreMulti, GhcCoreResult(..))
import Frankenstein.MercuryBridge.HldsParse
import Frankenstein.MercuryBridge.CoreTranslate
import Frankenstein.RustBridge.MirParse
import Frankenstein.RustBridge.CoreTranslate
import Frankenstein.KokaBridge.Driver (compileKokaFile)
import Frankenstein.PythonBridge.AstParse (parsePython)
import Frankenstein.PythonBridge.CoreTranslate (translatePythonAst)
import Frankenstein.GoBridge.AstParse (parseGo)
import Frankenstein.GoBridge.CoreTranslate (translateGoAst)
import Frankenstein.FutharkBridge.Parser (parseFutharkFile)
import Frankenstein.FutharkBridge.CoreTranslate (translateFuthark)
import Frankenstein.SchemeBridge.Reader (readSchemeFile)
import Frankenstein.SchemeBridge.CoreTranslate (translateScheme)
import Frankenstein.SwiftBridge.Driver (readSwiftFile, emitSilCounts, SilCounts(..))
import Frankenstein.SwiftBridge.CoreTranslate (translateSwift)
import Frankenstein.OCamlBridge.Driver (readOCamlFile)
import Frankenstein.OCamlBridge.CoreTranslate (translateOCaml)
import Frankenstein.ErlangBridge.Driver (readErlangFile)
import Frankenstein.ErlangBridge.CoreTranslate (translateErlang)
import Frankenstein.FSharpBridge.Driver (readFSharpFile)
import Frankenstein.FSharpBridge.CoreTranslate (translateFSharp)
import Frankenstein.IdrisBridge.Driver (readIdrisFile)
import Frankenstein.IdrisBridge.CoreTranslate (translateIdris)
import Frankenstein.MlirEmit.Emitter (emitProgram, emitProgramWasm, emitProgramWithEffects, compileToExecutable, compileToWasm, defaultEmitConfig, EmitConfig(..), CompileTarget(..))
import qualified Frankenstein.MlirEmit.PostProcess as PostProcess
import qualified Frankenstein.Core.TypeCheck as TC
import Frankenstein.OrganIR.Consumer (consumeProgram)
import Frankenstein.OrganIR.Emitter qualified as OrganEmit

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, lookupEnv)
import System.FilePath (takeExtension, takeBaseName)
import System.IO (hPutStrLn, stderr)
import Data.List (partition, dropWhile)

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    ShowHelp -> printHelp
    DemoMode flags -> do
      let prog = demoFactorialWithMain
      handleOutput prog flags
    DemoEffect flags -> do
      let prog = demoEffectProgram
      handleOutput prog flags
    DemoAbort flags -> do
      let prog = demoAbortProgram
      handleOutput prog flags
    DemoMultiEffect flags -> do
      let prog = demoMultiEffectProgram
      handleOutput prog flags
    SwiftCrossCheck files -> mapM_ swiftCrossCheck files
    PostProcessMlir files -> mapM_ PostProcess.postProcessFile files
    TypeCheck files -> do
      -- Compile each file to Core, then type-check.
      results <- mapM (compileFile False True) files
      let (errs, rawProgsList) = partitionResults results
      mapM_ (\(p, e) -> TIO.putStrLn (T.pack p <> ": " <> e)) errs
      let prog = case concat rawProgsList of
            []  -> Program (QName "" (Name "" 0)) [] [] []
            ps  -> foldr1 mergeProgs ps
          mergeProgs a b = a
            { progDefs    = progDefs a ++ progDefs b
            , progData    = progData a ++ progData b
            , progEffects = progEffects a ++ progEffects b
            }
      let tyErrs = TC.typeCheckProgram prog
      if null tyErrs
        then TIO.putStrLn "OK: well-typed."
        else do
          mapM_ (TIO.putStrLn . ("ERROR: " <>) . TC.prettyError) tyErrs
          -- Don't exit non-zero — caller decides; this is a reporting tool.
          pure ()
    CompileFiles files flags -> do
      results <- mapM (compileFile (flagFromJson flags) (not (flagNoSimplify flags))) files
      -- Run evidence pass with a GLOBAL effect registry (from all modules)
      -- BEFORE the linker mangles names.  This enables cross-module effect
      -- dispatch: Module A can perform an effect handled by Module B.
      let (errs, rawProgsList) = partitionResults results
          rawProgs = concat rawProgsList
          -- Auto-derive record field selectors before the linker so they
          -- count as defined symbols and the linker doesn't warn about them.
          derived = map deriveSelectors rawProgs
          -- Build a global effect registry from ALL modules' declarations,
          -- enabling cross-module effect dispatch (Module A performs, Module B handles).
          allEffectDecls = concatMap progEffects derived
          globalEffects = collectGlobalEffects
            (Program (QName "" (Name "" 0)) [] [] allEffectDecls)
          -- Run evidence pass on each module using the global registry.
          -- --evidence=plotkin routes through the Plotkin-style pass.
          -- Plotkin mode needs a global top-name set so cross-module
          -- callees receive the evv parameter; runtime symbols (kk_*,
          -- foreign import ccall targets) are not in topNames and
          -- pass through unchanged.
          plotkinTopNames = EvidenceEvv.collectTopNames derived
          runEvidence = case flagEvidenceMode flags of
            EvidencePlotkin -> EvidenceEvv.evidencePassEvvGlobal plotkinTopNames
            EvidenceInline  -> evidencePassGlobal globalEffects
          progs = if flagEmitEffectMlir flags
                  then derived  -- keep raw EHandle/EPerform for dialect output
                  else map runEvidence derived
      when (flagEvidenceMode flags == EvidencePlotkin) $
        hPutStrLn stderr "[Frankenstein] evidence-mode=plotkin"
      if not (null errs) then
        mapM_ (\(f, e) -> TIO.putStrLn $ "Error [" <> T.pack f <> "]: " <> e) errs
      else do
        let needsMain = flagCompile flags
        case linkProgramsWith needsMain progs of
          Left linkErrs -> mapM_ (TIO.putStrLn . formatLinkError) linkErrs
          Right lr -> do
            let prog = lrProgram lr
            when (length progs > 1) $
              hPutStrLn stderr $ "Linked " <> show (length progs)
                          <> " modules (main in " <> T.unpack (lrMainModule lr) <> ")"
            -- Print linker validation warnings to stderr so they don't
            -- pollute MLIR output piped to mlir-opt.
            when (not (null (lrWarnings lr))) $ do
              mapM_ (hPutStrLn stderr . T.unpack) (lrWarnings lr)
            handleOutput prog flags
  where
    when True  m = m
    when False _ = pure ()

-- Command-line parsing

data Flags = Flags
  { flagEmitCore :: !Bool
  , flagEmitMlir :: !Bool
  , flagEmitEffectMlir :: !Bool
  , flagEmitOrgan :: !Bool
  , flagEmitOrganPost :: !Bool
  , flagCompile  :: !Bool
  , flagOutput   :: !FilePath
  , flagFromJson :: !Bool
  , flagTarget   :: !CompileTarget
  , flagNoSimplify :: !Bool
  , flagEvidenceMode :: !EvidenceMode
  } deriving (Show)

data EvidenceMode = EvidenceInline | EvidencePlotkin
  deriving (Show, Eq)

defaultFlags :: Flags
defaultFlags = Flags False False False False False False "a.out" False TargetNative False EvidenceInline

data Command
  = ShowHelp
  | DemoMode Flags
  | DemoEffect Flags
  | DemoAbort Flags
  | DemoMultiEffect Flags
  | CompileFiles [FilePath] Flags
  | SwiftCrossCheck [FilePath]
  | PostProcessMlir [FilePath]
  | TypeCheck [FilePath]
  deriving (Show)

parseArgs :: [String] -> Command
parseArgs [] = ShowHelp
parseArgs args
  | "--help" `elem` args || "-h" `elem` args = ShowHelp
  | "--demo-effect" `elem` args = DemoEffect (parseFlags args)
  | "--demo-abort" `elem` args = DemoAbort (parseFlags args)
  | "--demo-multi-effect" `elem` args = DemoMultiEffect (parseFlags args)
  | "--demo" `elem` args = DemoMode (parseFlags args)
  | "--swift-crosscheck" `elem` args =
      let (files, _) = partition (not . isFlag) (removeArgValues args)
      in SwiftCrossCheck files
  | "--postprocess-mlir" `elem` args =
      let (files, _) = partition (not . isFlag) (removeArgValues args)
      in PostProcessMlir files
  | "--typecheck" `elem` args =
      let (files, _) = partition (not . isFlag) (removeArgValues args)
      in TypeCheck files
  | otherwise =
      let flags = parseFlags args
          (files, _flagArgs) = partition (not . isFlag) (removeArgValues args)
      in if null files
         -- --from-json with no file argument means read stdin
         then if flagFromJson flags
              then CompileFiles ["-"] flags
              else ShowHelp
         else CompileFiles files flags

isFlag :: String -> Bool
isFlag ('-':'-':_) = True
isFlag "-o" = True
isFlag _ = False

-- | Remove flag arguments (e.g. the path after -o or --output) from the list
removeArgValues :: [String] -> [String]
removeArgValues [] = []
removeArgValues ("-o":_:rest) = removeArgValues rest
removeArgValues ("--output":_:rest) = removeArgValues rest
removeArgValues ("--target":_:rest) = removeArgValues rest
removeArgValues ("--evidence":_:rest) = removeArgValues rest
removeArgValues (x:rest) = x : removeArgValues rest

parseFlags :: [String] -> Flags
parseFlags args = Flags
  { flagEmitCore = "--emit-core" `elem` args
  , flagEmitMlir = "--emit-mlir" `elem` args
  , flagEmitEffectMlir = "--emit-effect-mlir" `elem` args
  , flagEmitOrgan = "--emit-organ" `elem` args
                 || "--emit-organ-post-passes" `elem` args
  , flagEmitOrganPost = "--emit-organ-post-passes" `elem` args
  , flagCompile  = "--compile" `elem` args
  , flagOutput   = case dropWhile (/= "--output") args of
                     ("--output":o:_) -> o
                     _ -> case dropWhile (/= "-o") args of
                            ("-o":o:_) -> o
                            _ -> "a.out"
  , flagFromJson = "--from-json" `elem` args || "--from-organ" `elem` args
  , flagTarget   = case dropWhile (/= "--target") args of
                     ("--target":"wasm32":_) -> TargetWasm32
                     ("--target":"wasm":_)   -> TargetWasm32
                     _ -> TargetNative
  , flagNoSimplify = "--no-simplify" `elem` args
  , flagEvidenceMode =
      let modes = [ m | a <- args, m <- evidenceArg a ]
      in case modes of
           (m:_) -> m
           []    -> EvidenceInline
  }
  where
    evidenceArg "--evidence-plotkin"      = [EvidencePlotkin]
    evidenceArg "--evidence-inline"       = [EvidenceInline]
    evidenceArg s | take 11 s == "--evidence=" =
        case drop 11 s of
          "plotkin" -> [EvidencePlotkin]
          "inline"  -> [EvidenceInline]
          _         -> []
    evidenceArg _ = []

-- Compilation dispatch

compileFile :: Bool -> Bool -> FilePath -> IO (Either (FilePath, Text) [Program])
compileFile fromJson simplify path = do
  let ext = takeExtension path
  -- Haskell uses multi-module compilation (chases imports)
  if not fromJson && ext == ".hs"
    then do
      result <- compileHaskellMulti simplify path
      pure $ case result of
        Left err    -> Left (path, err)
        Right progs -> Right progs
    else do
      result <- case () of
        _ | fromJson || ext == ".json" || ext == ".organ" || path == "-"
                    -> compileOrganIR path
          | otherwise -> case ext of
              ".hs" -> compileHaskell simplify path  -- unreachable, handled above
              ".kk" -> compileKoka path
              ".m"  -> compileMercury path
              ".rs" -> compileRust path
              ".py" -> compilePython path
              ".go" -> compileGo path
              ".fut" -> compileFuthark path
              ".scm" -> compileScheme path
              ".swift" -> compileSwift path
              ".ml"  -> compileOCaml path
              ".erl" -> compileErlang path
              ".fs"  -> compileFSharp path
              ".fsx" -> compileFSharp path
              ".idr" -> compileIdris path
              _     -> pure $ Left $ "Unknown file extension: " <> T.pack ext
      pure $ case result of
        Left err   -> Left (path, err)
        Right prog -> Right [prog]

compileHaskell :: Bool -> FilePath -> IO (Either Text Program)
compileHaskell simplify inputFile = do
  hPutStrLn stderr $ "Compiling Haskell: " <> inputFile
  result <- compileToCoreWith simplify inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "  GHC bridge error: " <> err
      TIO.putStrLn $ "  Using demo program..."
      pure $ Right demoHaskellProgram
    Right gcr -> pure $ Right (gcrProgram gcr)

-- | Multi-module Haskell compilation: compile the target file and all
-- its local imports (home-package modules) through the GHC bridge.
compileHaskellMulti :: Bool -> FilePath -> IO (Either Text [Program])
compileHaskellMulti simplify inputFile = do
  hPutStrLn stderr $ "Compiling Haskell: " <> inputFile
  result <- compileToCoreMulti simplify inputFile
  case result of
    Left err -> do
      TIO.hPutStrLn stderr $ "  GHC bridge error: " <> err
      TIO.hPutStrLn stderr $ "  Using demo program..."
      pure $ Right [demoHaskellProgram]
    Right gcrs -> do
      if length gcrs > 1
        then hPutStrLn stderr $ "  Compiled " <> show (length gcrs)
               <> " modules: " <> unwords [T.unpack (gcrModuleName g) | g <- gcrs]
        else pure ()
      pure $ Right [gcrProgram g | g <- gcrs]

compileMercury :: FilePath -> IO (Either Text Program)
compileMercury inputFile = do
  hPutStrLn stderr $ "Compiling Mercury: " <> inputFile
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

compileKoka :: FilePath -> IO (Either Text Program)
compileKoka inputFile = do
  hPutStrLn stderr $ "Compiling Koka: " <> inputFile
  result <- compileKokaFile inputFile
  case result of
    Left err -> do
      TIO.putStrLn $ "  Koka bridge error: " <> err
      TIO.putStrLn $ "  Using demo program..."
      pure $ Right demoKokaProgram
    Right prog -> pure $ Right prog

compileRust :: FilePath -> IO (Either Text Program)
compileRust inputFile = do
  hPutStrLn stderr $ "Compiling Rust: " <> inputFile
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

compilePython :: FilePath -> IO (Either Text Program)
compilePython inputFile = do
  hPutStrLn stderr $ "Compiling Python: " <> inputFile
  result <- parsePython inputFile
  case result of
    Left err -> pure $ Left $ "Python bridge error: " <> err
    Right sexpr ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translatePythonAst modName sexpr

compileGo :: FilePath -> IO (Either Text Program)
compileGo inputFile = do
  hPutStrLn stderr $ "Compiling Go: " <> inputFile
  result <- parseGo inputFile
  case result of
    Left err -> pure $ Left $ "Go bridge error: " <> err
    Right sexpr -> pure $ translateGoAst sexpr

compileFuthark :: FilePath -> IO (Either Text Program)
compileFuthark inputFile = do
  hPutStrLn stderr $ "Compiling Futhark: " <> inputFile
  result <- parseFutharkFile inputFile
  case result of
    Left err -> pure $ Left $ "Futhark bridge error: " <> err
    Right ast ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateFuthark modName ast

compileScheme :: FilePath -> IO (Either Text Program)
compileScheme inputFile = do
  hPutStrLn stderr $ "Compiling Scheme: " <> inputFile
  result <- readSchemeFile inputFile
  case result of
    Left err -> pure $ Left $ "Scheme bridge error: " <> err
    Right forms ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateScheme modName forms

compileSwift :: FilePath -> IO (Either Text Program)
compileSwift inputFile = do
  hPutStrLn stderr $ "Compiling Swift: " <> inputFile
  result <- readSwiftFile inputFile
  case result of
    Left err -> pure $ Left $ "Swift bridge error: " <> err
    Right ast ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateSwift modName ast

compileOCaml :: FilePath -> IO (Either Text Program)
compileOCaml inputFile = do
  hPutStrLn stderr $ "Compiling OCaml: " <> inputFile
  result <- readOCamlFile inputFile
  case result of
    Left err -> pure $ Left $ "OCaml bridge error: " <> err
    Right ast ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateOCaml modName ast

compileErlang :: FilePath -> IO (Either Text Program)
compileErlang inputFile = do
  hPutStrLn stderr $ "Compiling Erlang: " <> inputFile
  result <- readErlangFile inputFile
  case result of
    Left err -> pure $ Left $ "Erlang bridge error: " <> err
    Right ast ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateErlang modName ast

compileFSharp :: FilePath -> IO (Either Text Program)
compileFSharp inputFile = do
  hPutStrLn stderr $ "Compiling F#: " <> inputFile
  result <- readFSharpFile inputFile
  case result of
    Left err -> pure $ Left $ "F# bridge error: " <> err
    Right ast ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateFSharp modName ast

compileIdris :: FilePath -> IO (Either Text Program)
compileIdris inputFile = do
  hPutStrLn stderr $ "Compiling Idris2: " <> inputFile
  result <- readIdrisFile inputFile
  case result of
    Left err -> pure $ Left $ "Idris2 bridge error: " <> err
    Right decls ->
      let modName = T.pack (takeBaseName inputFile)
      in pure $ translateIdris modName decls

-- | Count Perceus RC operations in a program's expressions.
countPerceusOps :: Program -> (Int, Int, Int)
countPerceusOps prog = foldr addDef (0,0,0) (progDefs prog)
  where
    addDef d acc = go (defExpr d) acc
    add3 (a,b,c) (x,y,z) = (a+x, b+y, c+z)
    go e acc = case e of
      ERetain  _    -> add3 acc (1,0,0)
      ERelease _    -> add3 acc (0,1,0)
      EDrop    _    -> add3 acc (0,0,1)
      ELam _ b      -> go b acc
      EApp f as     -> foldr go (go f acc) as
      ELet bss body -> foldr (\bs a -> foldr (\(Bind _ _ be _) a' -> go be a') a bs) (go body acc) bss
      ECase s bs    -> foldr (\(Branch _ _ be) a -> go be a) (go s acc) bs
      EHandle _ h b -> go h (go b acc)
      EPerform _ as -> foldr go acc as
      EDelay b      -> go b acc
      EForce b      -> go b acc
      _             -> acc

-- | Run swiftc SIL + Frankenstein Perceus and compare RC counts.
swiftCrossCheck :: FilePath -> IO ()
swiftCrossCheck path = do
  TIO.putStrLn $ "=== Swift Perceus cross-check: " <> T.pack path <> " ==="
  -- Frankenstein side
  result <- compileSwift path
  case result of
    Left err -> TIO.putStrLn $ "Frankenstein bridge failed: " <> err
    Right rawProg -> do
      let optProg = effectOptimize rawProg
          prog    = insertPerceus (evidencePassGlobal (collectGlobalEffects optProg) optProg)
          (fkRetain, fkRelease, fkDrop) = countPerceusOps prog
      TIO.putStrLn $ "Frankenstein Perceus: retain=" <> T.pack (show fkRetain)
                  <> " release=" <> T.pack (show fkRelease)
                  <> " drop=" <> T.pack (show fkDrop)
      -- swiftc SIL side
      silR <- emitSilCounts path
      case silR of
        Left err -> TIO.putStrLn $ "swiftc SIL failed: " <> err
        Right sc -> do
          TIO.putStrLn $ "swiftc -O -emit-sil:    strong_retain=" <> T.pack (show (silStrongRetain sc))
                      <> " strong_release=" <> T.pack (show (silStrongRelease sc))
                      <> " copy_value=" <> T.pack (show (silCopyValue sc))
                      <> " destroy_value=" <> T.pack (show (silDestroyValue sc))
          let fkTotal  = fkRetain + fkRelease + fkDrop
              silTotal = silStrongRetain sc + silStrongRelease sc
                       + silCopyValue sc + silDestroyValue sc
          if fkTotal == 0 && silTotal == 0
            then TIO.putStrLn "Agreement: both emit zero RC ops (trivial Int program)."
            else if fkTotal == silTotal
                 then TIO.putStrLn "Agreement: total RC op counts match."
                 else TIO.putStrLn $ "Divergence: Frankenstein=" <> T.pack (show fkTotal)
                                  <> " vs swiftc=" <> T.pack (show silTotal)

compileOrganIR :: FilePath -> IO (Either Text Program)
compileOrganIR inputFile = do
  let label = if inputFile == "-" then "<stdin>" else inputFile
  hPutStrLn stderr $ "Compiling OrganIR: " <> label
  jsonText <- if inputFile == "-"
              then TIO.getContents
              else TIO.readFile inputFile
  case consumeProgram jsonText of
    Left err   -> pure $ Left $ "OrganIR consumer error: " <> T.pack err
    Right prog -> pure $ Right prog

-- Output handling

handleOutput :: Program -> Flags -> IO ()
handleOutput progRaw flags = do
  -- Optional per-pass AST dump for host-vs-self-host divergence debugging.
  -- Enabled by FRANKENSTEIN_DUMP_AST env var. Both this host path and the
  -- self-host driver.c emit dumps with identical "=== AST after <pass> ==="
  -- markers, so per-pass diffs localize the diverging pass.
  dumpEnv <- lookupEnv "FRANKENSTEIN_DUMP_AST"
  let dumpStage label p = case dumpEnv of
        Nothing -> pure ()
        Just _  -> do
          hPutStrLn stderr ("=== AST after " ++ label ++ " ===")
          TIO.hPutStrLn stderr (dumpProgram p)
  dumpStage "consumer" progRaw
  -- Flatten nested patterns so downstream passes see only one level of
  -- constructor destructuring per case branch.
  -- NOTE: deriveSelectors already ran before the linker — do NOT re-run
  -- here because the linker mangles existing selector names (e.g.
  -- "progName" -> "Frankenstein.Core.Types_progName") and re-running
  -- would create duplicate selectors that only match the unmangled name.
  let prog0 = flattenPatterns progRaw
  dumpStage "flattenPatterns" prog0
  -- Run effect optimizations before evidence pass
  let (optProg, optStats) = effectOptimizeWithStats prog0
  dumpStage "effectOptimize" optProg
  -- Run global evidence pass again on the merged program.  The pre-linker
  -- pass resolves most effects; this catches any that survived (e.g. effects
  -- introduced by flattenPatterns or deriveSelectors re-run).
  let globalEffects = collectGlobalEffects optProg
      progEv = evidencePassGlobal globalEffects optProg
  dumpStage "evidencePass" progEv
  let prog = insertPerceus progEv
      config = defaultEmitConfig
        { ecOutputPath = flagOutput flags
        , ecKokaRuntimePath = Just "runtime/kk_runtime.c"
        , ecTarget = flagTarget flags
        }
  dumpStage "insertPerceus" prog
  -- Print optimization stats if any optimizations fired (not for effect-dialect mode)
  let totalOpts = eosInlined optStats + eosEliminated optStats + eosTailRes optStats
  if totalOpts > 0 && not (flagEmitEffectMlir flags)
    then TIO.putStrLn $ "Effect opts: " <> T.pack (show (eosInlined optStats)) <> " inlined, "
                <> T.pack (show (eosEliminated optStats)) <> " eliminated, "
                <> T.pack (show (eosTailRes optStats)) <> " tail-resumptive"
    else pure ()
  case () of
    _ | flagCompile flags && flagTarget flags == TargetWasm32 -> do
          TIO.putStrLn "=== Compiling to WebAssembly ==="
          result <- compileToWasm config prog
          case result of
            Left err -> TIO.putStrLn $ "Compilation error: " <> err
            Right path -> TIO.putStrLn $ "Compiled: " <> T.pack path
      | flagCompile flags -> do
          TIO.putStrLn "=== Compiling to native ==="
          result <- compileToExecutable config prog
          case result of
            Left err -> TIO.putStrLn $ "Compilation error: " <> err
            Right path -> TIO.putStrLn $ "Compiled: " <> T.pack path
      | flagEmitOrgan flags -> do
          -- Emit OrganIR JSON from the raw (pre-optimization) program.
          -- normalizePatterns converts Bool PatCon → PatLit and adds
          -- PatWild defaults to exhaustive constructor cases, so the
          -- self-hosted compiler avoids buggy code paths.
          if flagEmitOrganPost flags
            then TIO.putStrLn $ OrganEmit.emitProgram prog
            else TIO.putStrLn $ OrganEmit.emitProgram (normalizePatterns progRaw)
      | flagEmitEffectMlir flags -> do
          -- Emit MLIR with frankenstein.* dialect ops — skip both the
          -- effect optimizer (which would inline handlers) and the
          -- evidence pass, so EHandle/EPerform appear as dialect ops.
          let effectProg = insertPerceus prog0
          TIO.putStrLn $ emitProgramWithEffects effectProg
      | flagEmitCore flags -> do
          TIO.putStrLn "=== Frankenstein Core ==="
          TIO.putStrLn $ prettyProgram prog
          -- Show cycle analysis results
          let cycles = analyzeCycles prog
              cyclicDefs = filter ciCyclic cycles
          if null cyclicDefs
            then TIO.putStrLn "\n=== Cycle Analysis: all definitions acyclic ==="
            else do
              TIO.putStrLn "\n=== Cycle Analysis ==="
              mapM_ (\ci -> TIO.putStrLn $ "  " <> nameText (qnameName (ciName ci))
                            <> ": " <> ciReason ci) cyclicDefs
      | flagEmitMlir flags && flagTarget flags == TargetWasm32 ->
          TIO.putStrLn $ emitProgramWasm prog
      | flagEmitMlir flags ->
          TIO.putStrLn $ emitProgram prog
      | otherwise -> do
          TIO.putStrLn "=== Frankenstein Core ==="
          TIO.putStrLn $ prettyProgram prog
          TIO.putStrLn ""
          TIO.putStrLn "=== MLIR ==="
          TIO.putStrLn $ emitProgram prog

-- Helpers

partitionResults :: [Either (FilePath, Text) [Program]] -> ([(FilePath, Text)], [[Program]])
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
formatLinkError (AmbiguousReference name mods) =
  "Link error: ambiguous reference '" <> name
  <> "' defined in multiple modules: " <> T.intercalate ", " mods

printHelp :: IO ()
printHelp = do
  putStrLn "Frankenstein — Polyglot Compiler"
  putStrLn ""
  putStrLn "Usage: frankenstein [options] <input-files...>"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  --emit-core       Print Frankenstein Core IR"
  putStrLn "  --emit-mlir       Print MLIR output (after evidence lowering)"
  putStrLn "  --emit-effect-mlir Print MLIR with frankenstein.* dialect ops"
  putStrLn "  --emit-organ      Print OrganIR JSON (interchange format)"
  putStrLn "  --compile         Compile to native executable (or .wasm)"
  putStrLn "  --target wasm32   Target WebAssembly (use with --compile)"
  putStrLn "  -o, --output      Output path (default: a.out)"
  putStrLn "  --from-json       Treat input as OrganIR JSON (also: --from-organ)"
  putStrLn "  --no-simplify     Skip GHC Core simplifier (for self-hosting)"
  putStrLn "  --demo            Run built-in demo (factorial)"
  putStrLn "  --swift-crosscheck Compare Frankenstein Perceus vs swiftc SIL RC ops"
  putStrLn "  -h, --help        Show this help"
  putStrLn ""
  putStrLn "Supported input formats:"
  putStrLn "  .hs     Haskell   (via GHC API)"
  putStrLn "  .kk     Koka      (via Koka compiler library)"
  putStrLn "  .m      Mercury   (via mmc --dump-hlds)"
  putStrLn "  .rs     Rust      (via rustc MIR)"
  putStrLn "  .py     Python    (via CPython ast module)"
  putStrLn "  .go     Go        (via go/parser stdlib helper)"
  putStrLn "  .fut    Futhark   (via in-tree Pratt parser)"
  putStrLn "  .scm    Scheme    (via in-tree reader + CPS converter, supports call/cc)"
  putStrLn "  .swift  Swift     (via swiftc -dump-ast; Int subset; Perceus cross-check)"
  putStrLn "  .ml     OCaml     (via ocamlc -dparsetree; Int subset)"
  putStrLn "  .erl    Erlang    (via escript + erl_scan/erl_parse; Int subset)"
  putStrLn "  .fs     F#        (via dotnet fsi + FSharp.Compiler.Service; Int subset)"
  putStrLn "  .idr    Idris 2   (in-tree source parser; Int subset)"
  putStrLn "  .json   OrganIR   (organ-bank JSON)"
  putStrLn "  .organ  OrganIR   (organ-bank JSON)"
  putStrLn ""
  putStrLn "OrganIR JSON can also be piped from stdin:"
  putStrLn "  rustc-shim foo.rs | frankenstein --from-json -"
  putStrLn "  frankenstein --from-json < dump.json"
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

-- | Demo: tail-resumptive reader effect
--
-- effect ask { get : () -> int }
-- fun main():
--   handle<ask> { get() + get() } with \() -> 42
-- Expected output: 84
--
-- Naming convention:
--   EffectRow QName: QName "" "ask"  → effectRowName = "ask"
--   EPerform QName:  QName "ask" "get" → effName = "ask" (matches)
demoEffectProgram :: Program
demoEffectProgram = Program
  { progName = QName "demo" (Name "effects" 0)
  , progDefs =
      [ Def
          { defName = QName "" (Name "main" 10)
          , defType = TFun [] EffectRowEmpty intType
          , defExpr =
              EHandle
                (EffectRowExtend (QName "" (Name "ask" 0)) EffectRowEmpty)
                -- Handler: \() -> 42  (tail-resumptive: returns a constant)
                (ELam [(Name "x" 1, intType)] (ELit (LitInt 42)))
                -- Body: perform ask/get() + perform ask/get()
                (EApp (EVar (Name "+" 0))
                  [ EPerform (QName "ask" (Name "get" 0)) []
                  , EPerform (QName "ask" (Name "get" 0)) []
                  ])
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects =
      [ EffectDecl
          { effectName = QName "" (Name "ask" 0)
          , effectParams = []
          , effectOps = [OpDecl (QName "ask" (Name "get" 0)) intType]
          }
      ]
  }
  where intType = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

-- | Demo: abort effect (exception-like, uses setjmp/longjmp)
--
-- effect exn { raise : int -> never }
-- fun main():
--   handle<exn> {
--     perform exn/raise(42)   -- aborts here
--     99                      -- NEVER reached
--   } with \msg -> msg        -- abort handler: returns the raised value
-- Expected output: 42
demoAbortProgram :: Program
demoAbortProgram = Program
  { progName = QName "demo" (Name "abort" 0)
  , progDefs =
      [ Def
          { defName = QName "" (Name "main" 10)
          , defType = TFun [] EffectRowEmpty intType
          , defExpr =
              EHandle
                (EffectRowExtend (QName "" (Name "exn" 0)) EffectRowEmpty)
                -- Abort handler: \msg resume -> msg (resume unused → abort)
                (ELam [(Name "msg" 1, intType), (Name "resume" 3, intType)]
                       (EVar (Name "msg" 1)))
                -- Body: perform exn/raise(42); 99
                -- (The 99 should NEVER be reached due to abort)
                (ELet [[Bind
                  { bindName = Name "_unused" 2
                  , bindType = intType
                  , bindExpr = EPerform (QName "exn" (Name "raise" 0)) [ELit (LitInt 42)]
                  , bindSort = DefVal
                  }]]
                  (ELit (LitInt 99)))
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects =
      [ EffectDecl
          { effectName = QName "" (Name "exn" 0)
          , effectParams = []
          , effectOps = [OpDecl (QName "exn" (Name "raise" 0)) intType]
          }
      ]
  }
  where intType = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

-- | Multi-effect demo: compose a reader effect (tail-resumptive) with an
-- exception effect (abort) in the same program.
--
-- handle<exn> {
--   handle<ask> {
--     let x = perform ask/get()       -- returns 10
--     if x > 5: perform exn/raise(x)  -- aborts with 10
--     x + 1
--   } with \() -> 10                  -- reader: always returns 10
-- } with \v resume -> v * 2           -- abort: doubles the value
--
-- Expected: ask returns 10, x > 5 is true, exn/raise(10) aborts,
-- abort handler computes 10 * 2 = 20.
demoMultiEffectProgram :: Program
demoMultiEffectProgram = Program
  { progName = QName "demo" (Name "multi_effect" 0)
  , progDefs =
      [ Def
          { defName = QName "" (Name "main" 10)
          , defType = TFun [] EffectRowEmpty intType
          , defExpr =
              -- Outer: handle<exn> { ... } with \v resume -> v * 2
              EHandle
                (EffectRowExtend (QName "" (Name "exn" 0)) EffectRowEmpty)
                -- Abort handler: \v resume -> v * 2 (resume unused → abort)
                (ELam [(Name "v" 1, intType), (Name "resume" 3, intType)]
                  (EApp (EVar (Name "*" 0)) [EVar (Name "v" 1), ELit (LitInt 2)]))
                -- Inner: handle<ask> { body } with \() -> 10
                (EHandle
                  (EffectRowExtend (QName "" (Name "ask" 0)) EffectRowEmpty)
                  -- Reader handler: \() -> 10 (tail-resumptive)
                  (ELam [(Name "x" 4, intType)] (ELit (LitInt 10)))
                  -- Body: let x = perform ask/get()
                  --       in if x > 5 then perform exn/raise(x) else x + 1
                  (ELet [[Bind
                    { bindName = Name "x" 5
                    , bindType = intType
                    , bindExpr = EPerform (QName "ask" (Name "get" 0)) []
                    , bindSort = DefVal
                    }]]
                    (ECase (EApp (EVar (Name ">" 0)) [EVar (Name "x" 5), ELit (LitInt 5)])
                      [ Branch (PatLit (LitInt 1)) Nothing
                          (EPerform (QName "exn" (Name "raise" 0)) [EVar (Name "x" 5)])
                      , Branch (PatVar (Name "_" 0) intType) Nothing
                          (EApp (EVar (Name "+" 0)) [EVar (Name "x" 5), ELit (LitInt 1)])
                      ])))
          , defSort = DefFun
          , defVisibility = Public
          }
      ]
  , progData = []
  , progEffects =
      [ EffectDecl
          { effectName = QName "" (Name "ask" 0)
          , effectParams = []
          , effectOps = [OpDecl (QName "ask" (Name "get" 0)) intType]
          }
      , EffectDecl
          { effectName = QName "" (Name "exn" 0)
          , effectParams = []
          , effectOps = [OpDecl (QName "exn" (Name "raise" 0)) intType]
          }
      ]
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

demoKokaProgram :: Program
demoKokaProgram = Program
  { progName = QName "demo" (Name "koka" 0)
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
          , defType = TFun [] (EffectRowExtend (QName "std" (Name "io" 0)) EffectRowEmpty)
                          intType
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
prettyExpr (EFunRef qn) = "&" <> ppQName qn
prettyExpr _ = "..."

ppQName :: QName -> Text
ppQName qn
  | T.null (qnameModule qn) = nameText (qnameName qn)
  | otherwise = qnameModule qn <> "." <> nameText (qnameName qn)
