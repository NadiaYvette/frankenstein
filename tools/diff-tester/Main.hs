{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
-- | DB6 differential test driver.
--
-- Generates small random Haskell programs (data decls + case-expression-heavy
-- functions), compiles each through TWO pipelines, and reports any byte-level
-- MLIR divergence:
--
--   (host)    .hs --> frankenstein --evidence=plotkin --emit-mlir
--   (stage 1) .hs --> frankenstein --emit-organ --> stage1 --> MLIR
--
-- A self-hosted compiler at fixed point produces identical MLIR via both
-- routes. QuickCheck shrinks any divergent program to a minimal failing case.
module Main where

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Gen as QC
import qualified Test.QuickCheck.Random as QC

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Map.Strict (Map)
import           Data.Set (Set)

import Control.Exception (catch, SomeException)
import Control.Monad (when, unless, forM, forM_, replicateM)
import Data.IORef
import Data.List (intercalate, isPrefixOf, nub, sortBy)
import Data.Ord (Down(..), comparing)
import System.Directory (doesFileExist, createDirectoryIfMissing, getTemporaryDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>), (<.>))
import System.IO
import System.IO.Temp (withSystemTempDirectory)
import System.Process

-- =============================================================================
-- AST: a tiny monomorphic-Int Haskell subset
-- =============================================================================

data Prog = Prog
  { progDatas :: [DataDecl]
  , progFns   :: [FnDef]
  , progMain  :: MainExpr
  } deriving Show

data DataDecl = DataDecl
  { ddName  :: String          -- type name (TName)
  , ddCtors :: [(String, Int)] -- (ctor name, Int arity)
  } deriving Show

data FnDef = FnDef
  { fnName :: String
  , fnArgs :: [(String, ArgTy)]
  , fnBody :: Expr
  } deriving Show

data ArgTy = TyInt | TyData String
  deriving Show

data Expr
  = EInt Int
  | EVar String
  | EAdd Expr Expr
  | ECase String [Branch]  -- scrutinee always a bare arg name; keeps gen simple
  | ECtor String [Expr]    -- saturated ctor application
  deriving Show

data Branch = Branch Pat Expr deriving Show

data Pat
  = PVar String
  | PWild
  | PCon String [String]   -- saturated ctor; sub-patterns are variables
  deriving Show

-- An entry point: pick an existing fn and saturate it.
data MainExpr = MainExpr String [Expr] deriving Show

-- =============================================================================
-- Pretty printer
-- =============================================================================

prettyProg :: String -> Prog -> String
prettyProg modName (Prog ds fns m) = unlines $
  [ "module " ++ modName ++ " where"
  , "" ] ++
  concatMap (\d -> [prettyData d, ""]) ds ++
  concatMap (\f -> [prettyFnSig f, prettyFnDef f, ""]) fns ++
  [ "main :: Int"
  , "main = " ++ prettyMain m
  ]

prettyData :: DataDecl -> String
prettyData (DataDecl n cs) =
  "data " ++ n ++ " = " ++ intercalate " | " (map prettyCtor cs)
  where prettyCtor (cn, ar) = unwords (cn : replicate ar "Int")

prettyFnSig :: FnDef -> String
prettyFnSig (FnDef n as _) =
  n ++ " :: " ++ intercalate " -> " (map (prettyTy . snd) as ++ ["Int"])

prettyTy :: ArgTy -> String
prettyTy TyInt = "Int"
prettyTy (TyData t) = t

prettyFnDef :: FnDef -> String
prettyFnDef (FnDef n as b) =
  n ++ " " ++ unwords (map fst as) ++ " = " ++ prettyExpr b

prettyExpr :: Expr -> String
prettyExpr = \case
  EInt i       -> show i
  EVar v       -> v
  EAdd a b     -> "(" ++ prettyExpr a ++ " + " ++ prettyExpr b ++ ")"
  ECtor c args -> "(" ++ unwords (c : map prettyExpr args) ++ ")"
  ECase sc brs -> "case " ++ sc ++ " of { "
                  ++ intercalate "; " (map prettyBranch brs)
                  ++ " }"

prettyBranch :: Branch -> String
prettyBranch (Branch p e) = prettyPat p ++ " -> " ++ prettyExpr e

prettyPat :: Pat -> String
prettyPat (PVar v)     = v
prettyPat PWild        = "_"
prettyPat (PCon c [])  = c
prettyPat (PCon c vs)  = "(" ++ unwords (c : vs) ++ ")"

prettyMain :: MainExpr -> String
prettyMain (MainExpr fn args) = unwords (fn : map (parenIfApp . prettyExpr) args)
  where
    parenIfApp s = if ' ' `elem` s && not ("(" `isPrefixOf` s) then "(" ++ s ++ ")" else s

-- =============================================================================
-- Generator
-- =============================================================================

instance QC.Arbitrary Prog where
  arbitrary = genProg
  shrink    = shrinkProg

genProg :: QC.Gen Prog
genProg = do
  nd <- QC.choose (0 :: Int, 2)
  let datas = [genDataDecl i | i <- [1..nd]]
  ds <- sequence datas
  let dctx = ds
  nf <- QC.choose (1 :: Int, 3)
  fns <- sequence [genFnDef dctx i | i <- [1..nf]]
  m <- genMainExpr dctx fns
  return (Prog ds fns m)

genDataDecl :: Int -> QC.Gen DataDecl
genDataDecl idx = do
  nctors <- QC.choose (1 :: Int, 3)
  cs <- forM [1..nctors] $ \j -> do
          ar <- QC.frequency [(2, return (0 :: Int)), (3, return 1), (2, return 2)]
          return ("C" ++ show idx ++ "_" ++ show j, ar)
  return (DataDecl ("T" ++ show idx) cs)

genFnDef :: [DataDecl] -> Int -> QC.Gen FnDef
genFnDef ds idx = do
  nargs <- QC.choose (1 :: Int, 2)
  args  <- forM [1..nargs] $ \k -> do
             ty <- if null ds
                     then return TyInt
                     else QC.frequency [(2, return TyInt),
                                        (3, TyData . ddName <$> QC.elements ds)]
             return ("x" ++ show k, ty)
  body <- genBody ds args
  return (FnDef ("f" ++ show idx) args body)

genBody :: [DataDecl] -> [(String, ArgTy)] -> QC.Gen Expr
genBody ds args =
  case [(n, t) | (n, TyData t) <- args] of
    []        -> genIntExpr (intsInScope args)
    ((an,tn):_) -> do
      let DataDecl _ ctors = headDecl tn ds
      brs <- genBranches args ctors
      return (ECase an brs)
  where
    headDecl t = head . filter (\d -> ddName d == t)
    intsInScope as = [n | (n, TyInt) <- as]

genBranches :: [(String, ArgTy)] -> [(String, Int)] -> QC.Gen [Branch]
genBranches args ctors = do
  shape <- QC.elements [ExhaustiveCtors, OnePlusDefault, AllDefault]
  case shape of
    ExhaustiveCtors -> mapM (genConBranch args) ctors
    OnePlusDefault  -> do
      n <- QC.choose (1 :: Int, length ctors)
      let used = take n ctors
      brs <- mapM (genConBranch args) used
      d   <- Branch PWild <$> genIntExpr (intsInScope args)
      return (brs ++ [d])
    AllDefault -> do
      d <- Branch PWild <$> genIntExpr (intsInScope args)
      return [d]
  where intsInScope as = [n | (n, TyInt) <- as]

data BranchShape = ExhaustiveCtors | OnePlusDefault | AllDefault
  deriving (Eq, Show)

genConBranch :: [(String, ArgTy)] -> (String, Int) -> QC.Gen Branch
genConBranch args (cn, ar) = do
  let vs = ["v" ++ show k | k <- [1..ar :: Int]]
  let scopeInts = [n | (n, TyInt) <- args] ++ vs
  body <- genIntExpr scopeInts
  return (Branch (PCon cn vs) body)

genIntExpr :: [String] -> QC.Gen Expr
genIntExpr scope = QC.sized go
  where
    go sz | sz <= 0 = leaf
          | otherwise = QC.frequency
              [ (3, leaf)
              , (2, EAdd <$> QC.resize (sz `div` 2) (genIntExpr scope)
                          <*> QC.resize (sz `div` 2) (genIntExpr scope))
              ]
    leaf
      | null scope = EInt <$> QC.choose (0 :: Int, 99)
      | otherwise  = QC.frequency
          [ (1, EInt <$> QC.choose (0 :: Int, 99))
          , (2, EVar <$> QC.elements scope)
          ]

genMainExpr :: [DataDecl] -> [FnDef] -> QC.Gen MainExpr
genMainExpr ds fns = do
  fn <- QC.elements fns
  args <- forM (fnArgs fn) $ \(_, ty) -> genArgValue ds ty
  return (MainExpr (fnName fn) args)

genArgValue :: [DataDecl] -> ArgTy -> QC.Gen Expr
genArgValue _  TyInt        = EInt <$> QC.choose (0 :: Int, 99)
genArgValue ds (TyData t)   = do
  let DataDecl _ ctors = head (filter (\d -> ddName d == t) ds)
  (cn, ar) <- QC.elements ctors
  args <- replicateM ar (EInt <$> QC.choose (0 :: Int, 99))
  return (ECtor cn args)

-- =============================================================================
-- Shrinking
-- =============================================================================

shrinkProg :: Prog -> [Prog]
shrinkProg (Prog ds fns m) =
  -- (1) drop a data decl (and any fn that referenced it)
  [ Prog ds' (rebindFns ds' fns) (rebindMain ds' fns m)
  | ds' <- shrinkList (const []) ds, validRefs ds' fns m ]
  ++
  -- (2) drop a fn
  [ Prog ds fns' (rebindMain ds fns' m)
  | fns' <- shrinkList (const []) fns, not (null fns'), validMain ds fns' m ]
  ++
  -- (3) shrink case branches in fns (drop a branch)
  [ Prog ds fns' m
  | fns' <- shrinkOne shrinkFnBranches fns ]
  ++
  -- (4) shrink main args (smaller ints, simpler ctors)
  [ Prog ds fns m'
  | m' <- shrinkMain m ]

validRefs :: [DataDecl] -> [FnDef] -> MainExpr -> Bool
validRefs ds fns _ =
  let avail = Set.fromList (map ddName ds)
      used  = Set.fromList [t | f <- fns, (_, TyData t) <- fnArgs f]
  in used `Set.isSubsetOf` avail

validMain :: [DataDecl] -> [FnDef] -> MainExpr -> Bool
validMain _ fns (MainExpr fn _) = any ((== fn) . fnName) fns

rebindFns :: [DataDecl] -> [FnDef] -> [FnDef]
rebindFns ds fns =
  let avail = Set.fromList (map ddName ds)
  in [ f | f <- fns, all (argOK avail) (fnArgs f) ]
  where argOK _ (_, TyInt) = True
        argOK s (_, TyData t) = Set.member t s

rebindMain :: [DataDecl] -> [FnDef] -> MainExpr -> MainExpr
rebindMain _ fns m@(MainExpr fn _)
  | any ((== fn) . fnName) fns = m
  | otherwise = case fns of
      (f:_) -> MainExpr (fnName f) [defaultArg ty | (_, ty) <- fnArgs f]
      []    -> m
  where defaultArg TyInt = EInt 0
        defaultArg (TyData _) = EInt 0  -- ill-typed but only used post-filter

shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkList _ [] = []
shrinkList sh xs =
  [ take i xs ++ drop (i+1) xs | i <- [0..length xs - 1] ]

shrinkOne :: (a -> [a]) -> [a] -> [[a]]
shrinkOne sh xs =
  [ take i xs ++ [x'] ++ drop (i+1) xs
  | (i, x) <- zip [0..] xs, x' <- sh x ]

shrinkFnBranches :: FnDef -> [FnDef]
shrinkFnBranches f@(FnDef n as b) =
  [ FnDef n as b' | b' <- shrinkExpr b ]

shrinkExpr :: Expr -> [Expr]
shrinkExpr (ECase sc brs) =
  [ ECase sc brs' | brs' <- shrinkList (const []) brs, not (null brs') ]
shrinkExpr (EAdd a b) = [a, b]
shrinkExpr _          = []

shrinkMain :: MainExpr -> [MainExpr]
shrinkMain (MainExpr fn args) =
  [ MainExpr fn args' | args' <- shrinkOne shrinkArg args ]
  where
    shrinkArg (EInt n) = [ EInt n' | n' <- [0, n `div` 2], n' /= n ]
    shrinkArg _        = []

-- =============================================================================
-- Differential runner
-- =============================================================================

data DiffResult
  = Agree
  | Diverge !BS.ByteString !BS.ByteString  -- host MLIR, stage1 MLIR
  | HostFailed !String
  | Stage1Failed !String
  | OrganFailed !String
  deriving Show

data Mode = HostVsStage1 | Stage1VsStage2 | Stage2VsStage3 | HostRuntimeVsStage2Runtime
  deriving (Eq, Show)

parseMode :: String -> Maybe Mode
parseMode "host-vs-stage1"             = Just HostVsStage1
parseMode "stage1-vs-stage2"           = Just Stage1VsStage2
parseMode "stage2-vs-stage3"           = Just Stage2VsStage3
parseMode "host-runtime-vs-stage2-runtime" = Just HostRuntimeVsStage2Runtime
parseMode _                            = Nothing

data Tools = Tools
  { toolFrkBin    :: FilePath
  , toolStage1Bin :: FilePath
  , toolStage2Bin :: Maybe FilePath
  , toolStage3Bin :: Maybe FilePath
  } deriving Show

discoverTools :: IO Tools
discoverTools = do
  (xc, bin, ce) <- readProcessWithExitCode "cabal-3.16.1.0"
                     ["-v0", "list-bin", "-w", "/usr/lib64/ghc-9.14.1/bin/ghc",
                      "frankenstein"] ""
  when (xc /= ExitSuccess) $ do
    hPutStrLn stderr "cabal list-bin exited non-zero"
    hPutStrLn stderr ce
    exitFailure
  let frk = head (filter (not . null) (lines bin))
  okF <- doesFileExist frk
  unless okF $ do
    hPutStrLn stderr ("frankenstein binary not found at: " ++ frk)
    exitFailure
  let stage1 = "self-host/frankenstein-self-compiler"
      stage2 = "self-host/frankenstein-self-compiler-stage2"
      stage3 = "self-host/frankenstein-self-compiler-stage3"
  okS1 <- doesFileExist stage1
  unless okS1 $ do
    hPutStrLn stderr ("stage 1 binary missing: " ++ stage1)
    exitFailure
  okS2 <- doesFileExist stage2
  okS3 <- doesFileExist stage3
  return $ Tools frk stage1
    (if okS2 then Just stage2 else Nothing)
    (if okS3 then Just stage3 else Nothing)

-- A compile step: take an .hs file (already on disk) and return MLIR bytes.
type CompileStep = Tools -> FilePath -> FilePath -> IO (Either String BS.ByteString)

hostMlirDirect :: CompileStep
hostMlirDirect t _dir src = do
  (xh, hOut, hErr) <- readProcessWithExitCode (toolFrkBin t)
    [src, "--no-simplify", "--evidence=plotkin", "--emit-mlir"] ""
  return (if xh == ExitSuccess then Right (BSC.pack hOut)
                               else Left (hErr ++ "\n" ++ hOut))

-- Emit organ.json once for src, return its contents.
emitOrgan :: Tools -> FilePath -> IO (Either String String)
emitOrgan t src = do
  (xo, oOut, oErr) <- readProcessWithExitCode (toolFrkBin t)
    [src, "--no-simplify", "--emit-organ"] ""
  return (if xo == ExitSuccess then Right oOut
                               else Left (oErr ++ "\n" ++ oOut))

-- Match the bootstrap's invocation shape: pass the JSON as a file path and add
-- --no-perceus, exactly as self-host/build.sh does.  Stdin input + missing flag
-- produces a different code path (we verified output is identical, but match
-- bootstrap form so reported divergences reflect the bootstrap question).
stageEmit :: FilePath -> Tools -> String -> FilePath -> IO (Either String BS.ByteString)
stageEmit bin _t organJson dir = do
  let jsonPath = dir </> "input.organ.json"
      out      = dir </> "stage.mlir"
  writeFile jsonPath organJson
  (xs, sOut, sErr) <- readProcessWithExitCode bin
    [jsonPath, "--no-perceus", "-o", out] ""
  if xs /= ExitSuccess
    then return (Left (sErr ++ "\n" ++ sOut))
    else do
      b <- BS.readFile out
      return (Right b)

runDifferential :: Mode -> Tools -> Prog -> IO DiffResult
runDifferential mode tools prog =
  withSystemTempDirectory "frkdiff" $ \dir -> do
    let modName = "Repro"
        src     = dir </> modName <.> "hs"
    writeFile src (prettyProg modName prog)
    case mode of
      HostVsStage1 -> do
        a <- hostMlirDirect tools dir src
        case a of
          Left e -> return (HostFailed e)
          Right ha -> do
            o <- emitOrgan tools src
            case o of
              Left e -> return (OrganFailed e)
              Right json -> do
                b <- stageEmit (toolStage1Bin tools) tools json dir
                case b of
                  Left e -> return (Stage1Failed e)
                  Right sb -> return (if ha == sb then Agree else Diverge ha sb)
      Stage1VsStage2 -> compareTwoStages tools dir src
                          (toolStage1Bin tools) (must "stage2" (toolStage2Bin tools))
      Stage2VsStage3 -> compareTwoStages tools dir src
                          (must "stage2" (toolStage2Bin tools))
                          (must "stage3" (toolStage3Bin tools))
      HostRuntimeVsStage2Runtime -> compareRuntime tools dir src
                                      (must "stage2" (toolStage2Bin tools))
  where
    must lbl Nothing  = error ("required " ++ lbl ++ " binary not found")
    must _   (Just p) = p

-- Runtime mode: compile and run the program via host and via stage 2; compare
-- exit codes (since generated programs have main :: Int and the frankenstein
-- runtime maps that to the process exit code).  Differs from the MLIR-equality
-- modes because it catches semantic miscompilations regardless of MLIR-level
-- divergence — the right test for chasing the pattern-match dispatch bug.
compareRuntime :: Tools -> FilePath -> FilePath -> FilePath -> IO DiffResult
compareRuntime tools dir src s2Bin = do
  -- Host: produce a native binary via --compile.
  let hostBin = dir </> "host-bin"
  (xh, hOut, hErr) <- readProcessWithExitCode (toolFrkBin tools)
    [src, "--no-simplify", "--compile", "-o", hostBin] ""
  if xh /= ExitSuccess
    then return (HostFailed (hErr ++ "\n" ++ hOut))
    else do
      hostOk <- doesFileExist hostBin
      if not hostOk
        then return (HostFailed "host --compile did not produce a binary")
        else do
          (xhr, _, _) <- readProcessWithExitCode hostBin [] ""
          let hostExit = case xhr of
                ExitSuccess   -> 0
                ExitFailure n -> n
          -- Self-host stage 2: emit organ -> compile via s2 -> postprocess -> mlir-opt + clang.
          o <- emitOrgan tools src
          case o of
            Left e -> return (OrganFailed e)
            Right json -> do
              let jsonPath = dir </> "input.organ.json"
                  s2Mlir   = dir </> "s2.mlir"
                  s2Ll     = dir </> "s2.ll"
                  s2Obj    = dir </> "s2.o"
                  s2Bin'   = dir </> "s2-bin"
              writeFile jsonPath json
              (xs, sOut, sErr) <- readProcessWithExitCode s2Bin
                [jsonPath, "--no-perceus", "-o", s2Mlir] ""
              if xs /= ExitSuccess
                then return (Stage1Failed ("s2 emit: " ++ sErr ++ "\n" ++ sOut))
                else do
                  (xp, _, pErr) <- readProcessWithExitCode (toolFrkBin tools)
                    ["--postprocess-mlir", s2Mlir] ""
                  if xp /= ExitSuccess
                    then return (Stage1Failed ("postprocess: " ++ pErr))
                    else do
                      -- mlir-opt | mlir-translate -> s2.ll
                      let optArgs = ["--allow-unregistered-dialect"
                                    ,"--convert-scf-to-cf"
                                    ,"--convert-arith-to-llvm"
                                    ,"--convert-cf-to-llvm"
                                    ,"--convert-func-to-llvm"
                                    ,"--reconcile-unrealized-casts"
                                    , s2Mlir]
                      (xo, optOut, optErr) <- readProcessWithExitCode "mlir-opt" optArgs ""
                      if xo /= ExitSuccess
                        then return (Stage1Failed ("mlir-opt: " ++ optErr))
                        else do
                          (xt, llOut, tErr) <- readProcessWithExitCode
                            "mlir-translate" ["--mlir-to-llvmir"] optOut
                          if xt /= ExitSuccess
                            then return (Stage1Failed ("mlir-translate: " ++ tErr))
                            else do
                              writeFile s2Ll llOut
                              (xc, _, cErr) <- readProcessWithExitCode "clang"
                                ["-c", "-o", s2Obj, s2Ll] ""
                              if xc /= ExitSuccess
                                then return (Stage1Failed ("clang -c: " ++ cErr))
                                else do
                                  (xl, _, lErr) <- readProcessWithExitCode "clang"
                                    ["-o", s2Bin', s2Obj
                                    , "self-host/obj/kk_rt_standalone.o"
                                    , "self-host/obj/kk_arena_standalone.o"
                                    , "self-host/obj/kk_cycle_standalone.o"
                                    , "-lm"] ""
                                  if xl /= ExitSuccess
                                    then return (Stage1Failed ("clang link: " ++ lErr))
                                    else do
                                      (xrr, _, _) <- readProcessWithExitCode s2Bin' [] ""
                                      let s2Exit = case xrr of
                                            ExitSuccess   -> 0
                                            ExitFailure n -> n
                                          tag :: BS.ByteString -> BS.ByteString -> DiffResult
                                          tag h s = if h == s then Agree else Diverge h s
                                      return (tag (BSC.pack (show hostExit))
                                                  (BSC.pack (show s2Exit)))

compareTwoStages :: Tools -> FilePath -> FilePath -> FilePath -> FilePath
                 -> IO DiffResult
compareTwoStages tools dir src binA binB = do
  o <- emitOrgan tools src
  case o of
    Left e -> return (OrganFailed e)
    Right json -> do
      ra <- stageEmit binA tools json dir
      case ra of
        Left e -> return (Stage1Failed ("[A] " ++ e))
        Right ab -> do
          -- second emit goes in a fresh sub-dir so the .mlir paths don't clash
          createDirectoryIfMissing True (dir </> "b")
          rb <- stageEmit binB tools json (dir </> "b")
          case rb of
            Left e -> return (Stage1Failed ("[B] " ++ e))
            Right bb -> return (if ab == bb then Agree else Diverge ab bb)

-- =============================================================================
-- Driver
-- =============================================================================

data Stats = Stats
  { sAgree         :: !Int
  , sDiverge       :: !Int
  , sHostFailed    :: !Int
  , sOrganFailed   :: !Int
  , sStage1Failed  :: !Int
  } deriving Show

emptyStats :: Stats
emptyStats = Stats 0 0 0 0 0

-- A small fingerprint of a divergence so we can cluster.
-- We normalize the obvious noise (SSA numbers, Koka name mangling, comment lines)
-- so different programs that differ in the same WAY cluster together.
diffFingerprint :: BS.ByteString -> BS.ByteString -> String
diffFingerprint h s =
  let hl = map normalizeLine (filter (not . BSC.null) (BSC.lines h))
      sl = map normalizeLine (filter (not . BSC.null) (BSC.lines s))
      pairs = zip hl sl
      (i, mismatch) = case dropWhile (\(_, (a,b)) -> a == b)
                                     (zip [(0::Int)..] pairs) of
        []           -> (-1, "")
        ((j, (a,b)):_) -> (j, BSC.unpack a ++ "  <vs>  " ++ BSC.unpack b)
  in classify mismatch ++ " | line " ++ show i ++ ": " ++ take 240 mismatch
  where
    classify m
      | "%evv_p" `isInfixOfStr` m       = "[evv-param-shape]"
      | "// External"     `isInfixOfStr` m = "[external-imports]"
      | "scf.if"          `isInfixOfStr` m = "[scf.if-mismatch]"
      | "func.func"       `isInfixOfStr` m = "[func-signature]"
      | "func.call"       `isInfixOfStr` m = "[func-call-shape]"
      | "llvm.call"       `isInfixOfStr` m = "[llvm-call-shape]"
      | "arith."          `isInfixOfStr` m = "[arith-shape]"
      | "memref."         `isInfixOfStr` m = "[memref-shape]"
      | otherwise                          = "[other]"

isInfixOfStr :: String -> String -> Bool
isInfixOfStr needle = go
  where
    go hay | length hay < length needle = False
           | needle `isPrefixOf` hay    = True
           | otherwise                  = go (drop 1 hay)

-- | Classify a failure message so cluster fingerprints stay short and
--   semantic.  The compiler-tool exit paths produce wildly different
--   error text (mlir-opt diagnostics, clang link errors, GHC syntax
--   errors); we pull out a 1-line category and trim.
classifyFail :: String -> String
classifyFail err =
  let firstLine = take 1 (lines err)
      stem = case firstLine of
        []    -> "(empty)"
        (l:_) -> l
      cat
        | "mlir-opt"        `isInfixOfStr` err = "mlir-opt"
        | "mlir-translate"  `isInfixOfStr` err = "mlir-translate"
        | "clang -c"        `isInfixOfStr` err = "clang-compile"
        | "clang link"      `isInfixOfStr` err = "clang-link"
        | "use of undeclared SSA value name" `isInfixOfStr` err = "undeclared-SSA"
        | "reference to undefined function"  `isInfixOfStr` err = "undefined-func"
        | "reference to function with mismatched type" `isInfixOfStr` err = "type-mismatch"
        | "s2 emit"         `isInfixOfStr` err = "s2-emit"
        | "postprocess"     `isInfixOfStr` err = "postprocess"
        | otherwise = "other"
  in cat ++ ": " ++ take 120 stem

-- Strip per-program-unique noise from a line so that semantically-identical
-- diffs in different programs share a fingerprint.
normalizeLine :: BS.ByteString -> BS.ByteString
normalizeLine = BSC.pack . normSSA . BSC.unpack
  where
    -- Replace Koka SSA digit suffixes `_zd<digits>(<digits>)*` -> `_zd*`
    normSSA []      = []
    normSSA ('_':'z':'d':rest)
      | (_, rest') <- span (`elem` digits) rest = "_zd*" ++ normSSA rest'
    -- Also flatten bare numeric chunks longer than 3 digits (SSA values, line
    -- counters, etc.) to a single token "<N>".
    normSSA s@(c:_) | c `elem` digits =
      let (ds, rest) = span (`elem` digits) s
      in if length ds >= 4 then "<N>" ++ normSSA rest
                           else ds   ++ normSSA rest
    normSSA (c:cs) = c : normSSA cs
    digits = "0123456789" :: String

runBatch :: Mode -> Tools -> Int -> Int -> IO ()
runBatch mode tools n seedBase = do
  stats <- newIORef emptyStats
  divs  <- newIORef (Map.empty :: Map String (Int, Prog))
  hSetBuffering stdout NoBuffering
  putStrLn ("=== Generating " ++ show n ++ " programs (mode " ++ show mode
            ++ ", seed " ++ show seedBase ++ ") ===")
  forM_ [0 .. n-1] $ \i -> do
    let seed = seedBase + i
    let prog = QC.unGen QC.arbitrary (QC.mkQCGen seed) 6
    putStr ("." :: String)
    when (i `mod` 50 == 49) (putStrLn (" " ++ show (i+1)))
    res <- runDifferential mode tools prog
            `catch` \(e :: SomeException) ->
              return (HostFailed ("exception: " ++ show e))
    case res of
      Agree -> modifyIORef' stats (\s -> s { sAgree = sAgree s + 1 })
      HostFailed e    -> do
        modifyIORef' stats (\s -> s { sHostFailed   = sHostFailed s + 1 })
        let fp = "[host-failed] " ++ classifyFail e
        modifyIORef' divs (Map.insertWith
          (\(_, np) (k, op) -> (k + 1, if progSize np < progSize op then np else op))
          fp (1, prog))
      OrganFailed _   -> modifyIORef' stats (\s -> s { sOrganFailed  = sOrganFailed s + 1 })
      Stage1Failed e  -> do
        modifyIORef' stats (\s -> s { sStage1Failed = sStage1Failed s + 1 })
        let fp = "[stage1-failed] " ++ classifyFail e
        modifyIORef' divs (Map.insertWith
          (\(_, np) (k, op) -> (k + 1, if progSize np < progSize op then np else op))
          fp (1, prog))
      Diverge h s -> do
        modifyIORef' stats (\st -> st { sDiverge = sDiverge st + 1 })
        let fp = diffFingerprint h s
        modifyIORef' divs (Map.insertWith
          (\(_, np) (k, op) -> (k + 1, if progSize np < progSize op then np else op))
          fp (1, prog))
  putStrLn ""
  finalStats <- readIORef stats
  finalDivs  <- readIORef divs
  reportResults finalStats finalDivs

progSize :: Prog -> Int
progSize (Prog ds fns _) =
  length ds + sum [length (ddCtors d) | d <- ds]
  + length fns + sum [exprSize (fnBody f) | f <- fns]

exprSize :: Expr -> Int
exprSize = \case
  EInt _       -> 1
  EVar _       -> 1
  EAdd a b     -> 1 + exprSize a + exprSize b
  ECtor _ as   -> 1 + sum (map exprSize as)
  ECase _ brs  -> 1 + sum [exprSize e | Branch _ e <- brs]

reportResults :: Stats -> Map String (Int, Prog) -> IO ()
reportResults st divs = do
  putStrLn ""
  putStrLn "=== Summary ==="
  putStrLn ("  Agree:           " ++ show (sAgree st))
  putStrLn ("  Diverge:         " ++ show (sDiverge st))
  putStrLn ("  Host failed:     " ++ show (sHostFailed st))
  putStrLn ("  Organ failed:    " ++ show (sOrganFailed st))
  putStrLn ("  Stage 1 failed:  " ++ show (sStage1Failed st))
  when (not (Map.null divs)) $ do
    putStrLn ""
    putStrLn "=== Divergence clusters (top 10 by count) ==="
    let ranked = take 10 $ sortBy (comparing (Down . fst . snd)) (Map.toList divs)
    forM_ (zip [1::Int ..] ranked) $ \(rank, (fp, (count, repProg))) -> do
      putStrLn ("--- #" ++ show rank ++ " (count " ++ show count ++ ", size "
                ++ show (progSize repProg) ++ ") ---")
      putStrLn ("  fingerprint: " ++ fp)
      putStrLn "  representative program:"
      mapM_ (putStrLn . ("    " ++)) (lines (prettyProg "Repro" repProg))

-- Try to shrink a known-divergent program to a minimum.
shrinkDivergent :: Mode -> Tools -> Prog -> IO Prog
shrinkDivergent mode tools p0 = go p0
  where
    go p = do
      let cands = shrinkProg p
      mNext <- tryCands cands
      case mNext of
        Just p' -> go p'
        Nothing -> return p
    tryCands [] = return Nothing
    tryCands (c:cs) = do
      r <- runDifferential mode tools c
              `catch` \(_ :: SomeException) -> return Agree
      case r of
        Diverge _ _ -> return (Just c)
        _           -> tryCands cs

-- =============================================================================
-- main
-- =============================================================================

data Args = Args
  { argN     :: Int
  , argSeed  :: Int
  , argMode  :: Mode
  } deriving Show

parseArgs :: [String] -> Args
parseArgs = foldl step (Args 100 1 HostVsStage1)
  where
    step a ('-':'-':'m':'o':'d':'e':'=':v) =
      case parseMode v of
        Just m  -> a { argMode = m }
        Nothing -> error ("unknown mode: " ++ v
                          ++ " (expected host-vs-stage1 | stage1-vs-stage2 | stage2-vs-stage3)")
    step a ('-':'-':'s':'e':'e':'d':'=':v) = a { argSeed = read v }
    step a ('-':'-':'n':'=':v)             = a { argN    = read v }
    step a s | all (`elem` (['0'..'9'] :: String)) s && not (null s) =
        if argN a == 100 then a { argN = read s }
                         else a { argSeed = read s }
    step _ s = error ("unknown arg: " ++ s)

main :: IO ()
main = do
  raw <- getArgs
  let a = parseArgs raw
  tools <- discoverTools
  putStrLn ("Frankenstein binary:  " ++ toolFrkBin tools)
  putStrLn ("Stage 1 binary:       " ++ toolStage1Bin tools)
  case toolStage2Bin tools of
    Just p  -> putStrLn ("Stage 2 binary:       " ++ p)
    Nothing -> putStrLn  "Stage 2 binary:       (not built)"
  case toolStage3Bin tools of
    Just p  -> putStrLn ("Stage 3 binary:       " ++ p)
    Nothing -> putStrLn  "Stage 3 binary:       (not built)"
  case argMode a of
    Stage1VsStage2 | toolStage2Bin tools == Nothing ->
      error "stage1-vs-stage2 requested but stage 2 binary missing"
    Stage2VsStage3 | toolStage2Bin tools == Nothing || toolStage3Bin tools == Nothing ->
      error "stage2-vs-stage3 requested but stage 2 or stage 3 binary missing"
    HostRuntimeVsStage2Runtime | toolStage2Bin tools == Nothing ->
      error "host-runtime-vs-stage2-runtime requested but stage 2 binary missing"
    _ -> return ()
  runBatch (argMode a) tools (argN a) (argSeed a)
