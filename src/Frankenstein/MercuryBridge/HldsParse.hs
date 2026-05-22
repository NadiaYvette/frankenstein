-- | Mercury HLDS Parser
--
-- Parses the textual HLDS dump produced by `mmc --dump-hlds 50`.
-- The HLDS contains Mercury programs after type/mode/determinism checking.

module Frankenstein.MercuryBridge.HldsParse
  ( MercuryHLDS(..)
  , MercuryPred(..)
  , MercuryTypeDecl(..)
  , MercuryMode(..)
  , MercuryDet(..)
  , MercuryGoal(..)
  , parseHldsDump
  , dumpHlds
  , dumpHldsProgram
  ) where

import Control.Monad (filterM)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readCreateProcessWithExitCode, proc, cwd)
import System.Exit (ExitCode(..))
import System.Directory (listDirectory, getTemporaryDirectory, createDirectoryIfMissing, makeAbsolute, copyFile, removeDirectoryRecursive, doesFileExist, doesDirectoryExist)
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))
import Data.List (isPrefixOf, find)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Control.Exception (try, catch, IOException)

-- Mercury determinism categories
data MercuryDet
  = Det | Semidet | Multi | Nondet | Failure | Erroneous | CCMulti | CCNondet
  deriving (Show, Eq)

-- Mercury mode
data MercuryMode
  = ModeIn | ModeOut | ModeDi | ModeUo | ModeUnused
  deriving (Show, Eq)

-- A Mercury predicate in HLDS
data MercuryPred = MercuryPred
  { predName       :: !Text
  , predArity      :: !Int
  , predDet        :: !MercuryDet
  , predModes      :: ![MercuryMode]
  , predArgTypes   :: ![Text]
  , predGoal       :: !(Maybe MercuryGoal)
  , predArgNames   :: ![Text]
  } deriving (Show)

-- HLDS goal representation
data MercuryGoal
  = GoalUnify Text Text                           -- X = Y (assignment/test)
  | GoalCall Text [Text]                           -- predicate call
  | GoalConj [MercuryGoal]                         -- (A, B, C)
  | GoalDisj [MercuryGoal]                         -- (A ; B)
  | GoalNot MercuryGoal                            -- not(G)
  | GoalIfThenElse MercuryGoal MercuryGoal MercuryGoal
  | GoalSwitch Text [(Text, MercuryGoal)]          -- switch on variable
  | GoalConstruct Text Text [Text]                 -- Var = functor(Args)
  | GoalDeconstruct Text Text [Text]               -- functor(Args) = Var
  | GoalLambda Text [Text] (Maybe Text) MercuryGoal
    -- ^ LHS = (pred|func(params) :- body).  Binds LHS to a closure
    -- value.  Last field is the optional output-variable name for
    -- the func form (which yields the value bound to that var).
  | GoalForeign Text
  | GoalUnparsed Text
  deriving (Show)

-- A Mercury type declaration in HLDS
data MercuryTypeDecl = MercuryTypeDecl
  { typeDeclName   :: !Text
  , typeDeclParams :: ![Text]
  , typeDeclCtors  :: ![(Text, [Text])]  -- (ctor name, [field type strings])
  } deriving (Show)

data MercuryHLDS = MercuryHLDS
  { hldsModule :: !Text
  , hldsPreds  :: ![MercuryPred]
  , hldsTypes  :: ![MercuryTypeDecl]
  } deriving (Show)

-- | Invoke mmc to dump HLDS for a Mercury source file.
-- mmc dumps files into the current directory, so we copy the source into
-- a temp directory, run mmc there, and look for the .hlds_dump file.
dumpHlds :: FilePath -> IO (Either Text Text)
dumpHlds inputPath = do
  absPath <- makeAbsolute inputPath
  let moduleName = takeBaseName inputPath
  tmpDir <- getTemporaryDirectory
  let workDir = tmpDir </> "frankenstein-mercury-" ++ moduleName
  -- Clean any stale files from previous runs (mmc reuses .o/.c files)
  removeDirectoryRecursive workDir `catch` (\(_ :: IOException) -> pure ())
  createDirectoryIfMissing True workDir
  -- Copy source file to temp working directory
  copyFile absPath (workDir </> takeFileName inputPath)
  result <- try $ readCreateProcessWithExitCode
    (proc "mmc" [ "--dump-hlds", "50", "--compile-only", takeFileName inputPath ])
      { cwd = Just workDir }
    ""
  case result :: Either IOException (ExitCode, String, String) of
    Left exc -> pure $ Left $ T.pack $ "Failed to invoke mmc: " ++ show exc
    Right (exitCode, _, stderr) -> do
      -- Find the dump file in the work directory.
      -- mmc may exit with failure (e.g. link error for library modules)
      -- but still generate the HLDS dump file, which is all we need.
      files <- listDirectory workDir
      let dumpFile = find (\f -> (moduleName ++ ".hlds_dump") `isPrefixOf` f) files
      case dumpFile of
        Just f -> do
          contents <- TIO.readFile (workDir </> f)
          pure $ Right contents
        Nothing -> case exitCode of
          ExitFailure code ->
            pure $ Left $ T.pack $ "mmc failed (exit " ++ show code ++ "): " ++ stderr
          ExitSuccess ->
            pure $ Left $ "HLDS dump file not found after mmc in " <> T.pack workDir

-- | Discover transitive user-module imports starting from a Mercury source
-- file, then dump HLDS for each.  "User module" = a module whose .m file
-- lives in the same directory as 'inputPath'.  Stdlib (io, list, string,
-- integer, …) is left out and resolved at link time via runtime stubs.
--
-- Returns a list of (module-name, hlds-dump-text), with the entry module
-- first so 'translateMultiHlds' can pick it for progName / main detection.
dumpHldsProgram :: FilePath -> IO (Either Text [(Text, Text)])
dumpHldsProgram inputPath = do
  absPath <- makeAbsolute inputPath
  let srcDir   = takeDirectory absPath
      entryMod = T.pack (takeBaseName absPath)
  -- BFS: collect transitively-imported user modules (files that exist in srcDir).
  userMods <- bfsUserImports srcDir entryMod
  -- Shared workdir so mmc can resolve cross-module imports during type-check.
  tmpDir <- getTemporaryDirectory
  let workDir = tmpDir </> "frankenstein-mercury-prog-" ++ T.unpack entryMod
  removeDirectoryRecursive workDir `catch` (\(_ :: IOException) -> pure ())
  createDirectoryIfMissing True workDir
  mapM_ (\m -> copyFile (srcDir </> T.unpack m ++ ".m")
                        (workDir </> T.unpack m ++ ".m"))
        userMods
  -- mmc needs pre-built `.int` interfaces for cross-module type-check.
  -- A simple compile-from-source `mmc --compile-only` chain works for
  -- 1–2 imports (mmc auto-compiles direct siblings), but breaks on
  -- deeper transitive chains like surd's euler_integrate → poly →
  -- rational.  surd's `mmc --make demo_*` already populated
  -- `<srcDir>/Mercury/ints/` with .int files — point mmc at them via
  -- `-I` so the dump pass finds them without having to regenerate.
  let candidateDirs = [ srcDir </> "Mercury" </> "ints"
                      , srcDir </> "Mercury" </> "int2s"
                      , srcDir </> "Mercury" </> "int3s"
                      ]
  extraSearch <- filterM doesDirectoryExist candidateDirs
  -- Dump HLDS for each module in the shared workDir.
  dumps <- mapM (\m -> do
                    d <- dumpInWorkDirWith workDir extraSearch
                           (T.unpack m ++ ".m")
                    pure $ fmap (\t -> (m, t)) d)
                userMods
  case sequence dumps of
    Left err -> pure $ Left err
    Right rs -> pure $ Right rs

-- BFS user-module imports.  Only follows imports whose corresponding .m
-- file exists in 'srcDir' — stdlib references stop the walk.  Modules in
-- 'opaqueModules' are skipped even when a .m sibling exists (e.g. surd
-- vendors Mercury's stdlib 'integer.m'; we'd rather substitute runtime
-- stubs than parse its 2000+ line HLDS).
bfsUserImports :: FilePath -> Text -> IO [Text]
bfsUserImports srcDir start = go [start] (Set.singleton start) [start]
  where
    go []     _    acc = pure (reverse acc)
    go (m:ms) seen acc = do
      let p = srcDir </> T.unpack m ++ ".m"
      exists <- doesFileExist p
      if not exists
        then go ms seen acc
        else do
          imps <- readImports p
          let new = [i | i <- imps
                       , not (Set.member i seen)
                       , not (Set.member i opaqueModules)]
          newUser <- filterM (\i -> doesFileExist (srcDir </> T.unpack i ++ ".m")) new
          let seen' = foldr Set.insert seen newUser
              acc'  = newUser ++ acc
          go (ms ++ newUser) seen' acc'

-- | Mercury stdlib modules we always skip in HLDS aggregation, even when
-- a same-named .m file exists alongside the input (surd vendors several
-- of these).  These resolve to Frankenstein runtime stubs at link time —
-- typically lossy (e.g. arbitrary-precision 'integer' becomes plain i64),
-- but correct enough for first-pass bring-up.
opaqueModules :: Set.Set Text
opaqueModules = Set.fromList
  [ "integer"   -- vendored in surd-mercury; substitute with i64
  , "io"        -- runtime print_str / println_str
  , "list"      -- runtime kk_list_*
  , "string"    -- runtime str_*
  , "int"       -- builtin
  , "char"      -- builtin
  , "bool"      -- builtin
  , "maybe"     -- builtin
  , "require"   -- runtime abort
  , "exception" -- runtime _raise
  , "math"      -- libm via runtime
  , "float"     -- builtin
  ]

-- Parse ":- import_module foo." lines from a Mercury source file.
readImports :: FilePath -> IO [Text]
readImports p = do
  txt <- TIO.readFile p
  pure
    [ T.strip (T.dropEnd 1 mod_)  -- drop trailing "."
    | l <- T.lines txt
    , let stripped = T.strip l
    , Just rest <- [T.stripPrefix ":- import_module " stripped]
    , let mod_ = rest
    , not (T.null mod_)
    , T.last mod_ == '.'
    ]

-- Run mmc in an existing shared workDir; do not clean (caller owns it).
dumpInWorkDir :: FilePath -> FilePath -> IO (Either Text Text)
dumpInWorkDir workDir fileBase = dumpInWorkDirWith workDir [] fileBase

-- | Variant that also feeds extra `-I <dir>` search paths to mmc.
-- Used when a multi-module program needs pre-built `.int` files that
-- live alongside the entry source (e.g. surd's `Mercury/ints/`).
dumpInWorkDirWith :: FilePath -> [FilePath] -> FilePath -> IO (Either Text Text)
dumpInWorkDirWith workDir extraSearchDirs fileBase = do
  let moduleName = takeBaseName fileBase
      iFlags = concatMap (\d -> ["-I", d]) extraSearchDirs
  result <- try $ readCreateProcessWithExitCode
    (proc "mmc" (iFlags ++ [ "--dump-hlds", "50", "--compile-only", fileBase ]))
      { cwd = Just workDir }
    ""
  case result :: Either IOException (ExitCode, String, String) of
    Left exc -> pure $ Left $ T.pack $ "Failed to invoke mmc: " ++ show exc
    Right (exitCode, _, stderr) -> do
      files <- listDirectory workDir
      let dumpFile = find (\f -> (moduleName ++ ".hlds_dump") `isPrefixOf` f) files
      case dumpFile of
        Just f -> Right <$> TIO.readFile (workDir </> f)
        Nothing -> case exitCode of
          ExitFailure code ->
            pure $ Left $ T.pack $ "mmc failed on " ++ moduleName
                                ++ " (exit " ++ show code ++ "): " ++ stderr
          ExitSuccess ->
            pure $ Left $ "HLDS dump file not found for " <> T.pack moduleName

-- | Parse a textual HLDS dump into structured form.
parseHldsDump :: Text -> Either Text MercuryHLDS
parseHldsDump dumpText = do
  let ls = T.lines dumpText
      -- Extract module name from ":- module X."
      modName = case filter (":- module " `T.isPrefixOf`) ls of
        (l:_) -> T.strip $ T.dropEnd 1 $ T.drop (T.length ":- module ") l
        [] -> "unknown"
      -- Split into predicate blocks at "% pred id" markers
      preds = extractPredicates ls
      -- Extract type declarations from "% type ctor:" markers
      types = extractTypeDecls ls
  Right $ MercuryHLDS
    { hldsModule = modName
    , hldsPreds = preds
    , hldsTypes = types
    }

extractPredicates :: [Text] -> [MercuryPred]
extractPredicates [] = []
extractPredicates (l:ls)
  | "  % pred id " `T.isPrefixOf` l =
      -- Collect lines until next pred id
      let (block, rest) = span (\x -> not ("  % pred id " `T.isPrefixOf` x)) ls
      in if isStdlibPredHeader l
         then extractPredicates rest  -- skip Mercury stdlib preds
         else parsePredBlock l block : extractPredicates rest
  | otherwise = extractPredicates ls

-- | Recognise pred headers belonging to Mercury stdlib modules.  mmc's
-- HLDS dump for a user module includes pred blocks for imported stdlib
-- predicates that the user module references (for type-check context);
-- translating those would re-emit list.sort, list.merge, term.var, etc.
-- as user defs with mismatched names.  Skip them — runtime stubs cover
-- whatever the bridge actually needs to call into.
isStdlibPredHeader :: Text -> Bool
isStdlibPredHeader headerLine =
  case T.breakOn "`" headerLine of
    (_, rest) | not (T.null rest) ->
      let qname = T.takeWhile (/= '\'') (T.drop 1 rest)
          modName = case T.breakOnEnd "." qname of
            ("", _)   -> ""
            (m, _)    -> T.dropEnd 1 m  -- strip trailing "."
      in Set.member modName stdlibModules
    _ -> False

-- Mercury stdlib modules whose HLDS contributions we always skip.
-- Aligned with 'opaqueModules' (which controls BFS), plus a few
-- mmc-internal module names that surface in HLDS dumps but don't
-- correspond to anything a user can import.
stdlibModules :: Set.Set Text
stdlibModules = Set.fromList
  [ "integer", "list", "string", "io", "int", "char", "bool", "maybe"
  , "require", "exception", "math", "float", "array", "map", "set"
  , "tree234", "assoc_list", "pair", "ordering", "type_desc"
  , "term", "varset", "term_io"   -- term library (used internally by mmc)
  , "builtin", "private_builtin", "table_builtin", "type_info_hlds"
  , "rtti_implementation", "string.format", "stream"
  ]

parsePredBlock :: Text -> [Text] -> MercuryPred
parsePredBlock headerLine bodyLines =
  let -- Header: "  % pred id N: predicate `module.name'/arity"
      -- or "  % pred id N: a compiler-transformed version of predicate `module.name'/arity"
      header = T.strip headerLine
      -- Extract name and arity
      (name, arity) = extractNameArity header

      -- Find mode declaration: ":- mode name(modes) is det."
      modeLines = filter (\l -> ":- mode " `T.isPrefixOf` T.stripStart l) bodyLines
      (modes, det) = case modeLines of
        (ml:_) -> parseModeDecl ml
        [] -> ([], Det)

      -- Extract argument names from clause header: "module.name(Arg1, Arg2, ...) :-"
      argNames = extractArgNames bodyLines

      -- Parse the goal body (everything after the ":-" in the clause)
      goalText = extractGoalText bodyLines
      rawGoal  = parseGoalText goalText
      -- Rename every lambda LHS in this pred's goal to a per-pred
      -- unique name (prefix with the pred name and arity).  Without
      -- this, two preds that both bind a local lambda to e.g. `V_8`
      -- collide on the emitter's promote-to-top-level path
      -- (qualifyBindName hashes the source name + nameUnique, both 0
      -- for every bridge-built Name).  Renaming the lambda LHS AND
      -- every later reference to it inside the same pred body keeps
      -- the local scoping correct.
      uniqSuffix = "__" <> name <> "_" <> T.pack (show arity)
      goal = renameLambdaLhses uniqSuffix rawGoal

  in MercuryPred
    { predName = name
    , predArity = arity
    , predDet = det
    , predModes = modes
    , predArgTypes = []
    , predGoal = Just goal
    , predArgNames = argNames
    }

-- | Rewrite every @GoalLambda@ LHS in the given goal AST so its name
-- has a per-pred-unique suffix appended.  Also rewrites references to
-- that name in subsequent goals within the same conjunction so
-- bindings and uses stay coherent.  The suffix is built from the
-- pred's @name@ + @arity@ in 'parsePredBlock', so distinct preds
-- produce distinct lifted-lambda symbols downstream.
renameLambdaLhses :: Text -> MercuryGoal -> MercuryGoal
renameLambdaLhses suffix = goTop Set.empty
  where
    rn renamed v = if Set.member v renamed then v <> suffix else v
    goTop renamed g = case g of
      GoalLambda lhs ps mOut body ->
        let renamed' = Set.insert lhs renamed
            lhs' = lhs <> suffix
            body' = goTop renamed body  -- params shadow inside body
        in GoalLambda lhs' ps mOut body'
      GoalConj gs -> GoalConj (snd (foldl step (renamed, []) gs))
      GoalDisj gs -> GoalDisj (map (goTop renamed) gs)
      GoalNot inner -> GoalNot (goTop renamed inner)
      GoalIfThenElse c t e ->
        GoalIfThenElse (goTop renamed c) (goTop renamed t) (goTop renamed e)
      GoalSwitch v cases ->
        GoalSwitch (rn renamed v)
                   [(c, goTop renamed b) | (c, b) <- cases]
      GoalUnify x y -> GoalUnify (rn renamed x) (rn renamed y)
      GoalCall p args -> GoalCall p (map (rn renamed) args)
      GoalConstruct v ctor args ->
        GoalConstruct (rn renamed v) ctor (map (rn renamed) args)
      GoalDeconstruct v ctor args ->
        GoalDeconstruct (rn renamed v) ctor (map (rn renamed) args)
      other -> other
    step (renamed, acc) g =
      let g' = goTop renamed g
          renamed' = case g of
            GoalLambda lhs _ _ _ -> Set.insert lhs renamed
            _ -> renamed
      in (renamed', acc ++ [g'])

extractNameArity :: Text -> (Text, Int)
extractNameArity header =
  -- Look for `name'/N pattern
  case T.breakOn "`" header of
    (_, rest) | not (T.null rest) ->
      let afterQuote = T.drop 1 rest  -- after opening backtick
          name = T.takeWhile (/= '\'') afterQuote
          -- Strip module prefix: "check.check_threshold" → "check_threshold"
          baseName = case T.breakOnEnd "." name of
            ("", n) -> n    -- no dot, use as-is
            (_, n)  -> n    -- take part after last dot
          afterName = T.drop 1 $ T.dropWhile (/= '\'') afterQuote  -- after closing quote
          afterSlash = T.drop 1 $ T.dropWhile (/= '/') afterName
          arity = case reads (T.unpack (T.takeWhile (\c -> c >= '0' && c <= '9') afterSlash)) of
            [(n, _)] -> n
            _ -> 0
      in (baseName, arity)
    _ -> ("unknown", 0)

parseModeDecl :: Text -> ([MercuryMode], MercuryDet)
parseModeDecl line =
  -- Predicate form:  ":- mode name(in, in, out) is det."
  -- Function form:   ":- mode name(in) = out is det."
  -- The function form has an extra return-mode after the closing paren
  -- and before " is ".  Pick it up by inspecting the gap.
  --
  -- Higher-order modes like "in((pred(in) is semidet))" contain nested
  -- parens, so naive ')'-as-terminator and ','-as-separator both fail.
  -- Use paren-depth-aware extraction and splitting.
  let -- Nullary func form has no `(` (e.g. `:- mode zero = out is det`);
      -- distinguish from the predicate form to avoid emitting a bogus
      -- ModeIn placeholder for the missing param list.
      lineAfterModeKw = T.dropWhile (/= '(') line
      hasParamList    = not (T.null lineAfterModeKw)
      (paramText, afterCloseParen) =
        if hasParamList
          then takeParenBalanced lineAfterModeKw
          else (T.empty, line)
      paramModes = if hasParamList
                     then map parseMode (splitCommasOuter paramText)
                     else []
      (gapBeforeIs, _) = T.breakOn " is " afterCloseParen
      -- Strip optional leading whitespace then look for `= mode`.  The
      -- whole gapBeforeIs in the nullary-func form is the entire line
      -- minus the trailing ` is det.` (e.g. `:- mode zero = out`);
      -- find `=` anywhere and parse what follows as the return mode.
      retModes = case T.breakOn "=" (T.strip gapBeforeIs) of
        (_, eq) | not (T.null eq) ->
          [parseMode (T.strip (T.drop 1 eq))]
        _ -> []
      modes = paramModes ++ retModes
      afterIs = T.strip $ T.drop 4 $ snd $ T.breakOn " is " line
      det = parseDet $ T.takeWhile (/= '.') afterIs
  in (modes, det)
  where
    -- Given input starting with @'('@, return (inside, after) where
    -- @inside@ is the content between the opening @'('@ and its
    -- matching @')'@ (paren-depth-aware), and @after@ is everything
    -- past the matching close paren.  If there's no opening paren, return
    -- (empty, original).
    takeParenBalanced t = case T.uncons t of
      Just ('(', rest) -> goBal 1 0 rest
      _                -> (T.empty, t)
      where
        goBal :: Int -> Int -> Text -> (Text, Text)
        goBal d i s = case T.uncons (T.drop i s) of
          Nothing      -> (T.take i s, T.empty)
          Just ('(', _) -> goBal (d + 1) (i + 1) s
          Just (')', _)
            | d == 1   -> (T.take i s, T.drop (i + 1) s)
            | otherwise -> goBal (d - 1) (i + 1) s
          Just (_, _)  -> goBal d (i + 1) s

    -- Split @t@ on commas that lie at paren depth zero.  Mode args
    -- like "in((pred(in) is semidet))" contain inner commas at deeper
    -- depths that must NOT split.
    splitCommasOuter :: Text -> [Text]
    splitCommasOuter t = go 0 0
      where
        n = T.length t
        go :: Int -> Int -> [Text]
        go d i
          | i >= n = [T.strip t]
          | otherwise = case T.index t i of
              '(' -> go (d + 1) (i + 1)
              ')' -> go (d - 1) (i + 1)
              ',' | d == 0 ->
                    T.strip (T.take i t) : splitCommasOuter (T.drop (i + 1) t)
              _   -> go d (i + 1)

parseMode :: Text -> MercuryMode
parseMode t = case T.strip t of
  "in"  -> ModeIn
  "out" -> ModeOut
  "di"  -> ModeDi
  "uo"  -> ModeUo
  _     -> ModeIn

parseDet :: Text -> MercuryDet
parseDet t = case T.strip t of
  "det"       -> Det
  "semidet"   -> Semidet
  "multi"     -> Multi
  "nondet"    -> Nondet
  "failure"   -> Failure
  "erroneous" -> Erroneous
  "cc_multi"  -> CCMulti
  "cc_nondet" -> CCNondet
  _           -> Det

-- | Extract the goal body text from the HLDS block
-- | Extract argument names from the clause header line.
-- The clause head looks like "module.pred(Arg1, Arg2) :-"
-- Must skip ":- mode" and ":- end_module" lines.
extractArgNames :: [Text] -> [Text]
extractArgNames ls =
  -- Find the clause head: contains ":-" but doesn't start with ":-"
  case filter isClauseHead ls of
    (clauseHead:_) ->
      let afterParen = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') clauseHead
          commaArgs = map T.strip $ T.splitOn "," afterParen
          -- Mercury HLDS prints module-qualified infix operators with
          -- their args in the operator-form clause head:
          --   rational.(R1 < R2) :- ...
          -- That's a 2-arg predicate; comma-splitting yields one arg
          -- "R1 < R2".  Detect this and split on the infix operator.
          infixOps = [" < ", " > ", " =< ", " >= ", " == ", " + ", " - "
                     , " * ", " / "]
          -- Prefix-operator clause heads (unary +/-/~ on a single arg)
          -- arrive as a single comma-arg that begins with the op then a
          -- space: "+ Rat", "- HeadVar__1".  Strip the operator so the
          -- arg name matches references in the goal body.  Without this
          -- the param surfaces in MLIR as the sanitised "zp_Rat" /
          -- "zm_HeadVar__1" and the body's "Rat" / "HeadVar__1" refs
          -- escape as unresolved 0-arg calls.
          stripPrefixOp s =
            let s' = T.stripStart s
            in case T.uncons s' of
                 Just (c, rest)
                   | (c == '+' || c == '-' || c == '~')
                   , Just (' ', _) <- T.uncons rest
                   -> T.strip rest
                 _ -> s
          paramArgs = case commaArgs of
            [single] | any (`T.isInfixOf` single) infixOps ->
              let tryOps []         = [stripPrefixOp single]
                  tryOps (op:rest)
                    | op `T.isInfixOf` single =
                        let (l, r) = T.breakOn op single
                        in [T.strip l, T.strip (T.drop (T.length op) r)]
                    | otherwise = tryOps rest
              in tryOps infixOps
            [single] -> [stripPrefixOp single]
            _ -> commaArgs
          -- Function-form clause heads:  "pred(args) = OutVar :-"
          -- Capture the OutVar so the translator can bind the output of
          -- the body to it.  Without this, function-mode predicates lose
          -- their return-value slot and the body emits the deconstructed
          -- variable as a free reference at link time.
          afterCloseParen = T.strip $ T.drop 1 $ T.dropWhile (/= ')') clauseHead
          outArg = case T.stripPrefix "=" afterCloseParen of
            Just rest ->
              let rest' = T.strip rest
                  beforeColon = case T.breakOn ":-" rest' of
                    (b, _) -> T.strip b
              in [beforeColon | not (T.null beforeColon)]
            Nothing -> []
      in paramArgs ++ outArg
    [] -> []
  where
    isClauseHead l =
      let s = T.stripStart l
      in T.isInfixOf ":-" s && not (":- " `T.isPrefixOf` s) && not ("%" `T.isPrefixOf` s)

extractGoalText :: [Text] -> Text
extractGoalText ls =
  -- The goal starts after the clause head line "module.pred(args) :-"
  -- and ends before ":- end_module".
  let isClauseHead l =
        let s = T.stripStart l
        in T.isInfixOf ":-" s && not (":- " `T.isPrefixOf` s) && not ("%" `T.isPrefixOf` s)
      afterClauseHead = dropWhile (not . isClauseHead) ls
      goalLines = case afterClauseHead of
        (hd:rest) ->
          -- If the clause head has content after ":-" on the same line, include it
          let afterColonDash = T.strip $ snd $ T.breakOn ":-" hd
              firstPart = T.drop 2 afterColonDash  -- drop ":-"
              goalContent = if T.null (T.strip firstPart) then rest
                           else firstPart : rest
          in takeWhile (not . (":- end_module" `T.isPrefixOf`) . T.stripStart) goalContent
        [] -> []
  in T.unlines goalLines

-- | Parse HLDS goal text into structured goals
parseGoalText :: Text -> MercuryGoal
parseGoalText txt
  | T.null (T.strip txt) = GoalUnparsed "(empty)"
  | otherwise = parseGoalLines (T.lines txt)

parseGoalLines :: [Text] -> MercuryGoal
parseGoalLines ls0 =
  let -- Strip a fully-wrapped `( % conjunction ... )` shell first.
      -- The marker check below is INFIX over the line set, so once
      -- the wrapper line is gone the marker check returns False —
      -- we lose the "this is a conjunction" signal even though the
      -- structure is plainly a conjunction.  Track whether we
      -- stripped so we can bypass the marker check.
      (ls, wrappedConj) = case stripConjunctionWrapperMaybe ls0 of
        Just inner -> (inner, True)
        Nothing    -> (ls0, False)
      stripped = map T.strip ls
      -- Look for structural markers
      conjParts = splitOnMarker "," stripped
      disjParts = splitOnMarker ";" stripped
      -- Determine the OUTERMOST construct by looking at the first
      -- structural marker (conjunction / switch) in the line stream.
      -- A switch body can contain `% conjunction` markers inside its
      -- arms; matching by "any line" would treat the whole body as a
      -- conjunction.  Conversely, a conjunction can wrap a switch as
      -- one of its goals.  The leading marker wins.
      firstMarker = listToMaybe
        [ () | l <- stripped
             , "% conjunction" `T.isInfixOf` l
            || "% cannot_fail switch on" `T.isInfixOf` l
            || "% switch on" `T.isInfixOf` l
        ]
      firstIsConj = case firstMarker of
        Just () ->
          let firstLine = head
                [ l | l <- stripped
                    , "% conjunction" `T.isInfixOf` l
                   || "% cannot_fail switch on" `T.isInfixOf` l
                   || "% switch on" `T.isInfixOf` l
                ]
          in "% conjunction" `T.isInfixOf` firstLine
        Nothing -> False
      firstIsSwitch = case firstMarker of
        Just () ->
          let firstLine = head
                [ l | l <- stripped
                    , "% conjunction" `T.isInfixOf` l
                   || "% cannot_fail switch on" `T.isInfixOf` l
                   || "% switch on" `T.isInfixOf` l
                ]
          in "% cannot_fail switch on" `T.isInfixOf` firstLine
          || "% switch on" `T.isInfixOf` firstLine
        Nothing -> False
  in case ls of
    -- Recognise an inline lambda binding BEFORE the conjunction check
    -- — the lambda body contains its own `% conjunction` marker and
    -- `,` separators that would otherwise shatter it apart.
    _ | Just (lhs, params, mOut, bodyLs) <- splitLambda stripped ->
          GoalLambda lhs params mOut (parseGoalLines bodyLs)
      -- Mercury negation `not ( body )`.  HLDS prints `not (` as a
      -- line of its own, then the body, then a closing `)` on its
      -- own line.  Without this catch, the line set falls through
      -- to parseSingleGoal which sees a `=` inside the body (e.g.
      -- `E = re_lit(V_3)`) and concocts a unify-stub whose LHS is
      -- `not (\nE` (a fused goal-text leak that also leaves the
      -- inner deconstruct variables — V_3 here — unresolved at
      -- link).
      | Just bodyLs <- splitNot stripped ->
          GoalNot (parseGoalLines bodyLs)
      -- If we just stripped a `( % conjunction ... )` wrapper AND the
      -- inner content splits into multiple conjuncts at depth 0, take
      -- the conjunction path FIRST.  Otherwise an inner conjunct that
      -- happens to start with `( if` would be greedily consumed by
      -- splitIfThenElse, which extends its "else" block past the
      -- ITE's closing `)` and swallows the sibling conjuncts (e.g.
      -- the `, TypeClassInfo_for_field = ..., poly.div_mod(...)`
      -- that follows an inner ITE inside integrate_rational's outer
      -- else).  Those sibling conjuncts then surface as part of the
      -- ITE body's goal-text leak, fusing `then`, `else`, and the
      -- bound variables into a single unparsed meganame.
      | wrappedConj, length conjParts > 1 ->
          GoalConj (map parseGoalLines conjParts)
      | Just (condLs, thenLs, elseLs) <- splitIfThenElse stripped ->
          GoalIfThenElse (parseGoalLines condLs)
                         (parseGoalLines thenLs)
                         (parseGoalLines elseLs)
      | wrappedConj ->  -- single-goal conjunction body — just parse it
          parseSingleGoal (T.unlines (filter (not . isComment) stripped))
      -- Outer form is a conjunction wrapping (possibly) a nested
      -- switch/disjunction goal.  Split first so the inner construct
      -- isn't consumed in place of the outer.
      | firstIsConj, length conjParts > 1 ->
          GoalConj (map parseGoalLines conjParts)
      | firstIsSwitch -> parseSwitch stripped
      | any ("% conjunction" `T.isInfixOf`) stripped &&
        length conjParts > 1 ->
          GoalConj (map parseGoalLines conjParts)
      | length disjParts > 1 ->
          GoalDisj (map parseGoalLines disjParts)
      -- No structural marker, but multiple depth-0 conjuncts.  This
      -- happens inside lambda bodies after stripLambdaWrappers has
      -- removed the `% conjunction` line; the conjuncts survive as
      -- bare lines separated by depth-0 commas.  Without this catch
      -- the whole body falls into parseSingleGoal's unify-stub
      -- catchall and the `,` separators get sanitised into a single
      -- mangled name (e.g. `C__V_13_ze_rational_zt__3`).
      | length conjParts > 1 ->
          GoalConj (map parseGoalLines conjParts)
      | otherwise -> parseSingleGoal (T.unlines (filter (not . isComment) stripped))

-- | Recognise an inline lambda binding in HLDS-printed form:
--   <LHS> = (pred(LambdaHeadVar__1::in, ...) is <det> :-
--     <body...>
--   )
-- or the func variant:
--   <LHS> = (func(LambdaHeadVar__1::in, ...) = (LambdaHeadVar__N::out) is det :-
--     <body...>
--   )
-- Returns the LHS var, the list of input-mode parameter names, the
-- optional output-mode parameter name (Just for func form), and the
-- raw body lines (inside the lambda's outer parens, with the closing
-- `)` trimmed).
splitLambda :: [Text] -> Maybe (Text, [Text], Maybe Text, [Text])
splitLambda strippedLs = case strippedLs of
  []       -> Nothing
  (l0 : _) ->
    case T.breakOn " = (pred(" l0 of
      (lhs, rest) | not (T.null rest) ->
        Just $ extractLambda (T.strip lhs) False (T.drop (T.length " = ") rest)
                             (tail strippedLs)
      _ -> case T.breakOn " = (func(" l0 of
        (lhs, rest) | not (T.null rest) ->
          Just $ extractLambda (T.strip lhs) True (T.drop (T.length " = ") rest)
                               (tail strippedLs)
        _ -> Nothing
  where
    -- @restOfHead@ is the text from the opening '(' onward on the
    -- first line; @bodyLs@ is everything that came after.
    -- We trace paren depth from 1 (we are immediately inside the
    -- lambda's outer paren) until it hits 0 — that closing ')' bounds
    -- the lambda body.
    extractLambda lhs isFunc restOfHead bodyLs =
      let -- restOfHead starts with "(pred(args)..." or "(func(args)=(out)...".
          -- Strip the outer "(", then the keyword ("pred" or "func"), then
          -- consume up to the inner "(": that opens the param list.
          inside = T.drop 1 (T.dropWhile (/= '(')
                              (T.drop 1 restOfHead))
          (paramText, afterParamsOnL0) = case T.breakOn ")" inside of
              (params, after) -> (params, after)
          (inputs, mOut) = parseLambdaParams paramText isFunc afterParamsOnL0
          parenDelta t =
            let opens  = T.length (T.filter (== '(') t)
                closes = T.length (T.filter (== ')') t)
            in opens - closes
          -- The head line ALREADY opens the lambda's outer '(', so its
          -- parenDelta is the actual depth at the start of bodyLs.
          -- Walk terminates at the matching ')' (depth drops to 0).
          headDepth = parenDelta restOfHead
          walk _ acc []     = (reverse acc, [])
          walk d acc (b:bs)
            | d' <= 0   = (reverse acc, bs)
            | otherwise = walk d' (b:acc) bs
            where d' = d + parenDelta b
          (rawBody, _trailing) = walk headDepth [] bodyLs
          -- Mercury HLDS lambda bodies are reliably wrapped in
          --   some [] ( % compiler
          --     [( % conjunction]
          --       <real body>
          --     [)]
          --   )
          -- The bracketed conjunction wrapper is only present when the
          -- body is multi-goal.  Strip both wrappers so parseGoalLines
          -- gets a clean body — otherwise the `,` separating the inner
          -- conjunction and the wrapper's `)`s confuse the splitter.
          stripped = stripLambdaWrappers rawBody
      in (lhs, inputs, mOut, stripped)

    -- | Strip the standard `some [] ( % compiler ... )` and inner
    -- `( % conjunction ... )` wrappers Mercury places around every
    -- lambda body.  Operates on lines (already stripped of whitespace).
    stripLambdaWrappers :: [Text] -> [Text]
    stripLambdaWrappers ls0 =
      let dropOpener prefix ls = case ls of
            (l:rest)
              | T.isPrefixOf prefix (T.strip l) -> Just rest
            _ -> Nothing
          dropTrailingCloseParen ls = case reverse ls of
            (l:rest)
              | T.strip l == ")" || T.strip l == ")." -> Just (reverse rest)
            _ -> Nothing
          -- Sequence: try strip "some []" then conjunction wrappers,
          -- each requiring a matching trailing ')'.
          step ls = case dropOpener "some [] (" ls of
            Just inner -> case dropTrailingCloseParen inner of
              Just inner'  -> step (stripLambdaWrappers inner')
              Nothing      -> ls
            Nothing -> case dropOpener "( % conjunction" ls of
              Just inner -> case dropTrailingCloseParen inner of
                Just inner' -> stripLambdaWrappers inner'
                Nothing     -> ls
              Nothing -> ls
      in step ls0

    -- | Parse "LambdaHeadVar__1::in, LambdaHeadVar__2::in, ..." into
    -- the list of input variable names.  For the func form, the
    -- header continues "= (LambdaHeadVar__M::out)" — captured here
    -- from the 'afterParens' text following the input-param list.
    parseLambdaParams :: Text -> Bool -> Text -> ([Text], Maybe Text)
    parseLambdaParams paramText isFunc afterParens =
      let chunks = map T.strip (T.splitOn "," paramText)
          extractName ch = T.strip (T.takeWhile (\c -> c /= ':' && c /= ' ') ch)
          inputs = filter (not . T.null) (map extractName chunks)
          mOut =
            if not isFunc then Nothing
            else case T.breakOn "(" (T.dropWhile (/= '=') afterParens) of
                   (_, rest) | not (T.null rest) ->
                     let inside = T.takeWhile (/= ')') (T.drop 1 rest)
                     in Just (extractName inside)
                   _ -> Nothing
      in (inputs, mOut)

-- | Recognise Mercury HLDS if-then-else block structure.  HLDS prints:
--   ( if
--     <cond>
--   then
--     <then>
--   else
--     <else>
--   )
-- where 'if'/'then'/'else'/closing-')' may appear as their own lines or
-- embedded in surrounding parens.  Returns (cond-lines, then-lines,
-- else-lines) by scanning for 'if' / 'then' / 'else' markers at
-- top-of-block paren depth; returns Nothing if no clear ITE is present.
--
-- Nested ITEs work because 'parseGoalLines' is called recursively on each
-- segment and re-runs the splitter.
-- | Recognise Mercury negation HLDS form:
--   not (
--     <body...>
--   )
-- where the body may be a conjunction (and so contain its own
-- `% conjunction` marker and `,` separators).  Returns the body
-- lines on a match.
splitNot :: [Text] -> Maybe [Text]
splitNot rawLs = case rawLs of
  ("not (" : rest) -> Just (trimEnd rest)
  _                -> Nothing
  where
    isBlank l = T.null (T.strip l)
    isBareClose l =
      let s = T.strip (T.dropWhileEnd (`elem` (" ." :: String)) (T.strip l))
      in s == ")"
    trimEnd ls =
      let rev = reverse ls
          afterBlanks = dropWhile isBlank rev
      in case afterBlanks of
           (l:rest') | isBareClose l -> reverse rest'
           _                         -> reverse afterBlanks

splitIfThenElse :: [Text] -> Maybe ([Text], [Text], [Text])
splitIfThenElse rawLs =
  let strippedLs = map T.strip rawLs
      -- The HLDS form starts with "( if" — either as a single line or
      -- with "(" on one line and "if" on the next.  Detect either shape.
      hasIfHeader = case strippedLs of
        ("( if" : _)  -> True
        ("(" : "if" : _) -> True
        _             -> False
  in if not hasIfHeader then Nothing
     else
       -- Find indices of 'then' and 'else' at depth-0 within the ITE block,
       -- and the matching closing ')'.
       let pairs = zip [0 :: Int ..] strippedLs
           -- Helper: trace nesting starting at start index, counting
           -- '(' and ')' on each line (HLDS keeps these in their own
           -- lines or embedded — count syntactic open/close).
           parenDelta t =
             let opens  = T.length (T.filter (== '(') t)
                 closes = T.length (T.filter (== ')') t)
             in opens - closes
           -- Walk lines after the 'if' (line 0 or 1), tracking depth.
           -- depth starts at 1 (we are inside the opening '(').
           startIdx = if take 1 strippedLs == ["( if"] then 1 else 2
           walk _   []                acc = reverse acc
           walk dep ((i,t):rest)      acc
             | dep == 1 && t == "then" =
                 walk dep rest ((i,"THEN"):acc)
             | dep == 1 && t == "else" =
                 walk dep rest ((i,"ELSE"):acc)
             | dep == 0 = reverse acc  -- past the closing ')'
             | otherwise =
                 let dep' = dep + parenDelta t
                 in walk dep' rest acc
           markers = walk 1 (drop startIdx pairs) []
           thenIdx = lookup "THEN" [(v,k) | (k,v) <- markers]
           elseIdx = lookup "ELSE" [(v,k) | (k,v) <- markers]
       in case (thenIdx, elseIdx) of
            (Just tIdx, Just eIdx) | tIdx < eIdx ->
              let condLs = drop startIdx (take tIdx strippedLs)
                  thenLs = drop (tIdx + 1) (take eIdx strippedLs)
                  -- Else block extends to the last meaningful line.  The
                  -- ITE is terminated by exactly ONE bare-`)` line that
                  -- matches the `( if` opener; any inner `)`s belong to
                  -- inner constructs (e.g. the closing `)` of an else-arm's
                  -- `( % conjunction ... )` wrapper).  Drop trailing pure
                  -- whitespace lines, then drop exactly the first
                  -- bare-`)` line we encounter scanning from the end —
                  -- aggressive multi-`)` stripping eats the else-arm's
                  -- wrapper close, which prevents stripConjunctionWrapperMaybe
                  -- from recognising the wrapper and surfaces every inner
                  -- conjunct as a free-EVar leak fused with the surrounding
                  -- if/then/else markers.
                  rest   = drop (eIdx + 1) strippedLs
                  isBlank l = T.null (T.strip l)
                  isBareCloseParen l =
                    let s = T.strip (T.dropWhileEnd (`elem` (" ." :: String)) (T.strip l))
                    in s == ")"
                  -- Reverse, drop leading blank lines, drop ONE bare `)`,
                  -- then reverse back.  If no bare `)` line is present
                  -- (unusual but defensive) we leave the input alone past
                  -- the blank trim.
                  trimEnd ls =
                    let rev = reverse ls
                        afterBlanks = dropWhile isBlank rev
                    in case afterBlanks of
                         (l:rest') | isBareCloseParen l -> reverse rest'
                         _ -> reverse afterBlanks
                  elseLs = trimEnd rest
              in Just (condLs, thenLs, elseLs)
            _ -> Nothing

isComment :: Text -> Bool
isComment t = let s = T.strip t
              in "%" `T.isPrefixOf` s || T.null s ||
                 s == "(" || s == ")" || s == ")." ||
                 -- Mercury wraps disjunctions in "( % disjunction ... )" —
                 -- treat the opening paren+comment as a comment line.
                 ("(" `T.isPrefixOf` s && "%" `T.isInfixOf` s)

-- Split a list of lines on a delimiter that appears as a standalone
-- line, but only at the construct's top syntactic level.  Mercury HLDS
-- nests conjunctions inside if/then/else and disjunction blocks; a
-- depth-agnostic split shatters the outer conjunction into pieces
-- belonging to inner constructs.  We track paren depth across lines
-- and treat the minimum depth at which a marker appears as "this
-- construct's top level"; markers at exactly that depth are
-- conjunction/disjunction separators, deeper ones stay inside their
-- nested goal where they belong.
splitOnMarker :: Text -> [Text] -> [[Text]]
splitOnMarker marker ls =
  let parenDelta t =
        let opens  = T.length (T.filter (== '(') t)
            closes = T.length (T.filter (== ')') t)
        in opens - closes
      depths = scanl (+) 0 (map parenDelta ls)
      depthBefore = init depths
      markerDepths = [d | (l, d) <- zip ls depthBefore, T.strip l == marker]
      topDepth = case markerDepths of
        []  -> 0
        ds  -> minimum ds
      go acc [] = [reverse acc]
      go acc ((l, d):rest)
        | T.strip l == marker && d == topDepth =
            reverse acc : go [] rest
        | otherwise = go (l:acc) rest
  in go [] (zip ls depthBefore)

-- | Strip a fully-wrapped `( % conjunction ... )` shell, returning
-- only the inner conjuncts.  Idempotent: returns the input unchanged
-- if (1) the first stripped line doesn't have the expected
-- `( % conjunction` opener, or (2) the matching `)` isn't the last
-- non-blank line.  Restricting to "fully wrapped" prevents stripping
-- chunks of a split-conjunction (which start with the wrapper opener
-- but don't carry the matching closer — that closer is in the LAST
-- chunk).  Disjunction and switch wrappers are intentionally NOT
-- stripped: parseSwitch needs the `switch on \`Var\'` line on the
-- header, and GoalDisj's structure is captured by the `;` split.
-- | Returns Just <inner-lines> when the input is fully wrapped in a
-- `( % conjunction ... )` shell (the closing ')' is the last
-- non-blank line); else Nothing.  The caller can use Nothing as a
-- signal that no stripping occurred (so the existing marker checks
-- still apply).
stripConjunctionWrapperMaybe :: [Text] -> Maybe [Text]
stripConjunctionWrapperMaybe ls = case ls of
  (l : rest)
    | "( % conjunction" `T.isPrefixOf` T.strip l ->
        let parenDelta t =
              let opens  = T.length (T.filter (== '(') t)
                  closes = T.length (T.filter (== ')') t)
              in opens - closes
            go _ _ [] = Nothing
            go d i (x:xs) =
              let d' = d + parenDelta x
              in if d' <= 0
                 then if all (T.null . T.strip) xs
                      then Just i
                      else Nothing
                 else go d' (i + 1) xs
        in case go 1 (0 :: Int) rest of
             Just closeIdx -> Just (take closeIdx rest)
             Nothing       -> Nothing
  _ -> Nothing

parseSwitch :: [Text] -> MercuryGoal
parseSwitch ls =
  -- Find the switch variable from "switch on `Var'"
  let switchLine = head $ filter (\l -> "switch on" `T.isInfixOf` l) ls
      varName = T.takeWhile (/= '\'') $ T.drop 1 $ snd $ T.breakOn "`" switchLine
      -- Split arms at "has functor" markers
      arms = extractSwitchArms ls
  in GoalSwitch varName arms

extractSwitchArms :: [Text] -> [(Text, MercuryGoal)]
extractSwitchArms ls =
  -- Only treat `has functor` markers at the OUTER switch's paren depth
  -- as arm boundaries.  Mercury HLDS often nests switches inside one
  -- another (a switch-on-HeadVar__1 whose [|]/2 arm contains another
  -- switch-on-HeadVar__2 with its own `has functor` markers); a
  -- depth-agnostic split would slice the outer arms apart at every
  -- inner functor marker and lose the inner switch's structure
  -- entirely.
  let parenDelta t =
        let opens  = T.length (T.filter (== '(') t)
            closes = T.length (T.filter (== ')') t)
        in opens - closes
      depths = scanl (+) 0 (map parenDelta ls)
      depthBefore = init depths
      functorIxs =
        [ i | (i, (l, _d)) <- zip [0::Int ..] (zip ls depthBefore)
            , "has functor" `T.isInfixOf` l ]
      outerDepth = case functorIxs of
        []    -> 0
        (i:_) -> depthBefore !! i
      isArmBoundary (l, d) =
        "has functor" `T.isInfixOf` l && d == outerDepth
      walk acc curArm [] =
        reverse (case curArm of
          Just (f, ls') -> (f, parseGoalLines (reverse ls')) : acc
          Nothing       -> acc)
      walk acc curArm ((l, d):rest)
        | isArmBoundary (l, d) =
            let functor = T.strip $ snd $ T.breakOnEnd "functor " l
                acc' = case curArm of
                  Just (f, ls') -> (f, parseGoalLines (reverse ls')) : acc
                  Nothing       -> acc
            in walk acc' (Just (functor, [])) rest
        | otherwise =
            let curArm' = fmap (\(f, ls') -> (f, l : ls')) curArm
            in walk acc curArm' rest
  in walk [] Nothing (zip ls depthBefore)

parseSingleGoal :: Text -> MercuryGoal
parseSingleGoal txt
  | T.null stripped = GoalUnparsed "(empty)"
  -- Mercury's @true@ atom is the always-succeeds goal — emit as an
  -- empty conjunction so the CPS chain threads through with no
  -- side-effect, rather than falling to GoalUnparsed which surfaces
  -- as an `unparsed_goal$1` link symbol.  Same for @fail@ at the
  -- semantic level (mapped to the runtime fail stub).
  | stripped == "true" = GoalConj []
  | stripped == "fail" = GoalCall "mercury_fail" []
  -- Inline lambda RHS: 'V = (pred(...) is <det> :- body)' or
  -- 'V = (func(...) = ... is det :- body)'.  Full higher-order
  -- support isn't implemented yet, so emit a placeholder that binds
  -- the LHS to a stub @lambda_placeholder@ closure value.  Without
  -- this catch the entire lambda body falls into the unify catchall
  -- and sanitises into a gigantic fused symbol.
  | " = " `T.isInfixOf` stripped
  , let (lhs, rhs) = T.breakOn " = " stripped
        rhs' = T.strip (T.drop 3 rhs)
        lhs' = T.strip lhs
  , isLambdaRhs rhs'
  = GoalConstruct lhs' "lambda_placeholder" []
  -- Unification check runs FIRST so an LHS like "V_5 = integer.(..)" is
  -- correctly identified as an assignment.  Previously parseMercuryBuiltin
  -- ran first and would mis-classify the whole goal as a top-level call to
  -- "V_5 = integer.<op>", baking the LHS into the symbol name.
  | " = " `T.isInfixOf` stripped =
      let (lhs, rhs) = T.breakOn " = " stripped
          rhs' = T.strip (T.drop 3 rhs)
          lhs' = T.strip lhs
      in case parseMercuryBuiltin rhs' of
           -- RHS is "module.(X op Y)" or "module.(op X)" form — emit as
           -- a function call with LHS appended as the synthesised output
           -- arg.  GoalConstruct would route through ECon and allocate a
           -- bogus ctor; GoalCall routes through EApp to a runtime stub
           -- (e.g. @integer_zm) which is what these ops are.
           Just (GoalCall op opArgs) -> GoalCall op (opArgs ++ [lhs'])
           _ -> case parseCtorApp rhs' of
              -- LHS = module.ctor(args) or LHS = ctor(args) → construct/deconstruct
              -- with a properly extracted functor name and argument list. Which of
              -- construct vs deconstruct it is gets decided downstream based on
              -- the flow of bindings; we emit GoalConstruct here and let the
              -- translator reinterpret it if the LHS is already bound.
              Just (ctor, args) -> GoalConstruct lhs' ctor args
              -- Mercury list literals: 'list.[]' → 0-arg @list_nil@;
              -- 'list.[H | T]' → 2-arg @list_cons@ with H and T.
              -- Names are chosen so the C runtime can look them up
              -- (via tag from sanitisation) and walk lists for, e.g.,
              -- io.format's poly_type argument list.  Without this the
              -- old fallback baked the entire bracketed text into a
              -- 0-field ctor name with no fields set, so callers like
              -- io.format saw a list head of zero elements.
              Nothing | Just listGoal <- parseListLiteral lhs' rhs'
                          -> listGoal
              -- Mercury anonymous-tuple literals: 'V = {A, B, C}'.
              -- Construct a ctor named @tuple@ with N fields; pattern
              -- matches in deconstruct contexts (when LHS is already
              -- bound) reuse the same ctor name so the tags align.
              Nothing | Just tupleGoal <- parseTupleLiteral lhs' rhs'
                          -> tupleGoal
              -- Module-qualified no-arg call: 'V = integer.zero' should
              -- bind V to a call of integer.zero, not unify V with the
              -- string 'integer.zero'.  Detect a clean 'module.name'
              -- shape and emit GoalConstruct with zero args.
              Nothing | "." `T.isInfixOf` rhs'
                      , isCleanCallName rhs'
                          -> GoalConstruct lhs' rhs' []
              -- Module-qualified quoted-op call: 'V = list.'++'(A, B, C)'.
              -- Mercury prints binary operators as functions via the
              -- @list.'++'(...)@ shape.  Strip the quotes around the
              -- operator and emit a GoalCall with the LHS appended as
              -- the synthesised output arg.
              Nothing | Just (op, args) <- parseQuotedOpCall rhs'
                          -> GoalCall op (args ++ [lhs'])
              -- Module-qualified bare operator: 'Cmp = builtin.(=)',
              -- 'Cmp = builtin.(<)' etc.  These are 0-arg tag values
              -- (members of a comparison_result enum).  Treat them as
              -- 0-arg ctor constructions so LHS gets a let-binding
              -- rather than falling into the unify-stub catchall that
              -- leaks both sides as free references.
              Nothing | Just tag <- parseQualifiedOp rhs'
                          -> GoalConstruct lhs' tag []
              -- Otherwise it's a bare unification / assignment.
              Nothing -> GoalUnify lhs' rhs'
  -- Mercury builtin without an LHS bind (rare — typically tests inside a
  -- conjunction): "int.(X > Y)" yields a direct GoalCall.
  | Just builtinCall <- parseMercuryBuiltin stripped = builtinCall
  -- Predicate call: module.pred(args).  Require the name (the prefix
  -- before '(') to be a clean module-qualified or bare identifier — no
  -- spaces, no '=', no other goal-text leakage.  This filters out the
  -- multi-line if/then/else continuations that previously fused with
  -- subsequent text and produced symbols like
  -- "__if_rational_is_zero_Diff__then_Cmp_ze_…".
  | "(" `T.isInfixOf` stripped
  , let name = T.strip (T.takeWhile (/= '(') stripped)
  , isCleanCallName name =
      let argsText = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') stripped
          args = map T.strip $ T.splitOn "," argsText
      in GoalCall name args
  | otherwise = GoalUnparsed stripped
  where
    -- Drop trailing Mercury statement terminator '.' so atoms like "7."
    -- get recognised as the literal 7 downstream by CoreTranslate's
    -- readMaybe.  Without this, fact-form `is det` clauses (e.g.
    -- `seven(7).` translating to HLDS `HeadVar__1 = 7.`) fall through
    -- the unify branch as `GoalUnify "HeadVar__1" "7."` and the
    -- literal-on-RHS case in translateGoalK fails to match.
    stripped = let s = T.strip txt
               in case T.unsnoc s of
                    Just (rest, '.') -> rest
                    _                -> s
    -- A clean call-name is a (possibly module-qualified) identifier:
    -- letters/digits/underscores/dots, no whitespace, no '=', etc.
    -- Empty / starts-with-digit also rejected.
    isCleanCallName n =
      not (T.null n)
      && not (T.any (\c -> c == ' ' || c == '=' || c == '\n' || c == '\t'
                        || c == '`' || c == ';' || c == '|') n)
      && let h = T.head n
         in (h >= 'a' && h <= 'z') || (h >= 'A' && h <= 'Z') || h == '_'

-- | Parse a constructor application of the form @ctor(arg1, arg2, ...)@ or
-- @module.ctor(arg1, arg2, ...)@, returning the (possibly module-qualified)
-- name and the argument list.  The qualified form is preserved so the
-- translator can route to user-defined functions whose names happen to
-- share a module prefix with stdlib ctors (e.g. distinguish the user
-- function @rational.numer/1@ from the data ctor @rational.r/2@).
-- Returns 'Nothing' for anything that does not look like a functor
-- application (plain atoms, literals, variables, infix exprs).
parseCtorApp :: Text -> Maybe (Text, [Text])
parseCtorApp t
  | not ("(" `T.isInfixOf` t) = Nothing
  | otherwise =
      let name    = T.strip (T.takeWhile (/= '(') t)
          argsRaw = T.drop 1 (T.dropWhile (/= '(') t)
          -- Drop the matching trailing ')' (and any trailing '.').
          inside  = dropTrailingParen argsRaw
          args    = filter (not . T.null)
                  $ map T.strip
                  $ splitCtorArgs inside
          bareName = case T.breakOnEnd "." name of
            ("", n) -> n
            (_,  n) -> n
      in if T.null name || not (isCtorish bareName)
         then Nothing
         else Just (name, args)
  where
    -- A Mercury constructor name starts lowercase (atoms) or may be purely
    -- alphanumeric plus underscores. Reject strings containing whitespace
    -- or infix operators — those are expressions, not applications.
    isCtorish n =
      not (T.null n)
      && T.all (\c -> c == '_' || isAlphaNum' c) n
    isAlphaNum' c = (c >= 'a' && c <= 'z')
                 || (c >= 'A' && c <= 'Z')
                 || (c >= '0' && c <= '9')
    dropTrailingParen s =
      let s' = T.dropWhileEnd (== '.') (T.stripEnd s)
      in if not (T.null s') && T.last s' == ')'
         then T.init s'
         else s'

-- | Recognise a Mercury HLDS inline lambda RHS.  The bridge has no
-- higher-order support yet; emitting a placeholder ctor prevents the
-- lambda's body lines from fusing into the unification fallback's
-- huge sanitised symbol name at link time.
isLambdaRhs :: Text -> Bool
isLambdaRhs t =
  let s = T.stripStart t
  in "(pred(" `T.isPrefixOf` s
  || "(func(" `T.isPrefixOf` s
  || "(pred ("  `T.isPrefixOf` s
  || "(func ("  `T.isPrefixOf` s

-- | Recognise a Mercury list literal on the RHS of a unification:
-- 'list.[]' / '[]' for nil, 'list.[H | T]' / '[H | T]' for cons.
-- Emits a GoalConstruct that downstream translates to a real
-- two-field cons cell rather than a 0-field ctor with the bracketed
-- text baked into the name.  The chosen ctor names @list_nil@ /
-- @list_cons@ sanitise stably so the runtime can rely on the tags.
-- | Recognise Mercury's quoted-operator call syntax:
-- @module.'op'(arg1, arg2, ...)@.  Mercury prints binary operators as
-- functions this way (e.g. @list.'++'(TypeInfo, A, B)@).  Returns the
-- fully-qualified call name (e.g. @list.++@) and the comma-separated
-- arg list when matched, else 'Nothing'.  Uses the existing comma
-- splitter so nested parens in args (e.g. @type_ctor_info(a,b,c)@)
-- are respected.
parseQuotedOpCall :: Text -> Maybe (Text, [Text])
parseQuotedOpCall t = do
  -- Find ".'" which delimits the module from the quoted op.
  let s = T.strip t
  (modPart, after) <- case T.breakOn ".'" s of
    (m, rest) | not (T.null rest) -> Just (m, T.drop 2 rest)
              | otherwise         -> Nothing
  -- after = "op'(arg1, ...)" — find the closing quote of the op.
  (op, after2) <- case T.breakOn "'" after of
    (o, rest) | not (T.null rest) -> Just (o, T.drop 1 rest)
              | otherwise         -> Nothing
  -- after2 must be "(args...)" — strip the parens.
  case T.uncons after2 of
    Just ('(', inner) -> case T.unsnoc inner of
      Just (argsTxt, ')') ->
        let args = filter (not . T.null)
                 $ map T.strip
                 $ splitCtorArgs argsTxt
            callName = modPart <> "." <> op
        in Just (callName, args)
      _ -> Nothing
    _ -> Nothing

parseListLiteral :: Text -> Text -> Maybe MercuryGoal
parseListLiteral lhs rhs =
  let body = case T.stripPrefix "list." rhs of
        Just rest -> rest
        Nothing   -> rhs
  in case T.uncons body of
    -- Mercury list range syntax: @list.(Lo .. Hi)@ builds the list
    -- [Lo, Lo+1, ..., Hi].  Route to the runtime @list_range@ helper
    -- (kk_range_list under the hood).
    Just ('(', after) | Just (inner, ')') <- T.unsnoc after
                      , T.isInfixOf " .. " inner ->
      let (lo, hi) = T.breakOn " .. " (T.strip inner)
          loS = T.strip lo
          hiS = T.strip (T.drop 4 hi)  -- drop " .. "
      in Just (GoalCall "list.range" [loS, hiS, lhs])
    Just ('[', after) -> case T.unsnoc after of
      Just (inner, ']') ->
        let stripped = T.strip inner
        in if T.null stripped
           -- Names end in "Nil" / "Cons" so the bridge's
           -- Core.ConTags.assignProgramTags hits its fast path
           -- (kkNilTag = 31636, kkConsTag = 46589) — matching the C
           -- runtime's KK_HASKELL_NIL_TAG / KK_HASKELL_CONS_TAG so
           -- bridge-built and runtime-built list cells share tags.
           then Just (GoalConstruct lhs "list_Nil" [])
           else case T.breakOn "|" stripped of
             (h, t) | not (T.null t) ->
               Just (GoalConstruct lhs "list_Cons"
                       [T.strip h, T.strip (T.drop 1 t)])
             _ ->
               Just (GoalConstruct lhs "list_Cons"
                       [T.strip stripped, "[]"])
      _ -> Nothing
    _ -> Nothing

-- | Recognise a Mercury anonymous-tuple literal: @{A, B, C}@.
-- Returns a 'GoalConstruct' with ctor name @tuple@ and the inner
-- field names as args.  Same name used for both construct (LHS
-- fresh) and deconstruct (LHS bound) paths in 'translateGoalK
-- GoalConstruct' so tags align.  Reuses the comma-split helper
-- 'splitCtorArgs' (paren-depth-aware) for nested ctor args.
parseTupleLiteral :: Text -> Text -> Maybe MercuryGoal
parseTupleLiteral lhs rhs = case T.uncons (T.stripStart rhs) of
  Just ('{', after) -> case T.unsnoc (T.stripEnd after) of
    Just (inner, '}') ->
      let stripped = T.strip inner
          args = filter (not . T.null) (map T.strip (splitCtorArgs stripped))
      in Just (GoalConstruct lhs "tuple" args)
    _ -> Nothing
  _ -> Nothing

-- | Recognise a module-qualified bare operator atom like @builtin.(=)@,
-- @builtin.(<)@, @builtin.(>)@ — Mercury's comparison_result tags.
-- Returns the canonical "module.op" form (without the parens) so the
-- caller can emit a 0-arg ctor.  Reject anything with whitespace
-- inside the parens (those are real expressions, handled elsewhere).
parseQualifiedOp :: Text -> Maybe Text
parseQualifiedOp t =
  case T.breakOn ".(" t of
    (modPart, rest)
      | not (T.null modPart)
      , Just rest1 <- T.stripPrefix ".(" rest
      , Just (op, after) <- T.uncons rest1
      , Just (')', tail') <- fmap (\(c, _) -> (c, T.tail after)) (T.uncons after)
      , T.null (T.strip tail')
      , op /= '(' && op /= ')' && op /= ' '
      -> Just (modPart <> "." <> T.singleton op)
    _ -> Nothing

-- | Parse Mercury builtin operations like "int.(X > Y)", "int.(X + Y)"
-- These are module-qualified infix operations in the HLDS dump.
parseMercuryBuiltin :: Text -> Maybe MercuryGoal
parseMercuryBuiltin txt
  -- Pattern: "module.(lhs op rhs)" — binary infix.
  | Just (modPart, inner) <- breakOnDotParen txt
  , not (T.null inner)
  , Just (lhs, op, rhs) <- parseInfixExpr inner
  = Just $ GoalCall (modPart <> "." <> op) [lhs, rhs]
  -- Pattern: "module.(op arg)" — unary prefix (-, +, ~ on a single var).
  -- Distinct from infix by absence of a second operand to the left of
  -- the operator.
  | Just (modPart, inner) <- breakOnDotParen txt
  , not (T.null inner)
  , Just (op, arg) <- parsePrefixExpr inner
  = Just $ GoalCall (modPart <> "." <> op) [arg]
  | otherwise = Nothing
  where
    -- Break "int.(X > Y)" or "int.(X > Y)." into ("int", "X > Y")
    breakOnDotParen t =
      let (before, after) = T.breakOn ".(" t
      in if T.null after then Nothing
         else let inner = T.drop 2 after  -- skip ".("
                  -- Find matching closing paren
                  content = T.takeWhile (/= ')') inner
              in Just (before, content)

    -- Parse "X > Y" into (X, ">", Y).  Longer operators must come
    -- before their prefixes so "//" is matched ahead of "/", ">=" ahead
    -- of ">", etc.; otherwise the prefix wins and the remainder gets
    -- mis-split.
    parseInfixExpr e =
      let ops = [ " // ", " ++ ", " >= ", " =< ", " == ", " mod ", " rem "
                , " div "
                , " > ", " < ", " + ", " - ", " * ", " / " ]
          tryOp [] = Nothing
          tryOp (op:rest') =
            case T.breakOn op e of
              (lhs, rhs') | not (T.null rhs') ->
                Just (T.strip lhs, T.strip op, T.strip (T.drop (T.length op) rhs'))
              _ -> tryOp rest'
      in tryOp ops

    -- Parse "- Var" / "+ Var" / "~ Var" into (op, arg).  Reject any
    -- expression with internal whitespace beyond the prefix — that
    -- would be a binary form that parseInfixExpr should have caught.
    parsePrefixExpr e =
      let s = T.strip e
      in case T.uncons s of
           Just (c, rest)
             | (c == '-' || c == '+' || c == '~')
             , Just (' ', _) <- T.uncons rest
             , let arg = T.strip rest
             , not (T.null arg)
             , T.all (\ch -> ch /= ' ' && ch /= '\t') arg
             -> Just (T.singleton c, arg)
           _ -> Nothing

-------------------------------------------------------------------------------
-- Type declaration extraction
-------------------------------------------------------------------------------

-- | Extract type declarations from HLDS dump lines.
-- Looks for "% type ctor:" markers and the ":- type" declarations that
-- follow in the type table section of the dump.
extractTypeDecls :: [Text] -> [MercuryTypeDecl]
extractTypeDecls [] = []
extractTypeDecls (l:ls)
  -- Match ":- type T(params) ---> ctor1 ; ctor2 ; ..."
  -- The HLDS dump contains type definitions in the format:
  --   :- type tree(T) ---> empty ; node(T, tree(T), tree(T)).
  | ":- type " `T.isPrefixOf` T.stripStart l =
      case parseTypeDecl (T.strip l) of
        Just td -> td : extractTypeDecls ls
        Nothing -> extractTypeDecls ls
  -- Also look for "% type ctor:" markers in the HLDS
  | "% type ctor: " `T.isPrefixOf` T.stripStart l =
      case parseTypeCtorLine (T.strip l) ls of
        (Just td, rest) -> td : extractTypeDecls rest
        (Nothing, rest) -> extractTypeDecls rest
  | otherwise = extractTypeDecls ls

-- | Parse a ":- type T(params) ---> ctor1 ; ctor2." line
parseTypeDecl :: Text -> Maybe MercuryTypeDecl
parseTypeDecl line = do
  let afterType = T.drop (T.length ":- type ") line
      -- Split on "--->" to get the LHS (name + params) and RHS (constructors)
      (lhs, rest) = T.breakOn "--->" afterType
  if T.null rest
    then Nothing  -- no "--->" found
    else do
      let rhs = T.drop (T.length "--->") rest
          -- Parse LHS: name(Param1, Param2) or just name
          lhsStripped = T.strip lhs
          (typeName, params) = parseTypeLhs lhsStripped
          -- Parse RHS: ctor1 ; ctor2(arg1, arg2) ; ...
          ctorTexts = map T.strip $ T.splitOn ";" (T.dropWhileEnd (== '.') (T.strip rhs))
          ctors = map parseCtorText ctorTexts
      Just $ MercuryTypeDecl
        { typeDeclName = typeName
        , typeDeclParams = params
        , typeDeclCtors = ctors
        }

-- | Parse the LHS of a type decl: "tree(T)" -> ("tree", ["T"])
parseTypeLhs :: Text -> (Text, [Text])
parseTypeLhs t
  | "(" `T.isInfixOf` t =
      let name = T.takeWhile (/= '(') t
          paramsText = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') t
          params = map T.strip $ T.splitOn "," paramsText
      in (T.strip name, filter (not . T.null) params)
  | otherwise = (T.strip t, [])

-- | Parse a constructor text: "node(T, tree(T), tree(T))" -> ("node", ["T", "tree(T)", "tree(T)"])
-- or "empty" -> ("empty", [])
parseCtorText :: Text -> (Text, [Text])
parseCtorText t
  | "(" `T.isInfixOf` stripped =
      let name = T.takeWhile (/= '(') stripped
          argsText = T.dropEnd 1 $ T.drop 1 $ T.dropWhile (/= '(') stripped
          -- Simple split on ", " — not perfect for nested parens but good enough
          args = map T.strip $ splitCtorArgs argsText
      in (T.strip name, filter (not . T.null) args)
  | otherwise = (stripped, [])
  where stripped = T.strip t

-- | Split constructor arguments, respecting nested parentheses.
splitCtorArgs :: Text -> [Text]
splitCtorArgs t = go 0 T.empty (T.unpack t)
  where
    go :: Int -> Text -> String -> [Text]
    go _ acc [] = [acc]
    go depth acc ('(':rest) = go (depth + 1) (acc <> "(") rest
    go depth acc (')':rest) = go (depth - 1) (acc <> ")") rest
    go 0 acc (',':' ':rest) = acc : go 0 T.empty rest
    go 0 acc (',':rest) = acc : go 0 T.empty rest
    go depth acc (c:rest) = go depth (acc <> T.singleton c) rest

-- | Parse a "% type ctor:" marker line and collect the subsequent type body.
parseTypeCtorLine :: Text -> [Text] -> (Maybe MercuryTypeDecl, [Text])
parseTypeCtorLine marker ls =
  -- "% type ctor: module.typename/arity"
  let afterMarker = T.drop (T.length "% type ctor: ") marker
      -- Extract name: "module.typename/arity" -> "typename"
      nameWithModule = T.takeWhile (/= '/') afterMarker
      typeName = case T.breakOnEnd "." nameWithModule of
                   ("", n) -> n
                   (_, n)  -> n
      -- Look ahead for constructor lines (indented, before the next % marker)
      (bodyLines, rest) = span (\x -> not ("% type ctor:" `T.isPrefixOf` T.stripStart x)
                                    && not ("% pred id" `T.isPrefixOf` T.stripStart x)) ls
      -- Try to find ":- type" in bodyLines
      typeLines = filter (":- type" `T.isInfixOf`) bodyLines
  in case typeLines of
    (tl:_) -> (parseTypeDecl (T.strip tl), rest)
    [] ->
      -- No ":- type" line found; create a stub with just the name
      (Just $ MercuryTypeDecl typeName [] [], rest)
