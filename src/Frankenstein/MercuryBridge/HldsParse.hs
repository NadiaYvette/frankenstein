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
import System.Directory (listDirectory, getTemporaryDirectory, createDirectoryIfMissing, makeAbsolute, copyFile, removeDirectoryRecursive, doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, takeFileName, (</>))
import Data.List (isPrefixOf, find)
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
  -- Dump HLDS for each module in the shared workDir.
  dumps <- mapM (\m -> do
                    d <- dumpInWorkDir workDir (T.unpack m ++ ".m")
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
dumpInWorkDir workDir fileBase = do
  let moduleName = takeBaseName fileBase
  result <- try $ readCreateProcessWithExitCode
    (proc "mmc" [ "--dump-hlds", "50", "--compile-only", fileBase ])
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
      goal = parseGoalText goalText

  in MercuryPred
    { predName = name
    , predArity = arity
    , predDet = det
    , predModes = modes
    , predArgTypes = []
    , predGoal = Just goal
    , predArgNames = argNames
    }

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
  let afterParen = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') line
      paramModes = map parseMode $ T.splitOn "," afterParen
      afterCloseParen = T.drop 1 $ T.dropWhile (/= ')') line
      (gapBeforeIs, _) = T.breakOn " is " afterCloseParen
      retModes = case T.stripPrefix "=" (T.strip gapBeforeIs) of
        Just rest -> [parseMode (T.strip rest)]
        Nothing   -> []
      modes = paramModes ++ retModes
      afterIs = T.strip $ T.drop 4 $ snd $ T.breakOn " is " line
      det = parseDet $ T.takeWhile (/= '.') afterIs
  in (modes, det)

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
parseGoalLines ls =
  let stripped = map T.strip ls
      -- Look for structural markers
      conjParts = splitOnMarker "," stripped
      disjParts = splitOnMarker ";" stripped
  in case ls of
    _ | Just (condLs, thenLs, elseLs) <- splitIfThenElse stripped ->
          GoalIfThenElse (parseGoalLines condLs)
                         (parseGoalLines thenLs)
                         (parseGoalLines elseLs)
      | any ("% cannot_fail switch on" `T.isInfixOf`) stripped ||
        any ("% switch on" `T.isInfixOf`) stripped ->
          parseSwitch stripped
      | any ("% conjunction" `T.isInfixOf`) stripped &&
        length conjParts > 1 ->
          GoalConj (map parseGoalLines conjParts)
      | length disjParts > 1 ->
          GoalDisj (map parseGoalLines disjParts)
      | otherwise -> parseSingleGoal (T.unlines (filter (not . isComment) stripped))

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
                  -- Else block extends to the last meaningful line; trim
                  -- trailing ')' which closes the enclosing ITE paren.
                  rest   = drop (eIdx + 1) strippedLs
                  trim t = T.dropWhileEnd (`elem` (" )." :: String)) (T.strip t)
                  elseLs = reverse (dropWhile (\l -> T.null (trim l)) (reverse rest))
              in Just (condLs, thenLs, elseLs)
            _ -> Nothing

isComment :: Text -> Bool
isComment t = let s = T.strip t
              in "%" `T.isPrefixOf` s || T.null s ||
                 s == "(" || s == ")" || s == ")." ||
                 -- Mercury wraps disjunctions in "( % disjunction ... )" —
                 -- treat the opening paren+comment as a comment line.
                 ("(" `T.isPrefixOf` s && "%" `T.isInfixOf` s)

-- Split a list of lines on a delimiter that appears as a standalone line
splitOnMarker :: Text -> [Text] -> [[Text]]
splitOnMarker _ [] = []
splitOnMarker marker ls =
  let (chunk, rest) = span (\l -> T.strip l /= marker) ls
  in case rest of
    [] -> [chunk]
    (_:more) -> chunk : splitOnMarker marker more

parseSwitch :: [Text] -> MercuryGoal
parseSwitch ls =
  -- Find the switch variable from "switch on `Var'"
  let switchLine = head $ filter (\l -> "switch on" `T.isInfixOf` l) ls
      varName = T.takeWhile (/= '\'') $ T.drop 1 $ snd $ T.breakOn "`" switchLine
      -- Split arms at "has functor" markers
      arms = extractSwitchArms ls
  in GoalSwitch varName arms

extractSwitchArms :: [Text] -> [(Text, MercuryGoal)]
extractSwitchArms [] = []
extractSwitchArms (l:ls)
  | "has functor" `T.isInfixOf` l =
      let functor = T.strip $ snd $ T.breakOnEnd "functor " l
          (armLines, rest) = span (\x -> not ("has functor" `T.isInfixOf` x)) ls
          goal = parseGoalLines armLines
      in (functor, goal) : extractSwitchArms rest
  | otherwise = extractSwitchArms ls

parseSingleGoal :: Text -> MercuryGoal
parseSingleGoal txt
  | T.null stripped = GoalUnparsed "(empty)"
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
              -- Plain list literal syntax — keep the old best-effort path.
              Nothing | "[" `T.isInfixOf` rhs' -> GoalConstruct lhs' rhs' []
              -- Module-qualified no-arg call: 'V = integer.zero' should
              -- bind V to a call of integer.zero, not unify V with the
              -- string 'integer.zero'.  Detect a clean 'module.name'
              -- shape and emit GoalConstruct with zero args.
              Nothing | "." `T.isInfixOf` rhs'
                      , isCleanCallName rhs'
                          -> GoalConstruct lhs' rhs' []
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
-- @module.ctor(arg1, arg2, ...)@, returning the bare constructor name and
-- the argument list. Returns 'Nothing' for anything that does not look like
-- a functor application (plain atoms, literals, variables, infix exprs).
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
         else Just (bareName, args)
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
      let ops = [ " // ", " >= ", " =< ", " == ", " mod ", " rem "
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
