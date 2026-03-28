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
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (listDirectory, getCurrentDirectory)
import System.FilePath (takeBaseName, (</>))
import Data.List (isPrefixOf, find)
import Control.Exception (try, IOException)

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
dumpHlds :: FilePath -> IO (Either Text Text)
dumpHlds inputPath = do
  let moduleName = takeBaseName inputPath
  -- mmc dumps to cwd, so run from a temp-like location
  result <- try $ readProcessWithExitCode "mmc"
    [ "--dump-hlds", "50"
    , "--compile-only"
    , inputPath
    ] ""
  case result :: Either IOException (ExitCode, String, String) of
    Left exc -> pure $ Left $ T.pack $ "Failed to invoke mmc: " ++ show exc
    Right (ExitFailure code, _, stderr) ->
      pure $ Left $ T.pack $ "mmc failed (exit " ++ show code ++ "): " ++ stderr
    Right (ExitSuccess, _, _) -> do
      -- Find the dump file
      cwd <- getCurrentDirectory
      files <- listDirectory cwd
      let dumpFile = find (\f -> (moduleName ++ ".hlds_dump") `isPrefixOf` f) files
      case dumpFile of
        Nothing -> pure $ Left "HLDS dump file not found after mmc"
        Just f -> do
          contents <- TIO.readFile (cwd </> f)
          pure $ Right contents

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
      in parsePredBlock l block : extractPredicates rest
  | otherwise = extractPredicates ls

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
    }

extractNameArity :: Text -> (Text, Int)
extractNameArity header =
  -- Look for `name'/N pattern
  case T.breakOn "'" header of
    (_, rest) | not (T.null rest) ->
      let afterQuote = T.drop 1 rest  -- after opening quote
          name = T.takeWhile (/= '\'') afterQuote
          afterName = T.drop 1 $ T.dropWhile (/= '\'') afterQuote  -- after closing quote
          afterSlash = T.drop 1 $ T.dropWhile (/= '/') afterName
          arity = case reads (T.unpack (T.takeWhile (\c -> c >= '0' && c <= '9') afterSlash)) of
            [(n, _)] -> n
            _ -> 0
      in (name, arity)
    _ -> ("unknown", 0)

parseModeDecl :: Text -> ([MercuryMode], MercuryDet)
parseModeDecl line =
  -- ":- mode name(in, in, out) is det."
  let afterParen = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') line
      modes = map parseMode $ T.splitOn "," afterParen
      -- Extract determinism after "is"
      afterIs = T.strip $ T.drop 1 $ snd $ T.breakOn " is " line
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
extractGoalText :: [Text] -> Text
extractGoalText ls =
  -- The goal starts after "module.pred(args) :-" and ends at ")."
  let afterClauseHead = dropWhile (not . T.isInfixOf ":-") ls
      goalLines = case afterClauseHead of
        (_:rest) -> rest
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
    _ | any ("% cannot_fail switch on" `T.isInfixOf`) stripped ||
        any ("% switch on" `T.isInfixOf`) stripped ->
          parseSwitch stripped
      | any ("% conjunction" `T.isInfixOf`) stripped &&
        length conjParts > 1 ->
          GoalConj (map parseGoalLines conjParts)
      | any ("% disjunction" `T.isInfixOf`) stripped ||
        (length disjParts > 1) ->
          GoalDisj (map parseGoalLines disjParts)
      | otherwise -> parseSingleGoal (T.unlines (filter (not . isComment) stripped))

isComment :: Text -> Bool
isComment t = "%" `T.isPrefixOf` T.strip t || T.null (T.strip t) ||
              "(" == T.strip t || ")" == T.strip t || ")." == T.strip t

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
  -- Constructor: Var = module.functor[Args | Tail] or Var = module.functor(Args)
  | " = " `T.isInfixOf` stripped =
      let (lhs, rhs) = T.breakOn " = " stripped
          rhs' = T.drop 3 rhs
      in if "." `T.isInfixOf` rhs' || "[" `T.isInfixOf` rhs'
         then GoalConstruct (T.strip lhs) (T.strip rhs') []
         else GoalUnify (T.strip lhs) (T.strip rhs')
  -- Predicate call: module.pred(args)
  | "(" `T.isInfixOf` stripped =
      let name = T.takeWhile (/= '(') stripped
          argsText = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') stripped
          args = map T.strip $ T.splitOn "," argsText
      in GoalCall (T.strip name) args
  | otherwise = GoalUnparsed stripped
  where stripped = T.strip txt

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
