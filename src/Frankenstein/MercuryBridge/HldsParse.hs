-- | Mercury HLDS Parser
--
-- Parses the textual HLDS dump produced by `mmc --dump-hlds 50`.
-- The HLDS contains Mercury programs after type/mode/determinism checking.

module Frankenstein.MercuryBridge.HldsParse
  ( MercuryHLDS(..)
  , MercuryPred(..)
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

data MercuryHLDS = MercuryHLDS
  { hldsModule :: !Text
  , hldsPreds  :: ![MercuryPred]
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
  Right $ MercuryHLDS
    { hldsModule = modName
    , hldsPreds = preds
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
