-- | Cross-Module Linker
--
-- Merges multiple Frankenstein Core programs (potentially from different
-- source languages) into a single unified program. This is the key piece
-- that enables polyglot compilation.
--
-- Name resolution strategy:
--   1. Each source module's definitions are qualified by their module name
--   2. Cross-references between modules use qualified names
--   3. Duplicate definitions (same QName) are flagged as errors
--   4. A "main" function is located across all modules to serve as entry point

module Frankenstein.Core.Linker
  ( linkPrograms
  , linkProgramsWith
  , LinkError(..)
  , LinkResult(..)
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (partition)

-- | Errors that can occur during linking
data LinkError
  = DuplicateDefinition QName Text Text  -- ^ name, module1, module2
  | NoMainFunction                        -- ^ No 'main' found in any module
  | MultipleMainFunctions [Text]          -- ^ 'main' in multiple modules
  deriving (Show, Eq)

-- | Result of linking
data LinkResult = LinkResult
  { lrProgram    :: !Program
  , lrWarnings   :: ![Text]
  , lrMainModule :: !Text
  } deriving (Show, Eq)

-- | Link multiple programs into a single unified program.
--
-- Each program typically comes from a different source file/language.
-- The linker merges all definitions, data declarations, and effect
-- declarations, qualifying names to avoid collisions.
--
-- Set requireMain=True when compiling to an executable.
-- Set requireMain=False when just emitting Core or MLIR.
linkPrograms :: [Program] -> Either [LinkError] LinkResult
linkPrograms = linkProgramsWith True

linkProgramsWith :: Bool -> [Program] -> Either [LinkError] LinkResult
linkProgramsWith _requireMain [] = Left [NoMainFunction]
linkProgramsWith requireMain [p] =
  case findMain [p] of
    Right mainMod -> Right LinkResult
      { lrProgram = p
      , lrWarnings = []
      , lrMainModule = mainMod
      }
    Left _ | not requireMain -> Right LinkResult
      { lrProgram = p
      , lrWarnings = ["No main function found"]
      , lrMainModule = qnameToText (progName p)
      }
    Left err -> Left [err]
linkProgramsWith requireMain progs =
  let -- Collect and qualify all definitions
      allDefs = concatMap qualifyDefs progs
      allData = concatMap progData progs
      allEffects = concatMap progEffects progs

      -- Check for duplicates
      dupes = findDuplicates allDefs

      -- Find main
      mainResult = findMain progs

      -- Build unified module name
      modNames = map (qnameToText . progName) progs
      unifiedName = QName "frankenstein"
                          (Name (T.intercalate "+" modNames) 0)

      mkResult mainMod warns = Right LinkResult
        { lrProgram = Program
            { progName    = unifiedName
            , progDefs    = allDefs
            , progData    = allData
            , progEffects = allEffects
            }
        , lrWarnings = warns
        , lrMainModule = mainMod
        }

  in case (dupes, mainResult) of
    ([], Right mainMod) -> mkResult mainMod []
    ([], Left _) | not requireMain ->
      mkResult (T.intercalate "+" modNames) ["No main function found"]
    ([], Left err) -> Left [err]
    (errs, Left err) -> Left (errs ++ [err])
    (errs, _) -> Left errs

-- | Qualify definitions with their module name if not already qualified.
qualifyDefs :: Program -> [Def]
qualifyDefs prog =
  let modName = qnameModule (progName prog)
  in map (qualifyDef modName) (progDefs prog)

qualifyDef :: Text -> Def -> Def
qualifyDef modName def
  | T.null (qnameModule (defName def)) =
      def { defName = (defName def) { qnameModule = modName } }
  | otherwise = def

-- | Find duplicate definitions across different modules.
-- Duplicates within the same module are allowed (e.g. Mercury arity overloads).
findDuplicates :: [Def] -> [LinkError]
findDuplicates defs =
  let indexed = [(defName d, qnameModule (defName d)) | d <- defs]
      grouped = Map.fromListWith (++) [(n, [m]) | (n, m) <- indexed]
      -- Only flag as duplicate if the same QName appears in different modules
      crossModDupes = [ (n, ms)
                      | (n, ms) <- Map.toList grouped
                      , let uniqMods = nub ms
                      , length uniqMods > 1
                      ]
  in [ DuplicateDefinition n (head (nub ms)) ((nub ms) !! 1)
     | (n, ms) <- crossModDupes ]
  where
    nub [] = []
    nub (x:xs) = x : nub (filter (/= x) xs)

-- | Find which module contains 'main'.
findMain :: [Program] -> Either LinkError Text
findMain progs =
  let modsWithMain =
        [ qnameToText (progName p)
        | p <- progs
        , any isMainDef (progDefs p)
        ]
  in case modsWithMain of
    []  -> Left NoMainFunction
    [m] -> Right m
    ms  -> Left (MultipleMainFunctions ms)

isMainDef :: Def -> Bool
isMainDef d = nameText (qnameName (defName d)) == "main"

qnameToText :: QName -> Text
qnameToText qn
  | T.null (qnameModule qn) = nameText (qnameName qn)
  | otherwise = qnameModule qn <> "." <> nameText (qnameName qn)
