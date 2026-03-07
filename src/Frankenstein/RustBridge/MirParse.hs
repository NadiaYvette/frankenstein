-- | Rust MIR Parser
--
-- Parses the JSON output of the frankenstein-rustc-shim, which serializes
-- rustc's MIR (Mid-level Intermediate Representation) after borrow checking
-- and monomorphization.
--
-- MIR is a CFG of basic blocks. Each block contains statements and a terminator.
-- Ownership is already resolved: Move, Copy, Drop are explicit.

module Frankenstein.RustBridge.MirParse
  ( MirProgram(..)
  , MirBody(..)
  , MirLocalDecl(..)
  , MirBasicBlock(..)
  , MirMutability(..)
  , parseMirJson
  , parseMirText
  , dumpMir
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (try, IOException)

-- | Mutability flag
data MirMutability = MirMut | MirNot
  deriving (Show, Eq)

-- | A local variable declaration in MIR
data MirLocalDecl = MirLocalDecl
  { localIndex      :: !Int
  , localType       :: !Text    -- Type as string (for now)
  , localMutability :: !MirMutability
  } deriving (Show)

-- | A basic block in MIR
data MirBasicBlock = MirBasicBlock
  { bbIndex      :: !Int
  , bbStatements :: ![Text]   -- Statements as strings (for now)
  , bbTerminator :: !(Maybe Text)  -- Terminator as string
  } deriving (Show)

-- | A MIR function body
data MirBody = MirBody
  { mirName      :: !Text
  , mirArgCount  :: !Int
  , mirLocals    :: ![MirLocalDecl]
  , mirBlocks    :: ![MirBasicBlock]
  } deriving (Show)

-- | A full MIR program (multiple function bodies)
data MirProgram = MirProgram
  { mirBodies :: ![MirBody]
  } deriving (Show)

-- | Invoke the rustc shim to get MIR JSON for a Rust source file.
dumpMir :: FilePath -> IO (Either Text Text)
dumpMir inputPath = do
  -- Try the shim first; fall back to rustc -Z unpretty=mir
  shimResult <- try $ readProcessWithExitCode "rustc-mir-dump" [inputPath] ""
  case shimResult :: Either IOException (ExitCode, String, String) of
    Right (ExitSuccess, stdout, _) -> pure $ Right $ T.pack stdout
    _ -> do
      -- Fallback: use rustc's built-in MIR dump (needs nightly)
      rustcResult <- try $ readProcessWithExitCode "rustup"
        [ "run", "nightly", "rustc"
        , "--edition=2024"
        , "-Z", "unpretty=mir"
        , inputPath
        ] ""
      case rustcResult :: Either IOException (ExitCode, String, String) of
        Right (ExitSuccess, out2, _) -> pure $ Right $ T.pack out2
        Right (ExitFailure code, _, err2) ->
          pure $ Left $ T.pack $ "rustc MIR dump failed (exit " ++ show code ++ "): " ++ err2
        Left exc ->
          pure $ Left $ T.pack $ "Failed to invoke rustc: " ++ show exc

-- | Parse MIR JSON from the rustc shim output.
-- For now, this is a simplified parser. When we have proper JSON
-- from the shim, we'll use aeson.
parseMirJson :: Text -> Either Text MirProgram
parseMirJson jsonText =
  -- Phase 1: Parse the simplified JSON format from our rustc shim
  -- Phase 2: Use aeson for proper JSON parsing
  --
  -- The JSON structure is:
  -- [ { "name": "function_name",
  --     "arg_count": N,
  --     "local_decls": [ { "index": N, "ty": "TypeStr", "mutability": "mut"|"not" } ],
  --     "basic_blocks": [ { "index": N, "statements": ["..."], "terminator": "..." } ]
  --   }, ... ]
  Right $ MirProgram { mirBodies = [] }  -- TODO: implement JSON parsing

-- | Parse MIR from rustc's textual MIR dump (fallback format).
-- The textual format looks like:
--
-- fn function_name(_1: Type1, _2: Type2) -> RetType {
--     let mut _0: RetType;
--     let mut _3: TempType;
--
--     bb0: {
--         _3 = move _1;
--         _0 = Add(move _3, _2);
--         return;
--     }
-- }
parseMirText :: Text -> Either Text MirProgram
parseMirText mirText = do
  let ls = T.lines mirText
      bodies = extractBodies ls
  Right $ MirProgram { mirBodies = bodies }

extractBodies :: [Text] -> [MirBody]
extractBodies [] = []
extractBodies (l:ls)
  | "fn " `T.isPrefixOf` T.stripStart l =
      let (bodyLines, rest) = collectBlock 0 ls
          body = parseBody (T.stripStart l) bodyLines
      in body : extractBodies rest
  | otherwise = extractBodies ls

-- Collect lines until matching closing brace
collectBlock :: Int -> [Text] -> ([Text], [Text])
collectBlock _ [] = ([], [])
collectBlock depth (l:ls)
  | "{" `T.isSuffixOf` T.stripEnd l = let (rest, remaining) = collectBlock (depth + 1) ls
                                        in (l : rest, remaining)
  | "}" `T.isPrefixOf` T.stripStart l && depth <= 1 = ([l], ls)
  | "}" `T.isPrefixOf` T.stripStart l = let (rest, remaining) = collectBlock (depth - 1) ls
                                          in (l : rest, remaining)
  | otherwise = let (rest, remaining) = collectBlock depth ls
                in (l : rest, remaining)

parseBody :: Text -> [Text] -> MirBody
parseBody headerLine bodyLines =
  let -- Extract function name from "fn name(_1: T1, ...) -> Ret {"
      nameEnd = T.takeWhile (/= '(') (T.drop 3 headerLine)
      name = T.strip nameEnd

      -- Count arguments from the header
      argsPart = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') headerLine
      argCount = if T.null argsPart then 0
                 else length (T.splitOn "," argsPart)

      -- Parse local declarations and basic blocks
      (locals, blocks) = parseBodyContents bodyLines

  in MirBody
    { mirName = name
    , mirArgCount = argCount
    , mirLocals = locals
    , mirBlocks = blocks
    }

parseBodyContents :: [Text] -> ([MirLocalDecl], [MirBasicBlock])
parseBodyContents ls =
  let -- Local decls start with "let"
      localLines = filter (\l -> "let " `T.isPrefixOf` T.stripStart l) ls
      locals = zipWith parseLocalDecl [0..] localLines

      -- Basic blocks start with "bbN:"
      blockStarts = [(i, l) | (i, l) <- zip [0..] ls, "bb" `T.isPrefixOf` T.stripStart l && ":" `T.isSuffixOf` T.stripEnd l]
      blocks = []  -- TODO: parse basic block contents
  in (locals, blocks)

parseLocalDecl :: Int -> Text -> MirLocalDecl
parseLocalDecl idx line =
  let stripped = T.stripStart line
      isMut = "let mut" `T.isPrefixOf` stripped
      -- Extract type from "let [mut] _N: Type;"
      afterColon = T.strip $ T.drop 1 $ T.dropWhile (/= ':') stripped
      ty = T.takeWhile (/= ';') afterColon
  in MirLocalDecl
    { localIndex = idx
    , localType = T.strip ty
    , localMutability = if isMut then MirMut else MirNot
    }
