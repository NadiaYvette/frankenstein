-- | Rust MIR Parser
--
-- Parses the JSON output of the frankenstein-rustc-shim, which serializes
-- rustc's MIR (Mid-level Intermediate Representation) after borrow checking
-- and monomorphization.
--
-- MIR is a CFG of basic blocks. Each block contains statements and a terminator.
-- Ownership is already resolved: Move, Copy, Drop are explicit.
--
-- Since we can't rely on aeson (GHC 9.14.1 compatibility), this module
-- includes a hand-rolled JSON parser sufficient for the shim output format.

module Frankenstein.RustBridge.MirParse
  ( MirProgram(..)
  , MirBody(..)
  , MirLocalDecl(..)
  , MirBasicBlock(..)
  , MirMutability(..)
  , MirStmt(..)
  , MirRvalue(..)
  , MirOperand(..)
  , MirTerminator(..)
  , MirSwitchTarget(..)
  , parseMirJson
  , parseMirText
  , parseStmt
  , parseTerminator
  , parseOperand
  , dumpMir
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Char (isDigit, isSpace, isAlpha, isAlphaNum)
import Data.List (find)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import Control.Exception (try, IOException)

-- | Mutability flag
data MirMutability = MirMut | MirNot
  deriving (Show, Eq)

-- | A local variable declaration in MIR
data MirLocalDecl = MirLocalDecl
  { localIndex      :: !Int
  , localType       :: !Text    -- Type as string
  , localMutability :: !MirMutability
  } deriving (Show)

-- | A basic block in MIR
data MirBasicBlock = MirBasicBlock
  { bbIndex      :: !Int
  , bbStatements :: ![Text]        -- Raw statement strings
  , bbTerminator :: !(Maybe Text)  -- Raw terminator string
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

-- | Parsed MIR operand (move vs copy vs const)
data MirOperand
  = OpMove !Int             -- ^ move _N
  | OpCopy !Int             -- ^ copy _N
  | OpConst !Text           -- ^ const literal
  | OpFieldAccess !Int !Int !Text  -- ^ move/copy (_N.F: Type)
  | OpRef !Int              -- ^ &_N
  deriving (Show, Eq)

-- | Parsed MIR rvalue
data MirRvalue
  = RvUse !MirOperand                     -- ^ Simple use of an operand
  | RvBinOp !Text !MirOperand !MirOperand -- ^ Binary operation: Eq, Sub, Mul, Add, etc.
  | RvWithOverflow !Text !MirOperand !MirOperand -- ^ SubWithOverflow, MulWithOverflow, etc.
  | RvAggregate ![MirOperand]             -- ^ Tuple/array aggregate
  | RvRef !Int                            -- ^ &_N
  | RvFieldAccess !Int !Int !Text         -- ^ (_N.F: Type) - field projection
  | RvRaw !Text                           -- ^ Unparsed rvalue
  deriving (Show, Eq)

-- | Parsed MIR statement
data MirStmt
  = StmtAssign !Int !MirRvalue  -- ^ Assign(_N, rvalue)
  | StmtRaw !Text               -- ^ Unparsed statement
  deriving (Show, Eq)

-- | Switch target: value -> block
data MirSwitchTarget = MirSwitchTarget
  { switchVal   :: !(Maybe Integer)  -- Nothing = otherwise
  , switchBlock :: !Int
  } deriving (Show, Eq)

-- | Parsed MIR terminator
data MirTerminator
  = TermReturn                                    -- ^ return
  | TermGoto !Int                                 -- ^ goto -> bbN
  | TermSwitchInt !MirOperand ![MirSwitchTarget]  -- ^ switchInt(op) -> [...]
  | TermCall !Text ![MirOperand] !Int !(Maybe Int) !Int -- ^ func(args) -> [return: bbN, unwind: bbM], dest _N
  | TermCallSimple !Text ![Text] !Int             -- ^ Simplified call: func(args) -> bbN, dest _N
  | TermAssert !Text !Int                         -- ^ assert(...) -> [success: bbN, ...]
  | TermDrop !Int !Int                            -- ^ Drop(_N) -> bbM
  | TermRaw !Text                                 -- ^ Unparsed terminator
  deriving (Show, Eq)

------------------------------------------------------------------------
-- Hand-rolled JSON parser
------------------------------------------------------------------------

-- | A minimal JSON value type
data JValue
  = JNull
  | JBool !Bool
  | JNum !Integer
  | JStr !Text
  | JArr ![JValue]
  | JObj ![(Text, JValue)]
  deriving (Show)

type ParseResult a = Either Text (a, Text)

-- | Skip whitespace
skipWs :: Text -> Text
skipWs = T.dropWhile isSpace

-- | Parse a JSON value
parseJValue :: Text -> ParseResult JValue
parseJValue input =
  let s = skipWs input
  in case T.uncons s of
    Nothing -> Left "Unexpected end of input"
    Just ('"', _) -> parseJString s
    Just ('[', _) -> parseJArray s
    Just ('{', _) -> parseJObject s
    Just ('t', _) | "true" `T.isPrefixOf` s -> Right (JBool True, T.drop 4 s)
    Just ('f', _) | "false" `T.isPrefixOf` s -> Right (JBool False, T.drop 5 s)
    Just ('n', _) | "null" `T.isPrefixOf` s -> Right (JNull, T.drop 4 s)
    Just (c, _) | c == '-' || isDigit c -> parseJNum s
    Just (c, _) -> Left $ "Unexpected character: " <> T.singleton c

-- | Parse a JSON string
parseJString :: Text -> ParseResult JValue
parseJString input = case T.uncons input of
  Just ('"', rest) ->
    let (str, after) = parseStringContents rest
    in Right (JStr str, after)
  _ -> Left "Expected '\"'"

-- | Parse string contents, handling escape sequences
parseStringContents :: Text -> (Text, Text)
parseStringContents t = go T.empty t
  where
    go acc s = case T.uncons s of
      Nothing -> (acc, s)
      Just ('"', rest) -> (acc, rest)
      Just ('\\', rest) -> case T.uncons rest of
        Just ('"', rest2) -> go (acc <> "\"") rest2
        Just ('\\', rest2) -> go (acc <> "\\") rest2
        Just ('/', rest2) -> go (acc <> "/") rest2
        Just ('n', rest2) -> go (acc <> "\n") rest2
        Just ('t', rest2) -> go (acc <> "\t") rest2
        Just ('r', rest2) -> go (acc <> "\r") rest2
        Just ('b', rest2) -> go (acc <> "\b") rest2
        Just ('f', rest2) -> go (acc <> "\f") rest2
        Just (c, rest2) -> go (acc <> "\\" <> T.singleton c) rest2
        Nothing -> (acc, s)
      Just (c, rest) -> go (acc <> T.singleton c) rest

-- | Parse a JSON number (integers only for our purposes)
parseJNum :: Text -> ParseResult JValue
parseJNum input =
  let (neg, rest) = case T.uncons input of
        Just ('-', r) -> (True, r)
        _             -> (False, input)
      (digits, after) = T.span isDigit rest
  in if T.null digits
     then Left "Expected digit"
     else
       let n = read (T.unpack digits) :: Integer
           val = if neg then negate n else n
       in Right (JNum val, after)

-- | Parse a JSON array
parseJArray :: Text -> ParseResult JValue
parseJArray input = case T.uncons input of
  Just ('[', rest) ->
    let s = skipWs rest
    in case T.uncons s of
      Just (']', after) -> Right (JArr [], after)
      _ -> parseArrayElems s []
  _ -> Left "Expected '['"

parseArrayElems :: Text -> [JValue] -> ParseResult JValue
parseArrayElems input acc = do
  (val, rest) <- parseJValue input
  let s = skipWs rest
  case T.uncons s of
    Just (',', rest2) -> parseArrayElems (skipWs rest2) (acc ++ [val])
    Just (']', rest2) -> Right (JArr (acc ++ [val]), rest2)
    _ -> Left $ "Expected ',' or ']' in array, got: " <> T.take 20 s

-- | Parse a JSON object
parseJObject :: Text -> ParseResult JValue
parseJObject input = case T.uncons input of
  Just ('{', rest) ->
    let s = skipWs rest
    in case T.uncons s of
      Just ('}', after) -> Right (JObj [], after)
      _ -> parseObjFields s []
  _ -> Left "Expected '{'"

parseObjFields :: Text -> [(Text, JValue)] -> ParseResult JValue
parseObjFields input acc = do
  result <- parseJString (skipWs input)
  case result of { (JStr key, rest1) -> parseObjField key rest1 acc; _ -> Left "Expected string key in object" }

parseObjField :: Text -> Text -> [(Text, JValue)] -> ParseResult JValue
parseObjField key rest1 acc = do
  let s1 = skipWs rest1
  case T.uncons s1 of
    Just (':', rest2) -> do
      (val, rest3) <- parseJValue (skipWs rest2)
      let s2 = skipWs rest3
      case T.uncons s2 of
        Just (',', rest4) -> parseObjFields (skipWs rest4) (acc ++ [(key, val)])
        Just ('}', rest4) -> Right (JObj (acc ++ [(key, val)]), rest4)
        _ -> Left $ "Expected ',' or '}' in object, got: " <> T.take 30 s2
    _ -> Left "Expected ':' in object"

-- | Lookup a key in a JSON object
jLookup :: Text -> [(Text, JValue)] -> Maybe JValue
jLookup key = fmap snd . find ((== key) . fst)

-- | Extract a string from JValue
jStr :: JValue -> Maybe Text
jStr (JStr s) = Just s
jStr _ = Nothing

-- | Extract an integer from JValue
jInt :: JValue -> Maybe Int
jInt (JNum n) = Just (fromIntegral n)
jInt _ = Nothing

-- | Extract a bool from JValue
jBool :: JValue -> Maybe Bool
jBool (JBool b) = Just b
jBool _ = Nothing

-- | Extract an array from JValue
jArr :: JValue -> Maybe [JValue]
jArr (JArr xs) = Just xs
jArr _ = Nothing

-- | Extract an object from JValue
_jObj :: JValue -> Maybe [(Text, JValue)]
_jObj (JObj xs) = Just xs
_jObj _ = Nothing

------------------------------------------------------------------------
-- JSON -> MIR types
------------------------------------------------------------------------

-- | Parse MIR JSON from the rustc shim output.
--
-- The JSON structure is:
-- [ { "name": "function_name",
--     "arg_count": N,
--     "locals": [ { "idx": N, "ty": "TypeStr", "mut": true|false } ],
--     "blocks": [ { "idx": N, "stmts": ["..."], "term": "..." } ]
--   }, ... ]
parseMirJson :: Text -> Either Text MirProgram
parseMirJson jsonText = do
  (jval, _) <- parseJValue jsonText
  case jval of
    JArr bodies -> do
      mirBods <- mapM jsonToMirBody bodies
      Right $ MirProgram { mirBodies = mirBods }
    _ -> Left "Expected top-level JSON array"

jsonToMirBody :: JValue -> Either Text MirBody
jsonToMirBody (JObj fields) = do
  name <- case jLookup "name" fields >>= jStr of
    Just n  -> Right n
    Nothing -> Left "Missing 'name' field in MIR body"
  argCount <- case jLookup "arg_count" fields >>= jInt of
    Just n  -> Right n
    Nothing -> Left "Missing 'arg_count' field"
  locals <- case jLookup "locals" fields >>= jArr of
    Just ls -> mapM jsonToLocal ls
    Nothing -> Right []
  blocks <- case jLookup "blocks" fields >>= jArr of
    Just bs -> mapM jsonToBlock bs
    Nothing -> Right []
  Right $ MirBody
    { mirName     = name
    , mirArgCount = argCount
    , mirLocals   = locals
    , mirBlocks   = blocks
    }
jsonToMirBody _ = Left "Expected JSON object for MIR body"

jsonToLocal :: JValue -> Either Text MirLocalDecl
jsonToLocal (JObj fields) = do
  idx <- case jLookup "idx" fields >>= jInt of
    Just n  -> Right n
    Nothing -> Left "Missing 'idx' in local"
  ty <- case jLookup "ty" fields >>= jStr of
    Just t  -> Right t
    Nothing -> Left "Missing 'ty' in local"
  let mut = case jLookup "mut" fields >>= jBool of
              Just True -> MirMut
              _         -> MirNot
  Right $ MirLocalDecl
    { localIndex      = idx
    , localType       = ty
    , localMutability = mut
    }
jsonToLocal _ = Left "Expected JSON object for local decl"

jsonToBlock :: JValue -> Either Text MirBasicBlock
jsonToBlock (JObj fields) = do
  idx <- case jLookup "idx" fields >>= jInt of
    Just n  -> Right n
    Nothing -> Left "Missing 'idx' in block"
  stmts <- case jLookup "stmts" fields >>= jArr of
    Just ss -> Right [s | JStr s <- ss]
    Nothing -> Right []
  let term = jLookup "term" fields >>= jStr
  Right $ MirBasicBlock
    { bbIndex      = idx
    , bbStatements = stmts
    , bbTerminator = term
    }
jsonToBlock _ = Left "Expected JSON object for basic block"

------------------------------------------------------------------------
-- Statement and Terminator string parsers
------------------------------------------------------------------------

-- | Parse a MIR statement string into a structured MirStmt.
--
-- Examples:
--   "Assign((_2, Eq(copy _1, const 0_i64)))"
--   "Assign((_0, const 1_i64))"
--   "Assign((_5, SubWithOverflow(copy _1, const 1_i64)))"
--   "Assign((_4, move (_5.0: i64)))"
--   "Assign((_5, &_1))"
--   "Assign((_4, (move _5,)))"
parseStmt :: Text -> MirStmt
parseStmt t
  | "Assign((" `T.isPrefixOf` t =
      let inner = T.dropEnd 1 $ T.drop 8 t  -- strip "Assign((" and final ")"
          -- inner is like: "_2, Eq(copy _1, const 0_i64))"
          -- Find the target variable number
      in case parsePlace (T.strip inner) of
           Just (dest, rest) ->
             let -- rest starts after ", "
                 rvalStr = T.strip $ T.drop 1 $ T.dropWhile (/= ',') rest
                 -- strip trailing ")"
                 rvalStr' = if ")" `T.isSuffixOf` rvalStr
                            then T.dropEnd 1 rvalStr
                            else rvalStr
             in StmtAssign dest (parseRvalue rvalStr')
           Nothing -> StmtRaw t
  | otherwise = StmtRaw t

-- | Parse a place like "_2" and return (index, remaining text)
parsePlace :: Text -> Maybe (Int, Text)
parsePlace t = case T.uncons t of
  Just ('_', rest) ->
    let (digits, after) = T.span isDigit rest
    in if T.null digits
       then Nothing
       else case reads (T.unpack digits) of
              [(n, _)] -> Just (n, after)
              _        -> Nothing
  _ -> Nothing

-- | Parse an rvalue string
parseRvalue :: Text -> MirRvalue
parseRvalue t
  -- Eq(copy _1, const 0_i64)
  | "Eq(" `T.isPrefixOf` t = parseBinOpRv "Eq" (T.drop 3 t)
  | "Ne(" `T.isPrefixOf` t = parseBinOpRv "Ne" (T.drop 3 t)
  | "Lt(" `T.isPrefixOf` t = parseBinOpRv "Lt" (T.drop 3 t)
  | "Le(" `T.isPrefixOf` t = parseBinOpRv "Le" (T.drop 3 t)
  | "Gt(" `T.isPrefixOf` t = parseBinOpRv "Gt" (T.drop 3 t)
  | "Ge(" `T.isPrefixOf` t = parseBinOpRv "Ge" (T.drop 3 t)
  | "Add(" `T.isPrefixOf` t = parseBinOpRv "Add" (T.drop 4 t)
  | "Sub(" `T.isPrefixOf` t = parseBinOpRv "Sub" (T.drop 4 t)
  | "Mul(" `T.isPrefixOf` t = parseBinOpRv "Mul" (T.drop 4 t)
  | "Div(" `T.isPrefixOf` t = parseBinOpRv "Div" (T.drop 4 t)
  | "Rem(" `T.isPrefixOf` t = parseBinOpRv "Rem" (T.drop 4 t)
  -- Overflow-checking ops
  | "SubWithOverflow(" `T.isPrefixOf` t = parseOverflowRv "Sub" (T.drop 16 t)
  | "MulWithOverflow(" `T.isPrefixOf` t = parseOverflowRv "Mul" (T.drop 16 t)
  | "AddWithOverflow(" `T.isPrefixOf` t = parseOverflowRv "Add" (T.drop 16 t)
  -- Field access: move (_5.0: i64)  or  copy (_4.0: &i64)
  | "move (" `T.isPrefixOf` t = parseFieldAccess t
  | "copy (" `T.isPrefixOf` t = parseFieldAccess t
  -- Simple operands
  | "move _" `T.isPrefixOf` t = case parseOperand t of
      Just op -> RvUse op
      Nothing -> RvRaw t
  | "copy _" `T.isPrefixOf` t = case parseOperand t of
      Just op -> RvUse op
      Nothing -> RvRaw t
  | "const " `T.isPrefixOf` t = RvUse (OpConst (T.drop 6 t))
  -- Reference: &_N
  | "&_" `T.isPrefixOf` t =
      let (digits, _) = T.span isDigit (T.drop 2 t)
      in case reads (T.unpack digits) of
           [(n, _)] -> RvRef n
           _        -> RvRaw t
  -- Tuple aggregate: (move _5,) or [move _7]
  | "(" `T.isPrefixOf` t || "[" `T.isPrefixOf` t =
      let inner = T.dropEnd 1 (T.drop 1 t)  -- strip parens/brackets
          parts = splitOperands inner
          ops = [op | Just op <- map (parseOperand . T.strip) parts]
      in RvAggregate ops
  | otherwise = RvRaw t

-- | Parse binary op from inside parens: "copy _1, const 0_i64)"
parseBinOpRv :: Text -> Text -> MirRvalue
parseBinOpRv op inner =
  let -- strip outer parens
      content = T.strip $ stripParens inner
      parts = splitTopLevel content
  in case parts of
    [a, b] -> case (parseOperand (T.strip a), parseOperand (T.strip b)) of
      (Just opA, Just opB) -> RvBinOp op opA opB
      _ -> RvRaw (op <> "(" <> inner <> ")")
    _ -> RvRaw (op <> "(" <> inner <> ")")

-- | Parse overflow-checking op: "copy _1, const 1_i64)"
parseOverflowRv :: Text -> Text -> MirRvalue
parseOverflowRv op inner =
  let content = T.strip $ stripParens inner
      parts = splitTopLevel content
  in case parts of
    [a, b] -> case (parseOperand (T.strip a), parseOperand (T.strip b)) of
      (Just opA, Just opB) -> RvWithOverflow op opA opB
      _ -> RvRaw (op <> "WithOverflow(" <> inner <> ")")
    _ -> RvRaw (op <> "WithOverflow(" <> inner <> ")")

-- | Parse field access like "move (_5.0: i64)" or "copy (_4.0: &i64)"
parseFieldAccess :: Text -> MirRvalue
parseFieldAccess t =
  let -- Extract everything between ( and )
      inner = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') t
      -- inner is like "_5.0: i64"
  in case T.uncons inner of
    Just ('_', rest) ->
      let (baseDigits, afterBase) = T.span isDigit rest
      in case (reads (T.unpack baseDigits), T.uncons afterBase) of
        ([(baseIdx, _)], Just ('.', rest2)) ->
          let (fieldDigits, afterField) = T.span isDigit rest2
              ty = T.strip $ T.drop 1 $ T.dropWhile (/= ':') afterField
          in case reads (T.unpack fieldDigits) of
            [(fieldIdx, _)] -> RvFieldAccess baseIdx fieldIdx ty
            _ -> RvRaw t
        _ -> RvRaw t
    _ -> RvRaw t

-- | Strip matching outer parens from a string ending with ")"
stripParens :: Text -> Text
stripParens t
  | ")" `T.isSuffixOf` t = T.dropEnd 1 t
  | otherwise = t

-- | Parse an operand: "move _N", "copy _N", "const V"
parseOperand :: Text -> Maybe MirOperand
parseOperand t
  | "move _" `T.isPrefixOf` stripped =
      let (digits, _) = T.span isDigit (T.drop 6 stripped)
      in case reads (T.unpack digits) of
           [(n, _)] -> Just (OpMove n)
           _        -> Nothing
  | "copy _" `T.isPrefixOf` stripped =
      let (digits, _) = T.span isDigit (T.drop 6 stripped)
      in case reads (T.unpack digits) of
           [(n, _)] -> Just (OpCopy n)
           _        -> Nothing
  | "const " `T.isPrefixOf` stripped = Just (OpConst (T.drop 6 stripped))
  | "copy (" `T.isPrefixOf` stripped =
      -- copy (_4.0: &i64) — field access in operand position
      let inner = T.takeWhile (/= ')') $ T.drop 6 stripped
      in case T.uncons inner of
        Just ('_', rest) ->
          let (baseDigits, afterBase) = T.span isDigit rest
          in case (reads (T.unpack baseDigits), T.uncons afterBase) of
            ([(baseIdx, _)], Just ('.', rest2)) ->
              let (fieldDigits, afterField) = T.span isDigit rest2
                  ty = T.strip $ T.drop 1 $ T.dropWhile (/= ':') afterField
              in case reads (T.unpack fieldDigits) of
                [(fieldIdx, _)] -> Just (OpFieldAccess baseIdx fieldIdx ty)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
  | "move (" `T.isPrefixOf` stripped =
      -- move (_5.0: i64) — field access in operand position
      let inner = T.takeWhile (/= ')') $ T.drop 6 stripped
      in case T.uncons inner of
        Just ('_', rest) ->
          let (baseDigits, afterBase) = T.span isDigit rest
          in case (reads (T.unpack baseDigits), T.uncons afterBase) of
            ([(baseIdx, _)], Just ('.', rest2)) ->
              let (fieldDigits, afterField) = T.span isDigit rest2
                  ty = T.strip $ T.drop 1 $ T.dropWhile (/= ':') afterField
              in case reads (T.unpack fieldDigits) of
                [(fieldIdx, _)] -> Just (OpFieldAccess baseIdx fieldIdx ty)
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
  | "&_" `T.isPrefixOf` stripped =
      let (digits, _) = T.span isDigit (T.drop 2 stripped)
      in case reads (T.unpack digits) of
           [(n, _)] -> Just (OpRef n)
           _        -> Nothing
  | otherwise = Nothing
  where
    stripped = T.strip t

-- | Split on commas, respecting parenthesized groups
splitTopLevel :: Text -> [Text]
splitTopLevel = splitOperands

splitOperands :: Text -> [Text]
splitOperands t = go 0 T.empty (T.unpack t) []
  where
    go :: Int -> Text -> String -> [Text] -> [Text]
    go _ acc [] result = result ++ [acc | not (T.null (T.strip acc))]
    go depth acc ('(':cs) result = go (depth+1) (acc <> "(") cs result
    go depth acc (')':cs) result = go (max 0 (depth-1)) (acc <> ")") cs result
    go depth acc ('[':cs) result = go (depth+1) (acc <> "[") cs result
    go depth acc (']':cs) result = go (max 0 (depth-1)) (acc <> "]") cs result
    go 0 acc (',':cs) result = go 0 T.empty cs (result ++ [acc])
    go depth acc (c:cs) result = go depth (acc <> T.singleton c) cs result

-- | Parse a MIR terminator string into a structured MirTerminator.
--
-- Examples:
--   "return"
--   "goto -> bb6"
--   "switchInt(move _2) -> [0: bb2, otherwise: bb1]"
--   "_3 = factorial(move _4) -> [return: bb4, unwind continue]"
--   "_1 = factorial(const 10_i64) -> [return: bb1, unwind continue]"
--   "assert(!move (_5.1: bool), \"msg\", ...) -> [success: bb3, unwind continue]"
parseTerminator :: Text -> MirTerminator
parseTerminator t
  | t == "return" = TermReturn
  | "return" == T.strip t = TermReturn
  -- goto -> bbN
  | "goto -> bb" `T.isPrefixOf` t =
      let (digits, _) = T.span isDigit (T.drop 10 t)
      in case reads (T.unpack digits) of
           [(n, _)] -> TermGoto n
           _        -> TermRaw t
  -- switchInt(move _N) -> [0: bb2, otherwise: bb1]
  | "switchInt(" `T.isPrefixOf` t =
      let opStr = T.takeWhile (/= ')') (T.drop 10 t)
          op = case parseOperand opStr of
                 Just o  -> o
                 Nothing -> OpConst opStr
          -- Parse the target list after -> [...]
          afterArrow = T.strip $ T.drop 1 $ T.dropWhile (/= '[') t
          targetsStr = T.takeWhile (/= ']') afterArrow
          targets = parseSwitchTargets targetsStr
      in TermSwitchInt op targets
  -- _N = func(args) -> [return: bbN, unwind ...]
  | isCallTerm t =
      parseCallTerm t
  -- assert(...) -> [success: bbN, ...]
  | "assert(" `T.isPrefixOf` t =
      let afterArrow = T.strip $ T.drop 1 $ T.dropWhile (/= '[') t
          targets = T.takeWhile (/= ']') afterArrow
          -- Find "success: bbN"
          succPart = T.strip $ T.takeWhile (/= ',') targets
      in case T.stripPrefix "success: bb" succPart of
           Just digits -> case reads (T.unpack digits) of
             [(n, _)] -> TermAssert t n
             _        -> TermRaw t
           Nothing -> TermRaw t
  | otherwise = TermRaw t

-- | Check if a terminator is a function call: "_N = func(args) -> [...]"
isCallTerm :: Text -> Bool
isCallTerm t =
  let stripped = T.strip t
  in case T.uncons stripped of
    Just ('_', rest) ->
      let (digits, after) = T.span isDigit rest
      in not (T.null digits) && " = " `T.isInfixOf` T.take 5 after
         && " -> [" `T.isInfixOf` stripped
    _ -> False

-- | Parse a call terminator: "_N = func(args) -> [return: bbN, unwind ...]"
parseCallTerm :: Text -> MirTerminator
parseCallTerm t =
  let stripped = T.strip t
  in case T.uncons stripped of
    Just ('_', rest) ->
      let (destDigits, afterDest) = T.span isDigit rest
      in case reads (T.unpack destDigits) :: [(Int, String)] of
        [(_destIdx, _)] ->
          let -- after " = "
              afterEq = T.strip $ T.drop 3 afterDest
              -- Find the function name: everything up to first "("
              funcName = T.takeWhile (/= '(') afterEq
              -- Extract args between first "(" and matching ")"
              argsAndRest = T.drop 1 $ T.dropWhile (/= '(') afterEq
              -- Find the args part before " -> ["
              beforeArrow = T.strip $ fst $ T.breakOn " -> [" argsAndRest
              -- strip trailing ")"
              argsStr = if ")" `T.isSuffixOf` beforeArrow
                        then T.dropEnd 1 beforeArrow
                        else beforeArrow
              argParts = if T.null argsStr then [] else splitOperands argsStr
              -- Parse return target
              afterArrow = T.strip $ T.drop 1 $ T.dropWhile (/= '[') t
              targets = T.takeWhile (/= ']') afterArrow
              retPart = T.strip $ T.takeWhile (/= ',') targets
              retBb = case T.stripPrefix "return: bb" retPart of
                        Just d -> case reads (T.unpack d) of
                                    [(n, _)] -> n
                                    _        -> 0
                        Nothing -> 0
          in TermCallSimple funcName (map T.strip argParts) retBb
        _ -> TermRaw t
    _ -> TermRaw t

-- | Parse switch targets: "0: bb2, otherwise: bb1"
parseSwitchTargets :: Text -> [MirSwitchTarget]
parseSwitchTargets t =
  let parts = map T.strip $ T.splitOn "," t
  in map parseOneTarget parts

parseOneTarget :: Text -> MirSwitchTarget
parseOneTarget t
  | "otherwise: bb" `T.isPrefixOf` stripped =
      let (digits, _) = T.span isDigit (T.drop 13 stripped)
      in case reads (T.unpack digits) of
           [(n, _)] -> MirSwitchTarget Nothing n
           _        -> MirSwitchTarget Nothing 0
  | ": bb" `T.isInfixOf` stripped =
      let valStr = T.strip $ T.takeWhile (/= ':') stripped
          bbStr = T.strip $ T.drop 1 $ T.dropWhile (/= ':') stripped
          bbDigits = T.drop 2 $ T.strip bbStr  -- strip "bb"
      in case (reads (T.unpack valStr), reads (T.unpack bbDigits)) of
           ([(v, _)], [(b, _)]) -> MirSwitchTarget (Just v) b
           _                    -> MirSwitchTarget Nothing 0
  | otherwise = MirSwitchTarget Nothing 0
  where stripped = T.strip t

------------------------------------------------------------------------
-- dumpMir: invoke the rustc shim
------------------------------------------------------------------------

-- | Invoke the rustc shim to get MIR JSON for a Rust source file.
dumpMir :: FilePath -> IO (Either Text Text)
dumpMir inputPath = do
  -- Get nightly sysroot for LD_LIBRARY_PATH
  sysrootResult <- try $ readProcessWithExitCode "rustup"
    ["run", "nightly", "rustc", "--print", "sysroot"] ""
  let sysrootLibDir = case sysrootResult :: Either IOException (ExitCode, String, String) of
        Right (ExitSuccess, out, _) -> Just (trim out ++ "/lib")
        _ -> Nothing

  -- Try the shim binary from rustc-shim/target/ with LD_LIBRARY_PATH
  let shimPaths =
        [ "rustc-mir-dump"  -- on PATH
        , "./rustc-shim/target/rustc-mir-dump"  -- relative to project
        , "/home/nyc/src/frankenstein/rustc-shim/target/rustc-mir-dump"  -- absolute
        ]

  shimResult <- tryShims shimPaths sysrootLibDir inputPath
  case shimResult of
    Right stdout -> pure $ Right $ T.pack stdout
    Left shimErr -> do
      -- Fallback: use rustc's built-in MIR dump (needs nightly)
      -- Try as executable first, then as library crate
      rustcResult <- tryRustcMir inputPath []
      case rustcResult of
        Right out2 -> pure $ Right $ T.pack out2
        Left err1 -> do
          -- If it failed (possibly missing main), retry with --crate-type lib
          rustcLibResult <- tryRustcMir inputPath ["--crate-type", "lib"]
          case rustcLibResult of
            Right out2 -> pure $ Right $ T.pack out2
            Left err2 ->
              pure $ Left $ T.pack $ "Shim error: " ++ shimErr
                ++ "; " ++ err1 ++ "; " ++ err2

-- | Try rustc MIR dump with optional extra flags
tryRustcMir :: FilePath -> [String] -> IO (Either String String)
tryRustcMir inputPath extraFlags = do
  result <- try $ readProcessWithExitCode "rustup"
    ([ "run", "nightly", "rustc"
     , "--edition=2021"
     , "-Z", "unpretty=mir"
     ] ++ extraFlags ++ [inputPath]) ""
  case result :: Either IOException (ExitCode, String, String) of
    Right (ExitSuccess, out, _) -> pure $ Right out
    Right (ExitFailure code, _, err) ->
      pure $ Left $ "rustc fallback failed (exit " ++ show code ++ "): " ++ err
    Left exc ->
      pure $ Left $ "Failed to invoke rustc: " ++ show exc

-- | Try multiple shim paths
tryShims :: [FilePath] -> Maybe String -> FilePath -> IO (Either String String)
tryShims [] _ _ = pure $ Left "no shim binary found"
tryShims (shimPath:rest) sysrootLib inputPath = do
  let env = case sysrootLib of
              Just lib -> Just [("LD_LIBRARY_PATH", lib)]
              Nothing  -> Nothing
  result <- try $ readProcessWithExitCodeEnv shimPath [inputPath] "" env
  case result :: Either IOException (ExitCode, String, String) of
    Right (ExitSuccess, stdout, _) | not (null stdout) -> pure $ Right stdout
    Right (_, _, stderr) -> do
      rest' <- tryShims rest sysrootLib inputPath
      case rest' of
        Right ok -> pure $ Right ok
        Left err -> pure $ Left $ "(" ++ shimPath ++ ": " ++ stderr ++ ") " ++ err
    Left _ -> tryShims rest sysrootLib inputPath

-- | Read process with optional environment variables
readProcessWithExitCodeEnv :: FilePath -> [String] -> String -> Maybe [(String, String)] -> IO (ExitCode, String, String)
readProcessWithExitCodeEnv cmd args input menv = do
  case menv of
    Nothing -> readProcessWithExitCode cmd args input
    Just envVars -> do
      -- Use env command to set the environment
      let envArgs = concatMap (\(k,v) -> [k ++ "=" ++ v]) envVars
      readProcessWithExitCode "env" (envArgs ++ [cmd] ++ args) input

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

------------------------------------------------------------------------
-- Text-based MIR parser (fallback for rustc -Z unpretty=mir output)
------------------------------------------------------------------------

-- | Parse MIR from rustc's textual MIR dump (fallback format).
parseMirText :: Text -> Either Text MirProgram
parseMirText mirText = do
  -- First try to parse as JSON (from the shim)
  case parseMirJson mirText of
    Right prog -> Right prog
    Left _ -> do
      -- Fall back to textual MIR parsing
      let ls = T.lines mirText
          bodies = extractBodies ls
      Right $ MirProgram { mirBodies = bodies }

extractBodies :: [Text] -> [MirBody]
extractBodies [] = []
extractBodies (l:ls)
  | "fn " `T.isPrefixOf` T.stripStart l =
      -- The function header "fn name(...) {" already contains the opening brace,
      -- so we start at depth 1 to match the closing "}" of the function body.
      let (bodyLines, rest') = collectBlock 1 ls
          body = parseBody (T.stripStart l) bodyLines
      in body : extractBodies rest'
  | otherwise = extractBodies ls

-- Collect lines until matching closing brace
collectBlock :: Int -> [Text] -> ([Text], [Text])
collectBlock _ [] = ([], [])
collectBlock depth (l:ls)
  | "{" `T.isSuffixOf` T.stripEnd l = let (rest', remaining) = collectBlock (depth + 1) ls
                                        in (l : rest', remaining)
  | "}" `T.isPrefixOf` T.stripStart l && depth <= 1 = ([l], ls)
  | "}" `T.isPrefixOf` T.stripStart l = let (rest', remaining) = collectBlock (depth - 1) ls
                                          in (l : rest', remaining)
  | otherwise = let (rest', remaining) = collectBlock depth ls
                in (l : rest', remaining)

parseBody :: Text -> [Text] -> MirBody
parseBody headerLine bodyLines =
  let -- Extract function name from "fn name(_1: T1, ...) -> Ret {"
      nameEnd = T.takeWhile (/= '(') (T.drop 3 headerLine)
      name = T.strip nameEnd

      -- Parse arguments from header: "_1: T1, _2: T2, ..."
      argsPart = T.takeWhile (/= ')') $ T.drop 1 $ T.dropWhile (/= '(') headerLine
      argDecls = if T.null (T.strip argsPart) then []
                 else map parseHeaderArg (T.splitOn "," argsPart)
      argCount = length argDecls

      -- Parse local declarations and basic blocks
      (locals, blocks) = parseBodyContents bodyLines

      -- Merge: args (_1.._N) are not in `let` declarations, so synthesize them.
      -- Build a complete locals list sorted by index: _0 (return) from lets,
      -- _1.._N from header, remaining from lets.
      localMap = foldr (\l m -> insertIdx (localIndex l) l m) [] (argDecls ++ locals)

  in MirBody
    { mirName = name
    , mirArgCount = argCount
    , mirLocals = localMap
    , mirBlocks = blocks
    }

-- | Insert a local decl into a sorted-by-index list, skipping duplicates
insertIdx :: Int -> MirLocalDecl -> [MirLocalDecl] -> [MirLocalDecl]
insertIdx _ d [] = [d]
insertIdx _ d (x:xs)
  | localIndex d == localIndex x = x : xs  -- existing takes priority
  | localIndex d < localIndex x  = d : x : xs
  | otherwise                    = x : insertIdx (localIndex d) d xs

-- | Parse a single header argument like "_1: i64" into a MirLocalDecl
parseHeaderArg :: Text -> MirLocalDecl
parseHeaderArg arg =
  let stripped = T.strip arg
      (varPart, rest) = T.breakOn ":" stripped
      ty = T.strip (T.drop 1 rest)
      idx = case T.stripPrefix "_" (T.strip varPart) of
              Just numText -> case reads (T.unpack numText) of
                ((n, ""):_) -> n
                _           -> 0
              Nothing       -> 0
  in MirLocalDecl { localIndex = idx, localType = ty, localMutability = MirNot }

parseBodyContents :: [Text] -> ([MirLocalDecl], [MirBasicBlock])
parseBodyContents ls =
  let -- Local decls start with "let"
      localLines = filter (\l -> "let " `T.isPrefixOf` T.stripStart l) ls
      locals = zipWith parseLocalDecl [0..] localLines
      blocks = extractBlocks ls
  in (locals, blocks)

-- | Extract basic blocks from MIR text format.
-- Scans for "bbN: {" headers, collects lines until "}", builds MirBasicBlock.
extractBlocks :: [Text] -> [MirBasicBlock]
extractBlocks [] = []
extractBlocks (l:rest)
  | Just idx <- parseBbHeader l =
      let (bodyLines, remaining) = collectBlockBody rest
          (stmts, term) = classifyBlockLines bodyLines
      in MirBasicBlock
          { bbIndex = idx
          , bbStatements = stmts
          , bbTerminator = term
          } : extractBlocks remaining
  | otherwise = extractBlocks rest

-- | Parse "bb0: {" or "    bb12: {" → Just 0 or Just 12
parseBbHeader :: Text -> Maybe Int
parseBbHeader line =
  let stripped = T.stripStart line
  in if "bb" `T.isPrefixOf` stripped
     then let rest = T.drop 2 stripped
              numStr = T.takeWhile (\c -> c >= '0' && c <= '9') rest
              after = T.drop (T.length numStr) rest
          in if not (T.null numStr) && ": {" `T.isPrefixOf` T.stripStart after
             then case reads (T.unpack numStr) of
                    [(n, "")] -> Just n
                    _         -> Nothing
             else Nothing
     else Nothing

-- | Collect lines inside a { ... } block until closing }
collectBlockBody :: [Text] -> ([Text], [Text])
collectBlockBody = go []
  where
    go acc [] = (reverse acc, [])
    go acc (l:rest)
      | T.stripStart l == "}" = (reverse acc, rest)
      | otherwise = go (l : acc) rest

-- | Separate block lines into statements and terminator.
-- Skip StorageLive/StorageDead/debug/scope/nop lines.
-- The last non-skip line is the terminator; the rest are statements.
classifyBlockLines :: [Text] -> ([Text], Maybe Text)
classifyBlockLines ls =
  let meaningful = filter (not . isSkipLine) (map T.strip ls)
  in case reverse meaningful of
       [] -> ([], Nothing)
       (lastLine : revRest)
         | isTerminatorLine lastLine -> (map convertTextLine (reverse revRest), Just (convertTextLine lastLine))
         | otherwise -> (map convertTextLine meaningful, Nothing)

-- | Lines to skip (not semantically meaningful for our translation)
isSkipLine :: Text -> Bool
isSkipLine l =
  let s = T.stripStart l
  in T.null s
     || "StorageLive" `T.isPrefixOf` s
     || "StorageDead" `T.isPrefixOf` s
     || "debug " `T.isPrefixOf` s
     || "scope " `T.isPrefixOf` s
     || s == "nop;"

-- | Identify terminator lines
isTerminatorLine :: Text -> Bool
isTerminatorLine l =
  let s = T.stripStart l
  in "return;" `T.isPrefixOf` s
     || "goto ->" `T.isPrefixOf` s
     || "switchInt(" `T.isPrefixOf` s
     || "unreachable;" `T.isPrefixOf` s
     || "resume;" `T.isPrefixOf` s
     || "assert(" `T.isPrefixOf` s
     || ("-> [" `T.isInfixOf` s && not ("= " `T.isPrefixOf` s))

-- | Convert a text-format MIR line to the string format that parseStmt/parseTerminator expect.
-- For assignments like "_2 = Le(copy _1, const 0_i64);" → "Assign((_2, Le(copy _1, const 0_i64)))"
-- For terminators, strip trailing semicolons and convert to expected format.
convertTextLine :: Text -> Text
convertTextLine line =
  let s = T.strip line
      -- Strip trailing semicolons
      noSemi = if ";" `T.isSuffixOf` s then T.dropEnd 1 s else s
  in if " = " `T.isInfixOf` noSemi
     then let (lhs, rhs') = T.breakOn " = " noSemi
              rhs = T.drop 3 rhs'  -- drop " = "
          in "Assign((" <> lhs <> ", " <> rhs <> "))"
     else noSemi

parseLocalDecl :: Int -> Text -> MirLocalDecl
parseLocalDecl fallbackIdx line =
  let stripped = T.stripStart line
      isMut = "let mut" `T.isPrefixOf` stripped
      -- Extract actual index from "_N" in "let [mut] _N: Type;"
      afterLet = if isMut then T.drop 8 stripped   -- "let mut "
                 else T.drop 4 stripped             -- "let "
      varName = T.takeWhile (/= ':') (T.strip afterLet)
      idx = case T.stripPrefix "_" varName of
              Just numText -> case reads (T.unpack numText) of
                ((n, ""):_) -> n
                _           -> fallbackIdx
              Nothing       -> fallbackIdx
      -- Extract type from "let [mut] _N: Type;"
      afterColon = T.strip $ T.drop 1 $ T.dropWhile (/= ':') stripped
      ty = T.takeWhile (/= ';') afterColon
  in MirLocalDecl
    { localIndex = idx
    , localType = T.strip ty
    , localMutability = if isMut then MirMut else MirNot
    }
