{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Core.Interface
-- Description: Separate-compilation .organ-iface ABI, content-hashed tags, and cross-TU validation
--
-- Replaces closed-world whole-program assumptions (global tag numbering, local
-- arity guessing, asymmetric evidence passing, per-TU DCE) with deterministic,
-- separately checkable module interface contracts.
module Frankenstein.Core.Interface
  ( MemProfile(..)
  , ContentTag(..)
  , fnv1a64
  , mkContentTag
  , CtorDescriptor(..)
  , ExportAbi(..)
  , ImportReq(..)
  , OrganInterface(..)
  , InterfaceError(..)
  , checkInterfaceSet
  , checkInterfacePair
  , encodeOrganInterfaceJson
  , decodeOrganInterfaceJson
  ) where

import Data.Bits (xor)
import Data.Char (isDigit, isSpace, ord)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

-- | OrganIR 3-Profile Memory Model exported on every cross-TU ABI boundary.
data MemProfile
  = Profile0FreestandingLinear
    -- ^ Multiplicity = 1; zero RC header, zero malloc/free.
  | Profile1ThreadLocalPerceus
    -- ^ Non-atomic Perceus RC (is_mt branch statically elided).
  | Profile2ConcurrentShared
    -- ^ Atomic Release/Acquire RC matching gpfsl Arc semantics.
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Deterministic 64-bit content-hashed constructor tag.
newtype ContentTag = ContentTag { unContentTag :: Word64 }
  deriving stock (Eq, Ord, Show)

-- | 64-bit FNV-1a hash over UTF-8 code units (deterministic across hosts/TUs).
fnv1a64 :: Text -> Word64
fnv1a64 = T.foldl' step 0xcbf29ce484222325
  where
    step :: Word64 -> Char -> Word64
    step !h c = (h `xor` fromIntegral (ord c)) * 0x100000001b3

-- | Computes a globally unique, TU-independent constructor tag from @(module, type, ctor)@.
mkContentTag :: Text -> Text -> Text -> ContentTag
mkContentTag modName typeName ctorName =
  ContentTag (fnv1a64 (modName <> ":" <> typeName <> ":" <> ctorName))

-- | Constructor ABI metadata exported in @.organ-iface@.
data CtorDescriptor = CtorDescriptor
  { cdQualifiedName :: !Text
  , cdTag           :: !ContentTag
  , cdFieldCount    :: {-# UNPACK #-} !Int
  , cdIsLazy        :: !Bool
  } deriving stock (Eq, Ord, Show)

-- | Canonical calling-convention ABI for an exported OrganIR function.
data ExportAbi = ExportAbi
  { eaQualifiedName :: !Text
  , eaArity         :: {-# UNPACK #-} !Int
  , eaEvParams      :: {-# UNPACK #-} !Int
  , eaProfile       :: !MemProfile
  , eaIsSynthetic   :: !Bool
  } deriving stock (Eq, Ord, Show)

-- | External symbol contract required by a translation unit.
data ImportReq = ImportReq
  { irQualifiedName :: !Text
  , irExpectedArity :: {-# UNPACK #-} !Int
  , irExpectedEv    :: {-# UNPACK #-} !Int
  , irMinProfile    :: !MemProfile
  } deriving stock (Eq, Ord, Show)

-- | Self-contained @.organ-iface@ module header emitted per translation unit.
data OrganInterface = OrganInterface
  { oiSchemaVersion :: !Text
  , oiModuleName    :: !Text
  , oiSourceLang    :: !Text
  , oiCtors         :: ![CtorDescriptor]
  , oiExports       :: ![ExportAbi]
  , oiImports       :: ![ImportReq]
  } deriving stock (Eq, Show)

-- | Link-time interface mismatch surfaced before native codegen/linking.
data InterfaceError
  = DuplicateExport !Text !Text !Text
  | TagCollision !ContentTag !Text !Text
  | UnresolvedImport !Text !Text
  | ArityMismatch !Text !Int !Int
  | EvidenceMismatch !Text !Int !Int
  | ProfileIncompatibility !Text !MemProfile !MemProfile
  deriving stock (Eq, Show)

-- | Convenience validator for two translation units.
checkInterfacePair :: OrganInterface -> OrganInterface -> Either [InterfaceError] (Map Text ExportAbi)
checkInterfacePair i1 i2 = checkInterfaceSet [i1, i2]

-- | Validates a collection of separately compiled 'OrganInterface's for tag
-- disjointness, unique exports, and exact cross-TU ABI/profile compatibility.
checkInterfaceSet :: [OrganInterface] -> Either [InterfaceError] (Map Text ExportAbi)
checkInterfaceSet ifaces =
  let (expMap, dupErrs) = foldl' addExports (Map.empty, []) ifaces
      (_, tagErrs)      = foldl' addCtors (Map.empty, []) ifaces
      impErrs           = concatMap (checkImports expMap) ifaces
      allErrs           = reverse dupErrs ++ reverse tagErrs ++ impErrs
  in if null allErrs then Right expMap else Left allErrs
  where
    addExports acc iface =
      foldl'
        (\(m, errs) ex ->
           let sym = eaQualifiedName ex
           in case Map.lookup sym m of
                Just _  -> (m, DuplicateExport sym (oiModuleName iface) sym : errs)
                Nothing -> (Map.insert sym ex m, errs))
        acc
        (oiExports iface)

    addCtors acc iface =
      foldl'
        (\(m, errs) cd ->
           let t = cdTag cd
               q = cdQualifiedName cd
           in case Map.lookup t m of
                Just prev | prev /= q -> (m, TagCollision t prev q : errs)
                _                     -> (Map.insert t q m, errs))
        acc
        (oiCtors iface)

    checkImports expMap iface =
      concatMap (verifyOne (oiModuleName iface) expMap) (oiImports iface)

    verifyOne modName expMap req =
      let sym = irQualifiedName req
      in case Map.lookup sym expMap of
           Nothing -> [UnresolvedImport modName sym]
           Just ex ->
             [ArityMismatch sym (irExpectedArity req) (eaArity ex) | irExpectedArity req /= eaArity ex]
             ++ [EvidenceMismatch sym (irExpectedEv req) (eaEvParams ex) | irExpectedEv req /= eaEvParams ex]
             ++ [ProfileIncompatibility sym (irMinProfile req) (eaProfile ex) | eaProfile ex < irMinProfile req]

--------------------------------------------------------------------------------
-- Lightweight JSON Serialization & Deserialization
--------------------------------------------------------------------------------

escapeJson :: Text -> Text
escapeJson = T.concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = T.singleton c

-- | Serializes an 'OrganInterface' to pretty JSON text.
encodeOrganInterfaceJson :: OrganInterface -> Text
encodeOrganInterfaceJson oi =
  "{\n"
  <> "  \"schema_version\": \"" <> escapeJson (oiSchemaVersion oi) <> "\",\n"
  <> "  \"module_name\": \"" <> escapeJson (oiModuleName oi) <> "\",\n"
  <> "  \"source_lang\": \"" <> escapeJson (oiSourceLang oi) <> "\",\n"
  <> "  \"ctors\": [\n"
  <> T.intercalate ",\n" (map encodeCtor (oiCtors oi))
  <> (if null (oiCtors oi) then "" else "\n")
  <> "  ],\n"
  <> "  \"exports\": [\n"
  <> T.intercalate ",\n" (map encodeExport (oiExports oi))
  <> (if null (oiExports oi) then "" else "\n")
  <> "  ],\n"
  <> "  \"imports\": [\n"
  <> T.intercalate ",\n" (map encodeImport (oiImports oi))
  <> (if null (oiImports oi) then "" else "\n")
  <> "  ]\n"
  <> "}\n"
  where
    encodeCtor (CtorDescriptor q (ContentTag t) fc l) =
      "    {\"qualified_name\": \"" <> escapeJson q
      <> "\", \"tag\": \"" <> T.pack (show t)
      <> "\", \"field_count\": " <> T.pack (show fc)
      <> ", \"is_lazy\": " <> (if l then "true" else "false")
      <> "}"

    encodeExport (ExportAbi q a ev p s) =
      "    {\"qualified_name\": \"" <> escapeJson q
      <> "\", \"arity\": " <> T.pack (show a)
      <> ", \"ev_params\": " <> T.pack (show ev)
      <> ", \"profile\": \"" <> T.pack (show p)
      <> "\", \"is_synthetic\": " <> (if s then "true" else "false")
      <> "}"

    encodeImport (ImportReq q a ev p) =
      "    {\"qualified_name\": \"" <> escapeJson q
      <> "\", \"expected_arity\": " <> T.pack (show a)
      <> ", \"expected_ev\": " <> T.pack (show ev)
      <> ", \"min_profile\": \"" <> T.pack (show p)
      <> "\"}"

-- Minimal JSON AST
data JVal
  = JStr !Text
  | JNum !Integer
  | JBool !Bool
  | JNull
  | JArr ![JVal]
  | JObj ![(Text, JVal)]
  deriving stock (Eq, Show)

-- Minimal JSON Tokenizer & Parser
data JTok
  = TBraceOpen
  | TBraceClose
  | TBracketOpen
  | TBracketClose
  | TColon
  | TComma
  | TString !Text
  | TNumber !Integer
  | TTrue
  | TFalse
  | TNullTok
  deriving stock (Eq, Show)

tokenizeJson :: Text -> Either Text [JTok]
tokenizeJson txt = go (T.unpack txt)
  where
    go [] = Right []
    go (c : cs)
      | isSpace c = go cs
      | c == '{'  = (TBraceOpen :) <$> go cs
      | c == '}'  = (TBraceClose :) <$> go cs
      | c == '['  = (TBracketOpen :) <$> go cs
      | c == ']'  = (TBracketClose :) <$> go cs
      | c == ':'  = (TColon :) <$> go cs
      | c == ','  = (TComma :) <$> go cs
      | c == '"'  = parseString "" cs
      | isDigit c || c == '-' = parseNum [c] cs
      | "true" `isPrefixOf` (c : cs) = (TTrue :) <$> go (drop 4 (c : cs))
      | "false" `isPrefixOf` (c : cs) = (TFalse :) <$> go (drop 5 (c : cs))
      | "null" `isPrefixOf` (c : cs) = (TNullTok :) <$> go (drop 4 (c : cs))
      | otherwise = Left ("Unexpected JSON character: " <> T.singleton c)

    isPrefixOf p str = take (length p) str == p

    parseString acc [] = Left "Unterminated string literal in JSON"
    parseString acc ('"' : cs) = (TString (T.pack (reverse acc)) :) <$> go cs
    parseString acc ('\\' : '"' : cs) = parseString ('"' : acc) cs
    parseString acc ('\\' : '\\' : cs) = parseString ('\\' : acc) cs
    parseString acc ('\\' : 'n' : cs) = parseString ('\n' : acc) cs
    parseString acc ('\\' : 'r' : cs) = parseString ('\r' : acc) cs
    parseString acc ('\\' : 't' : cs) = parseString ('\t' : acc) cs
    parseString acc (c : cs) = parseString (c : acc) cs

    parseNum acc [] = Right [TNumber (read (reverse acc))]
    parseNum acc (c : cs)
      | isDigit c = parseNum (c : acc) cs
      | otherwise = (TNumber (read (reverse acc)) :) <$> go (c : cs)

parseJVal :: [JTok] -> Either Text (JVal, [JTok])
parseJVal [] = Left "Unexpected EOF parsing JSON"
parseJVal (TString s : rest) = Right (JStr s, rest)
parseJVal (TNumber n : rest) = Right (JNum n, rest)
parseJVal (TTrue : rest)     = Right (JBool True, rest)
parseJVal (TFalse : rest)    = Right (JBool False, rest)
parseJVal (TNullTok : rest)  = Right (JNull, rest)
parseJVal (TBracketOpen : rest) = parseArr [] rest
  where
    parseArr acc (TBracketClose : xs) = Right (JArr (reverse acc), xs)
    parseArr acc xs = do
      (v, xs1) <- parseJVal xs
      case xs1 of
        (TComma : xs2)        -> parseArr (v : acc) xs2
        (TBracketClose : xs2) -> Right (JArr (reverse (v : acc)), xs2)
        other                 -> Left ("Expected ',' or ']' in array, got: " <> T.pack (show other))
parseJVal (TBraceOpen : rest) = parseObj [] rest
  where
    parseObj acc (TBraceClose : xs) = Right (JObj (reverse acc), xs)
    parseObj acc (TString k : TColon : xs) = do
      (v, xs1) <- parseJVal xs
      case xs1 of
        (TComma : xs2)      -> parseObj ((k, v) : acc) xs2
        (TBraceClose : xs2) -> Right (JObj (reverse ((k, v) : acc)), xs2)
        other               -> Left ("Expected ',' or '}' in object, got: " <> T.pack (show other))
    parseObj _ other = Left ("Expected string key in object, got: " <> T.pack (show other))

-- | Parses an 'OrganInterface' from JSON text.
decodeOrganInterfaceJson :: Text -> Either Text OrganInterface
decodeOrganInterfaceJson txt = do
  toks <- tokenizeJson txt
  (jval, remaining) <- parseJVal toks
  if null remaining
    then parseInterface jval
    else Left "Trailing tokens after JSON root object"
  where
    parseInterface (JObj kvs) = do
      let m = Map.fromList kvs
      ver <- lookupStr "schema_version" m
      modN <- lookupStr "module_name" m
      srcL <- lookupStr "source_lang" m
      rawCtors <- lookupArr "ctors" m
      rawExps  <- lookupArr "exports" m
      rawImps  <- lookupArr "imports" m
      ctors <- mapM parseCtor rawCtors
      exps  <- mapM parseExport rawExps
      imps  <- mapM parseImport rawImps
      pure $ OrganInterface
        { oiSchemaVersion = ver
        , oiModuleName    = modN
        , oiSourceLang    = srcL
        , oiCtors         = ctors
        , oiExports       = exps
        , oiImports       = imps
        }
    parseInterface other = Left ("Root must be JSON object, got: " <> T.pack (show other))

    lookupStr k m = case Map.lookup k m of
      Just (JStr s) -> Right s
      _             -> Left ("Missing or non-string key: " <> k)

    lookupNum k m = case Map.lookup k m of
      Just (JNum n) -> Right (fromIntegral n)
      Just (JStr s) -> case reads (T.unpack s) of
        [(n, "")] -> Right n
        _         -> Left ("Cannot parse number from string: " <> s)
      _             -> Left ("Missing or non-numeric key: " <> k)

    lookupBool k m = case Map.lookup k m of
      Just (JBool b) -> Right b
      _              -> Left ("Missing or non-boolean key: " <> k)

    lookupArr k m = case Map.lookup k m of
      Just (JArr xs) -> Right xs
      _              -> Left ("Missing or non-array key: " <> k)

    parseProfile "Profile0FreestandingLinear" = Right Profile0FreestandingLinear
    parseProfile "Profile1ThreadLocalPerceus"  = Right Profile1ThreadLocalPerceus
    parseProfile "Profile2ConcurrentShared"    = Right Profile2ConcurrentShared
    parseProfile other                        = Left ("Unknown MemProfile: " <> other)

    parseCtor (JObj kvs) = do
      let m = Map.fromList kvs
      q <- lookupStr "qualified_name" m
      tagVal <- lookupNum "tag" m
      fc <- lookupNum "field_count" m
      l  <- lookupBool "is_lazy" m
      pure (CtorDescriptor q (ContentTag (fromIntegral tagVal)) fc l)
    parseCtor other = Left ("Expected ctor object, got: " <> T.pack (show other))

    parseExport (JObj kvs) = do
      let m = Map.fromList kvs
      q <- lookupStr "qualified_name" m
      a <- lookupNum "arity" m
      ev <- lookupNum "ev_params" m
      pStr <- lookupStr "profile" m
      p <- parseProfile pStr
      s <- lookupBool "is_synthetic" m
      pure (ExportAbi q a ev p s)
    parseExport other = Left ("Expected export object, got: " <> T.pack (show other))

    parseImport (JObj kvs) = do
      let m = Map.fromList kvs
      q <- lookupStr "qualified_name" m
      a <- lookupNum "expected_arity" m
      ev <- lookupNum "expected_ev" m
      pStr <- lookupStr "min_profile" m
      p <- parseProfile pStr
      pure (ImportReq q a ev p)
    parseImport other = Left ("Expected import object, got: " <> T.pack (show other))
