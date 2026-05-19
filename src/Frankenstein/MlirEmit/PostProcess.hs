{-# LANGUAGE OverloadedStrings #-}
-- | MLIR post-processing transformations, absorbed from
-- @self-host/fix-intra-module-calls.py@, @self-host/fix-dollar0-refs.py@,
-- and @self-host/fix-mlir-arity.py@.
--
-- = Background
--
-- The self-hosted compiler binary diverges from the GHC-compiled host on
-- specific patterns in @Emitter.hs@:
--
--   * Recursive @emitConChain@'s @mDefaultExpr@ argument is corrupted when
--     the self-host compiler emits the call — @Nothing@ becomes
--     @Just <previous-branch-body>@, producing dead-code emissions that
--     reference out-of-scope pattern binders ('$0()' externals).
--   * Split compilation produces cross-part references with arity-mangled
--     names ('Name$N') that don't resolve at link time.
--   * Some 'func.call' sites omit captures that are part of the declared
--     function arity (under-arity calls).
--
-- These transformations rewrite the emitted MLIR to repair the divergence.
--
-- = Immunity claim
--
-- This module is invoked from the host-compiled @frankenstein@ binary via
-- @--postprocess-mlir@. The self-hosted compiler binary never executes this
-- code path. Therefore the self-host runtime bugs that motivated these
-- transformations cannot affect them: this code runs in GHC-compiled
-- artifacts only.
module Frankenstein.MlirEmit.PostProcess
  ( postProcessFile
  , postProcessText
    -- Per-pass functions (exported for testing)
  , fixIntraModuleCalls
  , fixDollar0Refs
  , fixMlirArity
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Maybe (mapMaybe, fromMaybe, isJust)
import Data.List (foldl')
import Data.Char (isDigit)
import System.FilePath (takeDirectory)
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import qualified Data.Text.IO as T (readFile, writeFile)
import Control.Monad (forM)

-- | Apply the three transformations to a file in place.
postProcessFile :: FilePath -> IO ()
postProcessFile path = do
  contents <- T.readFile path
  stage1 <- loadStage1Cache (takeDirectory (takeDirectory path))
  let !out = postProcessText stage1 contents
  T.writeFile path out

-- | Pure-text driver: chain the three transformations.
postProcessText :: Map FilePath Text -> Text -> Text
postProcessText stage1 =
  fixDollar0Refs stage1
  . fixMlirArity
  . fixIntraModuleCalls

-------------------------------------------------------------------------------
-- fix-intra-module-calls.py port
-------------------------------------------------------------------------------

-- | Generate wrappers for unresolved @\@Name$N(...)@ private declarations
-- whose underlying @\@Name@ is defined in the same file.
fixIntraModuleCalls :: Text -> Text
fixIntraModuleCalls src =
  let lns      = T.lines src
      privates = collectPrivateDollarN lns
      defs     = collectFuncDefs lns
      (wrappers, removed) = generateWrappers privates defs
      lns' = filter (\l -> not (isPrivateInSet removed l)) lns
  in if null wrappers
       then src
       else insertBeforeFinalBrace wrappers (T.unlines lns')

-- | Parse @  func.func private \@Name$N(args) -> i64@ — return key
-- (e.g. "foo$3"), the underlying name, the suffix, and the parameter count.
collectPrivateDollarN :: [Text] -> Map Text (Text, Int, Int)
collectPrivateDollarN lns = Map.fromList $ mapMaybe go lns
  where
    go l = do
      rest1 <- T.stripPrefix "  func.func private @" l
      let (name, rest2) = T.break (== '$') rest1
      rest3 <- T.stripPrefix "$" rest2
      let (suffixT, rest4) = T.span isDigit rest3
      suf <- readInt suffixT
      rest5 <- T.stripPrefix "(" rest4
      let (paramsT, rest6) = T.break (== ')') rest5
      _ <- T.stripPrefix ") -> i64" rest6
      let nargs = countCommaSep paramsT
          key   = name <> "$" <> suffixT
      Just (key, (name, suf, nargs))

-- | Parse @  func.func \@Name(args) -> i64 {@ — accepts names with @$@.
collectFuncDefs :: [Text] -> Map Text Int
collectFuncDefs lns = Map.fromList $ mapMaybe go lns
  where
    go l = do
      rest1 <- T.stripPrefix "  func.func @" l
      -- Stop at '(' for the name — names may contain '$'.
      let (name, rest2) = T.break (== '(') rest1
      rest3 <- T.stripPrefix "(" rest2
      let (paramsT, rest4) = T.break (== ')') rest3
      _ <- T.stripPrefix ") -> i64 {" rest4
      Just (name, countCommaSep paramsT)

generateWrappers
  :: Map Text (Text, Int, Int)  -- privates: key -> (name, suffix, nargs)
  -> Map Text Int               -- defs:     name -> def_nargs
  -> ([Text], Set Text)
generateWrappers privates defs = Map.foldrWithKey go ([], Set.empty) privates
  where
    go key (name, _suffix, callArgs) (ws, rem_)
      | Just defNargs <- Map.lookup name defs =
          if Map.member key defs
            then -- $N variant already defined — just remove the private decl
                 (ws, Set.insert key rem_)
          else case compare callArgs defNargs of
            EQ -> (mkSimpleAlias key name callArgs : ws, Set.insert key rem_)
            GT -> (mkOversaturated key name defNargs callArgs : ws,
                   Set.insert key rem_)
            LT -> (mkUndersaturated key name defNargs callArgs ws,
                   Set.insert key rem_)
      | otherwise = (ws, rem_)  -- truly external, leave alone

-- | Case 1: same arity. Trivial forwarding wrapper.
mkSimpleAlias :: Text -> Text -> Int -> Text
mkSimpleAlias key name n =
  let params = T.intercalate ", " ["%a" <> tshow i <> ": i64" | i <- [0 .. n-1]]
      args   = T.intercalate ", " ["%a" <> tshow i | i <- [0 .. n-1]]
      tyList = T.intercalate ", " (replicate n "i64")
  in if n == 0
       then T.unlines
              [ "  func.func @" <> key <> "() -> i64 {"
              , "    %r = func.call @" <> name <> "() : () -> i64"
              , "    func.return %r : i64"
              , "  }"
              ]
       else T.unlines
              [ "  func.func @" <> key <> "(" <> params <> ") -> i64 {"
              , "    %r = func.call @" <> name <> "(" <> args
                  <> ") : (" <> tyList <> ") -> i64"
              , "    func.return %r : i64"
              , "  }"
              ]

-- | Case 2: oversaturated — call def with its args, get closure, indirect-call
-- with the extras.
mkOversaturated :: Text -> Text -> Int -> Int -> Text
mkOversaturated key name defN callN =
  let params    = T.intercalate ", " ["%a" <> tshow i <> ": i64" | i <- [0 .. callN-1]]
      defArgs   = T.intercalate ", " ["%a" <> tshow i | i <- [0 .. defN-1]]
      defTy     = T.intercalate ", " (replicate defN "i64")
      extraArgs = ["%a" <> tshow i | i <- [defN .. callN-1]]
      closArgs  = T.intercalate ", " ("%clos" : extraArgs)
      closTy    = T.intercalate ", " (replicate (1 + length extraArgs) "i64")
      defCall = if defN == 0
                  then "    %raw = func.call @" <> name <> "() : () -> i64"
                  else "    %raw = func.call @" <> name <> "("
                       <> defArgs <> ") : (" <> defTy <> ") -> i64"
  in T.unlines
       [ "  func.func @" <> key <> "(" <> params <> ") -> i64 {"
       , defCall
       , "    %clos = func.call @kk_thunk_force(%raw) : (i64) -> i64"
       , "    %idx0 = arith.constant 0 : i64"
       , "    %fptr_int = func.call @kk_field(%clos, %idx0) : (i64, i64) -> i64"
       , "    %fptr_ptr = llvm.inttoptr %fptr_int : i64 to !llvm.ptr"
       , "    %result = llvm.call %fptr_ptr(" <> closArgs
           <> ") : !llvm.ptr, (" <> closTy <> ") -> i64"
       , "    func.return %result : i64"
       , "  }"
       ]

-- | Case 3: undersaturated — build a CLOS that captures the supplied args and,
-- when fully applied, calls the original. Emits BOTH the @\$N@ wrapper and
-- its companion @\__pap_<name>_d<suffix>@ continuation function.
mkUndersaturated :: Text -> Text -> Int -> Int -> [Text] -> [Text]
mkUndersaturated key name defN callN existing =
  let remaining = defN - callN
      papName   = "__pap_" <> T.replace "$" "_d" key
      papParams = T.intercalate ", " (
                    "%clos_arg: i64"
                    : ["%r" <> tshow i <> ": i64" | i <- [0 .. remaining-1]])
      capExtractLines = concat
        [ [ "    %cap_idx" <> tshow i <> " = arith.constant " <> tshow (i+1) <> " : i64"
          , "    %cap" <> tshow i <> " = func.call @kk_field(%clos_arg, %cap_idx"
            <> tshow i <> ") : (i64, i64) -> i64"
          ]
        | i <- [0 .. callN-1]
        ]
      callArgs = ["%cap" <> tshow i | i <- [0 .. callN-1]]
              ++ ["%r"   <> tshow i | i <- [0 .. remaining-1]]
      callArgsStr = T.intercalate ", " callArgs
      callTy      = T.intercalate ", " (replicate defN "i64")
      papWrapper = T.unlines $
        [ "  func.func @" <> papName <> "(" <> papParams <> ") -> i64 {" ]
        ++ capExtractLines ++
        [ "    %pap_result = func.call @" <> name <> "(" <> callArgsStr
            <> ") : (" <> callTy <> ") -> i64"
        , "    func.return %pap_result : i64"
        , "  }"
        ]
      nFields = 1 + callN
      papFnTotal = 1 + remaining
      papFnTy = T.intercalate ", " (replicate papFnTotal "i64")
      mainParams = T.intercalate ", " ["%a" <> tshow i <> ": i64" | i <- [0 .. callN-1]]
      storeCaptures = concat
        [ [ "    %idx" <> tshow (i+1) <> " = arith.constant " <> tshow (i+1) <> " : i64"
          , "    func.call @kk_set_field(%clos, %idx" <> tshow (i+1)
            <> ", %a" <> tshow i <> ") : (i64, i64, i64) -> ()"
          ]
        | i <- [0 .. callN-1]
        ]
      mainWrapper = T.unlines $
        [ "  func.func @" <> key <> "(" <> mainParams <> ") -> i64 {"
        , "    %clos_tag = arith.constant 1129074515 : i64"
        , "    %n_fields = arith.constant " <> tshow nFields <> " : i64"
        , "    %clos = func.call @kk_alloc_con(%clos_tag, %n_fields) : (i64, i64) -> i64"
        , "    %fptr = func.constant @" <> papName <> " : (" <> papFnTy <> ") -> i64"
        , "    %fptr_llvm = builtin.unrealized_conversion_cast %fptr : ("
            <> papFnTy <> ") -> i64 to !llvm.ptr"
        , "    %fptr_int = llvm.ptrtoint %fptr_llvm : !llvm.ptr to i64"
        , "    %idx0 = arith.constant 0 : i64"
        , "    func.call @kk_set_field(%clos, %idx0, %fptr_int) : (i64, i64, i64) -> ()"
        ]
        ++ storeCaptures ++
        [ "    func.return %clos : i64"
        , "  }"
        ]
  in papWrapper : mainWrapper : existing

isPrivateInSet :: Set Text -> Text -> Bool
isPrivateInSet removed l =
  case T.stripPrefix "  func.func private @" l of
    Nothing -> False
    Just rest ->
      let (key, after) = T.break (== '(') rest
      in not (T.null after) && Set.member key removed

insertBeforeFinalBrace :: [Text] -> Text -> Text
insertBeforeFinalBrace wrappers result =
  let body  = "\n  // Intra-module $N wrappers (generated by PostProcess)\n"
              <> T.intercalate "\n" wrappers <> "\n"
      idx   = lastIndexOf "\n}" result
  in case idx of
       Just i -> T.take i result <> body <> T.drop i result
       Nothing -> result <> body

-------------------------------------------------------------------------------
-- fix-mlir-arity.py port
-------------------------------------------------------------------------------

-- | Pad under-arity calls with zero constants, trim over-arity calls of
-- phantom args that match the target's parameter names.
fixMlirArity :: Text -> Text
fixMlirArity src =
  let lns = T.lines src
      decls = collectFuncDeclsWithParams lns
      (lns', _) = foldl' (stepArity decls) ([], 0 :: Int) lns
  in T.unlines (reverse lns')

-- | Collect @func.func \@Name(%p1: i64, ...) -> i64@ — first occurrence wins.
-- Returns parameter names (without %).
collectFuncDeclsWithParams :: [Text] -> Map Text (Int, [Text])
collectFuncDeclsWithParams lns =
  Map.fromListWith (\_new old -> old) $ mapMaybe go lns
  where
    go l = do
      let l1 = T.stripStart l
      rest1 <- T.stripPrefix "func.func " l1
      let rest2 = fromMaybe rest1 (T.stripPrefix "private " rest1)
      rest3 <- T.stripPrefix "@" rest2
      let (name, rest4) = T.break (== '(') rest3
      rest5 <- T.stripPrefix "(" rest4
      let (paramsT, _rest6) = T.break (== ')') rest5
      let pnames = extractParamNames paramsT
      Just (name, (length pnames `max` countCommaSep paramsT, pnames))

extractParamNames :: Text -> [Text]
extractParamNames params
  | T.null (T.strip params) = []
  | otherwise =
      [ T.drop 1 (T.takeWhile (/= ':') (T.strip p))
      | p <- T.splitOn "," params
      , let s = T.strip p
      , not (T.null s)
      , "%" `T.isPrefixOf` s
      ]

-- | Process one line: detect a func.call, check arity, rewrite.
stepArity
  :: Map Text (Int, [Text])
  -> ([Text], Int)
  -> Text
  -> ([Text], Int)
stepArity decls (acc, counter) line =
  case parseFuncCall line of
    Just call
      | Just (need, pnames) <- Map.lookup (fcName call) decls
      , fcActual call > need ->
          let (kept, _removed) = trimPhantoms (fcArgs call) pnames need
          in if length kept == need
               then (rebuildCall call kept (fcPost call) : acc, counter)
               else (line : acc, counter)
      | Just (need, _pnames) <- Map.lookup (fcName call) decls
      , fcActual call < need ->
          let deficit  = need - fcActual call
              padNames = ["_arity_pad_" <> tshow (counter + i + 1) | i <- [0 .. deficit-1]]
              indent   = T.takeWhile (\c -> c == ' ' || c == '\t') line
              padLines = [ indent <> "%" <> p <> " = arith.constant 0 : i64"
                         | p <- padNames ]
              extra    = ["%" <> p | p <- padNames]
              kept'    = fcArgs call ++ extra
              newLine  = rebuildCall call kept' (fcPost call)
          in (newLine : reverse padLines ++ acc, counter + deficit)
    _ -> (line : acc, counter)

-- | Parsed slice of a @func.call \@name(args) : (types) -> i64@ line.
data FuncCall = FuncCall
  { fcPre    :: !Text   -- everything up to and including "@name("
  , fcName   :: !Text
  , fcArgs   :: ![Text] -- argument SSAs like "%v3"
  , fcPost   :: !Text   -- "-> i64" tail (after the close-paren of types)
  , fcActual :: !Int    -- length fcArgs
  }

parseFuncCall :: Text -> Maybe FuncCall
parseFuncCall line = do
  let (pre, rest1) = T.breakOn "func.call @" line
  rest2 <- T.stripPrefix "func.call @" rest1
  let (name, rest3) = T.break (== '(') rest2
  rest4 <- T.stripPrefix "(" rest3
  let (argsT, rest5) = T.break (== ')') rest4
  rest6 <- T.stripPrefix ") :" rest5
  let -- skip past the type list of args
      (_types, rest7) = T.breakOn ") -> " rest6
  post <- T.stripPrefix ")" rest7  -- consume the ")" of the type list
  let args = [ T.strip a | a <- T.splitOn "," argsT, not (T.null (T.strip a)) ]
  Just FuncCall
    { fcPre    = pre <> "func.call @" <> name <> "("
    , fcName   = name
    , fcArgs   = args
    , fcPost   = post
    , fcActual = length args
    }

-- | Strip args whose SSA name (minus %) matches one of the target function's
-- parameter names. Stop once we've trimmed enough.
trimPhantoms :: [Text] -> [Text] -> Int -> ([Text], [Text])
trimPhantoms args pnames target = go args (Set.fromList pnames) [] []
  where
    go [] _ kept removed = (reverse kept, reverse removed)
    go (a:rest) targets kept removed
      | length kept + length rest >= target
      , Set.member (T.drop 1 a) targets =
          go rest (Set.delete (T.drop 1 a) targets) kept (a:removed)
      | otherwise = go rest targets (a:kept) removed

rebuildCall :: FuncCall -> [Text] -> Text -> Text
rebuildCall call newArgs post =
  let argsStr  = T.intercalate ", " newArgs
      tyStr    = T.intercalate ", " (replicate (length newArgs) "i64")
      -- fcPost starts with " -> ret"; strip the leading " -> " we re-emit.
      retPart  = fromMaybe post (T.stripPrefix " -> " post)
  in fcPre call <> argsStr <> ") : (" <> tyStr <> ") -> " <> retPart

-------------------------------------------------------------------------------
-- fix-dollar0-refs.py port
-------------------------------------------------------------------------------

-- | Hard-coded set of known function names whose @\$0()@ references are
-- resolved by PAP-closure construction.
knownFunctions :: Set Text
knownFunctions = Set.fromList
  [ "OrganIR_Parse_asArr"
  , "OrganIR_Parse_asStr"
  , "OrganIR_Parse_asInt"
  , "OrganIR_Parse_decodeName"
  , "OrganIR_Parse_decodeQName"
  , "OrganIR_Parse_decodeTy"
  , "OrganIR_Parse_decodeTyVar"
  , "OrganIR_Parse_decodePatBinder"
  , "OrganIR_Parse_decodeLamParam"
  , "OrganIR_Parse_decodeVisibility"
  , "OrganIR_Parse_decodeSort"
  , "OrganIR_Parse_decodeMetadata"
  , "OrganIR_Parse_decodeConstructor"
  , "Frankenstein_RustBridge_MirParse_jStr"
  , "Frankenstein_RustBridge_MirParse_jInt"
  , "Frankenstein_RustBridge_MirParse_jArr"
  , "Frankenstein_RustBridge_MirParse_jBool"
  , "Frankenstein_GhcBridge_CoreTranslate_isStateVar"
  , "Frankenstein_MercuryBridge_CoreTranslate_extendBindingsFor"
  , "Frankenstein_MercuryBridge_HldsParse_isComment"
  , "Frankenstein_OrganIR_Consumer_consumeQName"
  , "Frankenstein_MlirEmit_Dialects_valName"
  , "Frankenstein_MlirEmit_Dialects_valType"
  , "Frankenstein_Core_Types_bindExpr"
  , "Frankenstein_Core_Types_defExpr"
  ]

loadStage1Cache :: FilePath -> IO (Map FilePath Text)
loadStage1Cache dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure Map.empty
    else do
      entries <- listDirectory dir
      let mlirs = filter (\f -> ".mlir" `T.isSuffixOf` T.pack f) entries
      pairs <- forM mlirs $ \fn -> do
        let p = dir <> "/" <> fn
        ok <- doesFileExist p
        if ok then do c <- T.readFile p; pure [(fn, c)] else pure []
      pure (Map.fromList (concat pairs))

-- | Replace bogus @\@frankenstein_<x>$0()@ calls. Three strategies:
--   * Trailing-digits match: find a @// let <name> = %v@ alias whose name
--     ends with the same unique number as the variable reference.
--   * PAP closure: known function names get a closure-construction sequence.
-- The third strategy (stage1 cross-reference) is omitted in this port — the
-- known-function set covers the OrganIR/Consumer.mlir cases we actually need
-- to rewrite (the other err output shows '13 by PAP closure, 55 unfixed' —
-- the unfixed ones produce private decls but no link errors in practice).
fixDollar0Refs :: Map FilePath Text -> Text -> Text
fixDollar0Refs stage1 src
  | not ("$0()" `T.isInfixOf` src) = src
  | otherwise =
      let lns      = T.lines src
          fixes    = collectDollar0Fixes stage1 lns
      in if null fixes
           then src
           else applyDollar0Fixes fixes lns

data Dollar0Fix
  = RenameFix !Int !Text !Text !Text  -- lineIdx, resultVar, correctVar, varName
  | PapFix    !Int !Text !Text !Text !Int
       -- lineIdx, resultVar, varName, papName, papNparams
  | ExternPapFix !Int !Text !Text !Int
       -- lineIdx, resultVar, varName, arity (discovered via stage1 cache).
       -- Generates a synthesized 'pap_extern_frankenstein_<varName>_0' wrapper
       -- and matching extern decl injected at module scope.
  | ReExtractFix !Int !Text !Text !Text !Int
       -- lineIdx, resultVar, varName, scrutinee SSA, field index.
       -- For pattern binders whose alias exists out-of-scope (in a sibling
       -- scf.if branch): replicate the kk_field(scrutinee, idx) extraction
       -- at the call site so the same Name value is recovered.

collectDollar0Fixes :: Map FilePath Text -> [Text] -> [Dollar0Fix]
collectDollar0Fixes stage1 lns =
  let indexedLns = zip [0..] lns
      mkFix (i, line) = do
        (_indent, resultVar, varName) <- parseDollar0 line
        let (_funcName, funcStart) = findEnclosingFunc lns i
            byUnique = fixByTrailingUnique varName lns i funcStart
        case byUnique of
          Just correctVar -> Just (RenameFix i resultVar correctVar varName)
          Nothing
            | Set.member varName knownFunctions ->
                case findPapWrapper varName lns of
                  Just (papName, nparams) ->
                    Just (PapFix i resultVar varName papName nparams)
                  Nothing -> tryReExtractThenExtern resultVar varName i funcStart
            | otherwise -> tryReExtractThenExtern resultVar varName i funcStart
      tryReExtractThenExtern resultVar varName i funcStart =
        case findReExtractTarget varName lns i funcStart of
          Just (scrut, idx) ->
            Just (ReExtractFix i resultVar varName scrut idx)
          Nothing -> tryExtern resultVar varName i
      tryExtern resultVar varName i =
        case findExternalArity varName stage1 of
          Just arity | arity > 0 ->
            Just (ExternPapFix i resultVar varName arity)
          _ -> Nothing
  in mapMaybe mkFix indexedLns

-- | For an unresolved @\@frankenstein_<varName>$0()@ pattern binder that has
-- a matching @// let <varName-suffix> = %ssa@ alias somewhere in the function
-- (potentially in a sibling scf.if branch), recover (scrutinee, field_idx)
-- from the kk_field call that produced %ssa. The fix then re-emits the same
-- kk_field at the call site so the binder's value is reconstructed in the
-- current branch's scope.
findReExtractTarget :: Text -> [Text] -> Int -> Maybe Int -> Maybe (Text, Int)
findReExtractTarget varName lns callLine funcStart = do
  uniq <- trailingDigits varName
  let searchStart = fromMaybe 0 funcStart
      funcEnd = findFuncEnd lns searchStart
      windowLns = take (funcEnd - searchStart) (drop searchStart lns)
      indexed = zip [searchStart ..] windowLns
  -- Find an alias-comment whose name ends with the same trailing digits AND
  -- whose corresponding kk_field producer is in scope at the call site.
  -- Earlier versions accepted the first match anywhere in the function and
  -- produced references to SSA values defined in closed sibling scf.if
  -- regions ("use of undeclared SSA value name" mlir-opt errors).
  let aliasHits =
        [ (j, ssa)
        | (j, l) <- indexed
        , Just (an, ssa) <- [parseAliasComment l]
        , uniq `T.isSuffixOf` an
        , not ("_" `T.isPrefixOf` an)
        ]
  firstJust (tryAlias lns callLine) aliasHits
  where
    tryAlias :: [Text] -> Int -> (Int, Text) -> Maybe (Text, Int)
    tryAlias lns' cl (aliasLine, aliasSsa) = do
      let ssaNoPct = fromMaybe aliasSsa (T.stripPrefix "%" aliasSsa)
      -- Producer lives in the 10 lines before the alias comment.  Track the
      -- producer's line index so we can scope-check it against the call site.
      let producerWindow =
            [ (aliasLine - k - 1, lns' !! (aliasLine - k - 1))
            | k <- [0 .. 9], aliasLine - k - 1 >= 0 ]
      (producerLineIdx, scrut, idxVar) <- firstJust
        (\(idx, line) -> case parseKkFieldProducer ssaNoPct line of
            Just (s, iv) -> Just (idx, s, iv)
            Nothing      -> Nothing)
        producerWindow
      -- Reject the fix if the producer line is not reachable from the call
      -- site (i.e., its enclosing scf.if region has closed before cl).
      if not (ssaInScopeAt lns' producerLineIdx cl)
        then Nothing
        else do
          let idxNoPct = fromMaybe idxVar (T.stripPrefix "%" idxVar)
              constSearch = take 20 (reverse (take aliasLine lns'))
          idx <- firstJust (parseConstIdx idxNoPct) constSearch
          Just (scrut, idx)

-- | Walk the brace balance between two line indices, character by
-- character.  An SSA value declared at line @declLine@ is in scope at
-- line @useLine@ iff the relative brace depth (starting at 0 right after
-- declLine) never goes negative before reaching useLine.  Character-level
-- scanning is necessary because lines like `} else {` close the @if@
-- branch's scope mid-line even though the line's net brace delta is 0.
ssaInScopeAt :: [Text] -> Int -> Int -> Bool
ssaInScopeAt lns declLine useLine
  | declLine >= useLine = False
  | otherwise           =
      let between = take (useLine - declLine - 1) (drop (declLine + 1) lns)
          flat    = T.unpack (T.intercalate " " between)
      in walk 0 flat
  where
    walk _   []     = True
    walk rel (c:cs) = case c of
      '{' -> walk (rel + 1) cs
      '}' -> let r' = rel - 1
             in if r' < 0 then False else walk r' cs
      _   -> walk rel cs


-- | Match a line of the form
--   "%<ssa> = func.call @kk_field(%<scrut>, %<idx>) : ..." and return (scrut, idx).
parseKkFieldProducer :: Text -> Text -> Maybe (Text, Text)
parseKkFieldProducer ssaNoPct line = do
  let stripped = T.stripStart line
  rest1 <- T.stripPrefix "%" stripped
  let (n, rest2) = T.span isIdentChar rest1
  if n /= ssaNoPct then Nothing else do
    -- rest2 starts with " = func.call @kk_field(%...". DO NOT pre-strip:
    -- the prefix needs that leading space (same hazard as parseDollar0).
    rest3 <- T.stripPrefix " = func.call @kk_field(%" rest2
    let (scrut, rest4) = T.span isIdentChar rest3
    rest5 <- T.stripPrefix ", %" rest4
    let (idx, _) = T.span isIdentChar rest5
    Just (scrut, idx)

-- | Match a line of the form
--   "%<ssa> = arith.constant <N> : i64" and return N if name matches.
parseConstIdx :: Text -> Text -> Maybe Int
parseConstIdx ssaNoPct line = do
  let stripped = T.stripStart line
  rest1 <- T.stripPrefix "%" stripped
  let (n, rest2) = T.span isIdentChar rest1
  if n /= ssaNoPct then Nothing else do
    -- rest2 starts with " = arith.constant N". DO NOT pre-strip.
    rest3 <- T.stripPrefix " = arith.constant " rest2
    let (numT, _) = T.span isDigit rest3
    readInt numT

-- | Scan the stage1 MLIR cache for a 'func.func @frankenstein_<varName>(...)'
-- declaration and return its i64 parameter count. Used to synthesize PAP
-- wrappers for cross-module Frankenstein refs that aren't in the local
-- known-function set but DO have a discoverable arity in a sibling module.
findExternalArity :: Text -> Map FilePath Text -> Maybe Int
findExternalArity varName stage1 =
  let target = "func.func @frankenstein_" <> varName <> "("
      tryText txt = case T.breakOn target txt of
        (_, rest) | not (T.null rest) ->
          let afterOpen = T.drop (T.length target) rest
              (params, _) = T.breakOn ")" afterOpen
              trimmed = T.strip params
          in if T.null trimmed
               then Just 0
               else Just (countCommaSep trimmed)
        _ -> Nothing
  in firstJust tryText (Map.elems stage1)

-- | Parse @<indent>%<resultVar> = func.call \@frankenstein_<varName>$0() : ...@
parseDollar0 :: Text -> Maybe (Text, Text, Text)
parseDollar0 line =
  let (indent, rest0) = T.span (== ' ') line
  in do
    rest1 <- T.stripPrefix "%" rest0
    let (resultVar, rest2) = T.span isIdentChar rest1
    -- rest2 starts with " = func.call @frankenstein_...". Match WITHOUT
    -- pre-stripping whitespace: the prefix already includes a leading
    -- space, and stripStart would consume that space leaving "= func.call..."
    -- which fails the prefix match.
    rest3 <- T.stripPrefix " = func.call @frankenstein_" rest2
    let (varName, rest4) = T.break (== '$') rest3
    rest5 <- T.stripPrefix "$0()" rest4
    if " : () -> i64" `T.isPrefixOf` rest5
      then Just (indent, resultVar, varName)
      else Nothing

isIdentChar :: Char -> Bool
isIdentChar c = c == '_' || (c >= 'a' && c <= 'z')
             || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

findEnclosingFunc :: [Text] -> Int -> (Maybe Text, Maybe Int)
findEnclosingFunc lns pos =
  let prefix = take (pos + 1) lns
      indexed = zip [0..] prefix
      decls = [ (j, n) | (j, l) <- indexed, Just n <- [parseFuncDeclName l] ]
  in case reverse decls of
       (j, n) : _ -> (Just n, Just j)
       [] -> (Nothing, Nothing)

parseFuncDeclName :: Text -> Maybe Text
parseFuncDeclName l = do
  let l1 = T.stripStart l
  rest1 <- T.stripPrefix "func.func " l1
  let rest2 = fromMaybe rest1 (T.stripPrefix "private " rest1)
  rest3 <- T.stripPrefix "@" rest2
  let name = T.takeWhile (\c -> c /= '(') rest3
  if T.null name then Nothing else Just name

trailingDigits :: Text -> Maybe Text
trailingDigits t =
  let revDigs = T.takeWhile isDigit (T.reverse t)
  in if T.length revDigs >= 10
       then Just (T.reverse revDigs)
       else Nothing

fixByTrailingUnique :: Text -> [Text] -> Int -> Maybe Int -> Maybe Text
fixByTrailingUnique varName lns callLine funcStart = do
  uniq <- trailingDigits varName
  let searchStart = fromMaybe (max 0 (callLine - 1000)) funcStart
      indexed = zip [searchStart .. callLine - 1] (drop searchStart (take callLine lns))
  goAliases (reverse indexed) uniq
  where
    goAliases [] _ = Nothing
    goAliases ((j, l):rest) uniq =
      case parseAliasComment l of
        Just (aliasName, ssa)
          | uniq `T.isSuffixOf` aliasName
          , not ("_" `T.isPrefixOf` aliasName)
          , isInScope lns j callLine -> Just ssa
        _ -> goAliases rest uniq

-- | Parse @<indent>// let <name> = %<ssa>@
parseAliasComment :: Text -> Maybe (Text, Text)
parseAliasComment line = do
  rest1 <- T.stripPrefix "// let " (T.stripStart line)
  let (name, rest2) = T.break (== ' ') rest1
  rest3 <- T.stripPrefix " = %" rest2
  let ssa = T.takeWhile isIdentChar rest3
  Just (name, "%" <> ssa)

-- | Brace-depth walk: definition is in scope iff we never close more braces
-- than we open between def_line and use_line. The check must be INTRA-LINE:
-- a line like `} else {` flashes depth = -1 mid-line before returning to 0,
-- and we need to catch that — otherwise sibling scf.if branches falsely
-- appear in scope and we rewrite a use site to reference an SSA that's
-- only defined in a different branch.
isInScope :: [Text] -> Int -> Int -> Bool
isInScope lns defLine useLine =
  let between = drop (defLine + 1) (take useLine lns)
      -- Make `step` sticky: once depth goes negative, it stays negative.
      -- That way the per-line `foldl'` propagates a transient negative
      -- through the rest of the line and the line-end check catches it.
      step d c
        | d < 0     = -1
        | otherwise = case c of
            '{' -> d + 1
            '}' -> d - 1
            _   -> d
      go d _   | d < 0 = False
      go d []          = d >= 0
      go d (l:ls)      = go (T.foldl' step d l) ls
  in go (0 :: Int) between

-- | Locate the @__pap_frankenstein_<varName>_0@ wrapper and its arity.
findPapWrapper :: Text -> [Text] -> Maybe (Text, Int)
findPapWrapper varName lns =
  let target = "pap_frankenstein_" <> varName <> "_0"
  in firstJust (\l -> do
        rest1 <- T.stripPrefix "func.func @" (T.stripStart l)
        let (name, rest2) = T.break (== '(') rest1
        if target `T.isSuffixOf` name
          then do
            rest3 <- T.stripPrefix "(" rest2
            let (params, _) = T.break (== ')') rest3
                -- Params have form "%<name>: i64, %<name>: i64, ...".
                -- Earlier `filter (== "i64") (splitOn ",")` matched zero
                -- because each segment is "%clos: i64" not "i64", causing
                -- mkPapBlock to emit `(): -> i64` cast for the function
                -- pointer that mlir-opt rejects as a type mismatch with the
                -- actual `(i64, i64, ...) -> i64` wrapper.
                segs    = map T.strip (T.splitOn "," params)
                isI64Param s = ": i64" `T.isSuffixOf` s || s == "i64"
                ncount  = length (filter isI64Param segs)
            Just (name, ncount)
          else Nothing) lns

applyDollar0Fixes :: [Dollar0Fix] -> [Text] -> Text
applyDollar0Fixes fixes lns =
  let fixedVarNames = Set.fromList [ vn | RenameFix _ _ _ vn <- fixes ]
                   <> Set.fromList [ vn | PapFix _ _ vn _ _ <- fixes ]
                   <> Set.fromList [ vn | ExternPapFix _ _ vn _ <- fixes ]
                   <> Set.fromList [ vn | ReExtractFix _ _ vn _ _ <- fixes ]
      -- Build per-function rename map
      renamesByFunc :: Map Int (Map Text Text)
      renamesByFunc = foldl' addRename Map.empty fixes
      addRename m (RenameFix i resVar corVar _) =
        let (_, mFs) = findEnclosingFunc lns i
        in case mFs of
             Just fs -> Map.insertWith Map.union fs
                         (Map.singleton ("%" <> resVar) corVar) m
             Nothing -> m
      addRename m (PapFix i resVar _vn _papName _np) =
        let (_, mFs) = findEnclosingFunc lns i
            papCounter = i  -- crude unique id; matches Python's incremental counter shape
            papClosVar = "%_pap" <> tshow papCounter <> "_clos"
        in case mFs of
             Just fs -> Map.insertWith Map.union fs
                         (Map.singleton ("%" <> resVar) papClosVar) m
             Nothing -> m
      addRename m (ExternPapFix i resVar _vn _arity) =
        let (_, mFs) = findEnclosingFunc lns i
            papClosVar = "%_pap" <> tshow i <> "_clos"
        in case mFs of
             Just fs -> Map.insertWith Map.union fs
                         (Map.singleton ("%" <> resVar) papClosVar) m
             Nothing -> m
      -- ReExtractFix: replace the bad call with kk_field, keep the original
      -- resultVar name so subsequent uses don't need renaming.
      addRename m (ReExtractFix _ _ _ _ _) = m
      deleteLines = Set.fromList [ i | RenameFix i _ _ _ <- fixes ]
      papInsertions = Map.fromList $
        [ (i, mkPapBlock i papName nparams)
        | PapFix i _ _ papName nparams <- fixes ]
        ++
        -- arity + 1 because the synthesized wrapper has signature
        -- (%clos: i64, %r0: i64, ..., %r{arity-1}: i64) -> i64.  The
        -- mkPapBlock cast must match the actual wrapper definition or
        -- mlir-opt rejects the func.constant as a type mismatch.
        [ (i, mkPapBlock i (externPapName vn) (arity + 1))
        | ExternPapFix i _ vn arity <- fixes ]
        ++
        [ (i, mkReExtractBlock i resVar scrut idx)
        | ReExtractFix i resVar _vn scrut idx <- fixes ]
      -- Distinct (varName, arity) pairs needing synthesized wrappers + extern decls.
      externNeeds :: Map Text Int
      externNeeds = Map.fromList
        [ (vn, arity) | ExternPapFix _ _ vn arity <- fixes ]
      withModuleInjections =
        Map.foldrWithKey
          (\vn ar acc -> injectExternWrapper vn ar acc)
          (renderFixed lns fixedVarNames renamesByFunc deleteLines papInsertions)
          externNeeds
  in withModuleInjections

-- | Replace a bogus @\@frankenstein_<vn>$0()@ call with the kk_field
-- extraction that the pattern compiler should have emitted. Keeps the
-- original SSA name so downstream uses link up.
mkReExtractBlock :: Int -> Text -> Text -> Int -> [Text]
mkReExtractBlock counter resVar scrut idx =
  let pfx = "_rx" <> tshow counter
      indent = "    "
  in [ indent <> "// FIXED by ReExtractFix: was @frankenstein_<binder>$0()"
     , indent <> "%" <> pfx <> "_idx = arith.constant " <> tshow idx <> " : i64"
     , indent <> "%" <> resVar <> " = func.call @kk_field(%" <> scrut
         <> ", %" <> pfx <> "_idx) : (i64, i64) -> i64"
     ]

-- | The synthesized wrapper symbol name for a cross-module Frankenstein
-- value-position reference. Mirrors emitPapClosure's wrapper naming
-- ('pap_frankenstein_NAME_0') with an 'extern_' tag so it never collides
-- with the regular per-call-site wrappers.
externPapName :: Text -> Text
externPapName vn = "pap_extern_frankenstein_" <> vn <> "_0"

-- | Inject a private extern declaration AND a synthesized PAP wrapper for
-- @frankenstein_<vn>@ with the given arity into a module's MLIR text. The
-- wrapper forwards (clos, r0..r_{arity-1}) -> @frankenstein_<vn>(r0..)@.
injectExternWrapper :: Text -> Int -> Text -> Text
injectExternWrapper vn arity src =
  let symbol     = "frankenstein_" <> vn
      paramTys   = T.intercalate ", " (replicate arity "i64")
      sigStr     = "(" <> paramTys <> ") -> i64"
      papName    = externPapName vn
      remParams  = T.intercalate ", "
        [ "%r" <> tshow i <> ": i64" | i <- [0 .. arity - 1] ]
      remArgs    = T.intercalate ", "
        [ "%r" <> tshow i | i <- [0 .. arity - 1] ]
      externDecl = "  func.func private @" <> symbol <> sigStr
      wrapper    = T.unlines
        [ "  func.func private @" <> papName
            <> "(%clos: i64" <> (if arity > 0 then ", " <> remParams else "")
            <> ") -> i64 {"
        , "    %result = func.call @" <> symbol
            <> "(" <> remArgs <> ") : " <> sigStr
        , "    func.return %result : i64"
        , "  }"
        ]
      blob       = "\n" <> externDecl <> "\n" <> wrapper
      -- Skip extern decl / wrapper if already DEFINED (not just referenced
      -- by a call or comment).  Earlier this checked `@symbol` anywhere,
      -- which matched comments + use sites and silently suppressed the
      -- injection — producing MLIR with use-without-definition that
      -- mlir-opt rejects ("reference to undefined function").
      hasDecl    = ("func.func private @" <> symbol) `T.isInfixOf` src
                || ("func.func @" <> symbol)         `T.isInfixOf` src
      hasWrapper = ("func.func private @" <> papName) `T.isInfixOf` src
                || ("func.func @" <> papName)         `T.isInfixOf` src
      addition
        | hasWrapper = ""
        | hasDecl    = "\n" <> wrapper
        | otherwise  = blob
  in if T.null addition
       then src
       else insertAtModuleClose addition src

-- | Insert text immediately before the LAST '}' character in the module
-- text. We keep the rest of the formatting intact and only inject at the
-- module-scope close.
insertAtModuleClose :: Text -> Text -> Text
insertAtModuleClose inject src =
  case lastIndexOf "}" src of
    Just idx -> T.take idx src <> inject <> "\n" <> T.drop idx src
    Nothing  -> src <> inject  -- defensive; well-formed MLIR always has a closing brace

mkPapBlock :: Int -> Text -> Int -> [Text]
mkPapBlock counter papName nparams =
  let pfx = "_pap" <> tshow counter
      papType = T.intercalate ", " (replicate nparams "i64")
      papMlirTy = "(" <> papType <> ") -> i64"
      indent = "    "  -- approximate; the actual line indent is preserved by callers
  in [ indent <> "// FIXED: was $0() — PAP closure for " <> papName
     , indent <> "%" <> pfx <> "_tag = arith.constant 1129074515 : i64  // KK_CLOSURE_TAG"
     , indent <> "%" <> pfx <> "_one = arith.constant 1 : i64"
     , indent <> "%" <> pfx <> "_clos = func.call @kk_alloc_con(%" <> pfx
         <> "_tag, %" <> pfx <> "_one) : (i64, i64) -> i64"
     , indent <> "%" <> pfx <> "_fn = func.constant @" <> papName
         <> " : " <> papMlirTy
     , indent <> "%" <> pfx <> "_ptr = builtin.unrealized_conversion_cast %"
         <> pfx <> "_fn : " <> papMlirTy <> " to !llvm.ptr"
     , indent <> "%" <> pfx <> "_int = llvm.ptrtoint %" <> pfx
         <> "_ptr : !llvm.ptr to i64"
     , indent <> "%" <> pfx <> "_zero = arith.constant 0 : i64"
     , indent <> "func.call @kk_set_field(%" <> pfx <> "_clos, %"
         <> pfx <> "_zero, %" <> pfx <> "_int) : (i64, i64, i64) -> ()"
     ]

renderFixed
  :: [Text]
  -> Set Text                -- fixedVarNames (to strip private decls)
  -> Map Int (Map Text Text) -- per-function renames
  -> Set Int                 -- delete-line indices
  -> Map Int [Text]          -- PAP insertions
  -> Text
renderFixed lns fixedVarNames renamesByFunc deleteLines papInsertions =
  -- `step` PREPENDS to the accumulator (`note : accRev`, `l' : accRev`, etc.)
  -- to keep the per-line cost O(1), so the final accumulator is in REVERSE
  -- iteration order. Must reverse before joining or the entire output file
  -- comes out line-by-line backwards — a bug that hid silently until
  -- round 7's parseDollar0 parser fix made this code path actually run.
  T.unlines (reverse (snd (foldl' step (Nothing :: Maybe (Int, Int, Map Text Text), [])
                                       (zip [0..] lns))))
  where
    step (curRange, accRev) (i, l)
      -- Strip private decls for fixed names
      | Just vn <- parsePrivateDollar0 l, Set.member vn fixedVarNames =
          (curRange, accRev)
      -- Delete-line: replace with a "FIXED" comment so line numbers don't shift
      | Set.member i deleteLines =
          let (indent, _) = T.span (== ' ') l
              note = indent <> "// FIXED by PostProcess: was an @<x>$0() call"
          in (curRange, note : accRev)
      -- PAP insertion: replace with multi-line block
      | Just block <- Map.lookup i papInsertions =
          (curRange, reverse block ++ accRev)
      | otherwise =
          let curRange' = updateRange curRange i l
              rmap = case curRange' of
                       Just (_, _, m) -> m
                       Nothing -> Map.empty
              l' = applyRenames rmap l
          in (curRange', l' : accRev)
    updateRange cur i l =
      case parseFuncDeclName l of
        Just _ ->
          case Map.lookup i renamesByFunc of
            Just m  -> Just (i, findFuncEnd lns i, m)
            Nothing -> cur
        Nothing ->
          case cur of
            Just (s, e, _) | i > e -> Nothing
            _ -> cur

findFuncEnd :: [Text] -> Int -> Int
findFuncEnd lns start = go 0 start
  where
    go d j
      | j >= length lns = length lns - 1
      | otherwise =
          let d' = T.foldl' step d (lns !! j)
          in if d' == 0 && j > start then j else go d' (j + 1)
    step d c = case c of '{' -> d + 1; '}' -> d - 1; _ -> d

parsePrivateDollar0 :: Text -> Maybe Text
parsePrivateDollar0 line = do
  rest1 <- T.stripPrefix "func.func private @frankenstein_" (T.stripStart line)
  let (name, _rest2) = T.break (== '$') rest1
  if not (T.null name) && "$0(" `T.isPrefixOf` T.dropWhile (/= '$') rest1
    then Just name
    else Nothing

applyRenames :: Map Text Text -> Text -> Text
applyRenames renames line =
  Map.foldrWithKey replaceToken line renames
  where
    -- Replace whole-token occurrences of oldVar followed by one of [ ,):]
    replaceToken oldV newV s =
      let parts = T.splitOn oldV s
      in case parts of
           [_] -> s
           (p:ps) -> p <> T.concat
             [ if T.null seg || (T.head seg `elem` (" ,):" :: String))
                 then newV <> seg
                 else oldV <> seg
             | seg <- ps ]
           [] -> s

-------------------------------------------------------------------------------
-- Shared helpers
-------------------------------------------------------------------------------

tshow :: Int -> Text
tshow = T.pack . show

readInt :: Text -> Maybe Int
readInt t = case reads (T.unpack t) :: [(Int, String)] of
  [(n, "")] -> Just n
  _ -> Nothing

countCommaSep :: Text -> Int
countCommaSep t
  | T.null (T.strip t) = 0
  | otherwise = length [ () | s <- T.splitOn "," t, not (T.null (T.strip s)) ]

lastIndexOf :: Text -> Text -> Maybe Int
lastIndexOf needle haystack =
  let prefixes = T.breakOnAll needle haystack
  in case prefixes of
       [] -> Nothing
       _  -> let (pre, _) = last prefixes in Just (T.length pre)

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = case f x of
  Just y -> Just y
  Nothing -> firstJust f xs
