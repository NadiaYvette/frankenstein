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
--
-- After merging, cross-module validation detects:
--   - Undefined symbols (referenced but not defined or primitive)
--   - Arity mismatches (call-site arg count vs definition)
--   - Duplicate exports (same unqualified name from multiple modules)

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
import Data.Set (Set)
import qualified Data.Set as Set
-- Data.List not needed; nub/nubTexts are defined locally

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
  let valWarns = validateProgram p
  in case findMain [p] of
    Right mainMod -> Right LinkResult
      { lrProgram = p
      , lrWarnings = valWarns
      , lrMainModule = mainMod
      }
    Left _ | not requireMain -> Right LinkResult
      { lrProgram = p
      , lrWarnings = "No main function found" : valWarns
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

      merged = Program
            { progName    = unifiedName
            , progDefs    = allDefs
            , progData    = allData
            , progEffects = allEffects
            }

      valWarns = validateProgram merged

      mkResult mainMod warns = Right LinkResult
        { lrProgram = merged
        , lrWarnings = warns ++ valWarns
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

-------------------------------------------------------------------------------
-- Cross-module validation
-------------------------------------------------------------------------------

-- | Run all validation checks on a merged program, returning warnings.
validateProgram :: Program -> [Text]
validateProgram prog =
  detectUndefinedSymbols prog
  ++ detectArityMismatches prog
  ++ detectDuplicateExports prog

-- A. Undefined symbol detection

-- | Known primitive operators that are provided by the runtime.
primitiveNames :: Set Text
primitiveNames = Set.fromList
  [ "+", "-", "*", "/", "%"
  , "==", "!=", "<", ">", "<=", ">="
  , "&&", "||", "!"
  , "negate"
  , "mod", "div", "rem"
  ]

-- | Known runtime functions (Koka/Perceus runtime, C runtime).
runtimeNames :: Set Text
runtimeNames = Set.fromList
  [ "kk_drop", "kk_retain", "kk_release", "kk_reuse"
  , "kk_dup", "kk_free", "kk_decref", "kk_incref"
  , "kk_box", "kk_unbox", "kk_unit"
  , "printf", "puts", "exit", "malloc", "free"
  , "fold", "map", "filter"  -- common higher-order builtins
  ]

-- | Detect references to symbols that are neither defined nor primitive.
detectUndefinedSymbols :: Program -> [Text]
detectUndefinedSymbols prog =
  let -- All defined names (by unqualified text, since EVar uses Name)
      definedNames :: Set Text
      definedNames = Set.fromList
        [ nameText (qnameName (defName d)) | d <- progDefs prog ]

      -- All defined QNames (for ECon checks)
      definedQNames :: Set (Text, Text)
      definedQNames = Set.fromList
        [ (qnameModule (defName d), nameText (qnameName (defName d)))
        | d <- progDefs prog ]

      -- Constructor names from data declarations
      conQNames :: Set (Text, Text)
      conQNames = Set.fromList
        [ (qnameModule (conName c), nameText (qnameName (conName c)))
        | dd <- progData prog, c <- dataCons dd ]

      conNames :: Set Text
      conNames = Set.fromList
        [ nameText (qnameName (conName c))
        | dd <- progData prog, c <- dataCons dd ]

      -- All names that are considered "known"
      allKnown = Set.unions [definedNames, conNames, primitiveNames, runtimeNames]

      -- All QNames that are considered "known" (for qualified references)
      allKnownQ = Set.union definedQNames conQNames

      -- Collect all referenced names from expressions
      refdNames :: Set Text
      refdNames = Set.fromList $ concatMap (collectVarNames . defExpr) (progDefs prog)

      refdQNames :: Set (Text, Text)
      refdQNames = Set.fromList $ concatMap (collectConQNames . defExpr) (progDefs prog)

      -- Undefined unqualified names
      undefNames = Set.toList $ Set.difference refdNames allKnown

      -- Undefined qualified constructor references (only check if module is non-empty)
      undefQNames =
        [ (m, n) | (m, n) <- Set.toList refdQNames
        , not (Set.member (m, n) allKnownQ)
        , not (Set.member n allKnown)
        ]

  in [ "Warning: undefined symbol '" <> n <> "'" | n <- undefNames ]
     ++ [ "Warning: undefined constructor '" <> m <> "." <> n <> "'"
        | (m, n) <- undefQNames ]

-- | Collect all unqualified variable names referenced in an expression.
collectVarNames :: Expr -> [Text]
collectVarNames = go
  where
    go (EVar n) = [nameText n]
    go (ELit _) = []
    go (ECon _) = []  -- handled separately as QNames
    go (EApp f args) = go f ++ concatMap go args
    go (ELam params body) =
      let bound = Set.fromList [nameText n | (n, _) <- params]
          bodyRefs = go body
      in filter (`Set.notMember` bound) bodyRefs
    go (ELet bgs body) =
      let bound = Set.fromList [nameText (bindName b) | bg <- bgs, b <- bg]
          bgRefs = concatMap (\bg -> concatMap (go . bindExpr) bg) bgs
          bodyRefs = go body
      in filter (`Set.notMember` bound) (bgRefs ++ bodyRefs)
    go (ECase scrut branches) =
      go scrut ++ concatMap goBranch branches
    go (ETypeApp e _) = go e
    go (ETypeLam _ e) = go e
    go (EPerform _ args) = concatMap go args
    go (EHandle _ handler body) = go handler ++ go body
    go (ERetain e) = go e
    go (ERelease e) = go e
    go (EDrop e) = go e
    go (EReuse e1 e2) = go e1 ++ go e2
    go (EDelay e) = go e
    go (EForce e) = go e

    goBranch (Branch pat guard body) =
      let bound = patternBinds pat
          guardRefs = maybe [] go guard
          bodyRefs = go body
      in filter (`Set.notMember` bound) (guardRefs ++ bodyRefs)

    patternBinds :: Pattern -> Set Text
    patternBinds (PatCon _ pats) = Set.unions (map patternBinds pats)
    patternBinds (PatVar n _) = Set.singleton (nameText n)
    patternBinds (PatWild _) = Set.empty
    patternBinds (PatLit _) = Set.empty

-- | Collect all qualified constructor names referenced in an expression.
collectConQNames :: Expr -> [(Text, Text)]
collectConQNames = go
  where
    go (ECon qn) = [(qnameModule qn, nameText (qnameName qn))]
    go (EVar _) = []
    go (ELit _) = []
    go (EApp f args) = go f ++ concatMap go args
    go (ELam _ body) = go body
    go (ELet bgs body) =
      concatMap (\bg -> concatMap (go . bindExpr) bg) bgs ++ go body
    go (ECase scrut branches) =
      go scrut ++ concatMap goBranch branches
    go (ETypeApp e _) = go e
    go (ETypeLam _ e) = go e
    go (EPerform _ args) = concatMap go args
    go (EHandle _ handler body) = go handler ++ go body
    go (ERetain e) = go e
    go (ERelease e) = go e
    go (EDrop e) = go e
    go (EReuse e1 e2) = go e1 ++ go e2
    go (EDelay e) = go e
    go (EForce e) = go e

    goBranch (Branch _ guard body) =
      maybe [] go guard ++ go body

-- B. Arity mismatch detection

-- | Detect call sites where argument count doesn't match definition arity.
detectArityMismatches :: Program -> [Text]
detectArityMismatches prog =
  let -- Build map from unqualified name -> arity (number of ELam params)
      arityMap :: Map Text Int
      arityMap = Map.fromList
        [ (nameText (qnameName (defName d)), arity)
        | d <- progDefs prog
        , let arity = countLamParams (defExpr d)
        , arity > 0  -- only track functions, not values
        , not (isAnyType (defType d))  -- skip if type is unknown
        ]

      -- Walk all expressions looking for EApp (EVar qn) args
      checkDef d = collectAppArities (defExpr d)

      mismatches =
        [ "Warning: arity mismatch for '" <> name <> "': called with "
          <> T.pack (show callArity) <> " args, but defined with "
          <> T.pack (show defArity)
        | d <- progDefs prog
        , (name, callArity) <- checkDef d
        , Just defArity <- [Map.lookup name arityMap]
        , callArity /= defArity
        ]

  in nubTexts mismatches

-- | Count the number of leading ELam parameters in an expression.
countLamParams :: Expr -> Int
countLamParams (ELam params _) = length params
countLamParams (ETypeLam _ e)  = countLamParams e  -- skip type abstractions
countLamParams _               = 0

-- | Check if a type is the "any" catch-all type (TAny equivalent).
isAnyType :: Type -> Bool
isAnyType (TCon tc) = nameText (qnameName (tcName tc)) == "any"
isAnyType _ = False

-- | Collect all (function-name, arg-count) pairs from EApp (EVar _) sites.
collectAppArities :: Expr -> [(Text, Int)]
collectAppArities = go
  where
    go (EApp (EVar n) args) = (nameText n, length args) : concatMap go args
    go (EApp f args) = go f ++ concatMap go args
    go (EVar _) = []
    go (ELit _) = []
    go (ECon _) = []
    go (ELam _ body) = go body
    go (ELet bgs body) =
      concatMap (\bg -> concatMap (go . bindExpr) bg) bgs ++ go body
    go (ECase scrut branches) =
      go scrut ++ concatMap goBranch branches
    go (ETypeApp e _) = go e
    go (ETypeLam _ e) = go e
    go (EPerform _ args) = concatMap go args
    go (EHandle _ handler body) = go handler ++ go body
    go (ERetain e) = go e
    go (ERelease e) = go e
    go (EDrop e) = go e
    go (EReuse e1 e2) = go e1 ++ go e2
    go (EDelay e) = go e
    go (EForce e) = go e

    goBranch (Branch _ guard body) =
      maybe [] go guard ++ go body

-- C. Duplicate export detection

-- | Detect when multiple modules export the same unqualified name.
detectDuplicateExports :: Program -> [Text]
detectDuplicateExports prog =
  let -- Group public definitions by unqualified name
      exports :: Map Text [Text]  -- unqualified name -> list of modules
      exports = Map.fromListWith (++)
        [ (nameText (qnameName (defName d)), [qnameModule (defName d)])
        | d <- progDefs prog
        , defVisibility d == Public
        ]

      dupes =
        [ "Warning: ambiguous export '" <> name <> "' defined in modules: "
          <> T.intercalate ", " (nubTexts mods)
        | (name, mods) <- Map.toList exports
        , let uniqMods = nubTexts mods
        , length uniqMods > 1
        ]
  in dupes

-- | Remove duplicate Text values, preserving order.
nubTexts :: [Text] -> [Text]
nubTexts = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | Set.member x seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs
