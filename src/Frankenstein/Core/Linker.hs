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
--   5. After merging, all EVar/ECon references are rewritten so that
--      unqualified call sites resolve to the correct module-qualified
--      definition.  This makes multi-file compilation work end-to-end.
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
  | AmbiguousReference Text [Text]        -- ^ unqualified name, list of modules
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
  -- Even for a single program, rewrite names for consistency so that
  -- qualified definitions are reachable from unqualified EVar sites.
  let (rwDefs, rwData, rwWarns, rwErrs) =
        rewriteNames (progDefs p) (progData p)
      rewritten = p { progDefs = rwDefs, progData = rwData }
      valWarns = validateProgram rewritten
  in case rwErrs of
    (_:_) -> Left rwErrs
    [] -> case findMain [p] of
      Right mainMod -> Right LinkResult
        { lrProgram = rewritten
        , lrWarnings = rwWarns ++ valWarns
        , lrMainModule = mainMod
        }
      Left _ | not requireMain -> Right LinkResult
        { lrProgram = rewritten
        , lrWarnings = "No main function found" : rwWarns ++ valWarns
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

      -- Rewrite cross-module name references so that EVar/ECon/PatCon
      -- references match the qualified definition names.
      (rewrittenDefs, rewrittenData, rewriteWarns, rewriteErrs) =
        rewriteNames allDefs allData

      merged = Program
            { progName    = unifiedName
            , progDefs    = rewrittenDefs
            , progData    = rewrittenData
            , progEffects = allEffects
            }

      valWarns = validateProgram merged

      mkResult mainMod warns = Right LinkResult
        { lrProgram = merged
        , lrWarnings = warns ++ valWarns
        , lrMainModule = mainMod
        }

      allErrors = dupes ++ rewriteErrs

  in case (allErrors, mainResult) of
    ([], Right mainMod) -> mkResult mainMod rewriteWarns
    ([], Left _) | not requireMain ->
      mkResult (T.intercalate "+" modNames)
               ("No main function found" : rewriteWarns)
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
-- Cross-module name rewriting
-------------------------------------------------------------------------------

-- | Mangle a module-qualified name into a flat identifier.
-- E.g. module "utils", name "helper" -> "utils_helper".
-- "main" is never mangled (the MLIR emitter expects it verbatim).
mangleName :: Text -> Text -> Text
mangleName modName nameT
  | nameT == "main" = "main"
  | T.null modName  = nameT
  | otherwise        = modName <> "_" <> nameT

-- | Symbol table: unqualified name text -> list of (module, mangled-name).
type SymbolTable = Map Text [(Text, Text)]

-- | Build a symbol table from definitions.
buildSymbolTable :: [Def] -> SymbolTable
buildSymbolTable defs =
  Map.fromListWith (++)
    [ ( nameText (qnameName (defName d))
      , [(qnameModule (defName d),
          mangleName (qnameModule (defName d))
                     (nameText (qnameName (defName d))))]
      )
    | d <- defs
    ]

-- | Build a constructor symbol table from data declarations.
buildConTable :: [DataDecl] -> SymbolTable
buildConTable decls =
  Map.fromListWith (++)
    [ ( nameText (qnameName (conName c))
      , [(qnameModule (conName c),
          mangleName (qnameModule (conName c))
                     (nameText (qnameName (conName c))))]
      )
    | dd <- decls, c <- dataCons dd
    ]

-- | Rewrite all definitions so that:
--   (a) Each definition's name is mangled to include its module prefix.
--   (b) All EVar references inside expressions are rewritten to use the
--       mangled name of the target definition.
--   (c) All ECon/PatCon references are similarly qualified.
--
-- Returns (rewritten defs, rewritten data, warnings, errors).
rewriteNames :: [Def] -> [DataDecl]
             -> ([Def], [DataDecl], [Text], [LinkError])
rewriteNames defs dataDecls =
  let symTab  = buildSymbolTable defs
      conTab  = buildConTable dataDecls

      -- Mangle definition names themselves
      mangledDefs = map mangleDef defs

      -- For each def, determine its home module, then rewrite its body
      (rewrittenDefs, allWarns, allErrs) =
        foldr rewriteOne ([], [], []) mangledDefs

      rewriteOne d (ds, ws, es) =
        let homeMod = qnameModule (defName d)
            (expr', ws', es') = rewriteExpr homeMod symTab conTab (defExpr d)
            d' = d { defExpr = expr' }
        in (d' : ds, ws' ++ ws, es' ++ es)

      -- Mangle constructor QNames in data declarations
      mangledData = map mangleDataDecl dataDecls

  in (rewrittenDefs, mangledData, allWarns, allErrs)

-- | Mangle a single definition's name by baking the module prefix into
-- the nameText of qnameName.
mangleDef :: Def -> Def
mangleDef d =
  let qn = defName d
      modN = qnameModule qn
      unqual = nameText (qnameName qn)
      mangled = mangleName modN unqual
  in d { defName = qn { qnameName = (qnameName qn) { nameText = mangled } } }

-- | Mangle constructor names in a data declaration.
mangleDataDecl :: DataDecl -> DataDecl
mangleDataDecl dd =
  dd { dataCons = map mangleConDecl (dataCons dd) }

mangleConDecl :: ConDecl -> ConDecl
mangleConDecl c =
  let qn = conName c
      modN = qnameModule qn
      unqual = nameText (qnameName qn)
      mangled = mangleName modN unqual
  in c { conName = qn { qnameName = (qnameName qn) { nameText = mangled } } }

-- | Resolve an unqualified name against the symbol table.
-- Prefers the entry from homeMod if there are multiple providers.
-- Returns (resolved name, warnings, errors).
resolveName :: Text -> SymbolTable -> Name
            -> (Name, [Text], [LinkError])
resolveName homeMod table nm =
  let unqual = nameText nm
  in case Map.lookup unqual table of
    Nothing ->
      -- Not a known definition -- could be a local variable,
      -- a lambda parameter, a let binding, or a primitive/runtime name.
      (nm, [], [])
    Just [(_, mangled)] ->
      -- Unique: rewrite unconditionally.
      (nm { nameText = mangled }, [], [])
    Just candidates ->
      -- Multiple providers -- try to pick the one from our home module.
      case filter (\(m, _) -> m == homeMod) candidates of
        [(_,mangled)] -> (nm { nameText = mangled }, [], [])
        [] ->
          -- None from home module.  If all candidates mangle to the
          -- same text (same unqualified name, just re-exported), pick any.
          let mangledNames = nubTexts (map snd candidates)
          in case mangledNames of
            [m] -> (nm { nameText = m }, [], [])
            _   -> (nm, [],
                    [AmbiguousReference unqual (map fst candidates)])
        ((_,mangled):_) ->
          -- Multiple definitions from the same module -- shouldn't happen
          -- after duplicate checking, but be safe.
          (nm { nameText = mangled },
              ["Warning: multiple definitions of '" <> unqual
                <> "' within module '" <> homeMod <> "'"],
              [])

-- | Resolve a QName against a symbol table (for ECon / PatCon).
resolveQName :: Text -> SymbolTable -> QName -> (QName, [Text], [LinkError])
resolveQName homeMod table qn =
  let unqual = nameText (qnameName qn)
  in case Map.lookup unqual table of
    Nothing -> (qn, [], [])
    Just [(modN, mangled)] ->
      ( qn { qnameModule = modN
           , qnameName = (qnameName qn) { nameText = mangled } }
      , [], [])
    Just candidates ->
      -- Prefer the module already on the QName, then same-module.
      let preferred
            | not (T.null (qnameModule qn)) =
                filter (\(m, _) -> m == qnameModule qn) candidates
            | otherwise =
                filter (\(m, _) -> m == homeMod) candidates
      in case preferred of
        [(modN, mangled)] ->
          ( qn { qnameModule = modN
               , qnameName = (qnameName qn) { nameText = mangled } }
          , [], [])
        [] ->
          let mangledNames = nubTexts (map snd candidates)
          in case mangledNames of
            [_] ->
              let (modN, mangled) = case candidates of
                    (x:_) -> x
                    []    -> ("", unqual)  -- unreachable
              in ( qn { qnameModule = modN
                       , qnameName = (qnameName qn) { nameText = mangled } }
                 , [], [])
            _ -> (qn, [],
                  [AmbiguousReference unqual (map fst candidates)])
        ((modN, mangled):_) ->
          ( qn { qnameModule = modN
               , qnameName = (qnameName qn) { nameText = mangled } }
          , [], [])

-- | Rewrite all name references within an expression.
-- Takes the home module, symbol table for defs, symbol table for
-- constructors, and returns (rewritten expr, warnings, errors).
rewriteExpr :: Text -> SymbolTable -> SymbolTable -> Expr
            -> (Expr, [Text], [LinkError])
rewriteExpr homeMod symTab conTab = go Set.empty
  where
    -- 'locals' tracks names bound by lambda/let/case that should NOT
    -- be rewritten (they shadow top-level definitions).
    go locals expr = case expr of
      EVar n
        | Set.member (nameText n) locals -> (expr, [], [])
        | otherwise ->
            let (n', ws, es) = resolveName homeMod symTab n
            in (EVar n', ws, es)

      ELit _ -> (expr, [], [])

      ECon qn ->
        let (qn', ws, es) = resolveQName homeMod conTab qn
        in (ECon qn', ws, es)

      EApp f args ->
        let (f', ws1, es1) = go locals f
            (args', ws2, es2) = goList locals args
        in (EApp f' args', ws1 ++ ws2, es1 ++ es2)

      ELam params body ->
        let bound = Set.fromList [nameText n | (n, _) <- params]
            locals' = Set.union locals bound
            (body', ws, es) = go locals' body
        in (ELam params body', ws, es)

      ELet bgs body ->
        let -- All names bound in this let (visible in all groups + body)
            bound = Set.fromList
              [nameText (bindName b) | bg <- bgs, b <- bg]
            locals' = Set.union locals bound
            (bgs', ws1, es1) = goBindGroups locals' bgs
            (body', ws2, es2) = go locals' body
        in (ELet bgs' body', ws1 ++ ws2, es1 ++ es2)

      ECase scrut branches ->
        let (scrut', ws1, es1) = go locals scrut
            (branches', ws2, es2) = goBranches locals branches
        in (ECase scrut' branches', ws1 ++ ws2, es1 ++ es2)

      ETypeApp e tys ->
        let (e', ws, es) = go locals e
        in (ETypeApp e' tys, ws, es)

      ETypeLam tvs e ->
        let (e', ws, es) = go locals e
        in (ETypeLam tvs e', ws, es)

      EPerform qn args ->
        let (args', ws, es) = goList locals args
        in (EPerform qn args', ws, es)

      EHandle eff handler body ->
        let (handler', ws1, es1) = go locals handler
            (body', ws2, es2) = go locals body
        in (EHandle eff handler' body', ws1 ++ ws2, es1 ++ es2)

      ERetain e ->
        let (e', ws, es) = go locals e in (ERetain e', ws, es)

      ERelease e ->
        let (e', ws, es) = go locals e in (ERelease e', ws, es)

      EDrop e ->
        let (e', ws, es) = go locals e in (EDrop e', ws, es)

      EReuse e1 e2 ->
        let (e1', ws1, es1) = go locals e1
            (e2', ws2, es2) = go locals e2
        in (EReuse e1' e2', ws1 ++ ws2, es1 ++ es2)

      EDelay e ->
        let (e', ws, es) = go locals e in (EDelay e', ws, es)

      EForce e ->
        let (e', ws, es) = go locals e in (EForce e', ws, es)

    goList locals exprs =
      let results = map (go locals) exprs
      in ( map (\(e,_,_) -> e) results
         , concatMap (\(_,w,_) -> w) results
         , concatMap (\(_,_,e) -> e) results
         )

    goBindGroups locals bgs =
      let results = map (goBindGroup locals) bgs
      in ( map (\(bg,_,_) -> bg) results
         , concatMap (\(_,w,_) -> w) results
         , concatMap (\(_,_,e) -> e) results
         )

    goBindGroup locals bg =
      let results = map (goBind locals) bg
      in ( map (\(b,_,_) -> b) results
         , concatMap (\(_,w,_) -> w) results
         , concatMap (\(_,_,e) -> e) results
         )

    goBind locals bnd =
      let (e', ws, es) = go locals (bindExpr bnd)
      in (bnd { bindExpr = e' }, ws, es)

    goBranches locals branches =
      let results = map (goBranch locals) branches
      in ( map (\(br,_,_) -> br) results
         , concatMap (\(_,w,_) -> w) results
         , concatMap (\(_,_,e) -> e) results
         )

    goBranch locals (Branch pat guard body) =
      let bound = patternBinds pat
          locals' = Set.union locals bound
          (pat', ws1, es1) = rewritePattern pat
          (guard', ws2, es2) = case guard of
            Nothing -> (Nothing, [], [])
            Just g  -> let (g', w, e) = go locals' g
                       in (Just g', w, e)
          (body', ws3, es3) = go locals' body
      in (Branch pat' guard' body', ws1 ++ ws2 ++ ws3, es1 ++ es2 ++ es3)

    patternBinds :: Pattern -> Set Text
    patternBinds (PatCon _ pats) = Set.unions (map patternBinds pats)
    patternBinds (PatVar n _) = Set.singleton (nameText n)
    patternBinds (PatWild _) = Set.empty
    patternBinds (PatLit _) = Set.empty

    -- Rewrite constructor references inside patterns.
    rewritePattern :: Pattern -> (Pattern, [Text], [LinkError])
    rewritePattern (PatCon qn pats) =
      let (qn', ws1, es1) = resolveQName homeMod conTab qn
          results = map rewritePattern pats
          pats' = map (\(p,_,_) -> p) results
          ws2 = concatMap (\(_,w,_) -> w) results
          es2 = concatMap (\(_,_,e) -> e) results
      in (PatCon qn' pats', ws1 ++ ws2, es1 ++ es2)
    rewritePattern p@(PatVar _ _) = (p, [], [])
    rewritePattern p@(PatWild _) = (p, [], [])
    rewritePattern p@(PatLit _) = (p, [], [])

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
  , "kk_evv_create", "kk_evv_set", "kk_evv_get", "kk_unhandled_effect"
  , "kk_thunk_create", "kk_thunk_force"
  , "evv_select"  -- evidence selection intrinsic (generated by evidence pass)
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
