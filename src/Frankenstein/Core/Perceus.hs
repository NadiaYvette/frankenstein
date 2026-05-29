-- | Perceus Reference Counting Insertion
--
-- A Core-to-Core pass that inserts retain/drop/reuse operations
-- based on variable usage and multiplicity annotations.
--
-- Key rules (from Koka's Perceus algorithm):
--   Linear values:  used exactly once → no refcount ops
--   Affine values:  used at most once → drop if unused, no retain
--   Many values:    retain for each use beyond the first, drop at scope exit
--
-- This pass runs after all bridges have translated to Core and before
-- MLIR emission.

module Frankenstein.Core.Perceus
  ( insertPerceus
  , analyzeUsage
  , freeVars
  , UsageInfo(..)
  , isUnboxedType
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Check if a type is known to be unboxed (non-heap-allocated).
-- Values of these types are plain machine integers, not heap pointers,
-- so retain/drop calls on them are no-ops at runtime. Eliding them
-- at compile time avoids the function call overhead.
isUnboxedType :: Type -> Bool
isUnboxedType (TCon tc) = isUnboxedTyCon (tcName tc)
isUnboxedType (TApp (TCon tc) _) = isUnboxedTyCon (tcName tc)
isUnboxedType _ = False

isUnboxedTyCon :: QName -> Bool
isUnboxedTyCon (QName modT (Name nameT _)) =
  -- std.int / std.unit / std.bool (used by most bridges)
  (modT == "std" && nameT `elem` ["int", "unit", "bool", "char", "float"])
  -- GHC primitive types: Int#, Word#, Char#, Double#, Float#
  || (modT == "GHC.Prim" || modT == "GHC.Internal.Prim")
     && nameT `elem` ["Int#", "Word#", "Char#", "Double#", "Float#", "Int64#", "Word64#"]
  -- GHC boxed wrappers that we unbox: Int, Char, Bool, Word
  || (modT == "GHC.Types" || modT == "GHC.Internal.Types"
      || modT == "GHC.Internal.Base"
      || modT == "GHC.Internal.Int" || modT == "GHC.Internal.Word"
      || modT == "GHC.Internal.Data.Bool")
     && nameT `elem` ["Int", "Char", "Bool", "Word", "Int64", "Word64"]

-- | Usage count for a variable
data UsageInfo = UsageInfo
  { usageCount       :: !Int
  , usageMultiplicity :: !Multiplicity
  } deriving (Show, Eq)

type UsageMap = Map Name UsageInfo

-- | Insert Perceus reference counting operations into a program.
insertPerceus :: Program -> Program
insertPerceus prog = prog
  { progDefs = map perceusDefTransform (progDefs prog)
  }

perceusDefTransform :: Def -> Def
perceusDefTransform def = def
  { defExpr = perceusExpr scope0 (defExpr def)
  }
  where
    -- Initial scope: function parameters
    scope0 = case defExpr def of
      ELam params _ -> Map.fromList
        [ (n, paramMultiplicity n params (defType def))
        | (n, _) <- params ]
      _ -> Map.empty

-- | Look up the multiplicity of a parameter from the function type.
-- Indexes into the MulType list by position.
paramMultiplicity :: Name -> [(Name, Type)] -> Type -> Multiplicity
paramMultiplicity n params (TFun args _ _) =
  case lookup n (zip (map fst params) (map fst args)) of
    Just m  -> m
    Nothing -> Many
paramMultiplicity _ _ _ = Many

-- | Transform an expression, inserting retain/drop as needed.
-- The scope maps variables to their multiplicity.
perceusExpr :: Map Name Multiplicity -> Expr -> Expr
perceusExpr scope expr = case expr of
  -- Variable: retain/drop decisions happen at binding sites
  EVar _ -> expr

  ELit _ -> expr
  ECon _ -> expr

  -- Application: recurse into subexpressions
  EApp fn args ->
    EApp (perceusExpr scope fn) (map (perceusExpr scope) args)

  -- Lambda: extend scope with parameter multiplicities, drop unused, retain multi-use
  ELam params body ->
    let scope' = foldl (\s (n, _) -> Map.insert n Many s) scope params
        -- Analyze which params are used in body
        bodyFree = freeVars body
        usage = analyzeUsage body
        -- Skip retain/drop for params with unboxed types (plain integers, not heap ptrs)
        unusedParams = [ n | (n, t) <- params
                       , not (Set.member n bodyFree)
                       , Map.findWithDefault Many n scope' /= Linear
                       , not (isUnboxedType t) ]
        body' = perceusExpr scope' body
        -- Drop unused affine/many params
        droppedBody = foldr (\n e -> ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar n)) DefVal]] e)
                            body' unusedParams
        -- Insert retains for Many-multiplicity params used more than once
        retainedBody = foldr
          (\(n, t) e ->
            let m = Map.findWithDefault Many n scope'
                count = Map.findWithDefault 0 n usage
            in if m == Many && count > 1 && not (isUnboxedType t)
               then wrapRetains n count e
               else e)
          droppedBody params
    in ELam params retainedBody

  -- Let: insert drops for unused bindings, retains for multi-use bindings
  ELet bgs body ->
    let -- Collect all bound names with their multiplicities and types
        boundInfo = [ (bindName b, bindMultiplicity b, bindType b)
                    | bg <- bgs, b <- bg ]
        boundNames = [ (n, m) | (n, m, _) <- boundInfo ]
        scope' = foldl (\s (n, m) -> Map.insert n m s) scope boundNames
        bodyFree = freeVars body
        usage = analyzeUsage body
        -- Transform binding expressions
        bgs' = map (map (perceusBindGroup scope')) bgs
        body' = perceusExpr scope' body
        -- Find bindings unused in body that need dropping (skip unboxed types)
        toDrop = [ n | (n, m, t) <- boundInfo
                 , not (Set.member n bodyFree)
                 , m /= Linear
                 , not (isUnboxedType t) ]
        droppedBody = foldr (\n e -> ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar n)) DefVal]] e)
                            body' toDrop
        -- Insert retains for Many-multiplicity vars used more than once (skip unboxed)
        retainedBody = foldr
          (\(n, m, t) e ->
            let count = Map.findWithDefault 0 n usage
            in if m == Many && count > 1 && not (isUnboxedType t)
               then wrapRetains n count e
               else e)
          droppedBody boundInfo
    in ELet bgs' retainedBody

  -- Case: Koka-style ownership.  When the scrutinee is a variable we can
  -- drop it inside each branch (consuming it).  Pattern-bound fields that
  -- the branch actually uses are retained first so they survive the
  -- recursive kk_drop of the scrutinee.  Dead fields are NOT dropped
  -- individually — they are freed when the scrutinee's rc reaches 0 and
  -- kk_drop recursively visits its children.
  --
  -- This is safe for shared scrutinees: if the scrutinee has rc > 1 the
  -- drop merely decrements without freeing, so the children stay alive for
  -- the next selector call.
  ECase scrut branches ->
    -- Normalize ANY non-EVar scrutinee by binding it to a fresh name
    -- and recurring.  The Koka-style ownership path (perceusBranch's
    -- mScrutVar=Just case) retains used fields and then drops the
    -- scrutinee, letting the recursive kk_drop cascade unused fields.
    -- When the scrutinee is a complex expression, mScrutVar would
    -- otherwise be Nothing and we'd take the "drop individual unused
    -- fields" fallback — which over-drops borrowed-from-field refs
    -- (kk_field returns a borrowed pointer, not a retained one).
    -- That manifests under KK_RECYCLE=1 as kk_thunk_force reading the
    -- KK_RECYCLE_FLAG sentinel of a recycled thunk cell.
    case scrut of
      EVar _ ->
        let scrut' = perceusExpr scope scrut
            mScrutVar = case scrut of
              EVar n -> Just n
              _      -> Nothing
            -- Per-branch usage of OUTER variables (free in branch body
            -- but not bound by the branch's pattern).  ECase counts
            -- branches via max, so the outer wrapRetains is sized for
            -- the heaviest branch; lighter branches must emit (max −
            -- this) drops or they leak (max − this) refs each.  This
            -- is the classic Perceus "balance branches" rule.
            --
            -- Concrete trigger: dropWhile's case-on-result-bool has
            -- TRUE = 1 use of predicate (recurse), FALSE = 0 uses
            -- (just build Cons).  Without this drop, every dropWhile
            -- termination at the FALSE branch leaked one closure ref
            -- — surd-quintic's 17.5M live CLOS cells.
            patVarsSet br =
              Set.fromList (map fst (patternVars (branchPattern br)))
            scrutSet = case mScrutVar of
              Just n  -> Set.singleton n  -- perceusBranch already drops it
              Nothing -> Set.empty
            -- Only emit drops on variables actually in scope.  Free
            -- EVars not in scope are top-level fn refs, not heap
            -- pointers — dropping them triggers PAP wrapper allocation
            -- and segfaults.
            branchOuterUsage br =
              let patNames = patVarsSet br
                  fullUsage = analyzeUsage (branchBody br)
                  excluded k =  Set.member k patNames
                             || Set.member k scrutSet
                             || not (Map.member k scope)
              in Map.filterWithKey (\k _ -> not (excluded k)) fullUsage
            branchUsages = map branchOuterUsage branches
            maxUsage = Map.unionsWith max branchUsages
            mkDrops br' bu =
              let body = branchBody br'
                  vars = Map.keys maxUsage
                  emitDrop n e = ELet [[Bind (Name "_drop" 0) unitType
                                          (EDrop (EVar n)) DefVal]] e
                  countDiff n = Map.findWithDefault 0 n maxUsage
                              - Map.findWithDefault 0 n bu
                  body' = foldr
                    (\n e ->
                       let d = countDiff n
                       in if d > 0 then iterate (emitDrop n) e !! d else e)
                    body vars
              in br' { branchBody = body' }
            branches' = zipWith
              (\br bu -> mkDrops (perceusBranch scope mScrutVar br) bu)
              branches branchUsages
        in ECase scrut' branches'
      _ ->
        let dsName = Name "_scrutinee" 0
            dsType = TCon (TypeCon (QName "" (Name "any" 0)) KindValue)
            dsBind = Bind dsName dsType scrut DefVal
            innerCase = ECase (EVar dsName) branches
        in perceusExpr scope (ELet [[dsBind]] innerCase)

  -- Retain/Drop/Release already present: recurse
  ERetain e -> ERetain (perceusExpr scope e)
  ERelease e -> ERelease (perceusExpr scope e)
  EDrop e -> EDrop (perceusExpr scope e)
  EReuse e1 e2 -> EReuse (perceusExpr scope e1) (perceusExpr scope e2)

  -- Laziness
  EDelay e -> EDelay (perceusExpr scope e)
  EForce e -> EForce (perceusExpr scope e)

  -- Type operations pass through
  ETypeApp e ts -> ETypeApp (perceusExpr scope e) ts
  ETypeLam tvs e -> ETypeLam tvs (perceusExpr scope e)

  -- Effects pass through
  EPerform qn args -> EPerform qn (map (perceusExpr scope) args)
  EHandle eff handler body ->
    EHandle eff (perceusExpr scope handler) (perceusExpr scope body)

  EFunRef _ -> expr

perceusBindGroup :: Map Name Multiplicity -> Bind -> Bind
perceusBindGroup scope b = b { bindExpr = perceusExpr scope (bindExpr b) }

perceusBranch :: Map Name Multiplicity -> Maybe Name -> Branch -> Branch
perceusBranch scope mScrutVar br =
  let -- Extend scope with pattern-bound variables
      patVars = patternVars (branchPattern br)
      scope' = foldl (\s (n, _) -> Map.insert n Many s) scope patVars
      body = branchBody br
      bodyFree = freeVars body
      -- Analyze on the PRE-Perceus body so we count real consuming uses,
      -- not retains we add ourselves.
      bodyUsage = analyzeUsage body
      body' = perceusExpr scope' body

      -- Used pattern variables (need retaining before scrutinee drop) paired
      -- with their consuming-use count.  Skip retain for unboxed types —
      -- they're plain integers, not heap ptrs.
      usedPats = [ (n, Map.findWithDefault 0 n bodyUsage)
                 | (n, t) <- patVars
                 , Set.member n bodyFree
                 , not (isUnboxedType t) ]
      -- Unused pattern variables (for fallback when no scrutinee var)
      unusedPats = [ n | (n, t) <- patVars
                   , not (Set.member n bodyFree)
                   , Map.findWithDefault Many n scope' /= Linear
                   , not (isUnboxedType t) ]
  in case mScrutVar of
    Just sv ->
      -- Koka-style: retain used fields FIRST, then drop the scrutinee.
      -- Order matters: retain must precede drop because kk_drop is
      -- recursive and would free the fields before retain bumps their rc.
      --
      -- Retain math: kk_field loads the pointer without bumping refcount,
      -- so the field enters at rc=1 (owned by parent).  After N retains
      -- and the scrutinee drop (which recursively decrements children by
      -- 1), the field has refcount N.  For @cnt@ consuming uses we need
      -- final rc = cnt, hence N = cnt retains via @wrapRetains n (cnt+1)@.
      --
      -- Even when patVars is empty (e.g. the Nil branch of a list
      -- match), the scrutinee must still be dropped — otherwise empty
      -- list values (and any 0-arity constructor) leak forever, which
      -- showed up under KK_STATS=1 as 27.9M live NIL cells on
      -- surd-quintic.  usedPats is empty in that case so the retain
      -- wrapping is a no-op.
      let -- First: drop the scrutinee (innermost — emitted AFTER retains)
          droppedBody = ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar sv)) DefVal]]
                             body'
          -- Then: wrap retains around the drop (outermost — emitted BEFORE drop)
          retainedBody = foldr
            (\(n, cnt) e -> wrapRetains n (cnt + 1) e)
            droppedBody usedPats
      in br { branchBody = retainedBody }
    Nothing ->
      -- No scrutinee variable (complex expression).  Drop unused pattern
      -- variables individually.
      let droppedBody = foldr
            (\n e -> ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar n)) DefVal]] e)
            body' unusedPats
      in br { branchBody = droppedBody }

-- | Extract multiplicity from a Bind.
-- Always returns Many since Bind carries no explicit multiplicity field.
-- Actual usage-based refcount decisions are handled by analyzeUsage/wrapRetains/wrapDrops.
-- The old code incorrectly extracted the multiplicity of the first function *argument*.
bindMultiplicity :: Bind -> Multiplicity
bindMultiplicity _ = Many

-- | Wrap an expression with (N-1) retain operations for a variable.
-- Mirrors the K spec's wrapRetains(Name, Count, Expr).
-- If count <= 1, no retains needed. Otherwise insert (count-1) retains.
wrapRetains :: Name -> Int -> Expr -> Expr
wrapRetains _ count body | count <= 1 = body
wrapRetains n count body =
  ELet [[Bind (Name "_retain" 0) unitType (ERetain (EVar n)) DefVal]]
       (wrapRetains n (count - 1) body)

-- | Collect free variables of an expression
freeVars :: Expr -> Set Name
freeVars (EVar n)         = Set.singleton n
freeVars (ELit _)         = Set.empty
freeVars (ECon _)         = Set.empty
freeVars (EApp f args)    = Set.unions (freeVars f : map freeVars args)
freeVars (ELam ps body)   = freeVars body `Set.difference` Set.fromList (map fst ps)
freeVars (ELet bgs body)  =
  let bound = Set.fromList [bindName b | bg <- bgs, b <- bg]
      bindFvs = Set.unions [freeVars (bindExpr b) | bg <- bgs, b <- bg]
  in (bindFvs `Set.union` freeVars body) `Set.difference` bound
freeVars (ECase s brs)    = Set.unions (freeVars s : map branchFreeVars brs)
freeVars (ERetain e)      = freeVars e
freeVars (ERelease e)     = freeVars e
freeVars (EDrop e)        = freeVars e
freeVars (EReuse a b)     = freeVars a `Set.union` freeVars b
freeVars (EDelay e)       = freeVars e
freeVars (EForce e)       = freeVars e
freeVars (ETypeApp e _)   = freeVars e
freeVars (ETypeLam _ e)   = freeVars e
freeVars (EPerform _ args) = Set.unions (map freeVars args)
freeVars (EHandle _ h b)  = freeVars h `Set.union` freeVars b
freeVars (EFunRef _)      = Set.empty

branchFreeVars :: Branch -> Set Name
branchFreeVars br =
  let patBound = Set.fromList (map fst (patternVars (branchPattern br)))
      guardFvs = maybe Set.empty freeVars (branchGuard br)
  in (freeVars (branchBody br) `Set.union` guardFvs) `Set.difference` patBound

-- | Extract variable bindings from a pattern
patternVars :: Pattern -> [(Name, Type)]
patternVars (PatVar n t)    = [(n, t)]
patternVars (PatCon _ pats) = concatMap patternVars pats
patternVars (PatWild _)     = []
patternVars (PatLit _)      = []

-- | Analyze usage of variables in an expression.
-- Returns a map from variable name to usage count.
-- For App/Let/Handle/Reuse: sum (each use is a separate reference).
-- For Case: scrutinee count + max over branches (only one branch executes).
analyzeUsage :: Expr -> Map Name Int
analyzeUsage expr = go expr
  where
    go (EVar n)         = Map.singleton n 1
    go (ELit _)         = Map.empty
    go (ECon _)         = Map.empty
    go (EApp f args)    = Map.unionsWith (+) (go f : map go args)
    go (ELam _ body)    = go body
    go (ELet bgs body)  = Map.unionsWith (+) (go body : [go (bindExpr b) | bg <- bgs, b <- bg])
    go (ECase s brs)    = Map.unionWith (+) (go s)
                            (Map.unionsWith max [go (branchBody br) | br <- brs])
    go (ERetain e)      = go e
    go (ERelease e)     = go e
    go (EDrop e)        = go e
    go (EReuse a b)     = Map.unionWith (+) (go a) (go b)
    go (EDelay e)       = go e
    go (EForce e)       = go e
    go (ETypeApp e _)   = go e
    go (ETypeLam _ e)   = go e
    go (EPerform _ args) = Map.unionsWith (+) (map go args)
    go (EHandle _ h b)  = Map.unionWith (+) (go h) (go b)
    go (EFunRef _)      = Map.empty

unitType :: Type
unitType = TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue)
