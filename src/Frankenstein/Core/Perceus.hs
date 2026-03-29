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
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

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
        unusedParams = [ n | (n, _) <- params
                       , not (Set.member n bodyFree)
                       , Map.findWithDefault Many n scope' /= Linear ]
        body' = perceusExpr scope' body
        -- Drop unused affine/many params
        droppedBody = foldr (\n e -> ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar n)) DefVal]] e)
                            body' unusedParams
        -- Insert retains for Many-multiplicity params used more than once
        retainedBody = foldr
          (\(n, _) e ->
            let m = Map.findWithDefault Many n scope'
                count = Map.findWithDefault 0 n usage
            in if m == Many && count > 1
               then wrapRetains n count e
               else e)
          droppedBody params
    in ELam params retainedBody

  -- Let: insert drops for unused bindings, retains for multi-use bindings
  ELet bgs body ->
    let -- Collect all bound names with their multiplicities
        boundNames = [ (bindName b, bindMultiplicity b)
                     | bg <- bgs, b <- bg ]
        scope' = foldl (\s (n, m) -> Map.insert n m s) scope boundNames
        bodyFree = freeVars body
        usage = analyzeUsage body
        -- Transform binding expressions
        bgs' = map (map (perceusBindGroup scope')) bgs
        body' = perceusExpr scope' body
        -- Find bindings unused in body that need dropping
        toDrop = [ n | (n, m) <- boundNames
                 , not (Set.member n bodyFree)
                 , m /= Linear ]  -- linear values must be used exactly once (error if not)
        droppedBody = foldr (\n e -> ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar n)) DefVal]] e)
                            body' toDrop
        -- Insert retains for Many-multiplicity vars used more than once
        retainedBody = foldr
          (\(n, m) e ->
            let count = Map.findWithDefault 0 n usage
            in if m == Many && count > 1
               then wrapRetains n count e
               else e)
          droppedBody boundNames
    in ELet bgs' retainedBody

  -- Case: each branch may use the scrutinee differently
  ECase scrut branches ->
    let scrut' = perceusExpr scope scrut
        branches' = map (perceusBranch scope) branches
    in ECase scrut' branches'

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

perceusBindGroup :: Map Name Multiplicity -> Bind -> Bind
perceusBindGroup scope b = b { bindExpr = perceusExpr scope (bindExpr b) }

perceusBranch :: Map Name Multiplicity -> Branch -> Branch
perceusBranch scope br =
  let -- Extend scope with pattern-bound variables
      patVars = patternVars (branchPattern br)
      scope' = foldl (\s (n, _) -> Map.insert n Many s) scope patVars
      body = branchBody br
      bodyFree = freeVars body
      -- Drop unused non-linear pattern variables (K spec: unusedNonLinear)
      unusedPats = [ n | (n, _) <- patVars
                   , not (Set.member n bodyFree)
                   , Map.findWithDefault Many n scope' /= Linear ]
      body' = perceusExpr scope' body
      droppedBody = foldr (\n e -> ELet [[Bind (Name "_drop" 0) unitType (EDrop (EVar n)) DefVal]] e)
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
analyzeUsage = go
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

unitType :: Type
unitType = TCon (TypeCon (QName "std" (Name "unit" 0)) KindValue)
