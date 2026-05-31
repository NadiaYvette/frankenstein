{-# LANGUAGE OverloadedStrings #-}
-- | Liveness analysis for Frankenstein Core IR.
--
-- For each free variable of an expression, computes the position in
-- depth-first evaluation order at which the variable is /last used/.
-- "Last use" is the point after which the variable is dead — no
-- further read happens along any execution path that flows through
-- this position.
--
-- Used by drop-insertion passes that need to release ownership of a
-- value at the latest legal moment (so the value remains available
-- for any earlier use) rather than at the syntactic end of its
-- binding scope.
--
-- == Position semantics
--
-- Positions are sequence numbers assigned in evaluation order:
--
--   * 'EApp' evaluates @f@ first, then arguments left-to-right.
--   * 'ELet' evaluates binding groups in source order, then the
--     body.
--   * 'ECase' evaluates the scrutinee first, then exactly one
--     branch.  Branches /share/ position numbers — for a "last use
--     across all paths" map we take the maximum, which is sound for
--     "after this position, the variable is definitely dead."
--   * 'ELam' and 'EDelay' /create/ closures / thunks: the captured
--     free variables are /transferred/ into the closure cell at the
--     'ELam'/'EDelay' position itself.  We don't recurse into the
--     suspended body for outer-scope liveness — the body's uses
--     happen later, on invocation/forcing, not in the enclosing
--     scope.  For inner-scope liveness inside the body, run
--     'lastUseMap' on the body separately.
--   * 'EHandle' evaluates the handler first, then the body.
--   * 'EPerform' evaluates arguments left-to-right.
--   * 'ERetain' / 'ERelease' / 'EDrop' / 'EForce' / 'ETypeApp':
--     pass through.
--
-- == What "last use" means for shadowing
--
-- A let-binding @let n = ...@ kills any outer @n@: uses of @n@
-- inside the let body refer to the inner @n@, not the outer.  This
-- module honors that — only the FREE variables of the expression
-- appear in the output map, scoped to the expression the function
-- is called on.
--
-- == Closure-create caveat
--
-- When an 'ELam' captures a free variable, the capture is
-- represented as a single use at the 'ELam' position (in the outer
-- scope's evaluation order).  That use is /the/ last outer use of
-- the variable, regardless of how many times the lambda body
-- references it — the lambda body runs in its own scope when the
-- closure is invoked, not here.
--
-- This is the correct convention for outer-scope drop insertion: a
-- closure stored in a constructor cell is dropped when the cell is
-- dropped (via cascade), not at the closure-creation point.
module Frankenstein.Core.Liveness
  ( -- * Last-use map
    LastUseMap
  , lastUseMap
    -- * Per-occurrence position
  , Position
  , useEvents
  , UseEvent(..)
    -- * Branch-aware last use
  , lastUseMapPerBranch
  , BranchIndex
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | An evaluation-order position.  Larger = later.
type Position = Int

-- | One occurrence of a free variable, tagged with the evaluation-
-- order position at which it's referenced.
data UseEvent = UseEvent
  { useName     :: !Name
  , usePosition :: !Position
  } deriving (Eq, Show)

-- | For each free variable of an expression, the position of its
-- last use in evaluation order.  See module header for position
-- semantics.
type LastUseMap = Map Name Position

-- | A branch index for 'ECase'.  0 = first branch in source order.
type BranchIndex = Int

-- | Compute the last-use position for each free variable of @e@.
lastUseMap :: Expr -> LastUseMap
lastUseMap e = foldr update Map.empty (useEvents e)
  where
    update (UseEvent n p) m = Map.insertWith max n p m

-- | Per-branch last-use info: for each 'ECase' in the expression
-- (identified by depth-first index), and for each free variable of
-- that branch's body, the position of the last use within that
-- branch.
--
-- Useful for inserting branch-local drops: if a variable is last-
-- used in only one branch of a case, the other branches can drop it
-- at branch entry.
--
-- The outer 'Position' is the case's scrutinee position; the inner
-- 'LastUseMap' uses positions local to the branch (independent
-- counter starting after the scrutinee).
lastUseMapPerBranch :: Expr -> Map (Position, BranchIndex) LastUseMap
lastUseMapPerBranch = snd . go Map.empty 0
  where
    -- @go acc p e@ walks @e@ at position @p@, accumulating branch
    -- maps into @acc@.  Returns @(next position, accumulator)@.
    go :: Map (Position, BranchIndex) LastUseMap
       -> Position
       -> Expr
       -> (Position, Map (Position, BranchIndex) LastUseMap)
    go acc p expr = case expr of
      ECase scrut brs ->
        let (p1, acc1) = go acc p scrut
            scrutPos = p1 - 1  -- position just consumed by scrut walk
            (_finalP, acc2) = foldr (stepBranch scrutPos) (p1, acc1)
                                    (zip [0..] brs)
        in (p1, acc2)  -- branches don't advance outer position
      EApp f args ->
        let (p1, acc1) = go acc p f
        in foldl (\(pp, aa) a -> go aa pp a) (p1, acc1) args
      ELet bgs body ->
        let (p1, acc1) = foldl walkBindGroup (p, acc) bgs
        in go acc1 p1 body
      EHandle _ h b ->
        let (p1, acc1) = go acc p h
        in go acc1 p1 b
      EPerform _ args ->
        foldl (\(pp, aa) a -> go aa pp a) (p, acc) args
      EReuse a b ->
        let (p1, acc1) = go acc p a
        in go acc1 p1 b
      ERetain e' -> go acc p e'
      ERelease e' -> go acc p e'
      EDrop e'   -> go acc p e'
      EForce e'  -> go acc p e'
      ETypeApp e' _ -> go acc p e'
      ETypeLam _ e' -> go acc p e'
      ELam _ _   -> (p + 1, acc)   -- closure create: single position, no recursion
      EDelay _   -> (p + 1, acc)   -- thunk create: single position, no recursion
      EVar _     -> (p + 1, acc)
      ELit _     -> (p, acc)
      ECon _     -> (p, acc)
      EFunRef _  -> (p, acc)

    stepBranch scrutPos (bi, br) (pInOuter, accIn) =
      -- For each branch, run a fresh local position counter starting
      -- at 0; record the branch's last-use map keyed by (scrutPos, bi).
      let branchEvents = useEvents (branchBody br)
          branchMap = foldr (\(UseEvent n pp) m -> Map.insertWith max n pp m)
                            Map.empty branchEvents
          accOut = Map.insert (scrutPos, bi) branchMap accIn
          -- Also recurse into the branch body to pick up nested cases.
          (_, accOut') = go accOut 0 (branchBody br)
      in (pInOuter, accOut')

    walkBindGroup (pp, acc) bg =
      foldl (\(p2, a2) b -> go a2 p2 (bindExpr b)) (pp, acc) bg

-- | Emit a stream of @UseEvent@s for each free-variable occurrence
-- in @e@, in depth-first evaluation order.  Positions are 0-indexed
-- and unique per use.
useEvents :: Expr -> [UseEvent]
useEvents = fst . go 0
  where
    -- Returns (events, next-position-after-this-expression).
    go :: Position -> Expr -> ([UseEvent], Position)
    go p expr = case expr of
      EVar n -> ([UseEvent n p], p + 1)

      ELit _    -> ([], p)
      ECon _    -> ([], p)
      EFunRef _ -> ([], p)

      EApp f args ->
        let (fs, p1) = go p f
            (argEs, p2) = goList p1 args
        in (fs ++ argEs, p2)

      ELet bgs body ->
        let bindExprs = [bindExpr b | bg <- bgs, b <- bg]
            (binderEs, p1) = goList p bindExprs
            (bodyEs, p2) = go p1 body
            boundNames = Set.fromList
              [ bindName b | bg <- bgs, b <- bg ]
            -- Frankenstein Core has Haskell-recursive let semantics:
            -- all bindings of the let are in scope of all binder
            -- expressions and the body.  Use-events for those bound
            -- names anywhere within this let are INNER uses; filter
            -- them out of the outer-scope event list.
            isOuter ev = not (Set.member (useName ev) boundNames)
            outerEs = filter isOuter (binderEs ++ bodyEs)
        in (outerEs, p2)

      ECase scrut brs ->
        let (scrutEs, p1) = go p scrut
            -- All branches share starting position p1.  Each branch
            -- advances independently; we union their events.  For
            -- "last use across all paths" semantics, the outer
            -- position after the case is max over branch ends.
            branchEvents = map (goBranch p1) brs
            branchPositions = [ pBr | (_, pBr) <- branchEvents ]
            maxBranchP = if null branchPositions then p1 else maximum branchPositions
            allBranchEs = concatMap fst branchEvents
        in (scrutEs ++ allBranchEs, maxBranchP)

      ELam params body ->
        -- Outer-scope: closure creation USES each free variable of
        -- the lambda body (minus params) once, at this position.
        let paramSet = Set.fromList (map fst params)
            outerFrees = freeVarsExpr body `Set.difference` paramSet
            evs = [ UseEvent n p | n <- Set.toList outerFrees ]
        in (evs, p + 1)

      EDelay body ->
        -- Same as ELam — captures transferred at thunk creation.
        let frees = freeVarsExpr body
            evs = [ UseEvent n p | n <- Set.toList frees ]
        in (evs, p + 1)

      EForce e' -> go p e'

      ETypeApp e' _ -> go p e'
      ETypeLam _ e' -> go p e'

      EPerform _ args -> goList p args

      EHandle _ handler body ->
        let (hs, p1) = go p handler
            (bs, p2) = go p1 body
        in (hs ++ bs, p2)

      ERetain  e' -> go p e'
      ERelease e' -> go p e'
      EDrop    e' -> go p e'

      EReuse a b ->
        let (as, p1) = go p a
            (bs, p2) = go p1 b
        in (as ++ bs, p2)

    goList :: Position -> [Expr] -> ([UseEvent], Position)
    goList p0 = foldl
      (\(acc, p) e -> let (es, p') = go p e in (acc ++ es, p'))
      ([], p0)

    goBranch :: Position -> Branch -> ([UseEvent], Position)
    goBranch p br =
      let patBound = Set.fromList (map fst (patternVars (branchPattern br)))
          (guardEs, p1) = case branchGuard br of
            Just g  -> go p g
            Nothing -> ([], p)
          (bodyEs, p2) = go p1 (branchBody br)
          -- Pattern bindings shadow outer variables; filter
          -- references to those names from guard + body events.
          isOuter ev = not (Set.member (useName ev) patBound)
          outerEs = filter isOuter (guardEs ++ bodyEs)
      in (outerEs, p2)

    patternVars :: Pattern -> [(Name, Type)]
    patternVars (PatVar n t)    = [(n, t)]
    patternVars (PatCon _ pats) = concatMap patternVars pats
    patternVars (PatWild _)     = []
    patternVars (PatLit _)      = []

    freeVarsExpr :: Expr -> Set Name
    freeVarsExpr (EVar n)        = Set.singleton n
    freeVarsExpr (ELit _)        = Set.empty
    freeVarsExpr (ECon _)        = Set.empty
    freeVarsExpr (EFunRef _)     = Set.empty
    freeVarsExpr (EApp f args)   = Set.unions (freeVarsExpr f : map freeVarsExpr args)
    freeVarsExpr (ELam ps body)  = freeVarsExpr body `Set.difference` Set.fromList (map fst ps)
    freeVarsExpr (ELet bgs body) =
      let bound   = Set.fromList [bindName b | bg <- bgs, b <- bg]
          bindFvs = Set.unions [freeVarsExpr (bindExpr b) | bg <- bgs, b <- bg]
      in (bindFvs `Set.union` freeVarsExpr body) `Set.difference` bound
    freeVarsExpr (ECase s brs) =
      Set.unions (freeVarsExpr s : map branchFreeVars brs)
    freeVarsExpr (ETypeApp e' _) = freeVarsExpr e'
    freeVarsExpr (ETypeLam _ e') = freeVarsExpr e'
    freeVarsExpr (EPerform _ as) = Set.unions (map freeVarsExpr as)
    freeVarsExpr (EHandle _ h b) = freeVarsExpr h `Set.union` freeVarsExpr b
    freeVarsExpr (ERetain e')    = freeVarsExpr e'
    freeVarsExpr (ERelease e')   = freeVarsExpr e'
    freeVarsExpr (EDrop e')      = freeVarsExpr e'
    freeVarsExpr (EReuse a b)    = freeVarsExpr a `Set.union` freeVarsExpr b
    freeVarsExpr (EDelay e')     = freeVarsExpr e'
    freeVarsExpr (EForce e')     = freeVarsExpr e'

    branchFreeVars :: Branch -> Set Name
    branchFreeVars br =
      let patBound = Set.fromList (map fst (patternVars (branchPattern br)))
          guardFvs = maybe Set.empty freeVarsExpr (branchGuard br)
      in (freeVarsExpr (branchBody br) `Set.union` guardFvs) `Set.difference` patBound
