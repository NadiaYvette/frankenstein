{-# LANGUAGE OverloadedStrings #-}
-- | Perceus-faithful consuming-use count.
--
-- Phase 12c (see ROADMAP).  This module is the analysis side of the
-- planned replacement for @emitLambdaLift@'s @countUses@ heuristic in
-- @Frankenstein.MlirEmit.Emitter@.
--
-- The current heuristic walks the IR counting EVar mentions of each
-- capture name.  That over-counts in every case where syntactic
-- occurrence diverges from a runtime consume — uses inside nested
-- @ELam@/@EDelay@ bodies are per-inner-invocation, distinct @Name@s
-- with shared text get conflated, pattern + let shadowing isn't
-- honored, and the @max-of-branches@ rule for @ECase@ is missing.
-- See the Phase 12c roadmap entry for the bisect record.
--
-- This module provides a Perceus-faithful count with no consumers
-- yet — it's a standalone analysis we can run via a debug flag,
-- compare against the heuristic on real programs, and then swap into
-- the emitter once we've verified what the drop-insertion sites
-- actually need to match.
module Frankenstein.Core.ConsumingUses
  ( consumingUseCount
  , consumingUseCounts
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | @consumingUseCount tgt e@ — number of times @tgt@ would be
-- consumed per evaluation of @e@ under Perceus semantics.
--
-- Rules:
--
--   * @EVar n@: 1 consume of @n@.  All other consuming sites bottom
--     out here.
--   * @EApp f args@: each of @f@ and @args@ is consumed
--     independently.  Their counts sum.
--   * @ELam ps body@: closure construction transfers exactly one
--     reference of every free var of @body@ into the closure's
--     field array.  Charge 1 if @tgt@ is free in @body@ (and not
--     shadowed by one of @ps@), else 0.  Uses inside @body@ are
--     per-invocation of the lifted lambda, NOT per-evaluation of
--     the @ELam@ expression itself.
--   * @EDelay body@: thunk creation captures free vars the same
--     way @ELam@ does.  Single capture event.
--   * @ELet bgs body@: each binding's RHS is evaluated once and
--     consumed by the binder.  Sum binding counts plus body count.
--     Shadowing within the @ELet@ stops the count for inner
--     contexts (see @shadowedBy@ for the precise rule).
--   * @ECase scrut brs@: the scrutinee is consumed once.  Exactly
--     one branch runs, so take the max of branch counts (not the
--     sum).  Branch patterns can shadow @tgt@.
--   * @EDrop e@: drop consumes its inner expression once.  Count
--     uses inside @e@.
--   * @ERetain e@, @ERelease e@: pure refcount operations; the
--     inner @EVar@ is NOT consumed.  Return 0.
--   * @EReuse a b@: both subexpressions are consumed.
--   * @EForce e@: forcing a thunk consumes the thunk reference.
--     Count uses inside @e@.
--   * @ETypeApp e _@, @ETypeLam _ e@: transparent for value-level
--     accounting — recurse.
--   * @EPerform _ args@: each arg consumed.
--   * @EHandle _ h b@: handler @h@ is installed (like a closure
--     capture — count once if @tgt@ is free in @h@).  Body @b@
--     runs synchronously — recurse.
--   * @ECon _@, @ELit _@, @EFunRef _@: no value-level consumes.
--
-- @Name@ equality includes both the text and the unique, so distinct
-- GHC synthetic variables with shared text don't get conflated.
consumingUseCount :: Name -> Expr -> Int
consumingUseCount tgt = goExpr
  where
    -- "Is @tgt@ free in @e@ (respecting binders)?"  Used for the
    -- closure-capture events at ELam / EDelay / EHandle.
    isFree :: Expr -> Bool
    isFree expr = case expr of
      EVar n          -> n == tgt
      ELit _          -> False
      ECon _          -> False
      EFunRef _       -> False
      EApp f as       -> isFree f || any isFree as
      ELam ps b
        | tgtBoundIn (map fst ps) -> False
        | otherwise               -> isFree b
      ELet bgs b
        | letShadows bgs          -> False
        | otherwise               -> isFree b
                                  || any (isFree . bindExpr) (concat bgs)
      ECase s brs     -> isFree s || any branchFree brs
      ETypeApp e _    -> isFree e
      ETypeLam _ e    -> isFree e
      EPerform _ as   -> any isFree as
      EHandle _ h b   -> isFree h || isFree b
      ERetain e       -> isFree e
      ERelease e      -> isFree e
      EDrop e         -> isFree e
      EReuse a b      -> isFree a || isFree b
      EDelay e        -> isFree e
      EForce e        -> isFree e

    branchFree :: Branch -> Bool
    branchFree br
      | tgtBoundIn (patternNames (branchPattern br)) = False
      | otherwise =
          isFree (branchBody br)
            || maybe False isFree (branchGuard br)

    -- Closure / thunk / handler capture event: charge 1 if free, else 0.
    captureCount :: Expr -> Int
    captureCount e = if isFree e then 1 else 0

    goExpr :: Expr -> Int
    goExpr expr = case expr of
      EVar n          -> if n == tgt then 1 else 0
      ELit _          -> 0
      ECon _          -> 0
      EFunRef _       -> 0
      EApp f as       -> goExpr f + sum (map goExpr as)
      ELam ps b
        | tgtBoundIn (map fst ps) -> 0   -- inner @tgt@ shadows
        | otherwise               -> captureCount b
      ELet bgs b
        | letShadows bgs          -> 0
        | otherwise -> goExpr b
                    + sum (map (goExpr . bindExpr) (concat bgs))
      ECase s brs     -> goExpr s + maxBranch brs
      ETypeApp e _    -> goExpr e
      ETypeLam _ e    -> goExpr e
      EPerform _ as   -> sum (map goExpr as)
      EHandle _ h b   -> captureCount h + goExpr b
      ERetain _       -> 0
      ERelease _      -> 0
      EDrop e         -> goExpr e
      EReuse a b      -> goExpr a + goExpr b
      EDelay e        -> captureCount e
      EForce e        -> goExpr e

    maxBranch :: [Branch] -> Int
    maxBranch [] = 0
    maxBranch brs = maximum (map goBranch brs)

    goBranch :: Branch -> Int
    goBranch br
      | tgtBoundIn (patternNames (branchPattern br)) = 0
      | otherwise =
          goExpr (branchBody br) + maybe 0 goExpr (branchGuard br)

    tgtBoundIn :: [Name] -> Bool
    tgtBoundIn = any (== tgt)

    -- An @ELet@ shadows @tgt@ if any of its binders has the same
    -- 'Name'.  Conservative: this disables counting for the entire
    -- @ELet@ (binding RHSs + body) even if the shadowing binder is
    -- in a later group than the RHS we're scanning.  Refining to
    -- "shadowed only after the binding group introducing the same
    -- name" is straightforward but adds complexity — defer until
    -- a workload demands it.
    letShadows :: [BindGroup] -> Bool
    letShadows bgs =
      any ((== tgt) . bindName) (concat bgs)

    patternNames :: Pattern -> [Name]
    patternNames p = case p of
      PatVar n _    -> [n]
      PatCon _ subs -> concatMap patternNames subs
      PatWild _     -> []
      PatLit _      -> []

-- | Batch convenience.  Given a list of capture names and an
-- expression, return a map of consuming-use counts.  The emitter
-- currently computes the same thing one capture at a time;
-- materializing the map lets a debug-print mode tabulate counts
-- without re-walking the IR per capture.
consumingUseCounts :: [Name] -> Expr -> Map Name Int
consumingUseCounts caps e =
  Map.fromList [ (n, consumingUseCount n e) | n <- caps ]
