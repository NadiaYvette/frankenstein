{-# LANGUAGE OverloadedStrings #-}
-- | Promote recursive let-bound lambdas: rewrite captures to explicit
-- leading parameters before Perceus.
--
-- The MLIR emitter lifts every let-bound lambda to a top-level
-- @func.func@.  For lambdas with captured free variables, the call sites
-- get the captures prepended as args.  Perceus, however, runs on the
-- pre-lift IR and analyses uses *without* knowing about that lift —
-- so a per-branch outer-variable balance can insert a drop on a capture
-- in a branch that, post-lift, will explicitly pass that capture to a
-- recursive call.  The drop then runs *before* the pass, recycling the
-- cell while the call site still holds a reference.
--
-- Concrete failure: GHC bridge @show_derived@ under @KK_RECYCLE=1@
-- prints @Node @ then freelist-sentinel garbage because Perceus drops
-- @z'@ (the captured ShowS continuation) in @go1@'s Cons branch right
-- before passing @z'@ to the recursive @go1(z', ys)@.  See
-- @memory/ghc_bridge_refcount.md@ for the full diagnosis.
--
-- This pass runs BEFORE @insertPerceus@: it converts each recursive
-- let-bound lambda's captures into explicit leading parameters and
-- rewrites every reference to the lambda accordingly.  After this,
-- Perceus sees explicit args and balances correctly — no spurious drop.
module Frankenstein.Core.PromoteRecursiveLambdas
  ( promoteRecursiveLambdas
  , promoteRecursiveLambdasProgram
  ) where

import Frankenstein.Core.Types

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Apply promotion to every top-level def in a program.
promoteRecursiveLambdasProgram :: Program -> Program
promoteRecursiveLambdasProgram prog =
  prog { progDefs = map promoteDef (progDefs prog) }

promoteDef :: Def -> Def
promoteDef def =
  let initialScope = case defExpr def of
        ELam params _ -> Set.fromList (map fst params)
        _ -> Set.empty
  in def { defExpr = promoteRecursiveLambdas initialScope Map.empty (defExpr def) }

-- | @promoteRecursiveLambdas scope capMap expr@
--
--   * @scope@ — variables currently bound in surrounding scope (lambda
--     params, pattern vars, let-bound names).  Excludes top-level fn
--     refs since those should never be lifted into capture lists.
--   * @capMap@ — for each promoted recursive lambda name in scope,
--     the ordered list of captures that have been prepended to its
--     parameter list (so every reference to that lambda must prepend
--     the same arguments).
promoteRecursiveLambdas :: Set Name -> Map Name [Name] -> Expr -> Expr
promoteRecursiveLambdas scope capMap expr = case expr of
  EVar _    -> expr
  ELit _    -> expr
  ECon _    -> expr
  EFunRef _ -> expr

  EApp f args ->
    let f'    = promoteRecursiveLambdas scope capMap f
        args' = map (promoteRecursiveLambdas scope capMap) args
        -- The callee may be EVar n directly OR ETypeApp (EVar n) ts.
        -- Both refer to the same name n; treat them uniformly.
        mTarget = case f' of
          EVar n                -> Just n
          ETypeApp (EVar n) _   -> Just n
          _                     -> Nothing
    in case mTarget of
      Just n | Just caps <- Map.lookup n capMap ->
        -- Prepend the captures as explicit args.  Subsequent passes
        -- (Perceus, then the emitter) see a fully-saturated call.
        EApp f' ([EVar c | c <- caps] ++ args')
      _ -> EApp f' args'

  ELam params body ->
    let scope' = scope `Set.union` Set.fromList (map fst params)
        body' = promoteRecursiveLambdas scope' capMap body
    in ELam params body'

  ELet bgs body -> handleLet scope capMap bgs body

  ECase scrut brs ->
    let scrut' = promoteRecursiveLambdas scope capMap scrut
        brs'   = map (promoteBranch scope capMap) brs
    in ECase scrut' brs'

  ETypeApp e ts  -> ETypeApp (promoteRecursiveLambdas scope capMap e) ts
  ETypeLam tvs e -> ETypeLam tvs (promoteRecursiveLambdas scope capMap e)
  EPerform qn as -> EPerform qn (map (promoteRecursiveLambdas scope capMap) as)
  EHandle eff h b ->
    EHandle eff (promoteRecursiveLambdas scope capMap h)
                (promoteRecursiveLambdas scope capMap b)
  ERetain e  -> ERetain  (promoteRecursiveLambdas scope capMap e)
  ERelease e -> ERelease (promoteRecursiveLambdas scope capMap e)
  EDrop e    -> EDrop    (promoteRecursiveLambdas scope capMap e)
  EReuse a b -> EReuse   (promoteRecursiveLambdas scope capMap a)
                         (promoteRecursiveLambdas scope capMap b)
  EDelay e   -> EDelay   (promoteRecursiveLambdas scope capMap e)
  EForce e   -> EForce   (promoteRecursiveLambdas scope capMap e)

promoteBranch :: Set Name -> Map Name [Name] -> Branch -> Branch
promoteBranch scope capMap br =
  let patNames = Set.fromList (map fst (patternVars (branchPattern br)))
      scope'   = scope `Set.union` patNames
      body'    = promoteRecursiveLambdas scope' capMap (branchBody br)
      guard'   = fmap (promoteRecursiveLambdas scope' capMap) (branchGuard br)
  in br { branchBody = body', branchGuard = guard' }

handleLet :: Set Name -> Map Name [Name] -> [BindGroup] -> Expr -> Expr
handleLet scope capMap [] body =
  promoteRecursiveLambdas scope capMap body
handleLet scope capMap (bg : rest) body =
  let groupNames = Set.fromList [bindName b | b <- bg]
      scope'     = scope `Set.union` groupNames
      -- The MLIR emitter's @isLambda@ predicate (which decides whether
      -- to promote a binding to a top-level func.func) unwraps through
      -- @ETypeLam@ and @EDelay@.  Mirror that here so polymorphic and
      -- delayed recursive lambdas also get capture-promoted.  If we
      -- miss them, the emitter still promotes-with-captures but Perceus
      -- sees implicit captures and inserts the same spurious drop the
      -- pass is meant to eliminate.
      isRecLam b = isLambdaExpr (bindExpr b) && isRecursive bg b
      (recLams, otherBinds) = partitionBy isRecLam bg
  in if null recLams
     then
       -- No recursive lambdas in this group; recurse normally.
       let bg'   = map (\b -> b { bindExpr = promoteRecursiveLambdas scope' capMap (bindExpr b) }) bg
           rest' = handleLet scope' capMap rest body
       in ELet [bg'] rest'
     else
       let -- Compute captures for the recursive group (fixed point for mutual recursion).
           allCaps = computeCapturesFixpoint scope groupNames capMap recLams
           newCapMap = Map.union allCaps capMap

           rewriteRecLam b =
             let caps = Map.findWithDefault [] (bindName b) newCapMap
                 -- @bindExpr b@ may be @ELam …@ or wrapped in @ETypeLam@/
                 -- @EDelay@ (the emitter's @unwrapLambda@ peels those).
                 -- Rewrite only the innermost ELam; preserve the outer
                 -- wrappers so the type abstraction / delay semantics
                 -- survive.
                 capParams = [(c, anyType) | c <- caps]
                 innerScope = scope
                              `Set.union` Set.fromList caps
                              `Set.union` groupNames
                 rewriteInner expr = case expr of
                   ELam ps lb ->
                     let innerScope' = innerScope `Set.union` Set.fromList (map fst ps)
                         newBody = promoteRecursiveLambdas innerScope' newCapMap lb
                     in ELam (capParams ++ ps) newBody
                   ETypeLam tvs inner -> ETypeLam tvs (rewriteInner inner)
                   EDelay inner       -> EDelay (rewriteInner inner)
                   _                  -> error "PromoteRecursiveLambdas: rewriteRecLam expected lambda after unwrap"
             in b { bindExpr = rewriteInner (bindExpr b) }

           recLams'    = map rewriteRecLam recLams
           otherBinds' = map (\b -> b { bindExpr = promoteRecursiveLambdas scope' newCapMap (bindExpr b) })
                             otherBinds
           bg'         = recLams' ++ otherBinds'
           rest'       = handleLet scope' newCapMap rest body
       in ELet [bg'] rest'

-- | Does an expression denote a lambda once @ETypeLam@/@EDelay@
-- wrappers are peeled?  Mirrors the MLIR emitter's @isLambda@.
isLambdaExpr :: Expr -> Bool
isLambdaExpr (ELam _ _)     = True
isLambdaExpr (ETypeLam _ e) = isLambdaExpr e
isLambdaExpr (EDelay e)     = isLambdaExpr e
isLambdaExpr _              = False

-- | Peel @ETypeLam@/@EDelay@ wrappers to reach the underlying @ELam@.
-- Mirrors the MLIR emitter's @unwrapLambda@.
unwrapLambdaExpr :: Expr -> Expr
unwrapLambdaExpr (ETypeLam _ e) = unwrapLambdaExpr e
unwrapLambdaExpr (EDelay e)     = unwrapLambdaExpr e
unwrapLambdaExpr e              = e

-- | A binding is recursive in its group if its body references any
-- name bound by the group.
isRecursive :: BindGroup -> Bind -> Bool
isRecursive bg b =
  let names = Set.fromList [bindName x | x <- bg]
  in not (Set.null (Set.intersection names (freeVars (bindExpr b))))

-- | Fixed-point computation of captures for a mutually-recursive group.
-- Each lambda's captures = direct free vars (excluding params, group
-- names, top-level fn refs) ∪ transitive captures of any callee in
-- the group or in an enclosing promoted lambda (read from @priorCaps@).
computeCapturesFixpoint :: Set Name -> Set Name -> Map Name [Name] -> [Bind] -> Map Name [Name]
computeCapturesFixpoint scope groupNames priorCaps recLams =
  let directCapsOf b =
        -- Peel @ETypeLam@/@EDelay@ to reach the inner @ELam@ — same
        -- treatment as @rewriteRecLam@.  Free-var analysis on the
        -- @ETypeLam@-wrapped form already skips type variables; what
        -- we want is the value-level params of the inner lambda.
        let (params, body) = case unwrapLambdaExpr (bindExpr b) of
              ELam ps bd -> (ps, bd)
              _          -> error "PromoteRecursiveLambdas: directCapsOf non-lambda"
            paramNames = Set.fromList (map fst params)
            fv         = freeVars body
            candidates = fv `Set.difference` paramNames
                            `Set.difference` groupNames
        in Set.filter (`Set.member` scope) candidates

      bindByName n = case [b | b <- recLams, bindName b == n] of
                       (b : _) -> b
                       []      -> error "PromoteRecursiveLambdas: bindByName lookup miss"

      initial :: Map Name (Set Name)
      initial = Map.fromList [(bindName b, directCapsOf b) | b <- recLams]

      step caps =
        Map.mapWithKey
          (\name direct ->
             let b        = bindByName name
                 callees  = collectCallTargets (bindExpr b)
                 transitive = Set.unions
                   [ case Map.lookup t caps of
                       Just s  -> s
                       Nothing -> Set.fromList (Map.findWithDefault [] t priorCaps)
                   | t <- Set.toList callees
                   , Set.member t groupNames || Map.member t priorCaps
                   ]
                 augmented = direct `Set.union` transitive
             in Set.filter (`Set.member` scope) augmented)
          caps

      fixedPoint caps =
        let caps' = step caps
        in if caps' == caps then caps else fixedPoint caps'

      finalSet = fixedPoint initial
  in Map.map Set.toAscList finalSet

-- | Find every name that appears as the head of an EApp anywhere in
-- @e@ (recursing through subexpressions and nested lambdas).
collectCallTargets :: Expr -> Set Name
collectCallTargets = go
  where
    go (EApp (EVar n) args) = Set.insert n (Set.unions (map go args))
    go (EApp f args)        = Set.union (go f) (Set.unions (map go args))
    go (ELet bgs body)      =
      Set.union (Set.unions [go (bindExpr b) | bg <- bgs, b <- bg]) (go body)
    go (ECase s brs)        =
      Set.union (go s) (Set.unions (map (go . branchBody) brs))
    go (ELam _ b)           = go b
    go (ERetain e)          = go e
    go (ERelease e)         = go e
    go (EDrop e)            = go e
    go (EReuse a b)         = Set.union (go a) (go b)
    go (EDelay e)           = go e
    go (EForce e)           = go e
    go (ETypeApp e _)       = go e
    go (ETypeLam _ e)       = go e
    go (EPerform _ as)      = Set.unions (map go as)
    go (EHandle _ h b)      = Set.union (go h) (go b)
    go _                    = Set.empty

-- | Free variables of an expression.  Same definition as
-- @Frankenstein.Core.Perceus.freeVars@ but not exported from that
-- module; duplicated here to keep the pass standalone.
freeVars :: Expr -> Set Name
freeVars (EVar n)         = Set.singleton n
freeVars (ELit _)         = Set.empty
freeVars (ECon _)         = Set.empty
freeVars (EFunRef _)      = Set.empty
freeVars (EApp f args)    = Set.unions (freeVars f : map freeVars args)
freeVars (ELam ps body)   = freeVars body `Set.difference` Set.fromList (map fst ps)
freeVars (ELet bgs body)  =
  let bound   = Set.fromList [bindName b | bg <- bgs, b <- bg]
      bindFvs = Set.unions [freeVars (bindExpr b) | bg <- bgs, b <- bg]
  in (bindFvs `Set.union` freeVars body) `Set.difference` bound
freeVars (ECase s brs)    =
  Set.union (freeVars s) (Set.unions (map branchFreeVars brs))
freeVars (ETypeApp e _)   = freeVars e
freeVars (ETypeLam _ e)   = freeVars e
freeVars (EPerform _ as)  = Set.unions (map freeVars as)
freeVars (EHandle _ h b)  = Set.union (freeVars h) (freeVars b)
freeVars (ERetain e)      = freeVars e
freeVars (ERelease e)     = freeVars e
freeVars (EDrop e)        = freeVars e
freeVars (EReuse a b)     = Set.union (freeVars a) (freeVars b)
freeVars (EDelay e)       = freeVars e
freeVars (EForce e)       = freeVars e

branchFreeVars :: Branch -> Set Name
branchFreeVars br =
  let patBound = Set.fromList (map fst (patternVars (branchPattern br)))
      guardFvs = maybe Set.empty freeVars (branchGuard br)
  in (freeVars (branchBody br) `Set.union` guardFvs) `Set.difference` patBound

patternVars :: Pattern -> [(Name, Type)]
patternVars (PatVar n t)    = [(n, t)]
patternVars (PatCon _ pats) = concatMap patternVars pats
patternVars (PatWild _)     = []
patternVars (PatLit _)      = []

partitionBy :: (a -> Bool) -> [a] -> ([a], [a])
partitionBy p = foldr select ([], [])
  where
    select x (ts, fs) | p x       = (x : ts, fs)
                     | otherwise = (ts, x : fs)

-- | Conservative placeholder type used for capture-promoted params.
-- The MLIR emitter flows every i64 through the closure dispatch
-- regardless of the carried 'Type', so the precise type isn't
-- material to code generation.
anyType :: Type
anyType = TCon (TypeCon (QName "" (Name "any" 0)) KindValue)
