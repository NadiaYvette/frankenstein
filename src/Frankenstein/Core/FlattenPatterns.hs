-- | Nested pattern flattening.
--
-- Core IR → Core IR pass that rewrites nested constructor patterns into
-- a sequence of single-level matches. This allows downstream stages
-- (the MLIR emitter in particular) to assume every @PatCon@ has only
-- simple sub-patterns: @PatVar@, @PatWild@, or @PatLit@.
--
-- == Rewrite
--
-- @
-- case xs of
--   Cons (Box x) ys -> body
-- @
--
-- becomes
--
-- @
-- case xs of
--   Cons _f0 ys -> case _f0 of
--                    Box x -> body
-- @
--
-- The fresh variable @_f0@ replaces the nested sub-pattern, and the inner
-- @ECase@ re-matches it at a single level. Nested constructors inside
-- that inner pattern are flattened recursively.
--
-- == Scope / limitations
--
-- The rewrite is performed independently per branch: it does not merge
-- sibling branches that share the same outer constructor. That means
-- for multi-outer-branch matches with differing inner patterns the
-- inner @ECase@ ends up with a single branch, which the emitter treats
-- as 'SingleConCase' (unconditional extraction). This is correct when
-- the inner type has a single constructor (@Box@, @Pair@, newtypes …)
-- or when the programmer has asserted a specific inner shape. It is
-- /not/ a complete pattern-matrix compiler — nested patterns on sum
-- types with multiple outer branches are an open TODO.
module Frankenstein.Core.FlattenPatterns
  ( flattenPatterns
  , flattenPatternsExpr
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)
import qualified Data.Text as T

-- | Flatten nested patterns throughout a program.
flattenPatterns :: Program -> Program
flattenPatterns prog =
  prog { progDefs = map flattenDef (progDefs prog) }

flattenDef :: Def -> Def
flattenDef d = d { defExpr = flattenPatternsExpr (defExpr d) }

-- | Flatten nested patterns in a single expression.
flattenPatternsExpr :: Expr -> Expr
flattenPatternsExpr = fst . goExpr 0

-- Monad-lite: thread a fresh counter through the walk.
type Fresh = Int

freshVar :: Fresh -> (Name, Fresh)
freshVar n = (Name (T.pack ("_f" ++ show n)) n, n + 1)

goExpr :: Fresh -> Expr -> (Expr, Fresh)
goExpr n e = case e of
  ECase scrut branches ->
    let (scrut', n1) = goExpr n scrut
        (branches', n2) = goBranches n1 branches
    in (ECase scrut' branches', n2)
  EApp f args ->
    let (f', n1) = goExpr n f
        (args', n2) = goList n1 args
    in (EApp f' args', n2)
  ELam params body ->
    let (body', n1) = goExpr n body
    in (ELam params body', n1)
  ELet groups body ->
    let (groups', n1) = goGroups n groups
        (body', n2) = goExpr n1 body
    in (ELet groups' body', n2)
  ETypeApp inner tys ->
    let (inner', n1) = goExpr n inner
    in (ETypeApp inner' tys, n1)
  ETypeLam tvs inner ->
    let (inner', n1) = goExpr n inner
    in (ETypeLam tvs inner', n1)
  EPerform qn args ->
    let (args', n1) = goList n args
    in (EPerform qn args', n1)
  EHandle row handler body ->
    let (handler', n1) = goExpr n handler
        (body', n2) = goExpr n1 body
    in (EHandle row handler' body', n2)
  ERetain  x -> let (x', n1) = goExpr n x in (ERetain  x', n1)
  ERelease x -> let (x', n1) = goExpr n x in (ERelease x', n1)
  EDrop    x -> let (x', n1) = goExpr n x in (EDrop    x', n1)
  EReuse a b ->
    let (a', n1) = goExpr n a
        (b', n2) = goExpr n1 b
    in (EReuse a' b', n2)
  EDelay x -> let (x', n1) = goExpr n x in (EDelay x', n1)
  EForce x -> let (x', n1) = goExpr n x in (EForce x', n1)
  -- Leaves: EVar, ELit, ECon, EFunRef
  _ -> (e, n)

goList :: Fresh -> [Expr] -> ([Expr], Fresh)
goList n []     = ([], n)
goList n (x:xs) =
  let (x', n1)  = goExpr n x
      (xs', n2) = goList n1 xs
  in (x' : xs', n2)

goGroups :: Fresh -> [BindGroup] -> ([BindGroup], Fresh)
goGroups n []     = ([], n)
goGroups n (g:gs) =
  let (g', n1)  = goGroup n g
      (gs', n2) = goGroups n1 gs
  in (g' : gs', n2)

goGroup :: Fresh -> BindGroup -> (BindGroup, Fresh)
goGroup n []     = ([], n)
goGroup n (b:bs) =
  let (be, n1)  = goExpr n (bindExpr b)
      (bs', n2) = goGroup n1 bs
  in (b { bindExpr = be } : bs', n2)

goBranches :: Fresh -> [Branch] -> ([Branch], Fresh)
goBranches n []     = ([], n)
goBranches n (br:rest) =
  let (br', n1)  = goBranch n br
      (rest', n2) = goBranches n1 rest
  in (br' : rest', n2)

-- | Rewrite a single branch: walk its body first, then lift any nested
-- constructor sub-patterns out of the pattern.
goBranch :: Fresh -> Branch -> (Branch, Fresh)
goBranch n (Branch pat guard body) =
  let (body1, n1) = goExpr n body
      (guard1, n2) = case guard of
        Nothing -> (Nothing, n1)
        Just g  -> let (g', n') = goExpr n1 g in (Just g', n')
      (pat', body', n3) = liftPattern n2 pat body1
  in (Branch pat' guard1 body', n3)

-- | If the pattern is a PatCon with nested PatCon sub-patterns, rewrite
-- each nested sub-pattern to a fresh @PatVar@ and wrap the body with an
-- inner @ECase@ that re-matches the field. Nested sub-patterns inside
-- that inner match are handled recursively (the inner 'Branch' itself
-- runs through 'goBranch' again).
liftPattern :: Fresh -> Pattern -> Expr -> (Pattern, Expr, Fresh)
liftPattern n (PatCon qn subs) body =
  let (subs', body', n') = liftSubs n subs body
  in (PatCon qn subs', body', n')
liftPattern n pat body = (pat, body, n)

liftSubs :: Fresh -> [Pattern] -> Expr -> ([Pattern], Expr, Fresh)
liftSubs n []     body = ([], body, n)
liftSubs n (p:ps) body =
  -- Process the tail first so the wrappers stack in left-to-right order
  -- after we prepend the current one.
  let (ps', body1, n1) = liftSubs n ps body
      (p', body2, n2)  = liftOneSub n1 p body1
  in (p' : ps', body2, n2)

-- | Replace a sub-pattern that is a non-trivial PatCon with a fresh
-- PatVar, and wrap the body with an inner @ECase@ that dispatches on the
-- fresh variable using the original pattern. Simple sub-patterns
-- (@PatVar@, @PatWild@, @PatLit@) are returned unchanged.
liftOneSub :: Fresh -> Pattern -> Expr -> (Pattern, Expr, Fresh)
liftOneSub n (PatCon innerQn innerSubs) body =
  let (freshName, n1) = freshVar n
      -- The sub-pattern's type is not tracked here; use a generic type.
      ty = anyType
      -- Recursively lift inside the inner pattern by routing it through
      -- a fresh Branch that goBranch will normalize.
      innerBranch = Branch (PatCon innerQn innerSubs) Nothing body
      (innerBranch', n2) = goBranch n1 innerBranch
      wrapped = ECase (EVar freshName) [innerBranch']
  in (PatVar freshName ty, wrapped, n2)
liftOneSub n p body = (p, body, n)

-- | Generic any-type placeholder for synthesized pattern variables.
anyType :: Type
anyType = TCon (TypeCon (QName (T.pack "std") (Name (T.pack "any") 0)) KindValue)
