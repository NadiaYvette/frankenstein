-- | Continuation-passing-style (CPS) conversion for Frankenstein Core
-- expressions, used to implement multi-shot effect handlers.
--
-- See @docs/multi-shot-design.md@ for the design context.
--
-- The core operation is:
--
-- > cps :: Expr -> (Expr -> Expr) -> Expr
--
-- which takes an expression and a /continuation builder/ — a Haskell
-- function from "what the value of the expression will be" to "the rest
-- of the computation" — and returns a transformed expression that has
-- each 'EPerform' site replaced by an explicit handler call carrying a
-- closure representing the remaining computation.
--
-- The converter is /pure/: no fresh-name source, no environment, no
-- IO. The single side-effecting concern (uniqueness of generated
-- continuation-parameter names) is handled by threading a counter
-- through the returned expression as it is built.
--
-- The implementation is the naïve Plotkin-style CPS transform.
-- Administrative redexes (immediate beta-redexes) are not optimised
-- away in this pass — downstream simplifiers can remove them.

module Frankenstein.Core.CpsConvert
  ( cpsExpr
  , cpsExprFresh
  , cpsTopLevel
    -- Re-exported for testing
  , runCps
  , Cps
  , freshK
  ) where

import Frankenstein.Core.Types

import qualified Data.Text as T

-- | The CPS-conversion monad. Threads an integer counter for generating
-- fresh continuation-parameter names. Not a real monad — just a thin
-- state wrapper, kept simple to avoid pulling in @mtl@ here.
newtype Cps a = Cps { unCps :: Int -> (a, Int) }

instance Functor Cps where
  fmap f (Cps run) = Cps $ \i -> let (a, i') = run i in (f a, i')

instance Applicative Cps where
  pure a = Cps $ \i -> (a, i)
  Cps rf <*> Cps ra = Cps $ \i ->
    let (f, i1) = rf i
        (a, i2) = ra i1
    in (f a, i2)

instance Monad Cps where
  return = pure
  Cps run >>= f = Cps $ \i ->
    let (a, i1) = run i
    in unCps (f a) i1

-- | Run a CPS computation starting at counter 0.
runCps :: Cps a -> a
runCps c = fst (unCps c 0)

-- | Allocate a fresh continuation-parameter name. Names are
-- @k_cps_<n>@; the @cps_@ infix keeps them distinguishable from
-- user-supplied @k@ identifiers.
freshK :: Cps Name
freshK = Cps $ \i ->
  (Name (T.pack ("k_cps_" ++ show i)) i, i + 1)

-- | A placeholder "any" type used for synthesised lambda parameters.
-- The MLIR emitter uses uniform @i64@ for all values at the function
-- boundary, so the precise type does not flow into codegen here.
anyTy :: Type
anyTy = TCon (TypeCon (QName "" (Name "Any" 0)) KindValue)

-- | CPS-convert an expression, given a continuation /builder/ that
-- takes the value-expression and produces the rest of the computation
-- (itself a 'Cps Expr' so the builder may do further CPS work).
cpsExpr :: Expr -> (Expr -> Cps Expr) -> Cps Expr

-- Leaves: pass the expression to the continuation.
cpsExpr e@(EVar _)    k = k e
cpsExpr e@(ELit _)    k = k e
cpsExpr e@(ECon _)    k = k e
cpsExpr e@(EFunRef _) k = k e

-- Application: convert the function, then the arguments, then apply.
cpsExpr (EApp f args) k =
  cpsExpr f $ \fv ->
    cpsExprs args $ \avs ->
      k (EApp fv avs)

-- Let bindings: transform binds left-to-right, then body.
cpsExpr (ELet bgs body) k = cpsBindGroups bgs body k

-- Case: convert the scrutinee, then transform each branch body with
-- the same outer continuation (each branch is a separate continuation
-- of the case expression).
cpsExpr (ECase scrut branches) k =
  cpsExpr scrut $ \sv -> do
    branches' <- mapM (cpsBranch k) branches
    pure (ECase sv branches')
  where
    cpsBranch kk b = do
      body' <- cpsExpr (branchBody b) kk
      pure b { branchBody = body' }

-- Perform: this is where the continuation reifies as a runtime
-- closure. The handler call shape is:
--   handlerFor(op)(args..., \v -> rest_of_body)
-- We leave 'EFunRef qn' as a sentinel — the evidence pass substitutes
-- the actual handler binding reference at lowering time.
cpsExpr (EPerform qn args) k =
  cpsExprs args $ \avs -> do
    vName    <- freshK
    restExpr <- k (EVar vName)
    let contLam    = ELam [(vName, anyTy)] restExpr
        handlerRef = EFunRef qn
    pure (EApp handlerRef (avs ++ [contLam]))

-- Lambda: a lambda is a value; pass it to the continuation. We do NOT
-- recurse into the body here — body has its own continuation scope.
cpsExpr e@(ELam _ _) k = k e

-- Type-level wrappers.
cpsExpr (ETypeLam tvs e) k = do
  e' <- cpsExpr e pure
  k (ETypeLam tvs e')
cpsExpr (ETypeApp e tys) k =
  cpsExpr e $ \v -> k (ETypeApp v tys)

-- Effect handler boundaries delimit CPS scope. We do NOT recurse into
-- the body — the evidence pass invokes CPS once per Multi handler,
-- on the handler's body, never recursively.
cpsExpr e@(EHandle _ _ _) k = k e

-- Perceus / laziness ops pass through. By the time multi-shot CPS
-- runs, these ops belong to the surrounding handler context, not to
-- the body's continuation reified at perform sites.
cpsExpr (ERetain e)  k = cpsExpr e $ \v -> k (ERetain v)
cpsExpr (ERelease e) k = cpsExpr e $ \v -> k (ERelease v)
cpsExpr (EDrop e)    k = cpsExpr e $ \v -> k (EDrop v)
cpsExpr (EReuse e1 e2) k =
  cpsExpr e1 $ \v1 -> cpsExpr e2 $ \v2 -> k (EReuse v1 v2)
cpsExpr (EDelay e) k = cpsExpr e $ \v -> k (EDelay v)
cpsExpr (EForce e) k = cpsExpr e $ \v -> k (EForce v)

-- | List-CPS helper: thread a list of subexpressions through CPS,
-- accumulating their value forms, then invoke the continuation on the
-- list of values. Used by 'EApp' and 'EPerform' argument lists.
cpsExprs :: [Expr] -> ([Expr] -> Cps Expr) -> Cps Expr
cpsExprs []     k = k []
cpsExprs (x:xs) k =
  cpsExpr x $ \xv ->
    cpsExprs xs $ \xvs -> k (xv : xvs)

-- | Bind-group helper. The proper Plotkin CPS for @let x = M in N@ fuses
-- the binding's value into the continuation:
--
-- > cps[let x = M in N] k  =  cps[M] (\v -> Let x = v in cps[N] k)
--
-- This is essential for multi-shot semantics: if M performs an effect,
-- the continuation captures "the rest of the let" (the body, with x
-- bound to the perform-result), so the handler can resume that rest
-- with different values arbitrarily many times.
--
-- For recursive bind groups or groups with multiple bindings we fall
-- back to the simpler treatment (CPS-convert each RHS independently
-- with @pure@ as the continuation, then ELet). This is unsound for
-- multi-shot with effectful recursive RHS but matches the existing
-- single-shot behaviour for those cases — multi-shot lets stay
-- non-recursive in practice.
cpsBindGroups :: [BindGroup] -> Expr -> (Expr -> Cps Expr) -> Cps Expr
cpsBindGroups [] body k = cpsExpr body k
-- Non-recursive single-binding case: fuse RHS into continuation.
cpsBindGroups ([Bind nm ty rhs srt] : rest) body k =
  cpsExpr rhs $ \v -> do
    inner <- cpsBindGroups rest body k
    pure (ELet [[Bind nm ty v srt]] inner)
cpsBindGroups (bg : rest) body k = do
  bg' <- mapM cpsBind bg
  rest' <- cpsBindGroups rest body k
  pure (ELet [bg'] rest')
  where
    cpsBind b = do
      e' <- cpsExpr (bindExpr b) pure
      pure b { bindExpr = e' }

-- | Top-level entry point: CPS-convert with an identity final
-- continuation. The body's return value /is/ the handle's overall
-- result modulo what the handler decides at perform sites.
cpsTopLevel :: Expr -> Expr
cpsTopLevel e = runCps (cpsExpr e pure)

-- | Convenience that also returns the fresh-counter end-state, useful
-- when the caller is threading multiple CPS conversions through a
-- shared name space.
cpsExprFresh :: Int -> Expr -> (Expr -> Cps Expr) -> (Expr, Int)
cpsExprFresh start e k =
  let Cps run = cpsExpr e k
  in run start
