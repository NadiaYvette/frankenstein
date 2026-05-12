-- | Effect Optimization Passes
--
-- Core IR → Core IR transformations that optimize algebraic effects
-- before the evidence-passing translation. These run before the
-- evidence pass, so EHandle/EPerform nodes are still present.
--
-- Passes:
--   1. Handler inlining: inline handler at perform site when both are local
--   2. Identity handler elimination: remove handlers that just resume
--   3. Tail-resumptive annotation: detect handlers that always resume in tail position
--
-- Evidence specialization (Phase 4b) is already handled by the existing
-- evidence pass, which directly binds known handler functions.

module Frankenstein.Core.EffectOpt
  ( effectOptimize
  , inlineLocalHandlers
  , eliminateIdentityHandlers
  , annotateTailResumptive
  , isAbortHandler
  , HandlerKind(..)
  , classifyHandler
  , EffectOptStats(..)
  , effectOptimizeWithStats
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)

-- | Statistics from effect optimization passes
data EffectOptStats = EffectOptStats
  { eosInlined     :: !Int  -- handlers inlined
  , eosEliminated  :: !Int  -- identity handlers eliminated
  , eosTailRes     :: !Int  -- tail-resumptive handlers detected
  } deriving (Show)

emptyStats :: EffectOptStats
emptyStats = EffectOptStats 0 0 0

-- | Run all effect optimizations on a program.
effectOptimize :: Program -> Program
effectOptimize = fst . effectOptimizeWithStats

-- | Run all effect optimizations, returning statistics.
effectOptimizeWithStats :: Program -> (Program, EffectOptStats)
effectOptimizeWithStats prog =
  let (defs1, s1) = foldDefs inlineLocalHandler emptyStats (progDefs prog)
      (defs2, s2) = foldDefs elimIdentityHandler s1 defs1
      (defs3, s3) = foldDefs annotateTailRes s2 defs2
  in (prog { progDefs = defs3 }, s3)

foldDefs :: (EffectOptStats -> Expr -> (Expr, EffectOptStats))
         -> EffectOptStats -> [Def] -> ([Def], EffectOptStats)
foldDefs f stats defs = foldr go ([], stats) defs
  where
    go def (acc, s) =
      let (expr', s') = f s (defExpr def)
      in (def { defExpr = expr' } : acc, s')

-------------------------------------------------------------------------------
-- Pass 1: Handler Inlining
-------------------------------------------------------------------------------

-- | If EHandle has a literal lambda handler and the body contains
-- EPerform for the same effect, inline the handler at each perform site.
--
-- Pattern:
--   EHandle eff (ELam params handlerBody) body
--     where body contains EPerform eff args
--   =>
--   body[EPerform eff args := applyHandler params handlerBody args]
--
-- Safe when: handler is a literal lambda (not a variable).
inlineLocalHandlers :: Expr -> Expr
inlineLocalHandlers = fst . inlineLocalHandler emptyStats

inlineLocalHandler :: EffectOptStats -> Expr -> (Expr, EffectOptStats)
inlineLocalHandler stats expr = case expr of
  EHandle effRow handler@(ELam _params _handlerBody) body
    -- Do NOT inline abort handlers — inlining an abort handler is semantically
    -- wrong because the continuation after the perform site would still execute.
    -- Abort handlers need setjmp/longjmp codegen in the evidence pass instead.
    | not (isAbortHandler handler) ->
    let effName = effectRowNameText effRow
        performCount = countPerforms effName body
    in if performCount > 0
       then
         -- Inline: replace each EPerform with handler application,
         -- then recurse into the result to enable further inlining.
         let body' = substitutePerforms effName handler body
             (body'', stats') = inlineLocalHandler
               (stats { eosInlined = eosInlined stats + performCount }) body'
         in (body'', stats')
       else
         -- No performs in body — fall through to generic traversal,
         -- which recurses into both handler and body.
         mapChildren inlineLocalHandler stats expr

  -- All other shapes: recurse into children via the generic traversal.
  _ -> mapChildren inlineLocalHandler stats expr

-- | Count EPerform nodes for a given effect in an expression.
countPerforms :: Text -> Expr -> Int
countPerforms effName expr = case expr of
  EPerform qn _ | qnameModule qn == effName -> 1 + sum (map (countPerforms effName) (performArgs expr))
  EPerform _ args -> sum (map (countPerforms effName) args)
  EApp fn as -> countPerforms effName fn + sum (map (countPerforms effName) as)
  ELam _ body -> countPerforms effName body
  ELet bgs body -> sum (map (sum . map (countPerforms effName . bindExpr)) bgs)
                   + countPerforms effName body
  ECase scrut brs -> countPerforms effName scrut
                     + sum (map (countPerforms effName . branchBody) brs)
  EHandle _ h b -> countPerforms effName h + countPerforms effName b
  EDelay e -> countPerforms effName e
  EForce e -> countPerforms effName e
  ERetain e -> countPerforms effName e
  EDrop e -> countPerforms effName e
  _ -> 0
  where
    performArgs (EPerform _ as) = as
    performArgs _ = []

-- | Replace EPerform nodes for a given effect with handler application.
substitutePerforms :: Text -> Expr -> Expr -> Expr
substitutePerforms effName handler expr = case expr of
  EPerform qn args | qnameModule qn == effName ->
    -- Replace: EPerform eff args => handler(args)
    EApp handler (map (substitutePerforms effName handler) args)
  EPerform qn args ->
    EPerform qn (map (substitutePerforms effName handler) args)
  EApp fn as ->
    EApp (substitutePerforms effName handler fn)
         (map (substitutePerforms effName handler) as)
  ELam params body ->
    ELam params (substitutePerforms effName handler body)
  ELet bgs body ->
    ELet (map (map (\b -> b { bindExpr = substitutePerforms effName handler (bindExpr b) })) bgs)
         (substitutePerforms effName handler body)
  ECase scrut brs ->
    ECase (substitutePerforms effName handler scrut)
          (map (\br -> br { branchBody = substitutePerforms effName handler (branchBody br) }) brs)
  EHandle er h b ->
    EHandle er (substitutePerforms effName handler h)
               (substitutePerforms effName handler b)
  EDelay e -> EDelay (substitutePerforms effName handler e)
  EForce e -> EForce (substitutePerforms effName handler e)
  ERetain e -> ERetain (substitutePerforms effName handler e)
  EDrop e -> EDrop (substitutePerforms effName handler e)
  _ -> expr

-------------------------------------------------------------------------------
-- Pass 2: Identity Handler Elimination
-------------------------------------------------------------------------------

-- | Remove handlers that immediately resume with the same value.
-- An identity handler is: \(x, resume) -> resume(x)
-- or more precisely: \x -> \k -> k(x)   (curried form)
--
-- Pattern:
--   EHandle eff identityHandler body  =>  body
eliminateIdentityHandlers :: Expr -> Expr
eliminateIdentityHandlers = fst . elimIdentityHandler emptyStats

elimIdentityHandler :: EffectOptStats -> Expr -> (Expr, EffectOptStats)
elimIdentityHandler stats expr = case expr of
  EHandle _effRow handler body
    | isIdentityHandler handler ->
        -- Drop the handler wrapper and recurse into the body.
        elimIdentityHandler
          (stats { eosEliminated = eosEliminated stats + 1 }) body

  -- All other shapes: recurse into children via the generic traversal.
  _ -> mapChildren elimIdentityHandler stats expr

-- | Check if a handler is the identity: \x k -> k(x)
-- Forms we recognize:
--   ELam [(x,_), (k,_)] (EApp (EVar k) [EVar x])       -- uncurried
--   ELam [(x,_)] (ELam [(k,_)] (EApp (EVar k) [EVar x]))  -- curried
isIdentityHandler :: Expr -> Bool
-- Uncurried: \(x, k) -> k(x)
isIdentityHandler (ELam [(x, _), (k, _)] (EApp (EVar kRef) [EVar xRef]))
  = nameText kRef == nameText k && nameText xRef == nameText x
-- Curried: \x -> \k -> k(x)
isIdentityHandler (ELam [(x, _)] (ELam [(k, _)] (EApp (EVar kRef) [EVar xRef])))
  = nameText kRef == nameText k && nameText xRef == nameText x
-- Single-arg identity: \k -> k(0) or similar — not a true identity
isIdentityHandler _ = False

-------------------------------------------------------------------------------
-- Pass 3: Tail-Resumptive Annotation
-------------------------------------------------------------------------------

-- | Detect handlers that always call resume in tail position.
-- These handlers don't need continuation capture at runtime — they
-- can be implemented as direct function calls (tail-resumptive optimization).
--
-- Currently this is a detection pass that adds stats. The actual
-- optimization (avoiding continuation capture) requires runtime changes.
annotateTailResumptive :: Expr -> Expr
annotateTailResumptive = fst . annotateTailRes emptyStats

annotateTailRes :: EffectOptStats -> Expr -> (Expr, EffectOptStats)
annotateTailRes stats expr = case expr of
  EHandle _effRow handler _body ->
    -- Count this handler if it's tail-resumptive, then recurse.
    let stats' = if isTailResumptiveHandler handler
                 then stats { eosTailRes = eosTailRes stats + 1 }
                 else stats
    in mapChildren annotateTailRes stats' expr

  -- All other shapes: recurse into children via the generic traversal.
  _ -> mapChildren annotateTailRes stats expr

-- | Check if a handler always calls resume (the last param) in tail position.
-- Pattern: \(args..., resume) -> ... resume(result)
-- where every control path ends with a call to resume.
isTailResumptiveHandler :: Expr -> Bool
-- Uncurried: \(x, k) -> k(expr)
isTailResumptiveHandler (ELam params body)
  | length params >= 2 =
      let resumeName = nameText (fst (last params))
      in isTailCall resumeName body
  | otherwise = False
isTailResumptiveHandler _ = False

-- | Check if a handler is an abort handler (never calls resume).
-- An abort handler is a lambda that doesn't reference its resume parameter.
-- Convention: resume is the LAST parameter (same as tail-resumptive check).
-- Handlers with fewer than 2 params have no resume param → always abort.
-- | Detect abort handlers: handlers that take a resume parameter but never call it.
-- Convention: in Koka-style effects, a handler takes (args..., resume).
-- If the last parameter (resume) is unused in the body, the handler is "abort"
-- (it discards the continuation). Single-param handlers are NOT abort — they
-- are tail-resumptive (the value replaces the perform site directly).
isAbortHandler :: Expr -> Bool
isAbortHandler (ELam params body)
  | length params >= 2 =
      let resumeName = nameText (fst (last params))
      in not (containsName resumeName body)
  | otherwise = False  -- single-param or no-param → not abort (tail-resumptive)
isAbortHandler _ = False

-- | Coarse classification of effect handlers. The evidence pass routes
-- each kind to a different lowering strategy. See docs/multi-shot-design.md.
data HandlerKind
  = HKAbort   -- ^ Handler discards the continuation (exn-style).
  | HKTail    -- ^ Handler calls the continuation exactly once in tail
              --   position (tail-resumptive — current default lowering).
  | HKMulti   -- ^ Handler calls the continuation more than once, or
              --   uses its result (must capture continuation as a value).
  deriving (Eq, Show)

-- | Classify a handler by how it uses the @resume@ continuation parameter.
-- Convention: the handler's last parameter is the continuation. We count
-- application sites of that parameter in the body.
--
--  * 0 references          → 'HKAbort'   (existing setjmp/longjmp lowering)
--  * 1 reference  in tail  → 'HKTail'    (existing inlining lowering)
--  * 1 ref non-tail / >=2  → 'HKMulti'   (CPS lowering, see CpsConvert)
classifyHandler :: Expr -> HandlerKind
classifyHandler (ELam params body)
  | length params >= 2 =
      let resumeName = nameText (fst (last params))
          n = countAppsOf resumeName body
      in if n == 0
           then HKAbort
           else if n == 1 && isTailCall resumeName body
                  then HKTail
                  else HKMulti
  | otherwise = HKTail  -- 1-param or 0-param handlers: treat as tail
classifyHandler _ = HKTail

-- | Count the number of @EApp (EVar name) _@ sites under @expr@.
-- Used by 'classifyHandler' to tell tail-resumptive from multi-shot.
countAppsOf :: Text -> Expr -> Int
countAppsOf name = go
  where
    go (EApp (EVar n) as)
      | nameText n == name = 1 + sum (map go as)
      | otherwise          = go (EVar n) + sum (map go as)
    go (EApp f as)         = go f + sum (map go as)
    go (ELam _ b)          = go b
    go (ELet bgs b)        =
      sum [ go (bindExpr bnd) | bg <- bgs, bnd <- bg ] + go b
    go (ECase s brs)       = go s + sum (map (go . branchBody) brs)
    go (EHandle _ h b)     = go h + go b
    go (EPerform _ as)     = sum (map go as)
    go (EDelay e)          = go e
    go (EForce e)          = go e
    go (ERetain e)         = go e
    go (ERelease e)        = go e
    go (EDrop e)           = go e
    go (EReuse e1 e2)      = go e1 + go e2
    go (ETypeApp e _)      = go e
    go (ETypeLam _ e)      = go e
    go (EVar _)            = 0
    go (ELit _)            = 0
    go (ECon _)            = 0
    go (EFunRef _)         = 0

-- | Check if a name appears anywhere in an expression
containsName :: Text -> Expr -> Bool
containsName name expr = case expr of
  EVar n         -> nameText n == name
  ELit _         -> False
  ECon _         -> False
  EApp fn as     -> containsName name fn || any (containsName name) as
  ELam _ body    -> containsName name body
  ELet bgs body  -> any (\bg -> any (containsName name . bindExpr) bg) bgs
                     || containsName name body
  ECase scrut brs -> containsName name scrut
                     || any (containsName name . branchBody) brs
  EHandle _ h b  -> containsName name h || containsName name b
  EPerform _ as  -> any (containsName name) as
  EDelay e       -> containsName name e
  EForce e       -> containsName name e
  ERetain e      -> containsName name e
  ERelease e     -> containsName name e
  EDrop e        -> containsName name e
  EReuse e1 e2   -> containsName name e1 || containsName name e2
  ETypeApp e _   -> containsName name e
  ETypeLam _ e   -> containsName name e
  EFunRef _      -> False

-- | Check if every path through an expression ends with a call to the named function.
isTailCall :: Text -> Expr -> Bool
isTailCall name (EApp (EVar fn) _) = nameText fn == name
isTailCall name (ECase _ branches) =
  all (isTailCall name . branchBody) branches
isTailCall name (ELet _ body) = isTailCall name body
isTailCall _ _ = False

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

effectRowNameText :: EffectRow -> Text
effectRowNameText (EffectRowExtend qn _) = qnameModule qn <> nameText (qnameName qn)
effectRowNameText (EffectRowVar tv) = nameText (tvName tv)
effectRowNameText EffectRowEmpty = "pure"

-- | Apply a pass function to every direct sub-expression of @e@, threading
-- the 'EffectOptStats' state left-to-right. This is the workhorse that
-- guarantees passes reach handlers/performs buried anywhere in the tree:
-- earlier revisions only recursed through a handful of constructors and
-- silently skipped 'EApp' arguments, 'ELet' bindings, and 'ECase' branches.
mapChildren
  :: (EffectOptStats -> Expr -> (Expr, EffectOptStats))
  -> EffectOptStats -> Expr -> (Expr, EffectOptStats)
mapChildren f s expr = case expr of
  -- Leaves
  EVar _    -> (expr, s)
  ELit _    -> (expr, s)
  ECon _    -> (expr, s)
  EFunRef _ -> (expr, s)

  EApp fn as ->
    let (fn', s1) = f s fn
        (as', s2) = mapList f s1 as
    in (EApp fn' as', s2)

  ELam params body ->
    let (body', s') = f s body
    in (ELam params body', s')

  ELet bgs body ->
    let (bgs', s1) = mapBindGroups f s bgs
        (body', s2) = f s1 body
    in (ELet bgs' body', s2)

  ECase scrut branches ->
    let (scrut', s1) = f s scrut
        (branches', s2) = mapBranches f s1 branches
    in (ECase scrut' branches', s2)

  ETypeApp e tys ->
    let (e', s') = f s e in (ETypeApp e' tys, s')
  ETypeLam tvs e ->
    let (e', s') = f s e in (ETypeLam tvs e', s')

  EPerform qn args ->
    let (args', s') = mapList f s args
    in (EPerform qn args', s')
  EHandle effRow handler body ->
    let (handler', s1) = f s handler
        (body', s2) = f s1 body
    in (EHandle effRow handler' body', s2)

  ERetain  e -> let (e', s') = f s e in (ERetain  e', s')
  ERelease e -> let (e', s') = f s e in (ERelease e', s')
  EDrop    e -> let (e', s') = f s e in (EDrop    e', s')
  EReuse a b ->
    let (a', s1) = f s a
        (b', s2) = f s1 b
    in (EReuse a' b', s2)

  EDelay e -> let (e', s') = f s e in (EDelay e', s')
  EForce e -> let (e', s') = f s e in (EForce e', s')

mapList
  :: (EffectOptStats -> Expr -> (Expr, EffectOptStats))
  -> EffectOptStats -> [Expr] -> ([Expr], EffectOptStats)
mapList _ s []     = ([], s)
mapList f s (e:es) =
  let (e', s1)  = f s e
      (es', s2) = mapList f s1 es
  in (e' : es', s2)

mapBindGroups
  :: (EffectOptStats -> Expr -> (Expr, EffectOptStats))
  -> EffectOptStats -> [BindGroup] -> ([BindGroup], EffectOptStats)
mapBindGroups _ s []     = ([], s)
mapBindGroups f s (g:gs) =
  let (g', s1)  = mapBindGroup f s g
      (gs', s2) = mapBindGroups f s1 gs
  in (g' : gs', s2)

mapBindGroup
  :: (EffectOptStats -> Expr -> (Expr, EffectOptStats))
  -> EffectOptStats -> BindGroup -> (BindGroup, EffectOptStats)
mapBindGroup _ s []     = ([], s)
mapBindGroup f s (b:bs) =
  let (be, s1)  = f s (bindExpr b)
      (bs', s2) = mapBindGroup f s1 bs
  in (b { bindExpr = be } : bs', s2)

mapBranches
  :: (EffectOptStats -> Expr -> (Expr, EffectOptStats))
  -> EffectOptStats -> [Branch] -> ([Branch], EffectOptStats)
mapBranches _ s []       = ([], s)
mapBranches f s (br:rest) =
  let (body', s1) = f s (branchBody br)
      (guard', s2) = case branchGuard br of
        Nothing -> (Nothing, s1)
        Just g  -> let (g', s') = f s1 g in (Just g', s')
      (rest', s3) = mapBranches f s2 rest
  in (br { branchBody = body', branchGuard = guard' } : rest', s3)
