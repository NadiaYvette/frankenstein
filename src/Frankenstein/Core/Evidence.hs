-- | Evidence-Passing Translation for Algebraic Effects
--
-- This is the key insight from Koka: algebraic effects can be compiled
-- by passing "evidence" records (structs of operation implementations)
-- as extra function arguments — similar to how typeclass dictionaries
-- are passed in GHC.
--
-- For each @EHandle eff handler body@:
--   1. Build an evidence record containing the handler's operations
--   2. Pass it as an extra argument to the body
--
-- For each @EPerform eff op args@:
--   1. Look up the evidence record in scope
--   2. Extract the operation from the evidence
--   3. Call it with the original args
--
-- After this pass, no EHandle or EPerform nodes remain in the AST.
-- All effect operations have been desugared to plain function calls.

module Frankenstein.Core.Evidence
  ( evidencePass
  , evidencePassDef
  , collectGlobalEffects
  , evidencePassGlobal
  ) where

import Frankenstein.Core.Types
import Frankenstein.Core.EffectOpt (isAbortHandler, HandlerKind(..), classifyHandler)
import Frankenstein.Core.CpsConvert (cpsTopLevel)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Evidence scope: maps effect names to evidence variable names
type EvidenceScope = Map Text Name

-- | For multi-op effects, maps (effect name, op name) to the bound
-- variable name that holds that specific operation handler.
type OpScope = Map (Text, Text) Name

-- | Abort effect scope: maps effect name to (tag literal, handler expression)
-- so that EPerform for an abort effect can emit kk_handler_abort(tag, handler(args)).
-- We store the handler expression (not just a name) so it can be inlined directly
-- into the lifted body lambda, avoiding free-variable capture issues.
type AbortScope = Map Text (Int, Expr)

-- | Combined scope for evidence pass
data Scope = Scope
  { scopeEvidence :: !EvidenceScope
  , scopeOps     :: !OpScope
  , scopeAbort   :: !AbortScope
  } deriving (Show)

emptyScope :: Scope
emptyScope = Scope Map.empty Map.empty Map.empty

insertEvidence :: Text -> Name -> Scope -> Scope
insertEvidence eff evName s = s { scopeEvidence = Map.insert eff evName (scopeEvidence s) }

insertOp :: Text -> Text -> Name -> Scope -> Scope
insertOp eff op opVarName s = s { scopeOps = Map.insert (eff, op) opVarName (scopeOps s) }

-- | Build a global effect registry from all effect declarations in a program.
-- This maps each effect's flattened name to its list of operation declarations,
-- enabling cross-module effect resolution after the linker merges modules.
collectGlobalEffects :: Program -> Map Text [OpDecl]
collectGlobalEffects prog =
  Map.fromList
    [ (qnameModule (effectName ed) <> nameText (qnameName (effectName ed)), effectOps ed)
    | ed <- progEffects prog
    ]

-- | Run the evidence-passing translation on an entire program.
-- After this pass, EHandle and EPerform are eliminated.
evidencePass :: Program -> Program
evidencePass prog = prog
  { progDefs = map (evidencePassDef (progEffects prog)) (progDefs prog)
  }

-- | Run the evidence-passing translation using a global effect registry.
-- Use this after the linker has merged multiple programs, so that effects
-- declared in one module can be handled/performed in another.
evidencePassGlobal :: Map Text [OpDecl] -> Program -> Program
evidencePassGlobal globalEffects prog =
  let -- Convert the global registry back to [EffectDecl] for the existing machinery,
      -- merging with any local effect declarations.
      globalDecls =
        [ EffectDecl
            { effectName = QName "" (Name effName 0)
            , effectParams = []
            , effectOps = ops
            }
        | (effName, ops) <- Map.toList globalEffects
        ]
      -- Combine: global registry takes precedence, then local declarations
      allEffects = globalDecls ++ progEffects prog
      -- Deduplicate by flattened name (keep first occurrence = global)
      seen = go Map.empty allEffects
      go acc [] = acc
      go acc (ed:rest) =
        let n = qnameModule (effectName ed) <> nameText (qnameName (effectName ed))
        in if Map.member n acc then go acc rest
           else go (Map.insert n ed acc) rest
      deduped = Map.elems seen
  in prog
    { progDefs = map (evidencePassDef deduped) (progDefs prog)
    }

-- | Transform a single definition
evidencePassDef :: [EffectDecl] -> Def -> Def
evidencePassDef effs def = def
  { defExpr = fst (evidenceExpr effs emptyScope 0 (defExpr def))
  }

-- | Transform an expression, replacing EHandle/EPerform with plain calls.
--
-- The strategy for EHandle:
--   EHandle effRow handler body
--   =>
--   For a single-op effect (like exn with only "raise"):
--     let ev_exn = handler
--     in body[perform exn/raise(args) => ev_exn(args)]
--
--   For a multi-op effect (like console with "println"/"print"):
--     let ev_console = handler
--         ev_console_println = evv_select(ev_console, 0)
--         ev_console_print   = evv_select(ev_console, 1)
--     in body[perform console/println(args) => ev_console_println(args)]
--
-- For abort handlers (no resume parameter), emit callback-based setjmp/longjmp:
--   EHandle effRow abortHandler body
--   =>
--   let tag = <unique int>
--       body_fn = \() -> body'    -- body closure passed to runtime
--   in kk_handler_exec(tag, body_fn)
--   where body' has: EPerform => kk_handler_abort(tag, handler(args))

evidenceExpr :: [EffectDecl] -> Scope -> Int -> Expr -> (Expr, Int)
evidenceExpr effs scope nextTag expr = case expr of
  -- The key transformation: handle -> let-bind evidence, transform body
  EHandle effRow handler body
    -- Multi-shot handler: CPS-convert the body so each EPerform site
    -- reifies the remaining computation as a closure value the handler
    -- can invoke any number of times. See docs/multi-shot-design.md.
    --
    -- The CPS converter emits @EApp (EFunRef qn) (args ++ [contLam])@
    -- at perform sites. We then substitute @EFunRef qn@ with @EVar
    -- evName@ (the handler-binding reference) using the freshly-built
    -- evidence scope, so the call resolves at lowering time.
    | classifyHandler handler == HKMulti ->
        let effName = effectRowName effRow
            evName  = Name ("ev_" <> effName) 0
            (handler', nextTag1) = evidenceExpr effs scope nextTag handler
            -- Single-op shortcut: bind ev directly, install op-level
            -- handler reference for the (effName, only-op) pair so
            -- substEFunRef can replace EFunRef qn → EVar evName.
            scope0 = insertEvidence effName evName scope
            scope1 = case lookupEffectDecl effs effName of
              Just ed | [singleOp] <- effectOps ed ->
                let opN = nameText (qnameName (opName singleOp))
                in insertOp effName opN evName scope0
              _ -> scope0
            cpsBody  = cpsTopLevel body
            cpsBody' = substEFunRef scope1 cpsBody
            evBind = Bind
              { bindName = evName
              , bindType = anyType
              , bindExpr = handler'
              , bindSort = DefVal
              }
            (cpsBody'', nextTag2) = evidenceExpr effs scope1 nextTag1 cpsBody'
        in (ELet [[evBind]] cpsBody'', nextTag2)

    -- Abort handler: emit callback-based setjmp/longjmp pattern.
    -- The body is wrapped in a 0-arg lambda and passed to kk_handler_exec
    -- as a closure. kk_handler_exec calls setjmp in its own frame, then
    -- invokes the body. If the body performs an abort, kk_handler_abort
    -- longjmps back to kk_handler_exec.
    --
    -- Generated code:
    --   let tag = <unique int>
    --       body_lambda = \() -> body'   -- body' has EPerform inlined
    --   in kk_handler_exec(tag, body_lambda)
    --
    -- Inside body', EPerform becomes:
    --   kk_handler_abort(tag_literal, handler_expr(args))
    -- The handler expression is inlined directly (not via name reference)
    -- so the body lambda has no free variables from the handle scope.
    | isAbortHandler handler ->
        let effName = effectRowName effRow
            (handler', nextTag1) = evidenceExpr effs scope nextTag handler
            tag = nextTag1
            -- Register this effect as abort in scope with the handler EXPRESSION
            -- (not just a name). This way EPerform sites inline the handler directly,
            -- keeping the body lambda free of handle-scope captures.
            scope' = scope { scopeAbort = Map.insert effName (tag, handler') (scopeAbort scope) }
            (body', nextTag2) = evidenceExpr effs scope' (tag + 1) body
            -- Build: kk_handler_exec(tag, \() -> body')
            -- The body is wrapped in a 0-arg lambda passed directly as an argument
            -- (NOT let-bound, to avoid the emitter promoting it to a CAF and calling
            -- it eagerly). The lambda-lift path in the emitter will create a proper
            -- closure with field 0 = fptr, which kk_handler_exec invokes via the
            -- standard closure ABI.
            bodyLam = ELam [] body'
            execCall = EApp (EVar (Name "kk_handler_exec" 0))
              [ELit (LitInt (fromIntegral tag)), bodyLam]
        in (execCall, nextTag2)

    -- Tail-resumptive / normal handler: existing logic
    | otherwise ->
      let effName = effectRowName effRow
          evName  = Name ("ev_" <> effName) 0
          (handler', nextTag1) = evidenceExpr effs scope nextTag handler
      in case lookupEffectDecl effs effName of
        -- Multi-op effect: bind handler, then project each operation
        Just ed | length (effectOps ed) > 1 ->
          let evBind = Bind
                { bindName = evName
                , bindType = anyType
                , bindExpr = handler'
                , bindSort = DefVal
                }
              ops = effectOps ed
              opBindsAndNames = zipWith (mkOpBind effName evName) [0..length ops - 1] ops
              opBinds  = map fst opBindsAndNames
              scope' = foldl (\s (opN, varN) -> insertOp effName opN varN s)
                         (insertEvidence effName evName scope)
                         (map snd opBindsAndNames)
              (body', nextTag2) = evidenceExpr effs scope' nextTag1 body
          in (ELet [[evBind] ++ opBinds] body', nextTag2)

        -- Single-op or unknown: the handler IS the evidence (simple case)
        _ ->
          let scope' = insertEvidence effName evName scope
              scope'' = case lookupEffectDecl effs effName of
                Just ed | [singleOp] <- effectOps ed ->
                  let opN = nameText (qnameName (opName singleOp))
                  in insertOp effName opN evName scope'
                _ -> scope'
              (body', nextTag2) = evidenceExpr effs scope'' nextTag1 body
              evBind = Bind
                { bindName = evName
                , bindType = anyType
                , bindExpr = handler'
                , bindSort = DefVal
                }
          in (ELet [[evBind]] body', nextTag2)

  -- The key transformation: perform -> call through evidence
  EPerform qn args ->
    let effName = qnameModule qn
        opN     = nameText (qnameName qn)
        (args', nextTag') = mapAccumEvidence effs scope nextTag args
    in case Map.lookup effName (scopeAbort scope) of
      -- Abort effect: emit kk_handler_abort(tag, handler(args, dummy_resume))
      -- The handler expression is inlined directly from the abort scope.
      -- Abort handlers take (perform_args..., resume) but never call resume,
      -- so we pass 0 as a dummy value for the resume parameter.
      Just (tag, handlerExpr) ->
        let allArgs = args' ++ [ELit (LitInt 0)]  -- dummy resume
            handlerCall = EApp handlerExpr allArgs
            abortCall = EApp (EVar (Name "kk_handler_abort" 0))
              [ ELit (LitInt (fromIntegral tag))
              , handlerCall
              ]
        in (abortCall, nextTag')
      Nothing ->
        -- Normal evidence-passing (tail-resumptive or unhandled)
        case Map.lookup (effName, opN) (scopeOps scope) of
          Just opVarName ->
            (EApp (EVar opVarName) args', nextTag')
          Nothing ->
            case Map.lookup effName (scopeEvidence scope) of
              Just evName ->
                if isSingleOpEffect effs effName
                  then (EApp (EVar evName) args', nextTag')
                  else let idx = lookupOpIndex effs effName opN
                           projName = Name ("ev_" <> effName <> "_" <> opN) 0
                           selectExpr = EApp (EVar (Name "evv_select" 0))
                                          [ EVar evName
                                          , ELit (LitInt (fromIntegral idx))
                                          ]
                           projBind = Bind
                             { bindName = projName
                             , bindType = anyType
                             , bindExpr = selectExpr
                             , bindSort = DefVal
                             }
                       in (ELet [[projBind]] (EApp (EVar projName) args'), nextTag')
              Nothing ->
                let defaultFn = Name (effName <> "_" <> opN) 0
                in (EApp (EVar defaultFn) args', nextTag')

  -- Recurse through all other expression forms
  EVar _     -> (expr, nextTag)
  ELit _     -> (expr, nextTag)
  ECon _     -> (expr, nextTag)

  EApp fn as ->
    let (fn', nextTag1) = evidenceExpr effs scope nextTag fn
        (as', nextTag2) = mapAccumEvidence effs scope nextTag1 as
    in (EApp fn' as', nextTag2)

  ELam params body ->
    let (body', nextTag') = evidenceExpr effs scope nextTag body
    in (ELam params body', nextTag')

  ELet bgs body ->
    let (bgs', nextTag1) = mapAccumBindGroups effs scope nextTag bgs
        (body', nextTag2) = evidenceExpr effs scope nextTag1 body
    in (ELet bgs' body', nextTag2)

  ECase scrut branches ->
    let (scrut', nextTag1) = evidenceExpr effs scope nextTag scrut
        (branches', nextTag2) = mapAccumBranches effs scope nextTag1 branches
    in (ECase scrut' branches', nextTag2)

  ETypeApp e ts   -> let (e', n) = evidenceExpr effs scope nextTag e in (ETypeApp e' ts, n)
  ETypeLam tvs e  -> let (e', n) = evidenceExpr effs scope nextTag e in (ETypeLam tvs e', n)

  -- Perceus ops: recurse
  ERetain e       -> let (e', n) = evidenceExpr effs scope nextTag e in (ERetain e', n)
  ERelease e      -> let (e', n) = evidenceExpr effs scope nextTag e in (ERelease e', n)
  EDrop e         -> let (e', n) = evidenceExpr effs scope nextTag e in (EDrop e', n)
  EReuse e1 e2    -> let (e1', n1) = evidenceExpr effs scope nextTag e1
                         (e2', n2) = evidenceExpr effs scope n1 e2
                     in (EReuse e1' e2', n2)

  -- Laziness: recurse
  EDelay e        -> let (e', n) = evidenceExpr effs scope nextTag e in (EDelay e', n)
  EForce e        -> let (e', n) = evidenceExpr effs scope nextTag e in (EForce e', n)

  -- Function reference: pass through
  EFunRef _       -> (expr, nextTag)

-- | Helper: map evidenceExpr over a list of expressions, threading the tag counter
mapAccumEvidence :: [EffectDecl] -> Scope -> Int -> [Expr] -> ([Expr], Int)
mapAccumEvidence _effs _scope n [] = ([], n)
mapAccumEvidence effs scope n (e:es) =
  let (e', n1) = evidenceExpr effs scope n e
      (es', n2) = mapAccumEvidence effs scope n1 es
  in (e':es', n2)

-- | Helper: map over bind groups threading the tag counter
mapAccumBindGroups :: [EffectDecl] -> Scope -> Int -> [[Bind]] -> ([[Bind]], Int)
mapAccumBindGroups _effs _scope n [] = ([], n)
mapAccumBindGroups effs scope n (bg:bgs) =
  let (bg', n1) = mapAccumBinds effs scope n bg
      (bgs', n2) = mapAccumBindGroups effs scope n1 bgs
  in (bg':bgs', n2)

mapAccumBinds :: [EffectDecl] -> Scope -> Int -> [Bind] -> ([Bind], Int)
mapAccumBinds _effs _scope n [] = ([], n)
mapAccumBinds effs scope n (b:bs) =
  let (e', n1) = evidenceExpr effs scope n (bindExpr b)
      (bs', n2) = mapAccumBinds effs scope n1 bs
  in (b { bindExpr = e' } : bs', n2)

-- | Helper: map over branches threading the tag counter
mapAccumBranches :: [EffectDecl] -> Scope -> Int -> [Branch] -> ([Branch], Int)
mapAccumBranches _effs _scope n [] = ([], n)
mapAccumBranches effs scope n (br:brs) =
  let (body', n1) = evidenceExpr effs scope n (branchBody br)
      (guard', n2) = case branchGuard br of
        Nothing -> (Nothing, n1)
        Just g  -> let (g', n') = evidenceExpr effs scope n1 g in (Just g', n')
      (brs', n3) = mapAccumBranches effs scope n2 brs
  in (br { branchBody = body', branchGuard = guard' } : brs', n3)


-- | Create a binding for a single operation extracted from an evidence record.
-- @ev_<eff>_<op> = evv_select(ev_<eff>, <index>)@
mkOpBind :: Text -> Name -> Int -> OpDecl -> (Bind, (Text, Name))
mkOpBind effName evName idx op =
  let opN     = nameText (qnameName (opName op))
      varName = Name ("ev_" <> effName <> "_" <> opN) 0
      selectExpr = EApp (EVar (Name "evv_select" 0))
                     [ EVar evName
                     , ELit (LitInt (fromIntegral idx))
                     ]
      b = Bind
        { bindName = varName
        , bindType = anyType
        , bindExpr = selectExpr
        , bindSort = DefVal
        }
  in (b, (opN, varName))


-- | Extract the primary effect name from an effect row.
-- For EffectRowExtend "exn" _, returns "exn".
effectRowName :: EffectRow -> Text
effectRowName (EffectRowExtend qn _) = qnameModule qn <> nameText (qnameName qn)
effectRowName (EffectRowVar tv)      = nameText (tvName tv)
effectRowName EffectRowEmpty         = "pure"

-- | Look up an effect declaration by its (flattened) name.
lookupEffectDecl :: [EffectDecl] -> Text -> Maybe EffectDecl
lookupEffectDecl effs effName =
  case [ ed | ed <- effs
       , let n = qnameModule (effectName ed) <> nameText (qnameName (effectName ed))
       , n == effName ] of
    [ed] -> Just ed
    _    -> Nothing

-- | Check if an effect has exactly one operation (like exn has only "raise").
-- Single-op effects use the handler directly as the evidence;
-- multi-op effects need a record with named projections.
-- Unknown effects default to False (multi-op) for safety, so they go
-- through the record-projection path which generates correct dispatch.
isSingleOpEffect :: [EffectDecl] -> Text -> Bool
isSingleOpEffect effs effName =
  case lookupEffectDecl effs effName of
    Just ed -> length (effectOps ed) <= 1
    Nothing -> False  -- unknown effect: assume multi-op (safe default)

-- | Look up the index of an operation within its effect declaration.
-- Returns 0 if the operation or effect is not found (fallback).
lookupOpIndex :: [EffectDecl] -> Text -> Text -> Int
lookupOpIndex effs effName opN =
  case lookupEffectDecl effs effName of
    Just ed -> go 0 (effectOps ed)
    Nothing -> 0
  where
    go _ [] = 0
    go i (op:ops)
      | nameText (qnameName (opName op)) == opN = i
      | otherwise = go (i + 1) ops

-- | Placeholder type for evidence records
anyType :: Type
anyType = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)

-- | After CPS conversion, replace each 'EFunRef qn' (the sentinel
-- emitted by 'CpsConvert.cpsExpr' at @EPerform@ sites) with a regular
-- 'EVar' pointing at the evidence binding installed for that effect's
-- operation. References to effects not in the current scope are left
-- alone (they will be resolved by an outer handler, or fall through
-- to the unresolved-external path in the emitter).
substEFunRef :: Scope -> Expr -> Expr
substEFunRef scp = go
  where
    go (EFunRef qn) =
      let eff = qnameModule qn
          op  = nameText (qnameName qn)
      in case Map.lookup (eff, op) (scopeOps scp) of
           Just n  -> EVar n
           Nothing -> EFunRef qn
    go (EApp f as)     = EApp (go f) (map go as)
    go (ELam ps b)     = ELam ps (go b)
    go (ELet bgs b)    = ELet [[ bnd { bindExpr = go (bindExpr bnd) }
                                | bnd <- bg ]
                              | bg <- bgs ] (go b)
    go (ECase s brs)   = ECase (go s)
                          [ br { branchBody = go (branchBody br)
                               , branchGuard = fmap go (branchGuard br) }
                          | br <- brs ]
    go (EHandle ef h b)  = EHandle ef (go h) (go b)
    go (EPerform qn as)  = EPerform qn (map go as)
    go (ETypeLam tvs e)  = ETypeLam tvs (go e)
    go (ETypeApp e ts)   = ETypeApp (go e) ts
    go (ERetain e)       = ERetain  (go e)
    go (ERelease e)      = ERelease (go e)
    go (EDrop e)         = EDrop    (go e)
    go (EReuse e1 e2)    = EReuse (go e1) (go e2)
    go (EDelay e)        = EDelay (go e)
    go (EForce e)        = EForce (go e)
    go e@(EVar _)        = e
    go e@(ELit _)        = e
    go e@(ECon _)        = e
