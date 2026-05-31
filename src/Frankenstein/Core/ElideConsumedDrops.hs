{-# LANGUAGE OverloadedStrings #-}
-- | Phase 12a step 3: elide drops on variables already consumed by
-- constructor args.
--
-- Perceus's insertPerceus pass inserts EDrop bindings for variables
-- whose scope ends, but it doesn't track that a variable was already
-- transferred to a constructor cell via `EApp (ECon …) [EVar n, …]`.
-- That transfer is a consume — the cell's eventual `kk_drop` cascade
-- will release the field — so the subsequent explicit EDrop is a
-- double-drop.
--
-- Without `KK_CASCADE=1` the cascade was a no-op, so the bogus drop
-- silently leaked.  With cascade on, the drop and the cascade both
-- fire and the cell is freed while another scope still holds a
-- borrowed pointer to it.  This pass walks the IR sequentially and
-- replaces those bogus EDrops with `ELit (LitInt 0)`.
--
-- Restricted to direct EVar args: `ERetain (EVar n)` is left alone
-- (the retain bumped rc; the dropper holds the bumped ref
-- independently).  Conservative enough to ship globally — the
-- pattern it elides is always a real bug.
module Frankenstein.Core.ElideConsumedDrops
  ( elideConsumedDrops
  , elideConsumedDropsProgram
  ) where

import Frankenstein.Core.Types

import Data.Set (Set)
import qualified Data.Set as Set

elideConsumedDropsProgram :: Program -> Program
elideConsumedDropsProgram prog =
  prog { progDefs = map elideDef (progDefs prog) }
  where
    elideDef d = d { defExpr = elideConsumedDrops (defExpr d) }

elideConsumedDrops :: Expr -> Expr
elideConsumedDrops = goExpr Set.empty
  where
    goExpr :: Set Name -> Expr -> Expr
    goExpr consumed expr = case expr of
      EDrop (EVar n)
        | Set.member n consumed -> ELit (LitInt 0)
      ELet bgs body ->
        let (bgs', consumed') = goBgs consumed bgs
        in ELet bgs' (goExpr consumed' body)
      EApp f args     -> EApp (goExpr consumed f) (map (goExpr consumed) args)
      ELam ps b       -> ELam ps (goExpr consumed b)
      ECase s brs     ->
        ECase (goExpr consumed s)
              [br { branchBody = goExpr consumed (branchBody br) } | br <- brs]
      ERetain e       -> ERetain (goExpr consumed e)
      ERelease e      -> ERelease (goExpr consumed e)
      EDrop e         -> EDrop (goExpr consumed e)
      EReuse a b      -> EReuse (goExpr consumed a) (goExpr consumed b)
      EDelay e        -> EDelay (goExpr consumed e)
      EForce e        -> EForce (goExpr consumed e)
      ETypeApp e ts   -> ETypeApp (goExpr consumed e) ts
      ETypeLam tvs e  -> ETypeLam tvs (goExpr consumed e)
      EPerform qn as  -> EPerform qn (map (goExpr consumed) as)
      EHandle eff h b -> EHandle eff (goExpr consumed h) (goExpr consumed b)
      _               -> expr  -- EVar, ELit, ECon, EFunRef

    -- Process binding groups left-to-right; each group can add to the
    -- consumed set for subsequent groups within the same ELet's bgs.
    goBgs :: Set Name -> [[Bind]] -> ([[Bind]], Set Name)
    goBgs c []        = ([], c)
    goBgs c (bg:rest) =
      let (bg', c')    = goBg c bg
          (rest', c'') = goBgs c' rest
      in (bg' : rest', c'')

    goBg :: Set Name -> [Bind] -> ([Bind], Set Name)
    goBg c []     = ([], c)
    goBg c (b:bs) =
      let rhs'      = goExpr c (bindExpr b)
          consumed' = c `Set.union` collectConsumed rhs'
          b'        = b { bindExpr = rhs' }
          (bs', c'') = goBg consumed' bs
      in (b' : bs', c'')

    -- Variables consumed by being passed to a constructor application
    -- as a direct EVar arg.  ERetain-wrapped EVars are NOT consumed
    -- (the retain bumped rc; the caller's scope still holds a ref).
    collectConsumed :: Expr -> Set Name
    collectConsumed (EApp (ECon _) args) =
      Set.fromList [n | EVar n <- args]
        `Set.union` Set.unions (map collectConsumed args)
    collectConsumed (EApp f args) =
      collectConsumed f `Set.union` Set.unions (map collectConsumed args)
    collectConsumed (ELet bgs body) =
      -- Explicit concatMap form (see commit a5a578c).
      Set.unions (collectConsumed body :
        concatMap (map (collectConsumed . bindExpr)) bgs)
    collectConsumed (ELam _ b)        = collectConsumed b
    collectConsumed (ECase s brs)     =
      collectConsumed s `Set.union`
        Set.unions [collectConsumed (branchBody br) | br <- brs]
    collectConsumed (ERetain _)       = Set.empty
    collectConsumed (ERelease _)      = Set.empty
    collectConsumed (EDrop _)         = Set.empty
    collectConsumed (EReuse a b)      = collectConsumed a `Set.union` collectConsumed b
    collectConsumed (EDelay e)        = collectConsumed e
    collectConsumed (EForce e)        = collectConsumed e
    collectConsumed (ETypeApp e _)    = collectConsumed e
    collectConsumed (ETypeLam _ e)    = collectConsumed e
    collectConsumed (EPerform _ as)   = Set.unions (map collectConsumed as)
    collectConsumed (EHandle _ h b)   = collectConsumed h `Set.union` collectConsumed b
    collectConsumed _                 = Set.empty
