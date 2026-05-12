-- | Normalize case-expression patterns for robust self-hosted compilation.
--
-- Two transformations:
--
-- 1. Bool patterns: @PatCon \"True\" []@ / @PatCon \"False\" []@ are replaced
--    with @PatLit (LitInt 1)@ / @PatLit (LitInt 0)@. This lets the emitter
--    classify them as 'IntLitCase' rather than 'ConCase', bypassing
--    'isBoolConCase' (which depends on string comparison that the self-hosted
--    binary gets wrong).
--
-- 2. Exhaustive multi-constructor cases: when every branch is a 'PatCon'
--    and there are >= 2 branches, append a @PatWild@ default with body
--    @ELit (LitInt 0)@. This forces the emitter through the \"con + default\"
--    path in 'emitConChain', avoiding a single-element-list code path that
--    produces incorrect output in the self-hosted binary.
--
-- This pass is applied to 'progDefs' before OrganIR JSON emission, so the
-- self-hosted compiler never encounters the problematic patterns.

module Frankenstein.Core.NormalizePatterns
  ( normalizePatterns
  ) where

import Frankenstein.Core.Types

import qualified Data.Text as T

-- | Run pattern normalization over all definitions in a 'Program'.
normalizePatterns :: Program -> Program
normalizePatterns prog =
  prog { progDefs = map normDef (progDefs prog) }

normDef :: Def -> Def
normDef d = d { defExpr = normExpr (defExpr d) }

normExpr :: Expr -> Expr
normExpr expr = case expr of
  ECase scrut branches ->
    let scrut'    = normExpr scrut
        branches' = map normBranch branches
    in ECase scrut' (normBranches branches')
  EApp f args     -> EApp (normExpr f) (map normExpr args)
  ELam ps body    -> ELam ps (normExpr body)
  ELet bgs body   -> ELet (map (map normBind) bgs) (normExpr body)
  ETypeLam tvs e  -> ETypeLam tvs (normExpr e)
  ETypeApp e tys  -> ETypeApp (normExpr e) tys
  EHandle ef h b  -> EHandle ef (normExpr h) (normExpr b)
  EPerform qn es  -> EPerform qn (map normExpr es)
  ERetain e       -> ERetain (normExpr e)
  ERelease e      -> ERelease (normExpr e)
  EDrop e         -> EDrop (normExpr e)
  EReuse e1 e2    -> EReuse (normExpr e1) (normExpr e2)
  EDelay e        -> EDelay (normExpr e)
  EForce e        -> EForce (normExpr e)
  -- Leaves: EVar, ELit, ECon, EFunRef — no sub-expressions
  _               -> expr

normBind :: Bind -> Bind
normBind b = b { bindExpr = normExpr (bindExpr b) }

normBranch :: Branch -> Branch
normBranch (Branch pat guard body) =
  Branch pat (fmap normExpr guard) (normExpr body)

-- | Apply Bool and exhaustive-case normalizations to a branch list.
normBranches :: [Branch] -> [Branch]
normBranches branches
  | length branches == 2, isBoolBranches branches =
      map boolToPat branches
  | length branches >= 2, allPatCon branches =
      branches ++ [Branch (PatWild intT) Nothing (ELit (LitInt 0))]
  | otherwise = branches

-- | Check if exactly two branches cover True and False.
isBoolBranches :: [Branch] -> Bool
isBoolBranches branches =
  let names = [ nameText (qnameName qn)
              | Branch (PatCon qn _) _ _ <- branches ]
  in length names == 2
     && T.pack "True"  `elem` names
     && T.pack "False" `elem` names

-- | Check if all branches use PatCon.
allPatCon :: [Branch] -> Bool
allPatCon = all isPatCon
  where
    isPatCon (Branch (PatCon _ _) _ _) = True
    isPatCon _                         = False

-- | Convert a Bool PatCon branch to PatLit.
boolToPat :: Branch -> Branch
boolToPat (Branch (PatCon qn _) guard body)
  | nameText (qnameName qn) == T.pack "True"  =
      Branch (PatLit (LitInt 1)) guard body
  | nameText (qnameName qn) == T.pack "False" =
      Branch (PatLit (LitInt 0)) guard body
boolToPat b = b

-- | A dummy Int type for PatWild defaults.
intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)
