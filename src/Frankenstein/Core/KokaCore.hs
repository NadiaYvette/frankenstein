-- | Bridge to Koka's actual Core IR
--
-- This module will contain the translation from Frankenstein's
-- simplified Core types to Koka's real Core.Core data types,
-- enabling us to feed into Koka's existing optimization pipeline
-- (Perceus, evidence-passing, inlining, etc.) and then emit via
-- either Koka's C backend or our MLIR backend.
--
-- For now: placeholder that documents the mapping.

module Frankenstein.Core.KokaCore
  ( toKokaCore
  , fromKokaCore
  ) where

import Frankenstein.Core.Types

import Data.Text (Text)

-- | Translate Frankenstein Core to Koka Core.
--
-- Mapping (Frankenstein -> Koka):
--   Program  -> Core.Core
--   Def      -> Core.DefGroup (NonRec or Rec)
--   DataDecl -> Core.TypeDefGroup
--   Expr     -> Core.Expr
--
-- Detailed expression mapping:
--   EVar n           -> Core.Var (TName n ty)
--   ELit l           -> Core.Lit l
--   ECon c           -> Core.Con (TName c ty) repr
--   EApp f args      -> Core.App f args
--   ELam params body -> Core.Lam [TName] body
--   ELet binds body  -> Core.Let [DefGroup] body
--   ECase scrut alts -> Core.Case [expr] [Branch]
--   ETypeApp e tys   -> Core.TypeApp e tys
--   ETypeLam tvs e   -> Core.TypeLam [TypeVar] e
--
-- Effect operations (need evidence-passing translation):
--   EPerform eff args -> Core.App (evv_lookup eff) args
--   EHandle eff h bod -> Core.Let [handler_defgroup] (install_handler body)
--
-- Resource operations (direct to Perceus):
--   ERetain e  -> (handled by Perceus pass, not in input Core)
--   ERelease e -> (handled by Perceus pass)
--   EDrop e    -> (handled by Perceus pass)
--   EReuse e f -> (handled by Perceus pass)
--
-- Laziness (desugared before reaching Koka Core):
--   EDelay e -> Core.App (Core.Var "kk_thunk_create") [Core.Lam [] e]
--   EForce e -> Core.App (Core.Var "kk_thunk_force") [e]

toKokaCore :: Program -> Either Text ()  -- placeholder: will return Koka Core
toKokaCore _prog = Left "Not yet connected to Koka codebase"

fromKokaCore :: () -> Either Text Program  -- placeholder
fromKokaCore _ = Left "Not yet connected to Koka codebase"
