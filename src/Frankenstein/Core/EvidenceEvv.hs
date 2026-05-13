{-# LANGUAGE LambdaCase #-}
-- | Plotkin-style evidence-vector dispatch (Frankenstein D-series).
--
-- An alternative lowering for 'EHandle'\/'EPerform' that uses a runtime
-- evidence stack instead of compile-time inline handler binding.
--
--   EHandle eff handler body
--   =>
--   let op_table = kk_optab_create(n_ops)
--       _       = kk_optab_set(op_table, 0, handler)
--       ...
--       evv'    = kk_evv_extend(evv, EFF_ID, op_table)
--   in body[evv -> evv']
--
--   EPerform eff.op args
--   =>
--   let tab  = kk_evv_lookup(evv, EFF_ID)
--       clos = kk_optab_get(tab, OP_IDX)
--   in clos(args)
--
-- The 'evv' parameter is currently a function-local 'EVar' bound at the
-- top of the def to 0 (empty evv). Cross-function threading via an
-- explicit @evv@ parameter is D3 work.
--
-- Effect ids are deterministic djb2 hashes of the effect name; both the
-- compiler and runtime use this hash so they agree.
--
-- This pass is gated behind '--evidence=plotkin' in the CLI; the default
-- pipeline still uses the inline 'evidencePass'.

module Frankenstein.Core.EvidenceEvv
  ( evidencePassEvv
  , hashEffectName
  ) where

import Frankenstein.Core.Types

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T

-- | Apply the Plotkin lowering to every top-level definition.
evidencePassEvv :: Program -> Program
evidencePassEvv prog = prog
  { progDefs = map (transformDef effs) (progDefs prog)
  }
  where
    effs = progEffects prog

-- | Lower one definition. Introduces a fresh @evv0 = 0@ binding (empty
-- evv) and rewrites the body so handles extend it and performs look it
-- up.
transformDef :: [EffectDecl] -> Def -> Def
transformDef effs d =
  d { defExpr = wrapWithEvv (transformExpr effs evvRoot (defExpr d)) }
  where
    evvRoot = Name "evv0" 0

    -- Bind evv0 = 0 (empty evidence vector) at the top of the function.
    wrapWithEvv inner = ELet [[evvBind]] inner
    evvBind = Bind evvRoot anyType (ELit (LitInt 0)) DefVal

-- | Walk an expression, rewriting EHandle and EPerform.
-- @evv@ names the current in-scope evidence vector EVar.
transformExpr :: [EffectDecl] -> Name -> Expr -> Expr
transformExpr effs evv = go
  where
    go expr = case expr of
      EHandle effRow handler body ->
        lowerHandle effRow handler body

      EPerform qn args ->
        lowerPerform qn (map go args)

      -- Pure structural recursion below.
      EVar _       -> expr
      ELit _       -> expr
      ECon _       -> expr
      EFunRef _    -> expr
      EApp f xs    -> EApp (go f) (map go xs)
      ELam ps b    -> ELam ps (go b)
      ELet bgs b   -> ELet [[bd { bindExpr = go (bindExpr bd) } | bd <- bg] | bg <- bgs] (go b)
      ECase s brs  -> ECase (go s)
                        [ Branch p (fmap go mg) (go bd) | Branch p mg bd <- brs ]
      ETypeApp e t -> ETypeApp (go e) t
      ETypeLam tv e -> ETypeLam tv (go e)
      ERetain e    -> ERetain (go e)
      ERelease e   -> ERelease (go e)
      EDrop e      -> EDrop (go e)
      EReuse a b   -> EReuse (go a) (go b)
      EDelay e     -> EDelay (go e)
      EForce e     -> EForce (go e)

    lowerHandle effRow handler body =
      let effName = effectRowName effRow
          effId   = hashEffectName effName
          numOps  = case lookupEffectDecl effs effName of
                      Just ed -> length (effectOps ed)
                      Nothing -> 1
          opTab    = freshName effName "op_table"
          newEvv   = freshName effName "evv"
          handler' = go handler
          opTabBind  = Bind opTab anyType
                         (eApp "kk_optab_create" [ELit (LitInt (fromIntegral numOps))])
                         DefVal
          opSetBind  = Bind (Name "_evv_set" 0) anyType
                         (eApp "kk_optab_set"
                            [EVar opTab, ELit (LitInt 0), handler'])
                         DefVal
          evvBind    = Bind newEvv anyType
                         (eApp "kk_evv_extend"
                            [EVar evv, ELit (LitInt effId), EVar opTab])
                         DefVal
          body'      = transformExpr effs newEvv body
      in ELet [[opTabBind, opSetBind, evvBind]] body'

    lowerPerform qn args =
      let effName = qnameModule qn
          opN     = nameText (qnameName qn)
          effId   = hashEffectName effName
          opIdx   = lookupOpIdx effs effName opN
          tabN    = freshName effName "tab"
          closN   = freshName effName "clos"
          lookupBind = Bind tabN anyType
                         (eApp "kk_evv_lookup"
                            [EVar evv, ELit (LitInt effId)])
                         DefVal
          getBind    = Bind closN anyType
                         (eApp "kk_optab_get"
                            [EVar tabN, ELit (LitInt (fromIntegral opIdx))])
                         DefVal
      in ELet [[lookupBind, getBind]]
           (EApp (EVar closN) args)

-- | Build @EApp (EVar fn) args@ where @fn@ is a runtime helper symbol.
eApp :: Text -> [Expr] -> Expr
eApp fn args = EApp (EVar (Name fn 0)) args

-- | Build a fresh-ish name. Not properly fresh, but unique enough given
-- one handler per effect per scope.
freshName :: Text -> Text -> Name
freshName ctx suffix = Name (ctx <> "_" <> suffix) 0

-- | djb2 hash of the effect name, used as the runtime effect id.
hashEffectName :: Text -> Integer
hashEffectName t =
  foldl step 5381 (T.unpack t) `mod` 0x7fffffff
  where
    step h c = (h * 33 + fromIntegral (ord c)) `mod` 0x7fffffff

-- | Look up the index of an operation within an effect declaration.
-- Returns 0 if the effect or op is unknown — the downstream lookup will
-- still happen, just to a default slot.
lookupOpIdx :: [EffectDecl] -> Text -> Text -> Int
lookupOpIdx effs effName opN =
  case lookupEffectDecl effs effName of
    Just ed ->
      case [ i | (i, op) <- zip [0..] (effectOps ed)
               , nameText (qnameName (opName op)) == opN ] of
        (i:_) -> i
        []    -> 0
    Nothing -> 0

-- | Find the effect declaration for a given (flattened) effect name.
lookupEffectDecl :: [EffectDecl] -> Text -> Maybe EffectDecl
lookupEffectDecl effs name =
  case [ ed | ed <- effs
            , flattenedName ed == name ] of
    (ed:_) -> Just ed
    []     -> Nothing
  where
    flattenedName ed =
      qnameModule (effectName ed) <> nameText (qnameName (effectName ed))

-- | Compute the canonical effect-row name. Empty rows fall back to "".
effectRowName :: EffectRow -> Text
effectRowName EffectRowEmpty           = ""
effectRowName (EffectRowVar _)         = ""
effectRowName (EffectRowExtend qn _)   = qnameModule qn <> nameText (qnameName qn)

-- | Any-typed placeholder when we don't track types in the pass.
anyType :: Type
anyType = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)
