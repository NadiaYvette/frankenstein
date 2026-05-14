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
  , evidencePassEvvGlobal
  , collectTopNames
  , hashEffectName
  ) where

import Frankenstein.Core.Types
import Frankenstein.Core.CpsConvert (cpsTopLevel)
import Frankenstein.Core.EffectOpt (HandlerKind(..), classifyHandler)

import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | Collect the set of (module, name) pairs of top-level definitions
-- across a list of programs. Use this when running 'evidencePassEvvGlobal'
-- per-module: cross-module call sites need to know which callees are
-- self-hosted (and so receive the evv parameter) versus which are C
-- shims (linked from outside our progDefs).
--
-- Restricted to modules under the @Frankenstein.@ namespace. Calls to
-- definitions in external packages (organ-ir, base, containers, etc.)
-- pass through unchanged — those modules are compiled outside our
-- pipeline and keep their original ABI; injecting evv there would
-- produce calls the C linker resolves to 1-arg callees.
collectTopNames :: [Program] -> Set (Text, Text)
collectTopNames progs = Set.fromList
  [ qnameToFlat (defName d)
  | p <- progs
  , d <- progDefs p
  , isFrankensteinModule (qnameModule (defName d))
  ]

-- | Is this module's ABI safe to change? We exclude known external
-- packages whose symbols are resolved by the C linker from cabal-
-- compiled binaries without the plotkin transform. Anything else is
-- fair game — including the small synthetic modules used by the
-- effect demos (EffectAskXfn, etc.), which are compiled end-to-end
-- through Frankenstein.
isFrankensteinModule :: Text -> Bool
isFrankensteinModule m = not (any (`T.isPrefixOf` m) externalPrefixes)
  where
    externalPrefixes =
      [ "OrganIR."         -- organ-ir local-path dep
      , "Data."            -- containers, text, etc.
      , "GHC."             -- base
      , "System."          -- base
      , "Control."         -- base, transformers, mtl
      , "Text."            -- text, pretty
      , "Language."        -- ghc itself
      , "Type."            -- ghc itself
      , "Numeric."         -- base
      , "Foreign."         -- base
      ]

-- | Single-module variant: builds topNames from the program itself.
-- Used by tests and the single-file CLI path.
evidencePassEvv :: Program -> Program
evidencePassEvv prog = evidencePassEvvGlobal (collectTopNames [prog]) prog

-- | Multi-module variant: takes a pre-built global topNames set so
-- cross-module call sites are recognised as self-hosted. Apply the
-- Plotkin lowering to every top-level definition of @prog@.
--
-- Each top-level definition gets an extra @evv@ parameter prepended to
-- its parameter list (D3 — full parameter threading). Every @EApp@
-- targeting a top-level definition (per @topNames@) is rewritten to
-- pass the current @evv@ as its first argument. The C @main@ wrapper
-- passes @0@.
evidencePassEvvGlobal :: Set (Text, Text) -> Program -> Program
evidencePassEvvGlobal topNames prog = prog
  { progDefs = map transformOrSkip (progDefs prog)
  }
  where
    effs = progEffects prog
    -- Only transform defs in the Frankenstein.* namespace. Defs from
    -- external packages (organ-ir, base, ...) leak in via the bridge
    -- but must keep their original ABI — the C linker resolves their
    -- symbols to cabal-compiled binaries without the evv parameter.
    transformOrSkip d
      | isFrankensteinModule (qnameModule (defName d)) =
          transformDef effs topNames d
      | otherwise = d

-- | Canonical (module, name) representation for matching call sites.
qnameToFlat :: QName -> (Text, Text)
qnameToFlat qn = (qnameModule qn, nameText (qnameName qn))

-- | Lower one definition.
--
-- (1) Prepend @evv@ as the first formal.
-- (2) Eta-expand if the existing 'defExpr' lambda arity is less than
--     'defType's flat arrow arity.
--
-- The combined effect: post-plotkin @defExpr@'s lambda arity matches
-- @defType@'s flat arrow arity exactly. Callers issue saturated calls
-- and never hit the emitter's oversaturated dispatch path
-- (Emitter.hs:1675), which has a bug for curried closure chains.
--
-- For a GHC-eta-reduced def like @runCps = compose fst etaBody@ with
-- type @Cps a -> a@: original 'defExpr' has 0 lambda params, 'defType'
-- has flat arity 1. After this pass:
--
--   defType: @(any, Cps a) -> a@           (flat arity 2)
--   defExpr: @\evv_p eta_p0 -> body eta_p0@ (lambda arity 2)
--
-- For a 1-param-lambda def like @consumeProgram = \txt -> ...@ with
-- type @Text -> Either ...@: original has 1 lambda param, type has
-- flat arity 1. After this pass:
--
--   defType: @(any, Text) -> Either ...@   (flat arity 2)
--   defExpr: @\evv_p txt -> ...@           (lambda arity 2)
transformDef :: [EffectDecl] -> Set (Text, Text) -> Def -> Def
transformDef effs topNames d
  | isFunctionType (defType d) =
      d { defExpr = funcExpr
        , defType = prependEvvType (defType d)
        }
  | otherwise =
      -- CAF (defType is not a function): don't add evv as a parameter
      -- and don't change defType — callers would mis-PAP the value
      -- and HOFs would receive a PAP where they expect the stored
      -- value. Instead, bind a synthetic @evv_p = 0@ at the top of
      -- the body so internal call sites that reference evv_p still
      -- resolve. CAFs are computed once and shared, so the empty-evv
      -- context is correct for non-effectful CAFs (the bootstrap case).
      d { defExpr =
            ELet [[Bind evvName anyType (ELit (LitInt 0)) DefVal]]
              (transformExpr effs topNames evvName (defExpr d))
        }
  where
    evvName = Name "evv_p" 0

    -- Is the declared type a function type (counting through TForall)?
    isFunctionType :: Type -> Bool
    isFunctionType (TForall _ t) = isFunctionType t
    isFunctionType (TFun {})     = True
    isFunctionType _             = False

    -- Function-typed def path: prepend evv as the first formal and
    -- eta-expand to match the declared flat arrow arity (so callers
    -- can issue saturated calls).
    funcExpr =
      let (existingParams, innerBody) = case defExpr d of
            ELam ps b -> (ps, b)
            b         -> ([], b)
          targetArity  = flatTypeArity (defType d) + 1
          currentArity = 1 + length existingParams
          missing      = max 0 (targetArity - currentArity)
          etaParams = [ (Name (T.pack ("eta_p" <> show i)) 0, anyType)
                      | i <- [0 .. missing - 1] ]
          bodyWithEta = case etaParams of
            []  -> innerBody
            ps  -> EApp innerBody [EVar n | (n, _) <- ps]
          allParams = (evvName, anyType) : existingParams ++ etaParams
      in ELam allParams (transformExpr effs topNames evvName bodyWithEta)

-- | Add @evv@ as the first arrow argument in the declared type. We
-- preserve any leading 'TForall'.
prependEvvType :: Type -> Type
prependEvvType = \case
  TForall vs t   -> TForall vs (prependEvvType t)
  TFun args e r  -> TFun ((Many, anyType) : args) e r
  t              -> TFun [(Many, anyType)] EffectRowEmpty t

-- | Total flat arrow arity of a type — sum of all nested 'TFun' arg
-- counts, after stripping 'TForall'.
flatTypeArity :: Type -> Int
flatTypeArity (TForall _ t)        = flatTypeArity t
flatTypeArity (TFun args _ result) = length args + flatTypeArity result
flatTypeArity _                    = 0

-- | Walk an expression, rewriting EHandle and EPerform.
-- @evv@ names the current in-scope evidence vector EVar.
-- @topNames@ is the set of (module, name) pairs that are top-level
-- definitions in this Program — call sites against these get @evv@
-- threaded as their first argument.
transformExpr :: [EffectDecl] -> Set (Text, Text) -> Name -> Expr -> Expr
transformExpr effs topNames evv = go
  where
    go expr = case expr of
      EHandle effRow handler body ->
        lowerHandle effRow handler body

      EPerform qn args ->
        lowerPerform qn (map go args)

      -- After CPS conversion of an HKMulti handler's body, perform
      -- sites appear as @EApp (EFunRef effect.op) (args ++ [contLam])@.
      -- Lower these the same way as raw EPerform: evv lookup + optab
      -- get + invoke (the continuation rides along as the last arg).
      EApp (EFunRef qn) xs
        | isEffectOp qn ->
            lowerPerform qn (map go xs)
      -- Inject evv as the first arg of every call targeting a top-level
      -- definition in this program. Calls to runtime helpers (kk_*) and
      -- to local closures (let-bound variables, lambda params) are not
      -- rewritten — their callees still take their original signature.
      EApp (EVar nm) xs
        | isTopLevel nm ->
            EApp (EVar nm) (EVar evv : map go xs)
      EApp (EFunRef qn) xs
        | isTopLevelQ qn ->
            EApp (EFunRef qn) (EVar evv : map go xs)
      EApp f xs    -> EApp (go f) (map go xs)

      -- Pure structural recursion below.
      EVar _       -> expr
      ELit _       -> expr
      ECon _       -> expr
      EFunRef _    -> expr
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

    -- An EVar refers to a top-level fn iff its name matches one of
    -- @topNames@. The bridge uses two encodings:
    --   * unqualified leaf name (e.g. "consumeName")
    --   * module/name slash-form (e.g. "Frankenstein.OrganIR.Consumer/consumeName")
    -- We accept either, splitting on the last @/@.
    isTopLevel :: Name -> Bool
    isTopLevel n =
      let txt = nameText n
          (mModule, leaf) = case T.breakOnEnd "/" txt of
            (pre, suf) | not (T.null pre) -> (Just (T.dropEnd 1 pre), suf)
            _                             -> (Nothing, txt)
      in case mModule of
           Just m  -> Set.member (m, leaf) topNames
           Nothing -> any (\(_, nm) -> nm == leaf) (Set.toList topNames)

    isTopLevelQ :: QName -> Bool
    isTopLevelQ qn = Set.member (qnameToFlat qn) topNames

    -- An EFunRef qn is an effect-op reference iff qn.module names a
    -- declared effect in this program. The CPS converter generates
    -- exactly these for EPerform sites; substEFunRef (inline pass) does
    -- the parallel job for the inline lowering. Here we lower the same
    -- shape directly to the evv-lookup pattern.
    isEffectOp :: QName -> Bool
    isEffectOp qn = case lookupEffectDecl effs (qnameModule qn) of
      Just _ -> True
      Nothing -> False

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
          -- For HKMulti handlers, CPS-convert the body before lowering.
          -- After CPS, every EPerform appears as
          --   EApp (EFunRef effect.op) (args ++ [contLam])
          -- which the EApp(EFunRef …) branch of transformExpr lowers to
          -- the evv-lookup + invoke pattern. The continuation lambda
          -- captures @newEvv@ as a free variable, so when the handler
          -- invokes the continuation later, it has the right evv in
          -- scope.
          bodyForTransform = case classifyHandler handler of
            HKMulti -> cpsTopLevel body
            _       -> body
          body'      = transformExpr effs topNames newEvv bodyForTransform
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
