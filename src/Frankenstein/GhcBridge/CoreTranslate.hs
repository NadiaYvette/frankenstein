-- | GHC Core -> Frankenstein Core Translation
--
-- Translates GHC's System FC Core into Frankenstein's Core IR.
-- Key mappings:
--   GHC Var        -> EVar
--   GHC App        -> EApp (collected multi-arg)
--   GHC Lam        -> ELam
--   GHC Let        -> ELet
--   GHC Case       -> ECase
--   GHC Cast       -> (coercion dropped, recurse on inner)
--   GHC Tick       -> (profiling, dropped)
--   GHC Type       -> (type argument, becomes ETypeApp)
--   GHC Coercion   -> (dropped / erased)
--
-- Demand annotations on binders -> laziness decisions:
--   Strict demand  -> no EDelay wrapper
--   Lazy demand    -> wrap in EDelay

module Frankenstein.GhcBridge.CoreTranslate
  ( translateProgram
  , translateExpr
  , translateTyCons
  ) where

import qualified Frankenstein.Core.Types as F

import GHC.Core
  ( CoreProgram, CoreBind, CoreExpr
  , Bind(..), Expr(..), Alt(..), AltCon(..)
  )
import GHC.Core.TyCo.Rep (Type(..), TyLit(..))
import GHC.Core.TyCon
  ( TyCon, tyConName, isAlgTyCon, tyConTyVars, tyConDataCons, tyConUnique
  )
import GHC.Types.Var
  ( Var, varName, varType, isTyVar
  , isInvisibleFunArg, binderVar, ForAllTyBinder
  )
import GHC.Types.Name (getOccString, nameUnique, nameModule_maybe)
import GHC.Types.Unique (getKey)
import GHC.Types.Literal (Literal(..))
import GHC.Types.Id (idDemandInfo, idDetails, isDataConId_maybe, isDeadBinder)
import GHC.Types.Id.Info (IdDetails(..))
import GHC.Types.ForeignCall (ForeignCall(..), CCallSpec(..), CCallTarget(..))
import GHC.Types.Demand (isStrictDmd, isAbsDmd)
import GHC.Core.DataCon (DataCon, dataConName, dataConOrigArgTys, dataConFieldLabels, dataConTyCon, isUnboxedTupleDataCon)
import GHC.Unit.Module (moduleNameString, moduleName)
import GHC.Data.FastString (unpackFS)
import GHC.Types.FieldLabel (flLabel)
import GHC.Core.TyCo.Rep (Scaled(..))
import Language.Haskell.Syntax.Basic (FieldLabelString(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Word as W

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Translate a full GHC Core program to a Frankenstein Program.
-- Takes the module name, the Core bindings, and the module's TyCons.
translateProgram :: Text -> CoreProgram -> [TyCon] -> Either Text F.Program
translateProgram modName binds tyCons =
  let defs = filter (not . isDictDef) (concatMap translateTopBind binds)
      -- Merge locally-defined TyCons with those referenced in expressions
      -- (e.g. [], Bool, Maybe from the standard library)
      referencedTyCons = collectReferencedTyCons binds
      allTyCons = dedupTyCons (tyCons ++ referencedTyCons)
      dataDecls = translateTyCons allTyCons
  in Right $ F.Program
    { F.progName    = F.QName modName (F.Name modName 0)
    , F.progDefs    = defs
    , F.progData    = dataDecls
    , F.progEffects = []
    }

-- | Translate a single GHC Core expression (public entry point).
translateExpr :: CoreExpr -> F.Expr
translateExpr = trExpr

-------------------------------------------------------------------------------
-- Top-level bindings -> Defs
-------------------------------------------------------------------------------

translateTopBind :: CoreBind -> [F.Def]
translateTopBind (NonRec b e) =
  [ F.Def
      { F.defName       = qualifyName b
      , F.defType       = translateType (varType b)
      , F.defExpr       = decideLaziness b e
      , F.defSort       = classifyBind b
      , F.defVisibility = F.Public
      }
  ]
translateTopBind (Rec pairs) =
  [ F.Def
      { F.defName       = qualifyName b
      , F.defType       = translateType (varType b)
      , F.defExpr       = trExpr e
      , F.defSort       = F.DefFun
      , F.defVisibility = F.Public
      }
  | (b, e) <- pairs
  ]

-------------------------------------------------------------------------------
-- Expression translation
-------------------------------------------------------------------------------

trExpr :: CoreExpr -> F.Expr
-- Data constructors (nullary or the worker id of a saturated con) appear
-- in GHC Core as Var nodes whose IdDetails marks them as DataConWorkId.
-- We translate these to ECon so the MLIR emitter can allocate a boxed
-- constructor rather than treating the name as an unresolved external.
trExpr (Var v)
  | Just dc <- isDataConId_maybe v =
      F.ECon (F.QName T.empty
               (F.Name (T.pack (getOccString (dataConName dc))) 0))
  | Just canonical <- normalizeGhcBuiltin v =
      F.EVar (F.Name canonical 0)
  -- foreign import ccall: extract the C function name from FCallId
  | FCallId (CCall (CCallSpec (StaticTarget _ clabel _ _) _ _)) <- idDetails v =
      F.EVar (F.Name (T.pack (unpackFS clabel)) 0)
trExpr (Var v) = F.EVar (translateName v)

trExpr (Lit l) = F.ELit (translateLit l)

-- Type application: App f (Type t) => ETypeApp
trExpr (App f (Type t)) = F.ETypeApp (trExpr f) [translateType t]

-- Detect unpackCString# applied to a string literal: build a [Char] cons-list
-- in the Core IR directly. After the simplifier runs, bare LitString values
-- are raw Addr# pointers, so we need to build the list here while we still
-- know the intent.
trExpr (App (Var v) (Lit (LitString bs)))
  | getOccString v `elem` ["unpackCString#", "unpackCStringUtf8#"] =
      let s = TE.decodeUtf8With lenientDecode bs
          nilCon = F.QName T.empty (F.Name "[]" 0)
          consCon = F.QName T.empty (F.Name ":" 0)
          go "" = F.ECon nilCon
          go t  = let (c, rest) = (T.head t, T.tail t)
                  in F.EApp (F.ECon consCon) [F.ELit (F.LitChar c), go rest]
      in go s

-- Regular application: collect args, strip dictionaries, simplify boxing
trExpr (App f a) =
  case collectArgs (App f a) of
    -- I#(literal) → just the literal (unbox)
    (Var v, [Lit l])
      | getOccString v `elem` ["I#", "C#"] -> F.ELit (translateLit l)
    -- I#(var) / C#(var) → just var (boxed/unboxed share i64 representation)
    (Var v, [arg])
      | getOccString v `elem` ["I#", "C#"] -> trExpr arg
    -- tagToEnum#(expr) → expr (Bool uses 0/1 integers, same as Int#)
    (Var v, [Type _, arg])
      | getOccString v == "tagToEnum#" -> trExpr arg
    -- putStrLn / hPutStr2 from GHC.Internal.IO.Handle.Text: GHC inlines
    -- `putStrLn s = hPutStr2 stdout s True` before our bridge sees Core.
    -- The runtime can't shim Handle/FD machinery, so route the call to
    -- println_haskell_chars (walks the [Char] cons-list and writes
    -- chars + trailing newline).  Args: (handle, string, addNewline).
    -- We use the addNewline flag to pick the right runtime helper.
    (Var v, args)
      | Just rt <- ghcIoOutputRuntime v
      , let valueArgs = filter (\x -> not (isDictArg x)
                                       && not (isRealWorldArg x)
                                       && not (isTypeArg x)) args
      , length valueArgs >= 2 ->
          let stringArg = valueArgs !! 1
              addNewline = case drop 2 valueArgs of
                (Var b : _) -> getOccString b /= "False"
                _           -> True
              fnName = if rt == "hPutStr2" && not addNewline
                       then "print_haskell_chars"
                       else "println_haskell_chars"
          in F.EApp (F.EVar (F.Name (T.pack fnName) 0)) [trExpr stringArg]
    -- General case: strip dictionary arguments and realWorld# state tokens
    (fun, args) ->
      let args' = filter (\x -> not (isDictArg x) && not (isRealWorldArg x)) args
      in F.EApp (trExpr fun) (map trExpr args')

-- Lambda: collect consecutive value binders into a single multi-param ELam
-- This ensures that e.g. \x y z -> body becomes ELam [(x,T),(y,T),(z,T)] body
-- rather than nested ELam [(x,T)] (ELam [(y,T)] (ELam [(z,T)] body))
trExpr (Lam b e)
  | isTyVar b = F.ETypeLam [translateTyVar b] (trExpr e)
  | otherwise  =
      let (moreParams, innerBody) = collectValueLams e
          allParams = (translateName b, translateType (varType b)) : moreParams
      in F.ELam allParams (trExpr innerBody)

-- Let: translate binding groups. If any binding produced an EDelay wrapper
-- (lazy), wrap every use of that name inside the body with EForce so the
-- thunk is actually forced before being used as a value.
trExpr (Let bind body) =
  let groups   = translateBind bind
      bodyCore = trExpr body
      lazyNames = Set.fromList
        [ F.bindName b
        | g <- groups, b <- g
        , case F.bindExpr b of
            F.EDelay _ -> True
            _          -> False
        ]
      forcedBody
        | Set.null lazyNames = bodyCore
        | otherwise          = wrapLazyUses lazyNames bodyCore
  in F.ELet groups forcedBody

-- Unboxed tuple from FFI: case ffi_call(args) of (# state_tok, result #) -> body
-- GHC wraps foreign import ccall results in (# State# RealWorld, result #).
-- After stripping realWorld# from args, the actual call returns just the value.
-- Bind the non-state binder to the scrutinee, ignore the state token.
trExpr (Case scrut _bndr _ty [Alt (DataAlt dc) bndrs rhs])
  | isUnboxedTupleDataCon dc
  , let nonStateBndrs = filter (not . isStateVar) bndrs
  , length nonStateBndrs < length bndrs =  -- at least one state token was filtered
      case nonStateBndrs of
        [resultBndr] ->
          -- Single non-state result: let result = scrut in rhs
          let bind = F.Bind
                { F.bindName = translateName resultBndr
                , F.bindType = translateType (varType resultBndr)
                , F.bindExpr = trExpr scrut
                , F.bindSort = F.DefVal
                }
          in F.ELet [[bind]] (trExpr rhs)
        [] ->
          -- All binders are state tokens (IO () call): just sequence
          trExpr rhs
        _ ->
          -- Multiple non-state results: fall through to general case
          F.ECase (trExpr scrut) [translateAlt (Alt (DataAlt dc) bndrs rhs)]
-- Case: simplify I# unboxing pattern, then translate
-- GHC Core: case scrut of bndr { I# ds# -> rhs } →
--   let bndr = scrut in let ds# = scrut in rhs
-- This strips the integer boxing that GHC uses internally.
-- The outer case binder (bndr) must also be bound since the rhs may
-- reference it (e.g. `case n of wild { I# x -> ... wild ... }`).
trExpr (Case scrut bndr _ty [Alt (DataAlt dc) [innerBndr] rhs])
  | getOccString (dataConName dc) `elem` ["I#", "C#"] =
      let innerBind = F.Bind
            { F.bindName = translateName innerBndr
            , F.bindType = translateType (varType innerBndr)
            , F.bindExpr = trExpr scrut
            , F.bindSort = F.DefVal
            }
          outerBind = F.Bind
            { F.bindName = translateName bndr
            , F.bindType = translateType (varType bndr)
            , F.bindExpr = trExpr scrut
            , F.bindSort = F.DefVal
            }
          binds = if isDeadBinder bndr then [innerBind] else [outerBind, innerBind]
      in F.ELet [binds] (trExpr rhs)
-- General case: translate scrutinee and alternatives.
-- When the case binder is live (used in alternatives, e.g. from @-patterns
-- like handler@(ELam ...)), bind it with a let so it's available in the body.
trExpr (Case scrut bndr _ty alts)
  | isDeadBinder bndr =
      F.ECase (trExpr scrut) (map translateAlt alts)
  | otherwise =
      let bndrName = translateName bndr
          bndrBind = F.Bind
            { F.bindName = bndrName
            , F.bindType = translateType (varType bndr)
            , F.bindExpr = trExpr scrut
            , F.bindSort = F.DefVal
            }
      in F.ELet [[bndrBind]]
           (F.ECase (F.EVar bndrName) (map translateAlt alts))

-- Cast: drop coercion, recurse
trExpr (Cast e _co) = trExpr e

-- Tick: drop profiling tick, recurse
trExpr (Tick _tick e) = trExpr e

-- Type: skip (shouldn't appear at expression level normally)
trExpr (Type _) = F.ELit (F.LitInt 0)

-- Coercion: skip
trExpr (Coercion _) = F.ELit (F.LitInt 0)

-------------------------------------------------------------------------------
-- Collect consecutive value lambda binders
-------------------------------------------------------------------------------

-- | Collect consecutive value (non-type) lambda binders, skipping type lambdas.
collectValueLams :: CoreExpr -> ([(F.Name, F.Type)], CoreExpr)
collectValueLams (Lam b e)
  | isTyVar b = collectValueLams e  -- skip type binders
  | otherwise =
      let (more, body) = collectValueLams e
      in ((translateName b, translateType (varType b)) : more, body)
collectValueLams (Cast e _) = collectValueLams e
collectValueLams (Tick _ e) = collectValueLams e
collectValueLams e = ([], e)

-------------------------------------------------------------------------------
-- Collect nested applications into (fun, [args])
-- Skips type arguments along the way
-------------------------------------------------------------------------------

collectArgs :: CoreExpr -> (CoreExpr, [CoreExpr])
collectArgs = go []
  where
    go args (App f (Type _)) = go args f            -- skip type args
    go args (App f a)        = go (a : args) f       -- collect value args
    go args e                = (e, args)

-- | Is this expression a typeclass dictionary argument?
-- GHC desugars typeclasses to dictionary-passing: e.g. (<=) $fOrdInt x y
-- We strip these since Frankenstein uses direct builtin operations.
isDictArg :: CoreExpr -> Bool
isDictArg (Var v) =
  let name = getOccString v
  in (isPrefixOf "$f" name && not (isInstanceMethod name))
     || isPrefixOf "$d" name  -- $dOrd, $dNum, etc. (alternative naming)
     || isPrefixOf "$c" name  -- $c==, etc. (method selectors)
     || isPrefixOf "$W" name  -- $WI# etc. (wrappers)
  where
    -- Instance methods like $fNumInt_$c+ are actual value arguments,
    -- not dictionaries. They appear after the simplifier inlines dictionary
    -- lookups. Don't filter them out.
    isInstanceMethod n = "_$c" `isInfixOf` n
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
    tails []     = [[]]
    tails s@(_:xs) = s : tails xs
isDictArg _ = False

-- | Is this expression a realWorld# state token?
-- GHC threads State# RealWorld through IO and foreign call wrappers.
-- We strip these since Frankenstein doesn't model the IO state token.
isRealWorldArg :: CoreExpr -> Bool
isRealWorldArg (Var v) = getOccString v == "realWorld#"
isRealWorldArg _ = False

-- | True for type arguments in App; GHC passes types as explicit args.
isTypeArg :: CoreExpr -> Bool
isTypeArg (Type _) = True
isTypeArg _        = False

-- | If this Var is one of GHC's stdout-writing IO primitives, return
-- the runtime helper to route through.  All currently recognised forms
-- take (handle, string, addNewline).  The bridge replaces the entire
-- call with a direct print on the string arg.
ghcIoOutputRuntime :: Var -> Maybe String
ghcIoOutputRuntime v =
  let occ = getOccString v
      mmod = case nameModule_maybe (varName v) of
               Just m  -> moduleNameString (moduleName m)
               Nothing -> ""
  in case (mmod, occ) of
       ("GHC.Internal.IO.Handle.Text", "hPutStr2")    -> Just "hPutStr2"
       ("GHC.IO.Handle.Text",          "hPutStr2")    -> Just "hPutStr2"
       _                                              -> Nothing

-- | Is this variable a State# RealWorld state token?
-- Used to detect the state component of unboxed tuples from IO/FFI operations.
isStateVar :: Var -> Bool
isStateVar v = isStateType (varType v)
  where
    isStateType (TyConApp tc _) = getOccString (tyConName tc) == "State#"
    isStateType _ = False

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Is this a compiler-generated dictionary/module definition?
-- These are GHC's desugared typeclass infrastructure and module metadata.
isDictDef :: F.Def -> Bool
isDictDef d =
  let n = T.unpack (F.nameText (F.qnameName (F.defName d)))
  in isPrefixOf "$f" n || isPrefixOf "$d" n || isPrefixOf "$c" n
     || isPrefixOf "$tr" n || isPrefixOf "$W" n || isPrefixOf "$tc" n
     || isPrefixOf "$krep" n

-------------------------------------------------------------------------------
-- Binding groups
-------------------------------------------------------------------------------

translateBind :: CoreBind -> [F.BindGroup]
translateBind (NonRec b e) =
  [[ F.Bind
      { F.bindName = translateName b
      , F.bindType = translateType (varType b)
      , F.bindExpr = decideLaziness b e
      , F.bindSort = classifyBind b
      }
  ]]
translateBind (Rec pairs) =
  [[ F.Bind
      { F.bindName = translateName b
      , F.bindType = translateType (varType b)
      , F.bindExpr = trExpr e
      , F.bindSort = F.DefFun
      }
   | (b, e) <- pairs
  ]]

-------------------------------------------------------------------------------
-- Case alternatives
-------------------------------------------------------------------------------

translateAlt :: Alt Var -> F.Branch
translateAlt (Alt con bndrs rhs) = F.Branch
  { F.branchPattern = translateAltCon con bndrs
  , F.branchGuard   = Nothing
  , F.branchBody    = trExpr rhs
  }

translateAltCon :: AltCon -> [Var] -> F.Pattern
translateAltCon DEFAULT _ = F.PatWild anyType
translateAltCon (LitAlt lit) _ = F.PatLit (translateLit lit)
translateAltCon (DataAlt dc) bndrs =
  F.PatCon
    (F.QName T.empty (F.Name (T.pack (getOccString (dataConName dc))) 0))
    (map (\b -> F.PatVar (translateName b) (translateType (varType b))) (filter (not . isTyVar) bndrs))

-------------------------------------------------------------------------------
-- Literals
-------------------------------------------------------------------------------

translateLit :: Literal -> F.Lit
translateLit (LitChar c)       = F.LitChar c
translateLit (LitNumber _ n)   = F.LitInt n
translateLit (LitFloat r)      = F.LitFloat (fromRational r)
translateLit (LitDouble r)     = F.LitFloat (fromRational r)
translateLit (LitString bs)    = F.LitString (TE.decodeUtf8With lenientDecode bs)
translateLit (LitLabel fs _)   = F.LitString (T.pack (unpackFS fs))
translateLit LitNullAddr       = F.LitInt 0  -- null pointer
translateLit (LitRubbish _ _)  = F.LitInt 0  -- undefined/bottom placeholder

-------------------------------------------------------------------------------
-- Names and type helpers
-------------------------------------------------------------------------------

translateName :: Var -> F.Name
translateName v = F.Name
  { F.nameText   = qualText
  , F.nameUnique = fromIntegral (getKey (nameUnique (varName v)))
  }
  where
    occName = T.pack (getOccString v)
    uniq    = getKey (nameUnique (varName v))
    -- Preserve module origin for imported names so the emitter can generate
    -- qualified cross-module calls (e.g. Frankenstein.Core.Types/nameText).
    -- For local names, append the GHC Unique to disambiguate float-out
    -- bindings (e.g. multiple 'lvl' from the simplifier).
    qualText = case nameModule_maybe (varName v) of
      Just m  -> let modStr = T.pack (moduleNameString (moduleName m))
                 in modStr <> "/" <> occName
      Nothing -> occName <> "$" <> T.pack (show uniq)

-- | Normalize well-known GHC typeclass method names to canonical builtins.
-- GHC inlines typeclass dictionaries, leaving references like
-- GHC.Internal.Num/+ which the rest of the pipeline (K oracle, MLIR emitter)
-- expects as bare "+".
normalizeGhcBuiltin :: Var -> Maybe Text
normalizeGhcBuiltin v =
  let occ = getOccString v
      mmod = case nameModule_maybe (varName v) of
               Just m  -> moduleNameString (moduleName m)
               Nothing -> ""
  in case (mmod, occ) of
    -- Num methods
    (m, "+")      | isGhcNum m -> Just "+"
    (m, "-")      | isGhcNum m -> Just "-"
    (m, "*")      | isGhcNum m -> Just "*"
    (m, "negate") | isGhcNum m -> Just "negate"
    (m, "abs")    | isGhcNum m -> Just "abs"
    (m, "signum") | isGhcNum m -> Just "signum"
    -- Integral methods
    (m, "div")    | isGhcReal m -> Just "/"
    (m, "mod")    | isGhcReal m -> Just "mod"
    (m, "quot")   | isGhcReal m -> Just "/"
    (m, "rem")    | isGhcReal m -> Just "mod"
    -- Eq methods
    (m, "==")     | isGhcEq m -> Just "=="
    (m, "/=")     | isGhcEq m -> Just "/="
    -- Ord methods
    (m, "<")      | isGhcOrd m -> Just "<"
    (m, ">")      | isGhcOrd m -> Just ">"
    (m, "<=")     | isGhcOrd m -> Just "<="
    (m, ">=")     | isGhcOrd m -> Just ">="
    -- GHC unboxed primops — these appear after the simplifier inlines
    -- Prelude functions, exposing the underlying Int# operations.
    (m, "+#")     | isGhcPrim m -> Just "+"
    (m, "-#")     | isGhcPrim m -> Just "-"
    (m, "*#")     | isGhcPrim m -> Just "*"
    (m, "remInt#")| isGhcPrim m -> Just "mod"
    (m, "quotInt#")| isGhcPrim m -> Just "/"
    (m, "==#")    | isGhcPrim m -> Just "=="
    (m, "/=#")    | isGhcPrim m -> Just "/="
    (m, "<#")     | isGhcPrim m -> Just "<"
    (m, ">#")     | isGhcPrim m -> Just ">"
    (m, "<=#")    | isGhcPrim m -> Just "<="
    (m, ">=#")    | isGhcPrim m -> Just ">="
    (m, "negateInt#") | isGhcPrim m -> Just "negate"
    -- Bitwise primops — GHC optimizes mod 2 to andI#, etc.
    (m, "andI#")   | isGhcPrim m -> Just "andI#"
    (m, "orI#")    | isGhcPrim m -> Just "orI#"
    (m, "xorI#")   | isGhcPrim m -> Just "xorI#"
    -- Typeclass instance methods exposed by the simplifier
    -- e.g. GHC.Internal.Num.$fNumInt_$c+ is the (+) method for Int
    (m, "$fNumInt_$c+")  | isGhcNum m -> Just "+"
    (m, "$fNumInt_$c-")  | isGhcNum m -> Just "-"
    (m, "$fNumInt_$c*")  | isGhcNum m -> Just "*"
    (m, "$fNumInt_$cnegate") | isGhcNum m -> Just "negate"
    (m, "$fNumInt_$cabs")    | isGhcNum m -> Just "abs"
    (m, "$fNumInt_$csignum") | isGhcNum m -> Just "signum"
    -- tagToEnum# converts Int# (0/1) to Bool — we treat it as identity
    -- since our Bool representation uses 0/1 integers.
    (m, "tagToEnum#") | isGhcPrim m -> Just "tagToEnum#"
    -- Address primops — appear after simplifier inlines unpackCString#
    (m, "indexCharOffAddr#") | isGhcPrim m -> Just "indexCharOffAddr#"
    (m, "plusAddr#")         | isGhcPrim m -> Just "plusAddr#"
    (m, "eqChar#")           | isGhcPrim m -> Just "=="
    (m, "neChar#")           | isGhcPrim m -> Just "/="
    (m, "ord#")              | isGhcPrim m -> Just "ord#"
    (m, "chr#")              | isGhcPrim m -> Just "chr#"
    _ -> Nothing
  where
    isGhcNum  m = m `elem` ["GHC.Internal.Num", "GHC.Num", "GHC.Internal.Num.Integer"]
    isGhcReal m = m `elem` ["GHC.Internal.Real", "GHC.Real"]
    isGhcEq   m = m `elem` ["GHC.Internal.Classes", "GHC.Classes"]
    isGhcOrd  m = m `elem` ["GHC.Internal.Classes", "GHC.Classes"]
    isGhcPrim m = m `elem` ["GHC.Internal.Prim", "GHC.Prim"]

translateTyVar :: Var -> F.TypeVar
translateTyVar v = F.TypeVar
  { F.tvName         = translateName v
  , F.tvKind         = F.KindStar
  , F.tvMultiplicity = F.Many
  }

-- | Create a QName for a top-level definition. Uses the module from GHC's name.
-- Note: translateName already embeds the module in the Name text for variable
-- references (EVar), but for definitions we use the short occurrence name and
-- store the module in QName.module instead, so the emitter can prefix correctly.
qualifyName :: Var -> F.QName
qualifyName v = F.QName modName (F.Name localName
                                        (fromIntegral (getKey (nameUnique (varName v)))))
  where
    occName = T.pack (getOccString v)
    uniq    = getKey (nameUnique (varName v))
    (modName, localName) = case nameModule_maybe (varName v) of
      Just m  -> (T.pack (moduleNameString (moduleName m)), occName)
      -- For internal names, disambiguate with unique (matches translateName)
      Nothing -> (T.empty, occName <> "$" <> T.pack (show uniq))

-------------------------------------------------------------------------------
-- Type translation
-------------------------------------------------------------------------------

-- | Translate a GHC Type to Frankenstein Core Type.
translateType :: Type -> F.Type

-- Function type: skip invisible (dictionary/constraint) arguments
translateType (FunTy flag _mult arg res)
  | isInvisibleFunArg flag = translateType res
  | otherwise =
      case translateType res of
        -- Accumulate consecutive function args into a single TFun
        F.TFun args eff retTy ->
          F.TFun ((F.Many, translateType arg) : args) eff retTy
        resTy ->
          F.TFun [(F.Many, translateType arg)] F.EffectRowEmpty resTy

-- Type constructor with no arguments
translateType (TyConApp tc []) =
  -- Unboxed primitive types: map to their boxed equivalents since our
  -- runtime represents everything as i64.
  case T.pack (getOccString (tyConName tc)) of
    n | n `elem` ["Int#", "Word#", "Int64#", "Word64#", "Addr#", "Char#"] ->
        F.TCon $ F.TypeCon
          { F.tcName = F.QName T.empty (F.Name "Int" 0)
          , F.tcKind = F.KindStar
          }
    n | n == "Double#" || n == "Float#" ->
        F.TCon $ F.TypeCon
          { F.tcName = F.QName T.empty (F.Name "Double" 0)
          , F.tcKind = F.KindStar
          }
    _ -> F.TCon $ F.TypeCon
          { F.tcName = tyConQName tc
          , F.tcKind = F.KindStar
          }

-- Type constructor applied to arguments
translateType (TyConApp tc args) =
  foldl F.TApp
    (F.TCon $ F.TypeCon { F.tcName = tyConQName tc, F.tcKind = F.KindStar })
    (map translateType args)

-- Type variable
translateType (TyVarTy v) =
  F.TVar $ F.TypeVar
    { F.tvName         = translateName v
    , F.tvKind         = F.KindStar
    , F.tvMultiplicity = F.Many
    }

-- Forall: collect consecutive foralls into one TForall
translateType (ForAllTy bndr ty) =
  let (bndrs, innerTy) = collectForAlls ty
      allBndrs = bndr : bndrs
      tvs = map forAllBndrToTypeVar allBndrs
  in F.TForall tvs (translateType innerTy)

-- Type application
translateType (AppTy f a) =
  F.TApp (translateType f) (translateType a)

-- Literal types: use string representation as a TCon
translateType (LitTy (NumTyLit n)) =
  F.TCon $ F.TypeCon
    { F.tcName = F.QName T.empty (F.Name (T.pack (show n)) 0)
    , F.tcKind = F.KindStar
    }
translateType (LitTy (StrTyLit fs)) =
  F.TCon $ F.TypeCon
    { F.tcName = F.QName T.empty (F.Name (T.pack (unpackFS fs)) 0)
    , F.tcKind = F.KindStar
    }
translateType (LitTy (CharTyLit c)) =
  F.TCon $ F.TypeCon
    { F.tcName = F.QName T.empty (F.Name (T.pack [c]) 0)
    , F.tcKind = F.KindStar
    }

-- Cast: ignore coercion, recurse on inner type
translateType (CastTy ty _) = translateType ty

-- Coercion type: fallback to anyType
translateType (CoercionTy _) = anyType

-- | Collect consecutive ForAllTy binders.
collectForAlls :: Type -> ([ForAllTyBinder], Type)
collectForAlls (ForAllTy b t) =
  let (bs, inner) = collectForAlls t
  in (b : bs, inner)
collectForAlls t = ([], t)

-- | Convert a ForAllTyBinder to a Frankenstein TypeVar.
forAllBndrToTypeVar :: ForAllTyBinder -> F.TypeVar
forAllBndrToTypeVar bndr =
  let tv = binderVar bndr
  in F.TypeVar
    { F.tvName         = translateName tv
    , F.tvKind         = F.KindStar
    , F.tvMultiplicity = F.Many
    }

-- | Build a QName from a GHC TyCon.
tyConQName :: TyCon -> F.QName
tyConQName tc =
  let n = tyConName tc
      nameT = T.pack (getOccString n)
      modPfx = case nameModule_maybe n of
                 Just m  -> T.pack (moduleNameString (moduleName m))
                 Nothing -> ""
  in F.QName modPfx (F.Name nameT 0)

-- | Generic "any" type placeholder (fallback for CoercionTy).
anyType :: F.Type
anyType = F.TCon $ F.TypeCon
  { F.tcName = F.QName T.empty (F.Name "any" 0)
  , F.tcKind = F.KindValue
  }

-------------------------------------------------------------------------------
-- Demand / laziness classification
-------------------------------------------------------------------------------

-- | Classify a binding based on GHC's demand information.
classifyBind :: Var -> F.DefSort
classifyBind b
  | isStrictDmd (idDemandInfo b) = F.DefVal
  | otherwise                     = F.DefFun

-- | Decide whether to wrap an expression in EDelay based on demand.
-- Strict bindings: use expression directly.
-- Absent bindings: dead code, emit a placeholder.
-- Lazy bindings: wrap in EDelay (thunk) — UNLESS the expression is a lambda,
-- since a lambda is already a value and thunking it is pointless.
decideLaziness :: Var -> CoreExpr -> F.Expr
decideLaziness b e
  | isAbsDmd (idDemandInfo b)    = F.ELit (F.LitInt 0)  -- dead code
  | isStrictDmd (idDemandInfo b) = trExpr e              -- strict: no thunk
  | isCheapValue e               = trExpr e              -- already a value: no thunk needed
  | otherwise                     = F.EDelay (trExpr e)  -- lazy: wrap in thunk

-- | Wrap every reference to a lazy name with EForce, respecting shadowing.
-- Used by the Let translator: when a let binding RHS was wrapped in EDelay,
-- each use of that name in the body must force the thunk before use.
wrapLazyUses :: Set F.Name -> F.Expr -> F.Expr
wrapLazyUses s e = case e of
  F.EVar n
    | Set.member n s -> F.EForce (F.EVar n)
    | otherwise       -> F.EVar n
  F.ELit _            -> e
  F.ECon _            -> e
  F.EFunRef _         -> e
  F.EApp f args       -> F.EApp (go f) (map go args)
  F.ELam params body  ->
    let s' = foldr (Set.delete . fst) s params
    in F.ELam params (wrapLazyUses s' body)
  F.ELet groups body  ->
    -- Translate RHSs with the outer set minus names being rebound by this let.
    -- (For non-recursive lets this is a conservative approximation; mutually
    -- recursive groups may over-wrap if a bound name shadows a lazy outer one.)
    let bound = Set.fromList [ F.bindName b | g <- groups, b <- g ]
        s' = s `Set.difference` bound
        groups' = map (map (\b -> b { F.bindExpr = wrapLazyUses s' (F.bindExpr b) })) groups
    in F.ELet groups' (wrapLazyUses s' body)
  F.ECase scrut brs ->
    F.ECase (go scrut) (map goBranch brs)
  F.ETypeApp e' ts    -> F.ETypeApp (go e') ts
  F.ETypeLam tvs e'   -> F.ETypeLam tvs (go e')
  F.EPerform q args   -> F.EPerform q (map go args)
  F.EHandle eff h b   -> F.EHandle eff (go h) (go b)
  F.ERetain e'        -> F.ERetain (go e')
  F.ERelease e'       -> F.ERelease (go e')
  F.EDrop e'          -> F.EDrop (go e')
  F.EReuse r a        -> F.EReuse (go r) (go a)
  F.EDelay e'         -> F.EDelay (go e')
  F.EForce e'         -> F.EForce (go e')
  where
    go = wrapLazyUses s
    goBranch br =
      let pBound = patternBoundNames (F.branchPattern br)
          s'     = s `Set.difference` pBound
      in br { F.branchBody = wrapLazyUses s' (F.branchBody br) }

-- | Collect names introduced by a pattern (so that branch bodies can shadow
-- outer lazy bindings correctly).
patternBoundNames :: F.Pattern -> Set F.Name
patternBoundNames p = case p of
  F.PatVar n _       -> Set.singleton n
  F.PatCon _ subs    -> Set.unions (map patternBoundNames subs)
  F.PatWild _        -> Set.empty
  F.PatLit _         -> Set.empty

-- | Check if a Core expression is already a value form that doesn't need
-- thunking: lambdas, literals, and bare variables. Wrapping these in EDelay
-- would only force a downstream EForce that we don't currently generate at
-- use sites — so the value would be read as a raw thunk pointer.
isCheapValue :: CoreExpr -> Bool
isCheapValue (Lam _ _)   = True
isCheapValue (Lit _)     = True
isCheapValue (Var _)     = True
isCheapValue (Cast e _)  = isCheapValue e
isCheapValue (Tick _ e)  = isCheapValue e
isCheapValue e@(App _ _) =
  case collectArgs e of
    -- I#(literal) is how GHC boxes small Int literals — cheap value.
    (Var v, [Lit _]) | getOccString v == "I#" -> True
    -- Type applications are cheap if the underlying expression is.
    (fun, args) | all isTypeArg args -> isCheapValue fun
    _ -> False
  where
    isTypeArg (Type _) = True
    isTypeArg _        = False
isCheapValue _           = False

-------------------------------------------------------------------------------
-- Collecting referenced TyCons from Core
-------------------------------------------------------------------------------

-- | Walk a CoreProgram and collect all TyCons referenced via DataCon usage.
-- This finds stdlib TyCons ([], Bool, Maybe, etc.) that are used in the
-- program but not present in mg_tcs (which only contains locally-defined
-- TyCons).
collectReferencedTyCons :: CoreProgram -> [TyCon]
collectReferencedTyCons = dedupTyCons . concatMap collectFromBind
  where
    collectFromBind (NonRec _ e) = collectFromExpr e
    collectFromBind (Rec pairs) = concatMap (collectFromExpr . snd) pairs

    collectFromExpr (Var v)
      | Just dc <- isDataConId_maybe v = [dataConTyCon dc]
      | otherwise = []
    collectFromExpr (Lit _) = []
    collectFromExpr (App f a) = collectFromExpr f ++ collectFromExpr a
    collectFromExpr (Lam _ e) = collectFromExpr e
    collectFromExpr (Let b e) = collectFromBind b ++ collectFromExpr e
    collectFromExpr (Case s _ _ alts) =
      collectFromExpr s ++ concatMap collectFromAlt alts
    collectFromExpr (Cast e _) = collectFromExpr e
    collectFromExpr (Tick _ e) = collectFromExpr e
    collectFromExpr (Type _) = []
    collectFromExpr (Coercion _) = []

    collectFromAlt (Alt (DataAlt dc) _ rhs) =
      dataConTyCon dc : collectFromExpr rhs
    collectFromAlt (Alt _ _ rhs) = collectFromExpr rhs

-- | Deduplicate TyCons by Unique.
dedupTyCons :: [TyCon] -> [TyCon]
dedupTyCons = go Set.empty
  where
    go :: Set W.Word64 -> [TyCon] -> [TyCon]
    go _ [] = []
    go seen (tc:tcs)
      | Set.member k seen = go seen tcs
      | otherwise = tc : go (Set.insert k seen) tcs
      where k = getKey (tyConUnique tc)

-------------------------------------------------------------------------------
-- TyCon -> DataDecl translation
-------------------------------------------------------------------------------

-- | Translate a list of GHC TyCons to Frankenstein DataDecls.
-- Only algebraic TyCons (data/newtype) are included; type synonyms,
-- type families, primitive TyCons, etc. are filtered out.
translateTyCons :: [TyCon] -> [F.DataDecl]
translateTyCons = map translateTyCon . filter isAlgTyCon

-- | Translate a single algebraic TyCon to a DataDecl.
translateTyCon :: TyCon -> F.DataDecl
translateTyCon tc = F.DataDecl
  { F.dataName   = tyConQName tc
  , F.dataParams = map translateTyVar (tyConTyVars tc)
  , F.dataCons   = map translateDataCon (tyConDataCons tc)
  , F.dataVis    = F.Public
  }

-- | Translate a GHC DataCon to a Frankenstein ConDecl.
translateDataCon :: DataCon -> F.ConDecl
translateDataCon dc =
  let dcName = F.QName T.empty (F.Name (T.pack (getOccString (dataConName dc))) 0)
      -- Field labels: if the constructor has record fields, use those names;
      -- otherwise generate positional names (field_0, field_1, ...)
      labels = dataConFieldLabels dc
      argTys = dataConOrigArgTys dc
      fields
        | null labels =
            [ (F.Name ("field_" <> T.pack (show i)) 0, translateType ty)
            | (i, Scaled _ ty) <- zip [(0 :: Int)..length argTys - 1] argTys
            ]
        | otherwise =
            [ (F.Name (T.pack (unpackFS (field_label (flLabel fl)))) 0, translateType ty)
            | (fl, Scaled _ ty) <- zip labels argTys
            ]
  in F.ConDecl
    { F.conName   = dcName
    , F.conFields = fields
    , F.conVis    = F.Public
    }
