-- | Scheme → Frankenstein Core, via CPS conversion.
--
-- Why CPS? Frankenstein's existing effect machinery (EHandle/EPerform)
-- compiles to single-shot inlined handlers — there's no continuation
-- capture in the runtime today. To make Scheme @call/cc@ have its
-- correct escape semantics anyway, we CPS-convert the Scheme front-end
-- before handing it to the OrganIR. After CPS, every function takes an
-- explicit continuation, and @(call/cc f)@ is just @(f k)@ where @k@
-- is the current continuation reified as a one-argument function. The
-- OrganIR sees ordinary higher-order code and the existing closure
-- machinery (PAP allocation, indirect dispatch via @kk_alloc_con@) does
-- the rest.
--
-- This validates the directions-file claim that the IR is principled
-- enough to host first-class continuations without runtime changes —
-- they fall out of plain lambdas and tail calls once we CPS-convert.
--
-- Implementation note: we use the classic HOAS / "Danvy-style" two-level
-- CPS transform. The transform takes a *meta-continuation* (a Haskell
-- function @Expr -> M Expr@) instead of a Core lambda, which makes
-- administrative beta-redexes disappear at translation time. Only the
-- continuations that are actually observed (passed to a function call
-- or to call/cc) get reified as real Core lambdas. This keeps the Core
-- output lean enough to stay on the fast path of the OrganIR backend.
--
-- Supported Scheme subset:
--
--   * @(define name expr)@ and @(define (name args ...) body)@
--   * @(lambda (args ...) body)@
--   * @(let ((x e) ...) body)@   — desugared to nested let-bindings
--   * @(if c t e)@               — zero is false, anything else true
--   * @(begin e1 e2 ...)@
--   * Integer literals (with optional unary minus)
--   * Identifiers
--   * Application @(f a b ...)@
--   * Primitives: @+ - * / < <= > >= = mod negate@
--   * @(call/cc f)@ — captures the current continuation
--
-- Anything else raises a translation error.

module Frankenstein.SchemeBridge.CoreTranslate
  ( translateScheme
  , translateSchemeFile
  ) where

import Frankenstein.Core.Types
import Frankenstein.SchemeBridge.Reader

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Strict

-- ---------------------------------------------------------------------------
-- Top-level entry points

translateSchemeFile :: Text -> [SExpr] -> Either Text Program
translateSchemeFile = translateScheme

translateScheme :: Text -> [SExpr] -> Either Text Program
translateScheme modName forms = do
  defs <- evalStateT (mapM (translateForm modName) forms) 0
  pure Program
    { progName    = QName modName (Name "main" 0)
    , progDefs    = concat defs
    , progData    = []
    , progEffects = []
    }

-- ---------------------------------------------------------------------------
-- Fresh-name supply

type M = StateT Int (Either Text)

fresh :: Text -> M Name
fresh prefix = do
  n <- get
  put (n + 1)
  pure (Name (prefix <> "_s" <> T.pack (show n)) 0)

failM :: Text -> M a
failM = lift . Left

-- ---------------------------------------------------------------------------
-- Meta-continuations
--
-- A 'MetaK' is a Haskell function that consumes a Core value and
-- produces the continuation application. Using MetaK we can beta-reduce
-- administrative redexes at translation time ("(λx. body) v" becomes
-- "body[v/x]" on the Haskell side with zero cost).
--
-- When we need a real Core lambda (for instance because we have to pass
-- the continuation to a function call or to call/cc), 'reifyK' builds
-- one: @λv. metaK (EVar v)@.

type MetaK = Expr -> M Expr

reifyK :: MetaK -> M Expr
reifyK k = do
  v <- fresh "v"
  body <- k (EVar v)
  pure $ ELam [(v, anyT)] body

-- ---------------------------------------------------------------------------
-- Top-level forms

translateForm :: Text -> SExpr -> M [Def]
translateForm modName form = case form of
  SList [SSym "define", SSym name, body] -> do
    def <- defineValue modName name body
    pure [def]

  SList (SSym "define" : SList (SSym name : params) : body : rest) -> do
    paramNames <- mapM expectSym params
    let bodyExpr = case rest of
          [] -> body
          _  -> SList (SSym "begin" : body : rest)
    def <- defineValue modName name
             (SList [SSym "lambda", SList (map SSym paramNames), bodyExpr])
    pure [def]

  _ -> failM $ "Unsupported top-level form: " <> T.pack (showHead form)

-- | Define a top-level binding.
--
-- @main@ is special: we evaluate it with the identity meta-continuation
-- so the final value is returned directly from main.
--
-- For other defs, if the body is a @(lambda (xs...) body)@ we emit a
-- direct Frankenstein function @name(xs..., k) = [body]_cps(k)@, so
-- call sites @(name a1 ... an)@ can call it with @(a1..., an, k)@
-- without an extra indirection. Non-lambda defines are rejected (use
-- main for scalar values).
defineValue :: Text -> Text -> SExpr -> M Def
defineValue modName name body
  | name == "main" = do
      bodyExpr <- cps body (\v -> pure v)
      pure Def
        { defName       = QName modName (Name name 0)
        , defType       = TFun [] EffectRowEmpty intT
        , defExpr       = bodyExpr
        , defSort       = DefFun
        , defVisibility = Public
        }
  | SList [SSym "lambda", SList params, lbody] <- body = do
      paramNames <- mapM expectSym params
      k <- fresh "k"
      bodyCps <- cps lbody (\v -> pure (EApp (EVar k) [v]))
      let lam = ELam ([(Name p 0, anyT) | p <- paramNames] ++ [(k, anyT)]) bodyCps
          argTys = replicate (length paramNames + 1) (Many, anyT)
      pure Def
        { defName       = QName modName (Name name 0)
        , defType       = TFun argTys EffectRowEmpty anyT
        , defExpr       = lam
        , defSort       = DefFun
        , defVisibility = Public
        }
  | otherwise =
      failM $ "Top-level (define " <> name
           <> " ...) requires a lambda body (only 'main' may be a scalar)"

-- ---------------------------------------------------------------------------
-- CPS transform
--
-- @cps form metaK@ evaluates @form@ and feeds the resulting Core
-- value to @metaK@, which decides how to consume it. This is the
-- classic higher-order CPS transform (Danvy "One-Pass Transformation").

cps :: SExpr -> MetaK -> M Expr
cps form metaK = case form of
  SInt n                        -> metaK (ELit (LitInt n))

  SSym sym
    | sym == "#t" || sym == "true"  -> metaK (ELit (LitInt 1))
    | sym == "#f" || sym == "false" -> metaK (ELit (LitInt 0))
    | otherwise                     -> metaK (EVar (Name sym 0))

  -- (lambda (xs...) body)
  -- After CPS: λ(xs... k'). [body]_cps(k')   — k' is a real Core lambda arg.
  SList [SSym "lambda", SList params, body] -> do
    paramNames <- mapM expectSym params
    k' <- fresh "k"
    bodyCps <- cps body (\v -> pure (EApp (EVar k') [v]))
    let lam = ELam ([(Name p 0, anyT) | p <- paramNames] ++ [(k', anyT)]) bodyCps
    metaK lam

  -- (let ((x e) ...) body) ≡ ((lambda (x ...) body) e ...)
  SList [SSym "let", SList bindings, body] -> do
    bindNames <- mapM extractLetBinding bindings
    let names  = map fst bindNames
        rhss   = map snd bindNames
        lam    = SList [SSym "lambda", SList (map SSym names), body]
    cps (SList (lam : rhss)) metaK

  -- (if c t e). The two arms share metaK, which would duplicate the
  -- continuation if inlined. So we reify metaK as a single Core lambda,
  -- bind it to a name, and apply that name in each arm.
  SList [SSym "if", c, t, e] -> do
    kname <- fresh "kif"
    kReified <- reifyK metaK
    tCps <- cps t (\v -> pure (EApp (EVar kname) [v]))
    eCps <- cps e (\v -> pure (EApp (EVar kname) [v]))
    cv <- fresh "cv"
    let ifExpr = ECase (EVar cv)
          [ Branch (PatLit (LitInt 0)) Nothing eCps
          , Branch (PatWild anyT)      Nothing tCps
          ]
    inner <- cps c (\cval -> pure (ELet [[Bind cv anyT cval DefVal]] ifExpr))
    pure $ ELet [[Bind kname anyT kReified DefVal]] inner

  -- (begin e1 e2) — sequence; ignore e1's result
  SList [SSym "begin", e1, e2] ->
    cps e1 (\_ -> cps e2 metaK)
  SList (SSym "begin" : rest@(_:_)) ->
    let go [x]      = cps x metaK
        go (x : xs) = cps x (\_ -> go xs)
        go []       = failM "(begin) with no forms"
    in go rest

  -- (call/cc f)
  -- Reifies metaK as a real Core continuation kOuter, builds a reified
  -- Scheme-level continuation cont that ignores its own k and resumes
  -- kOuter, then evaluates f and applies it to (cont, kOuter).
  SList [SSym "call/cc", f] -> cpsCallCC f metaK
  SList [SSym "call-with-current-continuation", f] -> cpsCallCC f metaK

  -- Primitive applications
  SList (SSym op : args)
    | Just core <- primOp op, length args == primArity op ->
        cpsArgs args $ \argVs ->
          metaK (EApp (EVar (Name core 0)) argVs)

  -- General application (f a1 a2 ...) — the callee is a CPS function
  -- taking n+1 args. We must pass a *real* Core lambda as the trailing
  -- continuation, so we reify metaK here.
  SList (fn : args) -> do
    kReified <- reifyK metaK
    cpsArgs (fn : args) $ \(fv : argVs) ->
      pure (EApp fv (argVs ++ [kReified]))
    where _ = (fn, args)  -- pattern exhaustive

  _ -> failM $ "Unsupported expression: " <> T.pack (showHead form)

cpsCallCC :: SExpr -> MetaK -> M Expr
cpsCallCC f metaK = do
  -- Reify outer continuation once.
  kOuter <- reifyK metaK
  kName  <- fresh "kcc"
  -- Build the reified Scheme continuation: λ(v, _k'). kName v
  vName    <- fresh "v"
  ignored  <- fresh "_k"
  let contLam = ELam [(vName, anyT), (ignored, anyT)] $
                  EApp (EVar kName) [EVar vName]
  -- After f is evaluated we call fv(contLam, kName).
  body <- cps f (\fv -> pure (EApp fv [contLam, EVar kName]))
  pure $ ELet [[Bind kName anyT kOuter DefVal]] body

-- | CPS-evaluate a list of arguments left-to-right and feed their
-- value expressions to the body builder.
cpsArgs :: [SExpr] -> ([Expr] -> M Expr) -> M Expr
cpsArgs forms body = go forms []
  where
    go []       acc = body (reverse acc)
    go (e:rest) acc = cps e (\v -> go rest (v : acc))

-- ---------------------------------------------------------------------------
-- Primitives table

primOp :: Text -> Maybe Text
primOp "+"         = Just "+"
primOp "-"         = Just "-"
primOp "*"         = Just "*"
primOp "/"         = Just "/"
primOp "mod"       = Just "mod"
primOp "modulo"    = Just "mod"
primOp "remainder" = Just "mod"
primOp "="         = Just "=="
primOp "eq?"       = Just "=="
primOp "=="        = Just "=="
primOp "/="        = Just "/="
primOp "<"         = Just "<"
primOp "<="        = Just "<="
primOp ">"         = Just ">"
primOp ">="        = Just ">="
primOp "negate"    = Just "negate"
primOp _           = Nothing

primArity :: Text -> Int
primArity "negate" = 1
primArity _        = 2

-- ---------------------------------------------------------------------------
-- Helpers

expectSym :: SExpr -> M Text
expectSym (SSym s) = pure s
expectSym other    = failM $ "Expected symbol, got: " <> T.pack (showHead other)

extractLetBinding :: SExpr -> M (Text, SExpr)
extractLetBinding (SList [SSym n, e]) = pure (n, e)
extractLetBinding other = failM $ "Bad let binding: " <> T.pack (showHead other)

intT :: Type
intT = TCon (TypeCon (QName "std" (Name "int" 0)) KindValue)

anyT :: Type
anyT = TCon (TypeCon (QName "std" (Name "any" 0)) KindValue)

showHead :: SExpr -> String
showHead (SList (h : _)) = "(" <> showHead h <> " ...)"
showHead (SList _)       = "()"
showHead (SSym s)        = T.unpack s
showHead (SInt n)        = show n
