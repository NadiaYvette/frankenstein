-- | Pattern matrix compilation — Maranget style.
--
-- Core → Core pass that rewrites 'ECase' expressions whose branches
-- may contain arbitrarily nested 'PatCon' patterns into a decision
-- tree of nested 'ECase's where every @PatCon@ contains only trivial
-- sub-patterns ('PatVar' \/ 'PatWild' \/ 'PatLit'). Sibling branches
-- that share the same outer constructor are /merged/ into a single
-- outer 'Branch' whose body is an inner case over the differing
-- sub-positions.
--
-- == Example
--
-- @
-- case xs of
--   Cons (Box n) ys   -> body1
--   Cons Unboxed  ys  -> body2
--   Nil               -> body3
-- @
--
-- compiles to (modulo fresh-name suffixes and let-bindings):
--
-- @
-- case xs of
--   Cons f0 f1 -> case f0 of
--                   Box f2   -> let n = f2 in let ys = f1 in body1
--                   Unboxed  -> let ys = f1 in body2
--   Nil        -> body3
-- @
--
-- == Algorithm
--
-- Classical Maranget \"Compiling Pattern Matching to Good Decision
-- Trees\" (ML '08), simplified for our subset:
--
--   * A row is @([patterns], Maybe guard, body)@.
--   * A column corresponds to one SSA-level scrutinee variable.
--   * We always select column 0 (leftmost-first heuristic).
--   * If every row has a trivial head pattern, drop the column
--     (binding any 'PatVar' to the column var via @let@).
--   * Otherwise, specialize per distinct head constructor appearing
--     in column 0 (in first-appearance order), producing one outer
--     @Branch@ per ctor. Wildcard\/var rows participate in every
--     specialized group as arity-many wildcards /and/ in the default.
--   * Guards are preserved by threading an @if-then-fall-through@
--     wrapper around the winning row's body when columns exhaust.
--
-- == Backward compatibility
--
-- 'ECase' expressions whose branches are already flat — no
-- 'PatCon' contains a nested 'PatCon' — are passed through
-- unchanged, so downstream emitter behavior on simple cases is
-- identical to the previous per-branch lifter.
module Frankenstein.Core.FlattenPatterns
  ( flattenPatterns
  , flattenPatternsExpr
  ) where

import Frankenstein.Core.Types

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | Rewrite every 'ECase' in a program via Maranget match compilation.
flattenPatterns :: Program -> Program
flattenPatterns prog =
  prog { progDefs = map flattenDef (progDefs prog) }

flattenDef :: Def -> Def
flattenDef d = d { defExpr = flattenPatternsExpr (defExpr d) }

-- | Rewrite every 'ECase' in an expression via Maranget match compilation.
flattenPatternsExpr :: Expr -> Expr
flattenPatternsExpr = fst . goExpr 0

-------------------------------------------------------------------------------
-- Fresh name supply
-------------------------------------------------------------------------------

type Fresh = Int

freshName :: Fresh -> Text -> (Name, Fresh)
freshName n prefix = (Name (prefix <> T.pack (show n)) n, n + 1)

freshNames :: Fresh -> Text -> Int -> ([Name], Fresh)
freshNames fr _ 0 = ([], fr)
freshNames fr prefix k =
  let (nm, fr1) = freshName fr prefix
      (rest, fr2) = freshNames fr1 prefix (k - 1)
  in (nm : rest, fr2)

-------------------------------------------------------------------------------
-- Expression walk
-------------------------------------------------------------------------------

-- | Recursively walk an expression. For every 'ECase' encountered,
-- first recurse into the scrutinee and branch children (so inner
-- nested cases are compiled bottom-up), then run the matrix compiler
-- on the current case.
goExpr :: Fresh -> Expr -> (Expr, Fresh)
goExpr fr e = case e of
  ECase scrut branches ->
    let (scrut', fr1) = goExpr fr scrut
        (branches', fr2) = goBranches fr1 branches
    in compileCase fr2 scrut' branches'
  EApp f args ->
    let (f', fr1) = goExpr fr f
        (args', fr2) = goList fr1 args
    in (EApp f' args', fr2)
  ELam params body ->
    let (body', fr1) = goExpr fr body
    in (ELam params body', fr1)
  ELet groups body ->
    let (groups', fr1) = goGroups fr groups
        (body', fr2) = goExpr fr1 body
    in (ELet groups' body', fr2)
  ETypeApp inner tys ->
    let (inner', fr1) = goExpr fr inner
    in (ETypeApp inner' tys, fr1)
  ETypeLam tvs inner ->
    let (inner', fr1) = goExpr fr inner
    in (ETypeLam tvs inner', fr1)
  EPerform qn args ->
    let (args', fr1) = goList fr args
    in (EPerform qn args', fr1)
  EHandle row handler body ->
    let (handler', fr1) = goExpr fr handler
        (body', fr2) = goExpr fr1 body
    in (EHandle row handler' body', fr2)
  ERetain  x -> let (x', fr1) = goExpr fr x in (ERetain  x', fr1)
  ERelease x -> let (x', fr1) = goExpr fr x in (ERelease x', fr1)
  EDrop    x -> let (x', fr1) = goExpr fr x in (EDrop    x', fr1)
  EReuse a b ->
    let (a', fr1) = goExpr fr a
        (b', fr2) = goExpr fr1 b
    in (EReuse a' b', fr2)
  EDelay x -> let (x', fr1) = goExpr fr x in (EDelay x', fr1)
  EForce x -> let (x', fr1) = goExpr fr x in (EForce x', fr1)
  -- Leaves: EVar, ELit, ECon, EFunRef
  _ -> (e, fr)

goList :: Fresh -> [Expr] -> ([Expr], Fresh)
goList fr []     = ([], fr)
goList fr (x:xs) =
  let (x', fr1)  = goExpr fr x
      (xs', fr2) = goList fr1 xs
  in (x' : xs', fr2)

goGroups :: Fresh -> [BindGroup] -> ([BindGroup], Fresh)
goGroups fr []     = ([], fr)
goGroups fr (g:gs) =
  let (g', fr1)  = goGroup fr g
      (gs', fr2) = goGroups fr1 gs
  in (g' : gs', fr2)

goGroup :: Fresh -> BindGroup -> (BindGroup, Fresh)
goGroup fr []     = ([], fr)
goGroup fr (b:bs) =
  let (be, fr1)  = goExpr fr (bindExpr b)
      (bs', fr2) = goGroup fr1 bs
  in (b { bindExpr = be } : bs', fr2)

goBranches :: Fresh -> [Branch] -> ([Branch], Fresh)
goBranches fr []     = ([], fr)
goBranches fr (br:rest) =
  let (br', fr1)  = goBranch fr br
      (rest', fr2) = goBranches fr1 rest
  in (br' : rest', fr2)

goBranch :: Fresh -> Branch -> (Branch, Fresh)
goBranch fr (Branch pat guard body) =
  let (body', fr1) = goExpr fr body
      (guard', fr2) = case guard of
        Nothing -> (Nothing, fr1)
        Just g  -> let (g', fr') = goExpr fr1 g in (Just g', fr')
  in (Branch pat guard' body', fr2)

-------------------------------------------------------------------------------
-- Matrix compilation entry point
-------------------------------------------------------------------------------

-- | If the case needs no compilation (all patterns flat), pass it
-- through. Otherwise bind the scrutinee to a var (if it isn't one
-- already) and invoke the matrix compiler with a single-column
-- matrix.
compileCase :: Fresh -> Expr -> [Branch] -> (Expr, Fresh)
compileCase fr scrut branches
  | not (anyNested branches) = (ECase scrut branches, fr)
  | otherwise =
      let (scrutVar, wrap, fr1) = bindScrut fr scrut
          rows = [ Row [p] g b | Branch p g b <- branches ]
          (inner, fr2) = compileMatrix fr1 [scrutVar] rows
      in (wrap inner, fr2)

-- | Does any branch contain a 'PatCon' nested inside another 'PatCon'?
anyNested :: [Branch] -> Bool
anyNested = any (patNested . branchPattern)
  where
    patNested (PatCon _ subs) = any isCon subs || any patNested subs
    patNested _ = False
    isCon (PatCon _ _) = True
    isCon _ = False

-- | If the scrutinee is already an 'EVar', use that name directly.
-- Otherwise invent a fresh name and return a let-wrapping continuation.
bindScrut :: Fresh -> Expr -> (Name, Expr -> Expr, Fresh)
bindScrut fr (EVar v) = (v, id, fr)
bindScrut fr e =
  let (v, fr1) = freshName fr "s"
      wrap body = ELet [[Bind v anyType e DefVal]] body
  in (v, wrap, fr1)

-------------------------------------------------------------------------------
-- The pattern matrix
-------------------------------------------------------------------------------

-- | A row of the pattern matrix:
-- @Row patterns guard body@. The pattern list has one entry per
-- active column and is the same length across all rows in a given
-- matrix.
data Row = Row [Pattern] (Maybe Expr) Expr

-- | Compile a pattern matrix into an 'Expr'. The column list names
-- the SSA variables that the rows' patterns are matching against.
compileMatrix :: Fresh -> [Name] -> [Row] -> (Expr, Fresh)
compileMatrix fr _ [] =
  -- No matching row. In a well-typed Haskell program the frontend
  -- has already checked exhaustiveness, so this is "impossible".
  -- Emit a sentinel; downstream classifyBranches treats a lone
  -- default as unconditional evaluation, which at worst yields 0.
  (ELit (LitInt 0), fr)
compileMatrix fr [] (Row _ Nothing body : _) = (body, fr)
compileMatrix fr [] (Row _ (Just g) body : rest) =
  -- Guard: test the guard; on success run the body, otherwise
  -- fall through to the remaining rows.
  let (restE, fr1) = compileMatrix fr [] rest
  in ( ECase g
        [ Branch (PatLit (LitInt 1)) Nothing body
        , Branch (PatWild anyType)   Nothing restE
        ]
     , fr1 )
compileMatrix fr (col : cols) rows
  | all headTrivial rows =
      -- Whole column has no ctor patterns — drop it after binding
      -- any 'PatVar' entries to the column var.
      let rows' = map (stripTrivialCol col) rows
      in compileMatrix fr cols rows'
  | otherwise =
      let ctors = distinctHeadCtors rows
          (ctorBranches, fr1) =
            buildCtorBranches fr col cols ctors rows
          (mDefault, fr2) =
            buildDefaultBranch fr1 col cols rows
      in ( ECase (EVar col) (ctorBranches ++ maybeToList mDefault)
         , fr2
         )
  where
    headTrivial :: Row -> Bool
    headTrivial (Row (PatCon _ _ : _) _ _) = False
    headTrivial _                          = True

    maybeToList Nothing  = []
    maybeToList (Just x) = [x]

-- | Drop column 0 of a row. If the head is a 'PatVar', bind the
-- variable to the column's SSA name via a @let@ so later rows see
-- the right environment.
stripTrivialCol :: Name -> Row -> Row
stripTrivialCol col (Row (p : ps) g body) = case p of
  PatVar n _ ->
    Row ps g (ELet [[Bind n anyType (EVar col) DefVal]] body)
  PatWild _  -> Row ps g body
  PatLit _   -> Row ps g body  -- shouldn't occur outside ctor payloads
  PatCon _ _ -> error "stripTrivialCol: called on PatCon row"
stripTrivialCol _ r@(Row [] _ _) = r

-- | Distinct constructor heads in column 0, in first-appearance order.
-- Returns @(qname, arity)@ pairs.
distinctHeadCtors :: [Row] -> [(QName, Int)]
distinctHeadCtors rows = go [] rows
  where
    go acc [] = reverse acc
    go acc (Row (PatCon qn subs : _) _ _ : rest) =
      if any ((== qn) . fst) acc
        then go acc rest
        else go ((qn, length subs) : acc) rest
    go acc (_ : rest) = go acc rest

-- | For each head constructor, specialize the matrix and recursively
-- compile the new (wider) matrix. Produces one outer 'Branch' per
-- distinct head ctor.
buildCtorBranches
  :: Fresh
  -> Name          -- column var being specialized
  -> [Name]        -- remaining column vars
  -> [(QName, Int)]
  -> [Row]
  -> ([Branch], Fresh)
buildCtorBranches fr0 col cols ctors rows =
  foldl step ([], fr0) ctors
  where
    step (acc, fr) (qn, arity) =
      let (subCols, fr1) = freshNames fr "f" arity
          specialized    = mapMaybe (specializeRow col qn arity) rows
          (body, fr2)    =
            compileMatrix fr1 (subCols ++ cols) specialized
          pat = PatCon qn [PatVar sc anyType | sc <- subCols]
      in (acc ++ [Branch pat Nothing body], fr2)

-- | Specialize a row for a particular head ctor:
--
--   * @PatCon qn subs@ with matching @qn@ → prepend @subs@ to the
--     row's pattern list.
--   * @PatVar n _@ → prepend @arity@ wildcards; also bind @n@ to the
--     column var.
--   * @PatWild _@ → prepend @arity@ wildcards.
--   * anything else → row does not contribute to this ctor group.
specializeRow :: Name -> QName -> Int -> Row -> Maybe Row
specializeRow _col qn arity (Row (PatCon qn' subs : ps) g body)
  | qn == qn' =
      -- If the ctor arity doesn't match what the row declared (would
      -- indicate an ill-typed input), prefer the declared subs and
      -- pad/truncate rather than crashing.
      let subs' = take arity (subs ++ repeat (PatWild anyType))
      in Just (Row (subs' ++ ps) g body)
  | otherwise = Nothing
specializeRow col _qn arity (Row (PatVar n _ : ps) g body) =
  let wilds = replicate arity (PatWild anyType)
      body' = ELet [[Bind n anyType (EVar col) DefVal]] body
  in Just (Row (wilds ++ ps) g body')
specializeRow _col _qn arity (Row (PatWild _ : ps) g body) =
  let wilds = replicate arity (PatWild anyType)
  in Just (Row (wilds ++ ps) g body)
specializeRow _ _ _ (Row (PatLit _ : _) _ _) = Nothing
specializeRow _ _ _ (Row [] _ _)             = Nothing

-- | The default branch receives rows whose column-0 pattern is a
-- wildcard or variable (and therefore does not commit to any
-- particular head ctor). Returns 'Nothing' if no such rows exist,
-- which tells the caller that the set of specialized ctor branches
-- is treated as exhaustive.
buildDefaultBranch
  :: Fresh -> Name -> [Name] -> [Row] -> (Maybe Branch, Fresh)
buildDefaultBranch fr col cols rows =
  let defRows = mapMaybe keepDefault rows
  in if null defRows
       then (Nothing, fr)
       else
         let (body, fr1) = compileMatrix fr cols defRows
         in (Just (Branch (PatWild anyType) Nothing body), fr1)
  where
    keepDefault (Row (PatCon _ _ : _) _ _) = Nothing
    keepDefault r = Just (stripTrivialCol col r)

-------------------------------------------------------------------------------
-- Common placeholder type
-------------------------------------------------------------------------------

-- | Placeholder type for synthesized pattern variables and let-binds.
-- The emitter only needs the tag/field layout and treats everything
-- as @i64@ at the LLVM level, so the specific 'Type' is irrelevant.
anyType :: Type
anyType =
  TCon (TypeCon (QName (T.pack "std") (Name (T.pack "any") 0)) KindValue)
