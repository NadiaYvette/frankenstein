{-# LANGUAGE OverloadedStrings #-}
-- | Side-by-side comparison of @emitLambdaLift@'s legacy
-- @countUses@ heuristic against the Perceus-faithful
-- 'Frankenstein.Core.ConsumingUses' analysis.
--
-- Driven by the @--emit-perceus-counts@ CLI flag.  Walks the
-- post-Perceus IR, finds every @ELam@ (whether let-bound or
-- expression-position), computes both counts for each free variable,
-- and tabulates the divergences.
--
-- The output is the empirical artifact Phase 12c needs before
-- swapping the heuristic out: it lets us audit where the legacy
-- count over- or under-charges per real program, and decide whether
-- the surrounding drop-insertion sites need re-tuning.
module Frankenstein.Debug.PerceusCounts
  ( analyzeProgram
  , LambdaSite(..)
  , CaptureCount(..)
  , renderReport
  , heuristicCountUses
  ) where

import Frankenstein.Core.Types
import Frankenstein.Core.ConsumingUses (consumingUseCount)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sortBy)
import Data.Ord (comparing, Down(..))

-- | One lambda site in the program, with its capture analysis.
data LambdaSite = LambdaSite
  { lsDef       :: !QName        -- ^ Enclosing top-level definition
  , lsBindName  :: !(Maybe Name) -- ^ @Just n@ when @ELam@ appears as
                                 --   the RHS of @let n = fn(...) ...@,
                                 --   @Nothing@ for expression-position
                                 --   anonymous lambdas
  , lsCaptures  :: ![CaptureCount]
  } deriving (Show, Eq)

data CaptureCount = CaptureCount
  { ccName       :: !Name
  , ccHeuristic  :: !Int  -- ^ What @emitLambdaLift@.@countUses@ reports
  , ccAnalysis   :: !Int  -- ^ What 'consumingUseCount' reports
  } deriving (Show, Eq)

-- | Walk every def in the program; for every @ELam@ encountered,
-- compute its capture set (free vars of body minus params, minus
-- top-level definition names) and the two count functions per
-- capture.  The top-level filter mirrors the emitter's
-- @emitLambdaLift@ which excludes top-level fn refs from the
-- captured-and-retained list (they're called by symbol, not boxed
-- into the closure cell).  Without this filter, the diagnostic
-- reports false-positive divergences on every reference to a
-- top-level helper, drowning the genuine closure-capture cases.
analyzeProgram :: Program -> [LambdaSite]
analyzeProgram prog =
  let topNames = Set.fromList [ qnameName (defName d) | d <- progDefs prog ]
  in concatMap (analyzeDef topNames) (progDefs prog)
  where
    analyzeDef topNames def =
      goExpr topNames (defName def) Nothing (defExpr def)

    -- @goExpr topNames enclosingDef enclosingBind expr@ — descend
    -- into @expr@, yielding one 'LambdaSite' per @ELam@ seen.  The
    -- @enclosingBind@ is the let-binding's name if we just descended
    -- through a @let f = fn(...) ...@, else 'Nothing'.
    goExpr :: Set Name -> QName -> Maybe Name -> Expr -> [LambdaSite]
    goExpr topNames def mBind expr = case expr of
      ELam ps body ->
        let captures = freeVars body
                         `Set.difference` Set.fromList (map fst ps)
                         `Set.difference` topNames
            inner = goExpr topNames def Nothing body
        in mkSite def mBind (Set.toList (Set.filter likelyLocal captures)) body
             : inner
      ELet bgs body ->
        concat
          [ goExpr topNames def (Just (bindName b)) (bindExpr b)
          | bg <- bgs, b <- bg ]
          ++ goExpr topNames def Nothing body
      EApp f as       -> goExpr topNames def Nothing f
                      ++ concatMap (goExpr topNames def Nothing) as
      ECase s brs     ->
        goExpr topNames def Nothing s
          ++ concatMap
               (\br -> goExpr topNames def Nothing (branchBody br)
                    ++ maybe [] (goExpr topNames def Nothing) (branchGuard br))
               brs
      ETypeApp e _    -> goExpr topNames def Nothing e
      ETypeLam _ e    -> goExpr topNames def Nothing e
      EPerform _ as   -> concatMap (goExpr topNames def Nothing) as
      EHandle _ h b   -> goExpr topNames def Nothing h
                      ++ goExpr topNames def Nothing b
      ERetain e       -> goExpr topNames def Nothing e
      ERelease e      -> goExpr topNames def Nothing e
      EDrop e         -> goExpr topNames def Nothing e
      EReuse a b      -> goExpr topNames def Nothing a
                      ++ goExpr topNames def Nothing b
      EDelay e        -> goExpr topNames def Nothing e
      EForce e        -> goExpr topNames def Nothing e
      _               -> []

    mkSite def mBind caps body = LambdaSite
      { lsDef      = def
      , lsBindName = mBind
      , lsCaptures =
          [ CaptureCount c
              (heuristicCountUses c body)
              (consumingUseCount c body)
          | c <- caps
          ]
      }

-- | Approximation of the emitter's "would actually be a heap capture"
-- filter, run without access to @esTopFns@/@esAliases@.  The emitter's
-- @captured@ list excludes:
--
--   * Top-level fn refs (handled by the @topNames@ subtraction above).
--   * Unqualified runtime helpers like @<@, @kk_*@ — these enter the
--     IR with @nameUnique == 0@.
--   * Qualified external symbols like @Data.Text.Internal/pack@ —
--     the GHC bridge emits these as @EVar (Name "Mod/sym" u)@; their
--     text contains a forward slash (the module-qualifier separator).
--
-- These three exclusions cover the noise classes seen on the
-- self-host modules; remaining divergences are local pattern vars or
-- let-bound names whose count actually feeds @kk_retain@ emission.
likelyLocal :: Name -> Bool
likelyLocal n =
  nameUnique n /= 0
    && not (T.any (== '/') (nameText n))

-- | The 'countUses' currently inlined in
-- @Frankenstein.MlirEmit.Emitter.emitLambdaLift@.  Cloned here
-- verbatim so step 2 can compare it side-by-side without taking a
-- dependency from a debug helper into the emitter.  Will be removed
-- once Phase 12c step 4 retires the heuristic.
heuristicCountUses :: Name -> Expr -> Int
heuristicCountUses tgt = go
  where
    tgtT = nameText tgt
    go (EVar n)         = if nameText n == tgtT then 1 else 0
    go (ELit _)         = 0
    go (ECon _)         = 0
    go (EFunRef _)      = 0
    go (EApp f as)      = go f + sum (map go as)
    go (ELam _ b)       = go b
    go (ELet bgs b)     =
      go b + sum [go (bindExpr bd) | bg <- bgs, bd <- bg]
    go (ECase s brs)    =
      go s + maximum (0 : [go (branchBody br) | br <- brs])
    go (ERetain _)      = 0
    go (ERelease _)     = 0
    go (EDrop e)        = go e
    go (EReuse a b)     = go a + go b
    go (EDelay e)       = go e
    go (EForce e)       = go e
    go (ETypeApp e _)   = go e
    go (ETypeLam _ e)   = go e
    go (EPerform _ as)  = sum (map go as)
    go (EHandle _ h b)  = go h + go b

-- | Free variables of an expression, respecting binders.  Inlined
-- here (rather than importing from 'Frankenstein.Core.Perceus') so
-- this debug module has no cross-package surface beyond
-- 'ConsumingUses'.
freeVars :: Expr -> Set Name
freeVars (EVar n)         = Set.singleton n
freeVars (ELit _)         = Set.empty
freeVars (ECon _)         = Set.empty
freeVars (EFunRef _)      = Set.empty
freeVars (EApp f args)    = Set.unions (freeVars f : map freeVars args)
freeVars (ELam ps body)   = freeVars body `Set.difference` Set.fromList (map fst ps)
freeVars (ELet bgs body)  =
  let bound   = Set.fromList [bindName b | bg <- bgs, b <- bg]
      bindFvs = Set.unions [freeVars (bindExpr b) | bg <- bgs, b <- bg]
  in (bindFvs `Set.union` freeVars body) `Set.difference` bound
freeVars (ECase s brs)    =
  Set.union (freeVars s) (Set.unions (map branchFreeVars brs))
freeVars (ETypeApp e _)   = freeVars e
freeVars (ETypeLam _ e)   = freeVars e
freeVars (EPerform _ as)  = Set.unions (map freeVars as)
freeVars (EHandle _ h b)  = Set.union (freeVars h) (freeVars b)
freeVars (ERetain e)      = freeVars e
freeVars (ERelease e)     = freeVars e
freeVars (EDrop e)        = freeVars e
freeVars (EReuse a b)     = Set.union (freeVars a) (freeVars b)
freeVars (EDelay e)       = freeVars e
freeVars (EForce e)       = freeVars e

branchFreeVars :: Branch -> Set Name
branchFreeVars br =
  let patBound = Set.fromList (patternNames (branchPattern br))
      guardFvs = maybe Set.empty freeVars (branchGuard br)
  in (freeVars (branchBody br) `Set.union` guardFvs)
       `Set.difference` patBound

patternNames :: Pattern -> [Name]
patternNames p = case p of
  PatVar n _    -> [n]
  PatCon _ subs -> concatMap patternNames subs
  PatWild _     -> []
  PatLit _      -> []

-- | Render a human-readable report from the per-lambda analysis.
-- Default mode summarizes: per (def, lambda) site, list the
-- captures whose heuristic and analysis counts disagree.  Sites
-- with no divergences are dropped to keep the output focused on
-- the audit targets.
renderReport :: [LambdaSite] -> Text
renderReport sites =
  let divergentSites =
        [ site { lsCaptures = divergent }
        | site <- sites
        , let divergent = filter isDivergent (lsCaptures site)
        , not (null divergent)
        ]
      totalDivergences = sum (map (length . lsCaptures) divergentSites)
      totalSites       = length sites
      totalDivergentSites = length divergentSites
      header =
        [ "=== Perceus consuming-use counts (heuristic vs. analysis) ==="
        , ""
        , "Walked " <> T.pack (show totalSites)
            <> " ELam site(s); "
            <> T.pack (show totalDivergentSites)
            <> " have at least one divergent capture ("
            <> T.pack (show totalDivergences)
            <> " divergent capture(s) total)."
        , ""
        ]
      body =
        concatMap renderSite (sortBy (comparing siteOrder) divergentSites)
  in T.unlines (header ++ body)
  where
    isDivergent cc = ccHeuristic cc /= ccAnalysis cc

    -- Sort sites by largest divergence-magnitude first so the
    -- worst offenders are at the top of the output.
    siteOrder site =
      let worst = maximum
            (0 : [ abs (ccHeuristic cc - ccAnalysis cc)
                 | cc <- lsCaptures site ])
      in Down worst

    renderSite site =
      let defText  = qnameModule (lsDef site) <> "."
                   <> nameText (qnameName (lsDef site))
          bindText = case lsBindName site of
                       Just n  -> "let "  <> nameText n
                       Nothing -> "anon"
          captures = sortBy (comparing captureOrder) (lsCaptures site)
      in [ defText <> "  /  " <> bindText ]
         ++ [ "    "
              <> nameText (ccName cc)
              <> "  heuristic=" <> T.pack (show (ccHeuristic cc))
              <> "  analysis="  <> T.pack (show (ccAnalysis cc))
              <> divergenceTag cc
            | cc <- captures
            ]
         ++ [ "" ]

    captureOrder cc = Down (abs (ccHeuristic cc - ccAnalysis cc))

    divergenceTag cc
      | ccHeuristic cc > ccAnalysis cc = "   [OVER-COUNT by "
            <> T.pack (show (ccHeuristic cc - ccAnalysis cc))
            <> "]"
      | ccHeuristic cc < ccAnalysis cc = "   [UNDER-COUNT by "
            <> T.pack (show (ccAnalysis cc - ccHeuristic cc))
            <> "]"
      | otherwise                      = ""
