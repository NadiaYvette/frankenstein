-- | Per-program constructor tag assignment.
--
-- Assigns a deterministic integer tag to every constructor referenced in
-- a 'Program'. The assignment has two phases:
--
-- 1. For each 'DataDecl' in 'progData', constructors are numbered
--    @0..n-1@ in declaration order within that type. Intra-type case
--    dispatch only needs to distinguish ctors of a single type, so
--    different types may safely share low tag values.
--
-- 2. Constructors that appear in 'ECon' / 'PatCon' nodes but are
--    /not/ declared in any 'DataDecl' (e.g. synthetic ctors from a
--    bridge, or built-in ctors injected directly by the front-end)
--    are assigned fresh sequential tags starting after the largest
--    tag already used. This guarantees a total function from
--    constructor name to tag with no hashing and no collisions.
--
-- The output is a @Map Text Int@ keyed on the bare constructor name
-- (@nameText . qnameName@), because bridges routinely emit
-- constructors with an empty module part and the intra-type dispatch
-- story means module qualification is not required for correctness.
module Frankenstein.Core.ConTags
  ( assignProgramTags
  , collectReferencedCtors
  , conKey
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | The key used to look up a constructor in the tag table. Bridges
-- emit constructors inconsistently with respect to module qualification,
-- so we key on the bare name.
conKey :: QName -> Text
conKey qn = nameText (qnameName qn)

-- | Walk a 'Program' and assign a deterministic tag to every
-- constructor name referenced by 'DataDecl's, 'ECon' nodes, or
-- 'PatCon' patterns. Guaranteed total: every ctor name the emitter
-- can encounter via the usual walk is present in the returned map.
assignProgramTags :: Program -> Map Text Int
assignProgramTags prog =
  let -- Phase 1: declared ctors, numbered 0..n-1 within each DataDecl.
      declared :: Map Text Int
      declared = Map.fromList
        [ (conKey (conName cd), i)
        | dd <- progData prog
        , (i, cd) <- zip [0..] (dataCons dd)
        ]

      -- Phase 2: orphan ctors (referenced but not declared).
      referenced :: Set Text
      referenced = collectReferencedCtors prog

      orphans :: [Text]
      orphans =
        [ k
        | k <- Set.toAscList referenced
        , not (Map.member k declared)
        ]

      -- Start orphan numbering after the largest declared tag, so that
      -- orphans never collide with declared ctors even if someone ever
      -- compares tags across types (they shouldn't, but we're paranoid).
      startOrphan :: Int
      startOrphan = case Map.elems declared of
        [] -> 0
        xs -> maximum xs + 1

      orphanMap :: Map Text Int
      orphanMap = Map.fromList (zip orphans [startOrphan ..])

  in declared `Map.union` orphanMap

-- | Gather the set of constructor names referenced by a program.
-- Scans both 'DataDecl's (so declared ctors always appear, even if
-- unused) and every 'Expr' tree reachable from 'progDefs'.
collectReferencedCtors :: Program -> Set Text
collectReferencedCtors prog =
  let fromData = Set.fromList
        [ conKey (conName cd)
        | dd <- progData prog
        , cd <- dataCons dd
        ]
      fromDefs = Set.unions (map (ctorsInExpr . defExpr) (progDefs prog))
  in fromData `Set.union` fromDefs

-- | Recursively collect every constructor name referenced by an
-- expression — both as 'ECon' / 'EApp (ECon _) _' and inside
-- 'PatCon' patterns.
ctorsInExpr :: Expr -> Set Text
ctorsInExpr = go
  where
    go (ECon qn)           = Set.singleton (conKey qn)
    go (EVar _)            = Set.empty
    go (ELit _)            = Set.empty
    go (EFunRef _)         = Set.empty
    go (EApp f args)       = Set.unions (go f : map go args)
    go (ELam _ body)       = go body
    go (ELet bgs body)     =
      Set.unions (go body : [go (bindExpr b) | bg <- bgs, b <- bg])
    go (ECase scrut bs)    =
      Set.unions (go scrut : map branch bs)
    go (ETypeApp e _)      = go e
    go (ETypeLam _ e)      = go e
    go (EPerform _ args)   = Set.unions (map go args)
    go (EHandle _ h b)     = go h `Set.union` go b
    go (ERetain e)         = go e
    go (ERelease e)        = go e
    go (EDrop e)           = go e
    go (EReuse e1 e2)      = go e1 `Set.union` go e2
    go (EDelay e)          = go e
    go (EForce e)          = go e

    branch (Branch pat mg body) =
      Set.unions [pat' pat, maybe Set.empty go mg, go body]

    pat' (PatCon qn subs) =
      Set.insert (conKey qn) (Set.unions (map pat' subs))
    pat' (PatVar _ _)     = Set.empty
    pat' (PatWild _)      = Set.empty
    pat' (PatLit _)       = Set.empty
