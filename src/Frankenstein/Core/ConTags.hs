-- | Per-program constructor tag assignment.
--
-- Assigns a deterministic integer tag to every constructor referenced in
-- a 'Program'.  Tags are computed by hashing the bare constructor name
-- (djb2 variant, mod 65521).  This makes tags /stable across modules/:
-- every compilation unit that references the same constructor gets the
-- same tag, which is critical for the self-hosted pipeline where values
-- constructed in one module (e.g. Consumer) are pattern-matched in
-- another (e.g. Perceus, Emitter).
--
-- The output is a @Map Text Int@ keyed on the bare constructor name
-- (@nameText . qnameName@), because bridges routinely emit
-- constructors with an empty module part and the intra-type dispatch
-- story means module qualification is not required for correctness.
module Frankenstein.Core.ConTags
  ( assignProgramTags
  , stableConTag
  , collectReferencedCtors
  , conKey
  ) where

import Frankenstein.Core.Types

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T

-- | The key used to look up a constructor in the tag table. Bridges
-- emit constructors inconsistently with respect to module qualification,
-- so we key on the bare name.
conKey :: QName -> Text
conKey qn = nameText (qnameName qn)

-- | Deterministic hash of a constructor name to a stable tag.
-- Uses a djb2-variant hash mod 65521 (largest prime < 2^16).
-- Every module that references the same constructor name gets the
-- same tag, enabling cross-module pattern matching in the self-hosted
-- pipeline.
stableConTag :: Text -> Int
stableConTag = (`mod` 65521) . abs . T.foldl' step 5381
  where step acc c = acc * 33 + fromEnum c

-- | Walk a 'Program' and assign a deterministic tag to every
-- constructor name referenced by 'DataDecl's, 'ECon' nodes, or
-- 'PatCon' patterns.  Tags are computed by 'stableConTag' (hash-based),
-- so they are stable across independently compiled modules.
assignProgramTags :: Program -> Map Text Int
assignProgramTags prog =
  let referenced = collectReferencedCtors prog
  in Map.fromList [(k, stableConTag k) | k <- Set.toList referenced]

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
ctorsInExpr expr = go expr
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
