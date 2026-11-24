{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Module: Frankenstein.Db.Incremental
-- Description: Salsa-style pull-based durable revisions, early-cutoff memoization, and MVCC CodeDb
--
-- Provides the pure MVCC incremental computation engine behind Ingolstadt's
-- code-in-database compiler and IDE: 3-tier durability barriers skip checking
-- cold stdlib/driver subgraphs in O(1), while content-hash early cutoff halts
-- downstream invalidation whenever a declaration signature hash stays unchanged.
module Frankenstein.Db.Incremental
  ( Revision(..)
  , Durability(..)
  , QueryKey(..)
  , InputCell(..)
  , MemoEntry(..)
  , CodeDb(..)
  , emptyCodeDb
  , lastRevAtDurability
  , setInput
  , isEntryVerified
  , recordMemoWithCutoff
  , runTrackedQuery
  , runTrackedQueryWithStack
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Frankenstein.Core.Interface (fnv1a64)

-- | Monotonically increasing global database revision.
newtype Revision = Revision { unRevision :: Word64 }
  deriving stock (Eq, Ord, Show)

-- | Salsa-style durability tiers separating hot active edits from cold stdlib/drivers.
data Durability
  = DurabilityLow
    -- ^ Active buffer being edited interactively.
  | DurabilityMedium
    -- ^ Sibling modules in the current workspace.
  | DurabilityHigh
    -- ^ Read-only stdlib interfaces and @drivers/*.dhall@ manifests.
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Typed cache key for incremental compiler & IDE queries.
data QueryKey
  = QSourceText !FilePath
  | QParseModule !FilePath
  | QDeclSignature !FilePath !Text
  | QLowerOrgan !FilePath !Text
  | QSheafEdge !Text
  deriving stock (Eq, Ord, Show)

-- | Versioned input cell stored in the MVCC database.
data InputCell = InputCell
  { icValue       :: !Text
  , icContentHash :: {-# UNPACK #-} !Word64
  , icDurability  :: !Durability
  , icChangedAt   :: !Revision
  } deriving stock (Eq, Show)

-- | Cached derived query output with verification/change stamps and dependency list.
data MemoEntry = MemoEntry
  { meValue      :: !Text
  , meValueHash  :: {-# UNPACK #-} !Word64
  , meDurability :: !Durability
  , meVerifiedAt :: !Revision
  , meChangedAt  :: !Revision
  , meDeps       :: ![QueryKey]
  } deriving stock (Eq, Show)

-- | Pure immutable MVCC snapshot of Ingolstadt's incremental compiler database.
data CodeDb = CodeDb
  { dbRevision       :: !Revision
  , dbDurabilityRevs :: !(Map Durability Revision)
  , dbInputs         :: !(Map QueryKey InputCell)
  , dbMemos          :: !(Map QueryKey MemoEntry)
  } deriving stock (Eq, Show)

-- | Initial empty database at @Revision 1@.
emptyCodeDb :: CodeDb
emptyCodeDb =
  let r0 = Revision 1
  in CodeDb
       { dbRevision       = r0
       , dbDurabilityRevs =
           Map.fromList
             [ (DurabilityLow, r0)
             , (DurabilityMedium, r0)
             , (DurabilityHigh, r0)
             ]
       , dbInputs         = Map.empty
       , dbMemos          = Map.empty
       }

-- | Returns the latest 'Revision' at which any input of durability @>= d@ changed.
lastRevAtDurability :: Durability -> CodeDb -> Revision
lastRevAtDurability d db =
  Map.findWithDefault (dbRevision db) d (dbDurabilityRevs db)

-- | Updates an input cell; performs input-level early cutoff if the content hash is unchanged.
setInput :: QueryKey -> Durability -> Text -> CodeDb -> CodeDb
setInput key dur newVal db =
  let !newHash = fnv1a64 newVal
  in case Map.lookup key (dbInputs db) of
       Just old | icContentHash old == newHash && icDurability old == dur -> db
       _ ->
         let !nextRev = Revision (unRevision (dbRevision db) + 1)
             updatedDur =
               Map.mapWithKey
                 (\tier prevR -> if tier <= dur then nextRev else prevR)
                 (dbDurabilityRevs db)
             cell = InputCell newVal newHash dur nextRev
         in db
              { dbRevision       = nextRev
              , dbDurabilityRevs = updatedDur
              , dbInputs         = Map.insert key cell (dbInputs db)
              }

-- | O(1) Salsa durability fast-path checking whether a cached 'MemoEntry' is guaranteed valid.
isEntryVerified :: MemoEntry -> CodeDb -> Bool
isEntryVerified entry db =
  meVerifiedAt entry >= dbRevision db
    || meVerifiedAt entry >= lastRevAtDurability (meDurability entry) db

-- | Stores a recomputed memo entry with content-hash early cutoff: if the new hash
-- matches the previous hash, @meChangedAt@ is preserved so downstream dependents skip recomputing.
recordMemoWithCutoff
  :: QueryKey
  -> Durability
  -> [QueryKey]
  -> Text
  -> CodeDb
  -> (Bool, CodeDb)
recordMemoWithCutoff key dur deps newVal db =
  let !newHash = fnv1a64 newVal
      !curRev  = dbRevision db
      (changedAt, hitEarlyCutoff) =
        case Map.lookup key (dbMemos db) of
          Just old | meValueHash old == newHash -> (meChangedAt old, True)
          _                                     -> (curRev, False)
      entry = MemoEntry
        { meValue      = newVal
        , meValueHash  = newHash
        , meDurability = dur
        , meVerifiedAt = curRev
        , meChangedAt  = changedAt
        , meDeps       = deps
        }
  in (hitEarlyCutoff, db { dbMemos = Map.insert key entry (dbMemos db) })

-- | Pull-based query evaluator with cycle detection and early-cutoff caching.
runTrackedQuery
  :: QueryKey
  -> Durability
  -> (CodeDb -> Either Text (Text, [QueryKey], CodeDb))
  -> CodeDb
  -> Either Text (Text, Bool, CodeDb)
runTrackedQuery = runTrackedQueryWithStack []

-- | Internal worker for 'runTrackedQuery' carrying an active query cycle stack.
runTrackedQueryWithStack
  :: [QueryKey]
  -> QueryKey
  -> Durability
  -> (CodeDb -> Either Text (Text, [QueryKey], CodeDb))
  -> CodeDb
  -> Either Text (Text, Bool, CodeDb)
runTrackedQueryWithStack stack key dur computeFn db
  | key `elem` stack = Left ("cycle detected: " <> T.pack (show (key : stack)))
  | otherwise =
      case Map.lookup key (dbMemos db) of
        Just entry | isEntryVerified entry db ->
          -- O(1) durability fast-path: fresh without inspecting dependencies
          Right (meValue entry, True, db)
        Just entry ->
          -- Check whether any recorded dependency has changed since this entry was verified
          let anyDepChanged = any (isDepChanged (meVerifiedAt entry)) (meDeps entry)
          in if not anyDepChanged
               then
                 -- Dependencies unchanged: refresh meVerifiedAt to current revision
                 let refreshed = entry { meVerifiedAt = dbRevision db }
                     updatedDb = db { dbMemos = Map.insert key refreshed (dbMemos db) }
                 in Right (meValue entry, True, updatedDb)
               else doRecompute
        Nothing -> doRecompute
  where
    isDepChanged verifiedRev depKey =
      case Map.lookup depKey (dbInputs db) of
        Just ic -> icChangedAt ic > verifiedRev
        Nothing ->
          case Map.lookup depKey (dbMemos db) of
            Just me -> meChangedAt me > verifiedRev
            Nothing -> True -- Unknown dependency treated as changed

    doRecompute = do
      (newVal, deps, dbAfterCompute) <- computeFn db
      let (_cutoff, finalDb) = recordMemoWithCutoff key dur deps newVal dbAfterCompute
      Right (newVal, False, finalDb)
