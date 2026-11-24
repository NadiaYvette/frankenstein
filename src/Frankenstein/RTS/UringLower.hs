{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Module: Frankenstein.RTS.UringLower
-- Description: OrganIR / MLIR lowering hook for pervasive io_uring batches with Perceus 3-profile ownership
--
-- Lowers fused SQE submission batches into textual @organ.uring_batch@ operations
-- with explicit Perceus 3-profile buffer ownership tags and 4-way continuation branch exits:
--   - ^bb_ok: successful completion (res >= 0)
--   - ^bb_eof: zero-byte EOF / peer shutdown
--   - ^bb_canceled: cancellation safety (-ECANCELED / -ETIME)
--   - ^bb_err: kernel error code (-res)
module Frankenstein.RTS.UringLower
  ( OrganMemProfile(..)
  , profileTag
  , profileMlirAttr
  , UringOpCode(..)
  , opCodeName
  , UringSqe(..)
  , UringBatch(..)
  , renderUringBatchOp
  , lowerUringBatch
  ) where

import qualified Data.Bits as Bits
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)

-- | 3-Profile Memory Model for OrganIR / Perceus DMA buffers.
data OrganMemProfile
  = Profile0FreestandingLinear  -- ^ Multiplicity = 1; 0 RC header, linear move
  | Profile1ThreadLocalPerceus  -- ^ Thread-local Perceus RC (is_mt elided)
  | Profile2ConcurrentShared    -- ^ Atomic Release/Acquire RC (gpfsl Arc)
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Canonical short tag for textual representation.
profileTag :: OrganMemProfile -> Text
profileTag Profile0FreestandingLinear = "freestanding_linear"
profileTag Profile1ThreadLocalPerceus = "thread_local_perceus"
profileTag Profile2ConcurrentShared   = "concurrent_shared"

-- | MLIR attribute representation matching OrganIR spec (#organ<profile>).
profileMlirAttr :: OrganMemProfile -> Text
profileMlirAttr p = "#organ<" <> profileTag p <> ">"

-- | io_uring standard submission opcode subset supported by Penelope and Frankenstein.
data UringOpCode
  = OpNop
  | OpReadv
  | OpWritev
  | OpReadFixed
  | OpWriteFixed
  | OpFsync
  | OpTimeout
  | OpLinkTimeout
  | OpRecvMultishot
  | OpSendZc
  | OpMsgRing
  deriving stock (Eq, Ord, Show, Enum, Bounded)

opCodeName :: UringOpCode -> Text
opCodeName OpNop           = "IORING_OP_NOP"
opCodeName OpReadv         = "IORING_OP_READV"
opCodeName OpWritev        = "IORING_OP_WRITEV"
opCodeName OpReadFixed     = "IORING_OP_READ_FIXED"
opCodeName OpWriteFixed    = "IORING_OP_WRITE_FIXED"
opCodeName OpFsync         = "IORING_OP_FSYNC"
opCodeName OpTimeout       = "IORING_OP_TIMEOUT"
opCodeName OpLinkTimeout   = "IORING_OP_LINK_TIMEOUT"
opCodeName OpRecvMultishot = "IORING_OP_RECV_MULTISHOT"
opCodeName OpSendZc        = "IORING_OP_SEND_ZC"
opCodeName OpMsgRing       = "IORING_OP_MSG_RING"

-- | Single SQE descriptor within a fused batch.
data UringSqe = UringSqe
  { sqeOpCode   :: !UringOpCode
  , sqeFd       :: !Int
  , sqeBufRef   :: !Text    -- ^ SSA value or buffer label (e.g. "%buf0")
  , sqeBytes    :: !Word32
  , sqeOffset   :: !Word64
  , sqeIsLinked :: !Bool    -- ^ Sets IOSQE_IO_LINK in kernel
  } deriving stock (Eq, Show)

-- | Fused SQE batch with 4-way continuation branching and Perceus ownership.
data UringBatch = UringBatch
  { batchId              :: !Text
  , batchProfile         :: !OrganMemProfile
  , batchSqes            :: ![UringSqe]
  , batchSuccessorOk     :: !Text  -- ^ Successor label: ^bb_ok
  , batchSuccessorEof    :: !Text  -- ^ Successor label: ^bb_eof
  , batchSuccessorCancel :: !Text  -- ^ Successor label: ^bb_canceled
  , batchSuccessorErr    :: !Text  -- ^ Successor label: ^bb_err
  } deriving stock (Eq, Show)

-- | Compute io_link_mask bitmask from SQE sequence.
computeLinkMask :: [UringSqe] -> Word32
computeLinkMask sqes =
  foldr (\(idx, s) acc -> if sqeIsLinked s then acc Bits..|. (1 `Bits.shiftL` idx) else acc) 0 (zip [0..] sqes)

-- | Render a single SQE in textual format.
renderSqe :: UringSqe -> Text
renderSqe s =
  "{ op = " <> opCodeName (sqeOpCode s)
  <> ", fd = " <> T.pack (show (sqeFd s))
  <> ", buf = " <> sqeBufRef s
  <> ", len = " <> T.pack (show (sqeBytes s))
  <> ", off = " <> T.pack (show (sqeOffset s))
  <> (if sqeIsLinked s then ", flags = [IOSQE_IO_LINK]" else "")
  <> " }"

-- | Renders the textual organ.uring_batch operation.
renderUringBatchOp :: UringBatch -> Text
renderUringBatchOp b =
  let linkMask = computeLinkMask (batchSqes b)
      renderedSqes = "[" <> T.intercalate ", " (map renderSqe (batchSqes b)) <> "]"
  in "organ.uring_batch {\n"
     <> "  profile = " <> profileMlirAttr (batchProfile b) <> ",\n"
     <> "  io_link_mask = " <> T.pack (show linkMask) <> " : i32,\n"
     <> "  sqes = " <> renderedSqes <> "\n"
     <> "} [" <> batchSuccessorOk b <> ", "
              <> batchSuccessorEof b <> ", "
              <> batchSuccessorCancel b <> ", "
              <> batchSuccessorErr b <> "]"

-- | Lower a UringBatch to textual OrganIR / MLIR.
lowerUringBatch :: UringBatch -> Text
lowerUringBatch = renderUringBatchOp
