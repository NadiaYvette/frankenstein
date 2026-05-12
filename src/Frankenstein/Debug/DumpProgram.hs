-- | Debug helper: serialize a 'Program' to a deterministic textual form for
-- per-pass differential comparison between the host (GHC-compiled) and the
-- self-hosted (Frankenstein-compiled) compiler binaries. The goal is to
-- localize runtime divergence to a specific pass: if both binaries produce
-- identical dumps after pass N but differ after pass N+1, the bug is in
-- pass N+1.
--
-- Uses Haskell's derived 'Show' on the Core types, which is deterministic
-- (modulo Map.toList ordering, which is also deterministic by key).
module Frankenstein.Debug.DumpProgram
  ( dumpProgram
  ) where

import qualified Data.Text as T
import Frankenstein.Core.Types

dumpProgram :: Program -> T.Text
dumpProgram = T.pack . show
