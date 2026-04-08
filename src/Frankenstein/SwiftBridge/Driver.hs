-- | Shell out to @swiftc -dump-ast@ and return the parsed AST tree,
-- plus a separate entry point for running @swiftc -O -emit-sil@ and
-- counting strong_retain / strong_release instructions so we can
-- cross-check Perceus against swiftc's ARC decisions on the same
-- source file.
module Frankenstein.SwiftBridge.Driver
  ( readSwiftFile
  , emitSilCounts
  , SilCounts(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Frankenstein.SwiftBridge.AstParse

-- | Invoke @swiftc -dump-ast@ on the given file and parse the result.
readSwiftFile :: FilePath -> IO (Either Text Node)
readSwiftFile path = do
  (ec, out, err) <- readProcessWithExitCode "swiftc" ["-dump-ast", path] ""
  -- swiftc 6.x writes the dumped AST to stdout. Fall back to stderr in
  -- case an older toolchain ever sends it there.
  let text = if null out then err else out
  case ec of
    ExitSuccess   -> pure (parseSwiftAst (T.pack text))
    ExitFailure _ -> case parseSwiftAst (T.pack text) of
      Right n  -> pure (Right n)
      Left _pe -> pure (Left ("swiftc -dump-ast failed:\n" <> T.pack err))

-- | Counts of reference-count operations swiftc emits in its SIL
-- output. Used to cross-check Frankenstein's Perceus pass.
data SilCounts = SilCounts
  { silStrongRetain  :: !Int
  , silStrongRelease :: !Int
  , silCopyValue     :: !Int
  , silDestroyValue  :: !Int
  } deriving (Show, Eq)

-- | Run @swiftc -O -emit-sil@ and count RC operations.
emitSilCounts :: FilePath -> IO (Either Text SilCounts)
emitSilCounts path = do
  (ec, out, err) <- readProcessWithExitCode "swiftc" ["-O", "-emit-sil", path] ""
  let text = T.pack (out <> err)
      count needle = length (T.breakOnAll needle text)
  case ec of
    ExitFailure _ -> pure (Left ("swiftc -emit-sil failed:\n" <> T.pack err))
    ExitSuccess   -> pure $ Right SilCounts
      { silStrongRetain  = count "strong_retain"
      , silStrongRelease = count "strong_release"
      , silCopyValue     = count "copy_value"
      , silDestroyValue  = count "destroy_value"
      }
