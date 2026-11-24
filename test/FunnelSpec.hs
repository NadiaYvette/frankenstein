{-# LANGUAGE OverloadedStrings, DataKinds, TypeApplications, TypeOperators, MagicHash #-}
module Main (main) where

import System.Exit (exitFailure)
import Frankenstein.Funnel.MVP
import OrganBank.Plugin (LanguagePlugin(..))
import Kogaki.Text.VirtualCharList (packUTF8Literal)
import Penelope.Backend.Plugin (defaultHostTargetLayout)
import Penelope.Perceus (RTSProfile(..))
import Polytropos.RowMorph (unsafeMkRowMorph)
import Polytropos.Core (Pair(..), Rep(..), RowRep(..))
import Data.Primitive.SmallArray (smallArrayFromList)
import Penelope.Backend.MLIR (mlirBackendPlugin, SSAVal(..))
import Penelope.Core.Syntax (Penelope(..))
import GHC.Exts (unsafeCoerce#)
import Frankenstein.RTS.UringLower
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn "Testing Frankenstein.Funnel.MVP..."
  let rope = packUTF8Literal "module Main where\nmain = putStrLn \"hello\""
  case lpIngressParse haskellMVPPlugin rope of
    Left err -> putStrLn ("Haskell ingress parse failed: " ++ show err) >> exitFailure
    Right ast -> do
      putStrLn ("Ingress parsed AST: " ++ show ast)
      let morph = unsafeMkRowMorph []
          term :: Penelope '[] '[] '["x" ':= Int] '["x" ':= Int]
          term = Route morph
          initFrame = RowRep (Rep 0 (smallArrayFromList [unsafeCoerce# (SSAVal "%arg0")]))
          soa = compileSeparateObjectABI "Main" defaultHostTargetLayout RTSFreestandingLinear morph mlirBackendPlugin initFrame term
      putStrLn ("Emitted C-Header:\n" ++ show (soaCHeader soa))
      putStrLn "Testing Frankenstein.RTS.UringLower..."
      let sqe0 = UringSqe OpWriteFixed 3 "%buf0" 4096 0 True
          sqe1 = UringSqe OpFsync 3 "%buf0" 0 0 False
          batch0 = UringBatch "batch0" Profile0FreestandingLinear [sqe0, sqe1] "^bb_ok" "^bb_eof" "^bb_canceled" "^bb_err"
          lowered0 = lowerUringBatch batch0
      if not ("profile = #organ<freestanding_linear>" `T.isInfixOf` lowered0)
        then putStrLn "Profile 0 test failed" >> exitFailure
        else putStrLn "Profile 0 verified."

      let batch1 = batch0 { batchProfile = Profile1ThreadLocalPerceus }
          lowered1 = lowerUringBatch batch1
      if not ("profile = #organ<thread_local_perceus>" `T.isInfixOf` lowered1)
        then putStrLn "Profile 1 test failed" >> exitFailure
        else putStrLn "Profile 1 verified."

      let batch2 = batch0 { batchProfile = Profile2ConcurrentShared }
          lowered2 = lowerUringBatch batch2
      if not ("profile = #organ<concurrent_shared>" `T.isInfixOf` lowered2)
        then putStrLn "Profile 2 test failed" >> exitFailure
        else putStrLn "Profile 2 verified."
      putStrLn "Frankenstein.Funnel.MVP and UringLower tests passed."
