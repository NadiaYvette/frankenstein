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
      putStrLn "Frankenstein.Funnel.MVP tests passed."
