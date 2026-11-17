#!/usr/bin/env bash
set -euo pipefail

echo "=== [TP-007E] Deploying Frankenstein.Funnel.MVP ==="

mkdir -p src/Frankenstein/Funnel
mkdir -p test

# 1. Mount seed
cp -f /home/nyc/src/plans/seeds/TP-007E-FrankensteinMVP.hs src/Frankenstein/Funnel/MVP.hs

# 2. Update frankenstein.cabal
python3 -c '
with open("frankenstein.cabal", "r") as f:
    content = f.read()

if "Frankenstein.Funnel.MVP" not in content:
    content = content.replace("        Frankenstein.OrganIR.Emitter\n", "        Frankenstein.OrganIR.Emitter\n        Frankenstein.Funnel.MVP\n")

deps = """        ghc == 9.14.*,
        ghc-boot-th,
        koka,
        organ-ir,
        primitive,
        polytropos-core >= 0.1 && < 0.2,
        penelope-core >= 0.1 && < 0.2,
        kogaki-core >= 0.1 && < 0.2"""

if "polytropos-core" not in content:
    content = content.replace("""        ghc == 9.14.*,
        ghc-boot-th,
        koka,
        organ-ir""", deps)

funnel_test = """
test-suite funnel-tests
    import:           warnings
    type:             exitcode-stdio-1.0
    main-is:          FunnelSpec.hs
    hs-source-dirs:   test
    build-depends:
        base >= 4.17 && < 5,
        frankenstein,
        penelope-core,
        penelope-backend-mlir,
        organ-ir,
        kogaki-core,
        polytropos-core,
        primitive,
        text
    default-language: GHC2024
"""

if "test-suite funnel-tests" not in content:
    content = content.replace("test-suite frankenstein-tests", funnel_test + "\ntest-suite frankenstein-tests")

with open("frankenstein.cabal", "w") as f:
    f.write(content)
'

# 3. Create test/FunnelSpec.hs
cat << 'EOF' > test/FunnelSpec.hs
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
EOF

echo "=== [TP-007E] Building and testing ==="
cabal v2-build lib:frankenstein -j20 --semaphore
cabal v2-test frankenstein:test:funnel-tests

echo "=== [TP-007E] Script execution complete ==="
