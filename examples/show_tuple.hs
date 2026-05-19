module ShowTuple where

-- Demonstrates that the Show instance for tuples works through the
-- GHC bridge for the common shapes:
--   - 2-tuples
--   - n-tuples (3, 4, 5, ...)
--   - tuples with negative numbers
--   - nested tuples
--
-- GHC's Show specialiser emits a chain of comma-separated showFn
-- applications through `$fShowCallStack_$sgo`, whose second arg is
-- a static cons-list of show closures.  The bridge unrolls this
-- chain at translation time via `expandTupleShowChain` so all
-- elements appear in the output.
--
-- Numbered CAFs from GHC.Internal.Show are inlined via
-- knownShowCharCAF / knownShowCAF:
--   showList__1 = ','   showList__2 = ']'   showList__3 = '['
--   showList__4 = "[]"  $fShowCallStack8 = '-' (minus sign)
-- The Show-class dictionary's never-forced showList field is
-- substituted with `dummy_show_caf` so the dict structure links cleanly.

main :: IO ()
main = do
  print ((1, 2) :: (Int, Int))
  print ((1, 2, 3) :: (Int, Int, Int))
  print ((-5, 100) :: (Int, Int))
  print ((42, 0, -7, 13) :: (Int, Int, Int, Int))
  print (((7, 13), 42) :: ((Int, Int), Int))
