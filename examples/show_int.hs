module ShowInt where

-- Demonstrates that `show :: Int -> String` works through the bridge.
-- GHC simplifies `show 42` and `print 42` to calls of either the
-- $w$cshowsPrec2 worker (with precedence) or the $fShowCallStack_itos'
-- helper (without).  Both are intercepted in
-- Frankenstein.GhcBridge.CoreTranslate (isShowIntWorker + pickShowArgs)
-- and rewritten to a runtime int_to_haskell_chars call that builds the
-- decimal cons-list directly.  The IS / I# constructor wrapping the
-- value is peeled at translation time by unboxIntCon.

main :: IO ()
main = do
  print 0
  print 42
  print (-7)
  putStrLn (show ((20 * 21) `div` 2 :: Int))
