module ShowTuple where

-- Demonstrates that the Show instance for `(Int, Int)` works through the
-- GHC bridge.  GHC's Show (a, b) specialiser routes the format through
--   "(" ++ showsPrec 0 a ("," ++ showsPrec 0 b ")")
-- using the helpers:
--   - showList__1 (','), the inter-element separator — handled by
--     knownShowCharCAF (Frankenstein.GhcBridge.CoreTranslate).
--   - $fShowCallStack_$sgo, a 3-arg `showFn _ tail → showFn tail`
--     applier — handled by isShowTupleSgo in the same module.
--
-- Tuples wider than 2 currently fail: GHC's Show (a, b, c) (and beyond)
-- emits an IR shape that drops the trailing elements in our bridge —
-- see ROADMAP → BRIDGE_haskell_strings.  Multiple tuple prints in one
-- module also hit additional unshimmed CAFs (e.g. $fShowCallStack8).

main :: IO ()
main = print ((7, 13) :: (Int, Int))
