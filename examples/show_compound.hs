module ShowCompound where

-- Demonstrates that the GHC bridge handles Show for the two most common
-- compound types: lists of Int and Maybe Int.
--
-- For `show [Int]`, GHC specialises to the $fShowInt_$cshowList worker,
-- intercepted by isShowIntListMethod → routed to the runtime
-- int_list_to_haskell_chars (builds "[n1,n2,n3]").
--
-- For `show (Just n)`, GHC inlines to
--   unpackAppendCString# $fShowMaybe1 (showsPrec 11 n "")
-- where $fShowMaybe1 is a top-level CAF holding "Just ".  We can't link
-- the CAF, so knownShowCAF recognises it by name and substitutes the
-- literal cons-list at translation time.  Same for $fShowMaybe3 = "Nothing".

main :: IO ()
main = do
  print ([] :: [Int])
  print [42 :: Int]
  print [1, 2, 3, 4, 5 :: Int]
  print [(-7), 0, 100 :: Int]
  print (Just 42 :: Maybe Int)
  print (Nothing :: Maybe Int)
