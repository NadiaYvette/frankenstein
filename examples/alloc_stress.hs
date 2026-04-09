module AllocStress where

-- Allocator microbenchmark: build a Cons list of length N, sum it,
-- throw it away. Each Cons becomes a kk_alloc_con call, so the
-- program executes O(N) constructor allocations + drops. Compare
-- arena vs libc malloc with:
--
--     time ./alloc_stress
--     KK_NO_ARENA=1 time ./alloc_stress
--
-- N is chosen to keep the recursive C-stack usage of buildList /
-- sumList / kk_drop comfortably inside the default stack limit.

data IntList = Nil | Cons Int IntList

buildList :: Int -> IntList
buildList 0 = Nil
buildList n = Cons n (buildList (n - 1))

sumList :: IntList -> Int
sumList Nil         = 0
sumList (Cons x xs) = x + sumList xs

-- Sum N copies of a length-K list. Total allocs = N * K, but the
-- recursion depth on any single list is only K — so a moderate K
-- and a large N stresses the allocator without exploding the stack.
sumMany :: Int -> Int -> Int
sumMany 0 _ = 0
sumMany n k = sumList (buildList k) + sumMany (n - 1) k

main :: Int
main = sumMany 200 1000
