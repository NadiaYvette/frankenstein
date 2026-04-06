module WorkerWrapperTest where

-- At -O1, GHC splits this into $wfib (strict Int# worker) and fib (lazy wrapper)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = print (fib 10)
