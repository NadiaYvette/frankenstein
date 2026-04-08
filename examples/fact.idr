module Fact

fact : Int -> Int
fact n = if n <= 1 then 1 else n * fact (n - 1)

main : Int
main = fact 10
