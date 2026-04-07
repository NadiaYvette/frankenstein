-- Recursive factorial in Futhark.
-- Exercises the in-tree Pratt parser end-to-end.

let factorial (n: i64) : i64 =
  if n <= 1 then 1 else n * factorial (n - 1)

let main : i64 = factorial 10
