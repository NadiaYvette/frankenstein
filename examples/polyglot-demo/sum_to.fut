let sum_to (n: i64) : i64 =
  if n <= 0 then 0 else n + sum_to (n - 1)
