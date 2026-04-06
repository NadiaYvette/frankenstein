fun fib(n : int) : div int
  if n <= 1 then n
  else fib(n - 1) + fib(n - 2)

fun main()
  println(fib(42).show)
