fun factorial(n : int) : div int
  if n <= 0 then 1
  else n * factorial(n - 1)

fun main() : <console,div> ()
  println(factorial(10))
