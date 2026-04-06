fun ack(m : int, n : int) : div int
  if m == 0 then n + 1
  elif n == 0 then ack(m - 1, 1)
  else ack(m - 1, ack(m, n - 1))

fun main()
  println(ack(3, 8).show)
