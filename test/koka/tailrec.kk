fun len(ss)
  match ss
    Nil -> 0
    Cons(_, ss) -> 1 + len(ss)

fun iota(n)
  match n
    0 -> []
    n -> Cons(n, iota(n - 1))

fun main()
  println(len(iota(1000000)))
