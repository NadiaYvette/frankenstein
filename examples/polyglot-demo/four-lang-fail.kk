// Test that Mercury's semidet failure works: check_threshold(10, 60) fails
// because 10 is NOT > 60. The exn.fail handler returns 0.

extern fibonacci( n : int ) : int {
  c "fibonacci"
}

extern double( n : int ) : int {
  c "double"
}

extern check-threshold( n : int, threshold : int ) : int {
  c "check_threshold"
}

pub fun main() : int
  val fib = fibonacci(5)        // 5
  val dbl = double(3)           // 6
  val sum = fib + dbl           // 11
  // Mercury: check if 11 > 60 — should FAIL (returns 0 via exn handler)
  val ok = check-threshold(sum, 60)
  ok * sum + 1                  // 0 * 11 + 1 = 1
