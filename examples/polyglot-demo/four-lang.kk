// Frankenstein 4-language demo: Koka orchestrates Haskell + Rust + Mercury
//
// Language roles:
//   Haskell (fib.hs)      — pure recursive fibonacci
//   Rust    (double.rs)    — affine ownership, doubles an integer
//   Mercury (check.m)      — semidet: check_threshold, performs exn on failure
//   Koka    (this file)    — handles effects, orchestrates all languages
//
// For the semidet Mercury predicate, failure maps to exn.fail which
// returns 0 (caught by the evidence-passing transform at link time).
// The evidence pass replaces EPerform(exn/fail) with a call to mercury_exn_fail().

extern fibonacci( n : int ) : int {
  c "fibonacci"
}

extern double( n : int ) : int {
  c "double"
}

// Mercury's check_threshold: succeeds (returns 1) or fails (returns 0)
// We treat it as a pure extern here; the exn effect is handled at Core level.
extern check-threshold( n : int, threshold : int ) : int {
  c "check_threshold"
}

pub fun main() : int
  val fib = fibonacci(10)       // Haskell: 55
  val dbl = double(7)           // Rust: 14
  val sum = fib + dbl           // 69
  // Mercury: check if sum > 60 — should succeed (69 > 60) returning 1
  val ok = check-threshold(sum, 60)
  ok * sum                      // 1 * 69 = 69
