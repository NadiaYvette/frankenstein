// Frankenstein 7-Language Demo
//
// A single binary composed from SEVEN programming languages:
//   1. Haskell  (fib.hs)     — recursive fibonacci via GHC API
//   2. Rust     (double.rs)  — affine ownership, doubles an integer
//   3. Mercury  (check.m)    — semidet predicate (exn effect on failure)
//   4. Python   (square.py)  — imperative, squares an integer
//   5. Go       (gcd.go)     — imperative, greatest common divisor
//   6. Futhark  (sum_to.fut) — array language, sum 1..n
//   7. Koka     (this file)  — algebraic effects, orchestrates all languages
//
// Each language is compiled through its own bridge to OrganIR,
// linked by the polyglot linker, and emitted as a single MLIR module.

// Haskell: recursive fibonacci
extern fibonacci( n : int ) : int {
  c "fibonacci"
}

// Rust: double a number (affine ownership in MIR)
extern double( n : int ) : int {
  c "double"
}

// Mercury: semidet check — returns 1 if n > threshold, else 0
extern check-threshold( n : int, threshold : int ) : int {
  c "check_threshold"
}

// Python: square a number
extern square( n : int ) : int {
  c "square"
}

// Go: greatest common divisor
extern gcd( a : int, b : int ) : int {
  c "gcd"
}

// Futhark: sum of 1..n
extern sum-to( n : int ) : int {
  c "sum_to"
}

pub fun main() : int
  val fib   = fibonacci(8)           // Haskell:  21
  val dbl   = double(fib)            // Rust:     42
  val sq    = square(7)              // Python:   49
  val g     = gcd(sq, dbl)           // Go:       gcd(49, 42) = 7
  val s     = sum-to(g)              // Futhark:  1+2+3+4+5+6+7 = 28
  val ok    = check-threshold(s, 20) // Mercury:  28 > 20 → 1
  ok * (fib + dbl + sq + g + s)      // 1 * (21 + 42 + 49 + 7 + 28) = 147
