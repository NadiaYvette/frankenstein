// Frankenstein 12-Language Demo
//
// A single binary composed from TWELVE programming languages:
//   1.  Haskell  (fib.hs)       — recursive fibonacci via GHC API
//   2.  Rust     (negate.rs)    — affine ownership, negation
//   3.  Mercury  (check.m)      — semidet predicate (exn effect)
//   4.  Python   (square.py)    — imperative, squares an integer
//   5.  Go       (gcd.go)       — imperative, greatest common divisor
//   6.  Futhark  (sum_to.fut)   — array language, sum 1..n
//   7.  Swift    (pow4.swift)   — fourth power via SIL
//   8.  OCaml    (halve.ml)     — integer division via Lambda IR
//   9.  Erlang   (modulo.erl)   — modulo via Core Erlang
//   10. F#       (abs_val.fsx)  — absolute value via typed tree
//   11. Idris    (clamp.idr)    — clamp to range via in-tree parser
//   12. Koka     (this file)    — algebraic effects, orchestrates all
//
// Each language is compiled through its real compiler's API/IR,
// translated to OrganIR, linked by the polyglot linker, and
// emitted as a single MLIR module → native binary.

// 1. Haskell: recursive fibonacci
extern fibonacci( n : int ) : int {
  c "fibonacci"
}

// 2. Rust: negate
extern negate-val( n : int ) : int {
  c "negate_val"
}

// 3. Mercury: semidet check — returns 1 if n > threshold, else 0
extern check-threshold( n : int, threshold : int ) : int {
  c "check_threshold"
}

// 4. Python: square
extern square( n : int ) : int {
  c "square"
}

// 5. Go: greatest common divisor
extern gcd( a : int, b : int ) : int {
  c "gcd"
}

// 6. Futhark: sum of 1..n
extern sum-to( n : int ) : int {
  c "sum_to"
}

// 7. Swift: fourth power
extern pow4( n : int ) : int {
  c "pow4"
}

// 8. OCaml: integer halving
extern halve( n : int ) : int {
  c "halve"
}

// 9. Erlang: modulo
extern modulo( a : int, b : int ) : int {
  c "modulo"
}

// 10. F#: absolute value
extern abs-val( n : int ) : int {
  c "abs_val"
}

// 11. Idris: clamp to range [lo, hi]
extern clamp( lo : int, hi : int, x : int ) : int {
  c "clamp"
}

pub fun main() : int
  val fib  = fibonacci(7)             // Haskell:  13
  val sq   = square(fib)              // Python:   169
  val g    = gcd(sq, 78)              // Go:       gcd(169, 78) = 13
  val s    = sum-to(g)                // Futhark:  1+2+...+13 = 91
  val p    = pow4(3)                  // Swift:    81
  val h    = halve(p)                 // OCaml:    40
  val m    = modulo(s, h)             // Erlang:   91 mod 40 = 11
  val a    = abs-val(negate-val(m))   // F#+Rust:  abs(-11) = 11
  val c    = clamp(5, 20, a)          // Idris:    clamp(5,20,11) = 11
  val ok   = check-threshold(c, 0)   // Mercury:  11 > 0 → 1

  // Final: ok * (fib + sq + g + s + p + h + m + a + c)
  //      = 1 * (13 + 169 + 13 + 91 + 81 + 40 + 11 + 11 + 11) = 440
  ok * (fib + sq + g + s + p + h + m + a + c)
