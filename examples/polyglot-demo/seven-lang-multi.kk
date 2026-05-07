// Frankenstein 7-Language Multi-Module Demo
//
// A single binary composed from SEVEN programming languages,
// with intra-language multi-module Haskell compilation:
//   1. Haskell  (CrossModuleMain.hs imports CrossModuleLib.hs)
//      - CrossModuleLib: double, triple, sumList
//      - CrossModuleMain: analyze (calls double+triple+sumList)
//   2. Rust     (negate.rs)  — affine ownership
//   3. Mercury  (check.m)    — semidet predicate (exn effect)
//   4. Python   (square.py)  — imperative
//   5. Go       (gcd.go)     — imperative
//   6. Futhark  (sum_to.fut) — array language
//   7. Koka     (this file)  — algebraic effects, orchestrates all
//
// Demonstrates: intra-language multi-module (Haskell→Haskell import)
// PLUS cross-language calls (Koka→Haskell/Rust/Mercury/Python/Go/Futhark).

// Haskell (from CrossModuleLib via multi-module compilation)
extern hs-double( x : int ) : int {
  c "double"
}

extern hs-triple( x : int ) : int {
  c "triple"
}

// Haskell (from CrossModuleMain, imports CrossModuleLib)
extern analyze( n : int ) : int {
  c "analyze"
}

// Rust (negate.rs — avoids name collision with Haskell's double)
extern negate-val( n : int ) : int {
  c "negate_val"
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
  // Haskell multi-module: analyze(5) = double(5) + triple(5) + sum[1..5]
  //                                  = 10 + 15 + 15 = 40
  val a = analyze(5)

  // Haskell direct: double(6) = 12
  val d = hs-double(6)

  // Python: square(6) = 36
  val p = square(6)

  // Go: gcd(36, 12) = 12
  val g = gcd(p, d)

  // Futhark: sum_to(12) = 1+2+...+12 = 78
  val f = sum-to(g)

  // Rust: negate_val(3) = -3
  val r = negate-val(3)

  // Mercury: check 40 > 30? → 1
  val ok = check-threshold(a, 30)

  // Final: ok * (a + d + p + g + f + r)
  //      = 1 * (40 + 12 + 36 + 12 + 78 + (-3)) = 175
  ok * (a + d + p + g + f + r)
