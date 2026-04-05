// Frankenstein 4-language choice effect demo
//
// Language roles:
//   Haskell (fib.hs)      — pure recursive fibonacci
//   Rust    (double.rs)    — affine ownership, doubles an integer
//   Mercury (pick.m)       — multi: pick() generates 1,2,3 via choice effect
//   Koka    (this file)    — orchestrates all languages
//
// The Mercury multi predicate uses the choice algebraic effect.
// mercury_collect_choices runs the body for all binary choice paths,
// summing results: pick_all() = 1 + 2 + 3 = 6.

extern fibonacci( n : int ) : int {
  c "fibonacci"
}

extern double( n : int ) : int {
  c "double"
}

// pick_all: collects all solutions from Mercury's multi predicate pick()
// via iterative path enumeration of the choice effect
extern pick-all() : int {
  c "pick_all"
}

pub fun main() : int
  val picks = pick-all()         // Mercury multi: 1+2+3 = 6
  val dbl = double(picks)        // Rust: 6*2 = 12
  val fib = fibonacci(dbl)       // Haskell: fib(12) = 144
  fib                            // 144
