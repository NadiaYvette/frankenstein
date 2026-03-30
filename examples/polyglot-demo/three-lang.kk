// Frankenstein 3-language demo: Koka main calls Haskell + Rust
//
// Language roles:
//   Haskell (fib.hs)    — pure recursive fibonacci
//   Rust    (double.rs) — affine ownership, doubles an integer
//   Koka    (this file) — orchestrates: fibonacci(10) + double(7)

extern fibonacci( n : int ) : int {
  c "fibonacci"
}

extern double( n : int ) : int {
  c "double"
}

pub fun main() : int
  fibonacci(10) + double(7)
