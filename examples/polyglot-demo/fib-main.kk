// Frankenstein Phase 1 Simple Demo — 2-language pure calling
//
// Koka main calls Haskell's fibonacci function.
// No effects, no string formatting — just pure cross-language function application.
// This tests the minimal polyglot pipeline:
//   fib.hs -> GHC bridge -> OrganIR
//   fib-main.kk -> Koka bridge -> OrganIR
//   Linker merges both -> MLIR -> native

// External declaration: Haskell's fibonacci compiled through the GHC bridge
extern fibonacci( n : int ) : int {
  c "fibonacci"
}

pub fun main() : int
  fibonacci(10)
