// Frankenstein Phase 1 Simple Demo — 2-language pure calling
//
// Koka main calls Haskell's fibonacci function.
// No effects, no backtracking — just pure cross-language function application.
// This tests the minimal polyglot pipeline:
//   fib.hs -> GHC bridge -> OrganIR
//   fib-main.kk -> Koka bridge -> OrganIR
//   Linker merges both -> MLIR -> native

// External declaration: Haskell's fibonacci compiled through the GHC bridge
extern fibonacci( n : int ) : int

fun main() : <console> ()
  val n = 10
  val result = fibonacci(n)
  println("fibonacci(" ++ n.show ++ ") = " ++ result.show)
