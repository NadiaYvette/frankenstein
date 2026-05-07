// Frankenstein FFI Cross-Language Demo
//
// Haskell uses `foreign import ccall` to call Python's square and Go's gcd.
// Koka then calls the Haskell wrapper and adds its own computation.
//
// Flow: Koka → Haskell (foreign import ccall → Python, Go) → result
//
//   gcd(36, 24) = 12          (Go, called by Haskell via FFI)
//   square(12)  = 144         (Python, called by Haskell via FFI)
//   fibonacci(7) = 13         (Haskell, called directly by Koka)
//   result = 144 + 13 = 157

extern squared-gcd( a : int, b : int ) : int {
  c "FfiImport_squaredGcd"  // Haskell, which calls Python+Go via foreign import ccall
}

extern fibonacci( n : int ) : int {
  c "fibonacci"
}

pub fun main() : int
  val from-haskell-ffi = squared-gcd(36, 24)  // 144
  val fib = fibonacci(7)                       // 13
  from-haskell-ffi + fib                       // 157
