// Frankenstein Rust extern "C" Cross-Language Demo
//
// Rust uses `extern "C"` to call Python's square and Haskell's fibonacci.
// Koka calls the Rust wrapper and adds its own computation.
//
// Flow: Koka → Rust (extern "C" → Python + Haskell) → result
//
//   square(7)     = 49    (Python, called by Rust via extern "C")
//   fibonacci(7)  = 13    (Haskell, called by Rust via extern "C")
//   rust_combined = 49 + 13 = 62
//   result = 62 + 7 = 69

extern rust-combined( n : int ) : int {
  c "rust_combined"  // Rust, which calls Python+Haskell via extern "C"
}

pub fun main() : int
  val from-rust-ffi = rust-combined(7)  // 62
  from-rust-ffi + 7                      // 69
