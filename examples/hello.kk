// Phase A hello-world for the Koka bridge.
// main returns a string; the MLIR main-wrapper auto-detects the
// `string` return type and emits kk_println_str(%result).
pub fun main() : string
  "Hello, World!"
