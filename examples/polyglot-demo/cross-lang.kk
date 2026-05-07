// Cross-Language Multi-Module Demo
//
// Koka orchestrator that calls functions from TWO Haskell modules:
//   - CrossModuleLib.hs: defines double, triple, sumList
//   - CrossModuleMain.hs: imports CrossModuleLib, defines analyze
//
// The GHC bridge compiles both Haskell modules in a single session
// (multi-module compilation). The Frankenstein linker then merges
// all three programs (2 Haskell + 1 Koka) into a single binary.
//
// This demonstrates:
//   1. Intra-language multi-module (Haskell -> Haskell import)
//   2. Cross-language calls (Koka -> Haskell)
//   3. Unified linking across language boundaries

// Extern declarations for Haskell functions.
// The C names are the unqualified Haskell function names —
// the linker resolves them against the symbol table.
extern double( x : int ) : int {
  c "double"
}

extern triple( x : int ) : int {
  c "triple"
}

extern analyze( n : int ) : int {
  c "analyze"
}

pub fun main() : int
  // Call Haskell functions from Koka
  val a = double(10)    // 20
  val b = triple(5)     // 15
  val c = analyze(5)    // double(5) + triple(5) + sum[1..5] = 10 + 15 + 15 = 40
  a + b + c             // 20 + 15 + 40 = 75
