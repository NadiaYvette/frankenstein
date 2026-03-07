# Frankenstein

**A polyglot compiler stitching together GHC, Mercury, rustc, Koka, and MLIR.**

Frankenstein reuses the frontends of existing, battle-tested compilers --
GHC for Haskell, mmc for Mercury, and rustc for Rust -- and funnels their
internal representations through a shared Core IR (inspired by Koka's Core)
with Perceus reference counting, then lowers to MLIR and LLVM for native
code generation.

The philosophy: why write a new frontend when you can raid the parts from
the best compilers already out there? Like the original monster, Frankenstein
is assembled from donor parts -- each one alive in its own right.

## Architecture

```
 Source Languages          Bridges              Shared Pipeline
 ==================    ================    ==========================

 Haskell (.hs)
   |
   v
 GHC API (ghc 9.14)
   |  GHC Core
   +--------------------+
                         |
 Mercury (.m)            |
   |                     v
   v               +-----------+     +----------+     +------+     +--------+
 mmc --dump-hlds   | Koka-like |     |          |     |      |     |        |
   |  HLDS         | Core IR   +---->+ Perceus  +---->+ MLIR +---->+  LLVM  +---> native
   +-------------->+           |     | (refcount)|    |      |     |        |     binary
                   +-----------+     +----------+     +------+     +--------+
 Rust (.rs)            ^
   |                   |
   v                   |
 rustc (nightly)       |
   |  MIR JSON         |
   +-------------------+
```

### Bridges

- **GHC Bridge** (`Frankenstein.GhcBridge`) -- Uses the GHC API (the `ghc`
  library package) to compile Haskell source to GHC Core, then translates
  Core expressions into Frankenstein's IR.

- **Mercury Bridge** (`Frankenstein.MercuryBridge`) -- Invokes `mmc
  --dump-hlds` to extract Mercury's High-Level Data Structure (HLDS), parses
  the determinism-annotated dump, and translates it to Core IR.

- **Rust Bridge** (`Frankenstein.RustBridge`) -- Builds a small rustc shim
  (`rustc-shim/`) that hooks into `rustc_private` to dump MIR as JSON, then
  parses and translates it. Ownership and borrowing information is preserved
  as affine/linear multiplicity annotations in the Core IR.

### Core IR

The shared intermediate representation is modeled after
[Koka](https://koka-lang.github.io/koka/doc/index.html)'s Core language.
Key features:

- **Algebraic effects** via effect rows
- **Multiplicity tracking** (Many, Affine, Linear) to represent Rust ownership
- **Perceus reference counting** -- automatic insertion of retain/release/drop
  operations, following the algorithm from Reinking et al.

### Backend

The MLIR emitter produces standard MLIR dialects (func, arith, scf, cf)
which are then lowered through MLIR's pass pipeline to LLVM IR and compiled
to a native binary.

## Quick Start

### Prerequisites

| Dependency     | Version          | Notes                                  |
|----------------|------------------|----------------------------------------|
| GHC            | 9.14.1           | Required for `ghc` library API         |
| cabal-install  | 3.14+            | Build system                           |
| Mercury (mmc)  | ROTD or 22.01+   | Mercury compiler                       |
| Rust (nightly) | nightly          | With `rustc-dev` and `llvm-tools`      |
| MLIR/LLVM      | 21               | `mlir-opt`, `mlir-translate`, `clang`  |

### Build

```bash
# Build the rustc shim first (needs nightly)
cd rustc-shim
cargo build
cd ..

# Build Frankenstein itself
cabal build
```

### Verify

```bash
# Run the built-in demo (no external tools needed)
cabal run frankenstein -- --demo
```

## Usage

### Built-in Demo

```bash
# Show Core IR and MLIR for a demo factorial program
cabal run frankenstein -- --demo

# Show only Core IR
cabal run frankenstein -- --demo --emit-core

# Show only MLIR
cabal run frankenstein -- --demo --emit-mlir

# Compile to native binary
cabal run frankenstein -- --demo --compile
```

### Compile Haskell

```bash
cabal run frankenstein -- test/haskell/Factorial.hs --emit-core
cabal run frankenstein -- test/haskell/Factorial.hs --emit-mlir
cabal run frankenstein -- test/haskell/Factorial.hs --compile
```

### Compile Mercury

```bash
cabal run frankenstein -- test/mercury/append.m --emit-core
cabal run frankenstein -- test/mercury/append.m --compile
```

### Compile Rust

```bash
cabal run frankenstein -- test/rust/factorial.rs --emit-core
cabal run frankenstein -- test/rust/factorial.rs --compile
```

### Multi-file Polyglot Mode

Multiple source files from different languages can be compiled and linked
together into a single program:

```bash
# Link Haskell + Rust into one Core program
cabal run frankenstein -- Scoring.hs helpers.rs --emit-core

# Link all three languages
cabal run frankenstein -- Factorial.hs append.m helpers.rs --emit-core

# Compile to native binary
cabal run frankenstein -- Factorial.hs helpers.rs --compile -o myprogram
```

Each language is detected by extension (`.hs`, `.m`, `.rs`) and routed to
the appropriate bridge. The cross-module linker merges all definitions into
a single unified Core program, runs Perceus, and emits MLIR.

## Project Structure

```
frankenstein.cabal              -- Package definition
app/Main.hs                    -- CLI entry point
src/Frankenstein/
    Core/
        Types.hs               -- Core IR AST (Koka-inspired)
        KokaCore.hs            -- Core IR utilities
        Perceus.hs             -- Perceus RC insertion
        Linker.hs              -- Cross-module polyglot linker
    GhcBridge/
        Driver.hs              -- GHC API session management
        CoreTranslate.hs       -- GHC Core -> Frankenstein Core
    MercuryBridge/
        HldsParse.hs           -- Parse mmc --dump-hlds output
        CoreTranslate.hs       -- HLDS -> Frankenstein Core
    RustBridge/
        MirParse.hs            -- Parse rustc MIR JSON
        CoreTranslate.hs       -- MIR -> Frankenstein Core
    MlirEmit/
        Emitter.hs             -- Core -> MLIR text
        Dialects.hs            -- MLIR dialect definitions
rustc-shim/                    -- Rust nightly crate for MIR extraction
test/                          -- Test inputs (Haskell, Mercury, Rust)
donors/                        -- Vendored reference code (not committed)
```

## Status

**Experimental / Research.** This is a proof-of-concept exploring polyglot
compilation through compiler-component reuse.

### What works

- Core IR type system with effects, multiplicities, and algebraic data types
- Perceus reference counting pass
- MLIR emission (func, arith, scf, cf dialects)
- MLIR -> LLVM -> native compilation pipeline
- Demo mode with built-in factorial program (end-to-end)
- GHC Bridge: GHC API initialization and Core extraction
- Mercury Bridge: HLDS dump parsing and translation
- Rust Bridge: MIR JSON dump and parsing

### In progress

- Full translation coverage for all GHC Core constructs
- Mercury effect mapping (IO, det/semidet -> effect rows)
- Rust lifetime/borrow translation to affine types
- ~~Multi-module / cross-language linking~~ **Done** -- basic polyglot linker working
- Koka frontend bridge

## References

- **Perceus: Garbage Free Reference Counting with Reuse.**
  Alex Reinking, Ningning Xie, Leonardo de Moura, Daan Leijen.
  MSR-TR-2020-42. PLDI 2021.
  <https://www.microsoft.com/en-us/research/publication/perceus-garbage-free-reference-counting-with-reuse/>

- **Koka: Programming with Row-Polymorphic Effect Types.**
  Daan Leijen. MSFP 2014.
  <https://koka-lang.github.io/koka/doc/index.html>

- **MLIR: Scaling Compiler Infrastructure for Domain Specific Computation.**
  Chris Lattner et al. CGO 2021.
  <https://mlir.llvm.org/>

## License

Frankenstein is licensed under the **GNU General Public License, version 2**
(GPL-2.0-only).

This license was chosen because the Mercury compiler is GPL-2 licensed, and
Frankenstein links against / invokes Mercury's toolchain. All other donor
components have compatible licenses:

- GHC: BSD-3-Clause
- Rust/rustc: MIT OR Apache-2.0
- Koka: Apache-2.0
- MLIR/LLVM: Apache-2.0 with LLVM Exception

See [LICENSE](LICENSE) for the full text and
[LICENSES-THIRD-PARTY.md](LICENSES-THIRD-PARTY.md) for details on each
donor codebase.
