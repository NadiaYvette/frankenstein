# Third-Party Licenses

Frankenstein incorporates code from, links against, or invokes the following
projects. Each is listed with its license and how it is used.

---

## GHC (The Glasgow Haskell Compiler)

- **License:** BSD-3-Clause
- **Copyright:** The GHC Team
- **Usage:** Frankenstein uses the GHC API (the `ghc` library package,
  version 9.14) to parse and compile Haskell source files to GHC Core, which
  is then translated to Frankenstein's Core IR.
- **Source:** <https://gitlab.haskell.org/ghc/ghc>

---

## Mercury

- **License:** GPL-2.0-only
- **Copyright:** The University of Melbourne / The Mercury Team
- **Usage:** Frankenstein invokes `mmc` (the Melbourne Mercury Compiler) as
  an external tool with `--dump-hlds` to extract Mercury's High-Level Data
  Structure, which is parsed and translated to Core IR. Mercury's GPL-2
  license is the reason Frankenstein is GPL-2 licensed.
- **Source:** <https://mercurylang.org/>

---

## Rust / rustc

- **License:** MIT OR Apache-2.0
- **Copyright:** The Rust Project Contributors
- **Usage:** The `rustc-shim/` crate links against `rustc_private` (Rust
  nightly internal APIs) to extract MIR (Mid-level Intermediate
  Representation) from Rust source files. The MIR is serialized as JSON and
  translated to Core IR. Requires Rust nightly with the `rustc-dev` component.
- **Source:** <https://github.com/rust-lang/rust>

---

## Koka

- **License:** Apache-2.0
- **Copyright:** Microsoft Research, Daan Leijen
- **Usage:** Frankenstein's Core IR design is inspired by Koka's Core
  language, including its representation of algebraic effects via effect rows,
  and the Perceus reference counting algorithm. Some type definitions in
  `donors/koka/` are adapted from the Koka compiler source.
- **Source:** <https://github.com/koka-lang/koka>

---

## MLIR / LLVM

- **License:** Apache-2.0 WITH LLVM-exception
- **Copyright:** LLVM Project Contributors
- **Usage:** Frankenstein emits MLIR text and invokes `mlir-opt`,
  `mlir-translate`, and `clang` (all from the LLVM 21 toolchain) as external
  tools to lower MLIR to LLVM IR and compile to native binaries.
- **Source:** <https://llvm.org/> / <https://mlir.llvm.org/>
