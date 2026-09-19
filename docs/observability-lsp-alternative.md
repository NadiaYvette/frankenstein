# Organ Bank Infrastructure as an LSP Alternative & Cross-Language Observability Sidecar

## 1. Overview

While [Frankenstein](file:///home/nyc/src/frankenstein) focuses on compiling polyglot programs into a unified MLIR/Perceus pipeline, its companion harvesting facility, [Organ Bank](file:///home/nyc/src/organ-bank), provides intermediate representations (IR) across 29 compiler frontends in a uniform schema ([`OrganIR`](file:///home/nyc/src/organ-bank/spec/organ-ir.schema.json)).

Architectural work pioneered in `intelli-monad` identified that this existing compiler harvesting infrastructure can serve as an **effect-aware, cross-language Language Server Protocol (LSP) alternative and observability sidecar**.

### 1.1 The Polyglot LSP Problem

Modern software development and autonomous coding agents face steep hurdles in multi-language repositories:
- **Fragmented Language Servers**: Running separate LSPs (`haskell-language-server`, `rust-analyzer`, `gopls`, `clangd`) demands gigabytes of memory and distinct process trees.
- **FFI Blind Spots**: No conventional LSP understands foreign function boundaries. When Rust calls C or Koka calls Haskell, type checking and semantic analysis halt at `extern "C"`.
- **Effect Ignorance**: Mainstream LSPs lack representation for algebraic effect rows, purity, or affine/linear resource multiplicity.

### 1.2 The Organ Bank Solution

Organ Bank's four-stage extraction pipeline maps directly onto the requirements of an advanced language server:

```
  Source files (.hs, .rs, .kk, .ml, .m, .zig, .c, ...)
        │
        ▼  [Stage 1: Resilient Concrete Tree (RCT)]
  29 Compiler Shims (harvesting Core, MIR, HLDS, Koka Core)
        │
        ▼  [Stage 2: Generalized Skeleton IR (GS-IR)]
  OrganIR.Definition (sorts, visibility, type signatures without bodies)
        │
        ▼  [Stage 3: Paradigm-Abstract IR (PA-IR)]
  EffectRow (<div, exn>, IO) + Multiplicity (affine, linear)
        │
        ▼  [Stage 4: Unified Graph]
  OrganIR JSON / organ-index SQLite WAL
        │
   ┌────┴─────────────────────────────┐
   ▼                                  ▼
[Frankenstein Compiler Backend]    [Observability Sidecar / Agent Tools]
(MLIR -> Perceus -> LLVM)          (organ_repo_map, organ_check_boundary, ...)
```

---

## 2. Architecture: The `organ-index` Observability Sidecar

The sidecar operates as an out-of-process daemon or CLI (`organ-index`) backed by an SQLite WAL database:
- **Resilience**: Operates even when compilation fails or code is mid-edit. Shims emit partial OrganIR alongside self-describing diagnostic envelopes (`organ_diag_version: "1"`).
- **Zero In-Process Coupling**: Consumes JSON artifacts over pipes or file paths, avoiding compiler dependency collisions.
- **Verbatim Semantic Storage**: Types and effect rows are stored verbatim as OrganIR JSON, ensuring that downstream checkers and LLM agents see the exact AST rather than a lossy relational projection.

### 2.1 Tool Capabilities

1. `organ_repo_map`: Produces a token-budgeted skeleton map across modules and donor languages, displaying definitions, type signatures, and effect rows without function bodies.
2. `organ_find_symbol`: Cross-language symbol search resolving definitions and call references across language boundaries.
3. `organ_check_boundary`: Compares two boundary signatures across languages, classifying type and effect compatibilities.
4. `organ_diagnostics`: Surfaces compiler-grade errors, warnings, and verbatim compiler stderr.

---

## 3. Boundary Typechecking & the PMWA Connection

In Frankenstein today, cross-language calls meet at a uniform `i64` ABI without dynamic type enforcement (see [docs/ffi-cross-language.md](file:///home/nyc/src/frankenstein/docs/ffi-cross-language.md)). The observability sidecar provides the path toward sound interop based on the **Patterson–Mushtak–Wagner–Ahmed (PMWA)** foreign-call typechecking framework:

- **Stage 0 (Current Linker)**: Name-based symbol matching over `i64`.
- **Stage 1 (Observability & Descriptive Reconciliation)**: `organ_check_boundary` inspects the caller and callee signatures in OrganIR. It performs direction-aware verification:
  - *Arguments*: Flow `caller -> callee` (narrowing is refused).
  - *Return Values*: Flow `callee -> caller` (widening is refused).
  - *Effect Rows*: Callee effects must be a subset of caller effects, or must declare an explicit effect-surfacing convention (e.g. status sentinel `-2^63` or error-code handling).
- **Stage 2 (Representation Dictionary & Decision Procedure)**: An auditable axiom table (`IntelliMonad.Tools.OrganBank.Dictionary`) classifies type representations by bitwidth and family (`FSigned`, `FUnsigned`, `FBigSigned`, `FBigUnsigned`, `FFloat`, `FDynamic`). Crossings license as `licensed-lossless`, `licensed-widening`, or `licensed-bounded`.
- **Stage 3 (Verified Interop Claims)**: Claims are backed by explicit evidence classes (`ESpec` > `EPracticed` > `EProbed` > `EConventional`).

---

## 4. Role in Frankenstein's Endgame

A common question is how this sidecar relates to Frankenstein's ultimate pipeline:

1. **Scaffolding vs. Target Pipeline**:
   The multi-runtime island spikes (running GHC RTS, kklib, OCaml RTS, and Rust in one process) are bridge-tier scaffolding. In the compiler's endgame, code from Mercury, Rust, and Haskell is lowered directly into Frankenstein's shared Core IR and managed under Perceus reference counting.
2. **The Dictionary Feeds the Lowering**:
   The Representation Dictionary and Axiom Table (proven by live compile-and-link probes) provide the exact machine-level memory layouts and invariant constraints that Frankenstein's Core-IR lowering and MLIR emitter must respect.
3. **Auditing the Endgame**:
   After complete unification into Perceus MLIR, `organ_check_boundary` does not disappear. Instead, it transitions from an FFI permission gate into a **compile-time representation audit layer**, guaranteeing that lowerings have not violated memory invariants or silently corrupted representations, while continuing to check external C libraries and OS interfaces.

---

## 5. References

- [Organ Bank Observability Documentation](file:///home/nyc/src/organ-bank/doc/observability-lsp-alternative.md)
- [Frankenstein Cross-Language FFI](file:///home/nyc/src/frankenstein/docs/ffi-cross-language.md)
- [OrganIR JSON Schema](file:///home/nyc/src/organ-bank/spec/organ-ir.schema.json)
