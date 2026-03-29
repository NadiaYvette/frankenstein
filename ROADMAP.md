# Frankenstein Roadmap

## Vision

Frankenstein is the only compiler that combines formally verified reference
counting (Perceus, K-proven), four real compiler frontends stolen from
production compilers (GHC, rustc, mmc, Koka), algebraic effects as the
unified cross-language abstraction, and MLIR as the backend. This roadmap
charts the path from working prototype to research contribution.

---

## Phase 1: The Polyglot Promise Made Real

**Goal**: A single binary where 4 functions in 4 languages compose through
shared algebraic effects. This is the demo that makes people understand
what Frankenstein is.

### 1a. Cross-Language Calling Convention

Write a program where:
- **Haskell** defines a pure recursive function (e.g., fibonacci)
- **Rust** defines an accumulator with affine ownership
- **Mercury** defines a search predicate with backtracking (choice effect)
- **Koka** defines the main with algebraic effect handlers

All four compile through their bridges to OrganIR, link via the polyglot
linker, and produce a single native binary via MLIR.

**Prerequisites**: The linker name-rewriting (done), evidence pass for
cross-module effects, MLIR emitter handling all four bridges' output patterns.

**Deliverable**: `frankenstein fib.hs accumulate.rs search.m main.kk --compile && ./a.out`

### 1b. Polyglot Test Suite

Create `examples/polyglot-demo/` with the 4-language program above plus
a test script that:
1. Compiles each file individually through its bridge (`--emit-core`)
2. Links them together
3. Compiles to native
4. Verifies output against K oracle (see Phase 2)

---

## Phase 2: K as the Living Specification

**Goal**: Make K Framework the source of truth for the entire IR, not just
Perceus. Every transformation verified against a formal model.

### 2a. Full OrganIR Operational Semantics in K

Extend `organ-ir.k` from typing rules + Perceus to a complete executable
semantics:
- **Evaluation rules**: `EApp`, `ELam` (beta-reduction), `ELet`, `ECase`
  (pattern matching), `EDelay`/`EForce` (thunk semantics)
- **Effect semantics**: `EPerform` searches the handler stack, `EHandle`
  pushes/pops handlers, evidence-passing translation as K rewrite rules
- **Memory model**: Heap allocation for `kk_alloc_con`, refcount tracking
  for `kk_retain`/`kk_drop`, thunk memoization

This turns `krun` into a reference interpreter for OrganIR.

### 2b. Property-Based Testing via K Oracle

- Generate random OrganIR programs (QuickCheck-style)
- Run through both `krun` (K reference) and Frankenstein MLIR pipeline
- Compare outputs — any divergence is a compiler bug
- Integrate into `cabal test` as a slow test suite

### 2c. Bridge Bisimulation Proofs

For each bridge, prove (or test) that the translation preserves observable
behavior:
- **GHC bridge**: For pure functions, `krun(translateGHC(e))` equals
  GHC's evaluation of `e`
- **Koka bridge**: `krun(translateKoka(e))` equals Koka's evaluation
- Use K's LAMBDA semantics as the reference for the pure functional subset
- Use the bundled IMP/SIMPLE semantics for imperative fragments

### 2d. Extend kprove Claims

Beyond Perceus (20 claims verified), add claims for:
- Evidence-passing preserves effect semantics
- Linker name-rewriting preserves call graph
- MLIR emission preserves evaluation order
- Bridge-specific invariants (already 47 property tests, promote to claims)

---

## Phase 3: Perceus for Haskell — Replace GC with RC

**Goal**: Compile a non-trivial Haskell program through Frankenstein and
run it without a garbage collector, purely on Perceus reference counting +
thunks. If performance is even remotely competitive, this is a paper.

### 3a. Haskell RC Feasibility Study

- Take `test/haskell/Factorial.hs` end-to-end through the full pipeline:
  GHC bridge → Perceus → MLIR → native
- Profile the retain/drop overhead vs. GHC's GC
- Identify the pain points: circular references (need cycle detection or
  weak refs), large lazy data structures (thunk chains), typeclass
  dictionaries (currently erased)

### 3b. Handle GHC Core Patterns

The GHC bridge currently handles the simple cases. For real Haskell:
- **Typeclass dictionaries**: GHC desugars typeclasses to dictionary-passing.
  The bridge sees `App (Var dictFun) (Var dictArg)` — translate to explicit
  struct-field access
- **Unboxed types**: GHC's `Int#`, `Double#` — map to MLIR `i64`/`f64`
  directly (no boxing)
- **Worker/wrapper**: GHC splits functions into strict workers and lazy
  wrappers at `-O1` — the bridge should prefer the worker
- **Join points**: GHC Core's `join` construct — translate to labeled
  continuations or loop heads

### 3c. Cycle Detection

Pure Perceus cannot handle cyclic data structures (refcount never reaches 0).
Options:
- **Trial deletion** (Bacon-Rajan): periodic cycle collection for suspected
  cyclic roots
- **Weak references**: Mark back-edges in recursive data as weak (no refcount)
- **Static analysis**: The K spec could prove absence of cycles for certain
  program patterns, allowing the runtime to skip cycle checks

### 3d. Benchmark Suite

- nofib subset: spectral/boyer, spectral/constraints, imaginary/wheel-sieve
- Compare: Frankenstein-Perceus vs. GHC-GC vs. Rust (manual) vs. Koka-Perceus
- Metric: wall time, peak memory, allocation count, retain/drop count

---

## Phase 4: MLIR Dialect for Algebraic Effects

**Goal**: Instead of lowering effects to evidence-passing in Haskell, define
a first-class `frankenstein` MLIR dialect. MLIR's pass infrastructure can
then optimize effect dispatch at the IR level.

### 4a. Dialect Definition

Define MLIR operations:
```mlir
%evv = frankenstein.evv_get : !frankenstein.evv
%result = frankenstein.perform %evv @effect_name @op_name(%args) : (i64) -> i64
frankenstein.handle @effect_name(%evv) {
  ^body:
    // ... code that may perform ...
  ^handler_op1(%arg, %resume):
    // ... handler clause ...
}
```

Register as an MLIR dialect with proper type checking, so `mlir-opt` can
validate effect usage.

### 4b. Effect Optimization Passes

MLIR passes that optimize the `frankenstein` dialect before lowering:
- **Handler inlining**: If a `handle` and `perform` are in the same function,
  inline the handler clause at the perform site
- **Effect elimination**: If a handler is the identity (resume immediately),
  eliminate both handle and perform
- **Evidence specialization**: Monomorphize evidence-passing for known handlers
- **Tail-resumptive optimization**: If the handler always resumes in tail
  position, eliminate the continuation capture

### 4c. Lowering to Standard MLIR

Lower `frankenstein.*` ops to `func`/`scf`/`llvm` dialect:
- `perform` → evidence vector lookup + indirect call
- `handle` → push/pop evidence + `scf.execute_region` or setjmp/longjmp
- Continuation capture → CPS or segmented stacks

---

## Phase 5: Wasm Backend

**Goal**: MLIR → Wasm, turning Frankenstein into a polyglot-to-web compiler.

### 5a. MLIR-to-Wasm Pipeline

- Use `mlir-translate` to go from LLVM dialect to `.ll`, then `llc` to Wasm
- Or: lower directly to MLIR's `wasm` dialect (experimental)
- Provide a `kk_runtime.wat` (WebAssembly text format) for the Perceus runtime

### 5b. KWasm Verification

- KWasm (github.com/runtimeverification/wasm-semantics) is actively maintained
  and pins K 7.1.313 (compatible with our 7.1.314)
- Validate: Frankenstein → MLIR → Wasm → KWasm execution matches K oracle
- This closes the verification loop: source → K oracle, binary → KWasm

### 5c. Browser Demo

- Compile a polyglot program to Wasm
- Run in the browser with a minimal JS harness
- Interactive playground: edit Haskell/Koka/Rust/Mercury, recompile, run

---

## Phase 6: Self-Hosting Bootstrap

**Goal**: Feed Frankenstein's own Haskell source through the GHC bridge.
Even partial self-hosting is a dramatic proof of capability.

### 6a. Compile Core/Types.hs

`Core/Types.hs` is a pure data definition module with no IO — the simplest
target. Compiling it through the GHC bridge → MLIR would prove the pipeline
handles real Haskell ADTs, pattern matching, and type-level constructs.

### 6b. Compile Core/Perceus.hs

The Perceus pass itself, compiled through the Perceus pass. Beautifully
recursive. This exercises: pattern matching on the Expr ADT, Set/Map
operations, recursive function calls, higher-order functions (map, filter).

### 6c. Full Self-Hosting

Compile all of `src/Frankenstein/` through the GHC bridge, link, and produce
a `frankenstein` binary that can compile the demo. This is the ultimate
stress test and would make Frankenstein self-sustaining — independent of GHC
for its own compilation (though still using GHC as a frontend).

---

## Current State (2026-03-29)

### What's Built and Working
- **4 bridges**: GHC (real API), Rust (MIR text+JSON), Mercury (HLDS), Koka (library API)
- **Core IR**: Multiplicity, effect rows, Perceus ops, laziness ops
- **Perceus pass**: Drop + retain insertion, formally verified (20 kprove claims)
- **Evidence pass**: Single-op and multi-op effect dispatch
- **Linker**: Multi-module merging with cross-module name rewriting
- **MLIR emitter**: func/arith/scf/llvm dialects, lambda lifting, closures with
  real function pointers, thunks, bool/char/int/float/string support
- **Runtime**: Perceus RC (`kk_retain`/`kk_drop`), boxed values, thunks
- **K specs**: OrganIR typing + Perceus (organ-ir.k), 73 krun tests, 47 bridge
  property tests, 20 kprove-verified claims
- **Test suite**: 28 cabal unit tests, K test oracle plan
- **End-to-end**: `--demo --compile` → 3628800, `factorial.kk --compile` → 3628800

### Recent Commits
- `093f0ce` — Closures, thunks, MIR parsing, linker, evidence, strings
- `95f43c8` — Perceus retains, MLIR improvements, data decls, tests, kprove
- `3a447a8` — Bridge property K specs, kprove claims, MLIR boolean codegen fix

---

## Appendix: Key Technical Details

### OrganIR Design Principles
- **Multiplicity annotations**: Linear (use once), Affine (use at most once),
  Many (unrestricted) — inferred per-bridge, refined by Perceus
- **Effect rows**: Open rows with extension/variables — Koka-style
- **Perceus RC**: Drop for unused, retain for multi-use, skip for linear
- **Laziness**: EDelay/EForce with memoizing thunks — Haskell semantics

### Bridge Translation Invariants (K-verified)
- GHC: strict → no delay, lazy → delay, absent → dead, forall → KStar+Many
- Rust: all args affine, move → no retain, copy → retain, scope exit → drop
- Mercury: det → pure, semidet → exn, multi → choice, di/uo → linear
- Koka: all mul Many (Perceus re-derives), effects preserved, synonyms expanded

### MLIR Pipeline
```
OrganIR → Evidence Pass → Perceus → MLIR Text → mlir-opt → mlir-translate → clang + kk_runtime.c → a.out
```

### K Verification Pipeline
```
organ-ir.k → kompile (LLVM backend) → krun tests (73 pass)
organ-ir.k → kompile (Haskell backend) → kprove claims (20 verified)
bridge-properties.k → kompile (LLVM backend) → krun tests (47 pass)
```
