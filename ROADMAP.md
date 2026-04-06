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

### 3a. Haskell RC Feasibility Study ✓

- **End-to-end proven**: `Factorial.hs` → GHC bridge → Perceus → MLIR → native → 2432902008176640000
- **Profile (factorial 20)**:
  - Binary: 14 KB vs GHC's 26 MB (1860x smaller)
  - Speed: 2.4ms vs 5.5ms per run (2.3x faster, dominated by startup)
  - RC ops: 42 retain calls, all no-ops (unboxed integers skip `kk_is_heap_ptr`)
  - Heap: 0 allocations, 0 drops, 0 frees — pure stack computation
- **Pain points identified**:
  1. **Circular references**: Haskell's lazy `let rec` (e.g., `xs = 1 : xs`) creates cycles
     that RC alone cannot collect. Needs trial deletion or weak refs.
  2. **Thunk chains**: Deep lazy evaluation (e.g., `foldl (+) 0 [1..10^6]`) builds O(n)
     thunk chains. Each thunk is heap-allocated with RC=1; forcing triggers a cascade
     of allocations. Not a leak, but high allocation pressure.
  3. **Typeclass dictionaries**: GHC desugars `show`, `+`, `==` etc. to dictionary-passing.
     Currently erased by the bridge. Real programs need dictionary structs with RC.
  4. **Sharing via laziness**: Haskell relies on thunk memoization for sharing
     (`let x = expensive in (x, x)` computes once). Current thunk impl supports this,
     but multi-reference thunks need correct retain/drop around force.
  5. **Unboxed vs boxed**: Current pipeline treats all values as i64. Real Haskell uses
     `Int#` (unboxed) and `Int` (boxed `I#` wrapper). The I# simplification helps but
     algebraic data (lists, trees) needs heap boxing with proper RC.

### 3b. Handle GHC Core Patterns ✓

All four GHC Core patterns now compile through the pipeline:
- **Typeclass dictionaries** ✓: GHC at `-O1` resolves dictionaries to concrete method
  selectors (`$fNumInt_$c+`). Our `isDictArg` filter strips dictionary args, method
  selectors map to builtins. `double(21) = 42` via typeclass `(+)`.
- **Unboxed types** ✓: GHC's `$w` workers operate on `Int#` with primops (`+#`, `-#`,
  `<#`, `==#`). MLIR emitter now handles `#`-suffixed primops. `sumTo(100) = 5050`.
- **Worker/wrapper** ✓: GHC splits into strict workers and lazy wrappers. Workers are
  `Rec` bindings (direct functions), wrappers are filtered. `fib(10) = 55` via `$wfib`.
- **Join points** ✓: GHC at `-O1` compiles guards/nested patterns to cascaded cases
  with primop comparisons. `classify(-5) + classify(0) + classify(42) = 0`.
- **Key fixes**: Don't thunk lambdas (`isLambda` check in `decideLaziness`), Bool→i64
  consistently, GHC primop name recognition (`+#`, `-#`, `<#`, `==#`, `negate`),
  Num method selectors (`$fNumInt_$c+/*/negate`).

### 3c. Cycle Detection ✓

- **Bacon-Rajan trial deletion** ✓: `runtime/kk_cycle.c` implements the synchronous
  cycle collector — MarkRoots (trial-delete internal refs), ScanRoots (identify
  live vs garbage), CollectRoots (free white objects). Uses color encoding in high
  byte of refcount word (black/purple/gray/white).
- **Runtime integration** ✓: `kk_drop()` registers cycle candidates when rc > 0
  after decrement. `kk_alloc_con()` registers nfields in side table for child
  scanning. Recursive child dropping on free. Existing programs unaffected.
- **Static cycle analysis** ✓: `Core/CycleAnalysis.hs` detects potential cycle
  sources (constructor applications capturing self-references). Reports in
  `--emit-core` output. All current test programs correctly identified as acyclic.
- **C test suite** ✓: 10/10 tests passing — acyclic data, cyclic pairs (A↔B),
  cyclic triples (A→B→C→A), self-reference (A→A), mixed acyclic+cyclic.
- **K tests** ✓: 8 new krun tests for constructor allocation, retain/drop of
  heap objects, thunk semantics (force/delay), and RC operations on constructed data.

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

## Current State (2026-04-06)

### What's Built and Working
- **4 bridges**: GHC (real API), Rust (MIR text+JSON), Mercury (HLDS), Koka (library API)
- **Core IR**: Multiplicity, effect rows, Perceus ops, laziness ops
- **Perceus pass**: Drop + retain insertion, formally verified (20 kprove claims)
- **Evidence pass**: Single-op and multi-op effect dispatch, 13 kprove claims
- **Linker**: Multi-module merging with cross-module name rewriting, 20 kprove claims
- **MLIR emitter**: func/arith/scf/llvm dialects, lambda lifting, closures with
  real function pointers, thunks, bool/char/int/float/string support
- **Runtime**: Perceus RC (`kk_retain`/`kk_drop`), boxed values, thunks
- **K specs**: OrganIR typing + Perceus + full effect semantics (organ-ir.k),
  104 krun tests (incl. 9 algebraic effect tests), 47 bridge property tests,
  120 kprove-verified claims (20 Perceus + 67 bridge + 13 evidence + 20 linker)
- **Effect semantics in K**: Full `EPerform`/`EHandle` with delimited continuation
  capture, abort (exn) and resume (choice) patterns, nested handler support
- **K oracle (Phase 2b)**: QuickCheck differential testing — random OrganIR programs
  run through both krun and MLIR pipeline, outputs compared (70 property tests)
- **Bridge bisimulation (Phase 2c)**: For each bridge, `krun(translate(source))`
  compared against expected values and native compiler output. Verified:
  - GHC: arithmetic + factorial(10)=3628800, with native `ghc` comparison
  - Koka: arithmetic, krun matches expected
  - Rust: arithmetic, with native `rustc` comparison
  - Mercury: structural (semantic pending HLDS variable resolution)
  - Expression cleaning pipeline: strip laziness/Perceus ops, normalize builtins,
    simplify I# boxing, reorder branches, self-application for recursion
- **Extended kprove claims (Phase 2d)**: 100 new claims beyond original 20 Perceus:
  - Bridge claims (67): all 47 property functions promoted to formal verification
    with concrete positive/negative test cases per property
  - Evidence claims (13): no-EHandle/EPerform post-pass, single-op/multi-op
    binding structure, nested effect scope preservation
  - Linker claims (20): local names preserved, main never mangled, module-prefix
    mangling correct, call graph preservation, shouldRewrite consistency
- **Phase 3a: Haskell RC feasibility** ✓: Factorial.hs end-to-end through full pipeline,
  profiled with instrumented runtime — 14KB binary, 2.3x faster than GHC, zero heap allocs,
  42 no-op RC calls. Pain points documented: cycles, thunk chains, dictionaries, sharing, boxing
- **Phase 3b: GHC Core patterns** ✓: All 4 patterns handled — typeclass dictionaries (resolved
  at -O1), unboxed primops (+#/-#/<#/==#), worker/wrapper ($w workers), join points (nested
  cases). Test programs: TypeclassTest(42), UnboxedTest(5050), WorkerWrapperTest(55), JoinPointTest(0)
- **Phase 3c: Cycle detection** ✓: Bacon-Rajan trial deletion cycle collector in
  `runtime/kk_cycle.c`, static cycle analysis in `Core/CycleAnalysis.hs`, 10/10 C
  tests (pairs, triples, self-ref, mixed), 8 new K tests for RC on heap objects
- **Runtime**: Perceus RC with cycle collection, recursive child dropping, nfields
  side table, color encoding in refcount word
- **Test suite**: 39 cabal tests (unit + property + bisimulation), 5 polyglot E2E,
  K test oracle, 112 krun tests, 10 cycle collector C tests
- **End-to-end**: `--demo --compile` → 3628800, 4-language polyglot → 69/1/144

### Recent Commits
- Phase 3c: Cycle detection — Bacon-Rajan collector, static analysis, C tests, K tests
- Phase 3b: GHC Core patterns — primops, lambda-not-thunk, Bool→i64, negate, test programs
- Phase 3a: Haskell RC feasibility — Factorial.hs E2E, I# simplification, print builtin, profiled runtime
- Phase 2d: extended kprove claims (120 total: bridge, evidence, linker)
- Phase 2c: bridge bisimulation proofs (GHC, Koka, Rust, Mercury)
- `ac1a533` — Phase 1b: polyglot test suite, Mercury choice effect (multi-shot)
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
organ-ir.k → kompile (LLVM backend) → krun tests (104 pass)
organ-ir.k → kompile (Haskell backend) → kprove perceus-claims.k (20 verified)
all-claims-def.k → kompile (Haskell backend) → kprove bridge-claims.k (67 verified)
all-claims-def.k → kompile (Haskell backend) → kprove evidence-claims.k (13 verified)
all-claims-def.k → kompile (Haskell backend) → kprove linker-claims.k (20 verified)
bridge-properties.k → kompile (LLVM backend) → krun tests (47 pass)
bridge bisimulation → krun(translate(source)) == native compiler (7 tests)
K oracle → krun(random_expr) == MLIR_pipeline(random_expr) (70 property tests)
```
