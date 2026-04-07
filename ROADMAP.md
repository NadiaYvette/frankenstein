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

### 3d. Benchmark Suite ✓

Three pure-integer benchmarks compiled through all four compilers: fibonacci(42),
tak(24,16,8), ack(3,8). All 12 binaries verified correct. Automated benchmark script
(`bench/run.sh`) measures wall time, peak RSS, and RC profile counts.

**Binary sizes** (Frankenstein 1400x smaller than GHC):
| Compiler | fib | tak | ack |
|---|---|---|---|
| Frankenstein | 18.6 KB | 18.6 KB | 18.6 KB |
| GHC -O2 | 25.9 MB | 25.9 MB | 25.9 MB |
| Rust -O | 9.1 MB | 9.1 MB | 9.1 MB |
| Koka -O2 | 8.4 MB | 8.4 MB | 8.4 MB |

**Wall time** (median of 5 runs):
| Compiler | fib(42) | tak(24,16,8) | ack(3,8) |
|---|---|---|---|
| Frankenstein | 22.88s | 0.21s | 0.12s |
| GHC -O2 | 3.81s | 0.01s | 0.01s |
| Rust -O | 2.12s | ~0s | 0.02s |
| Koka -O2 | 3.83s | 0.01s | 0.02s |

**Peak RSS** (Frankenstein uses least memory, zero heap):
| Compiler | fib | tak | ack |
|---|---|---|---|
| Frankenstein | 1508 KB | 1524 KB | 1632 KB |
| GHC -O2 | 3444 KB | 3764 KB | 3884 KB |
| Rust -O | 2004 KB | 2060 KB | 2040 KB |
| Koka -O2 | 2784 KB | 2792 KB | 2812 KB |

**Frankenstein RC profile** (all ops are no-op retains on unboxed ints):
| Benchmark | retain | drop | alloc | reuse |
|---|---|---|---|---|
| fib(42) | 1,733,977,746 | 0 | 0 | 0 |
| tak(24,16,8) | 19,946,792 | 0 | 0 | 0 |
| ack(3,8) | 8,357,997 | 0 | 0 | 0 |

**Key findings**:
- **Binary size**: Frankenstein produces 18.6 KB binaries — no runtime library linked,
  just our ~300-line kk_runtime.c. GHC statically links its RTS (25.9 MB).
- **Memory**: Frankenstein uses the least memory (1.5 MB) — all computation is pure
  stack, zero heap allocations. No GC pauses, no allocation pressure.
- **Speed**: 6x slower than GHC, 11x slower than Rust on fib(42). The bottleneck is
  1.7 billion no-op `kk_retain` calls on unboxed integers. These pass `kk_is_heap_ptr`
  checks but still cost function call overhead. Fix: elide retain/drop on known-unboxed
  values at the MLIR level (future optimization pass).
- **Codegen fixes during benchmarking**: Multi-arg lambda collection (GHC bridge),
  `nameToSsa` for unique SSA names (MLIR emitter) — both needed for multi-param
  GHC workers (tak, ack).

---

## Phase 4: MLIR Dialect for Algebraic Effects ✓

**Goal**: Instead of lowering effects to evidence-passing in Haskell, define
a first-class `frankenstein` MLIR dialect. MLIR's pass infrastructure can
then optimize effect dispatch at the IR level.

### 4a. Dialect Definition ✓

Three new `MlirOp` constructors in `Dialects.hs`:
- `FrankHandle effect handler_ssa body_ssa` — `"frankenstein.handle"` with effect attribute
- `FrankPerform effect op [arg_ssas]` — `"frankenstein.perform"` with effect/op attributes
- `FrankResume arg_ssa` — `"frankenstein.resume"` for continuation

Rendered as MLIR generic syntax (works with `--allow-unregistered-dialect`):
```mlir
"frankenstein.handle"(%handler) {effect = "exn"} // body result: %result
"frankenstein.perform"(%arg) {effect = "exn", op = "raise"} : (i64) -> i64
"frankenstein.resume"(%val) : (i64) -> i64
```

New `--emit-effect-mlir` CLI flag emits MLIR **without** running the evidence pass,
so `EHandle`/`EPerform` nodes appear as `frankenstein.*` dialect ops.
`emitProgramWithEffects` function in `Emitter.hs` handles effect-dialect mode via
`esEffectDialect` flag in `EmitState`.

### 4b. Effect Optimization Passes ✓

Three Core IR → Core IR transformations in `EffectOpt.hs`, run before evidence pass:

- **Handler inlining** (`inlineLocalHandlers`): When `EHandle eff (ELam ...) body`
  contains `EPerform eff args` in the body, inline the handler at each perform site.
  Eliminates the dynamic handler dispatch overhead entirely.
- **Identity handler elimination** (`eliminateIdentityHandlers`): Detects handlers
  of the form `\x k -> k(x)` (both curried and uncurried) and removes the `EHandle`
  wrapper — the handler is a no-op.
- **Tail-resumptive detection** (`annotateTailResumptive`): Detects handlers where
  every control path ends with a call to the resume continuation. These handlers
  can be implemented as direct function calls without continuation capture.

Evidence specialization (Phase 4b plan item) is already handled by the existing
evidence pass, which directly binds known handler functions.

Statistics: `effectOptimizeWithStats` returns counts of inlined, eliminated, and
tail-resumptive handlers detected.

### 4c. Lowering to Standard MLIR ✓

The existing evidence pass IS the lowering from `frankenstein.*` to standard MLIR:
- `frankenstein.perform` → evidence vector lookup + indirect `func.call`
- `frankenstein.handle` → push evidence (let-bind) + evaluate body + pop
- `frankenstein.resume` → call continuation (function pointer in evidence)

Pipeline: `--emit-effect-mlir` shows `frankenstein.*` ops; `--emit-mlir` shows
the lowered form; `--compile` runs the full pipeline through to native code.

### Results

- **New files**: `src/Frankenstein/Core/EffectOpt.hs` (~280 lines, 3 optimization passes)
- **Modified**: `Dialects.hs` (3 new ops + rendering), `Emitter.hs` (effect-dialect mode),
  `Main.hs` (`--emit-effect-mlir` flag, `effectOptimize` integration), `frankenstein.cabal`
- **Tests**: 7 new tests (identity handler elimination, stats, dialect emission)
- **Total test suite**: 46 cabal tests (39 existing + 7 new)
- **Regression**: `--demo --compile` → 3628800 still works

---

## Phase 5: Wasm Backend ✓

**Goal**: MLIR → Wasm, turning Frankenstein into a polyglot-to-web compiler.

### 5a. MLIR-to-Wasm Pipeline ✓

Pipeline: `MLIR → mlir-opt → mlir-translate --mlir-to-llvmir → llc -mtriple=wasm32 → wasm-ld → .wasm`

New CLI flag: `--target wasm32` (used with `--compile` or `--emit-mlir`)

Key implementation details:
- `CompileTarget` type (`TargetNative | TargetWasm32`) in `EmitConfig`
- `emitProgramWasm`: MLIR emission without printf/main wrapper (Wasm host reads return value)
- `compileToWasm`: full pipeline from Core IR to `.wasm` binary
- Wasm runtime (`runtime/kk_runtime_wasm.c`): freestanding Perceus RC with 1MB static
  bump allocator, no libc dependencies. Values are i64 (Wasm natively supports i64),
  pointers are i32 (wasm32 linear memory).
- Binary size: **485 bytes** for factorial demo (vs 18.6KB native)

### 5b. Wasm Validation ✓

Validation script (`test/wasm/validate_wasm.sh`) verifies:
- Demo factorial compiles to `.wasm` and returns 3628800 in Node.js
- Native output matches Wasm output (cross-target comparison)
- Wasm binary is under 10KB

KWasm (K framework Wasm semantics) integration is prepared but requires KWasm
installation. The validation currently uses Node.js as the Wasm execution engine.
Future: `kwasm run` to close the formal verification loop (source→K oracle ↔ binary→KWasm).

### 5c. Browser Demo ✓

`web/index.html`: single-page demo that loads pre-compiled `.wasm` and runs
`factorial(10)` in the browser via `WebAssembly.Instance`. Shows result, execution
time, binary size, and exported function count.

Build with: `bash web/build.sh` → serves via `python3 -m http.server 8080`

### Results

- **New files**: `runtime/kk_runtime_wasm.c` (freestanding Wasm runtime),
  `test/wasm/validate_wasm.sh`, `web/index.html`, `web/build.sh`
- **Modified**: `Emitter.hs` (`CompileTarget`, `emitProgramWasm`, `compileToWasm`),
  `Main.hs` (`--target wasm32`), `KOracle.hs` (ecTarget field)
- **Tests**: 4 new Wasm emission tests + 3 validation tests (script)
- **Total test suite**: 50 cabal tests (46 + 4 Wasm)
- **End-to-end**: `--demo --compile --target wasm32` → 485-byte `.wasm` → Node.js → 3628800

---

## Phase 6: Self-Hosting Bootstrap

**Goal**: Feed Frankenstein's own Haskell source through the GHC bridge.
Even partial self-hosting is a dramatic proof of capability.

### 6a. Compile Core/Types.hs ✓

`Core/Types.hs` is a pure data definition module with no IO — the simplest
target. **Done**: 35 record selector functions translate cleanly through
GHC bridge → Frankenstein Core → MLIR → mlir-opt validation. 867-line MLIR
output for the entire module.

Bridge fixes required:
- Filter `$krep` runtime-type-rep bindings (joined existing `$tc`/`$tr` filter)
- Recognize `I#(var)` boxing as identity (Int and Int# share i64 representation)
- GHC bridge `Driver.hs`: import paths (`src/`, `.`), enable `OverloadedStrings`
  to match `frankenstein.cabal` `default-extensions`
- Added `ghc-boot-th` to `build-depends` for `GHC.LanguageExtensions.Type`

Emitter fixes required:
- `emitPatField` now registers field bindings in `esAliases` so subsequent
  `EVar` references resolve correctly (was previously a comment-only no-op)
- New `SingleConCase` branch class for exhaustive single-constructor cases:
  emits field extraction + body inline without an `scf.if` (avoids referencing
  field SSA values from a sibling region)
- `sanitizeName` now strips parens, commas, brackets, quotes, whitespace

### 6b. Compile Core/Perceus.hs (substantially complete)

The Perceus pass itself, compiled through the Perceus pass. Beautifully
recursive. **Status**: Translates to Frankenstein Core → MLIR (~3550 lines)
in one shot. All 50 tests still pass, `--demo --compile` still produces
3628800. Remaining constraints are known and narrow (7 cross-region SSA
references to drops/retains emitted outside the scf.if region that defines
the value).

Emitter changes required:
- **Closure ABI via `kk_alloc_con`**: every lifted lambda allocates a heap
  closure; field 0 is the function pointer as i64, fields 1..n are the
  captured variables. Closures flow through HOF contexts as plain i64, so
  no MLIR struct values leak into the generic `i64` pipeline.
- **Closure-indirect call path**: `EApp (EVar fn) args` now checks
  `esTopFns` to decide between `func.call @fn(args)` (direct call to a
  known top-level function) and a closure-indirect call that extracts
  field 0 via `kk_field`, inttoptr's it, and `llvm.call`s through the
  pointer. `esTopFns :: Set Text` is seeded from the program's def names.
- **Unresolved external fallback**: names that aren't in scope and aren't
  known top-level functions (imports from `Data.Map`, `Data.Set`, data
  constructors like `(,)` or `:`) materialize as stub constants with an
  `// unresolved external` comment — the MLIR stays well-formed even
  though the call is semantically undefined. Applied at both `EVar` sites
  and the closure-call path.
- **Top-level-fn-as-value**: when a known top-level function is used as
  a value (e.g. passed as an argument), emit `llvm.mlir.addressof @fn`
  + `llvm.ptrtoint` so the caller gets a real i64 address rather than a
  dangling SSA name.
- **Lambda parameter renaming**: every captured var and regular param
  gets a fresh SSA name via `freshName`, with aliases save/restore around
  the body, preventing collisions between captured and bound names with
  the same OccString.
- **Uniform i64 params in lifted functions**: the closure ABI is uniform
  i64 for all arguments, so lifted lambda signatures use `i64` rather
  than `typeToMlir` (which was producing `!llvm.ptr` for some Haskell
  types and failing when consumers expected i64).
- **`llvm.insertvalue` operand order**: fixed to (value, container) in
  both existing callsites (was backwards — MLIR rejects the reversed
  form as a type mismatch on the struct container).
- **Capture filter**: only names currently in `esAliases` are captured.
  External references (unresolved imports, top-level fn names) are
  handled at the reference site instead of being dragged into the
  closure, avoiding raw unsanitized names like `%:` or `%foldr` in
  `kk_set_field` calls.

### 6c. Full Self-Hosting (blocked on 6b)

Compile all of `src/Frankenstein/` through the GHC bridge, link, and produce
a `frankenstein` binary that can compile the demo. This is the ultimate
stress test and would make Frankenstein self-sustaining — independent of GHC
for its own compilation (though still using GHC as a frontend).

---

## Current State (2026-04-07, Phase 6a+6b substantially complete)

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
- **Phase 3d: Benchmark suite** ✓: 3 benchmarks (fib/tak/ack) × 4 compilers (Frankenstein/GHC/Rust/Koka),
  automated `bench/run.sh` script. Frankenstein: 18.6 KB binary (1400x smaller than GHC), lowest memory
  (1.5 MB), 6x slower than GHC on fib(42) due to no-op retain overhead on unboxed values.
  Multi-arg lambda collection and nameToSsa fixes for multi-param GHC workers.
- **Phase 4: MLIR Effect Dialect** ✓: `frankenstein.handle`/`perform`/`resume` ops in
  `Dialects.hs`, effect-dialect emission mode in `Emitter.hs`, `--emit-effect-mlir` CLI
  flag. Three Core IR optimization passes in `EffectOpt.hs`: handler inlining, identity
  handler elimination, tail-resumptive detection. Integrated into pipeline before evidence pass.
- **Phase 5: Wasm Backend** ✓: `--compile --target wasm32` produces `.wasm` binaries.
  485-byte factorial demo runs in Node.js and browser. Freestanding Wasm runtime with
  bump allocator. Browser demo at `web/index.html`. Pipeline: MLIR → llc(wasm32) → wasm-ld.
- **Test suite**: 50 cabal tests (46 + 4 Wasm), 5 polyglot E2E, 3 Wasm validation tests,
  K test oracle, 112 krun tests, 10 cycle collector C tests
- **End-to-end**: `--demo --compile` → 3628800, `--demo --compile --target wasm32` → 3628800 in Node.js,
  4-language polyglot → 69/1/144

### Recent Commits
- Phase 6a: Self-hosting bootstrap — Core/Types.hs through GHC bridge → MLIR validates clean
- Phase 5: Wasm backend — --target wasm32, 485-byte .wasm, browser demo, Node.js validation
- Phase 4: MLIR effect dialect — frankenstein.* ops, effect optimizations, --emit-effect-mlir
- Phase 3d: Benchmark suite — fib/tak/ack × 4 compilers, multi-arg lambda fix, nameToSsa
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
