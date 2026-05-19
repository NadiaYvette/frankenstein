# Frankenstein Roadmap

## Vision

Frankenstein is the only compiler that combines formally verified reference
counting (Perceus, K-proven), four real compiler frontends stolen from
production compilers (GHC, rustc, mmc, Koka), algebraic effects as the
unified cross-language abstraction, and MLIR as the backend. This roadmap
charts the path from working prototype to research contribution.

---

## Phase 1: The Polyglot Promise Made Real ✓

**Goal**: A single binary where 4 functions in 4 languages compose through
shared algebraic effects. This is the demo that makes people understand
what Frankenstein is.

### 1a. Cross-Language Calling Convention ✓

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

### 1b. Polyglot Test Suite ✓

Create `examples/polyglot-demo/` with the 4-language program above plus
a test script that:
1. Compiles each file individually through its bridge (`--emit-core`)
2. Links them together
3. Compiles to native
4. Verifies output against K oracle (see Phase 2)

---

## Phase 2: K as the Living Specification ✓

**Goal**: Make K Framework the source of truth for the entire IR, not just
Perceus. Every transformation verified against a formal model.

### 2a. Full OrganIR Operational Semantics in K ✓

`organ-ir.k` (1229 lines, 240 rules) is a complete executable semantics:
- **Evaluation rules** ✓: `EApp`, `ELam` (closure-based beta-reduction), `ELet`,
  `ECase` (pattern matching with PatLit/PatCon/PatVar/PatWild), `EDelay`/`EForce`
  (thunk capture/memoization), `ETypeApp`/`ETypeLam` (type erasure)
- **Effect semantics** ✓: `EPerform` searches the handler stack via delimited
  continuation capture, `EHandle` pushes/pops handlers on `<effectStack>`, full
  abort and resume (tail-resumptive) patterns, nested handler support
- **Memory model** ✓: Perceus operations (`ERetain`, `EDrop`, `ERelease`, `EReuse`)
  as semantic no-ops in the reference interpreter, store-based variable binding
- **Builtins** ✓: Arithmetic (`+`, `-`, `*`, `/`, `mod`), comparisons
  (`==`, `<`, `>`, `<=`, `>=`), string concat (`++`), negate
- **118 krun shell tests** passing (typing, free vars, usage counting,
  Perceus transforms, evaluation, effects, Mercury semidet/choice patterns,
  bridge properties)

### 2b. Property-Based Testing via K Oracle ✓

`test/KOracle.hs` (479 lines):
- 4 QuickCheck properties: random pure, arithmetic, let+case, effect expressions
- Generators: `genArithExpr`, `genLetCaseExpr`, `genEffectExpr`
- Compares `krun(eval(expr))` against MLIR pipeline → native binary output
- Integrated into `cabal test` (13 krun tests in test suite, all passing)

### 2c. Bridge Bisimulation Proofs ✓

`test/BridgeBisim.hs`: 8 bridge test suites verifying translation preserves
observable behavior:
- **GHC bridge**: `krun(translateGHC(Arith.hs)) == 44`, factorial == 3628800,
  krun == ghc native comparison
- **Koka bridge**: `krun(translateKoka(arith.kk)) == 44`
- **Rust bridge**: `krun(translateRust(arith.rs)) == 44`, krun == rustc native
- **Mercury bridge**: structural tests (OrganIR defs produced)
- **Python bridge**: `krun(translatePython(arith.py)) == 44`
- **Go bridge**: `krun(translateGo(arith.go)) == 44`
- **Futhark bridge**: `krun(translateFuthark(arith.fut)) == 44`
- **Scheme bridge**: structural tests (main Def, call/cc)

### 2d. Extend kprove Claims ✓

313 claims across 5 files, verifiable via `k-specs/tests/run-kprove.sh`:
- **Perceus claims** (43): free var analysis, drop insertion, retain for
  multi-use, lambda scope drops, identity properties
- **Evidence claims** (30): no EHandle/EPerform post-pass, single-op/multi-op
  projection with evv_select, unhandled-effect fallthrough
- **Bridge claims** (155): GHC (lazy/strict/forall), Koka (constructor rewriting),
  Rust (ownership/affine), Mercury (determinism mapping), Python, Go, Futhark, Scheme
- **Linker claims** (44): name rewriting preserves local scope, main unmangled,
  mangling deterministic, cross-module resolution
- **EffectOpt claims** (41): identity handler detection, tail-resumptive detection,
  handler inlining substitution correctness

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

**Binary sizes** (Frankenstein 680x smaller than GHC):
| Compiler | fib | tak | ack |
|---|---|---|---|
| Frankenstein | 38.0 KB | 38.0 KB | 38.0 KB |
| GHC -O2 | 25.9 MB | 25.9 MB | 25.9 MB |
| Rust -O | 9.1 MB | 9.1 MB | 9.1 MB |
| Koka -O2 | 8.4 MB | 8.4 MB | 8.4 MB |

**Wall time** (median of 5 runs, after unboxed elision):
| Compiler | fib(42) | tak(24,16,8) | ack(3,8) |
|---|---|---|---|
| Frankenstein | 1.88s | 0.01s | 0.01s |
| GHC -O2 | 3.20s | 0.02s | 0.01s |
| Rust -O | 1.72s | 0.01s | 0.02s |
| Koka -O2 | 3.04s | 0.02s | 0.03s |

**Peak RSS** (Frankenstein uses least memory, zero heap):
| Compiler | fib | tak | ack |
|---|---|---|---|
| Frankenstein | 1876 KB | 2024 KB | 2020 KB |
| GHC -O2 | 3436 KB | 3620 KB | 3744 KB |
| Rust -O | 1964 KB | 2008 KB | 2068 KB |
| Koka -O2 | 2816 KB | 2784 KB | 2776 KB |

**Frankenstein RC profile** (zero RC ops after unboxed elision):
| Benchmark | retain | drop | alloc | reuse |
|---|---|---|---|---|
| fib(42) | 0 | 0 | 0 | 0 |
| tak(24,16,8) | 0 | 0 | 0 | 0 |
| ack(3,8) | 0 | 0 | 0 | 0 |

**Key findings**:
- **Binary size**: Frankenstein produces 38 KB binaries (kk_runtime + kk_arena + kk_cycle).
  GHC statically links its RTS (25.9 MB), 680x larger.
- **Memory**: Frankenstein uses the least memory (1.9 MB) — all computation is pure
  stack, zero heap allocations. No GC pauses, no allocation pressure.
- **Speed**: Frankenstein matches Rust on fib(42) (1.88s vs 1.72s) and beats both GHC
  (3.20s) and Koka (3.04s). On tak/ack all four compilers are within measurement noise.
- **Unboxed elision** (commit faa5319): The Perceus pass now skips retain/drop for
  values of known-unboxed types (Int, Char, Bool, Word, etc.) via `isUnboxedType`.
  This eliminated 1.73 billion no-op retain calls on fib(42), reducing wall time from
  22.88s to 1.88s — a 12.2x speedup. The RC profile shows zero runtime overhead.
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

### 6c. Full Self-Hosting (substantially complete)

Feed all 18 Haskell modules in `src/Frankenstein/` through the GHC bridge to MLIR.
**Result**: 3 modules (Types, KokaCore, KokaBridge.Driver) emit fully valid MLIR
that passes `mlir-opt --allow-unregistered-dialect` with zero errors. 14 modules
emit MLIR with 1–12 remaining errors out of thousands of lines each (e.g., Emitter
at 43,581 lines has just 1 remaining error; Linker at 14,344 lines has 5).
Only 1 module is a hard blocker: `OrganIR.Consumer` fails at the bridge layer
due to an external `text-2.1.3` vs `text-2.1.4` version conflict between our
session and the `organ-ir` package — outside our control. All 50 cabal tests
still pass and `--demo --compile` still produces 3628800.

GHC bridge Driver changes:
- **Module matching**: `runGhcCompile` now searches the module graph for the
  module whose `ml_hs_file` matches the input path, instead of taking the head
  (which picked an arbitrary dependency when the target imported other modules)
- **Language extensions**: enable `LambdaCase`, `BangPatterns`, `TupleSections`,
  `ScopedTypeVariables`, `Derive{Functor,Foldable,Traversable}`,
  `GeneralizedNewtypeDeriving`, `Flexible{Contexts,Instances}`, `RecordWildCards`,
  `NamedFieldPuns`, `MultiParamTypeClasses` to match `frankenstein.cabal`
- **Package visibility**: expose `ghc`, `koka`, `organ-ir` packages via
  `packageFlags` so our own modules can self-host
- **Package DBs**: add cabal store + dist-newstyle package DBs via
  `packageDBFlags` so inplace builds of our dependencies are discoverable

Emitter changes required:
- **Top-level param aliases**: `emitDef` now installs function parameters as
  identity entries in `esAliases` so `EVar` lookups find them (was relying on
  implicit SSA name matching before the alias-lookup rewrite)
- **`scf.if` branch alias scoping**: `emitConChain` now saves/restores
  `esAliases` around each case arm so pattern-bound field SSA values don't
  leak across sibling regions — the single biggest fix, dropping most
  modules from 100+ errors to 1–12
- **Thunk with captures**: `EDelay` bodies with captured free variables are
  inlined (degraded laziness) rather than lifted to a zero-arg thunk that
  can't reach the captures
- **MLIR string escaping**: `escapeMLIRString` now hex-escapes all non-printable
  chars via `printf "\%02X"` — raw control bytes would otherwise break MLIR's
  string literal parser
- **`func.constant` + `unrealized_conversion_cast`**: closure function pointers
  and thunk bodies use `func.constant @fn : ty` then cast to i64, because
  `llvm.mlir.addressof` rejects `func.func` references. `mlir-opt
  --reconcile-unrealized-casts` cleans these up after inlining

The remaining 1-error-per-module pattern is partial application of top-level
functions (callee has N params, call site supplies M<N args) — would require
eta-expanding partial applications into closures. Left as future work.

### 6d. Self-Hosting Cleanup ✓

**Result**: 17 of 18 modules emit fully valid MLIR. The 18th, `OrganIR.Consumer`,
fails earlier in the GHC frontend due to a pre-existing `text-2.1.3`/`2.1.4`
package skew unrelated to MLIR emission.

Emitter changes:
- **PAP closures**: `emitPapClosure` allocates a heap closure via `kk_alloc_con`
  (field 0 = wrapper fptr, fields 1..n = supplied args). `ensurePapWrapper`
  emits a per-(fn, supplied-arity) wrapper that loads captured args from the
  closure and tail-calls the original. Fires when `nArgs < arity` at a
  top-level call site.
- **Oversaturated path**: when `nArgs > arity`, call the top-level fn with the
  first `arity` args, then closure-indirect the remainder via field-0 fptr
  extraction (`kk_field`/`inttoptr`/`llvm.call`).
- **Uniform i64 ABI**: all top-level fn params and return types are i64,
  matching the closure ABI and avoiding `!llvm.ptr` leaking into kk_* runtime
  calls and PAP wrappers.
- **String literals → i64**: `ELit (LitString)` immediately `ptrtoint`s the
  global address so it lives in the i64 universe.
- **ELet alias scoping**: save/restore `esAliases` around the let body so
  bindings don't leak into sibling `scf.if` branches as undeclared SSA refs.
- **`ETypeLam` stripping in `emitDef`**: matches `buildTopFnArity` so emitted
  arity equals the call-site arity table (fixed `KokaBridge.CoreTranslate`'s
  `foldlM` mismatch).
- **No `llvm.unreachable` in unhandled-case fallback**: it was illegal as a
  non-terminator inside `scf.if` regions.

### 6e. Self-Hosted MLIR → Native Objects ✓

**Result**: All 17 self-hosted modules lower cleanly through
`mlir-opt → mlir-translate → clang -c` and produce real ELF objects, totalling
~1.8 MB. The biggest is `Emitter.o` at 702 KB.

| Module | .o size |
|--------|---------|
| Core/Types | 13 KB |
| Core/CycleAnalysis | 24 KB |
| KokaBridge/Driver | 32 KB |
| GhcBridge/Driver | 37 KB |
| Core/KokaCore | 43 KB |
| Core/Evidence | 43 KB |
| Core/Perceus | 45 KB |
| MlirEmit/Dialects | 47 KB |
| Core/EffectOpt | 50 KB |
| GhcBridge/CoreTranslate | 53 KB |
| RustBridge/CoreTranslate | 58 KB |
| MercuryBridge/CoreTranslate | 58 KB |
| KokaBridge/CoreTranslate | 80 KB |
| MercuryBridge/HldsParse | 119 KB |
| Core/Linker | 188 KB |
| RustBridge/MirParse | 237 KB |
| MlirEmit/Emitter | **702 KB** |

Pipeline:
```
frankenstein <file.hs> --emit-mlir
  | mlir-opt --allow-unregistered-dialect --reconcile-unrealized-casts
             --convert-scf-to-cf --convert-arith-to-llvm
             --convert-cf-to-llvm --convert-func-to-llvm
             --reconcile-unrealized-casts
  | mlir-translate --mlir-to-llvmir
  | clang -c -o file.o
```

Emitter fix:
- **`func.constant` → i64 via `!llvm.ptr`**: every closure-fptr cast now goes
  `func.constant @fn : (...) -> ty` → `unrealized_conversion_cast` to
  `!llvm.ptr` → `llvm.ptrtoint` to i64. The previous one-shot
  `unrealized_conversion_cast` to i64 left a function-typed cast that
  `reconcile-unrealized-casts` couldn't erase, so `mlir-translate` rejected the
  IR with "LLVM Translation failed for operation:
  builtin.unrealized_conversion_cast". Going via `!llvm.ptr` lets
  `--convert-func-to-llvm` rewrite the `func.constant` to `llvm.mlir.addressof`
  and reconcile then folds the redundant `ptr → ptr` cast.

End-to-end runnable validation: linked self-hosted `Core/Types.o` against the
C runtime (`kk_runtime.c` + `kk_cycle.c`) and a small driver, called the
frankenstein-compiled `bindName`/`bindExpr` record selectors on a heap-allocated
`Bind` value, and got back the correct field values. **Frankenstein has now
bootstrapped a piece of itself end-to-end: source → GHC bridge → Core IR →
Perceus → MLIR → LLVM IR → ELF object → executed in process.**

### 6f. Self-Hosted Compiler End-to-End ✓

**Result**: Self-hosted compiler runs 21 end-to-end examples, including
standard library types (`[Int]`, `Maybe`, `Bool`, tuples), Prelude HOFs
(`map`, `filter`, `sum`), strings, cross-module compilation, and algebraic
effects. All 23 modules compile to native objects and link into a working
compiler binary.

---

## Phase 7: Self-Hosted Factorial Validation ✓

**Goal**: The self-hosted compiler produces correct MLIR for a nontrivial
program. **Result**: `factorial(10)` compiled through the self-hosted emitter
yields `3628800`. Pipeline: Core IR (C) → self-hosted `emitProgramText` → MLIR
→ mlir-opt → mlir-translate → clang → native binary → 3628800.

67 self-tests pass across 14 modules exercising all 4 compiler passes
(assignProgramTags, insertPerceus, evidencePass, emitProgramText).

---

## Phase 8: End-to-End Examples Through Self-Hosted Compiler ✓

**Goal**: 21 example programs compile correctly through the stage 1
self-hosted compiler. **Result**: 21/21 pass.

| Example | Output | Features |
|---------|--------|----------|
| nested | 60 | Nested ADTs |
| maybesum | 42 | Custom Maybe |
| listsum | 15 | Custom list |
| tree | 6 | Binary tree |
| alloc_stress | 100100000 | Heavy allocation + RC |
| closure | 42 | Closures |
| mutual_rec | 5 | Mutual recursion |
| multi_adt | 317 | Multiple ADTs |
| higher_order | 12 | Higher-order functions |
| exhaust_tail | 36 | Exhaustive patterns |
| stdlib_list | 15 | Standard `[Int]` |
| stdlib_maybe | 141 | Standard `Maybe Int` |
| stdlib_bool | 7 | Guards, `Bool` |
| stdlib_tuple | 13 | Standard `(Int, Int)` |
| prelude_hof | 22 | `map`/`filter`/`foldr` |
| prelude_inline | 24 | Prelude inlined HOFs |
| prelude_comprehensive | 235 | `take`/`zipWith`/`foldl` |
| stdlib_string | 11 | `String` = `[Char]` |
| cross_module | 45 | Multi-module GHC bridge |
| effect_ask | 84 | Algebraic effect (ask) |
| effect_state | 100 | Algebraic effect (state) |

---

## Phase 9: Bootstrap Loop ✓

**Goal**: Three-stage bootstrap — host compiler → stage 1 → stage 2 → stage 3
— with the self-hosted compiler reaching a fixed point (stage 2 and stage 3
produce byte-identical MLIR for all 23 modules).

### 9a. Stage 2 Compilation ✓

All 23 modules compile through the stage 1 self-hosted compiler to produce
stage 2 MLIR → LLVM IR → native objects. Large modules (>1MB OrganIR JSON)
are automatically split by size (400KB target per part), compiled separately,
and merged. JSON is minified (`separators=(',',':')`) to reduce parser load.

Pre-processing (Core IR, host compiler):
- `Frankenstein.Core.NormalizePatterns` — converts `PatCon True/False` to
  `PatLit 1/0` and appends `PatWild` defaults to exhaustive multi-constructor
  cases. Replaces the former `fix-bool-patterns.py` script (1,584 fixes/run).

Post-processing pipeline for stage 2/3 MLIR:

The three Python scripts that previously patched self-host runtime
divergences have been absorbed into a single Haskell module
`Frankenstein.MlirEmit.PostProcess`, invoked via the host compiler's
`--postprocess-mlir` CLI flag. Architectural immunity: this code runs in
the GHC-compiled host binary, never in the self-hosted compiler, so
the runtime bugs it works around cannot affect it.

Active post-processing:
- `Frankenstein.MlirEmit.PostProcess` (Haskell) — replaces
  fix-intra-module-calls / fix-dollar0-refs / fix-mlir-arity.
  See `k-specs/postprocess-claims.k` for the formal immunity claim.
- `merge-mlir-parts.py` — deduplicate `func.func` across split parts
  (inherent to split compilation, not a divergence workaround)
- `extract-mlir-funcs.py` — per-part fallback extraction when a split
  part crashes (inherent to split-compile robustness)

Eliminated post-processing scripts (8 of 8 divergence-workaround scripts):
- `fix-bool-patterns.py` → Haskell `NormalizePatterns` pass
- `fix-captures.py` → dead code (already-fixed lambda-lift bug)
- `fix-fld-refs.py` → dead code (superseded by `A_sanitize_shim.c`)
- `fix-missing-else.py` → dead code (already-fixed emitter bug)
- `fix-orphan-decls.py` → Haskell EFunRef declaration in emitter
- `fix-intra-module-calls.py` → Haskell `PostProcess.fixIntraModuleCalls`
- `fix-dollar0-refs.py` → Haskell `PostProcess.fixDollar0Refs`
- `fix-mlir-arity.py` → Haskell `PostProcess.fixMlirArity`

Bootstrap fixed point: 24/24 modules match between stage 2 and stage 3,
21/21 E2E tests pass at every stage.

### 9b. Stage 2 Linking ✓

Stage 2 compiler binary: ~6.3 MB. Links against the same C runtime
(kk_runtime.c, kk_cycle.c, shim_*.c, text shims, JSON parser).

### 9c. Stage 2 Verification ✓

**Result**: 21/21 end-to-end examples pass through the stage 2 compiler,
producing identical outputs to the host compiler and stage 1.

### 9d. Stage 3 — Fixed-Point Convergence ✓

**Result**: All 23 modules compiled through the stage 2 compiler produce
**byte-identical MLIR** to stage 2 output. The self-hosted compiler has
reached a fixed point — it reproduces itself.

Pipeline: same 23 OrganIR JSON files → stage 2 compiler → stage 3 MLIR,
with the same 7 post-processing scripts applied. Stage 3 binary linked
and passes all 21 E2E tests.

**Key fix for convergence**: The monolithic `emitAppVar` function (~490 lines,
936KB OrganIR definition) caused the self-hosted compiler to hang indefinitely
during compilation. Refactored into 6 arity-dispatched functions (`emitAppVarWith0`
through `emitAppVarWith3` + `emitAppVarGeneral`), each producing a manageable
OrganIR definition that compiles in seconds.

**BOOTSTRAP FIXED POINT**: host → stage 1 → stage 2 → stage 3, all 23/23
modules match, all 21 E2E tests pass at every stage.

### Outstanding Issues

**Strict s2≡s3 fixed point: ACHIEVED 2026-05-19** (was 9-11/26 historically).
Bootstrap reports `*** FIXED POINT REACHED ***` — 26/26 byte-equal, 21/21 E2E
at every stage. Resolution: clean rebuild fixed driver.o staleness (9-11/26 →
19/26), then four `PostProcess.hs` bug fixes resolved the remaining 7 stage-3
emit failures (commit `9cd995f`). See `docs/strict-fixed-point.md` for
details.

- **Self-host runtime: pattern-match dispatch on ADT constructors is
  systematically wrong** — still latent, still worked around by
  PostProcess. Minimum reproducer saved at
  `examples/db7_reproducers/wrong_default_body.hs` (8 lines).
  Source-level refactor of `classifyBranches.defaultBranch` was
  attempted 2026-05-19 (case-of-list → `filter` + `null` + `head`) —
  caused massive regression (26/26 → 3/26, 21/21 → 6/21), reverted.
  Confirms the original observation: every refactor attempt triggers
  a different manifestation. The bug is in compiled-output shape
  fragility, not in any one Haskell construct. Future fix needs
  MLIR-level instrumentation of `classifyBranches` itself, not
  source-level rewrites. The `tools/diff-tester/`
  `--mode=host-runtime-vs-stage2-runtime` driver auto-finds new
  instances.
  Three observable consequences (all worked around by PostProcess,
  which runs in the host binary and is architecturally immune):
  1. `emitConChain`'s `mDefaultExpr` flips Nothing → Just, emitting
     dead-code references to out-of-scope pattern binders.
  2. Split-compile `$N` externals not resolving (each part only sees
     its own `esTopFns`).
  3. Lambda-lift capture-dropping at some `func.call` sites.
- **`sanitizeName` corruption**: Root cause identified — `T.concatMap encodeChar`
  in the self-hosted binary non-deterministically corrupts characters
  (closure dispatch or UTF-8 iteration). Mitigated by `A_sanitize_shim.c`.

- **BRIDGE_haskell_strings**: `main :: String`, `main = putStrLn "..."`,
  and chained `do { … }` blocks all print natively.  `main :: String`
  routes through `kk_println_haskell_chars` (cons-list walker using the
  hash-based tags `stableConTag "[]" = 31636` and `stableConTag ":" = 46589`).
  `putStrLn` (which GHC inlines to `hPutStr2 stdout list True` from
  `GHC.Internal.IO.Handle.Text`) is intercepted by `ghcIoOutputRuntime`
  in `Frankenstein.GhcBridge.CoreTranslate` and rewritten to a direct
  `println_haskell_chars` / `print_haskell_chars` call.  Multi-step
  `do`-blocks work because GHC keeps an explicit `\s -> ...` lambda
  binding the state token; `applyMainIfFunctionAlias` detects when the
  user's `main` is just an `EVar` reference to a lambda-bodied helper
  and rewrites the alias to apply it with a dummy state arg.  Codepoints
  > 127 are written as raw bytes (Latin-1ish); full UTF-8 re-encoding
  is future work.  `show :: Int -> String`, `print :: Int -> IO ()`, `show [Int]`,
  `show (Maybe Int)` (`Just n` / `Nothing`) all now work natively.
  Routing:
    - Int show: `isShowIntWorker` recognises `$w$cshowsPrec2` and
      `$fShowCallStack_itos'`, routed to `int_to_haskell_chars`.
    - [Int] show: `isShowIntListMethod` recognises `$fShowInt_$cshowList`,
      routed to `int_list_to_haskell_chars` (formats `[n1,n2,n3]`).
    - Maybe show: `knownShowCAF` recognises the prefix CAFs that GHC
      emits (`$fShowMaybe1` = "Just ", `$fShowMaybe3` = "Nothing"),
      inlining the literal `[Char]` cons-list at translation time.
    - `unpackAppendCString#` (both Lit and Var forms) handled in trExpr
      so the prefix CAF lookup composes with `unpackAppendCString# pfx (show inner)`.
    - `isDictArg` no longer filters `$fShow*` CAFs with digit suffixes
      (they're CString CAFs, not dictionaries) — without this, the
      first arg of `unpackAppendCString#` was being silently dropped.

  `deriving Show` for user ADTs now works for three common shapes:
    - Enum-only (`data Color = Red | Green | Blue deriving Show`)
    - Single-constructor with args (`data Pair = Pair Int Int deriving Show`)
    - Multi-constructor uniformly with-args
      (`data Tree = Leaf | Node Tree Int Tree deriving Show`)

  The pieces wired in:
    - `kk_haskell_chars_concat` runtime helper (`(++) :: [Char] -> [Char] -> [Char]`)
      with both `kk_*` and bare-name aliases for PAP wrappers.
    - `knownShowCharCAF` inlines `showSpace1` = `' '`,
      `$fShowCallStack2` = `')'`, `$fShowCallStack3` = `'('`,
      `$fShowCallStack4` = `','`.
    - `GHC.Internal.Show.itos` (unprimed) recognised by
      `isShowIntWorker`/`pickShowArgs` as a 2-arg int-to-cons-list helper.
    - `isDictDef` keeps `$cshowsPrec`/`$cshow`/`$ccompare`/`$cfmap` etc.
      (derived methods) while still filtering `$cshowList` (whose body
      references `showList__`, currently unshimmed).

  Mixed-ADT modules now work: `disambiguateLetBindings` post-pass in
  `Frankenstein.GhcBridge.CoreTranslate` gives every let-binding a
  fresh Unique by adding a per-site counter offset, so no two
  binding sites can collide.  Previously the lambda-lifter
  deduplicated by name and picked the capture set from whichever
  site it saw first; call sites in the other context then emitted
  the wrong argument count.

  Show for tuples works for shapes including 2-tuples, n-tuples
  (3, 4, 5, …), tuples with negative numbers, and nested tuples
  (e.g. `((Int, Int), Int)`).  The key bridge piece:
    - `$fShowCallStack_$sgo(showFn1, [showFn2, …, showFnN], tail)`
      is unrolled at translation time via `expandTupleShowChain` to
      `showFn1 (',' : showFn2 (',' : … (',' : showFnN tail) …))`.
      The middle list is a static cons-list of closures that GHC
      fully inlines for fixed-arity tuples; `collectStaticShowList`
      walks it at translation time.
    - Numbered CAFs `showList__N`, `$fShowCallStack8` (minus sign),
      etc. added to knownShowCharCAF/knownShowCAF.
    - `_$cshowList` matching anywhere in the def name is filtered,
      including the `$s$f...` specialised variants.
    - External GHC `$cshowList` references (via the never-forced
      thunk in the Show dict's showList slot) are routed through
      a `dummy_show_caf` runtime stub so the dict links cleanly.

  Still blocked: reading stdin/files, formatted output via
  `printf`/`Text.Printf`, GADT-style data declarations.

- **BRIDGE_rust_strings**: Rust `println!(...)` works for plain string
  literals, i64-arg format placeholders, and string (`&str`) args —
  including the leading-placeholder form (`println!("{} = {}", a, b)`).
  The bridge elides `Arguments::<'_>::from_str`/`from_str_nonconst`
  (thin Arguments wrappers) and pairs `Arguments::<'_>::new(template, args)`
  with a `rust_args_pack` runtime cell; `std::io::_print` becomes
  `rust_print_dispatch` which dispatches at runtime between
  kk_print_str (from_str path) and kk_rust_print_args (formatted
  path).  The template's raw bytes are preserved through the IR by
  hex-encoding under a `__RBYTES:` marker (UTF-8 round-tripping
  would mangle bytes ≥ 0x80, like the 0xc0 placeholder marker).
  `core::fmt::rt::Argument::<'_>::new_display::<T>` is elided to
  unwrap the value.  `_N.0` field access goes through
  `rust_field_safe` which dispatches between heap-tuple kk_field
  reads (with kk_retain on the extracted value, since Perceus may
  drop the parent tuple before the field is consumed) and
  WithOverflow-flattened identity returns.  Per-arg dispatch via
  `kk_is_string` selects between kk_print_str and printf("%ld").
  Debug `{:?}` format works for ints and strings: the bridge wraps
  `Argument::<'_>::new_debug::<T>` with a runtime
  KK_RUST_DEBUG_TAG cell, and the dispatcher applies a Debug
  formatter (surrounding `"…"` quotes and `\n`/`\t`/`\"`/`\\`
  escapes for strings; identity to %ld for ints).  See
  `examples/rust_fmt.rs` and `examples/rust_dbg.rs`.  Still blocked:
  field-spec syntax (`{:.2}`, `{:>5}`, etc.); non-i64 numeric types
  (i32/u64/f64).

- **BRIDGE_mercury_strings**: Mercury `:- pred main(io::di, io::uo) is det.`
  with `io.write_string`/`io.write_line`/`io.nl` calls now runs end-to-end.
  The bridge renames the user's `main` to `main_io_impl` and synthesises a
  no-arg `main` alias; `io.write_string(S, !IO)` is routed to the runtime's
  `print_str` (and `io.write_line`/`io.print_line` to `println_str`).
  String literals on the RHS of HLDS unifications bind to `LitString`.
  Trailing-period stripping in `parseSingleGoal` already let fact-form
  `is det` predicates compile.  See `examples/hello.m`.  Still blocked:
  Mercury's broader io module (read, file handles, formatted output)
  and complex ADT deconstruction (e.g. `examples/shape.m`) still hits
  HLDS `unify` fallback paths.

---

## Phase 10: Tier A Directions — Arrays and First-Class Continuations ✓

Two directions from `~/Dokumente/frankenstein.directions` pursued as one batch to
stress-test opposite corners of the IR: Futhark arrays light up MLIR's `linalg`
dialect (new backend territory), Scheme `call/cc` stresses the closure/higher-
order machinery (validates "principled IR" claim).

### 10a. Futhark arrays → MLIR linalg ✓

The Phase 9 Futhark bridge only handled scalar `i64`. Phase 10a extends it to
emit real `linalg.generic` reductions for `sum_iota n` and `dot_iota n`:

- Allocates `memref<?xi64>` via `memref.alloca`
- Fills it with `scf.for` over `arith.index_cast`
- Reduces via `linalg.generic { iterator_types = ["reduction"], indexing_maps = [affine_map<(d0) -> (d0)>, affine_map<(d0) -> ()>] }`
- Lowered through `--convert-linalg-to-loops --expand-strided-metadata
  --finalize-memref-to-llvm` added to the native pipeline

Validated: `examples/sum_iota.fut` → 4950, `dot_iota 100` → 328350.

### 10b. Scheme bridge with call/cc ✓

New `Frankenstein.SchemeBridge.{Reader,CoreTranslate}` module. The reader is a
hand-rolled S-expression parser; the translator is a Danvy-style HOAS CPS
converter with administrative-redex beta reduction baked in at translation time.

The design decision that makes this work: Frankenstein's existing `Evidence.hs`
is single-shot inline-only, with no runtime continuation capture. Rather than
add runtime machinery, we CPS-convert the source so `(call/cc f)` becomes plain
higher-order code — `[f]_cps(λfv. fv (λ(v _k). k v) k)` — and the existing
PAP/closure machinery carries the reified continuation. First-class continuations
with correct escape semantics, without a single runtime change specific to them.

Uncovered two pre-existing Emitter/runtime bugs in the process:

1. `EApp (non-var) args` was emitting `llvm.extractvalue` against an i64 closure
   pointer as if it were an LLVM struct. Fixed to use the same `kk_field`
   indirection as the variable path, with the closure threaded as the leading arg.
2. `kk_drop` walked every field of every boxed object — including field 0 of a
   closure, which holds a raw function pointer into `.text`, triggering a
   segfault on the refcount write. Fixed by giving closures a distinct tag
   (`KK_CLOSURE_TAG = 0x434C4F53 "CLOS"`) and skipping field 0 in `kk_drop`,
   `for_each_child`, and `collect_white`. Closures are also marked acyclic.

Validated: `examples/arith.scm` → 42, `examples/escape.scm` → 100 (call/cc
bypasses the surrounding `(+ 10 ...)`). `--demo --compile` regression still
produces 3628800. All 56 cabal tests pass plus 2 new Scheme structural tests.

---

## Phase 11: Native Multi-Shot Effect Handlers ✓

**Goal**: Make `EHandle`/`EPerform` support multi-shot continuations natively,
not just zero-shot (abort) and single-shot (tail-resumptive). Distinct from
the specialized `mercury_choose` binary-choice runtime primitive — this is a
general handler API where the handler receives `(args..., resume)` and may
invoke `resume` any number of times with any value.

### 11a. Handler classifier ✓

`Frankenstein.Core.EffectOpt.HandlerKind` = `HKAbort | HKTail | HKMulti` and
the `classifyHandler` function partition handlers by how the last parameter
(the resume continuation) is used:

  * 0 references → `HKAbort` (existing setjmp/longjmp lowering)
  * 1 reference in tail position → `HKTail` (existing inlining lowering)
  * 1 in non-tail position or ≥2 references → `HKMulti` (CPS lowering)

### 11b. CPS converter ✓

`Frankenstein.Core.CpsConvert` — pure Plotkin-style CPS transformation
threaded through a thin `Cps` monad for fresh-name generation. Handles all
expression forms. The key insight is let-fusion at `EPerform` sites:

```
cps[let x = M in N] k  =  cps[M] (\v -> Let x = v in cps[N] k)
```

so the handler's continuation captures the rest of the body, not just an
identity binder. 12 cabal unit tests cover both the classifier and the CPS
converter.

### 11c. Evidence-pass dispatch + sentinel substitution ✓

`Frankenstein.Core.Evidence.evidenceExpr` routes `HKMulti` handlers
through CPS conversion, then runs `substEFunRef` to replace the
sentinel `EFunRef qn` (left by the CPS converter at perform sites)
with `EVar evName` — a regular variable reference to the handler's
evidence binding. `Frankenstein.Core.EffectOpt.inlineLocalHandler`
has a matching guard to skip `HKMulti` so optimization doesn't
collapse multi-shot semantics back to single-shot.

### 11d. Nondeterminism demo ✓

`examples/effect_nondet.json`:

```haskell
effect nondet { choose : int }
handler = \dummy resume -> let r1 = resume(1)
                           in let r2 = resume(0)
                           in r1 + r2
body    = let b = perform choose 0 in case b of 1 -> 10; _ -> 20
```

After multi-shot evidence pass:

```
let ev_nondet = handler in
ev_nondet(0, \k -> let b = k in case b of 10 | 20)
```

When `resume(1)` is invoked, the continuation yields 10. `resume(0)`
yields 20. Handler returns `r1 + r2 = 30`.

**End-to-end native binary output: 30.** First working multi-shot
effect handler in Frankenstein, all the way through host →
MLIR → LLVM IR → native binary.

### 11e. K verification ✓

`k-specs/multishot-claims.k` — 9 kprove claims, all proven `#Top`:

  * **MS1**: Classifier on canonical shapes (abort / tail / multi).
  * **MS2**: `countAppsOf` composition correctness (literal, direct
    apply, nested apply).
  * **MS3**: Structural invariants (1-param ELam / non-lambda → never
    multi-shot).

New K helpers in `EFFECTOPT-CHECKERS`: `countAppsOf` (and list/BG/branch
variants), `isMultiShotHandler`.

### 11f. Bootstrap verification ✓

Full 3-stage bootstrap holds: **24/24 modules match between stage 2
and stage 3, 21/21 E2E tests pass at every stage.** Multi-shot
infrastructure is dormant for bootstrap modules (none of which use
multi-shot handlers) and active only when invoked.

---

## Cross-Module Effect Dispatch ✓

**Goal**: Module A performs an effect, Module B handles it — effects work across
language boundaries after linking.

**The problem**: The evidence pass ran per-module *before* the linker, so Module A's
`EPerform(exn/fail)` was resolved to a default handler call `exn_fail()` — never
reaching Module B's `EHandle(exn, ...)`.

**The fix**: Build a global effect registry from all modules' `progEffects` and
run `evidencePassGlobal` with that registry on each module *before* the linker
mangles names. This gives every module visibility into every other module's effect
declarations while keeping names unmangles for correct resolution.

```
BEFORE: compile → evidencePass (per-module, local effects only) → linker → emit
AFTER:  compile → evidencePassGlobal (global registry) → linker → emit
```

Pipeline change in `Main.hs`:
- Collect `allEffectDecls = concatMap progEffects` across all compiled modules
- Build global registry via `collectGlobalEffects` on the combined declarations
- Run `evidencePassGlobal globalEffects` on each module before linking
- The `--emit-effect-mlir` path still preserves raw `EHandle`/`EPerform`

**Result**: All 97 cabal tests pass (including new cross-module unit test),
5/5 polyglot E2E tests pass, `--demo --compile` → 3628800.

---

## Self-Hosted Binary — Real Compilation ✓

**Goal**: Link Frankenstein's self-compiled .o files into a standalone binary that
exercises the compiler's own code — proving Frankenstein can bootstrap real work.

**Result**: All 20 Frankenstein modules compile through the full pipeline
(`frankenstein <file.hs> --emit-mlir | mlir-opt | mlir-translate | clang -c`) and
link into a **1.4 MB self-hosted binary** with 67 passing tests. The binary runs
all four compiler passes (ConTags, Perceus, Evidence, MLIR emission) on Core IR
constructed in C, and the emitted MLIR compiles through the LLVM toolchain to
produce a native binary that computes **factorial(10) = 3628800**.

**Build**: `bash self-host/build.sh` — compiles all 20 modules, links, runs 67
self-tests, validates factorial MLIR through the full pipeline.

| Metric | Value |
|--------|-------|
| Modules compiled | 20/20 |
| Total .o size | ~2.5 MB |
| Binary size | 1.4 MB |
| Self-tests | 67/67 pass |
| Compiler passes exercised | assignProgramTags, insertPerceus, evidencePass, emitProgramText |
| Full pipeline validation | factorial(10) → MLIR → mlir-opt → clang → 3628800 |

**Lazy selector fix**: GHC compiles `let (a, b) = expr` as two lazy selector
thunks that share a cached pair. Perceus inserts drops in each selector for the
unused field, which originally caused use-after-free when both selectors force
the same cached pair. Fix: `kk_thunk_force` now retains the cached result on
every access (retain-on-force semantics), giving each consumer its own reference.
`kk_drop` is fully functional — all 12 stage 2 examples pass.

**C shim surface**: 423 external symbols across Data.Map, Data.Set, Data.Text,
GHC.Internal.*, State monad, and standard library functions. All resolved by
C shims in `self-host/shim_*.c` implementing minimal versions using the `kk_*`
runtime's allocation primitives.

**What the self-hosted binary proves**: Frankenstein's MLIR emitter (2500 lines
of Haskell with State monad, Data.Map, Data.Set, Data.Text, pattern matching,
closures, lazy evaluation) has been compiled through the compiler's own pipeline
and produces correct, optimizable MLIR. The factorial result matches the host
compiler's output, proving self-hosted compilation is functionally equivalent.

---

## Current State (2026-05-12, 3-stage bootstrap fixed point)

### What's Built and Working
- **8 bridges**: GHC (real API + `foreign import ccall` FFI), Rust (MIR text+JSON + `extern "C"` FFI),
  Mercury (HLDS), Koka (library API), Python (ast S-expr), Go (go/ast S-expr), Futhark (in-tree Pratt parser),
  Scheme (S-expr + CPS)
- **Multi-module compilation**: GHC bridge chases imports through the module graph
  (`compileToCoreMulti`), compiles all home-package modules in a single session, returns
  `[Program]`. Cross-module name resolution in the linker (`resolveName` parses
  `Module/name` format, disambiguates via `preferMod`).
- **Cross-language multi-module**: Koka extern declarations call Haskell functions compiled
  from multiple modules. Demo: 2 Haskell modules + 1 Koka module → single binary → 75.
- **Core IR**: Multiplicity, effect rows, Perceus ops, laziness ops
- **Perceus pass**: Drop + retain insertion, formally verified (20 kprove claims)
- **Evidence pass**: Single-op and multi-op effect dispatch with cross-module resolution, 13 kprove claims
- **Linker**: Multi-module merging with cross-module name rewriting, 20 kprove claims
- **MLIR emitter**: func/arith/scf/llvm dialects, lambda lifting, closures with
  real function pointers, thunks, bool/char/int/float/string support, cycle candidate
  marking (`kk_cycle_candidate` after `kk_alloc_con` in statically-detected cyclic defs)
- **Runtime**: Perceus RC (`kk_retain`/`kk_drop`), boxed values, thunks, retain-on-force
  semantics for shared lazy selectors
- **K specs**: OrganIR typing + Perceus + full effect semantics (organ-ir.k,
  1229 lines, 240 rules), 118 krun tests (incl. 42 algebraic effect tests
  with Mercury semidet/choice patterns), 47 bridge property tests,
  313 kprove claims (43 Perceus + 155 bridge + 30 evidence + 44 linker + 41 effectopt)
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
- **Phase 3e: Standard library types** ✓: Disabled GHC RULES pragmas (`-fno-enable-rewrite-rules`)
  so standard constructors (`:`, `[]`, `Just`, `Nothing`, `True`, `False`, `(,)`) survive -O1
  intact instead of being fused into `build`/`foldr`. Added `collectReferencedTyCons` to walk
  Core expressions and extract stdlib TyCons via `dataConTyCon`, merging with `mg_tcs` so
  DataDecls appear in progData. Five new end-to-end examples:
  - `stdlib_list.hs`: `sumList [1..5] = 15` — standard `[Int]` with pattern matching
  - `stdlib_maybe.hs`: `fromMaybe + Just/Nothing = 141` — standard `Maybe Int`
  - `stdlib_bool.hs`: guards, `otherwise`, `negate = 7` — standard `Bool`
  - `stdlib_tuple.hs`: `swap + addPair = 13` — standard `(Int, Int)`
  - `prelude_hof.hs`: `myMap/myFilter/myFoldr on [Int] = 22` — HOFs on stdlib lists with Perceus RC
  - `prelude_inline.hs`: real Prelude `map`/`filter`/`sum` = 24 — GHC inlines at `-O1` with
    `-fexpose-all-unfoldings -fspecialise-aggressively -fcross-module-specialise`
  - `prelude_comprehensive.hs`: real Prelude `map`/`filter`/`sum`/`take`/`zipWith`/`foldl` = 235
  All 21 examples compile and run correctly. Prelude HOFs are fully inlined by GHC's
  aggressive specialization flags — no C shims needed.
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
- **Self-hosted binary** ✓: 23/23 modules compile through own pipeline. 3-stage
  bootstrap reaches fixed point — stage 2 and stage 3 produce byte-identical MLIR
  for all 23 modules. All 21 E2E tests pass at every stage. All 4 compiler passes
  exercised (ConTags, Perceus, Evidence, MLIR). Full pipeline validation:
  factorial(10) → MLIR → mlir-opt → clang → 3628800.
- **FFI cross-language imports**: Native FFI mechanisms in major bridges resolve through
  the polyglot linker's symbol table, enabling symmetric multi-language composition:
  - **Haskell `foreign import ccall`**: GHC bridge detects `FCallId` in Core, extracts C
    function name from `CCallSpec`/`StaticTarget`, strips `realWorld#` state tokens and
    unboxed `(# State#, result #)` tuple destructuring. Haskell functions can call Python,
    Go, Rust, etc. via standard `foreign import ccall "symbol_name"` syntax.
  - **Rust `extern "C"`**: MIR bridge now correctly parses call terminators with external
    targets (`_0 = square(copy _1) -> [return: bb1, unwind unreachable]`). Fix: MIR text
    parser no longer wraps call terminators in `Assign((...))` which prevented the call
    terminator parser from recognizing them. Rust functions can call Python, Haskell, etc.
    via standard `extern "C" { fn symbol_name(...); }` syntax.
- **Cross-language coverage**: 12 polyglot E2E tests in `test-polyglot.sh`:
  - 3-lang (Haskell+Rust+Koka) → 69
  - 4-lang semidet success/failure (Haskell+Rust+Mercury+Koka) → 69/1
  - Cross-lang multi-module (Haskell×2+Koka) → 75
  - Haskell stdlib cross-lang (map/filter/sum from Koka) → 220
  - 7-lang all bridges (Haskell+Rust+Mercury+Python+Go+Futhark+Koka) → 147
  - 7-lang multi-module (Haskell×2+Rust+Mercury+Python+Go+Futhark+Koka) → 175
  - **12-lang all bridges** (Haskell+Rust+Mercury+Python+Go+Futhark+Swift+OCaml+Erlang+F#+Idris+Koka) → **440**
  - **Haskell FFI cross-lang** (Haskell `foreign import ccall` → Python+Go+Koka) → **157**
  - **Rust FFI cross-lang** (Rust `extern "C"` → Python+Haskell+Koka) → **69**
- **Organ-bank integration**: OCaml shim produces OrganIR JSON consumed end-to-end through
  frankenstein's `OrganIR.Consumer` → Core → MLIR → native (factorial(10)=3628800, cube(5)=125).
  SML/Lua/Erlang/Prolog/Forth frontends produce structured OrganIR; Lua shim consumable but
  runtime type mismatch (any vs int). C/C++ shims at wrong abstraction level (LLVM IR as strings).
- **Test suite**: 97 cabal tests (incl. cross-module effect test), 12 polyglot E2E, 3 Wasm validation tests,
  K test oracle, 118 krun tests, 10 cycle collector C tests, 21 self-host E2E examples (pass all 3 stages)
- **End-to-end**: `--demo --compile` → 3628800, `--demo --compile --target wasm32` → 3628800 in Node.js

### Recent Commits
- **Bootstrap fixed point (010016d)**: 3-stage self-hosted compiler converges — 23/23
  modules produce byte-identical MLIR in stages 2 and 3. Key fix: refactored monolithic
  `emitAppVar` (~490 lines, 936KB OrganIR) into 6 arity-dispatched functions to eliminate
  a compiler hang during self-compilation. Size-based JSON splitting (400KB target) with
  minification. `compile_stage()` and `run_e2e_tests()` extracted as reusable bash functions.
  All 21 E2E tests pass at all 3 stages.
- Stage 2 bootstrap: 23/23 — per-part fallback with global injection, cabal exec race fix, KokaCore TypeDefs, guard desugaring. Two modules (MlirEmit/Emitter, KokaBridge/Driver) still crash 2-3 split-parts each but fallback extraction + `fix-missing-else.py` truncated-scf repair + `llvm.mlir.global` injection from stage1 produces valid .o files. Regex fix: `extract-mlir-funcs.py` global name capture was `\S+` (greedily matched into string content), now `[A-Za-z0-9_.$]+`. Stage 2 compiler passes all 21 E2E tests.
- FFI cross-language imports — Haskell `foreign import ccall` and Rust `extern "C"` now resolve through the polyglot linker. GHC bridge detects `FCallId` vars via `idDetails`, extracts C function names from `CCallSpec`/`StaticTarget`, strips `realWorld#` state tokens and unboxed tuple `(# State# RealWorld, result #)` destructuring. MIR bridge fix: `convertTextLine` no longer wraps call terminators in `Assign((...))`, enabling proper parsing of `_0 = func(args) -> [return: bbN, ...]` patterns. Two new demo tests: Haskell FFI cross-lang (Haskell→Python+Go+Koka → 157), Rust FFI cross-lang (Rust→Python+Haskell+Koka → 69). Polyglot test suite now at 12 tests (11 passing, 1 pre-existing Mercury choice issue).
- 12-language demo — all 12 direct-style in-tree bridges (Haskell, Rust, Mercury, Python, Go, Futhark, Swift, OCaml, Erlang, F#, Idris, Koka) compose into a single binary → 440. Each function compiled through its real compiler's API/IR. Organ-bank OCaml shim verified end-to-end through OrganIR JSON → Consumer → Core → MLIR → native.
- Expanded cross-language multi-module coverage — 7-language demo (Haskell+Rust+Mercury+Python+Go+Futhark+Koka → 147), 7-language multi-module demo (Haskell×2 + 5 languages → 175), Haskell stdlib cross-language (map/filter/sum called from Koka → 220). Polyglot test suite expanded to 10 tests. Confirmed Prelude HOFs (map/filter/foldr/sum/take/zipWith/foldl) are fully inlined by GHC at -O1 with aggressive specialization flags.
- Multi-module GHC bridge + cross-language demo + cycle collector wiring — `compileToCoreMulti` chases imports through GHC module graph, `resolveName` handles `Module/name` cross-module references, `CycleAnalysis` results wired into MLIR emitter via `esCyclicDefs`/`emitCycleCandidate`, cross-language demo (2 Haskell + 1 Koka → 75), cross_module added to Phase 8 (19 examples pass stage 1, 13 pass stage 2).
- Phase 3f: String support + builtins as first-class values + stage 2 segfault fix — Three fixes: (1) `builtinWrapperSpec` in Emitter.hs generates wrapper closures for `+`, `-`, `*`, `/`, `mod`, `==`, `<`, etc. when used as first-class values (HOF arguments); (2) Address primops `indexCharOffAddr#`/`plusAddr#` for post-simplifier `unpackCString#` byte-walking loops, with `LitString` dual semantics (cons-list in Core IR, raw `Addr#` pointer in emitter); (3) `fix-intra-module-calls.py` generates 86 MLIR wrapper functions for split-compiled `MlirEmit_Emitter` — the split compilation broke `esTopFns` population causing cross-part function calls to resolve to null. All 18 host-compiled examples pass. Stage 2 compiler no longer segfaults — all 12 examples pass through stage 2 (alloc_stress fixed via retain-on-force in kk_thunk_force).
- Phase 3e: stdlib types — disable RULES, collect referenced TyCons, 5 new stdlib examples (list/maybe/bool/tuple/hof), all 16 examples pass
- Fix MlirEmit_Emitter stage2 compilation — split emitExpr, flatten deep nesting, 10-part split-compile, 23/23 modules compile, 12/12 e2e tests pass
- Self-hosted binary — 19/20 modules compile through own pipeline, link into 800 KB binary, 17/17 self-tests pass. Lambda/thunk module-prefix fix in emitter. `self-host/build.sh` + `self-host/main.c`.
- Cross-module effect dispatch — global effect registry enables Module A to perform effects handled by Module B. Pipeline reorder: `evidencePassGlobal` with combined registry runs before linker name-mangling. 97 cabal tests, 5/5 polyglot E2E.
- `ae4f4ee` — Phase 2: K as living specification, 116 krun tests, noPatterns function, Mercury semidet/choice krun tests
- Phase 9: Go + Futhark frontends (6th and 7th languages) — Two new bridges added in one go. **Go** (`Frankenstein.GoBridge.{AstParse,CoreTranslate}`) shells out to a small Go helper at `go-bridge/ast_to_sexp.go` that uses the standard library `go/parser` + `go/ast` to dump a tightly-restricted S-expression. The Haskell side runs the helper (auto-builds it via `go build` on first invocation), parses S-exprs (mirrors the Python S-expr parser), and translates the same statement-block early-return shape used by the Python bridge. Supported subset: `func`, `return`, `if/else`, `Assign`, `BasicLit` (int), `Ident`, `BinaryExpr`, `UnaryExpr`, `CallExpr`, `ParenExpr`, `GenDecl→Skip`. Op tokens align directly with canonical primitives (`+`, `<=`, `%→mod`, `&→andI#`, etc.). Goroutines/channels/methods/interfaces/structs/slices are explicitly out of scope. **Futhark** (`Frankenstein.FutharkBridge.{Parser,CoreTranslate}`) is fully in-tree — no external `futhark` binary dependency. A ~270-line hand-rolled Pratt/precedence-climbing parser in `Parser.hs` accepts top-level `let name (p: t) ... : ret = expr` definitions, integer literals, identifiers, function application by juxtaposition, parens, binary ops (`+ - * / %` arith, `== != < <= > >=` comparisons, `& | ^` bitwise) with proper precedence levels (2/3/4), unary minus, `if/then/else`, and `let x = e in body`. Type annotations are accepted and discarded (everything is `i64`). Arrays, SOACs, modules, lambdas, records, tuples are deliberately rejected. Both bridges wired into `compileFile` via `.go` and `.fut` extensions. End-to-end: `examples/factorial.go --compile` and `examples/factorial.fut --compile` each produce native binaries that print `3628800`. Test suite: 56 cabal tests (52 prior + 4: arith.go K-bisim, arith.fut K-bisim, factorial.go structural, factorial.fut structural). The factorial K-bisim is structural-only for the same reason as Python (early-return → `case (n<=1) of 0 -> ... ; _ -> ...` doesn't match the K oracle's constructor-pattern expectation), but the native pipeline handles them correctly.
- Phase 8: Python frontend (5th language) — `Frankenstein.PythonBridge.{AstParse,CoreTranslate}` shells out to a small `python-bridge/ast_to_sexp.py` helper that walks `ast.parse()` and emits a tightly-restricted S-expression. The Haskell side parses S-exprs (35 lines, no aeson dep) and translates to OrganIR. Supported subset: `def`, `return`, `if/else` (early-return pattern), `Assign`, integer literals, `Name`, `Call`, binary ops `+ - * // %`, comparisons `== != < <= > >=`, unary negate. All values typed as `int`, multiplicity `Many`. Operator names map directly to the canonical primitives the MLIR emitter recognises (`+`, `<=`, etc.) so no special-casing was needed in `Emitter.hs`. Wired into `compileFile` via `.py` extension. End-to-end: `examples/factorial.py --compile` produces a 19 KB ELF that prints `3628800`. Test suite: 52 cabal tests (50 prior + arith.py K-bisim + factorial.py structural). The factorial K-bisim is structural-only because the early-return pattern desugars to `case (n<=1) of 0 -> ... ; _ -> ...` and the K oracle expects constructor patterns, not `PatLit` on comparison results — the native pipeline handles it correctly.
- Phase 7: K-verify EffectOpt — 18 kprove claims for the three EffectOpt passes (`inlineLocalHandlers`, `eliminateIdentityHandlers`, `annotateTailResumptive`) in `effectopt-claims.k`. Helper predicates added to `EFFECTOPT-CHECKERS` module in `all-claims-def.k`: `isIdentityHandler` (uncurried + curried `\(x,k) -> k(x)`), `isTailCall`/`allBranchesTailCall` (recursive structural check through `ELet`/`ECase`), `isTailResumptiveHandler`, `countPerforms`/`countPerformsList` (per-effect-module count), `noPerformsOf`. Claim groups: EO1 identity-handler recogniser (5 claims, positive + negative cases), EO2 tail-call detection (4 claims), EO3 tail-resumptive detection (4 claims), EO4 perform counting (5 claims). All 18 claims `#Top` under kprove (Haskell backend), no rewrites needed — pure functional checks. Existing perceus/evidence/linker/bridge claim files still pass against the regenerated definition.
- Phase 6e: Self-hosted MLIR → native objects — all 17 self-hosted modules lower cleanly through `mlir-opt --convert-{scf,arith,cf,func}-to-llvm` → `mlir-translate --mlir-to-llvmir` → `clang -c` to real ELF objects (totalling ~1.8 MB; Emitter.o is 702 KB). Required fix: every `func.constant @fn` → i64 cast now goes via `!llvm.ptr` (`unrealized_conversion_cast` to `!llvm.ptr` then `llvm.ptrtoint`) so `reconcile-unrealized-casts` can erase the intermediate after `--convert-func-to-llvm`. Direct func-type → i64 casts were leaking past mlir-translate as LLVM-incompatible types. End-to-end runnable: linked the self-hosted `Core/Types.o` against the C runtime and a small driver, called the frankenstein-compiled `bindName`/`bindExpr` record selectors on a heap-allocated Bind, got back the correct field values. All 50 cabal tests pass; `--demo --compile` still produces 3628800.
- Phase 6d: Self-hosting cleanup — 17/18 modules emit fully valid MLIR (the 18th, OrganIR/Consumer.hs, fails earlier in the GHC frontend due to a pre-existing text-2.1.3/2.1.4 package skew unrelated to MLIR). PAP closures via `kk_alloc_con` for undersaturated top-level calls, oversaturated path that calls then closure-indirects the remainder, uniform i64 ABI at top-level fn boundaries, string literals immediately `ptrtoint`-ed to i64, ELet alias scoping (save/restore around let body to prevent leakage into sibling scf.if branches), `ETypeLam` stripping in `emitDef` so emitted arity matches `buildTopFnArity`, and dropping `llvm.unreachable` in unhandled-case fallback (was illegal inside scf.if regions). All 50 cabal tests pass; `--demo --compile` still produces 3628800.
- Phase 6c: Full self-hosting — all 18 modules through GHC bridge, 3 emit fully valid MLIR, 14 have 1–12 residual errors out of thousands of lines, closure ABI + scf.if alias scoping + func.constant fptrs
- Phase 6b: Self-hosting Perceus.hs — closure ABI via kk_alloc_con, capture filter, lambda param renaming
- Phase 6a: Self-hosting bootstrap — Core/Types.hs through GHC bridge → MLIR validates clean
- Phase 5: Wasm backend — --target wasm32, 485-byte .wasm, browser demo, Node.js validation
- Phase 4: MLIR effect dialect — frankenstein.* ops, effect optimizations, --emit-effect-mlir
- Phase 3d: Benchmark suite — fib/tak/ack × 4 compilers, multi-arg lambda fix, nameToSsa
- Phase 3c: Cycle detection — Bacon-Rajan collector, static analysis, C tests, K tests
- Phase 3b: GHC Core patterns — primops, lambda-not-thunk, Bool→i64, negate, test programs
- Phase 3a: Haskell RC feasibility — Factorial.hs E2E, I# simplification, print builtin, profiled runtime
- `89367e2` — Phase 1: Fix polyglot demo — semidet test result, HLDS comment parsing, external runtime dispatch
- `9ef75e4` — Redirect all diagnostic output to stderr for clean MLIR piping
- `54e11e1` — Phase 4: Fix effect optimization traversals and --emit-effect-mlir pipeline
- Phase 2d: extended kprove claims (313 total: perceus, bridge, evidence, linker, effectopt)
- Phase 2c: bridge bisimulation proofs (GHC, Koka, Rust, Mercury, Python, Go, Futhark, Scheme)
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
all-claims-def.k → kompile (Haskell backend) → kprove effectopt-claims.k (18 verified)
bridge-properties.k → kompile (LLVM backend) → krun tests (47 pass)
bridge bisimulation → krun(translate(source)) == native compiler (7 tests)
K oracle → krun(random_expr) == MLIR_pipeline(random_expr) (70 property tests)
```
