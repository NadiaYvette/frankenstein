# Cross-Language Calls in Frankenstein

*How a Koka function calls a Haskell function calls a Rust function calls a Mercury predicate, and what the type system does (and doesn't) check at each boundary.*

---

## Status of this document

This is an **interim explanation** of how cross-language calls currently
work in Frankenstein — written so that questions of the form "but how
do the type systems actually interact?" have a grounded answer until a
real FFI type-system design lands.

The short version, set up in detail below, is that **today there is no
cross-language type discipline**: the four bridges meet at a uniform
`i64` ABI, the linker only matches by name, and demo authors are
trusted to line up calling conventions by hand. That is enough for the
polyglot demos and the self-host bootstrap, but is clearly not where
the project wants to stop.

The closing sections sketch where a cross-language type checker would
plug in and list the design choices that need to be settled before one
gets written. Those open questions — granularity of the FFI type
lattice, treatment of `anyType` fallbacks, effect-row reconciliation,
refinement-vs-unification, source-level visibility of cross-language
ascriptions, and multiplicity at the boundary — are the things to
think through before committing to a design. Until those decisions
are made, this document describes what's actually happening.

---

## TL;DR

At the FFI boundary, there is essentially **no type-system interaction**.
Every cross-language call is a uniform `i64`-typed C call that resolves
**by name**. Each language keeps its own type checking against its host
compiler; the linker only matches symbols. The MLIR emitter throws away
the Core IR's type structure and emits a uniform `(i64, …, i64) -> i64`
calling convention. Soundness across language boundaries currently rests
on:

1. Each bridge faithfully translating its source language's values into
   the agreed uniform representation.
2. Demo authors aligning argument and return types manually (`Int` ↔
   `int` ↔ `i64` ↔ Mercury `int`).
3. No cross-language type checker between `rewriteNames` and emission.

---

## The pipeline

```
.hs  ─ GhcBridge     ─┐
.kk  ─ KokaBridge    ─┤
.rs  ─ RustBridge    ─├─ Core IR ─ linker (name resolution) ─ MLIR ─ clang ─ a.out
.m   ─ MercuryBridge ─┘
```

Every frontend parses its source through that language's own compiler:

| Bridge | Source driver | Module |
|---|---|---|
| GHC      | GHC API `mg_binds` | `src/Frankenstein/GhcBridge/` |
| Koka     | Koka's Core IR     | `src/Frankenstein/KokaBridge/` |
| Rust     | `rustc -Z unpretty=mir` (MIR text) | `src/Frankenstein/RustBridge/` |
| Mercury  | `mmc --dump-hlds 50`               | `src/Frankenstein/MercuryBridge/` |

Each bridge translates into the same Core IR
(`Frankenstein.Core.Types` — same `Expr`, `Type`, `Def`).

**Type checking and inference happen inside each bridge** against its
host compiler, which has already validated the source. The Core IR
types each bridge produces are best-effort translations:

- GHC's `CoercionTy` falls back to `anyType` (`GhcBridge/CoreTranslate.hs:1146`).
- Rust's MIR types translate to a coarser Core scheme.
- Mercury's modes (`in`/`out`/`di`/`uo`) influence multiplicity but
  collapse to uniform i64 at the ABI.

---

## Walk-through: `fibonacci(10)` in `four-lang.kk`

The demo (`examples/polyglot-demo/four-lang.kk`) starts with:

```koka
extern fibonacci( n : int ) : int {
  c "fibonacci"
}
```

The chain that makes this resolve to Haskell's `fibonacci`:

1. **Koka frontend** parses the `extern` and emits a wrapper def named
   `@extern-fibonacci` in its own Core.
2. **`KokaBridge.CoreTranslate.buildExternMap`** (`CoreTranslate.hs:131-137`)
   reads each `KC.External`, extracts the C target name from the `c "…"`
   format string, and builds the map
   `@extern-fibonacci → "fibonacci"`.
3. **`rewriteExternRefs`** (`CoreTranslate.hs:166`) walks every `EVar`
   reference in Koka's defs and rewrites `@extern-fibonacci` to
   `EVar "fibonacci"`. The wrapper def itself is dropped by
   `isExternWrapper`.
4. **Meanwhile**, the GHC bridge produces a `Def` with
   `defName = QName "Fib" (Name "fibonacci" 0)`.
5. **`Core.Linker.rewriteNames`** (`Linker.hs:244`) builds a global
   symbol table over every input frontend's defs, then walks every Core
   expression rewriting `EVar` references against that table. Koka's
   `EVar "fibonacci"` finds Haskell's def — the cross-language link is
   just name matching.
6. The merged single-program Core goes through the evidence pass,
   Perceus, and into the MLIR emitter.

That's the whole story for "how do I call across languages": **emit
`extern` with a C target name, define the target somewhere else in any
language, let the linker pair them by name**.

---

## What types actually flow at the boundary

At the MLIR level, **every function is `(i64, …, i64) -> i64`**
(`emitDef` shapes params at `MlirEmit/Emitter.hs:1970-1972`). All
bridges agree on this uniform calling convention:

| Source concept | At the FFI boundary |
|---|---|
| `Int` / `int` / `i64` / Mercury `int` | `i64`, literally — the value flows through a register |
| Pointer to a heap value | `i64` holding the address; `kk_is_heap_ptr` does the alignment + threshold check to distinguish from unboxed ints |
| Algebraic data constructor | heap cell `[refcount][tag][fields…]` allocated via `kk_alloc_con`; passed as `i64` pointer |
| String / `Text` | runtime `kk_string_t` (rope); passed as `i64` pointer |
| `Bool` | unboxed `i64` `0`/`1` (`emitExpr` for `True`/`False` at `Emitter.hs:2169-2174`) |
| `Float` / `Double` | `i64` bit pattern of the IEEE-754 float; `recordF64Bits` tracks which SSA names are float-tagged so binary ops dispatch to `arith.mulf` etc. |
| Function value / closure | `i64` pointer to a `KK_CLOSURE_TAG`-tagged cell whose field 0 is a function pointer; calls go through `call1`/`call2` trampolines |

There is **no Core-level type check that Koka's `int` matches Haskell's
`Int` matches Rust's `i64`**. The bridges' translators are trusted to
produce the uniform representation; the linker pairs names.

### What happens when types don't actually line up

If a Koka `int` were passed to a Rust function declared
`fn double(n: u32)`, MLIR would still type-check (everything is `i64`)
and the Rust side would silently read the wrong half of the register.
That's the cost of the simple ABI. The current demos align languages on
`int` / `i64` / `Int` / Mercury `int` deliberately to side-step this.

Other failure modes that are *not* currently detected:

- A lazy Haskell `[Int]` passed to Rust expecting `Vec<i64>` — wrong
  shape; Frankenstein doesn't insert a conversion.
- A Mercury list passed to Koka — different ADT encodings between
  bridges; would require a manual marshal.
- A Rust `Result<T, E>` returned to a Koka caller expecting an effectful
  value — no automatic effect-translation across the boundary.

---

## The one interesting cross-language type interaction: effects

Mercury's `check_threshold(N, Threshold) is semidet`
(`examples/polyglot-demo/check.m`) carries an `exn` effect in its Core
type. The Mercury bridge annotates `semidet → exn`. When Koka's `main`
calls it without a handler:

1. The evidence pass (`src/Frankenstein/Core/EvidenceEvv.hs`) sees a
   free `exn/fail` effect at the call site and desugars
   `EPerform(exn/fail)` into a direct call to `mercury_exn_fail()`
   (the runtime stub returns `0`).
2. The Mercury bridge arranges successful predicates to return `1`,
   failure to return `0`.

So the effect "boundary" collapses to an integer flag *before* it
crosses into Koka's compiled code. The uniform `i64` ABI hides the fact
that one side had an effect signature and the other side didn't.

---

## Summary: how the type systems interact (today)

- **Within a bridge**, the source language's own type system is the
  only one running.
- **Across bridges**, only names match — no cross-language unification.
- **Effects** are erased into integer flags by the evidence pass before
  they cross.
- The unified Core type structures (`TForall`, `TFun`, `TApp`, `TCon`,
  `TVar`, `TSyn`) carry multiplicity (`Many`/`Affine`/`Linear`) and
  effect rows so the bridges can preserve the source language's
  information.
- The linker does **not** unify or verify types between languages.
- The MLIR emitter discards type structure in favour of uniform `i64`.

---

## Where a cross-language type checker would plug in

If you want stronger guarantees, the natural seam is between
`rewriteNames` and merged-Core emission in
`Linker.hs:117-118`. At that point you have, for the first time:

- The full symbol table mapping mangled names to defs.
- Every call site's argument types (from the calling language's bridge).
- Every callee's declared parameter types (from the providing language's
  bridge).

A type checker placed there could:

- **Reject** an integer-shaped value flowing into a heap-pointer
  parameter (and vice versa) — the cheapest, highest-value check.
- **Warn** on mismatched bit-widths (Koka `int32` into a Rust `i64`
  argument).
- **Enforce** that effect rows agree where the callee has an unhandled
  effect.
- **Insert** explicit `kk_int_to_haskell_chars`-style adapter calls when
  a known conversion exists.

### Design choices to think about

1. **Granularity of the cross-language type lattice.** Do we unify on
   the existing Core `Type`, or introduce a coarser "ABI type" lattice
   (`AbiI64`, `AbiHeapPtr`, `AbiClosure(arity)`, `AbiString`,
   `AbiFloatBits`) that's just expressive enough to catch shape
   mismatches?

2. **What to do with `anyType`.** GHC's `CoercionTy` and a few other
   fallbacks produce `anyType`. A strict checker would have to either
   trust them (per-bridge whitelist) or refine them out of the bridges
   before they reach the linker.

3. **Effect-row reconciliation.** Mercury's `exn` survives the evidence
   pass as a runtime check; should the type checker insist that a Koka
   caller either handle it or propagate it? This is closer to a real
   type-system extension than an ABI check.

4. **Refinement vs. unification.** Frankenstein could either
   *refine* each bridge's types to a common substrate before linking
   (lossy, simple) or build a heterogeneous-but-related type lattice
   that names each bridge's conventions explicitly (lossless, more
   complex).

5. **How visible should it be at the source level?** Should Koka
   `extern` declarations carry richer cross-language type ascriptions
   (e.g. "this `int` is a Rust `u32`"), or should the checker infer
   compatibility from the providing language's bridge output?

6. **Multiplicity at the boundary.** A Rust function declared
   `fn(self: Box<T>)` consumes its argument (affine). A Haskell caller's
   reuse of that value after the call would currently be a soft bug;
   a type checker could either statically reject this or insert
   refcount-aware adapters.

The honest framing for now is: cross-language calls work because the
bridges agree on the i64 ABI, names line up, and demo authors are
careful. That's enough for the polyglot demo and the bootstrap, and
it's the right level of guarantee to *start* hardening once the design
choices above settle.

---

## Quick references

| Concern | File |
|---|---|
| Extern → C name rewriting (Koka) | `src/Frankenstein/KokaBridge/CoreTranslate.hs:131-194` |
| Cross-module name resolution | `src/Frankenstein/Core/Linker.hs:244-269` |
| Symbol-table construction | `src/Frankenstein/Core/Linker.hs` (`buildSymbolTable`) |
| Uniform-i64 emission | `src/Frankenstein/MlirEmit/Emitter.hs:1970-1972` |
| Evidence/effect erasure | `src/Frankenstein/Core/EvidenceEvv.hs` |
| Polyglot demo source | `examples/polyglot-demo/four-lang.kk` |
| Demo build script | `examples/polyglot-demo/simple-demo.sh` |
