# Plotkin Self-Host Binary — Runtime ABI Audit

Goal: replace the round-by-round crash-fix loop with a single
systematic pass that identifies every place in the plotkin-built
self-host binary where a value's *runtime shape* may differ from
what the consumer expects.

## Tag taxonomy

Every heap value in the bootstrap binary carries one of these tags
in the word at offset 0 from its pointer:

| Tag value | Name | Layout | Notes |
|---:|---|---|---|
| `0x434C4F53` ("CLOS") | `KK_CLOSURE_TAG` | `[tag, fn_ptr, captured_0, ...]` | Field-0 dispatch via existing closure ABI |
| `0x50415030` ("PAP0") | `KK_PAP_TAG` | `[tag, trampoline, wrapped_fn, supplied_0, ...]` | Plotkin-only. Field-0 is a `kk_pap_call_N` trampoline that re-injects evv |
| `0x4C415A59` ("LAZY") | `KK_THUNK_TAG` | `[tag, eval_flag, fn_or_result]` | `kk_thunk_force` is required before any other access |
| `0x45565632` ("EVV2") | `KK_EVV2_TAG` | `[tag, eff_id_0, op_table_0, ...]` | Plotkin only |
| `0x4F505442` ("OPTB") | `KK_OPTAB_TAG` | `[tag, closure_0, closure_1, ...]` | Plotkin only |
| `0x4B4B535452494E47` | KKSTRING | `kk_str_hdr_t` | Special layout, accessed via `text_borrow` / `kk_str_flatten` |
| user constructor tags | per-program | `[tag, field_0, field_1, ...]` | Per `assignProgramTags` output |

## Boundaries

### Boundary A — C driver → Frankenstein entry points

The driver invokes Haskell functions via their flat C symbols. Each
must be called with the right arity for the binary's compilation mode.

In **inline mode**, every entry point has its original Haskell arity.
In **plotkin mode**, every plotkin-transformed function gains +1 for
`evv` (always passed as `0`, the empty-evv sentinel).

| Entry point | Original arity | Plotkin arity | Status |
|---|---:|---:|---|
| `consumeProgram :: Text -> Either ...` | 1 | 2 | ✓ wired via `FRK_consumeProgram` |
| `flattenPatterns :: Program -> Program` | 1 | 2 | ✓ |
| `effectOptimize :: Program -> Program` | 1 | 2 | ✓ wired via direct `effectOptimize(0, prog)` (round 3 fix) |
| `collectGlobalEffects :: Program -> Map ...` | 1 | 2 | ✓ |
| `evidencePassGlobal :: Map -> Program -> Program` | 2 | 3 | ✓ |
| `insertPerceus :: Program -> Program` | 1 | 2 | ✓ |
| `emitProgramText :: Program -> Text` | 1 | 2 | ✓ |
| `dumpProgram :: Program -> Text` | 1 | 2 | ✓ |

All driver entry points are now correctly threaded. Boundary A is clean.

### Boundary B — Frankenstein code → shim layer

Plotkin mode does NOT transform shim functions (they're in `Data.*`,
`GHC.Internal.*`, etc. — excluded by `isFrankensteinModule`). So
calls from plotkin-transformed Frankenstein code TO shims pass args
without `evv`. The shim's signature stays as the original.

The hazard: a value passed to a shim may now have a different
*runtime shape* than the shim was written for. Specifically:

- **Thunks (LAZY tag)** can appear where inline mode had a direct
  value. Plotkin's eta-expansion of point-free defs introduces extra
  `kk_thunk_create_forced` wrappers (see `EDelay` lowering in
  `emitter.hs:1257`).
- **PAPs (CLOSURE_TAG with trampoline)** can appear where inline
  mode had a direct function value. Plotkin's `emitFnAsValue` wraps
  plotkin'd top-level fns into PAPs that pre-supply `evv`.
- **Eta-expansion** can change the value's structural arity: a
  point-free CAF that produced a closure (1 indirection) now
  produces the closure's body inline (0 indirections).

Each shim entry must therefore force its inputs before dereferencing
their layout-specific fields.

### Audit of shim entry points

For each shim, check: does it force its inputs before layout-specific
access? Or does it assume the caller forced them?

#### shim_data_text.c (1105 lines)

| Entry | First-arg tag expected | Forces? |
|---|---|---|
| `text_borrow` (helper, called from every Text shim) | KKSTRING | **✓ added round 4** |
| `text_isPrefixOf_2` | KKSTRING | indirectly via text_borrow |
| `Data_Text_unpack$1` | KKSTRING | check needed |
| `Data_Text_pack$1` | List of Char | check needed |
| `Data_Text_concat$1` | List of Text | check needed |
| `Data_Text_length$1` | KKSTRING | check needed |
| many more (~30 entry points) | KKSTRING / List | systematic force needed |

#### shim_ghc_list.c (994 lines)

List shims walk constructor-tagged values. They need to force the
input before reading the tag (`kk_tag`) or fields (`kk_field`).

Highest-frequency entries: `foldr$3`, `foldl$3`, `map$2`, `filter$2`,
`zip$2`, `replicate$2`, `concat$1`, `null$1`, `reverse$1`, `head$1`,
`tail$1`. Each is a candidate for added force.

#### shim_ghc_prim.c (672 lines)

`compose_apply_code` is the critical one: it's called by the
composition `(.) f g` and dispatches f and g. The bodies `f` and `g`
may be thunks/PAPs in plotkin mode.

```c
static int64_t compose_apply_code(int64_t clos, int64_t x) {
    int64_t f = kk_field(clos, 1);
    int64_t g = kk_field(clos, 2);
    return call1(f, call1(g, x));
}
```

`call1` already does `resolve_callable` (which forces). Should be ok.

#### shim_data_map.c, shim_data_set.c

Tree-walking operations. Need force on root and on each navigated
node.

#### shim_system.c

I/O. Less hazardous (most values pass through unchanged).

### Boundary C — Frankenstein → Frankenstein (internal call sites)

Already partially addressed:

- **EApp(fn, args) generic dispatch** (`Emitter.hs:1197`):
  `kk_thunk_force` added in round 1.
- **emitFnAsValue value sites**: PAP wrapping pre-supplies evv
  (gated by `esCurrentEvv` + `esScopeSsa` for scope correctness).
- **Promoted let-rec calls**: handled via `esPromotedFns` +
  `esPromotedCaptures` lookup.

Still missing:

- The OTHER closure-dispatch sites in the emitter (lines 1779,
  1798, ~1840 — local-var calls, promoted-fn calls in `emitAppVar`'s
  various branches). None of these `kk_thunk_force` before the
  `kk_field(_, 0)` read. They WOULD fail the same way as round 1
  if a thunk reaches them.

## Recommended fix order

1. **Comprehensive emitter force**: insert `kk_thunk_force` at
   every closure-indirect dispatch in `Emitter.hs` (lines 1779,
   1798, ~1840 — three sites). Same one-line fix as round 1, applied
   uniformly. Low risk, high reach.

2. **Universal shim entry force**: every shim entry point that
   dereferences its input's layout should call `kk_thunk_force` on
   that input first. Apply to all ~50 Text shims, all list shims,
   and the map/set shims. Most are mechanical — a single
   `s = kk_thunk_force(s)` at function entry. The text shim already
   has this via `text_borrow`; need to do equivalent for the
   list-walking shims (their inputs are lists, not strings).

3. **Audit `kk_str_flatten`**: ✅ done. Added validity check that
   `byte_len < (1 << 34)`. The check fires; the corrupt struct's
   internal layout reveals the upstream issue (see findings below).

   **Root cause traced**: `kk_str_concat` was building a CAT-node
   from a `cat.l` whose magic is *not* `KKSTRING` but `KK_CLOSURE_TAG`
   ("CLOS"). Its `byte_len` field is therefore some closure's field 1,
   not a length. The concat's resulting `byte_len = l->byte_len +
   r->byte_len` becomes a huge bogus value; `kk_str_flatten` then
   tries to `malloc(140TB)` and fails.

   **Why a closure flows in**: the plotkin Core for `sanitizeName`
   is `\evv eta_p0 -> delay(<expr>)(eta_p0)`. That expression
   evaluates correctly when fully reached. But its caller does
   `modPrefix <> sanitizeName m`, and somewhere on the chain the
   value type expected by `<>` is being satisfied with a *function
   closure* (a partial-application or PAP returning Text) instead
   of the Text itself.

   The same root pattern as the earlier `emptyStats` case: a Haskell
   value-typed expression now compiles to a closure that produces
   the value, not the value itself. `kk_str_concat` reads from it
   as if it were a `kk_string_t`, and the layout doesn't match.

   **Mitigation**: ✅ added `kk_thunk_force` at the top of
   `kk_str_concat`. The force is a no-op when the value is a closure
   (not a thunk), so this doesn't fix the *closure*-as-value case —
   only the thunk-as-value case. The remaining bug is closures
   leaking where Texts belong, which is a source-level / plotkin-
   pass issue, not a shim-level one.

4. **Address the source of stray thunks**: investigate why plotkin's
   `EDelay` lowering wraps values in `kk_thunk_create_forced` that
   are then passed to shims as inline-style direct values. Could we
   skip the thunk wrap in cases where the consumer is known to be a
   shim (i.e., an external symbol)?

## Closure-as-value leak audit (round 5)

Goal: identify every spot where the Plotkin pass + emitter combine
to push a closure (CLOS / PAP) into a slot the consumer reads as a
concrete value (Text, ADT, Int, …).

### L1 — Plotkin pass: `EApp` injection ignores callee kind

`Frankenstein.Core.EvidenceEvv.transformExpr` (`src/Frankenstein/Core/EvidenceEvv.hs:232-237`):

```haskell
EApp (EVar nm) xs
  | isTopLevel nm ->
      EApp (EVar nm) (EVar evv : map go xs)
```

The injection fires for *every* `EApp` whose head is a top-level
Frankenstein name. It does **not** check whether the callee is
function-typed. For a CAF (concrete-typed) top-level — e.g.
`emptyStats :: Stats` — any source-level `EApp emptyStats xs` (which
the bridge can produce via type-coercion-loaded Core or via
optimization quirks) becomes a 1+ arg call to a 0-arity symbol.

Combined with the emitter's oversaturated dispatch path
(`Emitter.hs:1735-1766`), the result is: read `field 0` of the CAF's
value (e.g. a Stats heap pointer) and call it as a function pointer.
That is a textbook closure-as-value confusion.

Fix candidates:
- Filter `isTopLevel` by also requiring `isFunctionType (defType d)`
  (i.e., plotkin only injects evv into calls whose callee is
  function-typed). This requires threading a `Map Name Type` of
  top-level types through `transformExpr` — straightforward.
- Alternatively, the emitter could special-case "arity-0 callee +
  non-empty args" by calling the CAF with 0 args, then continuing
  the dispatch loop using the *value* as an opaque heap pointer
  through the closure-indirect path — but this is a workaround that
  papers over the type-shape mismatch.

### L2 — Eta-expansion uses `flatTypeArity`, not value-shape

`Frankenstein.Core.EvidenceEvv.transformDef` (`EvidenceEvv.hs:146-189`)
eta-expands function-typed defs to match `flatTypeArity (defType d)`.

```haskell
targetArity  = flatTypeArity (defType d) + 1
currentArity = 1 + length existingParams
missing      = max 0 (targetArity - currentArity)
```

`flatTypeArity` walks `TFun` arrows directly: it does NOT step into
type synonyms / newtypes / Forall-bound aliases. Concretely:

```haskell
newtype Cps a = Cps { unCps :: forall r. (a -> r) -> r }

runCps :: Cps a -> a
runCps cps = unCps cps id
```

`flatTypeArity` of `Cps a -> a` is 1. But the *value* `runCps x`
returns is itself callable (because `a` is parametric). A source
expression `runCps x someArg` would become — after plotkin —
`runCps evv x someArg` (3 args) against a plotkin'd arity-2
`runCps`. The emitter hits the oversaturated path; force the result;
read field 0; dispatch.

For normal types this works (the runCps result is a real closure
holding the lambda body). The hazard is when the returned value's
runtime layout is **not** `[fptr, captures…]` — e.g. it has been
boxed by an upstream `kk_thunk_create_forced` (`EDelay` lowering).
The `kk_thunk_force` at `Emitter.hs:1751` covers the thunk case.
But if the returned value is itself a *constructor* (a record
containing closures, accessed via field N where N≠0), reading
field 0 yields the wrong word.

### L3 — `emitFnAsValue` pre-supplies evv to a CAF

`Emitter.hs:243-265`. The `arity > 0` guard correctly steers CAFs
(arity 0 in `buildTopFnArity` because their `defExpr` is `ELet`,
not `ELam`) to the no-evv branch. But `arityMap` is built per-module
(`Emitter.hs:360`); **cross-module** Frankenstein top-level
references fall through to the default `arity = 1` (`Emitter.hs:248`).

For a cross-module CAF reference:
- `qualSanitized` (with module prefix) won't be in this module's
  `arityMap` → default 1.
- `isPlotkinFn` (`"Frankenstein_" `T.isInfixOf` fnName`) is true.
- Emits a 1-arg PAP wrapper that calls `@CAF_symbol(captured_evv)`
  — but the actual MLIR signature is `() -> i64`. Arity mismatch
  → undefined behavior in `mlir-translate` / linker.

For a cross-module function reference:
- Default `arity = 1` is **wrong**: the real plotkin arity is
  `1 + flatTypeArity(defType)`, which could be 3, 4, …
- The 1-arg PAP wrapper calls the symbol with 1 arg (just evv)
  but the symbol expects 2+ args → again a sig mismatch.

Fix: build a **global** arityMap (analogous to `collectTopNames`)
that covers every Frankenstein module in the link set, threaded
through `emitProgramText`'s `EmitState` setup. Same shape as
`topNames`: pre-computed once and passed in.

### L4 — `kk_thunk_create_forced` over a closure

`Emitter.hs` lowering of `EDelay` (around line 1257 per the audit
above) wraps an expression in `kk_thunk_create_forced(expr)`. If
`expr` evaluates to a CLOS-tagged value (a PAP) rather than a
KKSTRING / ADT, the thunk's payload is a closure. Forcing returns
the closure unchanged. Subsequent code that reads it as Text /
ADT sees a CLOS layout.

This is not new under plotkin per se — `EDelay` predates the
plotkin work. But plotkin's eta-expansion and evv-injection
*increase* the rate at which closure-valued expressions appear
mid-pipeline, so the latent hazard now fires more often.

Fix candidate: when lowering `EDelay e`, peek at `e`. If it's
trivially a fully-applied call site whose callee is plotkin'd
top-level, the result is a real heap value and `kk_thunk_create_forced`
is harmless. If `e` is an unsaturated app (would produce a PAP),
defer the thunk wrap so the PAP itself flows to the consumer
unchanged (the consumer is presumably set up to call it).

### Concrete failing path in current bootstrap

The `kk_str_flatten` crash with `cat.l` tag = `0x434C4F53` (CLOS) is
consistent with either L3 or L1 firing somewhere in
`emitProgramText`'s `modPrefix` chain:

```haskell
modPrefix = let m = qnameModule (progName prog)
              in if T.null m then "" else sanitizeName m <> "_"
```

`progName` is a record accessor; `qnameModule` is too. If one of
these is a plotkin'd top-level fn somewhere in the link set and the
emitter's local arityMap misses it, L3 produces a malformed PAP that
is fed into the `<>` chain → eventually reaches `kk_str_concat`'s
catch-all branch in `append_impl`, which calls `kk_str_concat` on
the CLOS pointer → reads garbage `byte_len` → `kk_str_flatten`
trips the validity check.

### Round 5 follow-up — L3a attempt result

L3a fix landed (PostProcess.hs `ExternPapFix` strategy):
auto-synthesizes PAP wrappers + extern decls for any
`@frankenstein_<sym>$0()` call whose arity can be recovered from
the stage1 MLIR cache. Builds cleanly; inline `--demo` regression
passes.

**Did not reduce the Phase 9 crash.** Inspecting stage-2 MLIR after
the fix, the remaining unresolved `$0()` references are NOT
Frankenstein-prefixed — they are legitimate shim symbols like
`Data_Text_Internal_empty$0`, `GHC_Internal_Classes_not$0`,
`Data_Set_Internal_empty$0`, `GHC_Types_Var_isTyVar$0`. Each is
defined in `shim_*.c` with a matching `__asm__` symbol and links
correctly.

So the CLOS-as-Text leak in `kk_str_concat` is NOT from this
specific class. L3 as initially diagnosed was the wrong lever.

### Refined hypothesis

The crashing `cat.l` layout reads:
- `magic = 0x434C4F53` (CLOS)
- `rc    = 4196513` (= 0x400321 — unusual; not a typical rc value)
- `byte_len = 562949953421313` (= 0x2000000000001 — near-2^49,
  plausibly a function-pointer projection)
- `kind  = 0x4C415A59` (= "LAZY" — i.e. THUNK_TAG)

Reading these offsets against the closure layout:
- `field 0` (offset 16) reads as `byte_len` → that's a function pointer.
- `field 1` (offset 24) reads as `kind` → that's a captured value
  whose tag is `LAZY`.

So the runtime value is a **closure whose capture is a thunk**.
That matches the lambda-lifted-closure-with-thunk-payload pattern,
not a cross-module value-position ref. Candidates:

1. A `let`-bound function value (lifted lambda with captures) where
   one of the captures is a thunk produced by `EDelay` →
   `kk_thunk_create_forced`. The function value is passed through
   `<>` somewhere, hitting `append_impl`'s catch-all → `kk_str_concat`.
2. A `compose`-style HOF returning a closure that *holds* a thunk,
   and the consumer expected a Text result.

### Recommended next steps

1. ~~Build a global arityMap~~ (attempted as L3a — didn't fix the
   observed crash; left in place because it removes a separate class
   of latent breakage).

2. **Type-aware evv injection** in `transformExpr`: keep a
   `Set (Text, Text)` of CAF-typed top-level names alongside
   `topNames`; for those, skip the evv-injection branch and emit
   `EApp (EVar nm) xs` unchanged. The CAF's body still gets the
   `let evv_p = 0 in ...` wrapper, so internal evv use is fine.

3. **Diagnostic instrumentation**: print which top-level symbol is
   being wrapped as a PAP in `emitFnAsValue` (one log line per
   plotkin'd value-position emission). Re-run the bootstrap; the
   last symbol before the kk_str_flatten crash is the smoking gun.

## Verification

After each fix, run:
- `FRANKENSTEIN_EVIDENCE=plotkin bash self-host/build.sh` and check
  Phase 8/9c/10c E2E counts.
- The four small effect demos (`84/100/30/99`) and `--demo` (`3628800`)
  to confirm no regression in cleanly-working paths.

The end state target: all 21 E2E tests pass at every stage and
stage 2/3 MLIR matches byte-for-byte (fixed point).

Inline-mode bootstrap remains 24/24 + 21/21 + fixed point as the
non-regression baseline.

## DB5 closing summary (rounds 6-12)

Final state under `FRANKENSTEIN_EVIDENCE=plotkin`: **21/21 E2E parity at Phase 8 (stage 1), Phase 9c (stage 2), Phase 10c (stage 3)**. Strict MLIR byte-equality fixed-point stabilized at **9-11/26**.

### Commits

| Round | Commit | Headline |
|---|---|---|
| 5 | `f74c712` | A_sanitize_shim plotkin arity → Phase 8 0/21 → 21/21 |
| 6 | `6308751` | EvidenceEvv wired, isTopLevel cross-module, emitFnAsValue 0-evv fallback, main wrapper thunk-force, split disabled. Stage 2/3 0/21 → 2/21 |
| 7 | `ebe1253` | parseDollar0 `T.stripStart` bug → 2/21 → 18/21 across stages |
| 8 | `eb953c1` | etaParams unique-field naming (was producing dup `%eta_p0` params) → 18/21 → 19/21 |
| 9 | `49e3590` | emitFnAsValue isPlotkinFn widened (user-module fns recognized) → 19/21 → 21/21 |
| 10 | `0c461f7` | Split-compile re-enabled (L3b obsolete) |
| 11 | `d066980` | renderFixed missing-reverse + isInScope intra-line negative depth |
| 12 | `aea4313` | parseKkFieldProducer / parseConstIdx — same stripStart bug as round 7 |

### What we learned

1. **Class of bugs: latent post-process parsers**. Three separate `T.stripStart` calls in `PostProcess.hs` parsers silently consumed leading spaces the prefix matchers needed. The parsers always returned `Nothing`, silently disabling whole strategies for many rounds.

2. **Class of bugs: accumulator without final reverse**. `renderFixed` built its line accumulator via O(1) prepends but joined without a final `reverse` — entire MLIR file came out backwards. Only fired after round 7 activated the code path.

3. **Class of bugs: ABI mismatch between Haskell and C shims**. `A_sanitize_shim.c`'s C functions ignored the `evv` arg that plotkin's eta-expansion added, returning inline-mode closure values where Text was expected.

4. **Class of bugs: emitter pre-supply gate**. `emitFnAsValue`'s `"Frankenstein_" isInfixOf` check excluded user-module fns from PAP evv pre-supply — HOFs invoking those fns produced wrong-arity dispatch.

5. **Class of bugs: free-var analysis in lambda lifts**. `esCurrentEvv` tracking gets lost inside lifted lambdas; emit decisions depend on accumulator state at lift time.

### Why fixed-point gap remains

The strict 26/26 fixed-point target requires the stage 1 binary's compiled emitter to behave byte-for-byte identically to the host (GHC-compiled) emitter. The remaining ~15-17 modules diverge in:

- **classifyBranches** mis-classifying single-PatCon cases (see `single_ctor_test`)
- **Lambda-lift free-var sets** differing between stages (e.g. `cpsExpr_lambda1027` with 2 captures in stage 3 vs `cpsExpr_lambda1035` with 3 captures in stage 2 for the same source)
- **SSA-numbering cascading offsets** from upstream emit variance
- **Split-merged modules** failing mlir-opt at stage 3 (separate ABI issue with merged-part wrapper naming)

The most promising specific bug class is **pattern-guard miscompilation**: Haskell's `case x | g1, g2 -> ...` arms with chained `<-` pattern-binding guards don't lower correctly in self-compiled Frankenstein. Rewriting `classifyBranches`'s guard chain to use Bool guards over where-bound helpers did NOT fix this (round attempted, reverted) — suggesting the bug class is broader than the guard-chain rewrite addresses.

### Closing call

DB5 marked substantially complete. 21/21 E2E behavioral parity is the durable user-visible win. The 9-11/26 fixed-point gap is foundational pattern-compiler-correctness work — see `docs/strict-fixed-point.md` for the next investigation arc.
