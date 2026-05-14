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

3. **Audit `kk_str_flatten`**: the round-4 crash shows a valid-tagged
   KKSTRING that segfaults at field offset 0x10. Either the value's
   internal layout is corrupt (a shim or runtime helper wrote the
   wrong bytes) or `kk_str_flatten` reads it incorrectly. Add an
   integrity check at the top of `kk_str_flatten` that validates
   `byte_len > 0`, `kind` is in valid range, and one of the union
   pointers is mapped memory. If it fails, dump the value's
   refcount, address, and a few field bytes for offline analysis.

4. **Address the source of stray thunks**: investigate why plotkin's
   `EDelay` lowering wraps values in `kk_thunk_create_forced` that
   are then passed to shims as inline-style direct values. Could we
   skip the thunk wrap in cases where the consumer is known to be a
   shim (i.e., an external symbol)?

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
