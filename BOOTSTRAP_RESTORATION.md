# Bootstrap Restoration Status — 2026-05-26

## Baseline vs Current

| Metric | Baseline (`0c461f7`, May 17) | Current (`885f229`) |
|---|---|---|
| Phase 8 E2E (default) | 18/21 (was 21/21 in plotkin) | **0/21** |
| Phase 9c E2E | 18/21 | **0/21** |
| Phase 10c E2E | 18/21 | **0/21** |
| Fixed-point (s2→s3) | **26/26** (`*** REACHED ***`) | **2/26** |

## Regression Origin

170+ commits between `47a57ee` (May 19) and HEAD (May 26) — all bridge/runtime
feature work, none touched bootstrap or fixed-point.  Three bisect-confirmed
breaking commits:

| Commit | Subject | Impact |
|---|---|---|
| `47a57ee` | GHC bridge: Show for [Int] and Maybe Int | fix-point 26→15 |
| `890b18d` | Mercury io.write_string + Rust println! print natively | E2E 18→3 |
| `623a8c8` | GHC bridge: putStrLn prints natively via hPutStr2 intercept | E2E 3→1 |

After these three, the remaining 167 commits drifted fix-point 15→2 and E2E 1→0.

## Root Cause: 118 NULL-Returning Stubs

The host-built self-compiler ships with **118 `$0`-suffix symbols** that
compile to `xor %eax,%eax; ret` (return NULL).  These come from the
emitter's `EVar` fallback (`Emitter.hs:2143` ish) when a Haskell-source
identifier can't be resolved.  Linker passes `--unresolved-symbols=ignore-in-object-files`,
so undefined refs silently become these zero-returning stubs.

Both observed crashes (`parseJSON` and `flattenDef`) trace to:
```
kk_field(closure, 0)    ; get function pointer
call *%rax              ; call NULL — crash
```

When the stub's return value is used as a function pointer (e.g. via a
Functor instance dictionary), the call dies.

### Stub composition (118 total)

| Origin | Count | Notes |
|---|---|---|
| `GHC_Internal_*` | 47 | Haskell stdlib: `id`, `pure`, `(++)`, `(.)`, `max`, `not`, Show methods |
| `Frankenstein_Core_*` + `frankenstein_*` | 11+5 | **Two cased forms of our own refs — Linker.hs mangling bug** |
| `Data_Text_*` | 8 | Text library |
| `Data_Set_*` | 7 | Containers |
| `Data_Map_*` | 6 | Containers |
| `GHC_Driver_*` | 5 | GHC API |
| `Kind_Kind_*`, `Type_Type_*` | 7 | Type system |
| Other | ~22 | misc |

## Progress Log (2026-05-26)

### Target A: Linker case consistency — DONE (`134795a`)

Changed `unifiedName = QName "frankenstein"` → `QName "Frankenstein"` in
`Linker.hs:101` so multi-program linking's modPrefix is `Frankenstein_`
(matching single-program linking's prefix-match check on already-mangled
def names).  Updated PostProcess.hs's hardcoded `frankenstein_` extern
symbol prefix to match.

**Result**: Fixed-point check (s2 vs s3): **2/26 → 15/26** (matches
`715169a` baseline).  E2E still 0/21 (different root cause).

### Target B: GHC.Internal stubs — PARTIAL (`ef709a3`)

Provided minimal implementations for the top NULL-returning `$0` stubs
in new file `self-host/shim_ghc_dicts.c`:
- Either Monad/Applicative/Functor: return `KK_EITHER_MONAD_MARKER`
  (existing `shim_ghc_prim.c` dispatches on this for the actual ops)
- Show dicts for List/Maybe/Tuple2: placeholder closure
- emptyCallStack, fst, snd, not, dataToTagSmall#, isDigit, isSpace
- Data.Text.dropWhile$1: predicate-closure that walks bytes

Also: bulk renamed `frankenstein_Frankenstein_*` → `Frankenstein_*` in
driver.c / main.c / cross_module_aliases.c / A_sanitize_shim.c (these
hadn't been updated post-Target-A, causing call-to-NULL in main).

Critical fix: `build.sh:Phase 5b` now links shims BEFORE
Core_*.o / MlirEmit_*.o / etc., matching Phase 9b/10b's order.  Without
this, the auto-generated NULL-returning $0 stubs in module .o files
won (under `--allow-multiple-definition`).

**Crash trace movement**:
- `main → call <0> (unresolved direct)` — fixed
- `parseJSON → call *%rax (NULL via dropWhile)` — fixed
- `flattenDef → call *%rax (NULL via ???)` — still open

**Result so far**: Fixed-point still 15/26, E2E still 0/21.  $0 stub
count: 0 (down from 118 pre-fix).

### Target C: sanitizeName C shim ABI mismatch — FIXED

`self-host/A_sanitize_shim.c`'s default-mode (non-plotkin) override
declared `sanitizeName(void)` returning `kk_thunk_create_forced(closure)`,
expecting callers to dispatch the closure on the Text arg.  But every
one of the 18 compiled call sites does:

```
  call kk_thunk_force         ; force input thunk → Text
  mov %rax, %rdi              ; Text → arg
  call sanitizeName           ; treat result as Text directly
  ; pass result to <> / store in result slot — no closure dispatch
```

i.e. the compiled callers expect `sanitizeName(text) -> Text`.  When
the result-as-Text was passed to `<>` (`zlzg$2`), `kk_str_concat`
forced the thunk, got back the closure cell (tag `CLOS`), and aborted
with `non-string input magic=0x434c4f53`.

Fix: change default-mode shim to `c_sanitizeName(int64_t text) ->
sanitize_name_c(text)`, matching the plotkin variant's direct ABI.

**Crash trace movement**:
- `emitProgramText → qualifyDefName → kk_str_concat (CLOS thunk)` — fixed

**Result**: Hellos 25/26 + surd-mercury 9/9 still pass (no regression).
Bootstrap fixed-point/E2E still 15/26 + 0/21 — exposed a *deeper* bug:
stage 1 self-compiler now infinite-recurses on every input in
`Frankenstein_dszd...$200` (the let-go closure for
`collectReferencedCtors`'s flattened `(dd, cd)` iteration).

### Target D: maybeToList$1 + forM$1 NULL stubs — FIXED

Root cause of dszd200 infinite recursion: `GHC_Internal_Data_Maybe_maybeToList$1`
was a NULL-returning $1 stub (the $1 family was missed by Target B which
only handled $0).  In `OrganIR/Consumer.hs:62`:

```haskell
allData = datas ++ maybeToList (synthSConDataDecl (O.modDefs m))
```

For `examples/effect_state.json` (no data_decls, no SCon defs):
- `datas = []`
- `maybeToList Nothing` → NULL (stub)
- `[] ++ NULL` → NULL (via `kk_haskell_chars_concat`, since b=NULL)
- `progData = NULL`

`collectReferencedCtors → dszd196(NULL) → kk_field(0,_) → 0 → dszd200(0, dataCons(0)=0)`
which then loops indefinitely on `kk_field(0, 1) = 0`.

Fix in `self-host/shim_ghc_dicts.c`:
```c
int64_t ghc_maybeToList_1(int64_t m) __asm__("GHC_Internal_Data_Maybe_maybeToList$1");
int64_t ghc_maybeToList_1(int64_t m) {
    if (!kk_is_heap_ptr(m) || kk_nfields(m) == 0) return kk_nil();
    return kk_cons(kk_field(m, 0), kk_nil());
}
```

Also added `forM$1` (Traversable) to `shim_ghc_list.c` — paralleled
the existing `forM_$1` (Foldable underscore variant) but keeps results.

**Verification:** `examples/effect_state.json` now end-to-end
`compile → MLIR → bin → 100` (matches 10*10 from state effect).
Hellos 25/26 + surd-mercury 9/9 still pass.

**Instrumentation kept:** `runtime/kk_runtime.c` now has env-var-gated
field/tag trace (`KK_FIELD_TRACE=1`, `KK_TAG_TRACE_MAX=N` etc.).
`self-host/driver.c` has `FRANKENSTEIN_DUMP_PROGDATA=1` for per-pass
field[2] inspection.  Both inactive by default.

**Remaining:** 194 NULL `$N` stubs still exist; many are unreached.
The current next crash signature is in `emitLetBindings_lambda197299`
→ another NULL `$N` (TBD which) — iterate.

## Original Two Concrete Restoration Targets

### Target A: Linker case consistency (highest value)

`Frankenstein_Core_Types_bindExpr` and `frankenstein_Frankenstein_Core_Types_bindExpr`
both appear as $0 stubs.  Same definition, two different mangled forms — the
defined function exists under ONE name; refs from other modules use the OTHER.

Likely fix in `src/Frankenstein/Core/Linker.hs`:
- The `mangleName` function (~line 193) prepends module name with `_`
- `qualifyTop` in Emitter.hs uses `esModulePrefix` which may produce a different case
- Reconcile so the two converge

Eliminating these 16 case-collision stubs should remove a major class of
crashes (Frankenstein's own self-references).

### Target B: GHC.Internal.Base primitives

47 stubs are GHC.Internal.* — most critical:
- `GHC_Internal_Base_id$0` (identity)
- `GHC_Internal_Base_pure$0` (Applicative pure)
- `GHC_Internal_Base__$0` (function composition `.`)
- `GHC_Internal_Base_zpzp$0` (`++`)
- `GHC_Internal_Classes_not$0`, `_max$0`
- `GHC_Internal_Classes_zdfEqList$0`, `zdfOrdTuple2$0` (class instance dicts)

Two paths:
1. **Bridge-side**: have `GhcBridge.CoreTranslate.trExpr` substitute these
   primitives with inlined IR (e.g., `id` → `\x -> x`)
2. **Runtime-side**: provide real C functions in `runtime/kk_runtime.c`

Bridge-side is cleaner (no runtime/Frankenstein bridge mismatch later).

## Regression Guards In Place

- `bash test-hellos.sh` — 25/26 baseline (unchanged)
- `bash test-surd-mercury.sh` — 9/9 baseline (all byte-identical with native)
- `bash regression-guard.sh --strict` — umbrella, nonzero exit on degradation

Run before/after every bootstrap-restoration commit to confirm the
language-bridge / surd work isn't undone.

## Bootstrap Test Recipe

```bash
rm -rf self-host/obj
bash self-host/build.sh 2>&1 | tee /tmp/bootstrap.log | \
  grep -E 'Phase [0-9]+c? results|Fixed-point|FIXED POINT'
```

Both default and `FRANKENSTEIN_EVIDENCE=plotkin` modes should be tested.

## Useful Diagnostic Commands

```bash
# List all NULL-returning stubs
nm self-host/frankenstein-self-compiler | grep '\$0$' | awk '{print $3}'

# Count stubs by origin module
nm self-host/frankenstein-self-compiler | grep '\$0$' | \
  awk '{print $3}' | awk -F'_' '{print $1"_"$2}' | sort | uniq -c | sort -rn

# Find MLIR diff between stages for smallest module
for f in self-host/obj/stage2/*.mlir; do
  basename=$(basename "$f")
  s2=$(wc -l < "$f"); s3=$(wc -l < "self-host/obj/stage3/$basename" 2>/dev/null||echo 0)
  diff=$((s2 - s3)); abs=${diff#-}
  [ "$abs" -gt 0 ] && [ "$abs" -lt 50 ] && echo "$basename: diff=$diff"
done

# Get backtrace from latest crash
coredumpctl gdb $(realpath self-host/frankenstein-self-compiler) <<<'bt 10
quit'
```
