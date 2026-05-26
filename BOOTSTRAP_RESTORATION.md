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

## Two Concrete Restoration Targets

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
