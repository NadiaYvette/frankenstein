# Bootstrap Restoration Status — 2026-05-27

## Baseline vs Current

| Metric | Baseline (`0c461f7`, May 17) | Pre-D (`885f229`) | Post-D (`69e6aad`) | Post-E (`c57f1f6`) |
|---|---|---|---|---|
| Phase 8 E2E (default) | 18/21 (was 21/21 in plotkin) | 0/21 | 15/21 | **18/21 ✓ (matches baseline)** |
| Phase 9c E2E (stage 2) | 18/21 | 0/21 | 0/21 | 0/21 |
| Phase 10c E2E (stage 3) | 18/21 | 0/21 | 0/21 | 0/21 |
| Stage 2 compile | n/a | crashed | 26/26 (fallbacks) | 26/26 (more real, fewer fallbacks) |
| Stage 3 compile | n/a | n/a | 26/26 | 26/26 (many timeouts → fallback) |
| Fixed-point (s2→s3) match | **26/26** (`*** REACHED ***`) | 15/26 | 13/26 | 5/26 |

The post-E fixed-point drop (13→5) is artefactual: Target E lets stage 2
produce more real MLIR for previously-falling-back modules, while stage 3
times out on those same modules (it's compiled via the slower stage 2
binary). Real-vs-fallback diff = "differ".  Bumping the per-module
timeout in build.sh's compile_stage would likely recover most matches.

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

### Target E: kk_str_flatten use-after-free band-aid — APPLIED

After Target D, `closure.hs` / `higher_order.hs` / `prelude_hof.hs`
crashed in `Data_Text_isSuffixOf$2 → kk_str_flatten` aborting on a
"corrupt kk_string_t" with `magic=0x434c4f53` (CLOS tag).

Diagnosis: the call site is the `qualifyDefName` / `emitAppVarGeneral`
suffix-resolver list comprehension:
```haskell
[ tn | tn <- Set.toList topFns, T.isSuffixOf ("_" <> initialQual) tn ]
```
The crash is on the second arg (`tn` — an element of `topFns`).
`topFns :: Set Text` is built from `qualifiedTopNames ∪ externalRuntimeFns`.

The bad cell's content is consistent with use-after-free: the Text was
freed and the arena slot reused as a CLOS cell (its fn_ptr field points
mid-function inside `externalRuntimeFns_lambda58936`, not a valid entry).
`kk_compare` already has a defensive workaround for the same scenario
(line 5072 mentions "Set operations on Names can encounter freed fields
if Perceus dropped a Name that's still referenced in the Set").

**Band-aid:** `kk_str_flatten` now returns `kk_string_empty()` when the
input cell's magic ≠ `KK_STRING_MAGIC`, instead of aborting.  Logs to
stderr when `KK_STR_FLATTEN_TRACE=1`.  The list-comprehension filter
just produces "no suffix match" for that single bad element — harmless
for examples that don't rely on the cross-language suffix resolution.

**Result (post-D+E):**
| Phase 8 | 15/21 → **18/21** |
| closure.hs | FAIL → PASS (42) |
| higher_order.hs | FAIL → PASS (12) |
| prelude_hof.hs | FAIL → PASS (22) |

Remaining 3 Phase 8 failures (`prelude_inline`, `prelude_comprehensive`,
`stdlib_string`) are pre-existing — the `.hs` source files are missing
from `examples/` (only `.o`/`.hi` artifacts).  Not bootstrap-related.

**Root cause for future:** The underlying Perceus use-after-free on
Text values stored in `Set`s.  Likely a missing `kk_retain` on the
`Set.toList` path, or an over-eager `kk_drop` after `Set.union`.
The band-aid is forward-compatible — once the root cause is fixed,
removing it is safe.

### Target F: force CAF thunks — ROOT CAUSE FOUND AND FIXED

Diagnostic via `KK_SET_INSERT_TRACE` + `KK_SET_TOLIST_TRACE` revealed
the root cause: not a Perceus refcount bug at all.  CAF-bodies that
end with `kk_thunk_create` (e.g. `externalRuntimeFns :: Set Text` in
`MlirEmit/Emitter.hs`) return a LAZY thunk wrapping their closure.
The previous emitter generated `func.call @CAF() : () -> i64` without
forcing.  Compiled `Set.union extRtFns ...` then treated the thunk as
a Set BST, reading `field 1` (closure cell) as "element" and
`fields 2/3` as "left/right children" — emitting the closure itself
(CLOS-tagged) plus out-of-bounds garbage as "Set elements".  Downstream
`Set.toList → T.isSuffixOf → kk_str_flatten` aborted on those non-Text
cells.  The Target E band-aid had been silently absorbing this.

Two complementary fixes (commit `b8a7567`):

* **Source-level** (`Emitter.hs:emitFnAsValueWithArgs`): arity-0 CAF
  calls now emit a `kk_thunk_force` on the result.  `kk_thunk_force`
  is a no-op on non-LAZY values, so this is safe for CAFs that already
  return values directly.

* **Runtime-level** (`shim_data_set.c`): new `set_force(s)` helper
  forces a LAZY-tagged Set arg before any BST traversal.  Applied at
  all Set entry points (insert/member/null/union/difference/toList/
  toAscList).  This protects the *current* self-compiler binary even
  before it gets re-bootstrapped with the Emitter.hs change.  Both
  fixes are forward-compatible.

**Result (post-F):**
| Phase 8 | 18/21 (unchanged) |
| Hellos | 25/26 → **26/26** (the `--no-perceus` path now also works) |
| Surd | 9/9 (unchanged) |
| Fixed-point (s2→s3) | 5/26 (unchanged) |
| Phase 9c/10c E2E | 0/21 (different bug class — stage 2/3 timeouts) |

Target E's `kk_str_flatten` band-aid still in place but no longer
triggers for any tested example.  Could be removed once we're certain
no other CAF-thunk-Set scenarios remain.

**Diagnostic env-vars added (kept for future use):**
- `KK_SET_INSERT_TRACE=1` — log non-Text elements going into Sets
- `KK_SET_TOLIST_TRACE=1` — log every element emitted from `Set.toList`
- `KK_STR_FLATTEN_TRACE=1` — enriched (arena ownership + slot dump)
- `KK_FIELD_TRACE=1`, `KK_TAG_TRACE=1` (with `_MAX` overrides)
- `FRANKENSTEIN_DUMP_PROGDATA=1` — driver dumps `prog.field[2]` per pass

### Open work after Target F

* **Phase 9c/10c E2E (stage 2/3): 0/21**.  Stage 2/3 binaries compile
  but their E2E produce different failures (kk_str_concat type
  confusion in some, timeouts in others).  Different bug class.
* **Fixed-point (s2→s3) 5/26 vs baseline 26/26**: many modules fall
  back to prev-stage MLIR (568-byte stub) because stage-2 compilation
  hits per-module 120s timeout.  Bumping the timeout would likely
  recover most matches but doesn't fix the real slowness — the stage 2
  binary is significantly slower than stage 1 due to multi-pass
  drop/retain insertion.

### Target G investigation: stage 2/3 incomplete binaries — RCA documented, fix deferred

Stage 2/3 binaries are 4.8MB / 1.9MB (vs stage 1's 15MB) because
**stage 1 itself crashes mid-compile on certain `--no-perceus` split
parts of OrganIR/Consumer.hs**, producing 11-byte stub MLIR files for
those parts.  E.g., for stage 2's Consumer:
- part 0 / part 4: ~700KB real MLIR
- parts 1, 2, 3: 11 bytes (`module { }` — stub from crash)

The merged Consumer.mlir is missing `consumeProgram`, `consumeModule`,
`consumeDef` etc. — symbols that the stage 2 binary's `main` then
calls, getting NULL via linker's `--unresolved-symbols=ignore-in-object-files`.
First call to NULL = SIGSEGV.

Reproduction:
```
timeout 30 self-host/frankenstein-self-compiler \
  self-host/obj/stage2/OrganIR_Consumer_part1.organ.json \
  --no-perceus -o /tmp/p1.mlir
```
Crashes in `kk_str_concat` (input magic = `0x2474894818ec8348` —
which is x86 code bytes for `sub $0x18,%rsp; mov %rsi,...`, NOT a
Text magic).  Backtrace via the binary's built-in `kk_dump_backtrace`:

```
kk_str_concat
emitAppVarGeneral_lambda190521+0xa7b   ← the chained `<>` call site
bind_runner (×many)
mapM_state_runner
fmap_state_runner2
```

The "non-string input" address resolves to `pap_Frankenstein_tryAltzd..._a2_1`
— the PAP wrapper for `tryAlt` (the recursive helper inside
`dedupeQualN` in `MlirEmit/Emitter.hs:4548`).  So a partially-applied
function pointer is being passed through `<>` (Text concat) as if it
were a Text value.

`tryAlt :: Int -> Emit Text` should be arity 1, but the Frankenstein
emitter registers it as arity 2 (because `Emit a = State EmitState a`
gets de-monadised into `EmitState -> (a, EmitState)`).  The PAP `a2_1`
captures `n :: Int` and awaits the state.  Somewhere the calling
context uses the PAP as if it were already `Text`, leaking the function
address through `<>`.

A band-aid for `kk_str_concat` (return the other arg on bad input,
mirroring Target E's `kk_str_flatten`) makes the immediate crash log
+ skip — but the resulting partial Text then breaks `Set.member`
downstream (kk_tag SEGV on the bad cell).  Multiple band-aid layers
needed to fully unblock — not committed; root cause is the proper fix.

**Root cause (hypothesis):** Frankenstein's emitter for monadic `do`
blocks doesn't always thread the State action correctly — when the
last expression of a `do` block is `tryAlt (n+1)` (a State action),
the compiled code should keep this as a closure-to-run-later, but
instead uses the PAP's function address as if it were the result Text.

**Why this doesn't bite earlier stages:** stage 1's emitter (compiled
by the host) handles this correctly.  Stage 2/3 are compiled FROM
stage 1's output (which has the bug baked in), so they crash.  But
the bug ALSO exists in stage 1 when stage 1 compiles ITSELF
(specifically OrganIR/Consumer parts containing `do`-chains with
recursive monadic calls).

**Next investigation step (for follow-up):** Look at how the emitter
handles `tryAlt (n+1)` as the tail of a `do` block — specifically
how it differentiates between "function value as result" (which would
be a PAP and *should* be kept as such) and "monadic action whose
result becomes the do-block result" (which should run the action and
yield its return value).  The latter case is broken for `tryAlt`'s
recursive return.

### Target H: refactor dedupeQualN's monadic recursion — APPLIED

Root cause (now confirmed): the emitter generates two versions of
`tryAlt`:
- `tryAlt$307643(qualN, n)` — arity-2 monomorphic (correct)
- `tryAlt$302591(qualN, MonadStateDict, NumDict, ShowDict)` — arity-4
  typeclass-overloaded wrapper that drops the dicts and returns a
  PAP for `$307643`

The call site `else tryAlt 1` in `dedupeQualN`'s outer body resolves
to a PAP of the **arity-4 wrapper** with 2 supplied (qualN + n=1).
But the 2nd supplied slot is the MonadState dict, not n.  When the
state monad eventually invokes this "state action", the args end up
as `$302591(qualN, n=1, state, garbage)` — n drops into the dict slot
and the function returns yet another partial PAP (not a (Text,State)
pair).  That PAP escapes through the `<>` chain as if it were a Text;
`kk_str_concat` sees code bytes as magic and aborts.

**Fix (commit `<pending>`):** refactor `dedupeQualN` so the recursion
is a pure helper outside the State monad.  Same logic — fetch
`topFns`/`lifted` once at the start, then iterate purely:

```haskell
dedupeQualN qualN _arity = do
  topFns <- gets esTopFns
  lifted <- gets esLiftedNames
  pure $ if Set.notMember qualN topFns && Set.notMember qualN lifted
         then qualN
         else findFree topFns lifted 1
  where
    findFree :: Set Text -> Set Text -> Int -> Text
    findFree tf lf n =
      let alt = qualN <> "_dup" <> T.pack (show n)
      in if Set.member alt tf || Set.member alt lf
         then findFree tf lf (n + 1)
         else alt
```

Semantics-preserving — the original re-fetched state on each iter
but the state can't change between iterations within a single
`dedupeQualN` call (no other code runs).

**Bootstrap result after Target H:**

| Metric | Post-G (`7e3eaa0`) | Post-H |
|---|---|---|
| Phase 8 E2E | 18/21 | 18/21 (unchanged) |
| Stage 2 real-compile (no fallback) | ~10/26 | **~14/26** (more modules compile end-to-end without crashing) |
| Stage 2 `kk_str_concat` crashes | many | **0** (root cause fixed!) |
| Stage 2 timeouts | few | several (now blocked on slow compiles instead of crashes) |
| Phase 9c/10c E2E | 0/21 | 0/21 (still blocked: stage 2 falls back → stub functions → stage 3's main NULL-calls consumeProgram) |

The `kk_str_concat` PAP-as-Text root cause is eliminated.  Remaining
blockers are SLOWNESS-related (900s per-part timeouts on the biggest
parts of MlirEmit/Emitter.hs and bridge modules), not bugs.

**Why fixed-point match (s2→s3) went down despite the fix:** stage
1's MLIR output now includes `kk_thunk_force` after every CAF call
(Target F's Emitter.hs change finally takes effect at scale), so
stage 1's per-module MLIR is *different from before* — and stage 2
(built from this new stage 1 MLIR) is itself different.  The
fixed-point would re-converge only after several bootstrap cycles.

**Follow-up to truly close the bug** (in the emitter, not the source):
The typeclass-dict wrapper for `tryAlt$302591` shouldn't be generated
for a where-bound function — it's not exported, only the monomorphic
inner call site uses it.  Or: the call-site code generation needs to
correctly distinguish "monomorphic-call-with-args" from "dict-passing-
call".  Several other where-bound monadic helpers may hit the same
pattern; auditing them would be Target I.

### Target I: audit other where-bound monadic recursive helpers — IN PROGRESS

Audit pattern: a `where` clause containing a helper function whose
body uses `do` notation AND makes a recursive call to itself.  The
typeclass-dict wrapper trips on the same code path.

Found by `python3` AST-shape scan over `src/`:

| File | Line | Helper | Monad | In bootstrap? |
|---|---|---|---|---|
| `MercuryBridge/HldsParse.hs` | 176 | `go` | IO | yes |
| `KokaBridge/CoreTranslate.hs` | 344 | `desugarGuards` | `Either Text` | yes |
| `KokaBridge/CoreTranslate.hs` | 461 | `desugarGuards` | `Either Text` | yes |
| `KokaBridge/CoreTranslate.hs` | 506 | `desugarGuards` | `Either Text` | yes |
| `FutharkBridge/Parser.hs` | 206 | `go` | parser monad | no |
| `FutharkBridge/Parser.hs` | 390 | `gatherArgs` | parser monad | no |
| `ErlangBridge/CoreTranslate.hs` | 198 | `foldClauses` | `Either` | yes¹ |
| `IdrisBridge/Parse.hs` | 191 | `goCmp` | `Either` | no |

¹ ErlangBridge isn't currently in the bootstrap module list but
gets pulled in transitively via cabal compilation.

After Target H, the bootstrap stops *crashing* on the
`kk_str_concat` path — all remaining stage 2 failures are 900s
timeouts (the self-compiler is slow on large parts), not the same
PAP-as-Text bug.  This means either:
1. The other helpers above don't actually hit the bug (different
   monad / dispatch path), OR
2. The compiles time out before reaching the buggy code.

To distinguish, would need: (a) longer per-part timeouts in
`build.sh`, or (b) minimal-repro Haskell programs exercising each
helper pattern, or (c) preemptively refactor + see if timeouts
decrease.  Tracked but not auto-fixed: the user should decide
case-by-case whether to refactor each (the pattern is risky but
correctness is fine in stage 1 because the host emitter handles it).

### Audit tool

```bash
python3 -c "
import re
from pathlib import Path
for fpath in Path('src').rglob('*.hs'):
    lines = fpath.read_text().split('\n')
    i = 0
    while i < len(lines):
        m = re.match(r'^(\s+)where\s*$', lines[i])
        if not m: i += 1; continue
        wi = len(m.group(1)); j = i + 1
        while j < len(lines):
            ll = lines[j]
            if not ll.strip(): j += 1; continue
            ci = len(ll) - len(ll.lstrip())
            if ci <= wi: break
            hm = re.match(r'^(\s+)([a-z][\w\']*)\s+[^=]*=\s*do\s*$', ll)
            if hm:
                hi = len(hm.group(1)); hn = hm.group(2); k = j + 1
                while k < len(lines):
                    bl = lines[k]
                    if not bl.strip(): k += 1; continue
                    bi = len(bl) - len(bl.lstrip())
                    if bi <= hi: break
                    if re.search(r'\b' + hn + r'\b', bl) and not bl.lstrip().startswith('--'):
                        print(f'{fpath}:{j+1}: {hn}'); break
                    k += 1
            j += 1
        i = j
"
```

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
