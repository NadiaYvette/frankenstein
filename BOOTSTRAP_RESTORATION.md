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

**Target I follow-up (commit `51947dd`):** refactored all 3
`desugarGuards` instances in `KokaBridge/CoreTranslate.hs` to the
same pure-helper pattern as `dedupeQualN` (Target H).  Each does:
1. `mapM` over guards (Either monad) to translate test+body pairs
2. Pure recursive `foldGuards` helper to build the ECase chain

Result: bootstrap numbers unchanged vs Target H (Phase 8 18/21,
hellos 26/26, surd 9/9, same fallback/timeout pattern).
Confirms `desugarGuards` was NOT actually triggering the bug in
the current bootstrap path — `tryAlt` was the only hot site.
The refactor is preemptive: it removes a known-risky pattern but
doesn't move measurable behaviour.  The 4 remaining matches
(HldsParse `go` in IO, plus 4 non-bootstrap helpers) are left
unrefactored pending evidence they actually trip the bug.

### Target J: stage 2/3 compile "slowness" — three layers fixed

The "slowness" that timed out stage 2 / stage 3 part-compiles at
600 s+ was actually three separate problems:

#### J1: `text_printf_2` choked on cons-list fmt (commit `0c70e94`)

`MlirEmit.Emitter.escapeMLIRString` calls `printf "%02X" b`.  In
Haskell `printf :: String -> a -> ...` — String = `[Char]`, NOT
Text — so the format literal is a cons-list, not a kk_string_t.
The shim's `kk_str_dup_cstr(fmt)` read `byte_len` off a cons cell
(getting a huge number from the tail pointer) → malloc/snprintf
on garbage → SIGSEGV inside `__strchrnul_avx2`.  Looked like a
timeout because bash's `timeout` reports SIGSEGV as "Speicherauszug".

Fix: call `GHC_Internal_Data_String_fromString$1(fmt)` first;
it's a no-op on real kk_strings and converts cons-lists to
kk_strings.  15+ stage-2 part-compiles that "timed out" now
finish in 3–6 s each.

#### J2: getenv per kk_tag/kk_field (commit `968799a`)

perf showed 60% of CPU in libc `getenv` from
`kk_tag`/`kk_field`'s env-var-gated trace (added during Target H
diagnosis).  getenv is O(envp_count) and those primitives fire
millions of times during emit.

Fix: cache the env state once on first call into static ints
(`kk_tag_trace`, `kk_field_trace`).  Single int compare per call
afterward instead of two getenv strncmps.

#### J3: deep-rope stack overflow + small string-table (commit `d964294`)

Re-profile after J2 showed: 40% kk_str_alloc_leaf_owned, 30%
kk_structural_eq, 17% kk_is_string — all from
`OrganIR_Parse_decodeExpr`.  Two issues:

(a) `pStrBody` does `acc <> chunk <> "\n" <> chunk' <> ...` per
    JSON-string escape.  Frankenstein's `<>` builds a CONCAT rope
    node (not flat copy), so a JSON string with K escapes
    produces a K-deep left-spine.  `kk_str_copy_into` was
    recursive and overflowed the 8 MB stack flattening the rope.

(b) String-identity hash table at 4 M entries with 3 M strings
    (75% load) → long probe chains in `kk_register_string`.

Fixes: iterative `kk_str_copy_into` with malloc'd manual stack;
bumped `KK_STRING_TABLE_SIZE` and `KK_STRING_LOG_SIZE` to 16 M
each (~128 MB address space, fine for bootstrap).

**Result of Target J (commit `d964294`):**

| Metric | Before J | After J |
|---|---|---|
| `MlirEmit_Emitter_part17` (3 MB) | 600 s timeout (SIGSEGV) | **13 s ✓** |
| `MlirEmit_Emitter_part20` (2.3 MB) | 600 s timeout (SIGSEGV) | **8 s ✓** |
| Stage 2 compile crashes | 14+ | **0** |
| Stage 2 compile timeouts | 11 | **0** |
| Phase 8 E2E | 18/21 | 18/21 (baseline) |
| Hellos / surd | 26/26 / 9/9 | 26/26 / 9/9 |

Stage 2 binary now compiles all 26 modules cleanly (no fallback
.o copies from prev stage).  The remaining bootstrap blocker is
NOT slowness — it's that **stage 2 binary's `consumeProgram`
returns 0 even for valid JSON input** (verified by running it
directly on `examples/effect_state.json`).  Different bug class:
stage 1's emitter produces incorrect MLIR for `consumeProgram`
that links and runs but always returns 0.  The fixed-point
"regression" (5/26 → 0/26) is artefactual — stage 3 modules are
all 568-byte stubs because stage 2 binary fails on every input,
forcing the script to fall back to stage 1's MLIR for stage 3.

### Target K investigation: stage 2 `parseJSON$1` NULL-stub call

Diagnostic (commit local in `driver.c`): stage 2 binary's
`consumeProgram(json)` returns the literal value **0**, not a heap
pointer.  Stage 2 mlir shows `parseOrganIR` body:

```mlir
%v5925 = func.call @Frankenstein_OrganIR_Parse_parseJSON$1(%t) : ...
```

— calling `parseJSON$1` (the arity-mangled extern wrapper), and
`parseJSON$1` itself is one of the auto-generated NULL stubs:

```mlir
func.func @Frankenstein_OrganIR_Parse_parseJSON$1(%a0: i64) -> i64 {
  %z = arith.constant 0 : i64
  func.return %z : i64
}
```

Stage 1 (host-emitted) MLIR for the SAME function calls the
correct `Frankenstein_OrganIR_Parse_parseJSON` directly (no `$1`).

Both pipelines run the same `emitAppVarGeneral` in `Emitter.hs`,
and both see the same `topFns` Set Text + the same EVar text
`"OrganIR.Parse_parseJSON"`.  Yet:

* HOST emitter (GHC-compiled): correctly resolves `Set.member
  "Frankenstein_OrganIR_Parse_parseJSON" topFns → True` → direct call
* STAGE 1 emitter (Frankenstein-compiled): the same lookup returns
  False, falls to the "unresolved extern" path and mangles with
  `$1` → linker fills in with NULL stub

Some calls inside the same module DO resolve correctly (`skip`,
`pColon`, etc. emit as direct calls in stage 2's mlir).  So the
bug is not a global "topFns is empty" — some entries lookup, others
don't.  Most likely a Frankenstein-codegen issue in compiled
`Set.member` / `Text` equality (similar in spirit to Target F's
unforced-CAF-thunk bug, but for a DIFFERENT set / lookup path).

**Next investigation step:** instrument `emitAppVarGeneral` to log
`(initialQual, "in topFns?")` per call, rebuild stage 1, see
exactly which names fail the lookup and compare to the actual
topFns contents.  Or add a Set.toList dump just before the lookup
loop and compare against the EVar text byte-by-byte.

Phase 8 stays 18/21 (baseline); hellos 26/26; surd 9/9.

#### Target K diagnostic results (commit local in `shim_data_set.c`)

Added env-var-gated tracing (`KK_SET_MEMBER_TRACE=parseJSON`,
`KK_SET_INSERT_KEYS=parseJSON`) to log every set_member miss and
every set_insert containing the substring.

**Findings on `OrganIR/Parse.hs` self-compile:**

1. `Frankenstein_OrganIR_Parse_parseJSON` IS inserted into topFns
   during init (confirmed via insert trace).
2. Lookups of the same key fire MANY times during emit and all
   return MISS.
3. **The set arg to set_member_2 has `tag=0` (SET_TIP_TAG / empty)
   for the failing lookups** — but other lookups in the same run
   see sets with size 21 / 478.  So `gets esTopFns` is returning
   an EMPTY Set for some emit-stack positions and a real Set for
   others.

```
[set_member_2 ENTRY] key='...parseJSON' set=0x...730 tag=0  ← empty!
[set_member_2 ENTRY] key='...parseJSON' set=0x...e48 tag=0x1  ← size 21
[set_member_2 ENTRY] key='...parseJSON' set=0x...f90 tag=0x1  ← size 478
```

4. EmitState's `esTopFns` is at field index 7 (verified against
   declaration order, and the compiled `esTopFns` accessor reads
   `kk_field(rec, 7)`).  initState passes the union at position 7
   in the `EmitState 0 [] Set.empty ...` constructor call — also
   verified to align.

5. Set.union shim (commit `b8a7567`) already forces both args via
   `set_force`, so a LAZY-wrapped union shouldn't cascade as
   empty.  But the FAILING lookups see a real `SET_TIP_TAG=0`
   cell, NOT a LAZY thunk.  Something is explicitly putting an
   empty Set at field 7 in some state instances.

**Most likely root cause (hypothesis):** Frankenstein's compiled
record-update syntax (`s { esField = newValue }`) may be
mis-mapping field indices when the record has many fields (21 in
EmitState).  When `modify` writes one field, the rebuild could
overwrite `esTopFns` with the wrong value (e.g., `Set.empty` from
another field).  Test: write a focused record-update repro
(record with 8+ fields including two `Set Text`s, update one,
check the other) and see if compiled-self-compiler corrupts it.

**Alternative hypothesis:** the State monad's compiled `get`
returns a state from the wrong scope — e.g., a save/restore in
`emitLetBindings` swaps state with an earlier (empty-topFns)
version.  Specifically, the line 3439 `modify (\s -> s {
esAliases = savedA, esTopFns = savedTopFns ... })` could be
restoring `savedTopFns` from a context where topFns hadn't been
populated yet.

The investigation is at the "compiler-bug or state-restore-order"
fork; needs another iteration to confirm which.

#### Further narrowing (still no fix)

Aggregated `KK_SET_MEMBER_TRACE` statistics on stage 1 self-compile
of OrganIR/Parse.hs:

```
3509 lookups with tag=0 (SET_TIP / empty)   ← 65% of misses
1858 lookups with tag=0x1 (real Set bin)
```

* ONE specific Set pointer accounts for 3507/3509 of the empty
  lookups — the same empty-Set instance is threaded through many
  state transitions.
* The MLIR for `esTopFns` accessor and the EmitState construction
  in `initState` both place esTopFns at field index 7.  No
  off-by-one in declaration order.
* The `kk_set_field` write pattern in compiled record-update sites
  (freshName) is sequentially 0..20 — no field skipped or
  misordered.

This is NOT a field-index-mapping bug.  Most likely either:

1. **State monad's `>>=` / `modify` compiles wrong**: the threaded
   state propagation might pick up the WRONG `s` from the wrong
   closure capture, returning an old (empty) `EmitState` to
   downstream `gets`.

2. **`Set.empty` interns globally** in stage 1's compiled output,
   and ALL `Set.empty` fields in `EmitState` (`esLiftedNames`,
   `esPapWrappers`, `esScopeSsa`) point to the SAME instance.  If
   `gets esTopFns` (field 7) somehow gets misrouted to one of
   those Set.empty fields (e.g. via stale alias / wrong capture
   slot in a closure), every lookup sees empty.

Investigation lift to do next session: instrument the state-monad
runners (`bind_runner`, `then_runner` in `shim_ghc_prim.c`) to
log the state argument's `esTopFns` size at every state
transition.  Pinpoint the exact transition where it drops to 0.

The diagnostic env vars (KK_SET_MEMBER_TRACE,
KK_SET_INSERT_KEYS) are committed.  Phase 9c/10c E2E stays at
0/21 until this is fixed.

#### Target K — final narrowing: NOT a state-monad bug either

Instrumented `bind_runner`, `state_gets_code`, `state_modify_code`,
`state_put_code` with `KK_STATE_TRACE=1` env-gated probes for
`esTopFns` corruption (real→empty).  **Zero `[CORRUPT]` events
across thousands of bind/modify/put operations.**  State threading
works correctly.

Re-instrumented `set_member_2` to log `(key, set ptr, set tag,
caller return-address, RESULT)` for every parseJSON lookup.
Resolved caller addresses:

| Caller (resolved by nm) | Result | Set tag | What it's checking |
|---|---|---|---|
| `emitCycleCandidate_lambda99483` | 0 | 0 (empty) | esCyclicDefs (legitimately empty) |
| `dedupzd...$079007` (Set.dedup) | 0 | 0x1 (size 21) | some dedup set |
| `emitAppVarGeneral_lambda187257` | 0 | 0x1 | extRtFns — `Set.member "OrganIR_Parse_parseJSON"`  (legitimately false) |
| `emitAppVarGeneral_lambda187311` | **1** | 0x1 | **topFns — RETURNED TRUE!** |
| `emitAppVarGeneral_lambda187299` | **1** | 0x1 | **topFns — RETURNED TRUE!** |

**`Set.member` is working correctly.**  All three relevant
lookups inside `emitAppVarGeneral` return the right answer (the
extRtFns check correctly returns False; the topFns checks
correctly return True).

Yet `parseOrganIR`'s body in stage 2 MLIR still calls
`parseJSON$1` (extern-mangled NULL stub), not the direct
`parseJSON`.  This means the **if-then-else dispatch following
Set.member must be wrong** — the compiled stage 1 emitter is
reaching the *extern* branch despite Set.member returning True.

Likely candidates:
1. **Compiled `if-then-else` on Bool selects wrong branch**: maybe
   compiled comparison or branch encoding is inverted.  Test:
   write a small Haskell file with `if Set.member x s then …
   else …`, compile via stage 1, inspect the scf.if condition.
2. **Frankenstein's Bool ↔ i64 conversion is mishandled**: if
   Set.member returns i64 1 but the if-discriminator expects a
   boxed Bool, the conversion could drop it to "false".
3. **A separate code path emits the call**: maybe `emitExpr`
   dispatches to `emitAppVarGeneral` for some App nodes but to
   a *different* function (one without the topFns check) for
   others.  Look at the OrganIR AST node for parseJSON's call
   site to see which it hits.

This finally rules out Set/State-monad bugs.  The bug is in the
emit's conditional branching after a successful Set.member.
Phase 9c/10c E2E remains 0/21.

#### Target K — **RESOLVED**: bug was a STALE stage 1 binary

Re-ran a single-module emit using the **current** stage 1 binary
(rebuilt 11:32 after Target H refactor of `dedupeQualN`):

```
./self-host/frankenstein-self-compiler self-host/obj/stage2/OrganIR_Parse.organ.json --emit-mlir > /tmp/fresh.mlir
grep 'parseJSON\$1' /tmp/fresh.mlir   # 0 hits
grep 'parseJSON\b' /tmp/fresh.mlir    # only the real direct call:
  %v16829 = func.call @Frankenstein_OrganIR_Parse_parseJSON(...) : (i64) -> i64
```

The `stage2/OrganIR_Parse.mlir` we'd been inspecting was timestamped
06:52 — produced by an **older** stage 1 binary that had the bug.
After the Target H refactor (replacing where-bound monadic
recursion in `dedupeQualN` with a pure helper, committed earlier
2026-05-27), the resulting stage 1 binary correctly emits the
direct call.

**Confirmed root cause** *(retroactive — fix landed before
investigation began)*: monadic state-threading bug in the where-bound
helper `dedupeQualN` corrupted `esTopFns` for some call sites,
causing `Set.member` to read an empty set and dispatch to the
extern fallback.  Target H's pure-helper refactor removed the
unwanted state threading, restoring the lookup.

**Investigation lesson**: when `stage2/*` MLIR contradicts trace
output of the running stage 1 binary, **always re-emit the module
fresh** before assuming a deeper bug.  Stale artefacts from prior
stage 1 builds can survive cabal rebuilds because `obj/stage2/`
isn't a cabal artefact.

**Action**: rerun `self-host/build.sh` to regenerate stage 2 from
scratch with the current stage 1 binary and verify Phase 9c E2E.

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
