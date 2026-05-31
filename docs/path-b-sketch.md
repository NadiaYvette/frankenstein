# Path B sketch — invert the closure-arg contract

> **Status**: design sketch, NOT implemented.  Written 2026-05-31 as the
> follow-up to Phase 12c step 8 (the C-shim retain class).  See memory
> file `bootstrap_871afa7_drop_class.md` for the root-cause analysis
> that motivates this.
>
> **Authorship**: drafted by Claude in response to the question
> "would Path B force enough audit of the closure-arg contract to be
> worth trying?"  The answer is yes — but the audit is the value, not
> necessarily the implementation.

## Context

After commit 871afa7 ("emitter: lifted lambdas drop their closure-arg
before return") the calling convention became: each lifted lambda
body consumes one ref of its own closure cell.  Phase 12c step 8
identified that this contract was never propagated to the C-shim
boundary (`shim_data_text.c::call_closure_1`, `shim_ghc_*::call1`,
`shim_ghc_dicts.c::shim_call1`, `driver.c::collectGlobalEffects` site)
nor to several places in compiled Haskell (the dszd-named derived
dictionary recursion that crashes during `OrganIR_Parse.asObj`).

Five surgical retains were committed (908f813) and audit
infrastructure was extended (a8c2cdf) to identify the recycled cell's
original tag, size, and recycler PC.  Bootstrap E2E is still 0/21
because the deeper class of issue — generated Haskell code with the
same calling-convention mismatch — remains.

Path A ("blanket retain at every indirect call site") was attempted in
the emitter, kept hellos green but broke stage-1 self-compilation
(self-test crashed at end of run, several Phase 8 examples regressed).
The mechanical retain leaks for closures that don't follow the
"body drops" convention (PAP wrappers, 0-capture lambdas, etc.) — so
the contract violation isn't uniformly fixable without per-site
reasoning.

Path B is the architectural alternative: invert the contract entirely
so callers manage closure lifetimes and bodies just borrow.  This is
the convention used in mainstream Perceus-style RC compilers
(Lean4, Koka).

## Current contract (post-871afa7 / e9437193)

| Site | Action |
|---|---|
| Emit lifted lambda body | (a) prologue: `kk_retain(cap_i)` × `countUses(cap_i, body)` for each capture; (b) end-of-body: `kk_drop(clos)` — cascades to captures only when rc=0 |
| Emit indirect call site | Extract fn ptr from `kk_field(clos, 0)`, call.  No rc op. |
| Caller side (Perceus) | `wrapRetains` inserts (N-1) retains before N uses of a Many-multiplicity var |
| Caller drop | Perceus emits explicit `kk_drop(clos)` only when `clos` is unused after a point (let-bindings, params) |

**Implicit assumption**: the call site transfers ownership of one ref
of `clos` to the body, which is responsible for releasing it.  The
C-shim invariant is symmetric — the shim receives a closure with one
ref, calls the body once, body drops it.

**For C shims that call the closure N times** (e.g. `text_break_pred`),
the recent fix added `kk_retain(closure)` to `call_closure_1` so each
call effectively becomes "borrow".  But the per-shim
`kk_drop(pred)` *at the end of the shim function* is still missing —
so we leak one closure ref per shim invocation.  The leak is
non-fatal but accumulates.

## Inverted contract (Path B)

| Site | Action |
|---|---|
| Emit lifted lambda body | Remove end-of-body `kk_drop(clos)`.  Body uses `clos` for field reads only — borrow. |
| Emit lambda prologue | Remove `capCounts` retains.  Captures are borrowed on demand; no per-use retains needed since the closure (and therefore captures via cascade) survives the body invocation. |
| Emit indirect call site | Add `kk_drop(clos)` after the call IF `clos` is dead after this call site (last use). |
| Caller side (Perceus) | `wrapRetains` becomes a no-op for closure values: no extra retains needed because the call doesn't consume the ref.  Source-level multi-use is naturally satisfied by the single ref. |
| Caller drop | Driven by liveness analysis: drop closure at last-use point.  (Perceus already does this for non-closure values; needs extending or generalizing to all values.) |

**Implicit assumption**: the caller owns `clos` and is responsible for
releasing it when done.  The body just reads from it.

## Coordinated changes required

### `src/Frankenstein/MlirEmit/Emitter.hs`

1. **`emitLambdaLift`** — strip the prologue capture retains
   (`capCounts` / `wrapRetains` calls, around line 3633–3641 in the
   current source) and the end-of-body `kk_drop(closFresh)` injection.
   Keep the field-extraction prologue.

2. **`emitAppVarGeneral` — 3 indirect-call sites** (oversaturated
   top-fn, local closure-indirect, oversaturated promoted-let; lines
   3242–3254, 3274–3289, 3322–3334 in the current source).  Add
   `kk_drop(clos)` after each indirect `llvm.call`.  Requires knowing
   whether `clos` is dead after the call.  The emitter currently
   doesn't track liveness; we'd need either:

   - A Perceus pre-pass that marks each call site as "consuming"
     (drop after) or "borrowing" (keep alive), **or**
   - Always drop, and rely on `wrapRetains` to insert pre-retains for
     multi-use vars (basically the Current contract inverted — same
     complexity, just at a different site).

### `src/Frankenstein/Core/Perceus.hs`

3. **`wrapRetains`** — currently retains `(count-1)` times for
   `count > 1`.  Under B, if the call doesn't consume, the multi-use
   pattern doesn't need extra retains: one ref is enough for all N
   uses.  So `wrapRetains` for closure-valued vars becomes a no-op.
   **But** the function doesn't distinguish closures from data; you'd
   need either type info or a separate analysis pass.

4. **Drop insertion** — Perceus currently inserts `EDrop` for unused
   let-bindings and unused params.  Under B, it ALSO needs to insert
   `EDrop` at last-use points of closure-valued vars.  This is a real
   change: from "drop unused" to "drop after last use."

### `runtime/kk_runtime.c`

5. **`kk_drop` cascade logic** — unchanged.  Cascade still happens
   when rc → 0; just no longer triggered per-body-invocation.

### C shims (`self-host/shim_*.c`)

6. **Every shim that takes a closure as arg** — must `kk_drop(clos)`
   before returning.  Currently, the shims rely on the body-drop
   happening inside the called closure.  Under B, the shim itself
   must drop.  Affected (non-exhaustive):

   - `shim_data_text.c::text_break_pred`, `text_foldl_strict`,
     `text_dropWhile_runner` (and any other Text functional helper)
   - `shim_ghc_list.c::ghc_foldable_foldl_3`,
     `ghc_foldable_foldr_3`, `ghc_foldable_find_2`,
     `forM_state_runner` (already retains f explicitly — would now
     need to drop too)
   - `shim_ghc_dicts.c::dropWhile_runner` and similar
   - Estimated 10–20 sites total; an audit pass via `grep -n
     'call_closure\|call1\|call2' self-host/shim_*.c` enumerates them

7. **`call1` / `call2` / `call_closure_1` / `shim_call1`** — the
   recent retain we added (commit 908f813) becomes wrong.  Under B,
   the call doesn't consume the closure, so no retain is needed in
   the helper.  But the shim *function* that holds the closure must
   still drop it at the end of its lifetime.  Net: revert step 8's
   retains in the call helpers, add drops at end of each shim
   function that owns a closure.

## Predicted break points

| Risk | Severity | Detection signal |
|---|---|---|
| Forgetting to drop a closure in one of the C shims → silent leak (unbounded RSS) | High volume but non-fatal | RSS growth in long-running compilations (surd-quintic the canonical stressor) |
| Inserting drops at wrong call sites in emitter → use-after-drop in callers expecting closure to survive | Crash class | `KK_RECYCLE_AUDIT=1` catches; hellos may miss if pattern doesn't appear |
| `wrapRetains` retains that no longer balance any drops → leak per source-level multi-use | Low (memory only) | Same RSS growth signal |
| Captures cascade behavior changes — they only cascade when closure itself dies, not per-invocation.  If any code path was implicitly relying on per-invocation cascade-drop of captures, it breaks | Unknown; hellos catches some, surd-quintic catches more | Bytewise diff vs native for surd-quintic |
| Tail-call optimizations interfere — if a body's last operation is a tail-call returning a closure, under Current the body's drop runs before the tail-call; under B no drop runs and the tail-call must handle the closure itself | Subtle, hard to catch in tests | Tail-call hot paths in factorial / streamed recursion |
| The `analyzeUsage` count for captures (e9437193 `capCounts`) was sized for body-drop balance; under B both are removed, but if anything else depends on the count, it breaks | Unknown; needs grep audit | Likely none, but check `Frankenstein.Core.ConsumingUses` callers |

## Comparison to Current

- **Current** is internally consistent (hellos pass, simple programs
  work).  The bugs we see are at *boundaries* where the contract is
  violated (some lifted lambdas don't actually drop, some derived
  methods don't follow the convention, C shims call closures N times
  without retain).

- **Path B** is more standard (matches mainstream Perceus
  implementations like Lean4 and Koka).  But the conversion requires
  coordinated changes in 6 files (emitter, Perceus, runtime, 3+ shim
  files), and you'd need to verify hellos + mercury + idris2 +
  surd-quintic all hold at each step.

## Honest recommendation

Path B is the *architecturally* right fix.  It's also a **5–8 hour
focused effort** with high risk of partial-state breakage between
checkpoints.  The audit it forces is valuable, but the same audit can
be conducted as a **diagnostic-only pass without changing the
contract**: instrument the emitter to label every kk_retain / kk_drop
/ kk_thunk_force / closure-call with its semantic role
("prologue-cap-i", "end-of-body-cascade", "wrapRetain-source-multi-
use", "perceus-unused-drop"), run on a failing module, and tabulate
which roles fire on which closures.  That tabulation tells you
whether B is necessary or whether a narrower fix at a specific site
suffices.

If you want to commit to B, the recommended approach:

1. Spin up a worktree on a `path-b` branch
2. Implement changes 1–7 above in order (emitter first, then
   Perceus, then shims one-by-one)
3. Sanity-test hellos after each change
4. When hellos + mercury + idris2 + surd-quintic all hold, run
   bootstrap and check Phase 9c E2E

If you don't, the diagnostic-instrumentation route (one session)
gives most of the same audit value at lower risk.

## Open questions for the implementor

1. **Liveness analysis** — Perceus currently does only a rough form
   ("is this var used?").  Path B needs "is this the last use?" which
   is a stronger property.  Existing infrastructure that touches this:
   `Frankenstein.Core.ElideConsumedDrops`, `Frankenstein.Core.
   FlattenPatterns`.  Does either already compute liveness?

2. **Mixed-mode bootstrap** — could B coexist with A's retain-on-call
   in C shims?  The shim retain would still leak under B (since B
   says no consume), but the shim would also drop at end — net zero.
   This means the existing 908f813 retains can stay; we'd only add
   the matching drops.  That's a smaller diff than full revert + redo.

3. **Test bootstrap as the canonical witness** — the goal is Phase
   9c E2E going from 0/21 → 21/21.  Each intermediate fix should
   show measurable progress (it did under step 8: stage 2→3 MLIR
   convergence moved 0/26 → 10/26).  Track this delta.

4. **Risk of B without complete audit** — partial B (e.g. emitter
   changes without matching shim changes) creates a more severe
   inconsistency than Current.  All-or-nothing.

## See also

- Memory `bootstrap_871afa7_drop_class.md` — root-cause analysis
- Memory `arena_recycle.md` — KK_RECYCLE / KK_RECYCLE_AUDIT
- Commit `e9437193` — per-occurrence retain count (turned out NOT
  to be the regression introducer; reverting it had no effect)
- Commit `871afa7` — the body-drop change Path B inverts
- Commit `908f813` — the C-shim retain fixes (Phase 12c step 8)
- Commit `a8c2cdf` — the audit ledger extension (recycle entry
  records orig_tag, size, recycler PC)
