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
- Commit `78723c3` (on branch `path-b`) — first attempt at
  implementing Path B; **NEGATIVE RESULT** documented below

## Addendum 2026-05-31 — empirical result (NEGATIVE)

Branch `path-b` (`78723c3`) implements Steps 1–7 of this sketch:
- Emitter: strip end-of-body `kk_drop(closFresh)`, add `kk_drop(clos)`
  after each of 3 indirect call sites
- All 4 shim call helpers (`call1`/`call2`/`call_closure_1`/
  `shim_call1`): revert the 908f813 retains
- 6 shim functions that take a closure: add `kk_drop(closure)` at
  return paths (text_break_pred, text_foldl_strict, find_2, foldl_3,
  foldr_3, plus retain-removal in forM_state_runner/dropWhile_runner)
- `capCounts` prologue retains KEPT (revised from sketch — captures
  still need balance since the body consumes them N times)
- Driver retain (908f813) KEPT (top-level fns, not closures)

**Results:**
- Hellos 26/26: PASS
- Phase 8 E2E: REGRESSED from 7/14 → 0/21
- Phase 9 stage-2 module compile: massive segfaults on nearly every
  module, worse than pre-Path-B baseline
- Generated MLIR has dangling function references like
  `'Nested_sumBoxTree' does not reference a valid function`

**Diagnosis:**

The "all-or-nothing" Path B is genuinely all-or-nothing — and we got
"nothing" because the contract has more moving parts than the
sketch enumerated.  Three additional concerns surfaced:

1. **`analyzeUsage` and Perceus's drop placement interact with the
   EmitState.**  The dangling function references in stage-1's
   output mean that Perceus's drops, under the new contract, are
   affecting the state monad threading that tracks top-level defs.
   The emitter's `esLiftedFns`/`esTopFns` accumulator might be
   getting consumed unexpectedly when its surrounding StateT
   threading hits a borrow-vs-consume mismatch.

2. **`wrapRetains` semantics under B aren't actually a no-op.**
   The sketch claimed "wrapRetains becomes a no-op for closure
   values," but in practice wrapRetains handles ALL Many-multiplicity
   vars uniformly, including closures.  Under B, a multi-use closure
   var still needs (N-1) retains because each of N call-site drops
   consumes one ref — which is the same logic as Current.  So
   wrapRetains shouldn't change.  This part of the sketch was wrong;
   the implementation correctly left it alone, but the framing was
   confusing.

3. **Field-accessor pattern interacts with B.**  Functions like
   `progDefs` do `kk_field → kk_retain(field) → kk_drop(rec0)`.  Under
   B, the caller passes rec0 with consume convention (top-level fn).
   That's unchanged.  But if rec0 is a *closure* (rare but possible
   for some derived methods), the kk_drop semantics might differ.

**Open hypotheses worth investigating next:**

- The end-of-body drop in lifted lambdas might be the ONLY change
  needed — keeping the existing call-site behavior (no kk_drop after
  llvm.call).  This would test whether B's "remove body drop" alone
  fixes the cascade-related issues without introducing the
  call-site-drop cascade of regressions.  Smaller risk surface.

- Alternative: the Path B sketch might be largely correct but the
  call-site `kk_drop(clos)` placement is wrong — perhaps it should
  be at the END of the SSA scope where `clos` is last-used, not
  immediately after the call.  Liveness analysis becomes required.

- Or: maybe the actual root cause IS at a smaller fault — a single
  shim function or single Perceus inference rule — and our
  C-shim-class fix (908f813) plus targeted Perceus fixes are the
  right path, not contract inversion.

**Recommendation**: do NOT merge branch `path-b`.  Treat the negative
result as informative data.  A more conservative next attempt would
be to keep this sketch's "Step 1 alone" (remove body drop) and see
whether hellos + bootstrap improve, before adding call-site drops.

## Addendum 2 — Path B + liveness retry (NEGATIVE)

Branch `path-b` `b35f0d2` integrates `Frankenstein.Core.Liveness` via
a new pass `Frankenstein.Core.InsertDrops`:

- `closureCandidates`: names appearing in `EApp` function position,
  intersected with `boundInExpr` (filters out top-level fn refs and
  GHC primitives).
- `insertPathBDrops`: walks the IR with a position counter matching
  `Liveness`'s evaluation-order numbering; at each subexpression
  whose range contains a candidate's last-use position, wraps the
  expression with a sequenced `EDrop`.
- `recursivelyInsert`: descends into `ELam` / `EDelay` bodies (each
  becomes its own lifted function at emit time, so each needs its
  own last-use drops).
- Wired into `app/Main.hs` after `insertPerceus`.

Tested three variants (hellos PASS 26/26 in all of them):

1. With `stripRetains` for closure candidates — Phase 8 0/21, MLIR
   errors like `'Nested_sumBoxTree' does not reference a valid
   function`.
2. + `ELam` recursive descent — same 0/21.
3. Without `stripRetains` (just the drop insertion) — same 0/21,
   stage 1's emitted MLIR shows `Nested_sumBoxTree` *renamed* to
   `Nested_kk_retain` — i.e., memory corruption on Def records, the
   def's `name` field pointing into a freed/recycled cell.

### Root cause identified

`emitLambdaLift` constructs closures **without retaining captures**
(`Emitter.hs` lines 3710-3722).  The closure construction itself
transfers ownership: `kk_set_field(closure, idx, capture)` stores
the capture but doesn't bump its rc.  So from the outer scope's
perspective, the `ELam` *consumes* its captures.

But my Liveness module treats `ELam` as "captures used at this
position," which makes outer-scope last-use of a capture land AT
THE ELAM.  My InsertDrops then inserts a drop AFTER the ELam — but
the closure already consumed the capture during construction.
**Double-consume.**

The Liveness module's semantics are correct for "outer-scope last
use" — ELam captures ARE last-used at the ELam position.  But my
InsertDrops misinterprets that as a drop site.  ELam takes
ownership; that's where the var dies, no explicit drop needed.

### What this teaches us about Path B's depth

The closure-arg contract is *not* the only ownership convention in
play.  At least three others exist that interlock:

1. **Closure construction ownership** — `emitLambdaLift` consumes
   captures by storing them without retain.  Symmetric C helpers
   (`make_closureN` in `shim_ghc_list.c`) retain before
   `kk_set_field`.  These two implementations have inconsistent
   conventions (Haskell-side emitter consumes; C-side helper
   retains-then-stores, net effect of retaining once).
2. **Cascade-drop of captures** — when a closure cell reaches rc=0,
   `kk_drop` recursively drops its captures.  This balances the
   construction-time consume.
3. **Body-drop of closure-arg** — what 871afa7 added and Path B
   reverts.  Per-invocation drop of the closure cell itself.

The Current contract's internal consistency relies on all three
working together.  Inverting just (3) without auditing (1) and (2)
produces the double-consume we see.

### What would actually be needed for Path B

To make Path B work end-to-end:

- **emitLambdaLift should retain each capture** before
  `kk_set_field` (matching the C helpers).  Then closure
  construction is conceptually "retain + store" — borrow semantics
  for the outer scope.
- **InsertDrops can then safely insert a drop at the ELam-position
  last-use** because the outer-scope ref still exists (the closure
  has its own retained ref).
- **Body-drop removal** (Path B step 1) still applies — the body
  doesn't consume the closure-arg, the caller drops it.
- **Caller call-site drops** are inserted at last-use position via
  InsertDrops, on closure variables.
- **stripRetains for closure candidates** may still be needed (the
  Perceus wrapRetains added retains assuming Current's per-call
  consume; under Path B those retains leak).  Or wrapRetains itself
  needs Path-B awareness.

This is a 5–10 hour effort with multiple interlocking checkpoints
in `emitLambdaLift`, the `InsertDrops` pass, the C shims, and
possibly Perceus's `wrapRetains`.  Each step needs hellos to remain
green, and stage 1 needs to compile correctly for the emitter
itself to work as a self-host module.

## Addendum 3 — Capture-retain attempt (NEGATIVE)

Branch `path-b` `3d66cc0` applies the addendum-2 recommendation:
`emitLambdaLift` now emits `kk_retain(capture)` before each
`kk_set_field(closure, idx, capture)`.  This matches the C-side
`make_closureN` pattern and makes closure construction
borrow+store semantics — the outer scope's ref to each capture
survives the `ELam`.

Two configurations were tested:

1. **emitLambdaLift retain + Path B body-drop removal + InsertDrops**
   (capture-retain combined with drop-at-last-use):
   - Hellos: PASS 26/26
   - Phase 6 self-test: PASS (progress! prior Path B attempts
     crashed at end of Phase 6)
   - Phase 7 factorial: PASS
   - Phase 8 E2E: STILL 0/21
   - Stage 1's emitted MLIR has renamed functions
     (`Nested_sumBoxTree` → `Nested_kk_drop`), indicating Def-record
     memory corruption — different name from before (`kk_retain` →
     `kk_drop` this time), same class of bug.

2. **emitLambdaLift retain alone** (InsertDrops disabled):
   - Hellos: PASS 26/26
   - Phase 8 E2E: STILL 0/21
   - Same Def-record corruption pattern

### Conclusion across all three Path B attempts

| Variant | Hellos | Phase 6 | Phase 7 | Phase 8 E2E | Phase 9c E2E |
|---|---|---|---|---|---|
| Original Path B (8115f1e) | 26/26 | crash | PASS | 7/14 → broke | 0/21 |
| + liveness InsertDrops (b35f0d2) | 26/26 | (skipped) | PASS | 0/21 | (skipped) |
| + capture-retain (3d66cc0) | 26/26 | PASS | PASS | 0/21 | (skipped) |
| Capture-retain alone (no Drops) | 26/26 | PASS | PASS | 0/21 | (skipped) |

Each variant gets closer in some ways (no self-test crash with
capture-retain) but Phase 8 E2E never moves off 0/21.

### What this tells us

The closure-arg contract has interactions with the emitter's
StateT-monad-on-closures threading that our surgical changes
don't address.  Specifically: when stage 1's compiled emitter
runs on a real Haskell module, it threads through deep nests of
captured state-monad continuations.  Somewhere in that nesting,
the Path B semantic divergence corrupts Def records — the
`progDefs` list cells, the `Def.name` Text values, or both.

Multiple hypotheses for where it lives:

1. **Liveness over-eagerness on the StateT closure cell** — the
   StateT closure is the most heavily nested cell in the emitter.
   My liveness-driven drop sees the StateT's last EVar position
   and drops there, but the State monad's continuation chain
   means the StateT is implicitly still alive.  My Liveness module
   handles ELam captures via "used at the ELam position" but maybe
   that's not enough for nested-continuation patterns.

2. **`stripRetains` was wrong even when off** — even without
   stripping Perceus's wrapRetains, the new ELam-retain in
   emitLambdaLift creates double-retains.  We've now confirmed
   hellos pass with capture-retain alone, so this isn't the only
   issue.

3. **`wrapRetains` itself counts ELam captures as consumes** —
   Perceus's `analyzeUsage` counts an ELam-captured var as a use.
   Under Current contract that's correct (ELam construction
   consumes the capture).  Under Path B with capture-retain, the
   ELam construction is borrow — so wrapRetains over-retains.
   Need to fix analyzeUsage to not count ELam captures as
   consumes under Path B.

### Recommendation

Path B is parked.  Branch `path-b` preserves all three attempts
for reference.  Master's `Frankenstein.Core.Liveness` module is
sound and usable for other purposes (or for a future
better-scoped Path B retry where the entire contract is rebuilt
from first principles rather than incrementally inverted).

A SMALLER follow-up that might be tractable: make the C-shim
side fully match the existing Current contract by adding `kk_drop`
at the end of every shim function that takes a closure (the
mirror of what's already done for text_break_pred etc.).  Hellos
+ bootstrap should remain green, and surd-quintic / similar
heavy workloads might leak less.  That's Phase 12c step 8 *done
properly*, not a Path B retry.
