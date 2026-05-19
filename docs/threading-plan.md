# Threading Plan — port Koka's kklib concurrent runtime

**Status: design doc, no code written yet.** Captures what we'd take from
where, in what order, at what risk.  Future sessions pick up here.

## TL;DR

Frankenstein's runtime is currently single-threaded (no pthread, no
atomics, no thread-local storage).  Koka has implemented Perceus-style
concurrent RC + a pthread-based task scheduler — ~2,470 LOC of working
code already sitting in `donors/koka/kklib/`.  Plan: port that
layer-by-layer rather than rewriting from scratch.  Per the project's
stated architecture ("aggressive code reuse from donor compilers"),
this is exactly the kind of work the donor-bridge design enables.

## Current frankenstein runtime state

| File | LOC | Notes |
|---|---|---|
| `runtime/kk_runtime.c` | 1558 | All refcount RMW is non-atomic: `*rc = (*rc & ...) \| (count - 1)` |
| `runtime/kk_arena.c` | 205 | Bump-pointer allocator, no locking |
| `runtime/kk_cycle.c` | 283 | Bacon-Rajan trial-deletion cycle collector |
| `runtime/kk_runtime.h` | 183 | Public surface |

Zero thread primitives.  `grep -E "pthread|atomic|_Atomic|stdatomic|mutex|thread_local|__thread" runtime/*.c runtime/*.h` returns one false positive (a comment about `fwrite` atomicity).

## Reference implementation in donors/koka/kklib/

| File | LOC | What it gives us |
|---|---|---|
| `kklib/include/kklib/atomic.h` | 104 | All the atomic primitives we'd need (relaxed/acquire/release load/store/CAS/fence wrappers around C11/C++11 atomics) |
| `kklib/include/kklib/thread.h` | 40 | API surface: `kk_promise_t`, `kk_task_schedule(fun, ctx)`, `kk_task_set_default_concurrency`, `kk_lvar_t` (Lindsey Kuper's LVars), `kk_lvar_put/get` |
| `kklib/src/thread.c` | 597 | pthread-based task scheduler implementation, work-stealing, promise/LVar mechanics |
| `kklib/src/refcount.c` | 831 | Perceus synchronised/unsynchronised RC with sign-bit encoding (negative count = thread-shared, atomic ops; positive count = unique to current thread, non-atomic) |

Total: **2,470 lines** of battle-tested concurrent runtime we'd be porting, not writing.

## Academic grounding

Priority reading order:

1. **Perceus: Garbage Free Reference Counting with Reuse** (Reinking, Xie, de Moura, Leijen — PLDI 2021).  Section 6 covers the sign-bit-encoded synchronised/unsynchronised mechanism.  This IS the theoretical design that kklib implements.

2. **Biased Reference Counting** (Choi, Shull, Park, Torrellas — OSDI 2018).  Alternative concurrent RC scheme: one biased "fast path" thread plus atomic ops for everyone else.  Simpler than Perceus, used in CPython 3.13+.  Worth reading as a sanity check / fallback design.

3. **Project Verona** runtime papers (Microsoft Research) — concurrent ownership with isolated regions.  Verona shares contributors with Koka, and the "behavior-oriented concurrency" model is interesting for effect-handler interaction.

4. **`activation_perceus_demotion.md`** in `~/src/telix/docs/` — speculative design connecting scheduler activations (defunct on Linux since ~2003) with Perceus demotion.  The Phase-6+ direction once Linux gains the kernel mechanism (i.e. probably never; Telix's own kernel work is the more plausible host).

## Key Koka commits to study (from `~/src/koka` git history)

All by Daan Leijen, Sept-Oct 2021 — the burst of work that landed the
concurrent RC mechanism.  Read in this order:

| Commit | Date | Why it matters |
|---|---|---|
| `a517c3f8` | 2021-09-22 | "fix bug in multithread refcount" — entry point; small change that exposes the RC layout assumptions |
| `8c54b7a1` | 2021-10-13 | "make thread_shared field 8-bits; make static variables thread shared" — encoding decisions |
| `2bed9ad2` | 2021-10-18 | "fix acquire/release for reference counts and use a better scheme for the sticky range and thread shared reference counts" — **the big refactor**, 194 lines in `refcount.c` rewritten.  This is where the current scheme settled. |
| `f23b9517` | 2021-10-21 | "use atomic load/store for all accesses to the refcount field" — the universal-atomic-load/store policy that makes the rest sound |
| `dd336cb7` | 2021-10-24 | "small improvements to kklib and atomics" — polish |

**Note on Lorenzen's 2024 contribution**: The Koka v3.1.0 news entry credits Anton Lorenzen with "improved atomic refcounting".  His actual git track in 2023-2024 is on FIP/FBIP compile-time analysis (`Parc`, "drop specialization", "reuse specialisation").  Best reading: the news entry's framing means his *compile-time* analysis *reduces* the atomic ops needed at runtime, not that he changed the runtime mechanism itself.  Runtime atomic-RC mechanism = Daan Leijen 2021.  Compile-time reduction of atomic ops needed = Lorenzen 2023-2024.  Both layers matter.

## Layered porting plan

Each phase is independently shippable.  Cumulative effort estimates assume
careful work with no surprises (in practice, multiply by 1.5x).

### Phase 1 — atomic-RC primitives behind `-DKK_ATOMIC_RC`

**Goal**: kk_retain/kk_drop use atomic ops when the flag is defined, non-atomic otherwise.  Bootstrap continues passing on the non-atomic default path.

**Work**:
- Port `kklib/include/kklib/atomic.h` (104 LOC, mostly portable C11 wrappers) to `runtime/kk_atomic.h`.
- Audit `runtime/kk_runtime.c` for refcount sites (~10-15 RMW points in `kk_retain`/`kk_drop`/`kk_cycle.c`).
- Wrap each in `#ifdef KK_ATOMIC_RC` blocks that use `__atomic_fetch_add` / `__atomic_compare_exchange_n` / `kk_atomic_load_acquire` / `kk_atomic_store_release` matching Koka's choices.
- Add `-DKK_ATOMIC_RC` build variant to `self-host/build.sh`.
- Run bootstrap on both variants; confirm strict fixed point still holds.

**Effort**: 5-8h.  **Risk**: low (flag-gated).  **Performance impact** (when flag on): RC ops ~2-4x slower on x86, ~5x on ARM — but this is the price of correctness under sharing.

### Phase 2 — sign-bit-encoded sharing state

**Goal**: implement Perceus's two-state RC.  Heap blocks start unsynchronised (positive count, non-atomic ops).  When a second thread acquires a reference, the block flips to synchronised (negative count = sticky bit, atomic ops thereafter).  Once synchronised, stays synchronised (Perceus does not demote).

**Work**:
- Port `kklib/src/refcount.c`'s `kk_refcount_sticky`/`kk_refcount_dup`/`kk_refcount_drop` to our `kk_retain`/`kk_drop`.  These ~120 lines are the heart of the design.
- Reconcile with our current `KK_COLOR_MASK | KK_RC_MASK | KK_NFIELDS_MASK` layout — the sign bit needs to be available, which it currently isn't given our color encoding sits in the high byte.  Probably means widening the refcount word from i32 to i64 (we're already using i64) and reassigning bit ranges.
- Update the cycle collector (`kk_cycle.c`) to handle synchronised vs unsynchronised counts correctly — the existing trial-deletion algorithm needs an atomic-CAS-based variant for synchronised counts.

**Effort**: 8-12h.  **Risk**: medium-high (encoding change touches every refcount op; cycle collector interaction is subtle).

### Phase 3 — thread-safe arena

**Goal**: `kk_arena_alloc`/`kk_arena_free` are safe under concurrent calls.

**Work**:
- Simplest: one mutex around the bump pointer.  Lock contention will be ugly but it's correct.  ~30 LOC change.
- Better: per-thread arenas with a global reclaimable-block pool.  This is what kklib does (`kk_heap_t` is thread-local).  ~150 LOC change.
- Recommend: start with one mutex, profile, upgrade to per-thread if hot path measurement justifies it.

**Effort**: 3-5h for one-mutex variant; 8-12h for per-thread.

### Phase 4 — `kk_spawn` / `kk_join` / `kk_promise`

**Goal**: compiled code can spawn threads.  Port `kklib/src/thread.c`'s `kk_task_schedule`.

**Work**:
- `kk_promise_t` = heap-allocated `[mutex, condvar, result, is_done]` block.
- `kk_task_schedule(closure)` = enqueue closure to thread pool, return promise.
- `kk_promise_get(p)` = wait on condvar, return result.
- Thread pool init at runtime start: spawn `nproc` worker pthreads.
- Closure passed to a worker must be `retain`'d (worker holds a reference until done).

**Effort**: 4-6h.

### Phase 5 — bridge FFI

**Goal**: source-level threading primitives in at least one frontend.

**Recommendation**: Mercury first (per `activation_perceus_demotion.md` §6.4).  Mercury's mode system has uniqueness (`unique`, `mostly_unique`) and determinism (`det`/`semidet`/`multi`) annotations that map directly onto Perceus's sharing-state pre-marking — a `unique`-mode value transferred to a child thread can start unsynchronised without a runtime check.

**Work**:
- Mercury bridge: detect Mercury's parallel-conjunction `,` syntax and `thread.spawn` primitive; translate to `kk_task_schedule`.
- Haskell bridge: `forkIO :: IO () -> IO ThreadId` from `GHC.Conc` → wrap in our `kk_spawn` shim.
- Rust bridge: `std::thread::spawn` → ditto.

**Effort**: 3-5h per bridge.

### Phases 6-8 — Telix-style demotion (FUTURE, blocked)

Predicated on either Linux gaining scheduler activations (vanishingly unlikely) or running on Telix.  Adds:
- Synchronised → unsynchronised demotion when sole ownership is re-established
- Mercury mode pre-marking
- Stop-the-world coordination via kernel upcalls

See `~/src/telix/docs/activation_perceus_demotion.md`.  Out of scope for any near-term frankenstein work.

## Totals

| Scope | Effort | Sessions |
|---|---|---|
| Phase 1 alone (flag-gated atomic RC) | 5-8h | 1 |
| Phases 1+2 (atomic + sign-bit sharing) | 13-20h | 2-3 |
| Phases 1+2+3+4 (full naive shared-memory) | 20-33h | 4-6 |
| Phase 5 (one bridge wired up) | +3-5h | partial session |
| Phases 1-5 cumulative | 23-38h | 5-8 |

## Open questions / risks to think about

1. **Refcount word layout**.  Current: high byte = color, low bits = count, middle = nfields.  Perceus needs the sign bit.  Where does the sign bit go, and how does that interact with our cycle collector's color encoding?

2. **Cycle collector concurrency**.  Bacon-Rajan trial deletion fundamentally assumes a single-threaded view of the heap.  Either (a) world-stop during collection (simple, latency spikes), (b) port Koka's concurrent variant if one exists (need to check `kk_cycle.c` equivalents in kklib), or (c) per-thread collectors with cross-thread reference accounting (complex).

3. **Effect handler interaction**.  Frankenstein's effect handlers use the same heap as everything else.  Multi-shot resume capture across threads is a hard problem — see `docs/multi-shot-design.md`.  Probably needs threading and multi-shot to be wedded carefully or kept apart by construction.

4. **Bootstrap re-verification**.  Every phase needs the bootstrap to still hit `*** FIXED POINT REACHED ***` on the non-threaded default path.  Adds test cycle time.

5. **Mercury mode integration**.  The juicy part — Mercury's `unique`-mode annotations in HLDS can directly inform Perceus pre-marking decisions.  Documenting which Mercury attributes our HldsParse already extracts vs. which we'd need to add is pre-work for Phase 5.

## What's worth doing next session

**Phase 1** alone.  It's:
- Self-contained (flag-gated)
- Cheap (5-8h)
- Foundation-laying (gets atomic primitives into the codebase)
- Regression-safe (bootstrap stays on non-atomic path by default)
- Skippable if priorities change (doesn't lock us into Phases 2+)

After Phase 1 lands, decide whether Phase 2 (sign-bit sharing) is the next stop or whether Phase 3-4 (arena + spawn) make more visible progress.
