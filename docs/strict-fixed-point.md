# Strict s2≡s3 Fixed-Point — investigation arc

**Goal**: drive Phase 10's strict MLIR byte-equality check from the current 9-11/26 up to 26/26.

**Status**: ✓ ACHIEVED 2026-05-19. Bootstrap reports `*** FIXED POINT REACHED ***` — Stage 2 vs Stage 3 byte-equal on 26/26 modules, 21/21 E2E PASS at both stages.

## Resolution summary

The arc closed in two moves:

1. **Clean rebuild fixed driver.o staleness** (9-11/26 → 19/26). The on-disk `self-host/obj/driver.o` had been linked from an older `driver.c` that didn't call the evidence pass; stage 1 and stage 2 binaries silently skipped plotkin while stage 3 (linked at a different time) applied it. `bash self-host/build.sh` rebuilds `driver.o` unconditionally and brought all three stages onto the same driver. See `memory/bootstrap_state_drift.md` for the diagnostic recipe.

2. **Four PostProcess.hs bugs fixed** (19/26 → 26/26, commit `9cd995f`). The remaining 7 stage-3 emit failures were all `--postprocess-mlir` producing invalid rewrites:
   - **ReExtractFix scope violation** — accepted aliases anywhere in the function, including ones whose `kk_field` producer's enclosing `scf.if` had already closed. Fix: walk brace balance char-by-char from producer to call site, try each candidate alias.
   - **hasDecl/hasWrapper too permissive** — `T.isInfixOf` on `@<name>` matched comments and use sites, suppressing the wrapper injection. Fix: require the `func.func ... @<name>` definition syntax.
   - **findPapWrapper param counter** — filtered `(== "i64")` on segments like `"%clos: i64"`, always returning 0. Fix: match `": i64" `T.isSuffixOf`` per segment.
   - **ExternPapFix arity off-by-one** — wrapper has `arity + 1` i64 params (closure + args) but mkPapBlock was called with `arity`. Fix: pass `arity + 1`.

The QC differential test driver (`tools/diff-tester/`, commit `7159234`) was the major enabling lever: it characterized the divergences universally on small generated programs (clean-rebuild experiments showed 100/100 stage1≡stage2 and 100/100 stage2≡stage3 well before the bootstrap-scale fixes landed), which proved the remaining 7 failures were purely emit-pipeline mlir-opt rejection, not strict-equality compiler bugs.

## Underlying note

These fixes papered over the deeper bug ("Self-host runtime: pattern-match dispatch on ADT constructors is systematically wrong" — see Phase 9 Outstanding Issues in ROADMAP.md) by making PostProcess robust to its symptoms. The pattern-compiler root cause is still latent; the host-compiled `frankenstein` binary (which runs PostProcess) is architecturally immune.

---

# Historical record (pre-resolution)

## What we know

### Three identified divergence classes

1. **classifyBranches mis-classifies single-PatCon cases**
   - Minimal repro: `examples/single_ctor_test.hs` (built inline during DB5 investigation)
   - Source: `case p of Pair _ y -> y`
   - Host emits `SingleConCase` path (unconditional field extraction)
   - Self-host emits `ConCase`-with-default path (tag check + scf.if + dead else returning 0)
   - The naive fix (rewrite `<-` pattern guards as Bool guards) did NOT solve it — same buggy output.

2. **Lambda-lift free-var sets differ**
   - Minimal repro: `self-host/obj/stage{2,3}/Core_Evidence.mlir` — diff is 34839 lines, dominated by SSA-numbering cascading offset that starts at one lambda whose stage-2 emit captures 3 vars but stage-3 emit captures 2.
   - Cause: `emitLambdaLift`'s free-var analysis uses `esAliases` snapshot at lift time; tiny upstream variance in alias-map content cascades.

3. **Split-merged module mlir-opt failures**
   - Modules: `GhcBridge_Driver`, `MercuryBridge_CoreTranslate`, `RustBridge_CoreTranslate`, `MlirEmit_Emitter`, `KokaBridge_Driver`, `OrganIR_Consumer`
   - These get split into parts via build.sh's Python splitter (>1MB), compiled separately, merged via `merge-mlir-parts.py`.
   - After merge, stage-3 mlir-opt rejects with "expected operation name in quotes" or similar — the merged file has malformed structure.
   - Falls back to prev-stage .o → both stages have the SAME .o (copied from stage 1), but the MLIR doesn't byte-match because of cross-part SSA renaming.

### What we tried (and what didn't work)

| Attempt | Round | Result |
|---|---|---|
| Extend `normalizePatterns` to length≥1 | bt18 | MLIR ballooned 6MB→22MB (every record selector got dead scf.if); reverted |
| Rewrite `classifyBranches` SingleConCase arm as Bool guards | bt19-21 | Bug persists; reverted |

## Hypotheses to test

### H1 — `case`-of-pattern miscompilation broader than guards

The classifyBranches refactor used `case` expressions in where-bound helpers (`noDefault`, `headBranchIsPatCon`). Those are themselves pattern matches. If the self-host pattern-compiler miscompiles `case ... of Cons -> ...; Nothing -> ...`, the refactor doesn't escape the bug class.

**Test**: write a tiny `.hs` that exercises ONLY `case` (no `|`-guards, no where-helpers) over a known-buggy pattern shape. Compile via host and via stage 1; compare.

### H2 — Upstream pass inserts a wildcard branch

If `flattenPatterns` / `effectOptimize` / `evidencePassEvv` / `insertPerceus` add a wildcard branch to single-PatCon cases under self-host, `classifyBranches` sees 2 branches → falls to `ConCase`. The `normalizePatterns` pass is the obvious suspect (it does this for >=2 branches; maybe a self-host variant is incorrectly firing for 1 branch).

**Test**: instrument the driver pipeline to dump the branch count at each pass boundary. The DUMP_AST env var output was unreadable; needs real text-based pretty printing.

### H3 — `length` or `null` miscompile

The `where`-bound helpers used `length branches == 1` and `not (null conBranches)`. If `length`/`null` are buggy for short lists (e.g., return wrong count), Bool guards never trigger.

**Test**: micro-benchmark `length [x]` and `null []` under host vs stage 1.

### H4 — Lambda lift's `esAliases` accumulator non-determinism

`emitLambdaLift` reads `esAliases` and uses its content to compute free vars. If the alias map's iteration order depends on a hash with stage-dependent randomness, different stages produce different free-var sets.

**Test**: dump `esAliases` keyset before each lambda lift in both host and self-host; diff.

### H5 — Split-merge SSA renaming non-determinism

The Python `merge-mlir-parts.py` rewrites SSA names with a `_pI_` prefix. If the iteration order over part files is OS-dependent (`os.listdir` is not sorted), stages produce different prefix orderings.

**Test**: `grep "pap_p[0-9]_" self-host/obj/stage{2,3}/<module>.mlir` and check whether the partN labels match.

## Proposed approach for next session

1. **Build a deterministic guard-shape repro corpus**. Create `examples/guard_repros/` with one .hs per pattern class:
   - Single PatCon (record selector shape)
   - Multi PatCon exhaustive
   - PatCon with PatVar default
   - Chained `<-` pattern guards
   - Bool guards over where-bound helpers
   - Single-PatLit branch
   
   For each, compile via host (control) and via stage 1 (test). Diff the MLIR. Identify the smallest input that exhibits divergence.

2. **Bisect by upstream-pass elimination**. Skip each pass (flatten/effectOpt/plotkin/perceus) one at a time and check if the divergence still appears. That isolates which pass introduces the divergent input to `classifyBranches`.

3. **Read the `Frankenstein.Core.FlattenPatterns` source carefully**. It's likely doing the most aggressive AST restructuring; the bug may be there.

4. **Consider switching to a deterministic pattern-tree representation** that doesn't depend on per-pass accumulator state.

## Acceptance criteria for this arc

- 26/26 strict MLIR byte-equality between stages 2 and 3 under FRANKENSTEIN_EVIDENCE=plotkin
- Inline-mode bootstrap still 24/24+21/21+fixed-point
- 21/21 E2E parity preserved (Phase 8, 9c, 10c)

## Non-goals

- Don't chase Phase 9 MLIR comparison (stage 1 binary vs host CLI). That's a different question — host CLI uses GHC's compile, self-host uses Frankenstein's. Aligning them is foundational compiler-correctness work outside this arc's scope.
- Don't try to "fix" `normalizePatterns` to mask the bug. Round bt18 proved that path doesn't scale.
