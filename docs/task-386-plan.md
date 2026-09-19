# Task #386 — Plotkin self-compiler hang on recursive ADT processing

## Status: in_progress (deeper iteration)

## Root cause (confirmed by instrumented `replicate$2` shim)

Under `FRANKENSTEIN_EVIDENCE=plotkin`, the self-compiled stage-2 binary
hangs when compiling any program that recurses on an ADT. The hang is
in `GHC_Internal_List_replicate$2`, which receives `cnt` as a CLOS
cell (tag 0x434C4F53) instead of an `Int`. The cell's address (≈ 10^14)
wins the integer comparison in `max 1 (countUses n body)`, poisoning
the `allCounts` list inside `emitLambdaLift`'s `mkPrologue`.

The CLOS cell is a PAP wrapper for a function defined with arity 1
(a curried function returning another function) being **called with 2
args** through the bridge's generic closure-indirect dispatch path.
That path emits ONE `llvm.call` passing all args, expecting the
wrapper underneath to dispatch correctly. For a curry chain
`Name -> (Expr -> Int)`, the underlying function returns the inner
closure, and that inner closure IS what becomes the bogus "Int."

The bridge already detects this in `Core/Linker.hs:835` as
"arity mismatch: called with 2 args, but defined with 1" — emitted
as warnings for `Frankenstein.Core.ConsumingUses.consumingUseCount`,
`Frankenstein.Core.CpsConvert.unCps`, and
`Frankenstein.Core.EffectOpt.countAppsOf`. The warnings name the
broken call sites; nothing acts on them.

`emitAppVarGeneral` in `MlirEmit/Emitter.hs` already has a correct
oversaturated-call multi-step dispatch path — but only for **direct**
calls to known top-level functions. For let-bound function values
like
```haskell
let countUses = if useNewPerceusCount
                then consumingUseCount
                else legacyCountUses
in [ max 1 (countUses n body) | n <- captured ]
```
the EApp closure-indirect path takes over, and that path has no
multi-step logic.

## Failed attempts (don't repeat)

1. **Rewrite `legacyCountUses tgt = go` with `go` taking `tgtT`
   explicitly.** No effect — same 9/9 Plotkin Phase 9c with identical
   gdb backtrace. The arity-1-returning-function pattern lives in
   multiple places (consumingUseCount, unCps, countAppsOf) and the
   PAP-as-Int trap reaches them all.

2. **Also explicit `expr` param + `concatMap (\a -> [go tgtT a]) as`
   instead of `map (go tgtT) as`.** 0/18 regression — broke shared
   code paths used by all tests including previously-passing
   closure/maybesum/effect_*.

3. **Eliminate the `if useNewPerceusCount then ... else ...`
   conditional entirely (hardcode `countUses = legacyCountUses`).**
   Phase 6 self-test broke on basic record selectors. Phase 9c 0/18.
   Removing the conditional somehow changes how stage 1 compiles
   itself; consumingUseCount references aren't quite as inert as
   expected.

## Real fix

In `src/Frankenstein/MlirEmit/Emitter.hs`, around the
closure-indirect EApp path (`emitExpr (EApp fn args)` ~ line 2314):

1. Build/thread an arity map (already computed in
   `Core/Linker.hs:detectArityMismatches`'s `arityMap`). Put it on
   `EmitState`.
2. When emitting a closure-indirect EApp, peek at the callee's
   underlying function name (if known — let-bound to a top-level
   function reference, conditional over top-level function refs,
   etc.). If the underlying function's arity is less than the number
   of args being passed, emit multi-step dispatch:
   - First indirect call with `defArity` args → produces inner closure
   - Then closure-indirect dispatch with remaining args
3. The direct-call oversaturated path in `emitAppVarGeneral`
   (line ~3279) already implements this pattern; factor it out so
   both paths share it.

## Validation

Build a minimal host-compiled reproducer in a tmpfile:
```haskell
module Repro where
g x y = x + y :: Int
h x y = x * y :: Int

-- Curried 1-arg helper returning a function, called with 2 args
-- through a let-bound conditional that hides the choice.
countUses :: Int -> Int -> Int
countUses = if True then g else h

main :: Int
main = sum [ max 1 (countUses n 5) | n <- [1, 2, 3] ]
```

Compile under `--evidence=plotkin --compile`, run, check the result.
Expected: `(max 1 (1+5)) + (max 1 (2+5)) + (max 1 (3+5)) = 6+7+8 = 21`.
If the fix is correct the result is 21; if not, it's a giant number
(the PAP-cell addresses winning the max comparison).

Once the host reproducer passes, run the full plotkin self-host
rebuild (`FRANKENSTEIN_EVIDENCE=plotkin bash self-host/build.sh`) and
verify Phase 9c E2E ≥ 16/18 (matches Phase 8 baseline for the host
binary).

## Don't lose

- All three local-rewrite attempts FAILED. Don't try them again.
- `consumingUseCount`, `unCps`, `countAppsOf` are the canonical
  arity-1-returning-function offenders surfaced by the linker
  warnings. There are likely more.
- Phase 9c baseline (no fix attempts) is 9/9 stage 2 + 9/9 stage 3.
  Effects always pass. Recursive-ADT tests always fail.
- Files should be clean (`git status`) before kicking off any
  Plotkin rebuild — they take 2–3 hours.
