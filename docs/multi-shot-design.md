# Multi-shot Effect Handler Design

## Status

**Phase B2-medium**. Lays groundwork for B2-full (research-paper-quality
implementation with runtime stack capture, benchmark suite, formal
correctness proof).

This document captures the design decisions for native multi-shot
effect handlers in Frankenstein. The implementation lands in pieces:
classifier first, then CPS converter (pure, tested in isolation), then
evidence-pass wiring, then runtime ABI, then nondet demo, then K
verification.

## Background

Frankenstein currently supports two handler kinds:

  | Kind             | Mechanism                                  | Continuation calls |
  |------------------|--------------------------------------------|--------------------|
  | **Abort**        | `setjmp`/`longjmp` via `kk_handler_exec` / `kk_handler_abort` | 0 |
  | **Tail-resumptive** | Evidence-pass inlining: `perform op args` rewrites to `ev(args)` | 1 (in tail position) |

Both are stable; `mercury_choose` / `mercury_collect_choices` provides
*specialized* binary multi-shot via iterative path enumeration, but
only for the single `Choose() Bool` operation.

The gap: a *general* handler API where the body computes a value, the
handler receives `(args..., resume)`, and `resume` is a first-class
function value the handler may invoke any number of times with any
value. Examples: nondeterminism (resume twice — once with True, once
with False; collect results), backtracking, probabilistic programming,
generators, async/await desugarings.

## Source-level surface

Multi-shot handlers reuse the existing `EHandle`/`EPerform` syntax —
no new constructors. The handler shape determines the kind:

```haskell
-- Abort handler: takes (args..., resume), never references resume
\v resume -> error "boom"

-- Tail-resumptive: takes (args..., resume), references resume once in
-- tail position
\v resume -> resume (process v)

-- Multi-shot: takes (args..., resume), references resume more than once
-- OR in non-tail position (i.e., uses the result of resume)
\v resume -> resume True ++ resume False
```

A static classifier (`classifyHandler :: Expr -> HandlerKind`)
distinguishes the three at evidence-pass time.

## Handler ABI

For multi-shot, the handler signature is:

```
handler :: arg_1 → arg_2 → … → arg_N → (a → r) → r
```

  - `arg_i`: the operation's perform arguments
  - `(a → r)`: the continuation. `a` is the result type the body
    expected from the perform site; `r` is the result type of the
    whole handle.
  - Returns `r`.

The handler invokes the continuation by simple function application:
`resume(v)`. Each invocation runs the body forward from the perform
site with `v` as the result, all the way through to the body's
return — which itself is wrapped in a final continuation that
returns `a → r` (typically `id` for the outermost handle, or some
wrapper like `\v -> [v]` for nondeterminism).

## Implementation strategy: CPS conversion

The standard route. Convert the body to continuation-passing style at
evidence-pass time. Each `EPerform op args` site becomes a call to the
handler with the remaining computation as an explicit closure argument.

### CPS algorithm sketch

```haskell
cps :: Expr → (Expr → Expr) → Expr

cps (ELit n)         k = k (ELit n)
cps (EVar v)         k = k (EVar v)
cps (ELit n)         k = k (ELit n)

cps (EApp f args)    k = cps f      (\fv  →
                         cpsList args (\avs →
                         k (EApp fv avs)))

cps (ELet bgs body)  k = transformBinds bgs (cps body k)

cps (ECase scrut bs) k = cps scrut (\sv →
                         ECase sv (map (cpsBranch k) bs))

cps (EPerform op as) k = cpsList as (\avs →
                         let v = freshName "v"
                             rest = k (EVar v)
                             contLam = ELam [(v, anyTy)] rest
                         in  EApp (handlerExprFor op) (avs ++ [contLam]))

-- Other constructors (ETypeApp/ETypeLam/EDelay/EForce/...) pass
-- through with k applied to the recursed body.
```

The transformation is well-known; the trickiest parts are:

  - **Free variables**: each `\v -> k (EVar v)` closure captures the
    free variables of the rest of the computation. These are
    well-defined and tractable; the existing lambda-lifting machinery
    in `MlirEmit.Emitter` handles closure allocation.
  - **Multiple resumes**: each call to the continuation closure starts
    a fresh evaluation of the body suffix with a different `v`. The
    captured environment is reused (shared) across resumes — this is
    semantically correct because the body suffix is pure with respect
    to its free variables (no `IORef` mutation, etc., or those would
    flow through evidence).

### Where CPS lives

`Frankenstein.Core.CpsConvert` — a new pure module. Pure transformation,
testable in isolation. The evidence pass calls into it only for `Multi`
handlers; `Abort`/`Tail` paths are unchanged.

## Evidence-pass wiring

```haskell
case classifyHandler handler of
  Abort -> existing setjmp/longjmp path
  Tail  -> existing inlining path
  Multi -> do
    let handlerName = freshHandlerName
        cpsBody = cps body (\v → EApp (EVar finalKName) [v])
        finalK = ELam [(vName, anyTy)] (EVar vName)  -- identity wrapper for outer handle
    -- emit: let handler' = handler in let finalK = \v -> v in cpsBody
    -- where cpsBody has all EPerform replaced by handler' calls
```

## Runtime ABI

Three operations, all expressible with existing primitives:

  1. **Apply a continuation closure**: standard `kk_alloc_con` /
     closure-indirect call. No new runtime primitive.
  2. **Allocate the continuation**: standard `kk_alloc_con` with the
     captured free variables as fields. The CPS converter emits the
     allocation as plain `ELam` — the lambda-lifting pass in the
     emitter handles closure construction.
  3. **Return from the handle**: the final continuation is just a
     lambda value; the handler invokes it and the result bubbles up
     normally.

No `setjmp`/`longjmp` needed for multi-shot. No new C runtime code.
This is the key payoff of CPS over stack-capture.

## K verification claim

In `k-specs/multishot-claims.k`:

  > **Claim**: For a multi-shot handler `H` and body `B`, if the
  > handler invokes the continuation `N` times with values
  > `v_1, …, v_N`, then the CPS-transformed expression evaluates the
  > body once per invocation with `v_i` substituted at the perform
  > site, and the handler's final result is the value the handler
  > computes from those `N` body results.

This reduces to: `cps_apply(handler, args, k_body)` where `k_body`
invokes the body's suffix N times. Existing `kontVal` machinery in
`organ-ir.k` already models this (lines 837-840). The new claim ties
the CPS-converted AST to the operational semantics.

## Out of scope (for B2-medium)

These belong to B2-full:

  - Real delimited-continuation runtime primitive (`kk_cont_capture`
    / `kk_cont_invoke`) with stack copying. CPS is sufficient for
    correctness; the runtime primitive is a performance optimization.
  - Tail-call optimization for chained resumes.
  - Performance benchmark vs Koka, vs hand-CPS, vs Mercury choice.
  - Integration with effect optimization passes (handler inlining,
    identity-handler elimination).
  - Cross-module multi-shot handlers (currently each handler is
    monomorphic).

## Phase plan

| Step | Deliverable | Scope |
|------|-------------|-------|
| B2a-design | This document | ~1 hr |
| B2a-classify | `HandlerKind` + `classifyHandler` | ~30 min |
| B2b-dispatch | Evidence pass routes Multi to stub | ~30 min |
| B2b-cps | `CpsConvert` pure module | ~1.5 hr |
| B2b-tests | Cabal tests for CPS converter | ~30 min |
| B2-remaining | Document next steps for B2-full | ~30 min |
| **Future** | Wire CPS output into evidence pass; nondet demo | next session |
| **Future** | K verification claim | next session |
| **Future** | EffectOpt integration; B2-full benchmark suite | research |

## What landed in B2-medium + B2-full

  - `docs/multi-shot-design.md` — this document.
  - `Frankenstein.Core.EffectOpt.HandlerKind` + `classifyHandler` —
    distinguishes `HKAbort` / `HKTail` / `HKMulti` from how the handler's
    last parameter (the resume) is used.
  - `Frankenstein.Core.CpsConvert` — pure CPS converter. Handles
    `EVar`/`ELit`/`ECon`/`EFunRef`/`EApp`/`ELam`/`ELet`/`ECase`/
    `EPerform`/`ETypeLam`/`ETypeApp`/`EHandle`/`ERetain`/`ERelease`/
    `EDrop`/`EReuse`/`EDelay`/`EForce`. Plotkin-correct let-fusion at
    `EPerform` sites: `cps[let x = M in N] k = cps[M] (\v -> let x = v
    in cps[N] k)`.
  - 12 cabal unit tests covering both classifier and CPS converter.
  - `Frankenstein.Core.Evidence.evidenceExpr` — Multi-shot dispatch
    branch with sentinel substitution: replaces `EFunRef qn` in the
    CPS output with `EVar evName` (handler binding) based on
    `scopeOps`.
  - `Frankenstein.Core.EffectOpt.inlineLocalHandler` — guard against
    `HKMulti`, preserving the multi-shot semantics until the evidence
    pass.
  - `examples/effect_nondet.json` — nondeterminism demo. Handler
    resumes twice (with 1 and 0), body returns 10 or 20, handler
    sums them: **end-to-end native output is 30**.
  - `k-specs/multishot-claims.k` — 9 kprove claims (all `#Top`)
    covering classifier on canonical handler shapes, countAppsOf
    composition, and structural invariants.
  - Bootstrap fixed point unchanged: **24/24 + 21/21** at all stages.

## What's left for B2-full

### Resolving the handler reference inside CPS output

`CpsConvert.cpsExpr (EPerform qn args)` currently emits
`EApp (EFunRef qn) (avs ++ [contLam])` as the handler call shape. The
`EFunRef qn` is a *sentinel* — the actual handler binding is `evName`
in the surrounding `ELet`, not a top-level function `qn`. Two routes
to wire this:

  1. **Sentinel substitution** after CPS: walk the CPS output, replace
     `EFunRef qn` with `EVar evName` for the effect being handled. The
     evidence pass already maintains the mapping `effName -> evName` in
     `EvidenceScope`.

  2. **Parameterise CPS by handler env**: pass the `EvidenceScope` into
     `cpsExpr` and emit `EVar evName` directly. Cleaner but couples
     `CpsConvert` to `Evidence`.

Route (1) keeps `CpsConvert` independent (testable, K-modellable) and
is the recommended approach.

### Nondeterminism demo (B2c)

  1. Author `examples/effect_nondet.json` mirroring `effect_ask.json`
     shape: a `choose() Bool` operation, a handler `\() resume -> resume True ++ resume False`,
     a body `let b = perform Choose in [if b then 1 else 0]`.
  2. Run through host pipeline to MLIR → native binary.
  3. Expected stdout: a list/sum representing both outcomes.
  4. Add to `self-host/build.sh` E2E suite alongside `effect_ask` and
     `effect_state`.

### K verification (B2d)

  1. Author `k-specs/multishot-claims.k` with at minimum:
     - **Claim MS1**: For `EHandle Eff H B` with `H` multi-shot and
       `B` containing exactly one `EPerform`, the CPS-converted body
       evaluates the body suffix once per `resume` call.
     - **Claim MS2**: `classifyHandler H = HKMulti` iff resume is
       called more than once or in non-tail position.
     - **Claim MS3**: CPS conversion preserves observable behaviour
       for effect-free expressions (`cpsTopLevel e ≡ e` modulo
       administrative redexes).
  2. Add to `k-specs/all-claims-def.k`.
  3. Bring `kprove` over the new module.

### EffectOpt integration

  - Multi-shot handlers must NOT be inlined by `inlineLocalHandlers`
    (the CPS-rewritten body assumes the handler closure is stable).
    Add an `HKMulti` skip in the inliner.
  - `eliminateIdentityHandlers` is safe for multi-shot trivially —
    an identity handler discards the continuation and just returns
    the value, semantically zero-shot.
  - `annotateTailResumptive` already handles its own kind; the only
    interaction is to skip if `classifyHandler = HKMulti`.

### Runtime stack-capture alternative

CPS is correct but allocates an `ELam` closure per `EPerform`. A real
delimited-continuation runtime (Boehm-style `cont_create` / `cont_invoke`)
would avoid the closure traffic. Out of scope for B2-medium; revisit
once benchmarks show CPS overhead matters.

### Benchmarks

  - Compare against Koka's multi-prompt continuations on identical
    nondet and state-+-nondet workloads.
  - Profile closure allocations per second.

## References

  - K spec lines 837-840 in `organ-ir.k`: `kontVal` apply rule.
  - `SchemeBridge.CoreTranslate`: CPS conversion for call/cc, prior
    art in this codebase.
  - Koka: "Effect Handlers in Scope" (Wu, Schrijvers, Hinze 2014),
    "Generalized Evidence Passing for Effect Handlers" (Xie & Leijen
    2021). Frankenstein's evidence-pass shape follows the latter.
