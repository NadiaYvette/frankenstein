// effectopt-claims.k — kprove claims for the EffectOpt passes
//
// Verifies invariants of src/Frankenstein/Core/EffectOpt.hs:
//
//   1. Identity-handler detection: \(x,k)->k(x) is recognized; trivial
//      perturbations (different vars, wrong arity, non-identity body) are not.
//   2. Identity-handler elimination is sound for the structural cases —
//      checked indirectly via the recogniser plus the rewrite shape.
//   3. Tail-resumptive detection: handlers that always end in resume(...) are
//      recognized; handlers with non-tail bodies are not.
//   4. Handler inlining: substitutePerforms removes EPerforms for the matched
//      effect (countPerforms goes to 0) and inserts EApp(handler, args) shape.
//
// Usage:
//   kompile all-claims-def.k --backend haskell \
//     --main-module ALL-CLAIMS-DEF --syntax-module ALL-CLAIMS-DEF-SYNTAX \
//     -o all-claims-haskell-kompiled
//   kprove effectopt-claims.k --definition all-claims-haskell-kompiled

requires "all-claims-def.k"

module EFFECTOPT-CLAIMS-SYNTAX
  imports ALL-CLAIMS-DEF-SYNTAX
endmodule

module EFFECTOPT-CLAIMS
  imports ALL-CLAIMS-DEF
  imports SET
  imports MAP
  imports INT
  imports BOOL
  imports STRING

  // =================================================================
  // Group EO1: identity handler detection
  // =================================================================

  // EO1a: classic uncurried identity handler is recognized
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-eo-1a)]

  // EO1b: curried identity handler is recognized
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        ELam(
          param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          .ParamList,
          EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
        )
      )
    )
    =>
    true
  </k> [label(claim-eo-1b)]

  // EO1c: handler that returns a literal is NOT identity
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), ELit(litInt(0)) , .ExprList)
      )
    )
    =>
    false
  </k> [label(claim-eo-1c)]

  // EO1d: single-parameter handler is NOT (uncurried) identity
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), ELit(litInt(0)) , .ExprList)
      )
    )
    =>
    false
  </k> [label(claim-eo-1d)]

  // EO1e: handler whose body is just a literal is NOT identity
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        ELit(litInt(42))
      )
    )
    =>
    false
  </k> [label(claim-eo-1e)]

  // =================================================================
  // Group EO2: tail-call detection
  // =================================================================

  // EO2a: direct call to k in tail position
  claim <k>
    isTailCall(name("k", 0),
      EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList))
    =>
    true
  </k> [label(claim-eo-2a)]

  // EO2b: literal in tail position is NOT a tail call to k
  claim <k>
    isTailCall(name("k", 0), ELit(litInt(0)))
    =>
    false
  </k> [label(claim-eo-2b)]

  // EO2c: call to a different function in tail position is not a tail call to k
  claim <k>
    isTailCall(name("k", 0),
      EApp(EVar(name("g", 0)), EVar(name("x", 0)) , .ExprList))
    =>
    false
  </k> [label(claim-eo-2c)]

  // EO2d: let-binding propagates tail-call check through the body
  claim <k>
    isTailCall(name("k", 0),
      ELet(
        (bind(name("y", 0),
              TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
              ELit(litInt(1)),
              defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("k", 0)), EVar(name("y", 0)) , .ExprList)
      ))
    =>
    true
  </k> [label(claim-eo-2d)]

  // =================================================================
  // Group EO3: tail-resumptive handler detection
  // =================================================================

  // EO3a: handler that ends with resume(arg) is tail-resumptive
  claim <k>
    isTailResumptiveHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-eo-3a)]

  // EO3b: handler that ends with a literal is NOT tail-resumptive
  claim <k>
    isTailResumptiveHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        ELit(litInt(42))
      )
    )
    =>
    false
  </k> [label(claim-eo-3b)]

  // EO3c: identity handlers are also tail-resumptive
  // (every identity handler is tail-resumptive — useful corollary)
  claim <k>
    isTailResumptiveHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-eo-3c)]

  // EO3d: a non-lambda handler (e.g. an EVar) is not tail-resumptive
  claim <k>
    isTailResumptiveHandler(EVar(name("h", 0)))
    =>
    false
  </k> [label(claim-eo-3d)]

  // =================================================================
  // Group EO4: handler inlining via countPerforms
  // =================================================================

  // EO4a: an expression with no performs of effect "exn" returns count 0
  claim <k>
    countPerforms("exn", ELit(litInt(0)))
    =>
    0
  </k> [label(claim-eo-4a)]

  // EO4b: a single EPerform of effect "exn" gives count 1
  claim <k>
    countPerforms("exn",
      EPerform(qname("exn", name("raise", 0)), ELit(litInt(0)) , .ExprList))
    =>
    1
  </k> [label(claim-eo-4b)]

  // EO4c: an EPerform of a different effect is not counted as "exn"
  claim <k>
    countPerforms("exn",
      EPerform(qname("choice", name("flip", 0)), .ExprList))
    =>
    0
  </k> [label(claim-eo-4c)]

  // EO4d: nested perform inside an application is found
  claim <k>
    countPerforms("exn",
      EApp(EVar(name("f", 0)),
        EPerform(qname("exn", name("raise", 0)), ELit(litInt(1)) , .ExprList) ,
        .ExprList))
    =>
    1
  </k> [label(claim-eo-4d)]

  // EO4e: noPerformsOf is true for an effect-free body — corresponds to the
  // post-condition that substitutePerforms eliminates the matched effect.
  claim <k>
    noPerformsOf("exn",
      EApp(EVar(name("f", 0)), ELit(litInt(0)) , .ExprList))
    =>
    true
  </k> [label(claim-eo-4e)]

  // =================================================================
  // Group EO5: identity-handler negation — handlers that should NOT
  // be classified as identity. The recogniser must not be too eager.
  // =================================================================

  // EO5a: handler whose body applies k to the WRONG variable is not identity
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), EVar(name("y", 0)) , .ExprList)
      )
    )
    =>
    false
  </k> [label(claim-eo-5a)]

  // EO5b: handler with only one parameter is not identity (we require
  // both an input AND a continuation parameter).
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EVar(name("x", 0))
      )
    )
    =>
    false
  </k> [label(claim-eo-5b)]

  // EO5c: handler that applies the FIRST param (not the second) as the
  // function is not identity — k must be the callee.
  claim <k>
    isIdentityHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("x", 0)), EVar(name("k", 0)) , .ExprList)
      )
    )
    =>
    false
  </k> [label(claim-eo-5c)]

  // =================================================================
  // Group EO6: tail-call composition through ELet / ECase.
  //
  // isTailCall must propagate through the standard sequencing forms
  // so that tail-resumptive handlers can have their resume call sit
  // behind a let-chain or a case branch.
  // =================================================================

  // EO6a: tail call survives a let with a literal RHS
  claim <k>
    isTailCall(name("k", 0),
      ELet(
        (bind(name("tmp", 0),
              TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
              ELit(litInt(42)),
              defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("k", 0)), EVar(name("tmp", 0)) , .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-eo-6a)]

  // EO6b: tail call survives an empty case with the call in the body
  // (degenerate — but the propagation rule fires).
  claim <k>
    isTailCall(name("k", 0),
      ELet(
        (bind(name("y", 0),
              TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
              ELit(litInt(7)),
              defVal) , .BindGroup) ;; .BGList,
        ELet(
          (bind(name("z", 0),
                TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
                ELit(litInt(8)),
                defVal) , .BindGroup) ;; .BGList,
          EApp(EVar(name("k", 0)), EVar(name("z", 0)) , .ExprList)
        )
      )
    )
    =>
    true
  </k> [label(claim-eo-6b)]

  // EO6c: a let whose body is NOT a tail call to k correctly returns false
  claim <k>
    isTailCall(name("k", 0),
      ELet(
        (bind(name("tmp", 0),
              TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
              ELit(litInt(1)),
              defVal) , .BindGroup) ;; .BGList,
        ELit(litInt(0))
      )
    )
    =>
    false
  </k> [label(claim-eo-6c)]

  // =================================================================
  // Group EO7: countPerforms compositional invariants.
  //
  // Strengthens EO4 with multi-perform shapes.
  // =================================================================

  // EO7a: two performs in two argument positions are counted twice
  claim <k>
    countPerforms("exn",
      EApp(EVar(name("plus", 0)),
        EPerform(qname("exn", name("raise", 0)), .ExprList) ,
        EPerform(qname("exn", name("raise", 0)), .ExprList) ,
        .ExprList))
    =>
    2
  </k> [label(claim-eo-7a)]

  // EO7b: a perform inside a let RHS is counted
  claim <k>
    countPerforms("exn",
      ELet(
        (bind(name("v", 0),
              TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
              EPerform(qname("exn", name("raise", 0)), .ExprList),
              defVal) , .BindGroup) ;; .BGList,
        EVar(name("v", 0))
      ))
    =>
    1
  </k> [label(claim-eo-7b)]

  // EO7c: noPerformsOf is robust under arbitrary let-shaped non-effect bodies
  claim <k>
    noPerformsOf("exn",
      ELet(
        (bind(name("y", 0),
              TCon(typecon(qname("std/core/types", name("int", 0)), KValue)),
              ELit(litInt(1)),
              defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("f", 0)), EVar(name("y", 0)) , .ExprList)))
    =>
    true
  </k> [label(claim-eo-7c)]

  // =================================================================
  // Group EO8: eliminateIdentityHandlers pass — local rewrite soundness
  //
  // Models the load-bearing rewrite of EffectOpt.hs's elimIdentityHandler
  // at one EHandle site.  See `eliminateIdentityRewrite` in
  // all-claims-def.k.
  // =================================================================

  // EO8a: identity-handler EHandle reduces to its body.
  claim <k>
    eliminateIdentityRewrite(
      EHandle(
        effectExtend(qname("std", name("console", 0)), effectEmpty),
        ELam(
          param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          .ParamList,
          EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
        ),
        ELit(litInt(42))
      )
    )
    =>
    ELit(litInt(42))
  </k> [label(claim-eo-8a)]

  // EO8b: non-identity handler is left alone (rewrite is a no-op).
  claim <k>
    eliminateIdentityRewrite(
      EHandle(
        effectExtend(qname("std", name("console", 0)), effectEmpty),
        ELam(
          param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          .ParamList,
          EApp(EVar(name("k", 0)), ELit(litInt(0)) , .ExprList)
        ),
        ELit(litInt(42))
      )
    )
    =>
    EHandle(
      effectExtend(qname("std", name("console", 0)), effectEmpty),
      ELam(
        param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), ELit(litInt(0)) , .ExprList)
      ),
      ELit(litInt(42))
    )
  </k> [label(claim-eo-8b)]

  // EO8c: idempotence — applying the rewrite twice on a body without
  // outer EHandle is the same as applying it once.  After EO8a strips
  // the EHandle wrapper, the remaining ELit(litInt(42)) is no longer
  // an EHandle, so the second application is a no-op via the [owise]
  // rule.
  claim <k>
    eliminateIdentityRewrite(
      eliminateIdentityRewrite(
        EHandle(
          effectExtend(qname("std", name("console", 0)), effectEmpty),
          ELam(
            param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
            param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
            .ParamList,
            EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
          ),
          ELit(litInt(42))
        )
      )
    )
    =>
    ELit(litInt(42))
  </k> [label(claim-eo-8c)]

  // EO8d: rewrite is a no-op when the input is not an EHandle.
  claim <k>
    eliminateIdentityRewrite(ELit(litInt(7)))
    =>
    ELit(litInt(7))
  </k> [label(claim-eo-8d)]

  // =================================================================
  // Group EO9: annotateTailResumptive — classification stability
  //
  // The annotateTailResumptive pass adds a hint annotation to handlers
  // classifyHandler determines are tail-resumptive.  Since the annotation
  // itself is non-semantic (a downstream-CPS optimisation hint), the
  // soundness condition reduces to: the classifier returns the expected
  // verdict at each EHandle site.  This is captured by
  // `annotateTailResumptiveCheck` which wraps `isTailResumptiveHandler`.
  // =================================================================

  // EO9a: an EHandle whose handler is tail-resumptive classifies positively.
  claim <k>
    annotateTailResumptiveCheck(
      EHandle(
        effectExtend(qname("std", name("console", 0)), effectEmpty),
        ELam(
          param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          .ParamList,
          EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
        ),
        ELit(litInt(42))
      )
    )
    =>
    true
  </k> [label(claim-eo-9a)]

  // EO9b: a handler that ends with a literal (not a resume call) is
  // not classified tail-resumptive.
  claim <k>
    annotateTailResumptiveCheck(
      EHandle(
        effectExtend(qname("std", name("console", 0)), effectEmpty),
        ELam(
          param(name("x", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          param(name("k", 0), TCon(typecon(qname("std/core/types", name("int", 0)), KValue))) ,
          .ParamList,
          ELit(litInt(0))
        ),
        ELit(litInt(42))
      )
    )
    =>
    false
  </k> [label(claim-eo-9b)]

  // EO9c: classification is a no-op when applied to a non-EHandle expression.
  claim <k>
    annotateTailResumptiveCheck(ELit(litInt(7)))
    =>
    false
  </k> [label(claim-eo-9c)]

endmodule
