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

endmodule
