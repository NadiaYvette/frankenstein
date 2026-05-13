// multishot-claims.k — kprove claims for the multi-shot effect-handler
//                      classifier in src/Frankenstein/Core/EffectOpt.hs
//                      and the CPS converter in
//                      src/Frankenstein/Core/CpsConvert.hs.
//
// Behavioural specification mirrored in K helpers:
//
//   * isMultiShotHandler(E)
//       - a 2-param ELam whose last parameter (K) is applied either
//         more than once, OR exactly once but in non-tail position.
//
//   * countAppsOf(N, E)
//       - counts EApp sites whose head is EVar(N).
//
// Six claims, grouped:
//
//   MS1: classifier — concrete instances of Abort / Tail / Multi shapes.
//   MS2: handler ABI invariants — multi-shot detection composes
//        correctly with let / case / nested EPerform structures.
//   MS3: structural — every Multi handler has >= 2 params.
//
// Usage:
//   kompile all-claims-def.k --backend haskell \
//     --main-module ALL-CLAIMS-DEF --syntax-module ALL-CLAIMS-DEF-SYNTAX \
//     -o all-claims-haskell-kompiled
//   kprove multishot-claims.k --definition all-claims-haskell-kompiled

requires "all-claims-def.k"

module MULTISHOT-CLAIMS-SYNTAX
  imports ALL-CLAIMS-DEF-SYNTAX
endmodule

module MULTISHOT-CLAIMS
  imports ALL-CLAIMS-DEF
  imports BOOL
  imports INT
  imports STRING

  // =================================================================
  // MS1: Classifier on canonical shapes.
  //
  // The classifyHandler function in Haskell partitions handlers into
  // HKAbort | HKTail | HKMulti by counting tail-positioned applications
  // of the last parameter. The K analogues are countAppsOf + isTailCall.
  // =================================================================

  // MS1a: \(x, k) -> x          — resume never used → not multi-shot.
  claim <k>
    isMultiShotHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        .ParamList,
        EVar(name("x", 0))
      )
    )
    =>
    false
  </k> [label(claim-ms-1a-abort-shape-not-multi)]

  // MS1b: \(x, k) -> k(x)       — single tail resume → not multi-shot.
  claim <k>
    isMultiShotHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList)
      )
    )
    =>
    false
  </k> [label(claim-ms-1b-tail-resumptive-not-multi)]

  // MS1c: \(x, k) -> let r1 = k(1) in k(0)   — two resume calls → multi.
  claim <k>
    isMultiShotHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        .ParamList,
        ELet(
          bind(name("r1", 0),
               TCon(typecon(qname("std", name("int", 0)), KValue)),
               EApp(EVar(name("k", 0)), ELit(litInt(1)) , .ExprList),
               defVal) , .BindGroup ;; .BGList,
          EApp(EVar(name("k", 0)), ELit(litInt(0)) , .ExprList)
        )
      )
    )
    =>
    true
  </k> [label(claim-ms-1c-two-resumes-is-multi)]

  // MS1d: \(x, k) -> +(k(x), 1)   — single resume in non-tail position
  // (its result is consumed by `+`) → multi-shot.
  claim <k>
    isMultiShotHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        param(name("k", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        .ParamList,
        EApp(EVar(name("+", 0)),
             EApp(EVar(name("k", 0)), EVar(name("x", 0)) , .ExprList) ,
             ELit(litInt(1)) ,
             .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-ms-1d-non-tail-resume-is-multi)]

  // =================================================================
  // MS2: countAppsOf is composition-correct.
  //
  // The classifier and the CPS converter both rely on countAppsOf to
  // discover where the resume parameter is referenced. These claims
  // pin its behaviour at the structural boundaries the converter
  // walks.
  // =================================================================

  // MS2a: A literal references nothing.
  claim <k>
    countAppsOf(name("k", 0), ELit(litInt(42))) => 0
  </k> [label(claim-ms-2a-literal-no-apps)]

  // MS2b: A direct application EApp(EVar k, args) counts as one.
  claim <k>
    countAppsOf(
      name("k", 0),
      EApp(EVar(name("k", 0)), ELit(litInt(7)) , .ExprList)
    )
    =>
    1
  </k> [label(claim-ms-2b-direct-app-one)]

  // MS2c: Nested applications compose additively.
  //   k(k(0))       -> 2
  claim <k>
    countAppsOf(
      name("k", 0),
      EApp(EVar(name("k", 0)),
           EApp(EVar(name("k", 0)), ELit(litInt(0)) , .ExprList) ,
           .ExprList)
    )
    =>
    2
  </k> [label(claim-ms-2c-nested-applies-sum)]

  // =================================================================
  // MS3: Structural invariants.
  //
  // The Multi classification can only fire for ELams with >= 2 params.
  // 0-arg and 1-arg ELams default to non-multi (Tail in the Haskell
  // implementation; here, isMultiShotHandler returns false directly).
  // =================================================================

  // MS3a: 1-param lambda is never multi-shot.
  claim <k>
    isMultiShotHandler(
      ELam(
        param(name("x", 0), TCon(typecon(qname("std", name("int", 0)), KValue))) ,
        .ParamList,
        EVar(name("x", 0))
      )
    )
    =>
    false
  </k> [label(claim-ms-3a-one-param-not-multi)]

  // MS3b: Non-lambda expression is never multi-shot.
  claim <k>
    isMultiShotHandler(ELit(litInt(42))) => false
  </k> [label(claim-ms-3b-non-lambda-not-multi)]

endmodule
