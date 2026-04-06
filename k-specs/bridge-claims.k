// bridge-claims.k — kprove claims promoting bridge property tests to formal verification
//
// These claims correspond to the 47 boolean-valued property functions in
// bridge-properties.k. Each claim verifies that the property returns true
// for concrete OrganIR terms representative of each bridge's output.
//
// Usage:
//   kompile organ-ir.k --backend haskell --main-module ORGAN-IR --syntax-module ORGAN-IR-SYNTAX -o organ-ir-haskell-kompiled
//   kprove bridge-claims.k --definition organ-ir-haskell-kompiled

requires "all-claims-def.k"

module BRIDGE-CLAIMS-SYNTAX
  imports ALL-CLAIMS-DEF-SYNTAX
endmodule

module BRIDGE-CLAIMS
  imports ALL-CLAIMS-DEF
  imports SET
  imports MAP
  imports INT
  imports BOOL
  imports LIST

  // =================================================================
  // GHC Bridge Claims (5 properties)
  // =================================================================

  // --- checkGhcStrictNoDelay: strict bindings must not be EDelay ---

  // Claim G1a: A literal (strict) is not delayed
  claim <k>
    checkGhcStrictNoDelay(ELit(litInt(42)))
    =>
    true
  </k> [label(claim-ghc-strict-lit)]

  // Claim G1b: A variable reference (strict) is not delayed
  claim <k>
    checkGhcStrictNoDelay(EVar(name("x", 0)))
    =>
    true
  </k> [label(claim-ghc-strict-var)]

  // Claim G1c: An application (strict) is not delayed
  claim <k>
    checkGhcStrictNoDelay(EApp(EVar(name("f", 0)), EVar(name("x", 0)) , .ExprList))
    =>
    true
  </k> [label(claim-ghc-strict-app)]

  // Claim G1d: An EDelay is correctly detected as NOT strict
  claim <k>
    checkGhcStrictNoDelay(EDelay(ELit(litInt(42))))
    =>
    false
  </k> [label(claim-ghc-strict-delay-false)]

  // --- checkGhcLazyIsDelayed: lazy bindings must be EDelay ---

  // Claim G2a: EDelay wrapping is detected
  claim <k>
    checkGhcLazyIsDelayed(EDelay(ELit(litInt(42))))
    =>
    true
  </k> [label(claim-ghc-lazy-delayed)]

  // Claim G2b: Non-delayed expression fails lazy check
  claim <k>
    checkGhcLazyIsDelayed(ELit(litInt(42)))
    =>
    false
  </k> [label(claim-ghc-lazy-not-delayed)]

  // Claim G2c: EDelay wrapping a complex expression
  claim <k>
    checkGhcLazyIsDelayed(EDelay(EApp(EVar(name("f", 0)), EVar(name("x", 0)) , .ExprList)))
    =>
    true
  </k> [label(claim-ghc-lazy-delayed-app)]

  // --- checkGhcForallKind: ForAllTy maps to TForall with KStar + Many ---

  // Claim G3a: Well-formed TForall with KStar and Many
  claim <k>
    checkGhcForallKind(
      TForall(typevar(name("a", 0), KStar, many) , .TypeVarList,
              TCon(typecon(qname("s", name("int", 0)), KValue)))
    )
    =>
    true
  </k> [label(claim-ghc-forall-kstar)]

  // Claim G3b: Wrong kind fails
  claim <k>
    checkGhcForallKind(
      TForall(typevar(name("a", 0), KEffect, many) , .TypeVarList,
              TCon(typecon(qname("s", name("int", 0)), KValue)))
    )
    =>
    false
  </k> [label(claim-ghc-forall-wrong-kind)]

  // Claim G3c: Wrong multiplicity fails
  claim <k>
    checkGhcForallKind(
      TForall(typevar(name("a", 0), KStar, linear) , .TypeVarList,
              TCon(typecon(qname("s", name("int", 0)), KValue)))
    )
    =>
    false
  </k> [label(claim-ghc-forall-wrong-mul)]

  // --- checkGhcAbsentIsDead: absent binding -> ELit(litInt(0)) ---

  // Claim G4a: Dead binding placeholder
  claim <k>
    checkGhcAbsentIsDead(ELit(litInt(0)))
    =>
    true
  </k> [label(claim-ghc-absent-dead)]

  // Claim G4b: Non-zero literal fails
  claim <k>
    checkGhcAbsentIsDead(ELit(litInt(42)))
    =>
    false
  </k> [label(claim-ghc-absent-nonzero)]

  // --- checkGhcAllArgsMany: all TFun args have Many multiplicity ---

  // Claim G5a: Single Many arg
  claim <k>
    checkGhcAllArgsMany(
      TFun(
        multype(many, TCon(typecon(qname("s", name("int", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("int", 0)), KValue))
      )
    )
    =>
    true
  </k> [label(claim-ghc-args-many-single)]

  // Claim G5b: Two Many args
  claim <k>
    checkGhcAllArgsMany(
      TFun(
        multype(many, TCon(typecon(qname("s", name("int", 0)), KValue))) ,
        multype(many, TCon(typecon(qname("s", name("bool", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("int", 0)), KValue))
      )
    )
    =>
    true
  </k> [label(claim-ghc-args-many-two)]

  // Claim G5c: Linear arg fails
  claim <k>
    checkGhcAllArgsMany(
      TFun(
        multype(linear, TCon(typecon(qname("s", name("int", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("int", 0)), KValue))
      )
    )
    =>
    false
  </k> [label(claim-ghc-args-many-linear-fail)]

  // Claim G5d: Non-TFun passes vacuously
  claim <k>
    checkGhcAllArgsMany(
      TCon(typecon(qname("s", name("int", 0)), KValue))
    )
    =>
    true
  </k> [label(claim-ghc-args-many-nonTFun)]

  // =================================================================
  // Rust Bridge Claims (6 properties)
  // =================================================================

  // --- checkRustAffine: owned types must be linear or affine ---

  // Claim R1a: Affine is valid
  claim <k> checkRustAffine(affine) => true </k> [label(claim-rust-affine-ok)]

  // Claim R1b: Linear is valid
  claim <k> checkRustAffine(linear) => true </k> [label(claim-rust-linear-ok)]

  // Claim R1c: Many is invalid for Rust
  claim <k> checkRustAffine(many) => false </k> [label(claim-rust-many-fail)]

  // --- checkRustFunArgsAffine: all Rust function args must be affine/linear ---

  // Claim R2a: Single affine arg
  claim <k>
    checkRustFunArgsAffine(
      TFun(
        multype(affine, TCon(typecon(qname("s", name("i64", 0)), KValue))) , .MulTypeList,
        effectExtend(qname("std", name("io", 0)), effectEmpty),
        TCon(typecon(qname("s", name("i64", 0)), KValue))
      )
    )
    =>
    true
  </k> [label(claim-rust-fun-args-affine)]

  // Claim R2b: Many arg fails
  claim <k>
    checkRustFunArgsAffine(
      TFun(
        multype(many, TCon(typecon(qname("s", name("i64", 0)), KValue))) , .MulTypeList,
        effectExtend(qname("std", name("io", 0)), effectEmpty),
        TCon(typecon(qname("s", name("i64", 0)), KValue))
      )
    )
    =>
    false
  </k> [label(claim-rust-fun-args-many-fail)]

  // --- checkRustDropOnUnused: unused vars must have EDrop ---

  // Claim R3a: Unused variable has drop
  claim <k>
    checkRustDropOnUnused(
      name("x", 0),
      ELet(
        (bind(name("_drop", 0), TCon(typecon(qname("s", name("unit", 0)), KValue)),
              EDrop(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
        ELit(litInt(99))
      )
    )
    =>
    true
  </k> [label(claim-rust-drop-unused)]

  // Claim R3b: Used variable passes (no drop needed)
  claim <k>
    checkRustDropOnUnused(
      name("x", 0),
      EVar(name("x", 0))
    )
    =>
    true
  </k> [label(claim-rust-drop-used-ok)]

  // --- checkRustMoveNoRetain: moved (used-once) var has no retain ---

  // Claim R4a: Single use, no retain
  claim <k>
    checkRustMoveNoRetain(
      name("x", 0),
      EApp(EVar(name("f", 0)), EVar(name("x", 0)) , .ExprList)
    )
    =>
    true
  </k> [label(claim-rust-move-no-retain)]

  // Claim R4b: Zero uses, no retain — also valid
  claim <k>
    checkRustMoveNoRetain(
      name("x", 0),
      ELit(litInt(42))
    )
    =>
    true
  </k> [label(claim-rust-move-zero-use)]

  // --- checkRustCopyHasRetain: copy operand must be ERetain(EVar(_)) ---

  // Claim R5a: ERetain wrapping detected
  claim <k>
    checkRustCopyHasRetain(ERetain(EVar(name("x", 0))))
    =>
    true
  </k> [label(claim-rust-copy-retain)]

  // Claim R5b: Missing retain fails
  claim <k>
    checkRustCopyHasRetain(EVar(name("x", 0)))
    =>
    false
  </k> [label(claim-rust-copy-no-retain-fail)]

  // --- checkRustHasIoEffect: Rust functions carry IO effect ---

  // Claim R6a: IO effect present
  claim <k>
    checkRustHasIoEffect(
      TFun(
        multype(affine, TCon(typecon(qname("s", name("i64", 0)), KValue))) , .MulTypeList,
        effectExtend(qname("std", name("io", 0)), effectEmpty),
        TCon(typecon(qname("s", name("i64", 0)), KValue))
      )
    )
    =>
    true
  </k> [label(claim-rust-io-effect)]

  // Claim R6b: No IO effect fails
  claim <k>
    checkRustHasIoEffect(
      TFun(
        multype(affine, TCon(typecon(qname("s", name("i64", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("i64", 0)), KValue))
      )
    )
    =>
    false
  </k> [label(claim-rust-no-io-fail)]

  // =================================================================
  // Mercury Bridge Claims (7 properties)
  // =================================================================

  // --- checkMercuryDetPure: det mode -> empty effect row ---

  claim <k> checkMercuryDetPure(effectEmpty) => true </k>
    [label(claim-mercury-det-pure)]

  claim <k>
    checkMercuryDetPure(effectExtend(qname("mercury", name("exn", 0)), effectEmpty))
    =>
    false
  </k> [label(claim-mercury-det-not-exn)]

  // --- checkMercuryDetType: det TFun has empty effect row ---

  claim <k>
    checkMercuryDetType(
      TFun(
        multype(many, TCon(typecon(qname("s", name("int", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("int", 0)), KValue))
      )
    )
    =>
    true
  </k> [label(claim-mercury-det-type)]

  // --- checkMercurySemidetExn: semidet mode -> exn in effect row ---

  claim <k>
    checkMercurySemidetExn(
      effectExtend(qname("mercury", name("exn", 0)), effectEmpty)
    )
    =>
    true
  </k> [label(claim-mercury-semidet-exn)]

  claim <k>
    checkMercurySemidetExn(effectEmpty)
    =>
    false
  </k> [label(claim-mercury-semidet-no-exn)]

  // --- checkMercurySemidetExact: semidet effect row is exactly exn + empty ---

  claim <k>
    checkMercurySemidetExact(
      effectExtend(qname("mercury", name("exn", 0)), effectEmpty)
    )
    =>
    true
  </k> [label(claim-mercury-semidet-exact)]

  claim <k>
    checkMercurySemidetExact(
      effectExtend(qname("mercury", name("exn", 0)),
        effectExtend(qname("mercury", name("choice", 0)), effectEmpty))
    )
    =>
    false
  </k> [label(claim-mercury-semidet-not-exact)]

  // --- checkMercuryMultiChoice: multi mode -> choice in effect row ---

  claim <k>
    checkMercuryMultiChoice(
      effectExtend(qname("mercury", name("choice", 0)), effectEmpty)
    )
    =>
    true
  </k> [label(claim-mercury-multi-choice)]

  claim <k>
    checkMercuryMultiChoice(effectEmpty)
    =>
    false
  </k> [label(claim-mercury-multi-no-choice)]

  // --- checkMercuryNondetBoth: nondet mode -> both exn and choice ---

  claim <k>
    checkMercuryNondetBoth(
      effectExtend(qname("mercury", name("exn", 0)),
        effectExtend(qname("mercury", name("choice", 0)), effectEmpty))
    )
    =>
    true
  </k> [label(claim-mercury-nondet-both)]

  claim <k>
    checkMercuryNondetBoth(
      effectExtend(qname("mercury", name("exn", 0)), effectEmpty)
    )
    =>
    false
  </k> [label(claim-mercury-nondet-exn-only)]

  // --- checkMercuryDiLinear: di/uo mode -> linear multiplicity ---

  claim <k> checkMercuryDiLinear(linear) => true </k>
    [label(claim-mercury-di-linear)]

  claim <k> checkMercuryDiLinear(many) => false </k>
    [label(claim-mercury-di-many-fail)]

  claim <k> checkMercuryDiLinear(affine) => false </k>
    [label(claim-mercury-di-affine-fail)]

  // --- checkMercuryInMany: in mode -> many multiplicity ---

  claim <k> checkMercuryInMany(many) => true </k>
    [label(claim-mercury-in-many)]

  claim <k> checkMercuryInMany(linear) => false </k>
    [label(claim-mercury-in-linear-fail)]

  // =================================================================
  // Koka Bridge Claims (7 properties)
  // =================================================================

  // --- checkKokaEffectLength: effect row has expected length ---

  claim <k>
    checkKokaEffectLength(effectEmpty, 0)
    =>
    true
  </k> [label(claim-koka-effect-len-0)]

  claim <k>
    checkKokaEffectLength(
      effectExtend(qname("std", name("console", 0)), effectEmpty),
      1
    )
    =>
    true
  </k> [label(claim-koka-effect-len-1)]

  claim <k>
    checkKokaEffectLength(
      effectExtend(qname("std", name("console", 0)),
        effectExtend(qname("std", name("exn", 0)), effectEmpty)),
      2
    )
    =>
    true
  </k> [label(claim-koka-effect-len-2)]

  claim <k>
    checkKokaEffectLength(
      effectExtend(qname("std", name("console", 0)), effectEmpty),
      0
    )
    =>
    false
  </k> [label(claim-koka-effect-len-wrong)]

  // --- checkKokaEffectsPresent: all expected effects appear in row ---

  claim <k>
    checkKokaEffectsPresent(
      ListItem(qname("std", name("console", 0)))
      ListItem(qname("std", name("exn", 0)))
      .List,
      effectExtend(qname("std", name("console", 0)),
        effectExtend(qname("std", name("exn", 0)), effectEmpty))
    )
    =>
    true
  </k> [label(claim-koka-effects-present)]

  claim <k>
    checkKokaEffectsPresent(.List, effectEmpty)
    =>
    true
  </k> [label(claim-koka-effects-empty)]

  // --- checkKokaPureIsEmpty: total/pure -> EffectRowEmpty ---

  claim <k> checkKokaPureIsEmpty(effectEmpty) => true </k>
    [label(claim-koka-pure-empty)]

  claim <k>
    checkKokaPureIsEmpty(effectExtend(qname("std", name("exn", 0)), effectEmpty))
    =>
    false
  </k> [label(claim-koka-pure-not-empty)]

  // --- checkKokaAllMulMany: all arg multiplicities default to Many ---

  claim <k>
    checkKokaAllMulMany(
      TFun(
        multype(many, TCon(typecon(qname("s", name("int", 0)), KValue))) ,
        multype(many, TCon(typecon(qname("s", name("int", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("int", 0)), KValue))
      )
    )
    =>
    true
  </k> [label(claim-koka-all-mul-many)]

  claim <k>
    checkKokaAllMulMany(
      TFun(
        multype(affine, TCon(typecon(qname("s", name("int", 0)), KValue))) , .MulTypeList,
        effectEmpty,
        TCon(typecon(qname("s", name("int", 0)), KValue))
      )
    )
    =>
    false
  </k> [label(claim-koka-mul-affine-fail)]

  // --- checkKokaTypeVarMany: type variables have Many multiplicity ---

  claim <k>
    checkKokaTypeVarMany(typevar(name("a", 0), KStar, many))
    =>
    true
  </k> [label(claim-koka-typevar-many)]

  claim <k>
    checkKokaTypeVarMany(typevar(name("a", 0), KStar, linear))
    =>
    false
  </k> [label(claim-koka-typevar-linear-fail)]

  // --- checkKokaSynExpansion: TSyn body is not itself a TSyn ---

  claim <k>
    checkKokaSynExpansion(testSynExpanded())
    =>
    true
  </k> [label(claim-koka-syn-expanded)]

  claim <k>
    checkKokaSynExpansion(testSynNested())
    =>
    false
  </k> [label(claim-koka-syn-nested-fail)]

  claim <k>
    checkKokaSynExpansion(
      TCon(typecon(qname("s", name("int", 0)), KValue))
    )
    =>
    true
  </k> [label(claim-koka-syn-non-syn-ok)]

  // --- checkKokaSynHasBody: TSyn must have a body ---

  claim <k>
    checkKokaSynHasBody(testSynHasBody())
    =>
    true
  </k> [label(claim-koka-syn-has-body)]

  claim <k>
    checkKokaSynHasBody(
      TCon(typecon(qname("s", name("int", 0)), KValue))
    )
    =>
    false
  </k> [label(claim-koka-syn-not-syn)]

  // =================================================================
  // Cross-bridge helper claims (shared infrastructure)
  // =================================================================

  // --- effectRowContains ---
  claim <k>
    effectRowContains(
      effectExtend(qname("std", name("io", 0)), effectEmpty),
      qname("std", name("io", 0))
    )
    =>
    true
  </k> [label(claim-helper-effect-contains)]

  claim <k>
    effectRowContains(effectEmpty, qname("std", name("io", 0)))
    =>
    false
  </k> [label(claim-helper-effect-empty)]

  // --- isEffectRowEmpty ---
  claim <k> isEffectRowEmpty(effectEmpty) => true </k>
    [label(claim-helper-row-empty)]

  claim <k>
    isEffectRowEmpty(effectExtend(qname("s", name("e", 0)), effectEmpty))
    =>
    false
  </k> [label(claim-helper-row-notempty)]

  // --- effectRowLength ---
  claim <k> effectRowLength(effectEmpty) => 0 </k>
    [label(claim-helper-row-len-0)]

  claim <k>
    effectRowLength(
      effectExtend(qname("a", name("b", 0)),
        effectExtend(qname("c", name("d", 0)), effectEmpty))
    )
    =>
    2
  </k> [label(claim-helper-row-len-2)]

  // --- isDelayed / containsDelay ---
  claim <k> isDelayed(EDelay(ELit(litInt(1)))) => true </k>
    [label(claim-helper-is-delayed)]

  claim <k> isDelayed(ELit(litInt(1))) => false </k>
    [label(claim-helper-not-delayed)]

  claim <k> containsDelay(EDelay(ELit(litInt(1)))) => true </k>
    [label(claim-helper-contains-delay)]

  claim <k> containsDelay(ELit(litInt(1))) => false </k>
    [label(claim-helper-no-delay)]

  // --- containsDropOf / containsRetainOf ---
  claim <k>
    containsDropOf(EDrop(EVar(name("x", 0))), name("x", 0))
    =>
    true
  </k> [label(claim-helper-contains-drop)]

  claim <k>
    containsDropOf(ELit(litInt(1)), name("x", 0))
    =>
    false
  </k> [label(claim-helper-no-drop)]

  claim <k>
    containsRetainOf(ERetain(EVar(name("x", 0))), name("x", 0))
    =>
    true
  </k> [label(claim-helper-contains-retain)]

  claim <k>
    containsRetainOf(ELit(litInt(1)), name("x", 0))
    =>
    false
  </k> [label(claim-helper-no-retain)]

endmodule
