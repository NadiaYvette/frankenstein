requires "organ-ir.k"

// Bridge Translation Properties — K Specification
//
// Partial specification of correctness properties for each bridge's
// CoreTranslate module. These are boolean-valued K functions that
// check structural invariants on OrganIR output produced by each bridge.
//
// Each property can be tested via krun against concrete OrganIR terms.
//
// Depends on: organ-ir.k (ORGAN-IR-SYNTAX, ORGAN-IR)
//
// Usage:
//   kompile bridge-properties.k --backend llvm
//   krun -cPGM='checkGhcStrictNoDelay(ELit(litInt(42)))' \
//        --definition bridge-properties-kompiled

// =================================================================
// Module: BRIDGE-HELPERS — shared helpers for all bridge properties
// =================================================================

module BRIDGE-HELPERS
  imports ORGAN-IR
  imports BOOL
  imports INT
  imports STRING
  imports SET
  imports MAP
  imports LIST

  // --- Effect row membership ---
  // Check whether a named effect appears anywhere in an EffectRow
  syntax Bool ::= effectRowContains(EffectRow, QName) [function, symbol(effectRowContains)]

  rule effectRowContains(effectEmpty, _) => false
  rule effectRowContains(effectVar(_), _) => false
  rule effectRowContains(effectExtend(QN, _), QN) => true
  rule effectRowContains(effectExtend(_, Rest), QN)
    => effectRowContains(Rest, QN)
    [owise]

  // --- Effect row emptiness ---
  syntax Bool ::= isEffectRowEmpty(EffectRow) [function, symbol(isEffectRowEmpty)]
  rule isEffectRowEmpty(effectEmpty) => true
  rule isEffectRowEmpty(effectExtend(_, _)) => false
  rule isEffectRowEmpty(effectVar(_)) => false

  // --- Effect row length ---
  syntax Int ::= effectRowLength(EffectRow) [function, symbol(effectRowLength)]
  rule effectRowLength(effectEmpty) => 0
  rule effectRowLength(effectVar(_)) => 0
  rule effectRowLength(effectExtend(_, Rest)) => 1 +Int effectRowLength(Rest)

  // --- Check if an expression is wrapped in EDelay at the top ---
  syntax Bool ::= isDelayed(Expr) [function, symbol(isDelayed)]
  rule isDelayed(EDelay(_)) => true
  rule isDelayed(_)         => false [owise]

  // --- Check if an expression contains any EDelay subexpression ---
  syntax Bool ::= containsDelay(Expr) [function, symbol(containsDelay)]
  rule containsDelay(EDelay(_))        => true
  rule containsDelay(EVar(_))          => false
  rule containsDelay(ELit(_))          => false
  rule containsDelay(ECon(_))          => false
  rule containsDelay(ERetain(E))       => containsDelay(E)
  rule containsDelay(ERelease(E))      => containsDelay(E)
  rule containsDelay(EDrop(E))         => containsDelay(E)
  rule containsDelay(EForce(E))        => containsDelay(E)
  rule containsDelay(ETypeApp(E, _))   => containsDelay(E)
  rule containsDelay(ETypeLam(_, E))   => containsDelay(E)
  rule containsDelay(EApp(F, _))       => containsDelay(F)   // conservative: check func
  rule containsDelay(ELam(_, Body))    => containsDelay(Body)
  rule containsDelay(ECase(S, _))      => containsDelay(S)   // conservative: check scrutinee
  rule containsDelay(EReuse(A, _))     => containsDelay(A)
  rule containsDelay(EHandle(_, H, _)) => containsDelay(H)
  rule containsDelay(EPerform(_, _))   => false
  rule containsDelay(ELet(_, Body))    => containsDelay(Body) // conservative: check body

  // --- Check if an expression contains EDrop for a given name ---
  syntax Bool ::= containsDropOf(Expr, Name) [function, symbol(containsDropOf)]
  rule containsDropOf(EDrop(EVar(N)), N)      => true
  rule containsDropOf(EVar(_), _)             => false
  rule containsDropOf(ELit(_), _)             => false
  rule containsDropOf(ECon(_), _)             => false
  rule containsDropOf(EDrop(E), N)            => containsDropOf(E, N)  [owise]
  rule containsDropOf(ERetain(E), N)          => containsDropOf(E, N)
  rule containsDropOf(ERelease(E), N)         => containsDropOf(E, N)
  rule containsDropOf(EDelay(E), N)           => containsDropOf(E, N)
  rule containsDropOf(EForce(E), N)           => containsDropOf(E, N)
  rule containsDropOf(ETypeApp(E, _), N)      => containsDropOf(E, N)
  rule containsDropOf(ETypeLam(_, E), N)      => containsDropOf(E, N)
  rule containsDropOf(EReuse(A, B), N)        => containsDropOf(A, N) orBool containsDropOf(B, N)
  rule containsDropOf(EHandle(_, H, B), N)    => containsDropOf(H, N) orBool containsDropOf(B, N)
  rule containsDropOf(EPerform(_, _), _)      => false
  rule containsDropOf(EApp(F, _), N)          => containsDropOf(F, N)  // conservative
  rule containsDropOf(ELam(_, Body), N)       => containsDropOf(Body, N)
  rule containsDropOf(ECase(S, _), N)         => containsDropOf(S, N)  // conservative
  rule containsDropOf(ELet(_, Body), N)       => containsDropOf(Body, N)

  // --- Check if expression contains ERetain for a given name ---
  syntax Bool ::= containsRetainOf(Expr, Name) [function, symbol(containsRetainOf)]
  rule containsRetainOf(ERetain(EVar(N)), N)    => true
  rule containsRetainOf(EVar(_), _)             => false
  rule containsRetainOf(ELit(_), _)             => false
  rule containsRetainOf(ECon(_), _)             => false
  rule containsRetainOf(ERetain(E), N)          => containsRetainOf(E, N)  [owise]
  rule containsRetainOf(EDrop(E), N)            => containsRetainOf(E, N)
  rule containsRetainOf(ERelease(E), N)         => containsRetainOf(E, N)
  rule containsRetainOf(EDelay(E), N)           => containsRetainOf(E, N)
  rule containsRetainOf(EForce(E), N)           => containsRetainOf(E, N)
  rule containsRetainOf(ETypeApp(E, _), N)      => containsRetainOf(E, N)
  rule containsRetainOf(ETypeLam(_, E), N)      => containsRetainOf(E, N)
  rule containsRetainOf(EReuse(A, B), N)        => containsRetainOf(A, N) orBool containsRetainOf(B, N)
  rule containsRetainOf(EHandle(_, H, B), N)    => containsRetainOf(H, N) orBool containsRetainOf(B, N)
  rule containsRetainOf(EPerform(_, _), _)      => false
  rule containsRetainOf(EApp(F, _), N)          => containsRetainOf(F, N)
  rule containsRetainOf(ELam(_, Body), N)       => containsRetainOf(Body, N)
  rule containsRetainOf(ECase(S, _), N)         => containsRetainOf(S, N)
  rule containsRetainOf(ELet(_, Body), N)       => containsRetainOf(Body, N)

  // --- Extract multiplicity from a MulType list by position ---
  syntax Multiplicity ::= mulTypeAtIndex(MulTypeList, Int) [function, symbol(mulTypeAtIndex)]
  rule mulTypeAtIndex((multype(Mul, _) , _:MulTypeList), 0) => Mul
  rule mulTypeAtIndex((multype(_, _) , Rest:MulTypeList), I:Int)
    => mulTypeAtIndex(Rest, I -Int 1)
    requires I >Int 0

  // --- Extract effect row from a TFun ---
  syntax EffectRow ::= funEffectRow(Type) [function, symbol(funEffectRow)]
  rule funEffectRow(TFun(_, Eff, _)) => Eff

  // --- Extract return type from a TFun ---
  syntax Type ::= funRetType(Type) [function, symbol(funRetType)]
  rule funRetType(TFun(_, _, Ret)) => Ret

  // --- Extract the multiplicity from first position of a TFun arg list ---
  syntax Multiplicity ::= firstArgMul(Type) [function, symbol(firstArgMul)]
  rule firstArgMul(TFun((multype(M, _) , _:MulTypeList), _, _)) => M

  // --- Count occurrences of EVar(N) in an expression (top level, not deep) ---
  syntax Int ::= varOccurrences(Name, Expr) [function, symbol(varOccurrences)]
  rule varOccurrences(N, E) => usageCount(N, E)  // reuse from PERCEUS module

endmodule

// =================================================================
// Module: GHC-BRIDGE-PROPERTIES
//
// Properties for GhcBridge/CoreTranslate.hs:
//   1. Strict bindings must NOT be wrapped in EDelay
//   2. Lazy bindings (no demand info) MUST be wrapped in EDelay
//   3. ForAllTy maps to TForall with correct kind
// =================================================================

module GHC-BRIDGE-PROPERTIES
  imports BRIDGE-HELPERS

  // ---------------------------------------------------------------
  // Property 1: checkGhcStrictNoDelay
  //
  // A GHC binding with strict demand annotation must translate to
  // a Def/Bind whose expression is NOT wrapped in EDelay.
  //
  // Mirrors GhcBridge/CoreTranslate.hs decideLaziness:
  //   | isStrictDmd (idDemandInfo b) = trExpr e  -- no thunk
  //
  // Input: the translated expression of a strict binding
  // Returns: true iff the expression is NOT delayed
  // ---------------------------------------------------------------

  syntax Bool ::= checkGhcStrictNoDelay(Expr) [function, symbol(checkGhcStrictNoDelay)]
  rule checkGhcStrictNoDelay(EDelay(_)) => false
  rule checkGhcStrictNoDelay(_)         => true  [owise]

  // ---------------------------------------------------------------
  // Property 2: checkGhcLazyIsDelayed
  //
  // A GHC binding with lazy demand (neither strict nor absent)
  // must translate to an expression wrapped in EDelay at the top.
  //
  // Mirrors GhcBridge/CoreTranslate.hs decideLaziness:
  //   | otherwise = F.EDelay (trExpr e)  -- lazy: wrap in thunk
  //
  // Input: the translated expression of a lazy binding
  // Returns: true iff the expression IS delayed
  // ---------------------------------------------------------------

  syntax Bool ::= checkGhcLazyIsDelayed(Expr) [function, symbol(checkGhcLazyIsDelayed)]
  rule checkGhcLazyIsDelayed(EDelay(_)) => true
  rule checkGhcLazyIsDelayed(_)         => false [owise]

  // ---------------------------------------------------------------
  // Property 3: checkGhcForallKind
  //
  // A GHC ForAllTy must translate to a TForall. The translated
  // type vars should have KStar (since GHC bridge maps all
  // kind variables to KindStar).
  //
  // Mirrors GhcBridge/CoreTranslate.hs forAllBndrToTypeVar:
  //   tvKind = F.KindStar
  //   tvMultiplicity = F.Many
  //
  // Input: a translated Type
  // Returns: true iff it is a TForall and first type var has KStar + Many
  // ---------------------------------------------------------------

  syntax Bool ::= checkGhcForallKind(Type) [function, symbol(checkGhcForallKind)]
  rule checkGhcForallKind(
    TForall(typevar(_, KStar, many) , _, _)
  ) => true
  rule checkGhcForallKind(
    TForall(typevar(_, KStar, many) , .TypeVarList, _)
  ) => true
  rule checkGhcForallKind(_) => false [owise]

  // ---------------------------------------------------------------
  // Property 4: checkGhcAbsentIsDead
  //
  // An absent (dead) GHC binding produces a literal placeholder,
  // which must not be EDelay and must be ELit.
  //
  // Mirrors: | isAbsDmd (idDemandInfo b) = F.ELit (F.LitInt 0)
  // ---------------------------------------------------------------

  syntax Bool ::= checkGhcAbsentIsDead(Expr) [function, symbol(checkGhcAbsentIsDead)]
  rule checkGhcAbsentIsDead(ELit(litInt(0))) => true
  rule checkGhcAbsentIsDead(_)               => false [owise]

  // ---------------------------------------------------------------
  // Property 5: checkGhcFunTypeMultiplicity
  //
  // GHC bridge always assigns Many multiplicity to function args.
  // All MulType entries in a TFun produced by the GHC bridge
  // must have multiplicity many.
  //
  // Mirrors GhcBridge/CoreTranslate.hs translateType for FunTy:
  //   F.TFun ((F.Many, translateType arg) : ...) ...
  // ---------------------------------------------------------------

  syntax Bool ::= checkGhcAllArgsMany(Type)        [function, symbol(checkGhcAllArgsMany)]
  syntax Bool ::= checkMulListAllMany(MulTypeList)  [function, symbol(checkMulListAllMany)]

  rule checkGhcAllArgsMany(TFun(Args, _, _)) => checkMulListAllMany(Args)
  rule checkGhcAllArgsMany(_)                => true [owise]

  rule checkMulListAllMany(.MulTypeList)              => true
  rule checkMulListAllMany(multype(many, _) , Rest:MulTypeList)
    => checkMulListAllMany(Rest)
  rule checkMulListAllMany(multype(linear, _) , _)    => false
  rule checkMulListAllMany(multype(affine, _) , _)    => false

endmodule

// =================================================================
// Module: RUST-BRIDGE-PROPERTIES
//
// Properties for RustBridge/CoreTranslate.hs:
//   1. Affine/owned types must have Linear or Affine multiplicity
//   2. Every Rust variable in scope must have EDrop at scope exit if unused
//   3. Move semantics: used-once variables get no retain
// =================================================================

module RUST-BRIDGE-PROPERTIES
  imports BRIDGE-HELPERS
  imports PERCEUS  // for freeVars, usageCount

  // ---------------------------------------------------------------
  // Property 1: checkRustAffine
  //
  // Rust owned/affine types must translate with Linear or Affine
  // multiplicity — never Many.
  //
  // Mirrors RustBridge/CoreTranslate.hs translateBody:
  //   argTypes = [(Affine, localTypeToType l) | l <- argLocals]
  //
  // Input: a multiplicity from a Rust-bridge-produced arg type
  // Returns: true iff the multiplicity is linear or affine
  // ---------------------------------------------------------------

  syntax Bool ::= checkRustAffine(Multiplicity) [function, symbol(checkRustAffine)]
  rule checkRustAffine(linear) => true
  rule checkRustAffine(affine) => true
  rule checkRustAffine(many)   => false

  // ---------------------------------------------------------------
  // Property 1b: checkRustFunArgsAffine
  //
  // All argument multiplicities in a Rust-bridge-produced TFun
  // must be Affine (the bridge hardcodes Affine for all args).
  // ---------------------------------------------------------------

  syntax Bool ::= checkRustFunArgsAffine(Type)         [function, symbol(checkRustFunArgsAffine)]
  syntax Bool ::= checkMulListAllAffine(MulTypeList)    [function, symbol(checkMulListAllAffine)]

  rule checkRustFunArgsAffine(TFun(Args, _, _)) => checkMulListAllAffine(Args)
  rule checkRustFunArgsAffine(_)                => true [owise]

  rule checkMulListAllAffine(.MulTypeList)              => true
  rule checkMulListAllAffine(multype(affine, _) , Rest:MulTypeList)
    => checkMulListAllAffine(Rest)
  rule checkMulListAllAffine(multype(linear, _) , Rest:MulTypeList)
    => checkMulListAllAffine(Rest)     // linear is also acceptable
  rule checkMulListAllAffine(multype(many, _) , _)      => false

  // ---------------------------------------------------------------
  // Property 2: checkRustDropOnUnused
  //
  // If a Rust variable is in scope but unused in the body,
  // the output must contain EDrop(EVar(x)).
  //
  // Mirrors RustBridge/CoreTranslate.hs:
  //   StorageDead(_x) -> EDrop x (scope exit, release)
  //   TermDrop idx target -> EDrop (EVar ...)
  //
  // Input: variable name, the body expression
  // Returns: true iff the body contains EDrop(EVar(N)) when N is not free
  // ---------------------------------------------------------------

  syntax Bool ::= checkRustDropOnUnused(Name, Expr) [function, symbol(checkRustDropOnUnused)]
  rule checkRustDropOnUnused(N, Body)
    => ( isFree(N, Body) )              // variable IS used: no drop needed, pass
       orBool containsDropOf(Body, N)   // variable NOT used: must have drop
    // In other words: either the variable is used, or it has a drop.
    // This is universally true for well-translated Rust code.

  // ---------------------------------------------------------------
  // Property 3: checkRustMoveNoRetain
  //
  // A moved (used-once) Rust variable should NOT have ERetain.
  // Move semantics: ownership transfer, no refcount increment.
  //
  // Mirrors RustBridge/CoreTranslate.hs:
  //   OpMove idx -> EVar ...  (no ERetain inserted)
  //   OpCopy idx -> ERetain (EVar ...)  (explicit retain for Copy types)
  //
  // Input: variable name, the body expression
  // Returns: true iff used exactly once AND no retain exists for it
  // ---------------------------------------------------------------

  syntax Bool ::= checkRustMoveNoRetain(Name, Expr) [function, symbol(checkRustMoveNoRetain)]
  rule checkRustMoveNoRetain(N, Body)
    => (usageCount(N, Body) <=Int 1)
       andBool (notBool containsRetainOf(Body, N))

  // ---------------------------------------------------------------
  // Property 4: checkRustCopyHasRetain
  //
  // A copied Rust variable (OpCopy) must have ERetain wrapping it.
  //
  // Mirrors RustBridge/CoreTranslate.hs:
  //   OpCopy idx -> ERetain (EVar ...)
  //
  // Input: the expression produced by an OpCopy operand
  // Returns: true iff it is ERetain(EVar(_))
  // ---------------------------------------------------------------

  syntax Bool ::= checkRustCopyHasRetain(Expr) [function, symbol(checkRustCopyHasRetain)]
  rule checkRustCopyHasRetain(ERetain(EVar(_))) => true
  rule checkRustCopyHasRetain(_)                => false [owise]

  // ---------------------------------------------------------------
  // Property 5: checkRustHasIoEffect
  //
  // Rust functions carry the IO effect (all Rust functions can
  // perform arbitrary side effects).
  //
  // Mirrors RustBridge/CoreTranslate.hs:
  //   effRow = EffectRowExtend (QName "std" (Name "io" 0)) EffectRowEmpty
  // ---------------------------------------------------------------

  syntax Bool ::= checkRustHasIoEffect(Type) [function, symbol(checkRustHasIoEffect)]
  rule checkRustHasIoEffect(TFun(_, Eff, _))
    => effectRowContains(Eff, qname("std", name("io", 0)))
  rule checkRustHasIoEffect(_) => false [owise]

endmodule

// =================================================================
// Module: MERCURY-BRIDGE-PROPERTIES
//
// Properties for MercuryBridge/CoreTranslate.hs:
//   1. det mode -> pure/total effect (EffectRowEmpty)
//   2. semidet mode -> exn effect in EffectRow
//   3. di/uo mode args -> Linear multiplicity
// =================================================================

module MERCURY-BRIDGE-PROPERTIES
  imports BRIDGE-HELPERS

  // ---------------------------------------------------------------
  // Mercury-specific QNames for effects
  // ---------------------------------------------------------------

  syntax QName ::= mercuryExnQName()    [function, symbol(mercuryExnQName)]
  syntax QName ::= mercuryChoiceQName() [function, symbol(mercuryChoiceQName)]

  rule mercuryExnQName()    => qname("mercury", name("exn", 0))
  rule mercuryChoiceQName() => qname("mercury", name("choice", 0))

  // ---------------------------------------------------------------
  // Property 1: checkMercuryDetPure
  //
  // A Mercury predicate with determinism "det" must have a pure
  // effect (empty effect row).
  //
  // Mirrors MercuryBridge/CoreTranslate.hs detToEffectRow:
  //   detToEffectRow Det = EffectRowEmpty
  //
  // Input: the EffectRow of a det-mode predicate's translated type
  // Returns: true iff the effect row is empty
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercuryDetPure(EffectRow) [function, symbol(checkMercuryDetPure)]
  rule checkMercuryDetPure(effectEmpty) => true
  rule checkMercuryDetPure(_)           => false [owise]

  // ---------------------------------------------------------------
  // Property 1b: checkMercuryDetType
  //
  // Convenience: check the TFun type of a det predicate directly.
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercuryDetType(Type) [function, symbol(checkMercuryDetType)]
  rule checkMercuryDetType(TFun(_, effectEmpty, _)) => true
  rule checkMercuryDetType(_) => false [owise]

  // ---------------------------------------------------------------
  // Property 2: checkMercurySemidetExn
  //
  // A Mercury predicate with determinism "semidet" must have
  // the exn effect in its effect row.
  //
  // Mirrors MercuryBridge/CoreTranslate.hs detToEffectRow:
  //   detToEffectRow Semidet = EffectRowExtend (QName "mercury" (Name "exn" 0)) EffectRowEmpty
  //
  // Input: the EffectRow of a semidet-mode predicate's translated type
  // Returns: true iff the effect row contains exn
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercurySemidetExn(EffectRow) [function, symbol(checkMercurySemidetExn)]
  rule checkMercurySemidetExn(Eff) => effectRowContains(Eff, mercuryExnQName())

  // ---------------------------------------------------------------
  // Property 2b: checkMercurySemidetExact
  //
  // semidet effect row must be exactly: exn + empty
  // (not exn + choice or anything else)
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercurySemidetExact(EffectRow) [function, symbol(checkMercurySemidetExact)]
  rule checkMercurySemidetExact(effectExtend(qname("mercury", name("exn", 0)), effectEmpty))
    => true
  rule checkMercurySemidetExact(_) => false [owise]

  // ---------------------------------------------------------------
  // Property 3: checkMercuryMultiChoice
  //
  // A Mercury predicate with determinism "multi" must have
  // the choice effect in its effect row.
  //
  // Mirrors: detToEffectRow Multi = EffectRowExtend ... "choice" ...
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercuryMultiChoice(EffectRow) [function, symbol(checkMercuryMultiChoice)]
  rule checkMercuryMultiChoice(Eff) => effectRowContains(Eff, mercuryChoiceQName())

  // ---------------------------------------------------------------
  // Property 4: checkMercuryNondetBoth
  //
  // A Mercury predicate with determinism "nondet" must have
  // BOTH exn and choice effects in its effect row.
  //
  // Mirrors: detToEffectRow Nondet = exn(choice(empty))
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercuryNondetBoth(EffectRow) [function, symbol(checkMercuryNondetBoth)]
  rule checkMercuryNondetBoth(Eff)
    => effectRowContains(Eff, mercuryExnQName())
       andBool effectRowContains(Eff, mercuryChoiceQName())

  // ---------------------------------------------------------------
  // Property 5: checkMercuryDiLinear
  //
  // A Mercury di (destructive input) or uo (unique output) mode
  // argument must have Linear multiplicity.
  //
  // Mirrors MercuryBridge/CoreTranslate.hs modeToMult:
  //   modeToMult ModeDi = Linear
  //   modeToMult ModeUo = Linear
  //
  // Input: multiplicity from a di/uo argument's MulType
  // Returns: true iff linear
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercuryDiLinear(Multiplicity) [function, symbol(checkMercuryDiLinear)]
  rule checkMercuryDiLinear(linear) => true
  rule checkMercuryDiLinear(many)   => false
  rule checkMercuryDiLinear(affine) => false

  // ---------------------------------------------------------------
  // Property 5b: checkMercuryInMany
  //
  // A Mercury "in" (normal input) mode argument must have Many
  // multiplicity.
  //
  // Mirrors: modeToMult ModeIn = Many
  // ---------------------------------------------------------------

  syntax Bool ::= checkMercuryInMany(Multiplicity) [function, symbol(checkMercuryInMany)]
  rule checkMercuryInMany(many)   => true
  rule checkMercuryInMany(linear) => false
  rule checkMercuryInMany(affine) => false

endmodule

// =================================================================
// Module: KOKA-BRIDGE-PROPERTIES
//
// Properties for KokaBridge/CoreTranslate.hs:
//   1. Effect rows are preserved (bijection)
//   2. All multiplicities default to Many
//   3. Type synonyms carry their expansion
// =================================================================

module KOKA-BRIDGE-PROPERTIES
  imports BRIDGE-HELPERS

  // ---------------------------------------------------------------
  // Property 1: checkKokaEffectPreserved
  //
  // The Koka bridge preserves effect rows — if the Koka source has
  // an effect row with N effects, the OrganIR output must also
  // have exactly N effects.
  //
  // This is a weak structural check: verify that the translated
  // effect row has the expected length.
  //
  // Mirrors KokaBridge/CoreTranslate.hs translateEffect:
  //   labels -> foldr EffectRowExtend tailRow labels
  //
  // Input: translated EffectRow, expected number of effects
  // Returns: true iff the row has exactly the expected length
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaEffectLength(EffectRow, Int) [function, symbol(checkKokaEffectLength)]
  rule checkKokaEffectLength(Eff, N) => effectRowLength(Eff) ==Int N

  // ---------------------------------------------------------------
  // Property 1b: checkKokaEffectBijection
  //
  // Stronger property: given source effect names and a translated
  // EffectRow, verify all effects appear.
  //
  // Input: list of QNames expected, the translated EffectRow
  // Returns: true iff all expected effects are present
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaEffectsPresent(List, EffectRow) [function, symbol(checkKokaEffectsPresent)]
  rule checkKokaEffectsPresent(.List, _) => true
  rule checkKokaEffectsPresent(ListItem(QN:QName) Rest:List, Eff)
    => effectRowContains(Eff, QN) andBool checkKokaEffectsPresent(Rest, Eff)

  // ---------------------------------------------------------------
  // Property 1c: checkKokaPureIsEmpty
  //
  // Koka total/pure effect (typeTotal) maps to EffectRowEmpty.
  //
  // Mirrors: isEffectEmpty ... "()" or "<>" -> EffectRowEmpty
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaPureIsEmpty(EffectRow) [function, symbol(checkKokaPureIsEmpty)]
  rule checkKokaPureIsEmpty(effectEmpty) => true
  rule checkKokaPureIsEmpty(_)           => false [owise]

  // ---------------------------------------------------------------
  // Property 2: checkKokaAllMulMany
  //
  // All multiplicities in Koka-bridge output default to Many.
  // Perceus re-derives the actual multiplicities later.
  //
  // Mirrors KokaBridge/CoreTranslate.hs translateType for TFun:
  //   pure (F.Many, ty')) args
  // And translateTypeVar:
  //   tvMultiplicity = F.Many
  //
  // Input: a TFun from Koka bridge output
  // Returns: true iff all MulType entries have Many
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaAllMulMany(Type) [function, symbol(checkKokaAllMulMany)]
  rule checkKokaAllMulMany(TFun(Args, _, _))
    => checkMulListAllManyK(Args)
  rule checkKokaAllMulMany(_) => true [owise]

  syntax Bool ::= checkMulListAllManyK(MulTypeList) [function, symbol(checkMulListAllManyK)]
  rule checkMulListAllManyK(.MulTypeList)                         => true
  rule checkMulListAllManyK(multype(many, _) , Rest:MulTypeList)  => checkMulListAllManyK(Rest)
  rule checkMulListAllManyK(multype(linear, _) , _)               => false
  rule checkMulListAllManyK(multype(affine, _) , _)               => false

  // ---------------------------------------------------------------
  // Property 2b: checkKokaTypeVarMany
  //
  // All type variables from the Koka bridge have Many multiplicity.
  //
  // Input: a TypeVar from Koka bridge output
  // Returns: true iff multiplicity is Many
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaTypeVarMany(TypeVar) [function, symbol(checkKokaTypeVarMany)]
  rule checkKokaTypeVarMany(typevar(_, _, many))   => true
  rule checkKokaTypeVarMany(typevar(_, _, linear)) => false
  rule checkKokaTypeVarMany(typevar(_, _, affine)) => false

  // ---------------------------------------------------------------
  // Property 3: checkKokaSynExpansion
  //
  // Koka type synonyms must carry their expansion in TSyn.
  // A TSyn has three components: QName, [Type] (args), Type (body).
  // The body (expansion) must not be a bare TSyn itself (it should
  // be fully expanded).
  //
  // Mirrors KokaBridge/CoreTranslate.hs translateType for TSyn:
  //   KT.TSyn syn args body -> F.TSyn (translateQName ...) args' body'
  //
  // Input: a Type from Koka bridge output
  // Returns: true iff if it's a TSyn, the body is not itself a TSyn
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaSynExpansion(Type) [function, symbol(checkKokaSynExpansion)]
  rule checkKokaSynExpansion(TSyn(_, _, TSyn(_, _, _))) => false  // nested TSyn = not expanded
  rule checkKokaSynExpansion(TSyn(_, _, _))             => true   [owise]
  rule checkKokaSynExpansion(_)                         => true   [owise]

  // ---------------------------------------------------------------
  // Property 3b: checkKokaSynHasBody
  //
  // A TSyn must actually have a body type (the expansion).
  // This is a structural sanity check.
  // ---------------------------------------------------------------

  syntax Bool ::= checkKokaSynHasBody(Type) [function, symbol(checkKokaSynHasBody)]
  rule checkKokaSynHasBody(TSyn(_, _, _)) => true
  rule checkKokaSynHasBody(_)             => false [owise]

  // --- Test helpers that construct TSyn terms (avoids .TypeList parse issue) ---
  syntax Type ::= testSynExpanded()      [function, symbol(testSynExpanded)]
  syntax Type ::= testSynNested()        [function, symbol(testSynNested)]
  syntax Type ::= testSynHasBody()       [function, symbol(testSynHasBody)]

  rule testSynExpanded()
    => TSyn(qname("std", name("string", 0)), .TypeList,
            TCon(typecon(qname("std", name("list", 0)), KValue)))
  rule testSynNested()
    => TSyn(qname("std", name("a", 0)), .TypeList,
            TSyn(qname("std", name("b", 0)), .TypeList,
                 TCon(typecon(qname("s", name("c", 0)), KValue))))
  rule testSynHasBody()
    => TSyn(qname("std", name("s", 0)), .TypeList,
            TCon(typecon(qname("std", name("int", 0)), KValue)))

endmodule

// =================================================================
// Module: BRIDGE-PROPERTIES — top-level module
// =================================================================

module BRIDGE-PROPERTIES
  imports ORGAN-IR
  imports BRIDGE-HELPERS
  imports GHC-BRIDGE-PROPERTIES
  imports RUST-BRIDGE-PROPERTIES
  imports MERCURY-BRIDGE-PROPERTIES
  imports KOKA-BRIDGE-PROPERTIES
endmodule
