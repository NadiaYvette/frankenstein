// perceus-claims.k — K claims for verifying Perceus reference counting properties
//
// These claims are intended for kprove (Haskell backend).
// They verify the core invariants of the Perceus transformation
// defined in organ-ir.k (module PERCEUS).
//
// Usage:
//   kompile organ-ir.k --backend haskell --main-module ORGAN-IR --syntax-module ORGAN-IR-SYNTAX -o organ-ir-haskell-kompiled
//   kprove perceus-claims.k --definition organ-ir-haskell-kompiled
//
// To run only concrete claims (skipping symbolic ones):
//   kprove perceus-claims.k --definition organ-ir-haskell-kompiled \
//     --exclude claim-1a,claim-1b,claim-1c,claim-1d,claim-1e

requires "organ-ir.k"

module PERCEUS-CLAIMS-SYNTAX
  imports ORGAN-IR-SYNTAX
endmodule

module PERCEUS-CLAIMS
  imports ORGAN-IR
  imports SET
  imports MAP
  imports INT
  imports BOOL

  // Simplification lemmas: symbolic set membership for singleton sets.
  // The Haskell backend cannot prove `X in SetItem(X) => true` for
  // uninterpreted constructor terms without these hints.
  // K normalizes SetItem(X) to SetItem(X) |Set .Set internally.
  rule X:Name in SetItem(X)            => true [simplification]
  rule X:Name in (SetItem(X) |Set _:Set) => true [simplification]

  // =================================================================
  // Claim 1: Perceus preserves free variables (symbolic)
  //
  // These use universally quantified variables. They require the
  // simplification lemma above to resolve set membership.
  // =================================================================

  // Claim 1a: Perceus preserves free vars of EVar
  claim <k>
    isFree(X:Name, perceusS(_Scope:Map, EVar(X)))
    =>
    true
  </k> [label(claim-1a)]

  // Claim 1b: Perceus preserves free vars of EApp
  claim <k>
    isFree(N:Name, perceusS(_Scope:Map, EApp(EVar(N), .ExprList)))
    =>
    true
  </k> [label(claim-1b)]

  // Claim 1c: Perceus preserves free vars through ERetain
  claim <k>
    isFree(N:Name, perceusS(_Scope:Map, ERetain(EVar(N))))
    =>
    true
  </k> [label(claim-1c)]

  // Claim 1d: Perceus preserves free vars through EDrop
  claim <k>
    isFree(N:Name, perceusS(_Scope:Map, EDrop(EVar(N))))
    =>
    true
  </k> [label(claim-1d)]

  // Claim 1e: Perceus on literals has no free vars
  claim <k>
    freeVars(perceusS(_Scope:Map, ELit(_L:Lit)))
    =>
    .Set
  </k> [label(claim-1e)]

  // =================================================================
  // Claim 2: Unused non-linear bindings get exactly one drop (concrete)
  // =================================================================

  // Claim 2a: Unused many-multiplicity variable in unusedNonLinear
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)),
      name("x", 0) |-> many,
      ELit(litInt(7))
    )
    =>
    SetItem(name("x", 0))
  </k> [label(claim-2a)]

  // Claim 2b: Same for affine multiplicity
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)),
      name("x", 0) |-> affine,
      ELit(litInt(7))
    )
    =>
    SetItem(name("x", 0))
  </k> [label(claim-2b)]

  // Claim 2c: wrapDrops inserts EDrop for a single name
  claim <k>
    wrapDrops(SetItem(name("x", 0)), ELit(litInt(7)))
    =>
    ELet(
      (bind(name("_drop", 0), perceusUnitType(), EDrop(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
      ELit(litInt(7))
    )
  </k> [label(claim-2c)]

  // Claim 2d: Full Perceus on unused let binding inserts drop
  claim <k>
    perceusS(
      .Map,
      ELet(
        (bind(name("x", 0),
              TCon(typecon(qname("s", name("i", 0)), KValue)),
              ELit(litInt(42)), defVal) , .BindGroup) ;; .BGList,
        ELit(litInt(7))
      )
    )
    =>
    ELet(
      (bind(name("x", 0),
            TCon(typecon(qname("s", name("i", 0)), KValue)),
            ELit(litInt(42)), defVal) , .BindGroup) ;; .BGList,
      ELet(
        (bind(name("_drop", 0), perceusUnitType(), EDrop(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
        ELit(litInt(7))
      )
    )
  </k> [label(claim-2d)]

  // =================================================================
  // Claim 3: Multi-use many variables get (n-1) retains (concrete)
  // =================================================================

  // Claim 3a: wrapRetains count=2 → 1 retain
  claim <k>
    wrapRetains(name("x", 0), 2, ELit(litInt(99)))
    =>
    ELet(
      (bind(name("_retain", 0), perceusUnitType(), ERetain(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
      ELit(litInt(99))
    )
  </k> [label(claim-3a)]

  // Claim 3b: wrapRetains count=3 → 2 retains
  claim <k>
    wrapRetains(name("x", 0), 3, ELit(litInt(99)))
    =>
    ELet(
      (bind(name("_retain", 0), perceusUnitType(), ERetain(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
      ELet(
        (bind(name("_retain", 0), perceusUnitType(), ERetain(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
        ELit(litInt(99))
      )
    )
  </k> [label(claim-3b)]

  // Claim 3c: wrapRetains count=1 → no retain
  claim <k>
    wrapRetains(name("x", 0), 1, ELit(litInt(99)))
    =>
    ELit(litInt(99))
  </k> [label(claim-3c)]

  // Claim 3d: wrapRetains count=0 → no retain
  claim <k>
    wrapRetains(name("x", 0), 0, ELit(litInt(99)))
    =>
    ELit(litInt(99))
  </k> [label(claim-3d)]

  // Claim 3e: Full Perceus on double-use inserts 1 retain
  claim <k>
    perceusS(
      .Map,
      ELet(
        (bind(name("x", 0),
              TCon(typecon(qname("s", name("i", 0)), KValue)),
              ELit(litInt(42)), defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("x", 0)), EVar(name("x", 0)) , .ExprList)
      )
    )
    =>
    ELet(
      (bind(name("x", 0),
            TCon(typecon(qname("s", name("i", 0)), KValue)),
            ELit(litInt(42)), defVal) , .BindGroup) ;; .BGList,
      ELet(
        (bind(name("_retain", 0), perceusUnitType(), ERetain(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("x", 0)), EVar(name("x", 0)) , .ExprList)
      )
    )
  </k> [label(claim-3e)]

  // =================================================================
  // Claim 4: Linear variables never get drop or retain (concrete)
  // =================================================================

  // Claim 4a: Linear unused variable NOT in unusedNonLinear
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)),
      name("x", 0) |-> linear,
      ELit(litInt(7))
    )
    =>
    .Set
  </k> [label(claim-4a)]

  // Claim 4b: unusedNonLinearHelper with linear → empty
  claim <k>
    unusedNonLinearHelper(
      ListItem(name("x", 0)) .List,
      name("x", 0) |-> linear,
      .Set
    )
    =>
    .Set
  </k> [label(claim-4b)]

  // Claim 4c: isLinear correctly identifies multiplicities
  claim <k> isLinear(linear) => true </k> [label(claim-4c-linear)]
  claim <k> isLinear(many)   => false </k> [label(claim-4c-many)]
  claim <k> isLinear(affine) => false </k> [label(claim-4c-affine)]

  // Claim 4d: Mixed scope — linear excluded, many included
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)) SetItem(name("y", 0)),
      (name("x", 0) |-> linear) (name("y", 0) |-> many),
      ELit(litInt(7))
    )
    =>
    SetItem(name("y", 0))
  </k> [label(claim-4d)]

endmodule
