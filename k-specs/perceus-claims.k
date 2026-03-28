// perceus-claims.k — K claims for verifying Perceus reference counting properties
//
// These claims are intended for kprove (Haskell backend).
// They verify the core invariants of the Perceus transformation
// defined in organ-ir.k (module PERCEUS).
//
// Usage:
//   kompile organ-ir.k --backend haskell -d organ-ir-haskell-kompiled
//   kprove perceus-claims.k --definition organ-ir-haskell-kompiled
//
// Note: kprove requires universally quantified variables (symbolic).
// K variables that appear in claims are implicitly universally quantified.

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

  // =================================================================
  // Claim 1: Perceus preserves free variables
  //
  // For any scope and expression, the free variables of the original
  // expression are a subset of the free variables of the transformed
  // expression. Perceus may ADD names (like _drop, _retain bindings)
  // but never REMOVES an original free variable reference.
  //
  // Formally: freeVars(E) is a subset of freeVars(perceusS(Scope, E))
  //
  // We express this as: for any name N free in E, N is also free in
  // perceusS(Scope, E).
  //
  // We verify this for several representative expression forms since
  // K's claim mechanism works best with concrete structure.
  // =================================================================

  // Claim 1a: Perceus preserves free vars of EVar
  //
  // freeVars(perceusS(Scope, EVar(X))) contains X
  claim <k>
    isFree(X:Name, perceusS(Scope:Map, EVar(X)))
    =>
    true
  </k>

  // Claim 1b: Perceus preserves free vars of EApp
  //
  // If N is free in EApp(F, E1), then N is free in perceusS(Scope, EApp(F, E1))
  claim <k>
    isFree(N:Name, perceusS(Scope:Map, EApp(EVar(N), .ExprList)))
    =>
    true
  </k>

  // Claim 1c: Perceus preserves free vars through ERetain
  //
  // If N is free in E, then N is free in perceusS(Scope, ERetain(E))
  // (since perceusS just recurses through ERetain)
  claim <k>
    isFree(N:Name, perceusS(Scope:Map, ERetain(EVar(N))))
    =>
    true
  </k>

  // Claim 1d: Perceus preserves free vars through EDrop
  claim <k>
    isFree(N:Name, perceusS(Scope:Map, EDrop(EVar(N))))
    =>
    true
  </k>

  // Claim 1e: Perceus on literals has no free vars (vacuously preserved)
  claim <k>
    freeVars(perceusS(Scope:Map, ELit(L:Lit)))
    =>
    .Set
  </k>

  // =================================================================
  // Claim 2: Unused non-linear bindings get exactly one drop
  //
  // For ELet(bind(X, T, Rhs), Body) where X is not free in Body and
  // the multiplicity inferred from T is many (the default), the
  // Perceus output contains EDrop(EVar(X)).
  //
  // We instantiate with a concrete unused binding:
  //   let x = 42 in 7    (x not free in 7, mul = many by default)
  // =================================================================

  // Claim 2a: An unused many-multiplicity variable appears in unusedNonLinear
  //
  // If X is not free in Body and lookupMul(X, Scope) is not linear,
  // then X is in unusedNonLinear({X}, Scope, Body).
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)),
      name("x", 0) |-> many,
      ELit(litInt(7))
    )
    =>
    SetItem(name("x", 0))
  </k>

  // Claim 2b: Same for affine multiplicity
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)),
      name("x", 0) |-> affine,
      ELit(litInt(7))
    )
    =>
    SetItem(name("x", 0))
  </k>

  // Claim 2c: wrapDrops inserts EDrop for a single name
  //
  // wrapDrops({x}, body) produces ELet(bind(_drop, ..., EDrop(EVar(x)), ...), body)
  claim <k>
    wrapDrops(SetItem(name("x", 0)), ELit(litInt(7)))
    =>
    ELet(
      (bind(name("_drop", 0), perceusUnitType(), EDrop(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
      ELit(litInt(7))
    )
  </k>

  // Claim 2d: Full Perceus on unused let binding inserts drop
  //
  // perceus(let x = 42 in 7)  contains an EDrop(EVar(x))
  // We verify the exact output shape.
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
  </k>

  // =================================================================
  // Claim 3: Multi-use many variables get (n-1) retains
  //
  // For ELet(bind(X, T, Rhs), Body) where usageCount(X, Body) = n > 1
  // and multiplicity is many, the output wraps Body with (n-1) ERetain
  // bindings.
  //
  // Test case: let x = 42 in app(x, x)   -- usage = 2, so 1 retain
  // =================================================================

  // Claim 3a: wrapRetains with count=2 produces exactly 1 retain binding
  claim <k>
    wrapRetains(name("x", 0), 2, ELit(litInt(99)))
    =>
    ELet(
      (bind(name("_retain", 0), perceusUnitType(), ERetain(EVar(name("x", 0))), defVal) , .BindGroup) ;; .BGList,
      ELit(litInt(99))
    )
  </k>

  // Claim 3b: wrapRetains with count=3 produces exactly 2 retain bindings
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
  </k>

  // Claim 3c: wrapRetains with count=1 produces no retain (identity)
  claim <k>
    wrapRetains(name("x", 0), 1, ELit(litInt(99)))
    =>
    ELit(litInt(99))
  </k>

  // Claim 3d: wrapRetains with count=0 produces no retain (identity)
  claim <k>
    wrapRetains(name("x", 0), 0, ELit(litInt(99)))
    =>
    ELit(litInt(99))
  </k>

  // Claim 3e: Full Perceus on double-use binding inserts 1 retain
  //
  // perceus(let x = 42 in app(x, x)) should produce:
  //   let x = 42 in let _retain = retain(x) in app(x, x)
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
  </k>

  // =================================================================
  // Claim 4: Linear variables never get drop or retain
  //
  // For a variable with linear multiplicity in scope, Perceus does
  // NOT insert EDrop or ERetain for it, because:
  //   - unusedNonLinear filters out linear variables (isLinear check)
  //   - wrapRetains only fires for multi-use, but linear vars must be
  //     used exactly once
  //
  // We verify the key sub-property: unusedNonLinear returns empty set
  // for linear variables even when they are unused.
  // =================================================================

  // Claim 4a: Linear unused variable is NOT in unusedNonLinear result
  //
  // Even though x is not free in ELit(7), if x is linear in scope,
  // unusedNonLinear returns empty (no drop for linear).
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)),
      name("x", 0) |-> linear,
      ELit(litInt(7))
    )
    =>
    .Set
  </k>

  // Claim 4b: Perceus on let with linear unused binding -- no drop inserted
  //
  // let x_linear = 42 in 7  where x has linear multiplicity
  // We construct a type that lookupMulFromType maps to linear.
  // Since lookupMulFromType defaults to many for non-TFun types,
  // and the scope is what matters for unusedNonLinear, we test
  // with an explicit scope that has x as linear.
  //
  // perceusS({x |-> linear}, ELam(.ParamList, ELit(litInt(7))))
  // with x not appearing -- x is in scope as linear, but since
  // lambda has no params binding x, x from outer scope is not
  // dropped (Perceus only drops params/bindings it introduces).
  //
  // More directly: verify that for a lambda param with linear
  // multiplicity, no drop is inserted even if unused.
  // We test the helper directly.
  claim <k>
    unusedNonLinearHelper(
      ListItem(name("x", 0)) .List,
      name("x", 0) |-> linear,
      .Set
    )
    =>
    .Set
  </k>

  // Claim 4c: isLinear correctly identifies multiplicities
  claim <k> isLinear(linear) => true </k>
  claim <k> isLinear(many)   => false </k>
  claim <k> isLinear(affine) => false </k>

  // Claim 4d: Mixed scope -- linear var excluded, many var included
  //
  // With {x |-> linear, y |-> many} and neither x nor y free in body,
  // only y appears in unusedNonLinear.
  claim <k>
    unusedNonLinear(
      SetItem(name("x", 0)) SetItem(name("y", 0)),
      (name("x", 0) |-> linear) (name("y", 0) |-> many),
      ELit(litInt(7))
    )
    =>
    SetItem(name("y", 0))
  </k>

endmodule
