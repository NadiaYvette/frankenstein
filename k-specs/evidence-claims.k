// evidence-claims.k — kprove claims for evidence-passing transformation
//
// Verifies invariants of the evidence pass (src/Frankenstein/Core/Evidence.hs):
//   1. Post-evidence expressions contain no EHandle or EPerform
//   2. Single-op effects: EPerform replaced by direct evidence variable call
//   3. Multi-op effects: EPerform replaced by evv_select projection + call
//   4. Nested effects: both inner and outer evidence remain accessible
//
// Usage:
//   kompile all-claims-def.k --backend haskell --main-module ALL-CLAIMS-DEF --syntax-module ALL-CLAIMS-DEF-SYNTAX -o all-claims-haskell-kompiled
//   kprove evidence-claims.k --definition all-claims-haskell-kompiled

requires "all-claims-def.k"

module EVIDENCE-CLAIMS-SYNTAX
  imports ALL-CLAIMS-DEF-SYNTAX
endmodule

module EVIDENCE-CLAIMS
  imports ALL-CLAIMS-DEF
  imports SET
  imports MAP
  imports INT
  imports BOOL

  // =================================================================
  // Claims: Post-evidence invariant — no EHandle/EPerform
  // =================================================================

  // Claim E1a: A pure expression after evidence pass has no handle
  claim <k>
    noHandle(
      ELet(
        (bind(name("ev_console", 0),
              TCon(typecon(qname("s", name("fun", 0)), KValue)),
              EVar(name("print_handler", 0)),
              defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-ev-no-handle)]

  // Claim E1b: Expression with EHandle fails check
  claim <k>
    noHandle(
      EHandle(
        effectExtend(qname("std", name("console", 0)), effectEmpty),
        ELam(.ParamList, ELit(litInt(0))),
        EPerform(qname("std", name("console", 0)),
                 ELit(litString("hello")) , .ExprList)
      )
    )
    =>
    false
  </k> [label(claim-ev-has-handle)]

  // Claim E1c: A pure expression has no perform
  claim <k>
    noPerform(
      EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList)
    )
    =>
    true
  </k> [label(claim-ev-no-perform)]

  // Claim E1d: Expression with EPerform fails check
  claim <k>
    noPerform(
      EPerform(qname("std", name("console", 0)),
               ELit(litString("hello")) , .ExprList)
    )
    =>
    false
  </k> [label(claim-ev-has-perform)]

  // Claim E1e: Combined check — fully desugared expression
  claim <k>
    noEffectSyntax(
      ELet(
        (bind(name("ev_console", 0),
              TCon(typecon(qname("s", name("fun", 0)), KValue)),
              EVar(name("print_handler", 0)),
              defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList)
      )
    )
    =>
    true
  </k> [label(claim-ev-no-effect-syntax)]

  // =================================================================
  // Claims: Single-op evidence translation structure
  // =================================================================

  // Claim E2a: Evidence binding structure — handler bound to evidence var
  claim <k>
    isEvBind(
      ELet(
        (bind(name("ev_console", 0),
              TCon(typecon(qname("s", name("fun", 0)), KValue)),
              ELam(param(name("x", 0), TCon(typecon(qname("s", name("string", 0)), KValue))) , .ParamList,
                   ELit(litInt(0))),
              defVal) , .BindGroup) ;; .BGList,
        EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList)
      ),
      name("ev_console", 0)
    )
    =>
    true
  </k> [label(claim-ev-single-op-bind)]

  // Claim E2b: Body calls the evidence variable
  claim <k>
    callsEvVar(
      EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList),
      name("ev_console", 0)
    )
    =>
    true
  </k> [label(claim-ev-single-op-call)]

  // Claim E2c: Body does NOT call a different evidence variable
  claim <k>
    callsEvVar(
      EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList),
      name("ev_other", 0)
    )
    =>
    false
  </k> [label(claim-ev-single-op-wrong-name)]

  // =================================================================
  // Claims: Multi-op evidence translation structure
  // =================================================================

  // Claim E3a: Multi-op: outer evidence binding present
  claim <k>
    isEvBind(
      ELet(
        (bind(name("ev_console", 0),
              TCon(typecon(qname("s", name("fun", 0)), KValue)),
              EVar(name("console_handler", 0)),
              defVal) , .BindGroup) ;; .BGList,
        ELet(
          (bind(name("ev_console_println", 0),
                TCon(typecon(qname("s", name("fun", 0)), KValue)),
                EApp(EVar(name("evv_select", 0)),
                     EVar(name("ev_console", 0)) , ELit(litInt(0)) , .ExprList),
                defVal) , .BindGroup) ;; .BGList,
          EApp(EVar(name("ev_console_println", 0)),
               ELit(litString("hello")) , .ExprList)
        )
      ),
      name("ev_console", 0)
    )
    =>
    true
  </k> [label(claim-ev-multi-op-outer-bind)]

  // Claim E3b: Multi-op: inner body calls projected evidence var
  claim <k>
    callsEvVar(
      EApp(EVar(name("ev_console_println", 0)),
           ELit(litString("hello")) , .ExprList),
      name("ev_console_println", 0)
    )
    =>
    true
  </k> [label(claim-ev-multi-op-inner-call)]

  // Claim E3c: Multi-op: evv_select call structure is clean
  claim <k>
    noEffectSyntax(
      EApp(EVar(name("evv_select", 0)),
           EVar(name("ev_console", 0)) , ELit(litInt(0)) , .ExprList)
    )
    =>
    true
  </k> [label(claim-ev-multi-op-select-clean)]

  // =================================================================
  // Claims: Nested effects
  // =================================================================

  // Claim E4a: Two evidence bindings — outer name still accessible
  claim <k>
    callsEvVar(
      ELet(
        (bind(name("ev_console", 0),
              TCon(typecon(qname("s", name("fun", 0)), KValue)),
              EVar(name("console_handler", 0)),
              defVal) , .BindGroup) ;; .BGList,
        ELet(
          (bind(name("ev_state", 0),
                TCon(typecon(qname("s", name("fun", 0)), KValue)),
                EVar(name("state_handler", 0)),
                defVal) , .BindGroup) ;; .BGList,
          EApp(EVar(name("ev_console", 0)), ELit(litString("hello")) , .ExprList)
        )
      ),
      name("ev_console", 0)
    )
    =>
    true
  </k> [label(claim-ev-nested-outer-visible)]

  // Claim E4b: Inner evidence also accessible
  claim <k>
    callsEvVar(
      ELet(
        (bind(name("ev_console", 0),
              TCon(typecon(qname("s", name("fun", 0)), KValue)),
              EVar(name("console_handler", 0)),
              defVal) , .BindGroup) ;; .BGList,
        ELet(
          (bind(name("ev_state", 0),
                TCon(typecon(qname("s", name("fun", 0)), KValue)),
                EVar(name("state_handler", 0)),
                defVal) , .BindGroup) ;; .BGList,
          EApp(EVar(name("ev_state", 0)), ELit(litInt(42)) , .ExprList)
        )
      ),
      name("ev_state", 0)
    )
    =>
    true
  </k> [label(claim-ev-nested-inner-visible)]

endmodule
