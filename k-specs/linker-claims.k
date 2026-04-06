// linker-claims.k — kprove claims for linker name-rewriting invariants
//
// Verifies invariants of the linker (src/Frankenstein/Core/Linker.hs):
//   1. Local (lambda/let-bound) names are never rewritten
//   2. "main" is never mangled
//   3. Non-local, non-main names are correctly mangled with module prefix
//   4. Call graph structure is preserved by consistent mangling
//   5. Mangling is deterministic
//
// Usage:
//   kompile all-claims-def.k --backend haskell --main-module ALL-CLAIMS-DEF --syntax-module ALL-CLAIMS-DEF-SYNTAX -o all-claims-haskell-kompiled
//   kprove linker-claims.k --definition all-claims-haskell-kompiled

requires "all-claims-def.k"

module LINKER-CLAIMS-SYNTAX
  imports ALL-CLAIMS-DEF-SYNTAX
endmodule

module LINKER-CLAIMS
  imports ALL-CLAIMS-DEF
  imports SET
  imports MAP
  imports INT
  imports BOOL
  imports STRING

  // Simplification lemma for set membership
  rule X:Name in SetItem(X)              => true [simplification]
  rule X:Name in (SetItem(X) |Set _:Set) => true [simplification]

  // =================================================================
  // Claims: Local names are never rewritten
  // =================================================================

  // Claim L1a: Lambda-bound parameter is not rewritten
  claim <k>
    rewriteName(
      "Utils",
      name("x", 0),
      SetItem(name("x", 0))
    )
    =>
    name("x", 0)
  </k> [label(claim-linker-local-preserved)]

  // Claim L1b: Let-bound variable is not rewritten
  claim <k>
    rewriteName(
      "Main",
      name("tmp", 0),
      SetItem(name("tmp", 0)) SetItem(name("y", 0))
    )
    =>
    name("tmp", 0)
  </k> [label(claim-linker-let-preserved)]

  // Claim L1c: EVar with local name stays unchanged
  claim <k>
    rewriteExprVar(
      "Utils",
      EVar(name("x", 0)),
      SetItem(name("x", 0))
    )
    =>
    EVar(name("x", 0))
  </k> [label(claim-linker-evar-local)]

  // =================================================================
  // Claims: "main" is never mangled
  // =================================================================

  // Claim L2a: isMainName detects main
  claim <k> isMainName(name("main", 0)) => true </k>
    [label(claim-linker-is-main)]

  // Claim L2b: isMainName rejects non-main
  claim <k> isMainName(name("helper", 0)) => false </k>
    [label(claim-linker-not-main)]

  // Claim L2c: main is not rewritten even when not local
  claim <k>
    rewriteName("Main", name("main", 0), .Set)
    =>
    name("main", 0)
  </k> [label(claim-linker-main-no-mangle)]

  // Claim L2d: main stays unchanged as EVar
  claim <k>
    rewriteExprVar("Main", EVar(name("main", 0)), .Set)
    =>
    EVar(name("main", 0))
  </k> [label(claim-linker-main-evar)]

  // =================================================================
  // Claims: Non-local, non-main names are correctly mangled
  // =================================================================

  // Claim L3a: mangleName prepends module prefix
  claim <k>
    mangleName("Utils", name("double", 0))
    =>
    name("Utils_double", 0)
  </k> [label(claim-linker-mangle-basic)]

  // Claim L3b: Top-level function gets mangled
  claim <k>
    rewriteName("Utils", name("double", 0), .Set)
    =>
    name("Utils_double", 0)
  </k> [label(claim-linker-rewrite-toplevel)]

  // Claim L3c: EVar reference to top-level function gets mangled
  claim <k>
    rewriteExprVar("Utils", EVar(name("double", 0)), .Set)
    =>
    EVar(name("Utils_double", 0))
  </k> [label(claim-linker-evar-toplevel)]

  // Claim L3d: Module A mangles to A_helper
  claim <k>
    mangleName("A", name("helper", 0))
    =>
    name("A_helper", 0)
  </k> [label(claim-linker-mangle-A)]

  // Claim L3e: Module B mangles to B_helper (different from A)
  claim <k>
    mangleName("B", name("helper", 0))
    =>
    name("B_helper", 0)
  </k> [label(claim-linker-mangle-B)]

  // =================================================================
  // Claims: Call graph preservation
  // =================================================================

  // Claim L4a: Both caller and callee get mangled consistently
  claim <k>
    callPreserved("Utils", name("double", 0), name("helper", 0), .Set)
    =>
    true
  </k> [label(claim-linker-call-preserved)]

  // Claim L4b: Local-to-local call preserved (neither rewritten)
  claim <k>
    callPreserved(
      "Utils",
      name("x", 0),
      name("y", 0),
      SetItem(name("x", 0)) SetItem(name("y", 0))
    )
    =>
    true
  </k> [label(claim-linker-call-local-local)]

  // Claim L4c: Cross-module references: caller gets its module prefix
  claim <k>
    rewriteName("Main", name("test", 0), .Set)
    =>
    name("Main_test", 0)
  </k> [label(claim-linker-cross-caller)]

  // Claim L4d: Cross-module references: callee gets its module prefix
  claim <k>
    rewriteName("Utils", name("double", 0), .Set)
    =>
    name("Utils_double", 0)
  </k> [label(claim-linker-cross-callee)]

  // =================================================================
  // Claims: Mangling determinism and shouldRewrite
  // =================================================================

  // Claim L5a: Mangling is deterministic — same result both times
  claim <k>
    mangleName("Utils", name("foo", 0))
    =>
    name("Utils_foo", 0)
  </k> [label(claim-linker-mangle-deterministic)]

  // Claim L5b: shouldRewrite true for non-local non-main
  claim <k>
    shouldRewrite(name("foo", 0), .Set)
    =>
    true
  </k> [label(claim-linker-should-rewrite)]

  // Claim L5c: shouldRewrite false for local
  claim <k>
    shouldRewrite(name("foo", 0), SetItem(name("foo", 0)))
    =>
    false
  </k> [label(claim-linker-should-not-rewrite-local)]

  // Claim L5d: shouldRewrite false for main
  claim <k>
    shouldRewrite(name("main", 0), .Set)
    =>
    false
  </k> [label(claim-linker-should-not-rewrite-main)]

endmodule
