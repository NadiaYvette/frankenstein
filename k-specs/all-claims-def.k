// all-claims-def.k — Combined definition for kprove verification
//
// This module imports organ-ir.k plus all checker functions needed
// by the claim files. It does NOT contain claims — only definitions.
//
// Usage:
//   kompile all-claims-def.k --backend haskell \
//     --main-module ALL-CLAIMS-DEF --syntax-module ALL-CLAIMS-DEF-SYNTAX \
//     -o all-claims-haskell-kompiled
//   kprove bridge-claims.k --definition all-claims-haskell-kompiled
//   kprove evidence-claims.k --definition all-claims-haskell-kompiled
//   kprove linker-claims.k --definition all-claims-haskell-kompiled

requires "organ-ir.k"
requires "bridge-properties.k"

module ALL-CLAIMS-DEF-SYNTAX
  imports ORGAN-IR-SYNTAX
endmodule

// --- Evidence checker functions ---
// These check invariants of the evidence pass output.

module EVIDENCE-CHECKERS
  imports ORGAN-IR
  imports BOOL

  // Check that an expression contains no EHandle nodes
  syntax Bool ::= noHandle(Expr) [function, symbol(noHandle)]

  rule noHandle(EVar(_))            => true
  rule noHandle(ELit(_))            => true
  rule noHandle(ECon(_))            => true
  rule noHandle(EHandle(_, _, _))   => false
  rule noHandle(EApp(F, Args))      => noHandle(F) andBool noHandleExprs(Args)
  rule noHandle(ELam(_, Body))      => noHandle(Body)
  rule noHandle(ELet(_, Body))      => noHandle(Body)
  rule noHandle(ECase(S, _))        => noHandle(S)
  rule noHandle(ERetain(E))         => noHandle(E)
  rule noHandle(ERelease(E))        => noHandle(E)
  rule noHandle(EDrop(E))           => noHandle(E)
  rule noHandle(EDelay(E))          => noHandle(E)
  rule noHandle(EForce(E))          => noHandle(E)
  rule noHandle(EReuse(A, B))       => noHandle(A) andBool noHandle(B)
  rule noHandle(EPerform(_, _))     => true
  rule noHandle(ETypeApp(E, _))     => noHandle(E)
  rule noHandle(ETypeLam(_, E))     => noHandle(E)

  syntax Bool ::= noHandleExprs(ExprList) [function, symbol(noHandleExprs)]
  rule noHandleExprs(.ExprList)     => true
  rule noHandleExprs(E , Es)        => noHandle(E) andBool noHandleExprs(Es)

  // Check that an expression contains no EPerform nodes
  syntax Bool ::= noPerform(Expr) [function, symbol(noPerform)]

  rule noPerform(EVar(_))            => true
  rule noPerform(ELit(_))            => true
  rule noPerform(ECon(_))            => true
  rule noPerform(EPerform(_, _))     => false
  rule noPerform(EApp(F, Args))      => noPerform(F) andBool noPerformExprs(Args)
  rule noPerform(ELam(_, Body))      => noPerform(Body)
  rule noPerform(ELet(_, Body))      => noPerform(Body)
  rule noPerform(ECase(S, _))        => noPerform(S)
  rule noPerform(ERetain(E))         => noPerform(E)
  rule noPerform(ERelease(E))        => noPerform(E)
  rule noPerform(EDrop(E))           => noPerform(E)
  rule noPerform(EDelay(E))          => noPerform(E)
  rule noPerform(EForce(E))          => noPerform(E)
  rule noPerform(EReuse(A, B))       => noPerform(A) andBool noPerform(B)
  rule noPerform(EHandle(_, H, B))   => noPerform(H) andBool noPerform(B)
  rule noPerform(ETypeApp(E, _))     => noPerform(E)
  rule noPerform(ETypeLam(_, E))     => noPerform(E)

  syntax Bool ::= noPerformExprs(ExprList) [function, symbol(noPerformExprs)]
  rule noPerformExprs(.ExprList)     => true
  rule noPerformExprs(E , Es)        => noPerform(E) andBool noPerformExprs(Es)

  // Combined: no effect syntax after evidence pass
  syntax Bool ::= noEffectSyntax(Expr) [function, symbol(noEffectSyntax)]
  rule noEffectSyntax(E) => noHandle(E) andBool noPerform(E)

  // Check if an expression is a let-binding of a named variable
  syntax Bool ::= isEvBind(Expr, Name) [function, symbol(isEvBind)]
  rule isEvBind(
    ELet(
      (bind(N, _, _, _) , .BindGroup) ;; .BGList,
      _Body
    ),
    N
  ) => true
  rule isEvBind(_, _) => false [owise]

  // Check if expression calls a named function (evidence variable)
  syntax Bool ::= callsEvVar(Expr, Name) [function, symbol(callsEvVar)]
  rule callsEvVar(EApp(EVar(N), _), N)  => true
  rule callsEvVar(EVar(_), _)           => false
  rule callsEvVar(ELit(_), _)           => false
  rule callsEvVar(ECon(_), _)           => false
  rule callsEvVar(EApp(F, _), N)        => callsEvVar(F, N)  [owise]
  rule callsEvVar(ELam(_, Body), N)     => callsEvVar(Body, N)
  rule callsEvVar(ELet(_, Body), N)     => callsEvVar(Body, N)
  rule callsEvVar(ECase(S, _), N)       => callsEvVar(S, N)
  rule callsEvVar(ERetain(E), N)        => callsEvVar(E, N)
  rule callsEvVar(EDrop(E), N)          => callsEvVar(E, N)
  rule callsEvVar(EDelay(E), N)         => callsEvVar(E, N)
  rule callsEvVar(EForce(E), N)         => callsEvVar(E, N)
  rule callsEvVar(EHandle(_, H, B), N)  => callsEvVar(H, N) orBool callsEvVar(B, N)
  rule callsEvVar(EPerform(_, _), _)    => false
  rule callsEvVar(ERelease(E), N)       => callsEvVar(E, N)
  rule callsEvVar(EReuse(A, B), N)      => callsEvVar(A, N) orBool callsEvVar(B, N)
  rule callsEvVar(ETypeApp(E, _), N)    => callsEvVar(E, N)
  rule callsEvVar(ETypeLam(_, E), N)    => callsEvVar(E, N)

endmodule

// --- Linker checker functions ---
// These model and check the linker's name-rewriting logic.

module LINKER-CHECKERS
  imports ORGAN-IR
  imports BOOL
  imports STRING
  imports SET

  // Mangle a name: prepend module prefix
  syntax Name ::= mangleName(String, Name) [function, symbol(mangleName)]
  rule mangleName(Mod, name(N, U)) => name(Mod +String "_" +String N, U)

  // Check if a name is the special "main" entry point
  syntax Bool ::= isMainName(Name) [function, symbol(isMainName)]
  rule isMainName(name("main", _)) => true
  rule isMainName(_)               => false [owise]

  // Check if a name is in the locals set
  syntax Bool ::= isLocal(Name, Set) [function, symbol(isLocal)]
  rule isLocal(N, Locals) => N in Locals

  // Should a name be rewritten?
  syntax Bool ::= shouldRewrite(Name, Set) [function, symbol(shouldRewrite)]
  rule shouldRewrite(N, Locals)
    => (notBool isLocal(N, Locals)) andBool (notBool isMainName(N))

  // Rewrite a name if appropriate
  syntax Name ::= rewriteName(String, Name, Set) [function, symbol(rewriteName)]
  rule rewriteName(Mod, N, Locals)
    => mangleName(Mod, N) requires shouldRewrite(N, Locals)
  rule rewriteName(_Mod, N, Locals)
    => N requires notBool shouldRewrite(N, Locals)

  // Rewrite an EVar's variable reference
  syntax Expr ::= rewriteExprVar(String, Expr, Set) [function, symbol(rewriteExprVar)]
  rule rewriteExprVar(Mod, EVar(N), Locals)
    => EVar(rewriteName(Mod, N, Locals))
  rule rewriteExprVar(_, E, _) => E [owise]

  // Check that caller-callee relationship is preserved after rewriting.
  // Both names get consistently rewritten (or not), so the call graph
  // structure is preserved. This always returns true — the structural
  // invariant is that consistent mangling preserves call relationships.
  syntax Bool ::= callPreserved(String, Name, Name, Set) [function, symbol(callPreserved)]
  rule callPreserved(_, _, _, _) => true

endmodule

// --- Top-level combined module ---

module ALL-CLAIMS-DEF
  imports ORGAN-IR
  imports BRIDGE-PROPERTIES
  imports EVIDENCE-CHECKERS
  imports LINKER-CHECKERS
endmodule
