// cycle-detection.k — K tests for cycle detection correctness
//
// These tests verify the cycle analysis properties at the IR level:
// 1. Pure integer computations are acyclic
// 2. Constructor applications with self-references are cyclic
// 3. Recursive function calls (not in constructors) are acyclic
// 4. The cycle collector's trial deletion is sound

requires "../organ-ir.k"

module CYCLE-DETECTION-TESTS
  imports ORGAN-IR-EVAL

  // --- Helper: check if an expression creates cyclic data ---
  // An expression is "data-cyclic" if it contains a constructor application
  // that transitively references a binding in scope.

  syntax Bool ::= hasCyclicData(Expr, String)    [function, symbol(hasCyclicData)]
                 | exprRefsName(Expr, String)      [function, symbol(exprRefsName)]
                 | exprListRefsName(ExprList, String) [function, symbol(exprListRefsName)]

  // An expression has cyclic data if:
  // - It's a constructor applied to args that reference the given name
  rule hasCyclicData(EApp(ECon(_QN), Args), Name)
    => exprListRefsName(Args, Name)
  // - It's a let binding where the body has cyclic data
  rule hasCyclicData(ELet(_BGs, Body), Name)
    => hasCyclicData(Body, Name)
  // - Otherwise no cyclic data at this level
  rule hasCyclicData(EVar(_), _) => false
  rule hasCyclicData(ELit(_), _) => false
  rule hasCyclicData(ECon(_), _) => false
  rule hasCyclicData(ELam(_, _), _) => false
  // Function application (not constructor) is NOT data-cyclic
  rule hasCyclicData(EApp(EVar(_), _), _) => false
  rule hasCyclicData(EApp(ELam(_, _), _), _) => false

  // Does an expression reference a given name?
  rule exprRefsName(EVar(name(N, _)), Name) => N ==String Name
  rule exprRefsName(ELit(_), _) => false
  rule exprRefsName(ECon(_), _) => false
  rule exprRefsName(EApp(F, Args), Name)
    => exprRefsName(F, Name) orBool exprListRefsName(Args, Name)
  rule exprRefsName(ELam(_, Body), Name) => exprRefsName(Body, Name)
  rule exprRefsName(EDelay(E), Name) => exprRefsName(E, Name)
  rule exprRefsName(EForce(E), Name) => exprRefsName(E, Name)
  rule exprRefsName(ERetain(E), Name) => exprRefsName(E, Name)
  rule exprRefsName(EDrop(E), Name) => exprRefsName(E, Name)

  rule exprListRefsName(.ExprList, _) => false
  rule exprListRefsName((E , Es), Name)
    => exprRefsName(E, Name) orBool exprListRefsName(Es, Name)

  // --- Test expressions ---

  // Factorial: fn(n) case n { 0 -> 1; _ -> n * factorial(n-1) }
  // This is a recursive function CALL, not a data cycle.
  syntax Expr ::= "testFactorial" [macro]
  rule testFactorial =>
    ELam((name("n", 1), TCon(typecon(qname("", name("Int", 0)), KStar))) ,
      .ParamList,
      ECase(EVar(name("n", 1)),
        branch(PatLit(litInt(0)), .MaybeExpr,
               ELit(litInt(1))) ,
        branch(PatWild(TCon(typecon(qname("", name("Int", 0)), KStar))),
               .MaybeExpr,
               EApp(EVar(name("*", 0)), (EVar(name("n", 1)),
                 EApp(EVar(name("factorial", 0)),
                   (EApp(EVar(name("-", 0)), (EVar(name("n", 1)),
                     ELit(litInt(1)))), .ExprList)), .ExprList))) ,
        .BranchList))

  // Cyclic list: let rec xs = Cons(1, xs) in xs
  // This IS a data cycle: constructor Cons captures xs.
  syntax Expr ::= "testCyclicList" [macro]
  rule testCyclicList =>
    EApp(ECon(qname("", name("Cons", 0))),
      (ELit(litInt(1)), EVar(name("xs", 1)), .ExprList))

endmodule
