// OrganIR — Full K specification
//
// Algebraic specification of Frankenstein's Core IR with:
// 1. Abstract syntax (mirrors src/Frankenstein/Core/Types.hs)
// 2. Typing rules
// 3. Perceus reference counting rewrite theory
//
// Usage:
//   kompile organ-ir.k --backend llvm
//   krun -cPGM='typeOf(ELit(litInt(42)))' --definition organ-ir-kompiled

// =================================================================
// Module 1: SYNTAX — sorts and productions for the IR
// =================================================================

module ORGAN-IR-SYNTAX
  imports INT-SYNTAX
  imports FLOAT-SYNTAX
  imports STRING-SYNTAX
  imports BOOL-SYNTAX

  // --- Names ---
  syntax Name  ::= name(String, Int)          [symbol(name)]
  syntax QName ::= qname(String, Name)        [symbol(qname)]
  syntax Visibility ::= "public" | "private"

  // --- Multiplicity ---
  syntax Multiplicity ::= "many"    [symbol(many)]
                         | "affine"  [symbol(affine)]
                         | "linear"  [symbol(linear)]

  // --- Kinds ---
  syntax Kind ::= "KValue"       [symbol(KValue)]
                 | "KEffect"      [symbol(KEffect)]
                 | "KEffectRow"   [symbol(KEffectRow)]
                 | "KHeap"        [symbol(KHeap)]
                 | "KStar"        [symbol(KStar)]
                 | KArrow(Kind, Kind) [symbol(KArrow)]

  // --- Effect rows ---
  syntax EffectRow ::= "effectEmpty"                    [symbol(effectEmpty)]
                      | effectExtend(QName, EffectRow)   [symbol(effectExtend)]
                      | effectVar(TypeVar)               [symbol(effectVar)]

  // --- Type variables and constructors ---
  syntax TypeVar ::= typevar(Name, Kind, Multiplicity)  [symbol(typevar)]
  syntax TypeCon ::= typecon(QName, Kind)               [symbol(typecon)]

  // --- Types ---
  syntax TypeVarList ::= List{TypeVar, ","}
  syntax TypeList    ::= List{Type, ","}
  syntax MulType     ::= multype(Multiplicity, Type)    [symbol(multype)]
  syntax MulTypeList ::= List{MulType, ","}

  syntax Type ::= TForall(TypeVarList, Type)            [symbol(TForall)]
                | TFun(MulTypeList, EffectRow, Type)     [symbol(TFun)]
                | TApp(Type, Type)                       [symbol(TApp)]
                | TCon(TypeCon)                          [symbol(TCon)]
                | TVar(TypeVar)                          [symbol(TVar)]
                | TSyn(QName, TypeList, Type)             [symbol(TSyn)]

  // --- Literals ---
  syntax Lit ::= litInt(Int)        [symbol(litInt)]
               | litFloat(Float)    [symbol(litFloat)]
               | litChar(String)    [symbol(litChar)]
               | litString(String)  [symbol(litString)]

  // --- Patterns ---
  syntax PatternList ::= List{Pattern, ","}
  syntax Pattern ::= PatCon(QName, PatternList)    [symbol(PatCon)]
                    | PatVar(Name, Type)             [symbol(PatVar)]
                    | PatWild(Type)                   [symbol(PatWild)]
                    | PatLit(Lit)                     [symbol(PatLit)]

  // --- Branches ---
  syntax Guard      ::= "noGuard"    [symbol(noGuard)]
                       | guard(Expr)   [symbol(guard)]
  syntax Branch     ::= branch(Pattern, Guard, Expr) [symbol(branch)]
  syntax BranchList ::= List{Branch, ";"}

  // --- Bindings ---
  syntax DefSort     ::= "defFun" | "defVal" | "defVar"
  syntax Bind        ::= bind(Name, Type, Expr, DefSort) [symbol(bind)]
  syntax BindGroup   ::= List{Bind, ","}
  syntax BGList      ::= List{BindGroup, ";;"}

  // --- Lambda parameters ---
  syntax Param     ::= param(Name, Type)     [symbol(param)]
  syntax ParamList ::= List{Param, ","}

  // --- Expressions ---
  syntax ExprList ::= List{Expr, ","}

  syntax Expr ::= EVar(Name)                          [symbol(EVar)]
                | ELit(Lit)                            [symbol(ELit)]
                | ECon(QName)                          [symbol(ECon)]
                | EApp(Expr, ExprList)                 [symbol(EApp)]
                | ELam(ParamList, Expr)                [symbol(ELam)]
                | ELet(BGList, Expr)                   [symbol(ELet)]
                | ECase(Expr, BranchList)              [symbol(ECase)]
                | ETypeApp(Expr, TypeList)             [symbol(ETypeApp)]
                | ETypeLam(TypeVarList, Expr)           [symbol(ETypeLam)]
                // Effect operations
                | EPerform(QName, ExprList)             [symbol(EPerform)]
                | EHandle(EffectRow, Expr, Expr)        [symbol(EHandle)]
                // Perceus
                | ERetain(Expr)                         [symbol(ERetain)]
                | ERelease(Expr)                        [symbol(ERelease)]
                | EDrop(Expr)                           [symbol(EDrop)]
                | EReuse(Expr, Expr)                    [symbol(EReuse)]
                // Laziness
                | EDelay(Expr)                          [symbol(EDelay)]
                | EForce(Expr)                          [symbol(EForce)]

  // --- Top-level ---
  syntax Def     ::= def(QName, Type, Expr, DefSort, Visibility) [symbol(def)]
  syntax DefList ::= List{Def, ";;"}

  syntax ConField     ::= field(Name, Type)              [symbol(field)]
  syntax ConFieldList ::= List{ConField, ","}
  syntax ConDecl      ::= con(QName, ConFieldList, Visibility) [symbol(con)]
  syntax ConDeclList  ::= List{ConDecl, "|"}

  syntax DataDecl     ::= data(QName, TypeVarList, ConDeclList, Visibility) [symbol(data)]
  syntax DataDeclList ::= List{DataDecl, ";;"}

  syntax OpDecl       ::= op(QName, Type) [symbol(op)]
  syntax OpDeclList   ::= List{OpDecl, ","}
  syntax EffectDecl   ::= effectdecl(QName, TypeVarList, OpDeclList) [symbol(effectdecl)]
  syntax EffectDeclList ::= List{EffectDecl, ";;"}

  syntax Program ::= program(QName, DefList, DataDeclList, EffectDeclList) [symbol(program)]

endmodule

// =================================================================
// Module 2: TYPING — well-typedness rules
// =================================================================

module ORGAN-IR-TYPING
  imports ORGAN-IR-SYNTAX
  imports MAP
  imports SET
  imports BOOL
  imports INT
  imports STRING

  // --- Typing judgment ---
  syntax KItem ::= typeOf(Expr)              [symbol(typeOf)]
                  | "#type" "(" Type ")"      [symbol(typeResult)]
                  | "#ok"                      [symbol(ok)]
                  | "#error" "(" String ")"    [symbol(typeError)]
                  | "#dropResult"              [symbol(dropResult)]

  // --- Configuration ---
  configuration
    <k> $PGM:KItem </k>
    <typeEnv> .Map </typeEnv>
    <conEnv>  .Map </conEnv>
    <currentEffect> effectEmpty </currentEffect>

  // --- Well-known type names ---
  syntax QName ::= intQName()    [function, symbol(intQName)]
                  | floatQName()  [function, symbol(floatQName)]
                  | charQName()   [function, symbol(charQName)]
                  | stringQName() [function, symbol(stringQName)]
                  | unitQName()   [function, symbol(unitQName)]

  rule intQName()    => qname("std/core/types", name("int", 0))
  rule floatQName()  => qname("std/core/types", name("float64", 0))
  rule charQName()   => qname("std/core/types", name("char", 0))
  rule stringQName() => qname("std/core/types", name("string", 0))
  rule unitQName()   => qname("std/core/types", name("unit", 0))

  syntax Type ::= intType()    [function, symbol(intType)]
                | floatType()  [function, symbol(floatType)]
                | charType()   [function, symbol(charType)]
                | stringType() [function, symbol(stringType)]
                | unitType()   [function, symbol(unitType)]

  rule intType()    => TCon(typecon(intQName(),    KValue))
  rule floatType()  => TCon(typecon(floatQName(),  KValue))
  rule charType()   => TCon(typecon(charQName(),   KValue))
  rule stringType() => TCon(typecon(stringQName(), KValue))
  rule unitType()   => TCon(typecon(unitQName(),   KValue))

  // --- Literal typing ---
  rule <k> typeOf(ELit(litInt(_)))    => #type(intType())    ...</k>
  rule <k> typeOf(ELit(litFloat(_)))  => #type(floatType())  ...</k>
  rule <k> typeOf(ELit(litChar(_)))   => #type(charType())   ...</k>
  rule <k> typeOf(ELit(litString(_))) => #type(stringType()) ...</k>

  // --- Variable typing ---
  rule <k> typeOf(EVar(N)) => #type(T) ...</k>
       <typeEnv>... N |-> T ...</typeEnv>

  rule <k> typeOf(EVar(N)) => #error("unbound variable") ...</k>
       <typeEnv> Env </typeEnv>
    requires notBool (N in_keys(Env))

  // --- Constructor typing ---
  rule <k> typeOf(ECon(QN)) => #type(T) ...</k>
       <conEnv>... QN |-> T ...</conEnv>

  // --- Perceus operations preserve types ---
  // retain(e) : T  when  e : T
  rule <k> typeOf(ERetain(E)) => typeOf(E) ...</k>

  // drop(e) : unit  when  e : T
  rule <k> typeOf(EDrop(E)) => typeOf(E) ~> #dropResult ...</k>
  rule <k> #type(_) ~> #dropResult => #type(unitType()) ...</k>

  // release(e) : unit  when  e : T
  rule <k> typeOf(ERelease(E)) => typeOf(E) ~> #dropResult ...</k>

  // --- Laziness ---
  rule <k> typeOf(EDelay(E)) => typeOf(E) ...</k>
  rule <k> typeOf(EForce(E)) => typeOf(E) ...</k>

endmodule

// =================================================================
// Module 3: PERCEUS — reference counting rewrite theory
//
// Specifies the Perceus algorithm as K rewrite rules.
// The algorithm transforms Core expressions by inserting:
//   - EDrop:   for bindings/params unused in their scope (non-linear)
//   - ERetain: for each additional use of a `many` variable beyond the first
//   - EReuse:  for reusing allocations when a value is uniquely owned
//
// Scope is modeled as a Map from Name to Multiplicity.
// The key invariant:
//   For every heap-allocated value v at any program point:
//     refcount(v) == number of live references to v
//
// This mirrors src/Frankenstein/Core/Perceus.hs exactly.
// =================================================================

module PERCEUS
  imports ORGAN-IR-SYNTAX
  imports SET
  imports MAP
  imports LIST
  imports INT
  imports BOOL

  // ---------------------------------------------------------------
  // Free variable analysis
  //
  // Mirrors Perceus.hs freeVars :: Expr -> Set Name
  // ---------------------------------------------------------------

  syntax Set ::= freeVars(Expr)          [function, symbol(freeVars)]
               | freeVarsExprs(ExprList)  [function, symbol(freeVarsExprs)]
               | freeVarsBranches(BranchList) [function, symbol(freeVarsBranches)]
               | patternNames(Pattern)    [function, symbol(patternNames)]
               | patternNamesL(PatternList) [function, symbol(patternNamesL)]
  syntax Bool ::= isFree(Name, Expr)     [function, symbol(isFree)]
  syntax Int  ::= usageCount(Name, Expr) [function, symbol(usageCount)]
               |  usageCountExprs(Name, ExprList) [function, symbol(usageCountExprs)]
               |  usageCountMax(Name, BranchList) [function, symbol(usageCountMax)]

  // Atomic expressions
  rule freeVars(EVar(N))          => SetItem(N)
  rule freeVars(ELit(_))          => .Set
  rule freeVars(ECon(_))          => .Set

  // Compound expressions
  rule freeVars(EApp(F, Es))      => freeVars(F) |Set freeVarsExprs(Es)
  rule freeVars(ERetain(E))       => freeVars(E)
  rule freeVars(ERelease(E))      => freeVars(E)
  rule freeVars(EDrop(E))         => freeVars(E)
  rule freeVars(EDelay(E))        => freeVars(E)
  rule freeVars(EForce(E))        => freeVars(E)
  rule freeVars(ETypeApp(E, _))   => freeVars(E)
  rule freeVars(ETypeLam(_, E))   => freeVars(E)
  rule freeVars(EHandle(_, H, B)) => freeVars(H) |Set freeVars(B)
  rule freeVars(EReuse(A, B))     => freeVars(A) |Set freeVars(B)
  rule freeVars(EPerform(_, Es))  => freeVarsExprs(Es)

  // Lambda: body minus parameter names
  rule freeVars(ELam(.ParamList, Body))
    => freeVars(Body)
  rule freeVars(ELam(param(N, _) , Ps:ParamList, Body))
    => freeVars(ELam(Ps, Body)) -Set SetItem(N)

  // Let: (rhs fvs + body fvs) minus bound names
  rule freeVars(ELet(.BGList, Body))
    => freeVars(Body)
  rule freeVars(ELet((bind(N, _, Rhs, _) , .BindGroup) ;; Bgs:BGList, Body))
    => (freeVars(Rhs) |Set freeVars(ELet(Bgs, Body))) -Set SetItem(N)

  // Case: scrutinee fvs + branch fvs
  rule freeVars(ECase(S, Bs))
    => freeVars(S) |Set freeVarsBranches(Bs)

  // Free vars in expression lists
  rule freeVarsExprs(.ExprList) => .Set
  rule freeVarsExprs(E:Expr , Es:ExprList)
    => freeVars(E) |Set freeVarsExprs(Es)

  // Free vars in branches: body fvs minus pattern-bound names
  rule freeVarsBranches(.BranchList) => .Set
  rule freeVarsBranches(branch(P, noGuard, Body) ; Bs)
    => (freeVars(Body) -Set patternNames(P)) |Set freeVarsBranches(Bs)
  rule freeVarsBranches(branch(P, guard(G), Body) ; Bs)
    => ((freeVars(Body) |Set freeVars(G)) -Set patternNames(P))
       |Set freeVarsBranches(Bs)

  // Pattern-bound names
  rule patternNames(PatVar(N, _))    => SetItem(N)
  rule patternNames(PatWild(_))      => .Set
  rule patternNames(PatLit(_))       => .Set
  rule patternNames(PatCon(_, Ps))   => patternNamesL(Ps)
  rule patternNamesL(.PatternList)   => .Set
  rule patternNamesL(P:Pattern , Ps:PatternList)
    => patternNames(P) |Set patternNamesL(Ps)

  rule isFree(N, E) => N in freeVars(E)

  // ---------------------------------------------------------------
  // Usage counting
  //
  // Mirrors Perceus.hs analyzeUsage :: Expr -> Map Name Int
  // Simplified to count for a single variable.
  //
  // For App: sum over subexpressions (each use is a separate reference)
  // For Case: max over branches (only one branch executes)
  // ---------------------------------------------------------------

  rule usageCount(N, EVar(N))     => 1
  rule usageCount(_, EVar(_))     => 0  [owise]
  rule usageCount(_, ELit(_))     => 0
  rule usageCount(_, ECon(_))     => 0
  rule usageCount(N, EApp(F, Es)) => usageCount(N, F) +Int usageCountExprs(N, Es)
  rule usageCount(N, ERetain(E))  => usageCount(N, E)
  rule usageCount(N, EDrop(E))    => usageCount(N, E)
  rule usageCount(N, ERelease(E)) => usageCount(N, E)
  rule usageCount(N, EDelay(E))   => usageCount(N, E)
  rule usageCount(N, EForce(E))   => usageCount(N, E)
  rule usageCount(N, ETypeApp(E, _))  => usageCount(N, E)
  rule usageCount(N, ETypeLam(_, E))  => usageCount(N, E)
  rule usageCount(N, EHandle(_, H, B))
    => usageCount(N, H) +Int usageCount(N, B)
  rule usageCount(N, EReuse(A, B))
    => usageCount(N, A) +Int usageCount(N, B)
  rule usageCount(N, EPerform(_, Es)) => usageCountExprs(N, Es)

  // Lambda: count in body
  rule usageCount(N, ELam(_, Body)) => usageCount(N, Body)

  // Let: count in rhs + count in body
  rule usageCount(N, ELet(.BGList, Body)) => usageCount(N, Body)
  rule usageCount(N, ELet((bind(_, _, Rhs, _), .BindGroup) ;; Bgs:BGList, Body))
    => usageCount(N, Rhs) +Int usageCount(N, ELet(Bgs, Body))

  // Case: scrutinee count + max over branches (only one arm executes)
  rule usageCount(N, ECase(S, Bs))
    => usageCount(N, S) +Int usageCountMax(N, Bs)

  // Expression list: sum
  rule usageCountExprs(_, .ExprList) => 0
  rule usageCountExprs(N, E:Expr , Es:ExprList)
    => usageCount(N, E) +Int usageCountExprs(N, Es)

  // Branch list: max (conservative — only one branch executes)
  rule usageCountMax(_, .BranchList) => 0
  rule usageCountMax(N, branch(_, _, Body) ; Bs)
    => maxInt(usageCount(N, Body), usageCountMax(N, Bs))

  // ---------------------------------------------------------------
  // Perceus transformation
  //
  // perceusS : scoped transformation carrying a Map Name Multiplicity
  //            (the "scope" tracking which variables are in scope and
  //             their substructural annotations)
  //
  // perceus : convenience wrapper starting with empty scope
  // ---------------------------------------------------------------

  syntax Expr ::= perceus(Expr)            [function, symbol(perceus)]
                 | perceusS(Map, Expr)      [function, symbol(perceusS)]
  syntax ExprList ::= perceusExprsS(Map, ExprList)
                                            [function, symbol(perceusExprsS)]
  syntax BranchList ::= perceusBranchesS(Map, BranchList)
                                            [function, symbol(perceusBranchesS)]

  // Helper: wrap an expression with drops for a list of names
  syntax Expr ::= wrapDrops(Set, Expr)     [function, symbol(wrapDrops)]

  // Helper: wrap an expression with retains for N-1 extra uses
  syntax Expr ::= wrapRetains(Name, Int, Expr) [function, symbol(wrapRetains)]

  // Helper: get multiplicity from scope, default many
  syntax Multiplicity ::= lookupMul(Name, Map) [function, symbol(lookupMul)]

  // Helper: collect bound names from a single bind group
  syntax Set ::= bindGroupNames(BindGroup) [function, symbol(bindGroupNames)]

  // Helper: extend scope with param multiplicities (all default to many)
  syntax Map ::= extendScopeParams(Map, ParamList)  [function, symbol(extendScopeParams)]
  syntax Map ::= extendScopeBind(Map, BindGroup)     [function, symbol(extendScopeBind)]
  syntax Map ::= extendScopePattern(Map, Pattern)     [function, symbol(extendScopePattern)]
  syntax Map ::= extendScopePatterns(Map, PatternList) [function, symbol(extendScopePatterns)]

  // Helper: find unused non-linear names from a set, given a body
  syntax Set ::= unusedNonLinear(Set, Map, Expr)  [function, symbol(unusedNonLinear)]

  // Helper: find names used >1 times with `many` multiplicity
  syntax Map ::= retainCandidates(Set, Map, Expr) [function, symbol(retainCandidates)]

  // ---------------------------------------------------------------
  // perceus(e) = perceusS(.Map, e)
  // ---------------------------------------------------------------

  rule perceus(E) => perceusS(.Map, E)

  // ---------------------------------------------------------------
  // Atoms: no transformation
  // ---------------------------------------------------------------

  rule perceusS(_, EVar(N))  => EVar(N)
  rule perceusS(_, ELit(L))  => ELit(L)
  rule perceusS(_, ECon(QN)) => ECon(QN)

  // ---------------------------------------------------------------
  // Application: recurse
  // ---------------------------------------------------------------

  rule perceusS(Scope, EApp(F, Es))
    => EApp(perceusS(Scope, F), perceusExprsS(Scope, Es))

  // ---------------------------------------------------------------
  // Lambda: drop unused non-linear params
  //
  // Perceus rule: for each param P not free in body with mul != linear,
  // insert EDrop(EVar(P)) before the body.
  //
  // Mirrors Perceus.hs lines 77-88
  // ---------------------------------------------------------------

  rule perceusS(Scope, ELam(Ps, Body))
    => ELam(Ps,
         wrapDrops(
           unusedNonLinear(paramNames(Ps), extendScopeParams(Scope, Ps), Body),
           perceusS(extendScopeParams(Scope, Ps), Body)
         )
       )

  syntax Set ::= paramNames(ParamList) [function, symbol(paramNames)]
  rule paramNames(.ParamList) => .Set
  rule paramNames(param(N, _) , Ps:ParamList)
    => SetItem(N) |Set paramNames(Ps)

  // ---------------------------------------------------------------
  // Let: drop unused bindings, insert retains for multi-use
  //
  // For each binding x = rhs where x is NOT free in body
  // and mul(x) != linear:
  //   insert let _ = drop(x) in ... after the binding
  //
  // For each binding x = rhs where x is used N > 1 times in body
  // and mul(x) == many:
  //   insert (N-1) retains of x
  //
  // Mirrors Perceus.hs lines 91-106
  // ---------------------------------------------------------------

  // Single-binding let (the common case and the one where
  // Perceus insertion happens)
  rule perceusS(Scope,
    ELet((bind(X, T, Rhs, S) , .BindGroup) ;; .BGList, Body)
  ) =>
    ELet(
      (bind(X, T, perceusS(Scope, Rhs), S) , .BindGroup) ;; .BGList,
      wrapRetains(X, usageCount(X, Body),
        wrapDrops(
          unusedNonLinear(SetItem(X), Scope [ X <- lookupMulFromType(T) ], Body),
          perceusS(Scope [ X <- lookupMulFromType(T) ], Body)
        )
      )
    )

  // Helper: infer multiplicity from binding type
  syntax Multiplicity ::= lookupMulFromType(Type) [function, symbol(lookupMulFromType)]
  rule lookupMulFromType(TFun(multype(M, _) , _, _, _)) => M
  rule lookupMulFromType(_) => many  [owise]

  // Empty let: just transform body
  rule perceusS(Scope, ELet(.BGList, Body))
    => perceusS(Scope, Body)

  // Multi-binding let: recurse by nesting
  // let {x = a; y = b} in body  ==>  let x = a in let y = b in body
  rule perceusS(Scope,
    ELet((bind(X, T, Rhs, S) , .BindGroup) ;; (Bg2:BindGroup ;; Bgs:BGList), Body)
  ) =>
    perceusS(Scope,
      ELet((bind(X, T, Rhs, S) , .BindGroup) ;; .BGList,
        ELet(Bg2 ;; Bgs, Body)
      )
    )

  // ---------------------------------------------------------------
  // Case: recurse into scrutinee, transform each branch
  //
  // Each branch extends scope with pattern-bound variables.
  // Unused pattern bindings get drops inserted.
  //
  // Mirrors Perceus.hs lines 109-112, 137-141
  // ---------------------------------------------------------------

  rule perceusS(Scope, ECase(S, Bs))
    => ECase(perceusS(Scope, S), perceusBranchesS(Scope, Bs))

  // ---------------------------------------------------------------
  // Perceus ops already present: recurse
  // ---------------------------------------------------------------

  rule perceusS(Scope, ERetain(E))   => ERetain(perceusS(Scope, E))
  rule perceusS(Scope, ERelease(E))  => ERelease(perceusS(Scope, E))
  rule perceusS(Scope, EDrop(E))     => EDrop(perceusS(Scope, E))
  rule perceusS(Scope, EReuse(A, B))
    => EReuse(perceusS(Scope, A), perceusS(Scope, B))

  // ---------------------------------------------------------------
  // Effect ops: recurse
  // ---------------------------------------------------------------

  rule perceusS(Scope, EHandle(Eff, H, B))
    => EHandle(Eff, perceusS(Scope, H), perceusS(Scope, B))
  rule perceusS(Scope, EPerform(QN, Es))
    => EPerform(QN, perceusExprsS(Scope, Es))

  // ---------------------------------------------------------------
  // Laziness: recurse
  // ---------------------------------------------------------------

  rule perceusS(Scope, EDelay(E)) => EDelay(perceusS(Scope, E))
  rule perceusS(Scope, EForce(E)) => EForce(perceusS(Scope, E))

  // ---------------------------------------------------------------
  // Type ops: recurse (types are erased, no Perceus needed)
  // ---------------------------------------------------------------

  rule perceusS(Scope, ETypeApp(E, Ts))  => ETypeApp(perceusS(Scope, E), Ts)
  rule perceusS(Scope, ETypeLam(Tvs, E)) => ETypeLam(Tvs, perceusS(Scope, E))

  // ---------------------------------------------------------------
  // Expression list helper
  // ---------------------------------------------------------------

  rule perceusExprsS(_, .ExprList) => .ExprList
  rule perceusExprsS(Scope, E:Expr , Es:ExprList)
    => perceusS(Scope, E) , perceusExprsS(Scope, Es)

  // ---------------------------------------------------------------
  // Branch list helper: extend scope with pattern vars, drop unused
  // ---------------------------------------------------------------

  rule perceusBranchesS(_, .BranchList) => .BranchList
  rule perceusBranchesS(Scope, branch(P, G, Body) ; Bs)
    => branch(P, G,
         wrapDrops(
           unusedNonLinear(patternNames(P), extendScopePattern(Scope, P), Body),
           perceusS(extendScopePattern(Scope, P), Body)
         )
       ) ; perceusBranchesS(Scope, Bs)

  // ---------------------------------------------------------------
  // wrapDrops: insert let _ = drop(x) for each name in the set
  //
  // wrapDrops({x, y}, body)
  //   = let _ = drop(x) in let _ = drop(y) in body
  // wrapDrops({}, body) = body
  // ---------------------------------------------------------------

  // Unit type for drop/retain binding types
  syntax Type ::= perceusUnitType() [function, symbol(perceusUnitType)]
  rule perceusUnitType()
    => TCon(typecon(qname("std/core/types", name("unit", 0)), KValue))

  rule wrapDrops(.Set, Body) => Body
  rule wrapDrops(SetItem(N:Name) Rest:Set, Body)
    => ELet(
         (bind(name("_drop", 0), perceusUnitType(), EDrop(EVar(N)), defVal) , .BindGroup) ;; .BGList,
         wrapDrops(Rest, Body)
       )

  // ---------------------------------------------------------------
  // wrapRetains: if usage > 1 and multiplicity is many,
  // insert (N-1) retains before the expression.
  //
  // wrapRetains(x, 3, body) = let _ = retain(x) in
  //                           let _ = retain(x) in body
  // wrapRetains(x, 1, body) = body   (single use, no retain)
  // wrapRetains(x, 0, body) = body   (unused, handled by drop)
  // ---------------------------------------------------------------

  rule wrapRetains(_, Count, Body) => Body
    requires Count <=Int 1

  rule wrapRetains(N, Count, Body)
    => ELet(
         (bind(name("_retain", 0), perceusUnitType(), ERetain(EVar(N)), defVal) , .BindGroup) ;; .BGList,
         wrapRetains(N, Count -Int 1, Body)
       )
    requires Count >Int 1

  // ---------------------------------------------------------------
  // unusedNonLinear: from a set of names, return those that are
  // NOT free in the body AND have non-linear multiplicity in scope.
  //
  // This is the set of names that need EDrop insertion.
  // Linear values must be used exactly once — if unused, that's
  // a linearity violation (error), not a drop.
  // ---------------------------------------------------------------

  syntax Set ::= unusedNonLinearHelper(List, Map, Set)
                                        [function, symbol(unusedNonLinearHelper)]
  syntax List ::= setToList(Set)        [function, symbol(setToList)]

  rule unusedNonLinear(Names, Scope, Body)
    => unusedNonLinearHelper(setToList(Names), Scope, freeVars(Body))

  rule setToList(.Set) => .List
  rule setToList(SetItem(N) Rest:Set) => ListItem(N) setToList(Rest)

  syntax Bool ::= isLinear(Multiplicity)  [function, symbol(isLinear)]
  rule isLinear(linear) => true
  rule isLinear(many)   => false
  rule isLinear(affine) => false

  rule unusedNonLinearHelper(.List, _, _) => .Set
  rule unusedNonLinearHelper(ListItem(N:Name) Rest:List, Scope, BodyFV)
    => ( SetItem(N) |Set unusedNonLinearHelper(Rest, Scope, BodyFV) )
    requires (notBool (N in BodyFV))
     andBool (notBool isLinear(lookupMul(N, Scope)))
  rule unusedNonLinearHelper(ListItem(_:Name) Rest:List, Scope, BodyFV)
    => unusedNonLinearHelper(Rest, Scope, BodyFV)
    [owise]

  // ---------------------------------------------------------------
  // Scope management helpers
  // ---------------------------------------------------------------

  rule lookupMul(N, (N |-> M:Multiplicity) _) => M
  rule lookupMul(_, _) => many  [owise]

  rule extendScopeParams(Scope, .ParamList) => Scope
  rule extendScopeParams(Scope, param(N, _) , Ps:ParamList)
    => extendScopeParams(Scope [ N <- many ], Ps)

  rule extendScopeBind(Scope, .BindGroup) => Scope
  rule extendScopeBind(Scope, bind(N, T, _, _) , Bs:BindGroup)
    => extendScopeBind(Scope [ N <- lookupMulFromType(T) ], Bs)

  rule extendScopePattern(Scope, PatVar(N, _))
    => Scope [ N <- many ]
  rule extendScopePattern(Scope, PatWild(_))     => Scope
  rule extendScopePattern(Scope, PatLit(_))      => Scope
  rule extendScopePattern(Scope, PatCon(_, Ps))
    => extendScopePatterns(Scope, Ps)

  rule extendScopePatterns(Scope, .PatternList)  => Scope
  rule extendScopePatterns(Scope, P:Pattern , Ps:PatternList)
    => extendScopePatterns(extendScopePattern(Scope, P), Ps)

endmodule

// =================================================================
// Module 4: MAIN — the top-level module that ties everything together
// =================================================================

module ORGAN-IR
  imports ORGAN-IR-TYPING
  imports PERCEUS
endmodule
