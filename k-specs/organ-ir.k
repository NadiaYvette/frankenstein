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
// =================================================================

module PERCEUS
  imports ORGAN-IR-SYNTAX
  imports SET
  imports MAP
  imports INT
  imports BOOL

  // --- Free variable analysis ---
  syntax Set ::= freeVars(Expr)          [function, symbol(freeVars)]
  syntax Bool ::= isFree(Name, Expr)     [function, symbol(isFree)]
  syntax Int  ::= usageCount(Name, Expr) [function, symbol(usageCount)]

  rule freeVars(EVar(N))          => SetItem(N)
  rule freeVars(ELit(_))          => .Set
  rule freeVars(ECon(_))          => .Set
  rule freeVars(ERetain(E))       => freeVars(E)
  rule freeVars(ERelease(E))      => freeVars(E)
  rule freeVars(EDrop(E))         => freeVars(E)
  rule freeVars(EDelay(E))        => freeVars(E)
  rule freeVars(EForce(E))        => freeVars(E)
  rule freeVars(ETypeApp(E, _))   => freeVars(E)
  rule freeVars(ETypeLam(_, E))   => freeVars(E)
  rule freeVars(EHandle(_, H, B)) => freeVars(H) |Set freeVars(B)
  rule freeVars(EReuse(A, B))     => freeVars(A) |Set freeVars(B)

  rule isFree(N, E) => N in freeVars(E)

  // --- Usage counting ---
  rule usageCount(N, EVar(N))     => 1
  rule usageCount(_, EVar(_))     => 0  [owise]
  rule usageCount(_, ELit(_))     => 0
  rule usageCount(_, ECon(_))     => 0
  rule usageCount(N, ERetain(E))  => usageCount(N, E)
  rule usageCount(N, EDrop(E))    => usageCount(N, E)
  rule usageCount(N, ERelease(E)) => usageCount(N, E)
  rule usageCount(N, EDelay(E))   => usageCount(N, E)
  rule usageCount(N, EForce(E))   => usageCount(N, E)

  // --- Perceus transformation (as a function) ---
  syntax Expr ::= perceus(Expr) [function, symbol(perceus)]

  // Atoms: no transformation needed
  rule perceus(EVar(N))  => EVar(N)
  rule perceus(ELit(L))  => ELit(L)
  rule perceus(ECon(QN)) => ECon(QN)

  // Application: recurse into subexpressions
  rule perceus(EApp(F, Es)) => EApp(perceus(F), perceusExprs(Es))

  // Lambda: recurse into body
  // (Drop insertion for unused params handled at let-binding level)
  rule perceus(ELam(Ps, Body)) => ELam(Ps, perceus(Body))

  // Case: recurse into scrutinee and branches
  rule perceus(ECase(S, Bs)) => ECase(perceus(S), perceusBranches(Bs))

  // Already-present Perceus ops: recurse
  rule perceus(ERetain(E))   => ERetain(perceus(E))
  rule perceus(ERelease(E))  => ERelease(perceus(E))
  rule perceus(EDrop(E))     => EDrop(perceus(E))
  rule perceus(EReuse(A, B)) => EReuse(perceus(A), perceus(B))

  // Effect ops: recurse
  rule perceus(EHandle(Eff, H, B)) => EHandle(Eff, perceus(H), perceus(B))
  rule perceus(EPerform(QN, Es))   => EPerform(QN, perceusExprs(Es))

  // Laziness: recurse
  rule perceus(EDelay(E)) => EDelay(perceus(E))
  rule perceus(EForce(E)) => EForce(perceus(E))

  // Type ops: recurse
  rule perceus(ETypeApp(E, Ts))  => ETypeApp(perceus(E), Ts)
  rule perceus(ETypeLam(Tvs, E)) => ETypeLam(Tvs, perceus(E))

  // --- Helper: transform expression lists ---
  syntax ExprList ::= perceusExprs(ExprList) [function, symbol(perceusExprs)]
  rule perceusExprs(.ExprList) => .ExprList
  rule perceusExprs(E:Expr , Es:ExprList)
    => perceus(E) , perceusExprs(Es)

  // --- Helper: transform branch lists ---
  syntax BranchList ::= perceusBranches(BranchList) [function, symbol(perceusBranches)]
  rule perceusBranches(.BranchList) => .BranchList
  rule perceusBranches(branch(P, G, Body) ; Bs)
    => branch(P, G, perceus(Body)) ; perceusBranches(Bs)

endmodule

// =================================================================
// Module 4: MAIN — the top-level module that ties everything together
// =================================================================

module ORGAN-IR
  imports ORGAN-IR-TYPING
  imports PERCEUS
endmodule
