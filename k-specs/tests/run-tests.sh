#!/usr/bin/env bash
# OrganIR K specification test suite
KRUN="${KRUN:-$HOME/src/k/result/bin/krun}"
DEF="${DEF:-$(cd "$(dirname "$0")/.." && pwd)/organ-ir-kompiled}"
PASS=0
FAIL=0

check() {
  local desc="$1" input="$2" expected="$3"
  local output actual
  output=$($KRUN --definition "$DEF" -cPGM="$input" 2>&1)
  actual=$(echo "$output" | grep '<k>' -A1 | tail -1 | sed 's/^ *//')
  if echo "$actual" | grep -qF "$expected"; then
    echo "  PASS: $desc"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $desc"
    echo "    expected substring: $expected"
    echo "    actual k-cell line: $actual"
    FAIL=$((FAIL + 1))
  fi
}

# Check that a krun output contains a given string anywhere in full output
checkFull() {
  local desc="$1" input="$2" expected="$3"
  local output
  output=$($KRUN --definition "$DEF" -cPGM="$input" 2>&1)
  if echo "$output" | grep -qF "$expected"; then
    echo "  PASS: $desc"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $desc"
    echo "    expected substring: $expected"
    echo "    actual output (first 5 lines):"
    echo "$output" | head -5 | sed 's/^/      /'
    FAIL=$((FAIL + 1))
  fi
}

# Abbreviated type constructors for tests
# (K's parser accepts qname("s",name("i",0)) as a short stand-in)
T='TCon(typecon(qname("s", name("i",0)), KValue))'

echo "=== OrganIR Typing Rules ==="

check "int literal" \
  'typeOf(ELit(litInt(42)))' \
  '#type ( TCon ( typecon ( qname ( "std/core/types" , name ( "int" , 0 ) ) , KValue ) ) )'

check "string literal" \
  'typeOf(ELit(litString("hello")))' \
  '#type ( TCon ( typecon ( qname ( "std/core/types" , name ( "string" , 0 ) ) , KValue ) ) )'

check "float literal" \
  'typeOf(ELit(litFloat(3.14)))' \
  '#type ( TCon ( typecon ( qname ( "std/core/types" , name ( "float64" , 0 ) ) , KValue ) ) )'

check "unbound variable" \
  'typeOf(EVar(name("x", 0)))' \
  '#error ( "unbound variable" )'

check "retain preserves type" \
  'typeOf(ERetain(ELit(litInt(7))))' \
  '#type ( TCon ( typecon ( qname ( "std/core/types" , name ( "int" , 0 ) ) , KValue ) ) )'

check "drop gives unit" \
  'typeOf(EDrop(ELit(litInt(7))))' \
  '#type ( TCon ( typecon ( qname ( "std/core/types" , name ( "unit" , 0 ) ) , KValue ) ) )'

echo ""
echo "=== Free Variable Analysis ==="

check "freeVars: x is free in EVar(x)" \
  'isFree(name("x",0), EVar(name("x",0)))' \
  'true'

check "freeVars: x is not free in EVar(y)" \
  'isFree(name("x",0), EVar(name("y",0)))' \
  'false'

echo ""
echo "=== Usage Counting ==="

check "usageCount: x once in EVar(x)" \
  'usageCount(name("x",0), EVar(name("x",0)))' \
  '1'

check "usageCount: x zero in EVar(y)" \
  'usageCount(name("x",0), EVar(name("y",0)))' \
  '0'

check "usageCount: x twice in app(x, x)" \
  'usageCount(name("x",0), EApp(EVar(name("x",0)), EVar(name("x",0))))' \
  '2'

echo ""
echo "=== Multiplicity Predicates ==="

check "isLinear(linear) = true" \
  'isLinear(linear)' \
  'true'

check "isLinear(many) = false" \
  'isLinear(many)' \
  'false'

check "isLinear(affine) = false" \
  'isLinear(affine)' \
  'false'

echo ""
echo "=== Perceus: Structural Recursion ==="

check "perceus on literal (identity)" \
  'perceus(ELit(litInt(42)))' \
  'ELit ( litInt ( 42 ) )'

check "perceus on retain (recurse)" \
  'perceus(ERetain(EVar(name("x",0))))' \
  'ERetain ( EVar ( name ( "x" , 0 ) ) )'

echo ""
echo "=== Perceus: Drop Insertion (ELet) ==="

# let x = 42 in 7  (x unused, default many -> insert drop)
checkFull "let: unused binding inserts drop" \
  "perceus(ELet(bind(name(\"x\",0), $T, ELit(litInt(42)), defVal), ELit(litInt(7))))" \
  'EDrop ( EVar ( name ( "x" , 0 ) ) )'

# let x = 42 in x  (x used once -> no drop, no retain)
checkFull "let: used binding, no drop" \
  "perceus(ELet(bind(name(\"x\",0), $T, ELit(litInt(42)), defVal), EVar(name(\"x\",0))))" \
  'EVar ( name ( "x" , 0 ) )'

echo ""
echo "=== Perceus: Retain Insertion (multi-use) ==="

# let x = 42 in app(x, x)  (x used twice, many -> insert 1 retain)
checkFull "let: multi-use inserts retain" \
  "perceus(ELet(bind(name(\"x\",0), $T, ELit(litInt(42)), defVal), EApp(EVar(name(\"x\",0)), EVar(name(\"x\",0)))))" \
  'ERetain ( EVar ( name ( "x" , 0 ) ) )'

echo ""
echo "=== Perceus: Lambda Drop Insertion ==="

# \x -> 7   (x unused -> insert drop)
checkFull "lambda: unused param inserts drop" \
  "perceus(ELam(param(name(\"x\",0), $T), ELit(litInt(7))))" \
  'EDrop ( EVar ( name ( "x" , 0 ) ) )'

# \x -> x   (x used -> no drop)
checkFull "lambda: used param, no drop" \
  "perceus(ELam(param(name(\"x\",0), $T), EVar(name(\"x\",0))))" \
  'EVar ( name ( "x" , 0 ) )'

echo ""
echo "============================================================"
echo "=== Bridge Translation Properties ==="
echo "============================================================"

# Bridge properties use a separate kompiled definition
BDEF="${BDEF:-$(cd "$(dirname "$0")/.." && pwd)/bridge-properties-kompiled}"

# Helper that uses the bridge-properties definition
checkBridge() {
  local desc="$1" input="$2" expected="$3"
  local output actual
  output=$($KRUN --definition "$BDEF" -cPGM="$input" 2>&1)
  actual=$(echo "$output" | grep '<k>' -A1 | tail -1 | sed 's/^ *//')
  if echo "$actual" | grep -qF "$expected"; then
    echo "  PASS: $desc"
    PASS=$((PASS + 1))
  else
    echo "  FAIL: $desc"
    echo "    expected substring: $expected"
    echo "    actual k-cell line: $actual"
    FAIL=$((FAIL + 1))
  fi
}

echo ""
echo "--- GHC Bridge Properties ---"

checkBridge "ghc: strict binding has no EDelay" \
  'checkGhcStrictNoDelay(ELit(litInt(42)))' \
  'true'

checkBridge "ghc: strict binding rejects EDelay" \
  'checkGhcStrictNoDelay(EDelay(ELit(litInt(42))))' \
  'false'

checkBridge "ghc: lazy binding is delayed" \
  'checkGhcLazyIsDelayed(EDelay(EVar(name("x",0))))' \
  'true'

checkBridge "ghc: non-delayed fails lazy check" \
  'checkGhcLazyIsDelayed(EVar(name("x",0)))' \
  'false'

checkBridge "ghc: absent binding is dead (lit 0)" \
  'checkGhcAbsentIsDead(ELit(litInt(0)))' \
  'true'

checkBridge "ghc: absent binding rejects non-zero" \
  'checkGhcAbsentIsDead(ELit(litInt(7)))' \
  'false'

checkBridge "ghc: absent binding rejects EDelay" \
  'checkGhcAbsentIsDead(EDelay(ELit(litInt(0))))' \
  'false'

# ForAllTy with KStar and Many multiplicity
checkBridge "ghc: forall has KStar and Many" \
  'checkGhcForallKind(TForall(typevar(name("a",0), KStar, many), TCon(typecon(qname("std",name("int",0)), KValue))))' \
  'true'

# ForAllTy with wrong kind should fail
checkBridge "ghc: forall with KEffect fails" \
  'checkGhcForallKind(TForall(typevar(name("a",0), KEffect, many), TCon(typecon(qname("std",name("int",0)), KValue))))' \
  'false'

# TFun with all Many args (as GHC bridge produces)
checkBridge "ghc: fun args all Many" \
  'checkGhcAllArgsMany(TFun(multype(many, TCon(typecon(qname("s",name("i",0)),KValue))), multype(many, TCon(typecon(qname("s",name("j",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'true'

# TFun with a linear arg should fail for GHC bridge
checkBridge "ghc: fun arg linear fails all-Many check" \
  'checkGhcAllArgsMany(TFun(multype(linear, TCon(typecon(qname("s",name("i",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'false'

echo ""
echo "--- Rust Bridge Properties ---"

checkBridge "rust: affine multiplicity is valid" \
  'checkRustAffine(affine)' \
  'true'

checkBridge "rust: linear multiplicity is valid" \
  'checkRustAffine(linear)' \
  'true'

checkBridge "rust: many multiplicity is invalid for owned types" \
  'checkRustAffine(many)' \
  'false'

# Rust fun args should all be affine
checkBridge "rust: fun args all affine" \
  'checkRustFunArgsAffine(TFun(multype(affine, TCon(typecon(qname("s",name("i",0)),KValue))), multype(affine, TCon(typecon(qname("s",name("j",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'true'

checkBridge "rust: fun arg many fails affine check" \
  'checkRustFunArgsAffine(TFun(multype(many, TCon(typecon(qname("s",name("i",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'false'

# Move: used once, no retain
checkBridge "rust: moved var (used once) has no retain" \
  'checkRustMoveNoRetain(name("x",0), EVar(name("x",0)))' \
  'true'

# Move: unused, no retain
checkBridge "rust: unused var has no retain" \
  'checkRustMoveNoRetain(name("x",0), ELit(litInt(7)))' \
  'true'

# Copy: ERetain(EVar(_)) is correct
checkBridge "rust: copy produces retain" \
  'checkRustCopyHasRetain(ERetain(EVar(name("x",0))))' \
  'true'

# Copy: bare EVar is wrong
checkBridge "rust: bare var fails copy check" \
  'checkRustCopyHasRetain(EVar(name("x",0)))' \
  'false'

# Drop on unused: either used or has drop
checkBridge "rust: unused var with drop passes" \
  'checkRustDropOnUnused(name("x",0), EDrop(EVar(name("x",0))))' \
  'true'

checkBridge "rust: used var passes (no drop needed)" \
  'checkRustDropOnUnused(name("x",0), EVar(name("x",0)))' \
  'true'

# IO effect check
checkBridge "rust: function has io effect" \
  'checkRustHasIoEffect(TFun(multype(affine, TCon(typecon(qname("s",name("i",0)),KValue))), effectExtend(qname("std",name("io",0)), effectEmpty), TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'true'

checkBridge "rust: pure function fails io check" \
  'checkRustHasIoEffect(TFun(multype(affine, TCon(typecon(qname("s",name("i",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'false'

echo ""
echo "--- Mercury Bridge Properties ---"

checkBridge "mercury: det has pure effect (empty row)" \
  'checkMercuryDetPure(effectEmpty)' \
  'true'

checkBridge "mercury: det rejects non-empty row" \
  'checkMercuryDetPure(effectExtend(qname("mercury",name("exn",0)), effectEmpty))' \
  'false'

checkBridge "mercury: det type has empty effect" \
  'checkMercuryDetType(TFun(multype(many, TCon(typecon(qname("s",name("i",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'true'

checkBridge "mercury: semidet has exn effect" \
  'checkMercurySemidetExn(effectExtend(qname("mercury",name("exn",0)), effectEmpty))' \
  'true'

checkBridge "mercury: semidet exact: exn+empty" \
  'checkMercurySemidetExact(effectExtend(qname("mercury",name("exn",0)), effectEmpty))' \
  'true'

checkBridge "mercury: semidet rejects empty" \
  'checkMercurySemidetExn(effectEmpty)' \
  'false'

checkBridge "mercury: multi has choice effect" \
  'checkMercuryMultiChoice(effectExtend(qname("mercury",name("choice",0)), effectEmpty))' \
  'true'

checkBridge "mercury: nondet has both exn and choice" \
  'checkMercuryNondetBoth(effectExtend(qname("mercury",name("exn",0)), effectExtend(qname("mercury",name("choice",0)), effectEmpty)))' \
  'true'

checkBridge "mercury: nondet fails if only exn" \
  'checkMercuryNondetBoth(effectExtend(qname("mercury",name("exn",0)), effectEmpty))' \
  'false'

checkBridge "mercury: di mode is linear" \
  'checkMercuryDiLinear(linear)' \
  'true'

checkBridge "mercury: di mode rejects many" \
  'checkMercuryDiLinear(many)' \
  'false'

checkBridge "mercury: in mode is many" \
  'checkMercuryInMany(many)' \
  'true'

checkBridge "mercury: in mode rejects linear" \
  'checkMercuryInMany(linear)' \
  'false'

echo ""
echo "--- Koka Bridge Properties ---"

checkBridge "koka: pure effect is empty" \
  'checkKokaPureIsEmpty(effectEmpty)' \
  'true'

checkBridge "koka: non-empty is not pure" \
  'checkKokaPureIsEmpty(effectExtend(qname("std",name("io",0)), effectEmpty))' \
  'false'

checkBridge "koka: effect row length 0 for empty" \
  'checkKokaEffectLength(effectEmpty, 0)' \
  'true'

checkBridge "koka: effect row length 1" \
  'checkKokaEffectLength(effectExtend(qname("std",name("io",0)), effectEmpty), 1)' \
  'true'

checkBridge "koka: effect row length 2" \
  'checkKokaEffectLength(effectExtend(qname("std",name("exn",0)), effectExtend(qname("std",name("io",0)), effectEmpty)), 2)' \
  'true'

checkBridge "koka: effect row length mismatch fails" \
  'checkKokaEffectLength(effectExtend(qname("std",name("io",0)), effectEmpty), 2)' \
  'false'

# All multiplicities default to Many
checkBridge "koka: fun args all Many" \
  'checkKokaAllMulMany(TFun(multype(many, TCon(typecon(qname("s",name("i",0)),KValue))), multype(many, TCon(typecon(qname("s",name("j",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'true'

checkBridge "koka: fun arg linear fails all-Many check" \
  'checkKokaAllMulMany(TFun(multype(linear, TCon(typecon(qname("s",name("i",0)),KValue))), effectEmpty, TCon(typecon(qname("s",name("r",0)),KValue))))' \
  'false'

# Type variable Many check
checkBridge "koka: type var has Many multiplicity" \
  'checkKokaTypeVarMany(typevar(name("a",0), KStar, many))' \
  'true'

checkBridge "koka: type var linear fails Many check" \
  'checkKokaTypeVarMany(typevar(name("a",0), KStar, linear))' \
  'false'

# TSyn expansion
checkBridge "koka: TSyn with expanded body passes" \
  'checkKokaSynExpansion(testSynExpanded())' \
  'true'

checkBridge "koka: nested TSyn fails expansion check" \
  'checkKokaSynExpansion(testSynNested())' \
  'false'

checkBridge "koka: non-TSyn passes expansion trivially" \
  'checkKokaSynExpansion(TCon(typecon(qname("std",name("int",0)), KValue)))' \
  'true'

checkBridge "koka: TSyn has body" \
  'checkKokaSynHasBody(testSynHasBody())' \
  'true'

checkBridge "koka: non-TSyn fails has-body check" \
  'checkKokaSynHasBody(TCon(typecon(qname("std",name("int",0)), KValue)))' \
  'false'

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
exit $FAIL
