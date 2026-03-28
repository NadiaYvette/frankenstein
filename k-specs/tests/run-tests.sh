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
echo "=== Results: $PASS passed, $FAIL failed ==="
exit $FAIL
