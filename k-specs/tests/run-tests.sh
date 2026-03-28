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
  actual=$(echo "$output" | grep '<k>' -A1 | tail -1 | sed 's/^ *//' || true)
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
echo "=== Perceus Rewrite Rules ==="

check "perceus on literal (identity)" \
  'perceus(ELit(litInt(42)))' \
  'ELit ( litInt ( 42 ) )'

check "perceus on retain (recurse)" \
  'perceus(ERetain(EVar(name("x",0))))' \
  'ERetain ( EVar ( name ( "x" , 0 ) ) )'

check "freeVars: x is free in EVar(x)" \
  'isFree(name("x",0), EVar(name("x",0)))' \
  'true'

check "usageCount: x used once in EVar(x)" \
  'usageCount(name("x",0), EVar(name("x",0)))' \
  '1'

check "usageCount: x not used in EVar(y)" \
  'usageCount(name("x",0), EVar(name("y",0)))' \
  '0'

echo ""
echo "=== Results: $PASS passed, $FAIL failed ==="
exit $FAIL
