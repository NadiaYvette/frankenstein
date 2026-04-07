#!/bin/bash
# Validate Frankenstein Wasm output against native execution.
# For each test program: compile to native, compile to wasm, compare results.
#
# Phase 5b validation: currently uses Node.js for Wasm execution.
# Future: add KWasm (github.com/runtimeverification/wasm-semantics) path
# to close the verification loop: source → K oracle ↔ binary → KWasm.

set -euo pipefail
export LC_ALL=C

FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

pass=0
fail=0

check() {
    local name="$1"
    local expected="$2"
    local wasm_file="$3"

    local actual
    actual=$(node -e "
        const fs = require('fs');
        const m = new WebAssembly.Module(fs.readFileSync('$wasm_file'));
        const i = new WebAssembly.Instance(m, {env: {printf: () => 0}});
        const r = i.exports._frankenstein_main();
        process.stdout.write(String(Number(r)));
    " 2>&1)

    if [ "$actual" = "$expected" ]; then
        printf "  ✓ %-30s %s\n" "$name" "$actual"
        pass=$((pass + 1))
    else
        printf "  ✗ %-30s expected %s, got %s\n" "$name" "$expected" "$actual"
        fail=$((fail + 1))
    fi
}

echo "=== Frankenstein Wasm Validation ==="
echo ""

# Test 1: Built-in demo (factorial)
echo "Compiling demo to Wasm..."
$FRANK --demo --compile --target wasm32 -o "$TMPDIR/demo.wasm" 2>/dev/null
check "demo: factorial(10)" "3628800" "$TMPDIR/demo.wasm"

# Test 2: Native vs Wasm comparison for demo
echo "Compiling demo to native..."
$FRANK --demo --compile -o "$TMPDIR/demo.native" 2>/dev/null
native_result=$("$TMPDIR/demo.native")
wasm_result=$(node -e "
    const fs = require('fs');
    const m = new WebAssembly.Module(fs.readFileSync('$TMPDIR/demo.wasm'));
    const i = new WebAssembly.Instance(m, {});
    process.stdout.write(String(Number(i.exports._frankenstein_main())));
" 2>&1)
if [ "$native_result" = "$wasm_result" ]; then
    printf "  ✓ %-30s native=%s wasm=%s\n" "native == wasm" "$native_result" "$wasm_result"
    pass=$((pass + 1))
else
    printf "  ✗ %-30s native=%s wasm=%s\n" "native != wasm" "$native_result" "$wasm_result"
    fail=$((fail + 1))
fi

# Test 3: Wasm binary size check
wasm_size=$(stat -c%s "$TMPDIR/demo.wasm")
if [ "$wasm_size" -lt 10000 ]; then
    printf "  ✓ %-30s %d bytes\n" "wasm binary < 10KB" "$wasm_size"
    pass=$((pass + 1))
else
    printf "  ✗ %-30s %d bytes (expected < 10000)\n" "wasm binary < 10KB" "$wasm_size"
    fail=$((fail + 1))
fi

echo ""
echo "=== Results: $pass passed, $fail failed ==="
exit $fail
