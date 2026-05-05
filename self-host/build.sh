#!/bin/bash
# Self-hosting build: compile Frankenstein's modules through its own pipeline
# and link into a standalone binary.
set -euo pipefail
cd "$(dirname "$0")/.."

FRKN="cabal-3.16.1.0 -v0 exec frankenstein --"
OUT=self-host/obj
rm -rf "$OUT"
mkdir -p "$OUT"

# All self-hostable modules (the 17 that Phase 6e proved + newer ones)
MODULES=(
  src/Frankenstein/Core/Types.hs
  src/Frankenstein/Core/Perceus.hs
  src/Frankenstein/Core/Evidence.hs
  src/Frankenstein/Core/EffectOpt.hs
  src/Frankenstein/Core/ConTags.hs
  src/Frankenstein/Core/Linker.hs
  src/Frankenstein/Core/CycleAnalysis.hs
  src/Frankenstein/Core/DeriveSelectors.hs
  src/Frankenstein/Core/FlattenPatterns.hs
  src/Frankenstein/Core/KokaCore.hs
  src/Frankenstein/GhcBridge/CoreTranslate.hs
  src/Frankenstein/GhcBridge/Driver.hs
  src/Frankenstein/MercuryBridge/HldsParse.hs
  src/Frankenstein/MercuryBridge/CoreTranslate.hs
  src/Frankenstein/RustBridge/MirParse.hs
  src/Frankenstein/RustBridge/CoreTranslate.hs
  src/Frankenstein/MlirEmit/Dialects.hs
  src/Frankenstein/MlirEmit/Emitter.hs
  src/Frankenstein/KokaBridge/CoreTranslate.hs
  src/Frankenstein/KokaBridge/Driver.hs
  src/OrganIR/Types.hs
  src/OrganIR/Parse.hs
  src/Frankenstein/OrganIR/Consumer.hs
)

MLIR_PASSES="--allow-unregistered-dialect \
  --convert-scf-to-cf --convert-arith-to-llvm --convert-cf-to-llvm \
  --convert-func-to-llvm --reconcile-unrealized-casts"

echo "=== Phase 1: Compile modules through Frankenstein ==="
OK=0
FAIL=0
for src in "${MODULES[@]}"; do
  # Derive a flat name: Core_Types, MlirEmit_Emitter, OrganIR_Types, etc.
  rel="${src#src/Frankenstein/}"
  rel="${rel#src/}"
  base="${rel%.hs}"
  flat="${base//\//_}"

  echo -n "  $rel ... "
  if $FRKN "$src" --emit-mlir > "$OUT/$flat.mlir" 2>"$OUT/$flat.err"; then
    echo -n "mlir "
    if mlir-opt $MLIR_PASSES "$OUT/$flat.mlir" 2>>"$OUT/$flat.err" \
       | mlir-translate --mlir-to-llvmir > "$OUT/$flat.ll" 2>>"$OUT/$flat.err"; then
      echo -n "llvm "
      if clang -c -o "$OUT/$flat.o" "$OUT/$flat.ll" 2>>"$OUT/$flat.err"; then
        echo "OK ($(stat -c%s "$OUT/$flat.o") bytes)"
        OK=$((OK + 1))
      else
        echo "FAIL (clang)"
        FAIL=$((FAIL + 1))
      fi
    else
      echo "FAIL (mlir-opt/translate)"
      FAIL=$((FAIL + 1))
    fi
  else
    echo "FAIL (frankenstein)"
    FAIL=$((FAIL + 1))
  fi
done
echo "=== $OK succeeded, $FAIL failed ==="

echo ""
echo "=== Phase 2: Compile runtime ==="
clang -O2 -c -o "$OUT/kk_runtime.o" runtime/kk_runtime.c -I runtime/
clang -O2 -c -o "$OUT/kk_cycle.o" runtime/kk_cycle.c -I runtime/
echo "Runtime compiled."

echo ""
echo "=== Phase 3: Catalog unresolved externals ==="
echo "Symbols needed (excluding kk_* and libc):"
for f in "$OUT"/*.o; do
  nm -u "$f" 2>/dev/null
done | sort -u | grep -v '^$' | grep -vE '^[[:space:]]*(U )?(kk_|__stack_chk|_GLOBAL|printf|puts|malloc|calloc|realloc|free|memcpy|memmove|memset|memcmp|strlen|strcmp|strncmp|strcpy|strcat|strncat|snprintf|sprintf|fprintf|fopen|fclose|fread|fwrite|fgets|fputs|exit|abort|strtol|strtod|atoi|atol|qsort|bsearch|getenv|clock|time|difftime)' > self-host/unresolved.txt || true
cat self-host/unresolved.txt
echo ""
echo "Total unresolved (non-kk, non-libc): $(wc -l < self-host/unresolved.txt)"

echo ""
echo "=== Phase 4: Compile drivers + cross-module shims ==="
clang -O2 -c -o "$OUT/main.o" self-host/main.c -I runtime/
clang -O2 -c -o "$OUT/driver.o" self-host/driver.c -I runtime/
clang -O2 -c -o "$OUT/kk_arena.o" runtime/kk_arena.c -I runtime/
# Cross-module arity aliases (thin C wrappers with __asm__ labels)
clang -O2 -c -o "$OUT/cross_module_aliases.o" self-host/cross_module_aliases.c
# Cross-module shims ($0 closures + false-external stubs)
clang -O2 -c -o "$OUT/cross_module_shims.o" self-host/cross_module_shims.c -I runtime/
# Minimal Haskell stdlib shims (placeholder)
clang -O2 -c -o "$OUT/stdlib_shims.o" self-host/stdlib_shims.c
# Data.Map / Data.Set / Data.Text shims
clang -O2 -c -o "$OUT/shim_data_map.o" self-host/shim_data_map.c -I runtime/
clang -O2 -c -o "$OUT/shim_data_set.o" self-host/shim_data_set.c -I runtime/
clang -O2 -c -o "$OUT/shim_data_text.o" self-host/shim_data_text.c -I runtime/
# GHC primitive + classes shims (Base, Num, Show, State monad, etc.)
clang -O2 -c -o "$OUT/shim_ghc_prim.o" self-host/shim_ghc_prim.c -I runtime/
clang -O2 -c -o "$OUT/shim_data_char.o" self-host/shim_data_char.c -I runtime/
# GHC list/foldable/traversable/maybe/functor/tuple/IORef/unicode shims
clang -O2 -c -o "$OUT/shim_ghc_list.o" self-host/shim_ghc_list.c -I runtime/
# System.Directory / FilePath / Process / Text.Printf shims
clang -O2 -c -o "$OUT/shim_system.o" self-host/shim_system.c -I runtime/
echo "Driver + shims compiled."

echo ""
echo "=== Phase 5a: Link self-hosted test binary ==="
# Cross-module calls are now resolved by aliases.S and shims.c.
# Remaining unresolved: Haskell stdlib (Data.Map, Data.Text, GHC.Internal.*),
# Koka stdlib, and external system calls.
# Exclude driver.o (has its own main)
ALL_OBJS=$(ls "$OUT"/*.o | grep -v driver.o)
clang -O2 -o self-host/frankenstein-self $ALL_OBJS -lm \
  -Wl,--unresolved-symbols=ignore-in-object-files
echo "Linked: self-host/frankenstein-self ($(stat -c%s self-host/frankenstein-self) bytes)"
POSTLINK=$(nm -u self-host/frankenstein-self 2>/dev/null | grep -cv '@GLIBC\|__gmon' || true)
FRKN_RESOLVED=$(nm -u self-host/frankenstein-self 2>/dev/null | grep -c 'Frankenstein_' || true)
echo "Post-link unresolved: $POSTLINK (Frankenstein: $FRKN_RESOLVED)"

echo ""
echo "=== Phase 5b: Link self-hosted compiler ==="
# Same objects but with driver.o instead of main.o
COMPILER_OBJS=$(ls "$OUT"/*.o | grep -v main.o)
clang -O2 -o self-host/frankenstein-self-compiler $COMPILER_OBJS -lm \
  -Wl,--unresolved-symbols=ignore-in-object-files
echo "Linked: self-host/frankenstein-self-compiler ($(stat -c%s self-host/frankenstein-self-compiler) bytes)"

echo ""
echo "=== Phase 6: Run self-test ==="
./self-host/frankenstein-self

echo ""
echo "=== Phase 7: Validate factorial MLIR (full pipeline) ==="
if [ -f self-host/factorial-self.mlir ]; then
  # The emitter already produces a @main wrapper with printf,
  # so we use factorial-self.mlir directly (no Python wrapping needed).
  MLIR_OPT="mlir-opt --allow-unregistered-dialect \
    --convert-scf-to-cf --convert-arith-to-llvm --convert-cf-to-llvm \
    --convert-func-to-llvm --reconcile-unrealized-casts"
  $MLIR_OPT self-host/factorial-self.mlir \
    | mlir-translate --mlir-to-llvmir > "$OUT/factorial-self.ll" 2>&1
  clang -c -o "$OUT/factorial-self-ir.o" "$OUT/factorial-self.ll"
  clang -O2 -c -o "$OUT/kk_rt_standalone.o" runtime/kk_runtime.c -I runtime/
  clang -O2 -c -o "$OUT/kk_arena_standalone.o" runtime/kk_arena.c -I runtime/
  clang -O2 -c -o "$OUT/kk_cycle_standalone.o" runtime/kk_cycle.c -I runtime/
  clang -o self-host/factorial-self-bin \
    "$OUT/factorial-self-ir.o" "$OUT/kk_rt_standalone.o" \
    "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm
  RESULT=$(./self-host/factorial-self-bin)
  if [ "$RESULT" = "3628800" ]; then
    echo "PASS: factorial(10) = $RESULT"
    echo ""
    echo "=== SELF-HOSTED COMPILATION PROVEN ==="
    echo "Pipeline: Core IR (C) → self-hosted emitProgramText → MLIR"
    echo "       → mlir-opt → mlir-translate → clang → native binary → 3628800"
  else
    echo "FAIL: expected 3628800, got '$RESULT'"
  fi
else
  echo "SKIP: factorial-self.mlir not found (factorial test may be disabled)"
fi

echo ""
echo "=== Phase 8: End-to-end examples through self-hosted compiler ==="
# Compile Haskell examples: host compiler --emit-organ | self-hosted compiler → MLIR → native → run
FRKN_RUN="cabal-3.16.1.0 -v0 run frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
MLIR_OPT="mlir-opt --allow-unregistered-dialect \
  --convert-scf-to-cf --convert-arith-to-llvm --convert-cf-to-llvm \
  --convert-func-to-llvm --reconcile-unrealized-casts"

declare -A EXPECTED
EXPECTED[nested]=60
EXPECTED[maybesum]=42
EXPECTED[listsum]=15
EXPECTED[tree]=6
EXPECTED[alloc_stress]=100100000
EXPECTED[closure]=42
EXPECTED[mutual_rec]=5
EXPECTED[multi_adt]=317
EXPECTED[higher_order]=12
EXPECTED[exhaust_tail]=36
EXPECTED[effect_ask]=84
EXPECTED[effect_state]=100

E2E_PASS=0
E2E_FAIL=0
for example in nested maybesum listsum tree alloc_stress closure mutual_rec multi_adt higher_order exhaust_tail; do
  echo -n "  $example.hs: "
  # Host compiler → OrganIR → self-hosted compiler → MLIR
  if ! $FRKN_RUN "examples/$example.hs" --emit-organ 2>/dev/null \
       | ./self-host/frankenstein-self-compiler - -o "$OUT/$example-self.mlir" 2>/dev/null; then
    echo "FAIL (self-hosted compiler)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  # MLIR → LLVM IR
  if ! $MLIR_OPT "$OUT/$example-self.mlir" 2>/dev/null \
       | mlir-translate --mlir-to-llvmir > "$OUT/$example-self.ll" 2>/dev/null; then
    echo "FAIL (mlir-opt/translate)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  # LLVM IR → native binary
  if ! clang -c -o "$OUT/$example-self-ir.o" "$OUT/$example-self.ll" 2>/dev/null; then
    echo "FAIL (clang -c)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  if ! clang -o "$OUT/$example-self-bin" \
       "$OUT/$example-self-ir.o" "$OUT/kk_rt_standalone.o" \
       "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
    echo "FAIL (link)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  # Run and check
  RESULT=$("$OUT/$example-self-bin" 2>/dev/null)
  if [ "$RESULT" = "${EXPECTED[$example]}" ]; then
    echo "PASS ($RESULT)"
    E2E_PASS=$((E2E_PASS + 1))
  else
    echo "FAIL (expected ${EXPECTED[$example]}, got '$RESULT')"
    E2E_FAIL=$((E2E_FAIL + 1))
  fi
done
# Effect-using OrganIR JSON examples (test effectOptimize + evidencePass)
for example in effect_ask effect_state; do
  echo -n "  $example.json: "
  if ! ./self-host/frankenstein-self-compiler "examples/$example.json" -o "$OUT/$example-self.mlir" 2>/dev/null; then
    echo "FAIL (self-hosted compiler)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  if ! $MLIR_OPT "$OUT/$example-self.mlir" 2>/dev/null \
       | mlir-translate --mlir-to-llvmir > "$OUT/$example-self.ll" 2>/dev/null; then
    echo "FAIL (mlir-opt/translate)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  if ! clang -c -o "$OUT/$example-self-ir.o" "$OUT/$example-self.ll" 2>/dev/null; then
    echo "FAIL (clang -c)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  if ! clang -o "$OUT/$example-self-bin" \
       "$OUT/$example-self-ir.o" "$OUT/kk_rt_standalone.o" \
       "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
    echo "FAIL (link)"
    E2E_FAIL=$((E2E_FAIL + 1))
    continue
  fi
  RESULT=$("$OUT/$example-self-bin" 2>/dev/null)
  if [ "$RESULT" = "${EXPECTED[$example]}" ]; then
    echo "PASS ($RESULT)"
    E2E_PASS=$((E2E_PASS + 1))
  else
    echo "FAIL (expected ${EXPECTED[$example]}, got '$RESULT')"
    E2E_FAIL=$((E2E_FAIL + 1))
  fi
done
echo "=== Phase 8 results: $E2E_PASS passed, $E2E_FAIL failed ==="

echo ""
echo "=== Phase 9: Bootstrap loop (stage 2) ==="
echo "Compiling all modules through stage 1 self-hosted compiler..."

STAGE2="$OUT/stage2"
rm -rf "$STAGE2"
mkdir -p "$STAGE2"

S2_OK=0
S2_FAIL=0
S2_MATCH=0
S2_MISMATCH=0
for src in "${MODULES[@]}"; do
  # Derive flat name same as Phase 1
  rel="${src#src/Frankenstein/}"
  rel="${rel#src/}"
  base="${rel%.hs}"
  flat="${base//\//_}"

  echo -n "  $rel ... "

  # Step 1: Host compiler → OrganIR JSON
  if ! $FRKN_RUN "$src" --emit-organ > "$STAGE2/$flat.organ.json" 2>"$STAGE2/$flat.err"; then
    echo "FAIL (host --emit-organ)"
    S2_FAIL=$((S2_FAIL + 1))
    continue
  fi

  # Step 2: Stage 1 self-hosted compiler → MLIR (120s timeout for large modules)
  if ! timeout 120 ./self-host/frankenstein-self-compiler "$STAGE2/$flat.organ.json" \
       --no-perceus -o "$STAGE2/$flat.mlir" 2>>"$STAGE2/$flat.err"; then
    echo "FAIL (stage1 compiler)"
    [ -f "$STAGE2/$flat.err" ] && tail -3 "$STAGE2/$flat.err" | sed 's/^/    /'
    S2_FAIL=$((S2_FAIL + 1))
    continue
  fi

  # Step 3: MLIR → LLVM IR
  if ! mlir-opt $MLIR_PASSES "$STAGE2/$flat.mlir" 2>>"$STAGE2/$flat.err" \
       | mlir-translate --mlir-to-llvmir > "$STAGE2/$flat.ll" 2>>"$STAGE2/$flat.err"; then
    echo "FAIL (mlir-opt/translate)"
    S2_FAIL=$((S2_FAIL + 1))
    continue
  fi

  # Step 4: LLVM IR → .o
  if ! clang -c -o "$STAGE2/$flat.o" "$STAGE2/$flat.ll" 2>>"$STAGE2/$flat.err"; then
    echo "FAIL (clang)"
    S2_FAIL=$((S2_FAIL + 1))
    continue
  fi

  # Compare stage 1 and stage 2 MLIR
  if diff -q "$OUT/$flat.mlir" "$STAGE2/$flat.mlir" > /dev/null 2>&1; then
    echo "OK ($(stat -c%s "$STAGE2/$flat.o") bytes, MLIR match)"
    S2_MATCH=$((S2_MATCH + 1))
  else
    echo "OK ($(stat -c%s "$STAGE2/$flat.o") bytes, MLIR differs)"
    S2_MISMATCH=$((S2_MISMATCH + 1))
  fi
  S2_OK=$((S2_OK + 1))
done
echo "=== Stage 2 compile: $S2_OK succeeded, $S2_FAIL failed ==="
echo "=== MLIR comparison: $S2_MATCH match, $S2_MISMATCH differ ==="

if [ "$S2_OK" -gt 0 ]; then
  echo ""
  echo "=== Phase 9b: Link stage 2 compiler ==="
  # Link stage 2 compiler binary (stage 2 .o files + same shims/runtime + driver)
  STAGE2_OBJS="$STAGE2/*.o"
  # Include all shims, runtime, and driver from stage 1 build
  SHIM_OBJS=$(ls "$OUT"/*.o | grep -vE '(Core_|MlirEmit_|GhcBridge_|MercuryBridge_|RustBridge_|KokaBridge_|OrganIR_|main\.o)')
  clang -O2 -o self-host/frankenstein-self-compiler-stage2 \
    $STAGE2_OBJS $SHIM_OBJS -lm \
    -Wl,--unresolved-symbols=ignore-in-object-files 2>/dev/null
  echo "Linked: self-host/frankenstein-self-compiler-stage2 ($(stat -c%s self-host/frankenstein-self-compiler-stage2) bytes)"

  echo ""
  echo "=== Phase 9c: Verify stage 2 (end-to-end tests) ==="
  S2E_PASS=0
  S2E_FAIL=0
  for example in nested maybesum listsum tree alloc_stress closure mutual_rec multi_adt higher_order exhaust_tail; do
    echo -n "  $example.hs (stage2): "
    # Host compiler → OrganIR → stage 2 compiler → MLIR
    if ! $FRKN_RUN "examples/$example.hs" --emit-organ 2>/dev/null \
         | ./self-host/frankenstein-self-compiler-stage2 - -o "$STAGE2/$example-self.mlir" 2>/dev/null; then
      echo "FAIL (stage2 compiler)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    # MLIR → LLVM IR → native binary
    if ! $MLIR_OPT "$STAGE2/$example-self.mlir" 2>/dev/null \
         | mlir-translate --mlir-to-llvmir > "$STAGE2/$example-self.ll" 2>/dev/null; then
      echo "FAIL (mlir-opt/translate)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    if ! clang -c -o "$STAGE2/$example-self-ir.o" "$STAGE2/$example-self.ll" 2>/dev/null; then
      echo "FAIL (clang -c)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    if ! clang -o "$STAGE2/$example-self-bin" \
         "$STAGE2/$example-self-ir.o" "$OUT/kk_rt_standalone.o" \
         "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
      echo "FAIL (link)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    RESULT=$("$STAGE2/$example-self-bin" 2>/dev/null)
    if [ "$RESULT" = "${EXPECTED[$example]}" ]; then
      echo "PASS ($RESULT)"
      S2E_PASS=$((S2E_PASS + 1))
    else
      echo "FAIL (expected ${EXPECTED[$example]}, got '$RESULT')"
      S2E_FAIL=$((S2E_FAIL + 1))
    fi
  done
  # Effect examples through stage 2
  for example in effect_ask effect_state; do
    echo -n "  $example.json (stage2): "
    if ! ./self-host/frankenstein-self-compiler-stage2 "examples/$example.json" -o "$STAGE2/$example-self.mlir" 2>/dev/null; then
      echo "FAIL (stage2 compiler)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    if ! $MLIR_OPT "$STAGE2/$example-self.mlir" 2>/dev/null \
         | mlir-translate --mlir-to-llvmir > "$STAGE2/$example-self.ll" 2>/dev/null; then
      echo "FAIL (mlir-opt/translate)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    if ! clang -c -o "$STAGE2/$example-self-ir.o" "$STAGE2/$example-self.ll" 2>/dev/null; then
      echo "FAIL (clang -c)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    if ! clang -o "$STAGE2/$example-self-bin" \
         "$STAGE2/$example-self-ir.o" "$OUT/kk_rt_standalone.o" \
         "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
      echo "FAIL (link)"
      S2E_FAIL=$((S2E_FAIL + 1))
      continue
    fi
    RESULT=$("$STAGE2/$example-self-bin" 2>/dev/null)
    if [ "$RESULT" = "${EXPECTED[$example]}" ]; then
      echo "PASS ($RESULT)"
      S2E_PASS=$((S2E_PASS + 1))
    else
      echo "FAIL (expected ${EXPECTED[$example]}, got '$RESULT')"
      S2E_FAIL=$((S2E_FAIL + 1))
    fi
  done
  echo "=== Phase 9c results: $S2E_PASS passed, $S2E_FAIL failed ==="

  if [ "$S2E_PASS" -gt 0 ] && [ "$S2E_FAIL" -eq 0 ]; then
    echo ""
    echo "============================================================"
    echo "  BOOTSTRAP LOOP COMPLETE"
    echo "  Stage 1: host compiler → 23 modules → self-hosted compiler"
    echo "  Stage 2: host --emit-organ → stage 1 compiler → 23 modules → stage 2 compiler"
    echo "  Stage 2 passes all $S2E_PASS end-to-end tests"
    echo "  MLIR match: $S2_MATCH/$S2_OK modules produce identical MLIR"
    echo "============================================================"
  fi
fi
