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
)

MLIR_PASSES="--allow-unregistered-dialect --reconcile-unrealized-casts \
  --convert-scf-to-cf --convert-arith-to-llvm --convert-cf-to-llvm \
  --convert-func-to-llvm --reconcile-unrealized-casts"

echo "=== Phase 1: Compile modules through Frankenstein ==="
OK=0
FAIL=0
for src in "${MODULES[@]}"; do
  # Derive a flat name: Core_Types, MlirEmit_Emitter, etc.
  rel="${src#src/Frankenstein/}"
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
echo "=== Phase 4: Compile driver + cross-module shims ==="
clang -O2 -c -o "$OUT/main.o" self-host/main.c -I runtime/
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
# GHC list/foldable/traversable/maybe/functor/tuple/IORef/unicode shims
clang -O2 -c -o "$OUT/shim_ghc_list.o" self-host/shim_ghc_list.c -I runtime/
# System.Directory / FilePath / Process / Text.Printf shims
clang -O2 -c -o "$OUT/shim_system.o" self-host/shim_system.c -I runtime/
echo "Driver + shims compiled."

echo ""
echo "=== Phase 5: Link self-hosted binary ==="
# Cross-module calls are now resolved by aliases.S and shims.c.
# Remaining unresolved: Haskell stdlib (Data.Map, Data.Text, GHC.Internal.*),
# Koka stdlib, and external system calls.
clang -O2 -o self-host/frankenstein-self "$OUT"/*.o -lm \
  -Wl,--unresolved-symbols=ignore-in-object-files
echo "Linked: self-host/frankenstein-self ($(stat -c%s self-host/frankenstein-self) bytes)"
POSTLINK=$(nm -u self-host/frankenstein-self 2>/dev/null | grep -cv '@GLIBC\|__gmon' || true)
FRKN_RESOLVED=$(nm -u self-host/frankenstein-self 2>/dev/null | grep -c 'Frankenstein_' || true)
echo "Post-link unresolved: $POSTLINK (Frankenstein: $FRKN_RESOLVED)"

echo ""
echo "=== Phase 6: Run self-test ==="
./self-host/frankenstein-self
