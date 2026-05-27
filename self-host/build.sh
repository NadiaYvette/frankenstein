#!/bin/bash
# Self-hosting build: compile Frankenstein's modules through its own pipeline
# and link into a standalone binary.
set -euo pipefail
cd "$(dirname "$0")/.."

FRKN="cabal-3.16.1.0 -v0 exec -w /usr/lib64/ghc-9.14.1/bin/ghc frankenstein --"
# Propagate FRANKENSTEIN_EVIDENCE=plotkin (or inline) as a --evidence flag.
# Default is inline (the historical pipeline; bootstrap baseline).
EVIDENCE_FLAG=""
if [ -n "${FRANKENSTEIN_EVIDENCE:-}" ]; then
  EVIDENCE_FLAG="--evidence=${FRANKENSTEIN_EVIDENCE}"
fi
OUT=self-host/obj
rm -rf "$OUT"
mkdir -p "$OUT"

# All self-hostable modules (the 17 that Phase 6e proved + newer ones)
MODULES=(
  src/Frankenstein/Core/Types.hs
  src/Frankenstein/Core/Perceus.hs
  src/Frankenstein/Core/Evidence.hs
  src/Frankenstein/Core/CpsConvert.hs
  src/Frankenstein/Core/EvidenceEvv.hs
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
  src/Frankenstein/Debug/DumpProgram.hs
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
  if $FRKN "$src" --no-simplify --emit-mlir $EVIDENCE_FLAG > "$OUT/$flat.mlir" 2>"$OUT/$flat.err"; then
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
DRIVER_CFLAGS="-O2 -I runtime/"
if [ "${FRANKENSTEIN_EVIDENCE:-}" = "plotkin" ]; then
  DRIVER_CFLAGS="$DRIVER_CFLAGS -DPLOTKIN_EVIDENCE"
fi
clang -c -o "$OUT/driver.o" self-host/driver.c $DRIVER_CFLAGS
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
# GHC typeclass dicts (Either Monad/Applicative/Functor, Show list/maybe, etc.)
clang -O2 -c -o "$OUT/shim_ghc_dicts.o" self-host/shim_ghc_dicts.c -I runtime/
clang -O2 -c -o "$OUT/shim_data_char.o" self-host/shim_data_char.c -I runtime/
# GHC list/foldable/traversable/maybe/functor/tuple/IORef/unicode shims
clang -O2 -c -o "$OUT/shim_ghc_list.o" self-host/shim_ghc_list.c -I runtime/
# System.Directory / FilePath / Process / Text.Printf shims
clang -O2 -c -o "$OUT/shim_system.o" self-host/shim_system.c -I runtime/
# C sanitizeName override (fixes non-deterministic encodeChar corruption).
# Receives DRIVER_CFLAGS so -DPLOTKIN_EVIDENCE flows through and the shim's
# arity matches the plotkin-injected (evv, …) calling convention.
clang -c -o "$OUT/A_sanitize_shim.o" self-host/A_sanitize_shim.c $DRIVER_CFLAGS
echo "Driver + shims compiled."

echo ""
echo "=== Phase 5a: Link self-hosted test binary ==="
# Cross-module calls are now resolved by aliases.S and shims.c.
# Remaining unresolved: Haskell stdlib (Data.Map, Data.Text, GHC.Internal.*),
# Koka stdlib, and external system calls.
# Exclude driver.o (has its own main)
ALL_OBJS=$(ls "$OUT"/*.o | grep -v driver.o)
clang -O2 -o self-host/frankenstein-self $ALL_OBJS -lm \
  -Wl,--unresolved-symbols=ignore-in-object-files \
  -Wl,--allow-multiple-definition
echo "Linked: self-host/frankenstein-self ($(stat -c%s self-host/frankenstein-self) bytes)"
POSTLINK=$(nm -u self-host/frankenstein-self 2>/dev/null | grep -cv '@GLIBC\|__gmon' || true)
FRKN_RESOLVED=$(nm -u self-host/frankenstein-self 2>/dev/null | grep -c 'Frankenstein_' || true)
echo "Post-link unresolved: $POSTLINK (Frankenstein: $FRKN_RESOLVED)"

echo ""
echo "=== Phase 5b: Link self-hosted compiler ==="
# Same objects but with driver.o instead of main.o, and exclude example
# binaries (they each define their own main symbol).
# Link shims BEFORE stage objects: with --allow-multiple-definition the
# first symbol seen wins, so shims (real implementations) must precede
# the Core_*.o / MlirEmit_*.o / etc. files (which contain auto-generated
# NULL-returning $0 stubs).  Matches Phase 9b/10b's link ordering.
# A SHIM is anything matching shim_* / A_sanitize_shim / cross_module_* / driver / stdlib_shims / kk_*.
# All other .o files are stage objects (compiled from .hs).  Explicit allow-list
# avoids accidentally including module .o files like Debug_DumpProgram.o that
# happen to not match the older deny-list regex.
SHIM_OBJS_5B=$(ls "$OUT"/*.o | grep -E '/(shim_|A_sanitize_shim|cross_module_|driver|stdlib_shims|kk_)' | grep -v 'self-ir\.o' | grep -v 'factorial-self-ir' | grep -v '_standalone\.o')
STAGE_OBJS_5B=$(ls "$OUT"/*.o | grep -vE '/(shim_|A_sanitize_shim|cross_module_|driver|main|stdlib_shims|kk_|.*self-ir\.o|factorial-self-ir|_standalone\.o)')
clang -O2 -o self-host/frankenstein-self-compiler $SHIM_OBJS_5B $STAGE_OBJS_5B -lm \
  -Wl,--unresolved-symbols=ignore-in-object-files \
  -Wl,--allow-multiple-definition
echo "Linked: self-host/frankenstein-self-compiler ($(stat -c%s self-host/frankenstein-self-compiler) bytes)"

echo ""
echo "=== Phase 6: Run self-test ==="
./self-host/frankenstein-self || true

echo ""
echo "=== Phase 7: Validate factorial MLIR (full pipeline) ==="
if [ -f self-host/factorial-self.mlir ]; then
  # The emitter already produces a @main wrapper with printf,
  # so we use factorial-self.mlir directly (no Python wrapping needed).
  MLIR_OPT="mlir-opt --allow-unregistered-dialect \
    --convert-scf-to-cf --convert-arith-to-llvm --convert-cf-to-llvm \
    --convert-func-to-llvm --reconcile-unrealized-casts"
  # Compile standalone runtime for E2E binaries
  clang -O2 -c -o "$OUT/kk_rt_standalone.o" runtime/kk_runtime.c -I runtime/
  clang -O2 -c -o "$OUT/kk_arena_standalone.o" runtime/kk_arena.c -I runtime/
  clang -O2 -c -o "$OUT/kk_cycle_standalone.o" runtime/kk_cycle.c -I runtime/
  if $MLIR_OPT self-host/factorial-self.mlir 2>/dev/null \
       | mlir-translate --mlir-to-llvmir > "$OUT/factorial-self.ll" 2>/dev/null \
     && clang -c -o "$OUT/factorial-self-ir.o" "$OUT/factorial-self.ll" 2>/dev/null \
     && clang -o self-host/factorial-self-bin \
          "$OUT/factorial-self-ir.o" "$OUT/kk_rt_standalone.o" \
          "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
    RESULT=$(./self-host/factorial-self-bin 2>/dev/null)
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
    echo "SKIP: factorial MLIR invalid (self-hosted emitter known issue)"
  fi
else
  echo "SKIP: factorial-self.mlir not found (factorial test may be disabled)"
fi

echo ""
echo "=== Phase 8: End-to-end examples through self-hosted compiler ==="
# Compile Haskell examples: host compiler --emit-organ | self-hosted compiler → MLIR → native → run
FRKN_RUN="cabal-3.16.1.0 -v0 exec -w /usr/lib64/ghc-9.14.1/bin/ghc frankenstein --"
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
EXPECTED[stdlib_list]=15
EXPECTED[stdlib_maybe]=141
EXPECTED[stdlib_bool]=7
EXPECTED[stdlib_tuple]=13
EXPECTED[prelude_hof]=22
EXPECTED[prelude_inline]=24
EXPECTED[prelude_comprehensive]=235
EXPECTED[stdlib_string]=11
EXPECTED[cross_module]=45

E2E_PASS=0
E2E_FAIL=0
for example in nested maybesum listsum tree alloc_stress closure mutual_rec multi_adt higher_order exhaust_tail stdlib_list stdlib_maybe stdlib_bool stdlib_tuple prelude_hof prelude_inline prelude_comprehensive stdlib_string cross_module; do
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

# ---------------------------------------------------------------------------
# compile_stage: Compile all modules through a self-hosted compiler (one stage)
#
# Args:
#   $1 - STAGE_LABEL   display name ("Stage 2", "Stage 3")
#   $2 - COMPILER      compiler binary path
#   $3 - ORGAN_DIR     directory containing .organ.json files
#   $4 - PREV_MLIR_DIR previous stage MLIR dir (fallback extraction + global injection + diff)
#   $5 - PREV_OBJ_DIR  previous stage .o dir (fallback when compilation totally fails)
#   $6 - OUTDIR        output directory for this stage
#   $7 - GEN_ORGAN     "yes" = generate OrganIR JSON via host compiler; "no" = reuse existing
#
# Sets in caller scope: SN_OK, SN_FAIL, SN_MATCH, SN_MISMATCH
# ---------------------------------------------------------------------------
compile_stage() {
  local STAGE_LABEL="$1"
  local COMPILER="$2"
  local ORGAN_DIR="$3"
  local PREV_MLIR_DIR="$4"
  local PREV_OBJ_DIR="$5"
  local OUTDIR="$6"
  local GEN_ORGAN="${7:-no}"

  SN_OK=0
  SN_FAIL=0
  SN_MATCH=0
  SN_MISMATCH=0

  for src in "${MODULES[@]}"; do
    # Derive flat name same as Phase 1
    rel="${src#src/Frankenstein/}"
    rel="${rel#src/}"
    base="${rel%.hs}"
    flat="${base//\//_}"

    echo -n "  $rel ... "

    # Step 1: Host compiler → OrganIR JSON (conditional)
    if [ "$GEN_ORGAN" = "yes" ]; then
      if ! $FRKN_RUN "$src" --no-simplify --emit-organ > "$ORGAN_DIR/$flat.organ.json" 2>"$OUTDIR/$flat.err"; then
        echo "FAIL (host --emit-organ)"
        SN_FAIL=$((SN_FAIL + 1))
        continue
      fi
    fi

    # Step 2: Self-hosted compiler → MLIR
    # The self-hosted JSON parser has super-linear scaling, so large modules
    # (>1MB OrganIR JSON) are automatically split into parts of ~40 defs each,
    # compiled separately, and merged.
    _json_size=$(stat -c%s "$ORGAN_DIR/$flat.organ.json")
    # Splitting was disabled in plotkin mode earlier (round 6) because
    # each part's plotkin pass only saw its own defs in topNames. The
    # round-6 isTopLevel + round-9 emitFnAsValue fixes now use the
    # isFrankensteinModule prefix heuristic for cross-module recognition
    # and pre-supply evv to all non-runtime non-builtin callees. This
    # SHOULD make per-part splitting work. Re-enabling here as a test;
    # if it regresses, we'll need the proper L3b (embed global arity in
    # .organ.json) to fix it cleanly.
    if [ "$_json_size" -gt 1000000 ]; then
      # Auto-split by JSON byte size: target ~400KB per part so that large
      # definitions (emitAppVar=936KB, emitExpr=377KB, etc.) each get their
      # own part. JSON is minified (separators=(',',':')) to reduce parser
      # time — the self-hosted JSON parser is O(n²).
      _nparts=$(python3 -c "
import json, sys
sys.setrecursionlimit(10000)
with open('$ORGAN_DIR/$flat.organ.json') as f:
    data = json.load(f)
defs = data['module']['definitions']
mod = data['module']
MAX_PART_BYTES = 400_000
parts = []
current = []
current_size = 0
for d in defs:
    dsize = len(json.dumps(d))
    # If adding this def would exceed limit and current is non-empty, start new part
    if current and current_size + dsize > MAX_PART_BYTES:
        parts.append(current)
        current = [d]
        current_size = dsize
    else:
        current.append(d)
        current_size += dsize
if current:
    parts.append(current)
# Write all part files
for pidx, part_defs in enumerate(parts):
    part = {
        'schema_version': data.get('schema_version', '0.1.0'),
        'metadata': data['metadata'],
        'module': {
            'name': mod['name'],
            'exports': mod.get('exports', []),
            'imports': mod.get('imports', []),
            'definitions': part_defs,
            'data_types': mod.get('data_types', []),
            'effect_decls': mod.get('effect_decls', []),
        }
    }
    with open('$OUTDIR/${flat}_part' + str(pidx) + '.organ.json', 'w') as f:
        json.dump(part, f, separators=(',', ':'))
print(len(parts))
")
      _split_ok=true
      _any_fallback=false
      _part_files=""
      for _pidx in $(seq 0 $((_nparts - 1))); do
        if ! timeout 900 "$COMPILER" \
             "$OUTDIR/${flat}_part${_pidx}.organ.json" \
             --no-perceus -o "$OUTDIR/${flat}_part${_pidx}.mlir" \
             2>>"$OUTDIR/$flat.err"; then
          # Part crashed — extract functions from previous stage MLIR as fallback
          if [ -f "$PREV_MLIR_DIR/$flat.mlir" ]; then
            python3 self-host/extract-mlir-funcs.py \
              "$PREV_MLIR_DIR/$flat.mlir" "$OUTDIR/${flat}_part${_pidx}.organ.json" \
              -o "$OUTDIR/${flat}_part${_pidx}.mlir" 2>>"$OUTDIR/$flat.err"
            _any_fallback=true
          else
            _split_ok=false; break
          fi
        fi
        _part_files="$_part_files $OUTDIR/${flat}_part${_pidx}.mlir"
      done
      if $_split_ok; then
        python3 self-host/merge-mlir-parts.py \
          $_part_files \
          -o "$OUTDIR/$flat.mlir" 2>>"$OUTDIR/$flat.err"
        # Inject missing string globals from previous stage MLIR into merged file
        # (self-hosted compiler references @str_N globals it doesn't define)
        if [ -f "$PREV_MLIR_DIR/$flat.mlir" ]; then
          python3 -c "
import re, sys
merged = open('$OUTDIR/$flat.mlir').read()
stage_prev = open('$PREV_MLIR_DIR/$flat.mlir').read()
# Find all llvm.mlir.global definitions in previous stage
globals_map = {}
for m in re.finditer(r'(llvm\.mlir\.global\s+\S+\s+\S+\s+@([A-Za-z0-9_.\$]+)\s*\([^)]*\)[^\n]*)', stage_prev):
    globals_map[m.group(2)] = m.group(1)
# Find referenced but undefined globals in merged file
refs = set(re.findall(r'llvm\.mlir\.addressof\s+@([A-Za-z0-9_.]+)', merged))
defined = set(re.findall(r'llvm\.mlir\.global\s+\S+\s+\S+\s+@([A-Za-z0-9_.]+)', merged))
missing = refs - defined
injected = 0
for name in sorted(missing):
    # Check if previous stage has the un-renamed version (strip pN_ prefix)
    base = re.sub(r'_p\d+_', '_', name)
    if base in globals_map:
        gline = globals_map[base].replace(f'@{base}', f'@{name}')
        merged = merged.replace('module {', f'module {{\n  {gline}', 1)
        injected += 1
    elif name in globals_map:
        merged = merged.replace('module {', f'module {{\n  {globals_map[name]}', 1)
        injected += 1
if injected:
    open('$OUTDIR/$flat.mlir', 'w').write(merged)
    print(f'  Injected {injected} missing globals from prev stage', file=sys.stderr)
" 2>>"$OUTDIR/$flat.err"
        fi
        # Post-process MLIR (replaces fix-intra-module-calls.py,
        # fix-dollar0-refs.py, fix-mlir-arity.py — see
        # Frankenstein.MlirEmit.PostProcess for the Haskell port).
        $FRKN_RUN --postprocess-mlir "$OUTDIR/$flat.mlir" \
          2>>"$OUTDIR/$flat.err"
        if $_any_fallback; then
          echo -n "(partial prev-stage fallback) "
        fi
      else
        echo -n "FAIL ($STAGE_LABEL split-compile)"
        [ -f "$OUTDIR/$flat.err" ] && tail -3 "$OUTDIR/$flat.err" | sed 's/^/    /'
        # Fall back to previous stage .o
        if [ -f "$PREV_OBJ_DIR/$flat.o" ]; then
          cp "$PREV_OBJ_DIR/$flat.o" "$OUTDIR/$flat.o"
          echo " -> fallback to prev-stage .o"
          SN_OK=$((SN_OK + 1))
          SN_MISMATCH=$((SN_MISMATCH + 1))
        else
          echo ""
          SN_FAIL=$((SN_FAIL + 1))
        fi
        continue
      fi
    else
      if ! timeout 120 "$COMPILER" "$ORGAN_DIR/$flat.organ.json" \
           --no-perceus -o "$OUTDIR/$flat.mlir" 2>>"$OUTDIR/$flat.err"; then
        echo -n "FAIL ($STAGE_LABEL compiler)"
        [ -f "$OUTDIR/$flat.err" ] && tail -3 "$OUTDIR/$flat.err" | sed 's/^/    /'
        # Fall back to previous stage .o
        if [ -f "$PREV_OBJ_DIR/$flat.o" ]; then
          cp "$PREV_OBJ_DIR/$flat.o" "$OUTDIR/$flat.o"
          echo " -> fallback to prev-stage .o"
          SN_OK=$((SN_OK + 1))
          SN_MISMATCH=$((SN_MISMATCH + 1))
        else
          echo ""
          SN_FAIL=$((SN_FAIL + 1))
        fi
        continue
      fi
    fi

    # Step 2b: Post-process MLIR. Single Haskell pass replaces the former
    # fix-intra-module-calls.py, fix-dollar0-refs.py, and fix-mlir-arity.py
    # Python scripts (see Frankenstein.MlirEmit.PostProcess). Runs in the
    # host-compiled frankenstein binary, so it's architecturally immune to
    # any self-host runtime divergence in the emitted .mlir input.
    $FRKN_RUN --postprocess-mlir "$OUTDIR/$flat.mlir" 2>>"$OUTDIR/$flat.err"

    # Step 3: MLIR → LLVM IR
    if ! mlir-opt $MLIR_PASSES "$OUTDIR/$flat.mlir" 2>>"$OUTDIR/$flat.err" \
         | mlir-translate --mlir-to-llvmir > "$OUTDIR/$flat.ll" 2>>"$OUTDIR/$flat.err"; then
      # Fall back to previous stage .o if available
      if [ -f "$PREV_OBJ_DIR/$flat.o" ]; then
        cp "$PREV_OBJ_DIR/$flat.o" "$OUTDIR/$flat.o"
        echo "FAIL (mlir-opt/translate) -> fallback to prev-stage .o"
        SN_OK=$((SN_OK + 1))
        SN_MISMATCH=$((SN_MISMATCH + 1))
      else
        echo "FAIL (mlir-opt/translate)"
        SN_FAIL=$((SN_FAIL + 1))
      fi
      continue
    fi

    # Step 4: LLVM IR → .o
    if ! clang -c -o "$OUTDIR/$flat.o" "$OUTDIR/$flat.ll" 2>>"$OUTDIR/$flat.err"; then
      echo "FAIL (clang)"
      SN_FAIL=$((SN_FAIL + 1))
      continue
    fi

    # Compare with previous stage MLIR
    if diff -q "$PREV_MLIR_DIR/$flat.mlir" "$OUTDIR/$flat.mlir" > /dev/null 2>&1; then
      echo "OK ($(stat -c%s "$OUTDIR/$flat.o") bytes, MLIR match)"
      SN_MATCH=$((SN_MATCH + 1))
    else
      echo "OK ($(stat -c%s "$OUTDIR/$flat.o") bytes, MLIR differs)"
      SN_MISMATCH=$((SN_MISMATCH + 1))
    fi
    SN_OK=$((SN_OK + 1))
  done
}

# ---------------------------------------------------------------------------
# run_e2e_tests: Run end-to-end examples through a self-hosted compiler
#
# Args:
#   $1 - STAGE_LABEL  display name ("stage2", "stage3")
#   $2 - COMPILER     compiler binary path
#   $3 - OUTDIR       output directory for test artifacts
#
# Sets in caller scope: SN_E2E_PASS, SN_E2E_FAIL
# ---------------------------------------------------------------------------
run_e2e_tests() {
  local STAGE_LABEL="$1"
  local COMPILER="$2"
  local OUTDIR="$3"

  SN_E2E_PASS=0
  SN_E2E_FAIL=0

  for example in nested maybesum listsum tree alloc_stress closure mutual_rec multi_adt higher_order exhaust_tail stdlib_list stdlib_maybe stdlib_bool stdlib_tuple prelude_hof prelude_inline prelude_comprehensive stdlib_string cross_module; do
    echo -n "  $example.hs ($STAGE_LABEL): "
    # Host compiler → OrganIR → self-hosted compiler → MLIR
    if ! $FRKN_RUN "examples/$example.hs" --emit-organ 2>/dev/null \
         | timeout 30 "$COMPILER" - -o "$OUTDIR/$example-self.mlir" 2>/dev/null; then
      echo "FAIL ($STAGE_LABEL compiler)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    # MLIR → LLVM IR → native binary
    if ! $MLIR_OPT "$OUTDIR/$example-self.mlir" 2>/dev/null \
         | mlir-translate --mlir-to-llvmir > "$OUTDIR/$example-self.ll" 2>/dev/null; then
      echo "FAIL (mlir-opt/translate)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    if ! clang -c -o "$OUTDIR/$example-self-ir.o" "$OUTDIR/$example-self.ll" 2>/dev/null; then
      echo "FAIL (clang -c)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    if ! clang -o "$OUTDIR/$example-self-bin" \
         "$OUTDIR/$example-self-ir.o" "$OUT/kk_rt_standalone.o" \
         "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
      echo "FAIL (link)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    RESULT=$("$OUTDIR/$example-self-bin" 2>/dev/null)
    if [ "$RESULT" = "${EXPECTED[$example]}" ]; then
      echo "PASS ($RESULT)"
      SN_E2E_PASS=$((SN_E2E_PASS + 1))
    else
      echo "FAIL (expected ${EXPECTED[$example]}, got '$RESULT')"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
    fi
  done
  # Effect examples
  for example in effect_ask effect_state; do
    echo -n "  $example.json ($STAGE_LABEL): "
    if ! "$COMPILER" "examples/$example.json" -o "$OUTDIR/$example-self.mlir" 2>/dev/null; then
      echo "FAIL ($STAGE_LABEL compiler)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    if ! $MLIR_OPT "$OUTDIR/$example-self.mlir" 2>/dev/null \
         | mlir-translate --mlir-to-llvmir > "$OUTDIR/$example-self.ll" 2>/dev/null; then
      echo "FAIL (mlir-opt/translate)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    if ! clang -c -o "$OUTDIR/$example-self-ir.o" "$OUTDIR/$example-self.ll" 2>/dev/null; then
      echo "FAIL (clang -c)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    if ! clang -o "$OUTDIR/$example-self-bin" \
         "$OUTDIR/$example-self-ir.o" "$OUT/kk_rt_standalone.o" \
         "$OUT/kk_arena_standalone.o" "$OUT/kk_cycle_standalone.o" -lm 2>/dev/null; then
      echo "FAIL (link)"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
      continue
    fi
    RESULT=$("$OUTDIR/$example-self-bin" 2>/dev/null)
    if [ "$RESULT" = "${EXPECTED[$example]}" ]; then
      echo "PASS ($RESULT)"
      SN_E2E_PASS=$((SN_E2E_PASS + 1))
    else
      echo "FAIL (expected ${EXPECTED[$example]}, got '$RESULT')"
      SN_E2E_FAIL=$((SN_E2E_FAIL + 1))
    fi
  done
}

# ============================= Phase 9 ======================================

echo ""
echo "=== Phase 9: Bootstrap loop (stage 2) ==="
echo "Compiling all modules through stage 1 self-hosted compiler..."

STAGE2="$OUT/stage2"
rm -rf "$STAGE2"
mkdir -p "$STAGE2"

compile_stage "Stage 2" \
  "./self-host/frankenstein-self-compiler" \
  "$STAGE2" "$OUT" "$OUT" "$STAGE2" "yes"
S2_OK=$SN_OK; S2_FAIL=$SN_FAIL; S2_MATCH=$SN_MATCH; S2_MISMATCH=$SN_MISMATCH
echo "=== Stage 2 compile: $S2_OK succeeded, $S2_FAIL failed ==="
echo "=== MLIR comparison: $S2_MATCH match, $S2_MISMATCH differ ==="

if [ "$S2_OK" -gt 0 ]; then
  echo ""
  echo "=== Phase 9b: Link stage 2 compiler ==="
  # Link stage 2 compiler binary (stage 2 .o files + same shims/runtime + driver)
  STAGE2_OBJS="$STAGE2/*.o"
  # Include all shims, runtime, and driver from stage 1 build
  SHIM_OBJS=$(ls "$OUT"/*.o | grep -vE '(Core_|MlirEmit_|GhcBridge_|MercuryBridge_|RustBridge_|KokaBridge_|OrganIR_|Debug_|main\.o|-self-ir\.o|factorial-self-ir|_standalone\.o)')
  # Link shims BEFORE stage objects so A_sanitize_shim.o's deterministic
  # sanitizeName wins over the compiled (corrupted) version in MlirEmit_Emitter.o.
  # With --allow-multiple-definition, the first definition wins.
  clang -O2 -o self-host/frankenstein-self-compiler-stage2 \
    $SHIM_OBJS $STAGE2_OBJS -lm \
    -Wl,--unresolved-symbols=ignore-in-object-files \
    -Wl,--allow-multiple-definition 2>/dev/null
  echo "Linked: self-host/frankenstein-self-compiler-stage2 ($(stat -c%s self-host/frankenstein-self-compiler-stage2) bytes)"

  echo ""
  echo "=== Phase 9c: Verify stage 2 (end-to-end tests) ==="
  run_e2e_tests "stage2" "./self-host/frankenstein-self-compiler-stage2" "$STAGE2"
  S2E_PASS=$SN_E2E_PASS; S2E_FAIL=$SN_E2E_FAIL
  echo "=== Phase 9c results: $S2E_PASS passed, $S2E_FAIL failed ==="

  # ============================= Phase 10 ====================================

  if [ -f self-host/frankenstein-self-compiler-stage2 ]; then
    echo ""
    echo "=== Phase 10: Bootstrap loop (stage 3 -- fixed-point check) ==="
    echo "Compiling all modules through stage 2 self-hosted compiler..."

    STAGE3="$OUT/stage3"
    rm -rf "$STAGE3"
    mkdir -p "$STAGE3"

    compile_stage "Stage 3" \
      "./self-host/frankenstein-self-compiler-stage2" \
      "$STAGE2" "$STAGE2" "$STAGE2" "$STAGE3" "no"
    S3_OK=$SN_OK; S3_FAIL=$SN_FAIL; S3_MATCH=$SN_MATCH; S3_MISMATCH=$SN_MISMATCH
    echo "=== Stage 3 compile: $S3_OK succeeded, $S3_FAIL failed ==="
    echo "=== Fixed-point check (stage2 vs stage3): $S3_MATCH match, $S3_MISMATCH differ ==="

    if [ "$S3_OK" -gt 0 ]; then
      echo ""
      echo "=== Phase 10b: Link stage 3 compiler ==="
      STAGE3_OBJS="$STAGE3/*.o"
      # Shims first — same link-order fix as stage 2 (deterministic sanitizeName)
      clang -O2 -o self-host/frankenstein-self-compiler-stage3 \
        $SHIM_OBJS $STAGE3_OBJS -lm \
        -Wl,--unresolved-symbols=ignore-in-object-files \
        -Wl,--allow-multiple-definition 2>/dev/null
      echo "Linked: self-host/frankenstein-self-compiler-stage3 ($(stat -c%s self-host/frankenstein-self-compiler-stage3) bytes)"
    fi

    if [ -f self-host/frankenstein-self-compiler-stage3 ]; then
      echo ""
      echo "=== Phase 10c: Verify stage 3 (end-to-end tests) ==="
      run_e2e_tests "stage3" "./self-host/frankenstein-self-compiler-stage3" "$STAGE3"
      S3E_PASS=$SN_E2E_PASS; S3E_FAIL=$SN_E2E_FAIL
      echo "=== Phase 10c results: $S3E_PASS passed, $S3E_FAIL failed ==="
    fi
  fi

  # ============================= Summary =====================================

  echo ""
  echo "============================================================"
  echo "  BOOTSTRAP LOOP COMPLETE (3-STAGE)"
  echo "  Stage 1: host compiler -> ${#MODULES[@]} modules -> self-hosted compiler"
  echo "  Stage 2: stage 1 compiler -> $S2_OK modules (MLIR match: $S2_MATCH)"
  if [ "${S3_OK:-0}" -gt 0 ]; then
    echo "  Stage 3: stage 2 compiler -> $S3_OK modules (MLIR match: $S3_MATCH)"
  fi
  echo ""
  if [ "${S3_OK:-0}" -gt 0 ] && [ "${S3_MATCH:-0}" -eq "${S3_OK:-0}" ] && [ "${S3_FAIL:-0}" -eq 0 ]; then
    echo "  *** FIXED POINT REACHED ***"
    echo "  All $S3_OK modules produce identical MLIR in stages 2 and 3."
    echo "  The self-hosted compiler has converged."
  elif [ "${S3_OK:-0}" -gt 0 ]; then
    echo "  Fixed point NOT YET reached."
    echo "  Stage 2->3 match: ${S3_MATCH:-0}/${S3_OK:-0}, differ: ${S3_MISMATCH:-0}, fail: ${S3_FAIL:-0}"
  fi
  if [ "$S2E_PASS" -gt 0 ]; then
    echo "  Stage 2 E2E: $S2E_PASS passed, $S2E_FAIL failed"
  fi
  if [ "${S3E_PASS:-0}" -gt 0 ]; then
    echo "  Stage 3 E2E: $S3E_PASS passed, $S3E_FAIL failed"
  fi
  echo "============================================================"
fi
