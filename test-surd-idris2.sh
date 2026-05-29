#!/bin/bash
# Regression guard for surd-idris2 byte-identity-with-native.
#
# Builds each surd demo's ipkg through frankenstein-idris2 (custom
# `organir` codegen) to produce OrganIR JSON, then feeds that through
# the host frankenstein binary to compile to a native executable.
# Compares byte-for-byte against the same demo built natively by the
# system idris2.
#
# Usage:
#   bash test-surd-idris2.sh           # runs all, reports counts
#   bash test-surd-idris2.sh --strict  # nonzero exit if any non-zero diff
#
# Targets: surd-trig, surd-euler, surd-elliptic, surd-quintic.
# (surd-test runs 58/58 unit tests but its output volume + runtime make
# it less suitable as a tight regression target — invoke separately.)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
SURD_DIR="${SURD_DIR:-/home/nyc/src/surd/idris2}"
SHIM_BIN="${SHIM_BIN:-$SCRIPT_DIR/idris2-shim/build/exec/frankenstein-idris2}"
FRANK="cabal-3.16.1.0 -v0 exec frankenstein -w /usr/lib64/ghc-9.14.1/bin/ghc --"
IDRIS2_PREFIX="${IDRIS2_PREFIX:-/usr/lib64}"
STRICT="no"
[ "${1:-}" = "--strict" ] && STRICT="yes"

if [ ! -d "$SURD_DIR" ]; then
    echo "SURD_DIR=$SURD_DIR not found; set SURD_DIR env var to point to surd/idris2" >&2
    exit 2
fi

if [ ! -x "$SHIM_BIN" ]; then
    echo "SHIM_BIN=$SHIM_BIN not executable; build idris2-shim first" >&2
    exit 2
fi

# Each entry is "<ipkg-basename> <executable-name>"
TARGETS=(
    "surd-trig surd-trig"
    "surd-euler surd-euler"
    "surd-elliptic surd-elliptic"
    "surd-quintic surd-quintic"
)

PASS=0
FAIL=0
FAIL_LIST=""

echo "========================================"
echo " Frankenstein surd-idris2 regression"
echo "========================================"

cd "$SURD_DIR"

# Build native references if missing
for entry in "${TARGETS[@]}"; do
    exe="${entry##* }"
    if [ ! -x "build/exec/$exe" ]; then
        ipkg="${entry%% *}.ipkg"
        echo "Building native $exe (from $ipkg)..."
        IDRIS2_PREFIX="$IDRIS2_PREFIX" idris2 --build "$ipkg" > /dev/null 2>&1 \
            || echo "  WARN: native build failed for $exe"
    fi
done

for entry in "${TARGETS[@]}"; do
    ipkg_base="${entry%% *}"
    exe="${entry##* }"
    printf "  %-30s" "$exe"

    native="$SURD_DIR/build/exec/$exe"
    if [ ! -x "$native" ]; then
        echo "SKIP (no native binary)"
        continue
    fi

    # Step 1: idris2-shim → organ.json
    # Use a fresh build/exec dir so we don't conflict with the native build.
    rm -f "$SURD_DIR/build/exec/$exe.organ.json"
    IDRIS2_PREFIX="$IDRIS2_PREFIX" "$SHIM_BIN" --cg organir --build "$ipkg_base.ipkg" \
        > /tmp/idris2-shim-$exe.log 2>&1
    organ_json="$SURD_DIR/build/exec/$exe.organ.json"
    if [ ! -f "$organ_json" ]; then
        echo "FAIL (shim emit)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $exe(shim)"
        continue
    fi

    # Step 2: frankenstein --from-organ --compile
    bin="/tmp/i-$exe-regression"
    rm -f "$bin"
    (cd "$SCRIPT_DIR" && $FRANK --from-organ --compile -o "$bin" "$organ_json") \
        > /tmp/frank-$exe.log 2>&1
    if [ ! -x "$bin" ]; then
        echo "FAIL (Frank compile)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $exe(compile)"
        continue
    fi

    # Step 3: byte-diff native vs frankenstein output.
    # surd-quintic needs a longer timeout + the arena freelist:
    # it does heavy polynomial-arithmetic + numerical-root-finding
    # for 7 degree-3..7 polynomials and on frankenstein takes ~7
    # minutes vs <1s on native (Perceus refcount overhead + boxed
    # everything vs chez's tracing GC + unboxed flonums).  Bumping
    # to 600 s + setting KK_RECYCLE=1 (arena freelist reclamation)
    # gets it to byte-identical with native.  The other demos
    # finish in <30 s either way.
    case "$exe" in
        surd-quintic) bin_timeout=600 ;;
        *)            bin_timeout=90  ;;
    esac
    DL=$(diff <(timeout 90 "$native" 2>&1) \
              <(KK_RECYCLE=1 timeout "$bin_timeout" "$bin" 2>&1) 2>&1 | wc -l)
    if [ "$DL" -eq 0 ]; then
        echo "PASS  (byte-identical)"
        PASS=$((PASS + 1))
    else
        echo "FAIL  ($DL diff lines)"
        FAIL=$((FAIL + 1))
        FAIL_LIST="$FAIL_LIST $exe($DL)"
    fi
done

echo "========================================"
echo "  $PASS passed, $FAIL failed"
if [ -n "$FAIL_LIST" ]; then
    echo "  failing: $FAIL_LIST"
fi
echo "========================================"

if [ "$STRICT" = "yes" ] && [ "$FAIL" -gt 0 ]; then
    exit 1
fi
