#!/usr/bin/env bash
# Phase 3d Benchmark Suite — Frankenstein vs GHC vs Rust vs Koka
set -euo pipefail
export LC_ALL=C

BENCH_DIR="$(cd "$(dirname "$0")" && pwd)"
BIN_DIR="$BENCH_DIR/bin"
RUNS=${1:-5}

echo "============================================================"
echo "  Frankenstein Phase 3d Benchmark Suite"
echo "  ${RUNS} runs per benchmark, reporting median"
echo "============================================================"
echo

# Extract wall-clock seconds from /usr/bin/time -v output
get_time_and_rss() {
    local binary="$1"
    local output
    output=$( { /usr/bin/time -v "$binary" > /dev/null; } 2>&1 )
    local wall
    wall=$(echo "$output" | grep "Elapsed (wall clock)" | sed 's/.*: //')
    # Convert h:mm:ss or m:ss.ss to seconds
    local secs
    secs=$(echo "$wall" | awk -F: '{
        if (NF==3) print $1*3600+$2*60+$3;
        else if (NF==2) print $1*60+$2;
        else print $1
    }')
    local rss
    rss=$(echo "$output" | grep "Maximum resident set size" | awk '{print $NF}')
    echo "$secs $rss"
}

# Run a benchmark N times and report median wall time + peak RSS
bench() {
    local label="$1"
    local binary="$2"
    local times=()
    local rsses=()

    for i in $(seq 1 "$RUNS"); do
        read -r t r <<< "$(get_time_and_rss "$binary")"
        times+=("$t")
        rsses+=("$r")
    done

    # Sort times and pick median
    local sorted_times
    sorted_times=$(printf '%s\n' "${times[@]}" | sort -g)
    local mid=$(( (RUNS - 1) / 2 ))
    local median_time
    median_time=$(echo "$sorted_times" | sed -n "$((mid+1))p")

    # Sort RSS and pick median
    local sorted_rss
    sorted_rss=$(printf '%s\n' "${rsses[@]}" | sort -g)
    local median_rss
    median_rss=$(echo "$sorted_rss" | sed -n "$((mid+1))p")

    printf "  %-20s %8ss  %6s KB\n" "$label" "$median_time" "$median_rss"
}

# Binary sizes
echo "--- Binary Sizes ---"
printf "  %-20s %10s\n" "Compiler" "Size"
for name in fib tak ack; do
    echo "  $name:"
    for comp in frank ghc rust koka; do
        local_bin="$BIN_DIR/${name}_${comp}"
        if [ -f "$local_bin" ]; then
            sz=$(stat -c%s "$local_bin")
            if [ "$sz" -gt 1048576 ]; then
                printf "    %-18s %6.1f MB\n" "$comp" "$(echo "scale=1; $sz/1048576" | bc)"
            else
                printf "    %-18s %6.1f KB\n" "$comp" "$(echo "scale=1; $sz/1024" | bc)"
            fi
        fi
    done
done
echo

# Benchmarks
for bench_name in fib tak ack; do
    case $bench_name in
        fib) desc="fibonacci(42)" ;;
        tak) desc="tak(24,16,8)" ;;
        ack) desc="ack(3,8)" ;;
    esac
    echo "--- $desc ---"
    printf "  %-20s %9s  %9s\n" "Compiler" "Time" "Peak RSS"
    for comp in frank ghc rust koka; do
        local_bin="$BIN_DIR/${bench_name}_${comp}"
        if [ -f "$local_bin" ] && [ -x "$local_bin" ]; then
            bench "$comp" "$local_bin"
        else
            printf "  %-20s %9s  %9s\n" "$comp" "N/A" "N/A"
        fi
    done
    echo
done

# Frankenstein profiling (RC counts) — compile with profile runtime
echo "--- Frankenstein RC Profile (single run each) ---"
FRANK_RT="$BENCH_DIR/../runtime"
for bench_name in fib tak ack; do
    case $bench_name in
        fib) src="$BENCH_DIR/Fib.hs" ;;
        tak) src="$BENCH_DIR/Tak.hs" ;;
        ack) src="$BENCH_DIR/Ack.hs" ;;
    esac

    # Use the .ll file from the normal compile, link with profile runtime
    ll_file="$BIN_DIR/${bench_name}_frank.ll"
    prof_bin="$BIN_DIR/${bench_name}_prof"

    if [ -f "$ll_file" ]; then
        clang -O2 "$ll_file" \
            "$FRANK_RT/kk_runtime_profile.c" \
            "$FRANK_RT/kk_cycle.c" \
            -o "$prof_bin" -lm 2>/dev/null

        echo "  $bench_name:"
        # Profile output goes to stderr
        "$prof_bin" > /dev/null 2> "$BIN_DIR/${bench_name}_profile.txt" || true
        cat "$BIN_DIR/${bench_name}_profile.txt" | sed 's/^/    /'
    fi
done
echo
echo "============================================================"
echo "  Done."
echo "============================================================"
