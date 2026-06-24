#!/usr/bin/env bash
# membuild.sh — run a heavy build inside a memory-bounded, serialized scope so
# it can't drag the whole machine into swap-thrash, and so two project shells
# never run big builds at the same time.
#
# Two guards:
#   * flock on a SHARED lock  -> only one heavy build runs at a time across all
#     your per-project Claude shells (point every project's runner at the same
#     LOCK path to serialize across projects).
#   * systemd --user --scope with Memory{High,Max} -> if this build balloons,
#     only it gets throttled/killed; your desktop and other shells stay alive.
#
# Usage:
#   self-host/membuild.sh                          # default: bash self-host/build.sh
#   self-host/membuild.sh bash self-host/build.sh
#   FRANKENSTEIN_EVIDENCE=plotkin self-host/membuild.sh
#   MEM_MAX=24G MEM_HIGH=18G self-host/membuild.sh <cmd...>
#
# Tunables (env):
#   MEM_HIGH  soft cap — reclaim pressure on the build past this (slows, not dies). default 24G
#   MEM_MAX   hard cap — build is killed if it exceeds this; machine survives.      default 36G
#   LOCK      shared lock file; use the SAME path in every project to serialize.    default ~/.cache/heavy-build.lock
#   NO_LOCK   set to 1 to skip serialization (still memory-bounded).
set -uo pipefail

MEM_HIGH="${MEM_HIGH:-24G}"
MEM_MAX="${MEM_MAX:-36G}"
LOCK="${LOCK:-$HOME/.cache/heavy-build.lock}"

[ "$#" -gt 0 ] || set -- bash self-host/build.sh

# Build the memory-bounding command prefix, if user-scope cgroups are available.
if systemd-run --user --scope --quiet -p MemoryMax=1M -- true 2>/dev/null; then
  BOUND=(systemd-run --user --scope --quiet -p MemoryHigh="$MEM_HIGH" -p MemoryMax="$MEM_MAX" --)
  echo "[membuild] memory-bounded: MemoryHigh=$MEM_HIGH MemoryMax=$MEM_MAX" >&2
else
  BOUND=()
  echo "[membuild] WARN: memory cgroup not delegated to user scope; running uncapped." >&2
fi

if [ "${NO_LOCK:-0}" = 1 ]; then
  exec "${BOUND[@]}" "$@"
else
  mkdir -p "$(dirname "$LOCK")"
  echo "[membuild] acquiring build lock: $LOCK (waits if another build holds it)" >&2
  exec flock "$LOCK" "${BOUND[@]}" "$@"
fi
