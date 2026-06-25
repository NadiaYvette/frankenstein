#!/usr/bin/env bash
# Deploy frankenstein's workload-management tooling to its live locations.
#
#   tools/install.sh            user-level: symlink membuild + builds.slice, reload
#   tools/install.sh --system   also install the /etc sysctl + oomd drop-ins (sudo)
#
# Idempotent; safe to re-run.  This repo is the source of truth — the user-level
# paths are symlinks back here, so edits in tools/ take effect immediately (run
# `systemctl --user daemon-reload` after editing the slice unit).
set -euo pipefail
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "[install] user-level (no privilege)…"
mkdir -p "$HOME/.local/bin" "$HOME/.config/systemd/user"
ln -sfn "$HERE/membuild"             "$HOME/.local/bin/membuild"
ln -sfn "$HERE/systemd/builds.slice" "$HOME/.config/systemd/user/builds.slice"
systemctl --user daemon-reload
echo "  membuild     -> ~/.local/bin/membuild"
echo "  builds.slice -> ~/.config/systemd/user/builds.slice (reloaded)"
case ":$PATH:" in
  *":$HOME/.local/bin:"*) ;;
  *) echo "  WARN: ~/.local/bin is not on PATH — add it so 'membuild' resolves." ;;
esac

if [ "${1:-}" = "--system" ]; then
  echo "[install] system-level (sudo)…"
  sudo install -D -m0644 "$HERE/sysctl.d/99-dev-swappiness.conf" /etc/sysctl.d/99-dev-swappiness.conf
  sudo sysctl --system >/dev/null
  sudo install -D -m0644 "$HERE/oomd.conf.d/10-dev-oomd.conf"    /etc/systemd/oomd.conf.d/10-dev-oomd.conf
  sudo systemctl restart systemd-oomd
  echo "  /etc/sysctl.d/99-dev-swappiness.conf       (vm.swappiness now $(cat /proc/sys/vm/swappiness))"
  echo "  /etc/systemd/oomd.conf.d/10-dev-oomd.conf   (systemd-oomd restarted)"
else
  echo "[install] system tweaks skipped — apply with:  tools/install.sh --system"
fi
echo "[install] done."
