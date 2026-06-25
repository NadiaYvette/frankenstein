# tools/ — workload & dev-environment management

Cross-project build/test **resource management**, carried in-repo because
frankenstein is its own ultimate upstream. (Projects like pgcl can't carry this
— upstream Linux won't take their test/workload infra — but frankenstein,
telix, surd, etc. are unconstrained, so the scripts live with the project.)

These artifacts are **machine-wide**, not frankenstein-specific; frankenstein
just hosts the source of truth. The problem they solve: heavy self-host builds
(and other projects' test runs) were dragging a 62 GiB workstation into
swap-thrash and disturbing each other.

## Components

| File | Deploys to | Priv | Purpose |
|------|------------|------|---------|
| `membuild` | `~/.local/bin/membuild` (symlink) | user | Shared runner: places a build in `builds.slice` and takes a shared lock, so builds across every project shell can't thrash each other. |
| `systemd/builds.slice` | `~/.config/systemd/user/builds.slice` (symlink) | user | Shared cgroup budget: total **MemoryMax 44G / High 38G**, `CPUWeight`/`IOWeight` 40 (yield to interactive), and a systemd-oomd opt-in (kill a *thrashing build inside the slice*, not the desktop). |
| `sysctl.d/99-dev-swappiness.conf` | `/etc/sysctl.d/` | root | `vm.swappiness=10` — prefer RAM over disk-swap during builds. |
| `oomd.conf.d/10-dev-oomd.conf` | `/etc/systemd/oomd.conf.d/` | root | systemd-oomd acts on pressure (PSI 60% / 20s) and swap (85%) sooner, killing the worst offender. |
| `install.sh` | — | user (+sudo for `--system`) | Deploy everything. |

## Install

```sh
tools/install.sh            # user-level: symlinks + daemon-reload
tools/install.sh --system   # also the /etc sysctl + oomd drop-ins (sudo)
```

Idempotent. The user-level paths are symlinks back into this repo, so edits in
`tools/` are live — run `systemctl --user daemon-reload` after touching the
slice unit.

## Use

Prefix any heavy build/test with `membuild`, in any project, any shell:

```sh
membuild bash self-host/build.sh      # serialized + memory-bounded
MEM_MAX=24G membuild make -j8 test     # tighter per-build cap
NO_LOCK=1 membuild ./run-tests.sh      # concurrent, still slice-bounded
```

Adoption by another project is just that prefix — nothing else. By default
builds **serialize** (one at a time across all shells); the 44G slice cap is the
safety net under any concurrency. To let several run at once *fairly*, give each
project a `MEM_MAX` that sums within 44G (or carve per-project sub-slices).

## Verify

```sh
systemctl --user show builds.slice -p MemoryMax -p ManagedOOMMemoryPressureLimit
cat /proc/sys/vm/swappiness                 # -> 10
oomctl                                       # builds.slice listed when a build runs
systemd-cgtop                                # watch builds.slice live
```

## Tune

- Slice budget — `systemd/builds.slice` `MemoryMax`/`MemoryHigh`, then reload.
- Per-build caps — `MEM_MAX` / `MEM_HIGH` env to `membuild` (defaults 36G/24G).
- swappiness — raise toward 30–60 if page cache is dropped too aggressively.
- oomd — `SwapUsedLimit` / `DefaultMemoryPressureLimit` in the oomd drop-in.

## Revert

```sh
rm ~/.local/bin/membuild ~/.config/systemd/user/builds.slice
sudo rm -f /etc/sysctl.d/99-dev-swappiness.conf /etc/systemd/oomd.conf.d/10-dev-oomd.conf
sudo sysctl --system && sudo systemctl restart systemd-oomd
systemctl --user daemon-reload
```
