# Test Coverage Expansion Plan

**Status: design doc, no code written yet.** Captures three layered
test-base expansions, all building on each other.  Future sessions pick
up here.

## Motivation

The current test base proves end-to-end *function* through every bridge
(`arith.<ext>` returns 42 in each of 14 source languages, 21 self-host
E2E tests, polyglot demos through 12 bridges).  It does **not** prove
that the bridges' standard libraries — particularly C-backed shims — honor
the runtime invariants Perceus depends on (refcount discipline, cycle
candidate marking, tag/arity ABI consistency).

The shims today *do* mostly honor those invariants (see "Shim discipline
audit" below — the framing "shims leak memory" was off-base).  What's
not yet proven by tests is whether every code path through every shim
under load continues to honor them.  Cycle-collector bugs from
undercount, segfaults from tag mismatch, and runtime leaks from
forgotten `kk_drop` are all known failure shapes; the question is
whether any of them are lurking.

## Three layered phases

### Phase A: hello-world per language — SHIPPED easy-five (2026-05-19)

**Driver**: `test-hellos.sh` (run from repo root).  5/5 pass.

| Bridge | File | Mode | Output |
|---|---|---|---|
| Koka | `examples/hello.kk` | native string via `kk_println_str` | "Hello, World!" |
| Python | `examples/hello.py` | native string via `println_str` | "Hello, world" |
| Haskell | `examples/hello.hs` | native `putStrLn` → `kk_println_haskell_chars` | "Hello, World!" |
| Rust | `examples/hello.rs` | native `println!` → `kk_print_str` | "Hello, World!" |
| Mercury | `examples/hello.m` | native `io.write_string` → `kk_print_str` | "Hello, World!" |

All five hellos now use the natural source form for their language and
print real strings.  The original Phase A "easy five" was a 1-3h scope
estimate; closing the three follow-up string-ABI gaps (BRIDGE_haskell_strings,
BRIDGE_rust_strings, BRIDGE_mercury_strings) took the same session.

Chained IO actions (3-line do-blocks / sequential calls) also work for
Haskell, Rust, and Mercury — see `examples/chained_io.{hs,rs,m}`.

Haskell `show :: Int -> String` and `print :: Int -> IO ()` work via
the GHC bridge's `isShowIntWorker` intercept — see
`examples/show_int.hs`.  Show for [Int] and Maybe Int works via
`isShowIntListMethod` + `knownShowCAF` — see `examples/show_compound.hs`.
`deriving Show` for user ADTs works for the three common shapes —
see `examples/show_derived.hs`.  Show for tuples (2-tuples through
n-tuples, including negatives and nesting) — see
`examples/show_tuple.hs`.  Mixing enum + with-args ADTs in one
module — see `examples/show_mixed.hs`.  Rust formatted `println!`
with placeholders — see `examples/rust_fmt.rs` (Display),
`examples/rust_dbg.rs` (Debug), `examples/rust_radix.rs` (radix).
Test driver runs 16/16 hellos.

Remaining out-of-scope-for-hello-world gaps are listed in ROADMAP:
Rust field-spec syntax with applied width/precision, non-i64
numeric format types, file/stdin handles, full UTF-8 re-encoding.

**Goal**: prove each bridge's string ABI is wired end-to-end.

Today only Python has explicit `hello*.py` files.  Every other bridge
has `arith.<ext>` returning `Int`.  String literals, concat, and print
exercise a different code path through the bridge — string literal
desugaring, `kk_str_alloc_leaf_owned` plumbing, runtime string
registration — that numeric tests don't cover.

**Scope**: one `examples/hello.<ext>` per bridge, plus a
`test-hellos.sh` driver that compiles and runs each.  Easy bridges
first (Haskell, Koka, Rust, Mercury, Python — known to work),
incrementally pick up the rest.  Some bridges will reveal string-ABI
gaps; document those as `TODO: BRIDGE_<lang>_strings` in ROADMAP.

**Effort**: ~1-3h for the easy five.  Open-ended for languages with
gaps.  See user's notes (2026-05-19 thread) for the full discussion.

**Constraint on `main` location**: single-language tests can put `main`
in their natural per-bridge form (Haskell `main :: Int`, Rust
`fn main() -> i64`, Mercury `main(!IO)`, etc.).  Multi-language
polyglot programs require `main` in **Koka** — that's the orchestrator
language for the polyglot linker (see `examples/polyglot-demo/`,
where all entry files are `*.kk`).

### Phase B: standard-library coverage

**Goal**: exercise the per-bridge stdlib paths that go through C shims,
under workloads big enough to flush out latent bugs.

The shims that need coverage:

| Shim | Implements | Discipline summary |
|---|---|---|
| `self-host/shim_data_map.c` | `Data.Map.insert`/`lookup`/`size`/etc. | `kk_alloc_con` MAP_TIP/MAP_BIN tags + `kk_retain` on stored keys/vals.  Looks correct. |
| `self-host/shim_data_set.c` | `Data.Set.insert`/`member`/etc. | Same shape as Map.  Looks correct. |
| `self-host/shim_data_text.c` | `Data.Text.pack`/`unlines`/etc. | `malloc` raw byte buffers, hand off via `kk_str_alloc_leaf_owned` with `owns_bytes=1`; `kk_str_drop` calls `free()` at rc==0 (`runtime/kk_runtime.c:671`).  Properly tracked. |
| `self-host/shim_ghc_list.c` | `(:)`/`[]`/`map`/`filter`/etc. | `kk_alloc_con` CLOS_TAG closures + `kk_retain` discipline.  71 retain/alloc calls. |
| `self-host/shim_ghc_prim.c` | `Num`/`Show` class instances, `error` | Same shape. |
| `self-host/shim_system.c` | `System.Directory`/`FilePath`/`Process`/`Text.Printf` | Mostly thin wrappers around libc.  String ownership transfers need audit. |
| `self-host/shim_data_char.c` | `Data.Char` | Numeric — low risk. |
| `self-host/stdlib_shims.c` | catch-all placeholders | May contain stubs that return `0`.  Audit risk. |
| `self-host/A_sanitize_shim.c` | overrides `sanitizeName` because compiled version corrupts characters | Out-of-discipline by design; works because no heap interaction. |

**Tests we'd want**:
- For each Map/Set operation: build a structure of size N, traverse, mutate, drop.  Assert final heap leak count == 0 (the runtime's allocation counter exists in profile mode).
- For each Text operation: build long ropes, concatenate, slice, drop.  Same assertion.
- For each list HOF: long lists through map/filter/foldr.  Same assertion.

**Effort**: ~4-8h for an initial run.  Each shim that reveals a bug
becomes its own follow-up.

### Phase C: cross-shim and cross-language audit

**Goal**: catch bugs that only appear when a value crosses a shim → consumer
boundary or a language → language boundary.

Examples of the failure mode:
- Haskell shim returns a `Data.Map` to Koka code that doesn't know
  the MAP_BIN tag — Koka assumes a different ADT layout, reads field 0
  expecting a key, gets a left subtree pointer.
- Rust calls a Haskell shim that returns an owned `Text`; Rust doesn't
  call `kk_str_drop` on exit (Rust's `Drop` impl runs but the FFI
  binding might miss it).
- Cycle collector runs while a shim is mid-allocation; the partial
  object isn't yet rooted but is reachable through a thread-local.

**Tests we'd want**:
- Each polyglot-demo program compiled with the cycle collector
  configured to run after every allocation (slow but conclusive).
- For each bridge → bridge data passing test: pass a structure across
  the boundary in both directions, mutate, drop, check leak count.
- Optionally: a fuzz mode in the QC differential driver that generates
  programs combining 2-3 random bridges and asserts runtime invariants
  hold throughout.

**Effort**: open-ended; the per-test work is small, but enumeration is
combinatorial.  Probably want to start with the 12-lang polyglot
demos and add invariant assertions as a wrapper.

## Connection to existing infrastructure

- The **QC differential test driver** (`tools/diff-tester/`) already
  has a `host-runtime-vs-stage2-runtime` mode that compiles + runs
  generated programs through two pipelines and compares exit codes.
  Adding a "runtime-invariant" assertion mode (cycle-collector clean,
  leak count == 0 at exit) is a small extension.
- The **K verification** suites already prove Perceus invariants at the
  theoretical level (20 perceus-claims).  Property-based tests at the
  language level give us empirical evidence on the *implementation*
  side, complementing the K theorems.
- The **bootstrap E2E suite** (21 programs through 3 stages) covers
  function but not invariants.  Adding leak-count assertions to that
  suite is a one-line change per test.

## What's worth doing first

When this work picks up: **Phase A's easy five** (Haskell, Koka, Rust,
Mercury, Python hello-worlds + a `test-hellos.sh` driver).  Cheap,
self-contained, and lays the file-layout pattern that Phases B and C
inherit.

Then Phase B's `shim_data_map.c` coverage as the next decision point —
if Map is clean under stress, the discipline framing is validated; if
it leaks, we have a concrete bug to chase before going broader.
