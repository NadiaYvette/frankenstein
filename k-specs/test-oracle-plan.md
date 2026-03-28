# K Framework Test Oracle Plan for Frankenstein

## Overview

Use K Framework language semantics as ground-truth oracles: run a program through
a K semantics, run the same program through Frankenstein, and compare results.
K semantics are mathematically rigorous executable specifications, so their
output is definitionally correct.

We have K v7.1.314 built at `~/src/k/result/bin/`.

---

## 1. Available K Semantics

### 1a. Bundled with K (ready to use NOW)

These ship inside the K distribution at `~/src/k/k-distribution/` and have been
verified to kompile and run on our K 7.1.314 build.

#### IMP (imperative toy language)

- **Location**: `~/src/k/k-distribution/tests/regression-new/imp-llvm/imp.k`
- **Test programs**: `sum.imp`, `collatz.imp`, `primes.imp` (same directory)
- **Features**: Integer variables, arithmetic (+, /, unary -), booleans (<=, !, &&),
  assignment, if/else, while, variable declarations
- **Status**: VERIFIED WORKING. `kompile imp.k --main-module IMP --syntax-module IMP-SYNTAX --backend llvm` then `krun sum.imp` produces `sum |-> 5050`.
- **Kompile time**: ~10 seconds
- **Relevance to Frankenstein**: Low overlap (IMP is not a real language), but
  useful as a sanity check for the oracle pipeline itself.

#### LAMBDA (untyped lambda calculus with let/letrec)

- **Location**: `~/src/k/k-distribution/tests/regression-new/pl-tutorial/1_k/1_lambda/lesson_8/lambda.k`
- **Test programs**: `tests/factorial-letrec.lambda`, `tests/arithmetic.lambda`, `tests/fibbo.lambda`, etc.
- **Features**: Lambda abstraction, application, integers, booleans, arithmetic,
  let, letrec (via mu), substitution-based reduction
- **Status**: VERIFIED WORKING. `letrec f x = if x <= 1 then 1 else (x * (f (x + -1))) in (f 10)` yields `3628800`.
- **Relevance to Frankenstein**: HIGH. This is essentially the pure functional
  core. Frankenstein's `--demo` also computes 10! = 3628800. This is the
  **first test oracle target**.

#### SIMPLE Untyped (imperative + functions + arrays + exceptions + threads)

- **Location**: `~/src/k/k-distribution/tests/regression-new/pl-tutorial/2_languages/1_simple/1_untyped/simple-untyped.md`
- **Test programs**: `tests/diverse/factorial.simple`, `tests/diverse/collatz.simple`,
  `tests/diverse/matrix.simple`, `tests/diverse/sortings.simple`,
  `tests/exceptions/exceptions_*.simple`
- **Features**: Functions (first-class), arrays, local scoping, I/O (read/write),
  exceptions (try/catch/throw), threads (spawn/join), locks, rendezvous
- **Status**: VERIFIED WORKING. `factorial.simple` with input 10 outputs
  `Factorial of 10 is: 3628800`.
- **Kompile flags**: `--main-module SIMPLE-UNTYPED --syntax-module SIMPLE-UNTYPED-SYNTAX`
- **Relevance to Frankenstein**: MEDIUM. Overlaps with the imperative subset that
  the GHC and Rust bridges can produce. Good for testing function calls, recursion,
  and exceptions once Frankenstein supports those in codegen.

#### SIMPLE Typed (static + dynamic variants)

- **Location**: `~/src/k/k-distribution/tests/regression-new/pl-tutorial/2_languages/1_simple/2_typed/`
- **Status**: Available but not yet tested; likely works since SIMPLE Untyped does.

#### IMP++ (IMP + I/O + threads + increment + strings)

- **Location**: `~/src/k/k-distribution/tests/regression-new/imp++-llvm/imp.md`
- **Status**: Available, not yet tested.

### 1b. External Repos (actively maintained, K7-compatible)

#### KWasm (WebAssembly semantics)

- **Repo**: https://github.com/runtimeverification/wasm-semantics
- **Last pushed**: 2026-03-26 (actively maintained)
- **K version pin**: 7.1.313 (one patch behind us; should be compatible)
- **Build**: Nix flake (`nix build`)
- **Relevance to Frankenstein**: MEDIUM-HIGH. Frankenstein emits MLIR which can
  go to Wasm via `mlir-translate`. Could validate the entire pipeline:
  Haskell source -> Frankenstein -> MLIR -> Wasm -> KWasm execution.
- **Complexity**: High setup cost (nix build, blockchain-k-plugin deps).

#### KEVM (Ethereum Virtual Machine semantics)

- **Repo**: https://github.com/runtimeverification/evm-semantics
- **Last pushed**: 2026-03-17 (actively maintained)
- **K version pin**: 7.1.313
- **Build**: Nix flake
- **Relevance to Frankenstein**: LOW. EVM bytecode is not a Frankenstein target.

### 1c. External Repos (stale, likely incompatible with K7)

These all target old K versions (K3-K5 era) and would require significant porting
effort to work with K 7.1.314. Not recommended for initial oracle work.

| Repo | Language | Last pushed | K version |
|------|----------|-------------|-----------|
| `kframework/c-semantics` | C11/C14 | 2022-02 | Old (pre-K7) |
| `kframework/java-semantics` | Java 1.4 | 2021-09 | k-legacy (archived) |
| `kframework/javascript-semantics` | ES5.1 | 2016-10 | Custom `kjs` branch |
| `kframework/llvm-semantics` | LLVM IR | 2018-06 | Old |
| `kframework/haskell-core-semantics` | GHC Core | 2017-06 | Old |
| `kframework/X86-64-semantics` | x86-64 | 2020-03 | Old |
| `kframework/solidity-semantics` | Solidity | 2019-10 | Old |
| `kframework/p4-semantics` | P4 | 2021-05 | Old |
| `kframework/ocaml-semantics` | OCaml | 2013-10 | Very old |
| `kframework/cink-semantics` | C++ kernel | 2015-04 | Old |

**Notable**: `kframework/haskell-core-semantics` would be the ideal oracle for
the GHC bridge (it defines GHC Core in K!), but it last saw commits in 2017 and
targets an ancient K version. Porting it to K7 could be a valuable future project.

---

## 2. Test Oracle Pipeline Design

### Architecture

```
Source Program (e.g., factorial.hs)
    |
    +---> [K Path] kompile semantics; krun program --> K output (ground truth)
    |
    +---> [Frankenstein Path] frankenstein file --compile; ./a.out --> Frankenstein output
    |
    v
  Compare outputs (diff / structured comparison)
```

### Concrete Steps

1. **Kompile** the K semantics once (cached in `*-kompiled/` directory):
   ```sh
   export PATH="$HOME/src/k/result/bin:$PATH"
   kompile lambda.k --main-module LAMBDA --syntax-module LAMBDA-SYNTAX --backend llvm
   ```

2. **Run the test program through K**:
   ```sh
   krun test-program.lambda
   ```
   K outputs the final configuration (a term). For LAMBDA, this is the reduced
   value. For IMP, this includes the final state map.

3. **Run the equivalent program through Frankenstein**:
   ```sh
   frankenstein equivalent-program.hs --compile && ./a.out
   ```

4. **Compare**: Extract the numeric/string result from both and assert equality.

### Output Normalization

K outputs full configurations in its cell notation (e.g., `<k> 3628800 ~> .K </k>`).
The comparison script must:
- Parse K output to extract the result value from the `<k>` cell
- Parse Frankenstein stdout (which is just the printed value)
- Compare numerically or as strings

A simple approach: `krun --output raw` or pipe through `grep`/`sed` to extract
the value.

---

## 3. Language Overlap Analysis

| Frankenstein Bridge | Relevant K Semantics | Overlap Quality |
|---------------------|---------------------|-----------------|
| GHC Bridge (Haskell -> Core) | LAMBDA (functional core), haskell-core-semantics (stale) | GOOD for pure functions; LAMBDA covers the arithmetic/recursion subset |
| Rust Bridge (MIR -> Core) | c-semantics (stale), SIMPLE (imperative subset) | PARTIAL; SIMPLE covers imperative with functions |
| Mercury Bridge (HLDS -> Core) | LAMBDA (logic subset maps to functional) | WEAK; no Mercury/Prolog semantics in K |
| MLIR Emitter (Core -> MLIR) | KWasm (if targeting Wasm output) | POSSIBLE but complex |

**Key insight**: The LAMBDA semantics is the most natural oracle because
Frankenstein's core IR is fundamentally a lambda calculus with effects. Any
program that can be expressed in both LAMBDA syntax and as a Haskell/Rust/Mercury
program can serve as a cross-validation test.

---

## 4. Concrete First Test

### Test: 10! (factorial) via LAMBDA and Frankenstein

This test is the lowest-friction starting point because:
- LAMBDA is already kompiled and verified working
- Frankenstein's `--demo` already computes 10! = 3628800
- The expected answer is identical: **3628800**

#### K side (already verified):

```sh
export PATH="$HOME/src/k/result/bin:$PATH"
cd /tmp/k-lambda-test2
# lambda.k is already kompiled
krun factorial-letrec.lambda
# Output: <k> 3628800 ~> .K </k>
```

The test program (`factorial-letrec.lambda`):
```
letrec f x = if x <= 1 then 1 else (x * (f (x + -1)))
in (f 10)
```

#### Frankenstein side (already verified):

```sh
cd ~/src/frankenstein
cabal -v0 run frankenstein -- --demo --compile && ./a.out
# Output: 3628800
```

#### Comparison:

```sh
K_RESULT=$(krun factorial-letrec.lambda 2>&1 | grep -oP '\d+(?= ~>)')
F_RESULT=$(cd ~/src/frankenstein && cabal -v0 run frankenstein -- --demo --compile 2>/dev/null && ./a.out)
[ "$K_RESULT" = "$F_RESULT" ] && echo "PASS: both produce $K_RESULT" || echo "FAIL: K=$K_RESULT Frankenstein=$F_RESULT"
```

### Proposed Next Tests (in order of difficulty)

1. **Arithmetic expressions**: Simple integer computations (no recursion).
   Write equivalent LAMBDA programs and Haskell one-liners.

2. **Recursive functions**: Fibonacci, GCD, Ackermann.
   These exercise the recursion/call machinery in both systems.

3. **IMP programs via Rust bridge**: Write a C program equivalent to `sum.imp`,
   compile with `rustc` (or `gcc`), run through Frankenstein's Rust bridge,
   compare final state with K's IMP output.

4. **SIMPLE programs**: Once Frankenstein supports function calls in codegen,
   use SIMPLE's richer test suite for validation.

5. **KWasm pipeline**: Once MLIR-to-Wasm is working, validate end-to-end
   through KWasm.

---

## 5. Setup Checklist

- [x] K v7.1.314 built at `~/src/k/result/bin/`
- [x] IMP semantics kompiles and runs
- [x] LAMBDA semantics kompiles and runs
- [x] SIMPLE Untyped semantics kompiles and runs
- [x] Frankenstein `--demo --compile` produces 3628800
- [ ] Create `k-specs/tests/` directory with test programs and expected outputs
- [ ] Write `k-specs/run-oracle.sh` comparison script
- [ ] Add IMP++ and SIMPLE Typed to verified set
- [ ] Investigate porting `haskell-core-semantics` to K7 (long-term)
- [ ] Investigate KWasm as a backend validation oracle (medium-term)

---

## Sources

- K Framework projects page: https://kframework.org/projects/
- K GitHub org: https://github.com/kframework
- Runtime Verification GitHub org: https://github.com/runtimeverification
- C semantics: https://github.com/kframework/c-semantics
- Java semantics: https://github.com/kframework/java-semantics
- JavaScript semantics: https://github.com/kframework/javascript-semantics
- LLVM semantics: https://github.com/kframework/llvm-semantics
- Haskell Core semantics: https://github.com/kframework/haskell-core-semantics
- KWasm: https://github.com/runtimeverification/wasm-semantics
- KEVM: https://github.com/runtimeverification/evm-semantics
