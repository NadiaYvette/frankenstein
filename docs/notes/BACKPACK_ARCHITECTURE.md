# Backpack Module Architecture Across the Nadiverse (`smirk`, `cek`, `gargamelle`, `frankenstein`)

**Status:** Canonical Architecture Note  
**Constellation:** Cellarius / PykinosCEK / Ingolstadt  
**Policy Alignment:** `WORKSPACE_BUILD_HYGIENE.md` §4.3 (Stackage Nightly Quarantine)  
**Target Compilers:** GHC 9.12.4 (Stackage Nightly), GHC 9.14.1 (Primary Dev)  
**Date:** 2026-11-23  

---

## 1. Strategic Role & Two-Tier Policy (D1)

Backpack (Kilpatrick, Dreyer, Peyton Jones, Marlow, POPL 2014; Yang 2017) provides static mix-in linking, categorical signature merging (pushouts), applicative type identity, and guaranteed whole-unit GHC Core monomorphization with cross-boundary `{-# UNPACK #-}`—solving performance and modularity bottlenecks that typeclasses cannot solve without fragile `SPECIALIZE` pragmas.

To harvest these gains without compromising Stackage Nightly (`ghc-9.12.4`), Stack, or HLS stability, the constellation enforces a **two-tier Backpack policy**:

| Tier | Pattern | Visibility & Stackage Status | Allowed Repositories |
| :--- | :--- | :--- | :--- |
| **Tier 1: Private Backpack** *(Production Standard)* | Indefinite `.hsig` units and concrete adapter modules live strictly inside **Cabal internal sublibraries** (`library <name>-indef`, `library <name>-<impl>`). The main public `library` instantiates them via `mixins:` and `reexported-modules:`, exposing **100% definite modules**. | **Fully Stackage Nightly, Stack, and Hackage compatible.** Downstream consumers cannot even tell Backpack was used internally. | `smirk`, `cek`, `pykinos`, and any Stackage-eligible leaf/core package |
| **Tier 2: Explicit `*-indef` Packages** *(Formal/Research Quarantine)* | Standalone indefinite package with top-level `signatures:` in its main `library` stanza, paired with `gargamelle-core` institution theories and `gargamelle-sheaf` coboundary checks. | **Explicitly non-Stackage** (isolated under `packages/*-indef/`, never a transitive dependency of any Tier-0/Tier-1 Stackage package). | `gargamelle` institution & pushout experiments (`gargamelle-*-indef`) |

---

## 2. Theoretical Alignment: Backpack, Institutions, & Ingolstadt

### 2.1 Curry–Howard–Goguen Correspondence (`gargamelle`)

* **`.hsig` file $\leftrightarrow$ Institution Signature $\Sigma \in \mathbf{Sign}$:** Declares abstract sorts (`data T`), operations (`f :: ...`), and global coherence constraints (`instance Ord T`).
* **Cabal `mixins:` renaming $\leftrightarrow$ Signature Morphism $\sigma : \Sigma_1 \to \Sigma_2$.**
* **Backpack signature merging $\leftrightarrow$ Signature Pushout $\Sigma_B \amalg_{\Sigma_C} \Sigma_W$:** Merging two indefinite units sharing a signature name automatically computes the union of their requirements and unifies shared abstract types.
* **Applicative instantiation $\leftrightarrow$ Model Amalgamation ($H^0(G;\mathbf{Mod}) \cong \mathbf{Mod}(\operatorname{colim} D)$):** Two independent instantiations with the same concrete provider yield propositionally identical types.
* **Closing the Behavioral Gap:** Because `.hsig` checks only type/kind syntax, `gargamelle-*-indef` pairs each `.hsig` with a companion `ObsMor` / `AGContract` sentence bundle verified by `gargamelle-sheaf` (`H^1 == 0`) before linking.

### 2.2 Forward Bridge to `frankenstein` & `.organ-iface`

In Ingolstadt (`TP-META-016` / `TP-META-018`), `Frankenstein.Backpack.OpenRecordSig` represents Backpack signatures natively as **`LiquidSig`-refined `Polytropos` open records** instantiated at stage time by `penelope-stage` (`PolyCarrier OrganIRBuilder`) and linked across separately compiled TUs via `.organ-iface` and `Gargamelle.Sheaf.FFI`.

Maintaining clean, small `.hsig` kernels in `smirk`, `cek`, and `gargamelle` today serves two purposes simultaneously:
1. Immediate zero-dictionary monomorphization under GHC/Cabal.
2. A concrete lowering test suite (`GhcBridge` `.hsig` $\to$ `BackpackRecordSig` $\to$ `.organ-iface`) for `frankenstein`, where open-record staging removes GHC Backpack's lack of partial instantiation, opaque `HOLE` plugin blindness, and absence of behavioral refinements.

---

## 3. Hard Operational Guardrails (HLS, TH, Plugins, Build Times)

1. **Keep Indefinite Kernels Small & Hot:** Restrict indefinite units to tight, self-contained inner loops (100–500 LOC) where eliminating dictionary passing and enabling cross-boundary `UNPACK` measurably impacts runtime. Never place an entire multi-module compiler pipeline inside an indefinite unit (compile-time multiplication).
2. **Isolate Template Haskell & QuasiQuoters:** Indefinite units produce `.hi` files with zero object code, so TH splices *defined* inside an indefinite unit cannot execute. All QuasiQuoters (`[re|...|]`, `[glob|...|]`) and `derive*` splices stay strictly in definite wrapper libraries.
3. **Never Expect `Polytropos.Plugin` to Inspect Opaque `.hsig` Fields:** During Phase 1 typechecking of an indefinite unit, `data Key` from `Key.hsig` is an opaque `HOLE:Key.Key`. Use Backpack for monomorphizing hot polymorphic engines (`RegexTarget`, `Env`, `OrderedKey`), and `Polytropos.Block` for compile-time SoA `ByteArray#` offset synthesis over concrete rows—they are orthogonal, not competing.
4. **HLS / `hie.yaml` Discipline:** Keep `hie.yaml` explicitly listing definite components so HLS (`ghcide`) and `hls-autoheal.go` never choke on multi-instantiation cradles; validate `.hsig` kernels via `cabal v2-build` / `cabal v2-test`.
5. **Avoid Type-Synonym Signature Traps:** When a `.hsig` declares `data T` and also requires a typeclass instance on `T`, fill it with a `data`/`newtype` (not a `type` synonym) inside the concrete adapter sublibrary.
6. **Zero Orphan Instances:** Concrete adapter sublibraries supply thin `newtype` wrappers or re-export canonical modules without introducing orphan instances.

---

## 4. Dual Pilot Plan & Quantitative Acceptance Gates (D2)

To validate Backpack across two structurally distinct workloads before expanding further, we execute two compact Private Backpack pilots in parallel—each paired with a `tasty-bench` (`+RTS -T`) comparison against the existing typeclass/concrete baselines:

### Pilot 1: `smirk` — `RegexTarget.hsig` (`TP-SMIRK-BACKPACK-001`)
* **Indefinite internal sublibrary (`smirk-vm-indef`):** Requires `Smirk.Target.Sig` (`data Target`, `type Elem`, `targetLength`, `indexTarget`, `sliceTarget`) and implements a tight, zero-dictionary Pike/NFA matching kernel with `{-# UNPACK #-} !Target` cursor fields.
* **Concrete adapter sublibraries:** `smirk-target-text` (`Data.Text`) and `smirk-target-bs` (`Data.ByteString`).
* **Public definite re-exports:** `Smirk.VM.Text` and `Smirk.VM.ByteString` exposed from the main definite library.
* **Benchmark Gate:** Compare `Smirk.VM.Text` and `Smirk.VM.ByteString` against dictionary-polymorphic `RegexTarget` VM on literal, alternation, and backtracking patterns (10 KiB–100 KiB inputs), measuring both ns/op and heap bytes allocated (`+RTS -T`).

### Pilot 2: `cek` — `Env.hsig` (`TP-CEK-BACKPACK-001`)
* **Indefinite internal sublibrary (`cek-env-indef`):** Requires `Cek.Env.Sig` (`data Env v`, `emptyEnv`, `lookupEnv`, `insertEnv`) and implements the core tail-recursive CEK trampoline with zero environment dictionary passing.
* **Concrete adapter sublibraries:** `cek-env-art` (backed by `Pykinos.IntMap.ART`) and `cek-env-intmap` (backed by `Data.IntMap.Strict`).
* **Public definite re-exports:** `Cek.Machine.ART` and `Cek.Machine.IntMap` exposed cleanly for apples-to-apples benchmarking without duplicating the CEK transition loop.
* **Benchmark Gate:** Compare `Cek.Machine.ART` vs. `Cek.Machine.IntMap` vs. dictionary-polymorphic baseline on deep lambda/let-binding evaluation (Church numerals, Ackermann/Fib, 5,000-binding environments), recording ns/op and bytes allocated (`+RTS -T`).

### Pilot 3 (Follow-On Tier 2 Experiment): `gargamelle-inst-indef`
Once Pilots 1 & 2 validate our Cabal/CI/GHC 9.12.4+9.14.1 workflow, add a quarantined non-Stackage `packages/gargamelle-inst-indef/` package testing Backpack signature merging as a live pushout alongside `gargamelle-sheaf` $H^1$ checks.