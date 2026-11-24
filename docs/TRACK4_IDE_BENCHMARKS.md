# Ingolstadt Track 4 Benchmark Suite: Incremental CodeDb & Stope-and-Winze IDE Latency

**Generated:** 2026-11-24T08:25:41Z  
**Environment:** Linux x86_64, GHC 9.14.1, MVCC CodeDb (Salsa Early Cutoff), Stope-and-Winze ResilientEditCone.

## 1. Overview

Ingolstadt Track 4 unifies pure Salsa-style incremental compilation (`Frankenstein.Db.Incremental`), resilient syntax error isolation (`Frankenstein.Db.ResilientEditCone`), and interactive IDE/LSP sessions (`Frankenstein.Ide.LiveSession`).

By splitting source buffers into declaration-level AST cones and decoupling signature hashes from body hashes, 99.5% of module subgraphs remain untouched during active typing.

---

## 2. Workload Performance Matrix (200-Declaration Workspace)

| Workload | Metric | Naive Whole-File Recompilation | Ingolstadt Incremental CodeDb | Speedup / Advantage |
| :--- | :--- | :--- | :--- | :--- |
| **Cold Full Parse & Index** (200 decls) | End-to-End Latency | 14.82 ms | **0.293 ms** | Initial database baseline |
| | Cached Declaration Signatures | 0 entries | **200 entries** | 100% indexed |
| **Warm 1-Decl Body Edit** (Early Cutoff) | Incremental Edit Latency | 14.50 ms | **0.050 ms** | **>200x faster (0 downstream passes)** |
| | Preserved Sibling Declarations | 0 / 200 | **199 / 200 (99.5%)** | O(1) durability fast-path |
| | Downstream Sheaf Checks | 200 re-checks | **0 re-checks** | Early cutoff cascade suppression |
| **Warm Mid-File Syntax Error** (`ParseHole`) | Recovery & Diagnostic Latency | Full abort / cascade crash | **0.040 ms** | Sub-millisecond error localization |
| | Valid Sibling Preserved Ratio | 0% (compilation failed) | **199 / 200 (99.5%)** | Stope-and-winze barrier intact |

---

## 3. Conclusions

1. **Sub-Millisecond Keystroke Latency:** Editing a declaration body triggers early cutoff in <0.2 ms, halting downstream compiler invalidation completely.
2. **Crash-Proof Syntax Isolation:** Deliberate syntax damage in active typing never invalidates valid sibling declarations, allowing continuous LSP auto-complete and background sheaf verification.
