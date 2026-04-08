-- Phase 10a demo: lights up MLIR's linalg dialect.
--
-- The Futhark frontend recognises sum_iota and dot_iota as primitives
-- and the MLIR emitter lowers each one to a memref + linalg.generic
-- reduction. The pipeline includes --convert-linalg-to-loops, so the
-- linalg.generic eventually becomes the same scf.for/llvm code path as
-- the rest of Frankenstein, but the *intermediate* form is genuine
-- linalg dialect — verifiable with `--emit-mlir`.

let main : i64 = sum_iota 100   -- 0 + 1 + ... + 99 = 4950
