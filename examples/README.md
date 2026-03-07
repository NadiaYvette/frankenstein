# Examples

This directory contains polyglot example files demonstrating Frankenstein's
multi-language compilation pipeline.

## Planned Examples

- Haskell source files (`.hs`) compiled via the GHC Bridge
- Mercury source files (`.m`) compiled via the Mercury Bridge
- Rust source files (`.rs`) compiled via the Rust Bridge
- Cross-language examples showing how functions from different languages
  can be compiled through the same Core IR and linked together

## Running Examples

```bash
# Compile a Haskell example
cabal run frankenstein -- examples/example.hs --compile

# Show Core IR for a Mercury example
cabal run frankenstein -- examples/example.m --emit-core

# Show MLIR for a Rust example
cabal run frankenstein -- examples/example.rs --emit-mlir
```

For now, see `test/` for working input files in each language.
