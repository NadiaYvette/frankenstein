# Test Files

Test input files for each of Frankenstein's language bridges.

## Directory Layout

```
test/
  haskell/
    Factorial.hs    -- Haskell factorial, compiled via GHC API
  mercury/
    append.m        -- Mercury list append, compiled via mmc --dump-hlds
  rust/
    factorial.rs    -- Rust factorial, compiled via rustc MIR dump
```

## Running Tests

Each file can be passed directly to the `frankenstein` executable:

```bash
# Haskell
cabal run frankenstein -- test/haskell/Factorial.hs --emit-core
cabal run frankenstein -- test/haskell/Factorial.hs --compile

# Mercury
cabal run frankenstein -- test/mercury/append.m --emit-core
cabal run frankenstein -- test/mercury/append.m --compile

# Rust
cabal run frankenstein -- test/rust/factorial.rs --emit-core
cabal run frankenstein -- test/rust/factorial.rs --compile
```

## Notes

- Haskell compilation requires GHC 9.14.1 and the `ghc` library.
- Mercury compilation requires `mmc` on PATH.
- Rust compilation requires the `rustc-mir-dump` shim to be built
  (see `rustc-shim/README.md`).
- GHC may produce `.hi` and `.o` files in the Haskell test directory;
  these are ignored by `.gitignore`.
