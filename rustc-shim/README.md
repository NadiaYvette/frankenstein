# Frankenstein Rustc Shim

A small Rust crate that hooks into `rustc`'s internal APIs to extract MIR
(Mid-level Intermediate Representation) from Rust source files and serialize
it as JSON on stdout.

## What It Does

The `rustc-mir-dump` binary acts as a custom rustc driver. It:

1. Invokes the full rustc frontend (parsing, type checking, borrow checking)
2. Intercepts compilation after analysis via the `Callbacks` trait
3. Iterates over all MIR bodies in the crate
4. Prints a JSON array describing each function's locals, basic blocks,
   statements, and terminators
5. Stops compilation (no codegen)

Frankenstein's Haskell-side `Frankenstein.RustBridge.MirParse` module
consumes this JSON output and translates it to Core IR.

## Requirements

- **Rust nightly toolchain** (pinned via `rust-toolchain.toml`)
- **`rustc-dev` component** (provides `rustc_driver`, `rustc_middle`, etc.)
- **`llvm-tools` component** (required by `rustc_private`)

The `rust-toolchain.toml` in this directory ensures the correct toolchain
and components are installed automatically when you build.

## Build

```bash
cd rustc-shim
cargo build
```

The binary is placed at `target/debug/rustc-mir-dump`.

## Usage

```bash
# Dump MIR for a Rust source file as JSON
./target/debug/rustc-mir-dump input.rs

# Or let Frankenstein invoke it automatically:
cabal run frankenstein -- test/rust/factorial.rs --emit-core
```

## Output Format

The output is a JSON array of function descriptors:

```json
[
  {
    "name": "factorial",
    "arg_count": 1,
    "locals": [
      {"idx": 0, "ty": "i64", "mut": false},
      {"idx": 1, "ty": "i64", "mut": false}
    ],
    "blocks": [
      {
        "idx": 0,
        "stmts": ["Assign(...)"],
        "term": "SwitchInt(...)"
      }
    ]
  }
]
```
