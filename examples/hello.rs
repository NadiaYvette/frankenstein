// Phase A hello-world for the Rust bridge — heavily degraded form.
//
// Two Rust string-ABI gaps prevent a "natural" hello:
//   1. `println!(...)` pulls in Arguments::new / std::io::_print which
//      aren't shimmed (see ROADMAP → BRIDGE_rust_strings).
//   2. `str::len()` produces a mangled symbol mlir-opt rejects.
//
// This degraded form returns the byte-length of "Hello, World!" as a
// hardcoded constant.  It does *not* exercise any string ABI surface;
// it exists as a placeholder so the hello-test driver has an entry per
// bridge and any future Rust string work has an obvious file to update.
//
// Expected output: 13   (byte-length of "Hello, World!")

#[no_mangle]
pub fn main() -> i64 {
    let _greeting = "Hello, World!";
    13
}
