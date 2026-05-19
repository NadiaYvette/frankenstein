// Phase A hello-world for the Rust bridge — degraded form.
//
// The Rust bridge does not yet shim std::io / core::fmt::Arguments, so
// `println!("Hello, World!")` still fails to link (Arguments::new,
// std::io::_print remain unresolved).  See ROADMAP →
// BRIDGE_rust_strings.
//
// This degraded form computes the byte-length of "Hello, World!" via
// `str::len`, returning the i64 through the proven main-returns-Int
// wrapper.  The Rust bridge now remaps `core::str::<impl str>::len` to
// the runtime's `kk_str_len`, so this compiles and runs end-to-end.
//
// Expected output: 13   (byte-length of "Hello, World!")

#[no_mangle]
pub fn main() -> i64 {
    "Hello, World!".len() as i64
}
