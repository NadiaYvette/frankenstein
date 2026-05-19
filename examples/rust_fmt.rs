// Demonstrates that Rust's `println!("...", arg1, arg2, ...)` macro
// works through the Frankenstein bridge for the i64-arg cases.
//
// Rust expands `println!` to:
//   _3 = Arguments::<'_>::new::<N, M>(template_bytes, args_array)
//   _2 = std::io::_print(_3)
// where template_bytes is a packed Rust format encoding
// (see kk_rust_print_args in runtime/kk_runtime.c).
//
// The bridge pairs Arguments::new → rust_args_pack (a 2-field cell)
// and std::io::_print → rust_print_dispatch (dispatches kk_string vs
// packed cell at runtime).  The template is hex-encoded as
// `__RBYTES:HHHH…` to preserve raw bytes ≥ 0x80 through the bridge's
// UTF-8-typed string IR.

#[no_mangle]
pub fn double_plus_one(seed: i64) -> i64 {
    seed * 2 + 1
}

pub fn main() {
    let a = double_plus_one(20);
    let b = double_plus_one(50);
    println!("a={} b={}", a, b);
    println!("answer = {}", a + b);
}
