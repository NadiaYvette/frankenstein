// Demonstrates that Rust's `println!("...", arg1, arg2, ...)` macro
// works through the Frankenstein bridge for both i64 args and string
// (`&str`) args, including the leading-placeholder form
// (`"{} = {}\n"`).
//
// Rust expands `println!` to:
//   _3 = Arguments::<'_>::new::<N, M>(template_bytes, args_array)
//   _2 = std::io::_print(_3)
// where template_bytes is a packed Rust format encoding
//   template := piece* '\x00'
//   piece    := '\xc0'                 (placeholder)
//            |  <len> <byte>{len}      (literal)
// (See kk_rust_print_args in runtime/kk_runtime.c.)
//
// The bridge pairs Arguments::new → rust_args_pack (a 2-field cell)
// and std::io::_print → rust_print_dispatch which dispatches at
// runtime: kk_string (from_str path) → kk_print_str; packed cell
// (new path) → kk_rust_print_args.  Each placeholder's value is
// classified via kk_is_string at runtime — strings print verbatim,
// otherwise we printf("%ld") as i64.

#[no_mangle]
pub fn double_plus_one(seed: i64) -> i64 {
    seed * 2 + 1
}

pub fn main() {
    let a = double_plus_one(20);
    let b = double_plus_one(50);
    println!("a={} b={}", a, b);
    println!("answer = {}", a + b);
    let name = "frankenstein";
    println!("hello, {}", name);
    println!("{} = {}", name, a + b);
}
