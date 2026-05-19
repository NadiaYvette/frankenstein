// Demonstrates that Rust's `{:?}` Debug format works through the
// Frankenstein bridge.  Differences from `{}` Display format:
//   - Strings get surrounding double quotes and escape `\n` / `\t` /
//     `\"` / `\\` etc.
//   - Numbers print the same as Display.
//
// Encoding: Rust emits `Argument::<'_>::new_debug::<T>(value)` in
// place of `new_display`.  The template byte format is identical
// (placeholder `\xc0` doesn't distinguish Display vs Debug).
//
// The bridge wraps Debug-tagged args with `rust_arg_debug` (the
// runtime allocates a KK_RUST_DEBUG_TAG cell holding the inner
// value).  At print time, kk_rust_print_one_arg checks for the tag
// and dispatches to a debug formatter that emits `"..."` with
// escaped control characters for strings and falls back to %ld for
// other types.

#[no_mangle]
pub fn id(n: i64) -> i64 { n }

pub fn main() {
    let n = id(42);
    let s = "hello\tworld";
    println!("Display: {}", n);
    println!("Debug:   {:?}", n);
    println!("Display: {}", s);
    println!("Debug:   {:?}", s);
}
