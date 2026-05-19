// Demonstrates Rust's radix-format placeholders through the
// Frankenstein bridge: `{:x}` (lower hex), `{:X}` (upper hex),
// `{:o}` (octal), `{:b}` (binary).
//
// Rust selects these via per-radix Argument constructors —
// Argument::<'_>::new_lower_hex / new_upper_hex / new_octal /
// new_binary — distinct from new_display.  The template byte format
// stays the same (still plain 0xc0 placeholder), so the bridge
// wraps each in a tag cell at runtime and the dispatcher in
// kk_rust_print_one_arg routes to the matching printf format
// specifier (%lx / %lX / %lo / hand-rolled binary).

#[no_mangle]
pub fn id(n: i64) -> i64 { n }

pub fn main() {
    let n = id(255);
    println!("dec: {}",  n);
    println!("hex: {:x}", n);
    println!("HEX: {:X}", n);
    println!("oct: {:o}", n);
    println!("bin: {:b}", n);
}
