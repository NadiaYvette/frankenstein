// Demonstrates that Rust's `println!` works for non-i64 numeric types
// (i32, u32, u64, u16, i16, u8, i8).  Each type ends up at i64 width
// in the Frankenstein IR (everything is i64-valued), but the bridge
// preserves the source type by wrapping each Argument with a
// per-type runtime tag.  The renderer then masks / sign-extends as
// appropriate when printing:
//   u32 → mask to 32 bits, print unsigned
//   i32 → mask to 32 bits, interpret as signed
//   u64 → cast to uint64_t, print %llu (handles values > i64 max)
//   u16/i16/u8/i8 → analogous narrower masks

#[no_mangle] pub fn idu32(n: u32) -> u32 { n }
#[no_mangle] pub fn idi32(n: i32) -> i32 { n }
#[no_mangle] pub fn idu64(n: u64) -> u64 { n }
#[no_mangle] pub fn idu8(n: u8) -> u8 { n }
#[no_mangle] pub fn idi8(n: i8) -> i8 { n }

pub fn main() {
    let u: u32 = idu32(4000000000);  // > i32 max
    let i: i32 = idi32(-12345);
    let big: u64 = idu64(18000000000000000000);  // > i64 max
    let b: u8 = idu8(200);
    let s: i8 = idi8(-50);
    println!("u32: {}", u);
    println!("i32: {}", i);
    println!("u64: {}", big);
    println!("u8:  {}", b);
    println!("i8:  {}", s);
}
