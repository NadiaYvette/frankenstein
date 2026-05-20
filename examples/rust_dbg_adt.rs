// Demonstrates `println!("{:?}", v)` for user-defined ADTs marked
// `#[derive(Debug)]`.  Status: degraded.
//
// Rust expands derive(Debug) to a `<impl Debug for T>::fmt` method
// that walks the formatter API (`Formatter::debug_struct_field2_finish`
// etc.) — too invasive for our runtime to shim faithfully.  The
// bridge filters out these derived `<impl …>::fmt` bodies (see
// isDerivedFmt in Frankenstein.RustBridge.CoreTranslate) so the user
// code links cleanly, and the runtime's
// `kk_rust_print_one_arg`'s Debug-tagged-heap-pointer branch falls
// back to a positional `(field0, field1, …)` form for genuine
// heap-allocated structs.
//
// For structs constructed in user code, however, the bridge's MIR
// parser doesn't yet recognise `Point { x: 7, y: 13 }` syntax — it
// treats the rvalue as opaque text and produces a string-shaped
// std.tuple instead of a real heap-allocated struct.  The result is
// that `{:?}` on the struct prints the MIR source text in quotes
// rather than the field values.  Better than a link error, but
// not faithful Rust output.

#[derive(Debug)]
pub struct Point {
    x: i64,
    y: i64,
}

#[derive(Debug)]
pub enum Color { Red, Green, Blue }

pub fn main() {
    let p = Point { x: 7, y: 13 };
    let c = Color::Red;
    // Output today: `"Point { x: const 7_i64, y: const 13_i64 }"`
    println!("{:?}", p);
    // Output today: `"Color::Red"`
    println!("{:?}", c);
}
