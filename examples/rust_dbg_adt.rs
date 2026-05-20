// Demonstrates `println!("{:?}", v)` for user-defined ADTs marked
// `#[derive(Debug)]`.
//
// Rust expands derive(Debug) to a `<impl Debug for T>::fmt` method
// that walks the formatter API (`Formatter::debug_struct_field2_finish`
// etc.) — too invasive for our runtime to shim faithfully.  The
// bridge filters out these derived `<impl …>::fmt` bodies (see
// isDerivedFmt in Frankenstein.RustBridge.CoreTranslate) so the user
// code links cleanly.
//
// For struct construction the MIR parser now recognises
// `Point { x: 7, y: 13 }` and emits an RvStruct rvalue with the
// type name + field names preserved.  The bridge dispatches to one
// of the `rust_struct_N` runtime helpers which allocate a
// KK_RUST_STRUCT_TAG cell carrying the metadata, and the Debug
// formatter (kk_rust_print_one_arg) produces faithful
// `Point { x: 7, y: 13 }` output.
//
// Enum variant printing isn't yet fully wired: `Color::Red` falls
// through to the source-text fallback (printed as quoted MIR text).

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
    // Output: Point { x: 7, y: 13 }
    println!("{:?}", p);
    // Output: Red  (last-path-segment of Color::Red)
    println!("{:?}", c);
}
