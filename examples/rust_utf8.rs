// Rust println! with non-ASCII string literals.  The bridge stores
// the raw UTF-8 bytes verbatim in kk_string_from_literal, and the
// runtime print path emits the bytes directly — no decode/encode
// needed because Rust source and stdout are both UTF-8.  See the
// Haskell-side counterpart in examples/hello_utf8.hs, which has to
// decode codepoints and re-encode UTF-8 at output time.

pub fn main() {
    println!("ascii hello");
    println!("café");
    println!("한국어");
    println!("🎉 party");
    println!("mixed: a → b 한 c 🎉");
}
