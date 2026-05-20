// File I/O via std::fs::read_to_string.  The bridge remaps
//   std::fs::read_to_string::<&str>(path)
// to the runtime's `read_file` intrinsic, which returns a kk_string
// (empty on error).  The user's `.unwrap()` on the Result is the
// Result::unwrap pattern elided in CoreTranslate.

use std::fs;

pub fn main() {
    let contents = fs::read_to_string("/tmp/frankenstein-rust-file-read.txt").unwrap();
    print!("{}", contents);
}
