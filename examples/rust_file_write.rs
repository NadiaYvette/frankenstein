// std::fs::write + std::fs::read_to_string round-trip.  The bridge
// remaps both Rust paths to the runtime's `write_file` / `read_file`
// intrinsics, and elides the `.unwrap()` on the Result via the
// `Result::<...>::unwrap` pattern in CoreTranslate.

use std::fs;
pub fn main() {
    fs::write("/tmp/frankenstein-rwrite.txt", "round trip ok").unwrap();
    let back = fs::read_to_string("/tmp/frankenstein-rwrite.txt").unwrap();
    print!("{}", back);
}
