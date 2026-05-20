// stdin().read_line(&mut buf) — the bridge rewrites the call to a
// direct `read_line()` runtime call that returns the line and
// rebinds the underlying buffer (`_1` in MIR) so subsequent reads
// of `buf` see the line content.  The MIR shape that triggers the
// rebind is `_6 = &mut _1` followed by
// `_3 = Stdin::read_line(_, _6)` — CoreTranslate.findMutRefTarget
// chases the second arg back to its source local in the body's
// statements.

use std::io;
pub fn main() {
    let mut buf = String::new();
    io::stdin().read_line(&mut buf).unwrap();
    print!("got: {}", buf);
}
