// Demonstrates that three sequential Rust println! calls work through
// the Frankenstein bridge.  Each println! expands to
//   Arguments::<'_>::from_str → elided
//   std::io::_print → print_str
// and the three calls become three consecutive let-bound print_str
// applications in Frankenstein Core.

pub fn main() {
    println!("first");
    println!("second");
    println!("third");
}
