// Phase A hello-world for the Rust bridge.
//
// The bridge now recognises the println! macro's expanded MIR:
//   Arguments::<'_>::from_str(const "...") → identity (elided)
//   std::io::_print(arg) → print_str (the bridge's emitter routes
//                                       this to kk_print_str)
// and the splitOperands parser tracks string-literal depth so commas
// inside Rust string literals don't split the call arguments.

pub fn main() {
    println!("Hello, World!");
}
