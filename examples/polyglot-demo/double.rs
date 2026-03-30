/// Simple pure function: double an integer.
/// Rust bridge translates MIR to OrganIR with affine ownership.
/// Called from Koka's main alongside Haskell's fibonacci.
#[no_mangle]
pub fn double(n: i64) -> i64 {
    n + n  // avoid * const which triggers MulWithOverflow
}
