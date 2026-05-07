/// Rust: negate an integer. Affine ownership in MIR.
/// Used in the 7-language multi-module demo to avoid name collision
/// with Haskell's CrossModuleLib.double.
#[no_mangle]
pub fn negate_val(n: i64) -> i64 {
    0 - n
}
