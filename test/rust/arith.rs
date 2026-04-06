// Pure arithmetic for bridge bisimulation testing.
// Expected: compute(2) = 2 * (3 + 4) + 5 * 6 = 44
pub fn compute(x: i64) -> i64 {
    x * (3 + 4) + 5 * 6
}
