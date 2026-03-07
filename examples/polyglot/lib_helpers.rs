/// Library-only Rust crate (no main).
/// Demonstrates that the compiler handles library crates for polyglot linking.
pub fn double(x: i64) -> i64 {
    x * 2
}

pub fn sum_range(lo: i64, hi: i64) -> i64 {
    let mut total: i64 = 0;
    let mut i: i64 = lo;
    while i <= hi {
        total += i;
        i += 1;
    }
    total
}
