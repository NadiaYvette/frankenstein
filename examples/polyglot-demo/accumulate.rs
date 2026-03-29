/// Polyglot example: Rust provides the accumulator with affine ownership.
/// The Rust bridge translates this through MIR, preserving move semantics
/// as affine multiplicity in OrganIR.

/// Sum a slice of scores. Ownership of the slice is affine — it is consumed
/// exactly once, which Perceus translates to a single drop at the end.
fn sum_scores(scores: &[i64]) -> i64 {
    let mut total: i64 = 0;
    let mut i: usize = 0;
    while i < scores.len() {
        total += scores[i];
        i += 1;
    }
    total
}

/// Entry point for standalone testing only.
/// In the polyglot binary, Koka's main is the entry point.
fn main() {
    let scores = [34, 50, 61, 25, 41];
    let total = sum_scores(&scores);
    println!("Rust standalone total: {}", total);
}
