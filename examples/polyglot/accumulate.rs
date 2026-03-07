/// Polyglot example: Rust provides the entry point and accumulation logic.
/// In the full pipeline, this calls functions from Scoring.hs and search.m.

fn accumulate(pairs: &[(i64, i64)]) -> i64 {
    pairs.iter().map(|&(x, y)| x * x + y * y).sum()
}

fn main() {
    // In the full polyglot pipeline, these would come from Mercury's search
    let pairs = [(3, 5), (4, 6), (7, 2), (1, 8)];
    let total = accumulate(&pairs);
    println!("Total score: {}", total);
}
