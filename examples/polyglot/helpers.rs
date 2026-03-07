/// Helper: double an integer
pub fn double(x: i64) -> i64 {
    x * 2
}

/// Helper: add two integers
fn main() {
    // rustc requires main; in polyglot mode this wouldn't be the entry point
    let result = double(21);
    println!("{}", result);
}
