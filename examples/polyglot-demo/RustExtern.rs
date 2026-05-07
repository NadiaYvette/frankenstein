extern "C" {
    fn square(n: i64) -> i64;
    fn fibonacci(n: i64) -> i64;
}

pub fn rust_combined(n: i64) -> i64 {
    unsafe { square(n) + fibonacci(n) }
}
