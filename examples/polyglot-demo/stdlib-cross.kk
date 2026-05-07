// Cross-language demo: Koka calls Haskell functions that use
// real Prelude map/filter/sum on standard [Int] lists.
//
// This proves that Haskell's standard library types and HOFs
// work correctly across language boundaries with Perceus RC.

extern compute( n : int ) : int {
  c "compute"
}

pub fun main() : int
  // compute(10) = sum of squares of even numbers in [1..10]
  //             = 4 + 16 + 36 + 64 + 100 = 220
  compute(10)
