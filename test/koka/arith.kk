// Pure arithmetic for bridge bisimulation testing.
// Expected: compute(2) = 2 * (3 + 4) + 5 * 6 = 44
pub fun compute( x : int ) : int
  x * (3 + 4) + 5 * 6
