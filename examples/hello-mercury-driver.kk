// Phase A hello-world driver for the Mercury bridge.
// Pairs with examples/hello.m which exposes is_greater(N, M) — semidet
// returning 1 on success, 0 on failure (via exn evidence).
//
// Mercury cannot be tested standalone today (BRIDGE_mercury_strings); the
// established polyglot idiom is to expose semidet predicates and call
// them from Koka with the result treated as Int.  See
// examples/polyglot-demo/four-lang.kk for the canonical pattern.
extern is-greater( n : int, m : int ) : int {
  c "is_greater"
}

// Expected output: 13   (is_greater(7,6) returns 1 → 1 * (7+6) = 13)
pub fun main() : int
  val ok = is-greater(7, 6)
  ok * (7 + 6)
