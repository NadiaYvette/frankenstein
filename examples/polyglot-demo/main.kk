// Frankenstein Phase 1 Polyglot Demo — Koka orchestrator
//
// This Koka module is the "main brain" of the polyglot demo.
// It imports functions compiled through the GHC, Mercury, and Rust bridges,
// wires them together using algebraic effects, and produces a single result.
//
// Language roles:
//   Haskell (Scoring.hs)    — pure scoring function: score(x, y) -> int
//   Mercury (search.m)      — nondeterministic search via choice effect
//   Rust    (accumulate.rs) — affine accumulator: sum_scores(xs) -> int
//   Koka    (this file)     — effect handlers + orchestration

// ---------------------------------------------------------------------------
// External declarations for cross-language functions
// These map to definitions compiled through the respective bridges.
// The linker resolves these names across modules.
// ---------------------------------------------------------------------------

// Haskell: pure scoring — score(x, y) = x*x + y*y + bonus(x) + bonus(y)
extern score( x : int, y : int ) : int

// Rust: accumulate a list of scores into a total
extern sum-scores( xs : list<int> ) : int

// ---------------------------------------------------------------------------
// Choice effect — models Mercury's nondeterminism
//
// Mercury predicates with determinism `nondet` or `multi` translate to
// functions that perform the `choose` operation. The Koka handler below
// collects all solutions via backtracking.
// ---------------------------------------------------------------------------

effect choice
  ctl choose( n : int ) : int

// Mercury: find valid pairs where x + y > threshold
// In the full pipeline this is compiled from search.m; the bridge translates
// Mercury's `nondet` determinism into `choice` effect usage.
extern valid-pair-search( n : int, threshold : int ) : choice (int, int)

// ---------------------------------------------------------------------------
// Effect handler: collect all results from a nondeterministic computation
// ---------------------------------------------------------------------------

fun collect-all( action : () -> <choice,div> list<a> ) : <div> list<a>
  var results := []
  with handler
    ctl choose(n)
      val saved = results
      list(1, n).foreach fn(i)
        results := saved
        resume(i)
  val r = action()
  results := results ++ [r]
  results

// ---------------------------------------------------------------------------
// Main: compose all four languages
// ---------------------------------------------------------------------------

fun main() : <console,div> ()
  val n = 6
  val threshold = 8

  // Step 1: Mercury generates valid pairs via backtracking (choice effect)
  val pairs = collect-all
    [valid-pair-search(n, threshold)]

  println("Pairs found: " ++ pairs.length.show)

  // Step 2: Haskell scores each pair (pure function)
  val scores = pairs.map fn(p)
    match p
      (x, y) -> score(x, y)

  println("Scores: " ++ scores.show-list(show))

  // Step 3: Rust accumulates the scores (affine ownership)
  val total = sum-scores(scores)

  println("Total score: " ++ total.show)

  // Step 4: Summarize
  println("=== Polyglot demo: 4 languages, 1 binary ===")
  println("  Haskell: scored " ++ pairs.length.show ++ " pairs")
  println("  Mercury: searched " ++ n.show ++ "x" ++ n.show ++ " grid, threshold " ++ threshold.show)
  println("  Rust:    accumulated " ++ scores.length.show ++ " scores")
  println("  Koka:    handled choice effect, orchestrated pipeline")
  println("  Result:  " ++ total.show)
