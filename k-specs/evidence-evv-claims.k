// =================================================================
// Plotkin Evidence-Vector Dispatch Claims (D6)
//
// Soundness claims for the runtime evv semantics that
// Frankenstein.Core.EvidenceEvv compiles to (kk_evv_extend,
// kk_evv_lookup). The K model in EVV-CHECKERS mirrors the C runtime:
// extend pushes a new (effect_id, op_table) frame on top, lookup
// linear-searches from the top down.
//
// Together with the parity smoke tests in docs/evidence-evv-design.md,
// these claims establish that the new dispatch mechanism has the same
// observable behaviour as Plotkin/Pretnar's evidence-passing
// semantics — empty evv returns no handler, the most recently
// installed handler shadows older ones, unrelated effects pass
// through.
// =================================================================

requires "all-claims-def.k"

module EVIDENCE-EVV-CLAIMS
  imports ALL-CLAIMS-DEF
  imports INT

  // =================================================================
  // Group EVV1: Empty evv has no handler for any effect.
  // =================================================================

  // EVV1a: lookup against the empty evv returns 0 (unhandled).
  claim <k> evvLookup(emptyEvv, 100) => 0 </k> [label(claim-evv-1a)]

  // EVV1b: any effect id misses on emptyEvv.
  claim <k> evvLookup(emptyEvv, 193486532) => 0 </k> [label(claim-evv-1b)]

  // EVV1c: even effect-id 0 misses on emptyEvv (no false positive).
  claim <k> evvLookup(emptyEvv, 0) => 0 </k> [label(claim-evv-1c)]

  // =================================================================
  // Group EVV2: A single extend installs the handler for that
  // effect.
  // =================================================================

  // EVV2a: lookup against an extended evv with the same id returns
  // the table that was installed.
  claim <k>
    evvLookup(evvExtend(emptyEvv, 100, 42), 100)
    =>
    42
  </k> [label(claim-evv-2a)]

  // EVV2b: real djb2 hash for "ask" resolves to the installed table.
  claim <k>
    evvLookup(evvExtend(emptyEvv, 193486532, 999), 193486532)
    =>
    999
  </k> [label(claim-evv-2b)]

  // EVV2c: extending with a non-zero table id and looking it up
  // never returns 0 (preserves the data).
  claim <k>
    evvLookup(evvExtend(emptyEvv, 7, 13), 7)
    =>
    13
  </k> [label(claim-evv-2c)]

  // =================================================================
  // Group EVV3: Lookup for an unrelated effect passes through to
  // the parent evv.
  // =================================================================

  // EVV3a: a single frame for effect 100 doesn't answer for effect 200.
  claim <k>
    evvLookup(evvExtend(emptyEvv, 100, 42), 200)
    =>
    0
  </k> [label(claim-evv-3a)]

  // EVV3b: even with the same table id, the effect-id mismatch
  // means no handler.
  claim <k>
    evvLookup(evvExtend(emptyEvv, 1, 1), 2)
    =>
    0
  </k> [label(claim-evv-3b)]

  // EVV3c: two stacked frames for effect A, looked up for effect B,
  // skip past both to emptyEvv.
  claim <k>
    evvLookup(evvExtend(evvExtend(emptyEvv, 100, 1), 100, 2), 200)
    =>
    0
  </k> [label(claim-evv-3c)]

  // =================================================================
  // Group EVV4: Shadow semantics — the innermost handler wins.
  // =================================================================

  // EVV4a: extending twice with the same effect id, lookup returns
  // the second (outer-in-call-order) table.
  claim <k>
    evvLookup(evvExtend(evvExtend(emptyEvv, 100, 11), 100, 22), 100)
    =>
    22
  </k> [label(claim-evv-4a)]

  // EVV4b: shadowing also works when the parent has a different
  // effect interleaved.
  claim <k>
    evvLookup(
      evvExtend(
        evvExtend(
          evvExtend(emptyEvv, 100, 11),
          200, 33),
        100, 22),
      100)
    =>
    22
  </k> [label(claim-evv-4b)]

  // EVV4c: after shadowing 100, the parent's 200 is still reachable.
  claim <k>
    evvLookup(
      evvExtend(
        evvExtend(
          evvExtend(emptyEvv, 100, 11),
          200, 33),
        100, 22),
      200)
    =>
    33
  </k> [label(claim-evv-4c)]

  // =================================================================
  // Group EVV5: Compositional invariants of extend.
  // =================================================================

  // EVV5a: extending preserves access to other effects in the parent.
  claim <k>
    evvLookup(evvExtend(evvExtend(emptyEvv, 100, 11), 200, 22), 100)
    =>
    11
  </k> [label(claim-evv-5a)]

  // EVV5b: extending twice with the same id, the outer extend's
  // table is returned, not the inner's.
  claim <k>
    evvLookup(evvExtend(evvExtend(emptyEvv, 5, 100), 5, 200), 5)
    =>
    200
  </k> [label(claim-evv-5b)]

  // EVV5c: deep parent of 4 frames; lookup for a buried effect-id
  // still reaches it.
  claim <k>
    evvLookup(
      evvExtend(
        evvExtend(
          evvExtend(
            evvExtend(emptyEvv, 1, 10),
            2, 20),
          3, 30),
        4, 40),
      1)
    =>
    10
  </k> [label(claim-evv-5c)]

endmodule
