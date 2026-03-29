:- module search.
:- interface.
:- import_module int.

    % Find pairs (X, Y) where both are in [1..N] and X + Y > Threshold.
    % Determinism `nondet` maps to the `choice` algebraic effect in OrganIR.
:- pred valid_pair_search(int::in, int::in, int::out, int::out) is nondet.

:- implementation.

valid_pair_search(N, Threshold, X, Y) :-
    between(1, N, X),
    between(1, N, Y),
    X + Y > Threshold.

:- pred between(int::in, int::in, int::out) is nondet.
between(Lo, Hi, Lo) :- Lo =< Hi.
between(Lo, Hi, N) :- Lo < Hi, between(Lo + 1, Hi, N).
