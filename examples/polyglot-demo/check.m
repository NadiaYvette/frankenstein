:- module check.
:- interface.
:- import_module int.

    % Semidet predicate: succeeds only if N > Threshold.
    % Determinism `semidet` maps to the `exn` algebraic effect in OrganIR.
    % If the test fails, an `exn.fail` effect is performed.
:- pred check_threshold(int::in, int::in) is semidet.

:- implementation.

check_threshold(N, Threshold) :-
    N > Threshold.
