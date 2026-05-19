:- module hello.
:- interface.
:- import_module int.

    % Phase A hello-world for the Mercury bridge — polyglot fragment.
    %
    % Standalone Mercury (with `main(io::di, io::uo) is det`) needs
    % io.write_string from Mercury's stdlib io module, which the
    % MercuryBridge does not yet shim (BRIDGE_mercury_strings).
    %
    % Single-clause `is det` predicates (e.g. `seven(7).`) trigger HLDS's
    % unification machinery — the bridge does not link `unify`/`HeadVar`
    % for that path.  Single-clause `is det` functions (`func f = int.`)
    % have the same issue.
    %
    % Working pattern (proven by polyglot-demo/check.m): `is semidet`
    % predicates with `is_greater(N, M) :- N > M.` style bodies.  The
    % bridge wraps the test result in an ECase that returns 1 on success
    % and 0 on failure (via exn.fail evidence).
    %
    % Pair this file with hello-mercury-driver.kk.

:- pred is_greater(int::in, int::in) is semidet.

:- implementation.

is_greater(N, M) :- N > M.
