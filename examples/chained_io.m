:- module chained_io.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

    % Demonstrates that Mercury io.write_string calls chain through the
    % HLDS conjunction sequence.  The bridge sees three GoalCalls in a
    % GoalConj and emits a sequence of print_str calls; the !IO state
    % thread is discarded (the runtime is side-effectful regardless).
main(!IO) :-
    io.write_string("first\n", !IO),
    io.write_string("second\n", !IO),
    io.write_string("third\n", !IO).
