:- module hello.
:- interface.
:- import_module io.

    % Phase A hello-world for the Mercury bridge.
    %
    % Mercury's canonical `:- pred main(io::di, io::uo) is det.` entry
    % point now works: the bridge renames the user's `main` to
    % `main_io_impl` and synthesises a no-arg `main` alias.  HLDS calls
    % to `io.write_string`, `io.print_line`, etc. are routed to the
    % Frankenstein runtime's print_str / println_str.  String literals
    % in unifications (`V = "..."`) bind to LitString.
:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

main(!IO) :-
    io.write_string("Hello, World!\n", !IO).
