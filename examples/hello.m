:- module hello.
:- interface.
:- import_module int.

    % Phase A hello-world for the Mercury bridge.
    %
    % Mercury's standard `:- pred main(io::di, io::uo) is det.` entry point
    % requires io.write_string from Mercury's stdlib io module, which the
    % MercuryBridge does not yet shim — see ROADMAP →
    % BRIDGE_mercury_strings.
    %
    % This file uses the `:- pred main_int(int::out) is det.` convention
    % that MercuryBridge synthesises into a Frankenstein-Core main
    % returning Int.  Standalone Mercury programs with single-clause
    % fact bodies (e.g. `main_int(13).`) now compile cleanly thanks to
    % HldsParse stripping the trailing `.` from goal text so the literal
    % `13` is recognised by translateGoalK's bind-to-literal case.
    %
    % Expected output: 13   (byte-length of "Hello, World!" — hardcoded
    %                        since string printing isn't wired up yet)
:- pred main_int(int::out) is det.

:- implementation.
main_int(13).
