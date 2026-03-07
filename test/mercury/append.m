:- module append.
:- interface.
:- import_module list.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module string.

:- pred my_append(list(T)::in, list(T)::in, list(T)::out) is det.

my_append([], Ys, Ys).
my_append([X | Xs], Ys, [X | Zs]) :-
    my_append(Xs, Ys, Zs).

main(!IO) :-
    my_append([1, 2, 3], [4, 5, 6], Result),
    io.write_line(Result, !IO).
