:- module shape.
:- interface.
:- import_module int.

:- type shape ---> circle(int) ; square(int).

:- pred area(shape::in, int::out) is det.

:- pred main_int(int::out) is det.

:- implementation.

area(circle(R), A) :- A = R.
area(square(S), A) :- A = S.

main_int(X) :- area(circle(7), X).
