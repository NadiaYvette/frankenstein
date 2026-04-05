% Mercury multi predicate: generates multiple solutions via choice effect.
% pick(X) is multi — succeeds with X = 1, X = 2, or X = 3.
% The choice handler collects all solutions and sums them: 1+2+3 = 6.

:- module pick.
:- interface.
:- import_module int.

:- pred pick(int::out) is multi.

:- implementation.

pick(1).
pick(2).
pick(3).
